package modules

import _root_.scalikejdbc.{DB, DBSession, SQLInterpolation}
import org.apache.http.impl.client.HttpClients
import org.apache.http.{HttpResponse, HttpStatus}
import org.joda.time.DateTime
import collection.JavaConversions._
import modules.common.using

package object wikipedia extends SQLInterpolation with com.typesafe.scalalogging.slf4j.LazyLogging {
  private def createWikipediaURL(pageName:String):String =
     "http://ja.wikipedia.org/wiki/%s".format(java.net.URLEncoder.encode(pageName, "UTF-8"))

  private def getLastModified(response: org.apache.http.HttpResponse):Option[DateTime] = {
    Option(response.getFirstHeader("Last-Modified")).map { lastModified =>
      new DateTime(org.apache.http.client.utils.DateUtils.parseDate(lastModified.getValue))
    }
  }

  private def ensureOK(response:HttpResponse):Unit = {
    val statusCode = response.getStatusLine.getStatusCode
    if (statusCode != HttpStatus.SC_OK) throw new HttpStatusCodeIsNot200Exception(statusCode)
  }

  def parseWikipediaEntry(pageName:String):WikipediaEntry = {
    val url = createWikipediaURL(pageName)
    using(HttpClients.createDefault()) { httpClient =>
      logger.debug("Fetching: %s".format(url))
      using(httpClient.execute(new org.apache.http.client.methods.HttpGet(url))) { response =>
        // HTTPステータスコードが200であることを確認（そうでない場合例外を送出する）
        ensureOK(response)
        using(response.getEntity.getContent) { is =>
          val soup = org.jsoup.Jsoup.parse(is, "UTF-8", url)
          WikipediaEntry(
            // <h1 id="firstHeading">...</h1>
            title = soup.getElementById("firstHeading").text(),
            // <div id="mw-content-text"><p>...</p></div>
            content = soup.getElementById("mw-content-text").children().find(_.tagName.equals("p")).map(_.text().trim),
            // <link rel="canonical" href="...">
            canonical = soup.getElementsByTag("link")
              .find(elem => elem.hasAttr("rel") && elem.attr("rel").equals("canonical") && elem.hasAttr("href"))
              .map(link => Canonical(link.attr("href"))),
            // HTTPレスポンスヘッダから Last-Modifiedを取得（ない場合は現在時刻を入れておく）
            lastModified = getLastModified(response).getOrElse(new DateTime())
          )
        }
      }
    }
  }

  def getWikipediaEntry(pageName:String, ipAddress:String):WikipediaEntry = {
    (DB readOnly { implicit session =>
      sql"select * from wikipedia_cache where page_name=${pageName} and current_timestamp - fetched_at < 7".map { row =>
        logger.debug("Cache hit.")
        WikipediaEntry(
          row.string("title"),
          row.stringOpt("content"),
          row.stringOpt("canonical_url").map(Canonical(_)),
          row.jodaDateTime("last_modified"),
          row.jodaDateTimeOpt("fetched_at")
        )
        /* 本当は賞味期限切れのキャッシュに対してもHEADリクエストをWikipediaに送ってLast-Modifiedを確認することで
           賞味期限の延長を認める処理をするほうが理想的 */
      }.single.apply
    }).getOrElse {
      // キャッシュになければHTTPでWikipediaからフェッチ
      // 1秒以内に複数回 Wikipediaへのフェッチを発生させようとするユーザーを弾く
      DB localTx { implicit session =>
        sql"select 1 from wikipedia_clients where ip_address=${ipAddress} and datediff('SECOND', fetched_at, current_timestamp) < 1".map(_.int(1)).single.apply.foreach { one =>
          sql"update wikipedia_clients set fetched_at=current_timestamp where ip_address=${ipAddress}".update.apply
          throw new SecurityException("too fast re-fetch")
        }

        // 短時間の大量フェッチを阻止するために、Wikipediaからのフェッチを発生させたユーザーのIPアドレスを記録する
        sql"""MERGE INTO wikipedia_clients(ip_address,fetched_at) KEY(ip_address) values(${ipAddress}, current_timestamp)""".update.apply
        val entry = parseWikipediaEntry(pageName)
        // フェッチしたコンテンツはキャッシュに保存する
        sql"""MERGE INTO wikipedia_cache(page_name,title,content,canonical_url,last_modified,fetched_at) KEY(page_name)
           VALUES(${pageName}, ${entry.title},${entry.content},${entry.canonical.map(_.url)},${entry.lastModified}, current_timestamp)""".update.apply
        logger.debug("Cache updated.")
        entry
      }
    }
  }
}

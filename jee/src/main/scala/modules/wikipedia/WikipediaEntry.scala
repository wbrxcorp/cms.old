package modules.wikipedia

import org.joda.time.DateTime

case class WikipediaEntry(
      title:String, // 項目タイトル
      content:Option[String], // ページ内容（最初の段落）
      canonical:Option[Canonical], // ページの正式なURLと項目名
      lastModified:DateTime,  // Wikipedia上での最終更新時刻
      fetchedAt:Option[DateTime] = None // キャッシュに保存された時刻
)

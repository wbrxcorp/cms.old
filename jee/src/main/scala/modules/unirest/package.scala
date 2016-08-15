package modules

import com.mashape.unirest.request.{GetRequest,HttpRequestWithBody}
import com.mashape.unirest.http.JsonNode

package object unirest {
  object Unirest {
    def get(url:String)(implicit f:GetRequest=>GetRequest = {x=>x}):JsonNode = {
      val request = com.mashape.unirest.http.Unirest.get(url)
      f(request.header("accept",  "application/json")).asJson.getBody
    }

    def post(url:String)(implicit f:HttpRequestWithBody=>HttpRequestWithBody = {x=>x}):JsonNode = {
      val request = com.mashape.unirest.http.Unirest.post(url)
      f(request.header("accept",  "application/json")).asJson.getBody
    }
  }

}

package com.walbrix.cms.servlet

import java.awt.{Font, Color}
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import javax.imageio.ImageIO
import javax.servlet.http.HttpServletRequest

import collection.JavaConversions._

import jj.play.ns.nl.captcha.text.producer.DefaultTextProducer
import jj.play.ns.nl.captcha.text.renderer.DefaultWordRenderer
import org.apache.commons.codec.binary.Base64

/**
 * Created by shimarin on 14/11/24.
 */
class FeedbackServlet extends RestServlet {
  val renderer = new DefaultWordRenderer(new Color(0,0,0), Seq(new Font("Arial", Font.BOLD, 20), new Font("Courier", Font.BOLD, 20)))
  val producer = new DefaultTextProducer()

  private def captchaAttrName(path:String):String = {
    "captcha_%s".format(path)
  }

  override def get(request:HttpServletRequest, headers:ResponseHeader):AnyRef = {
    // TODO: generate cross origin headers : http://qiita.com/kawaz/items/6a22c2c970c8d932a3a1
    val path = "path/to/content" // TODO: impl
    val captcha = producer.getText
    request.getSession().setAttribute(captchaAttrName(path), captcha)
    val image = new BufferedImage(100, 64, BufferedImage.TYPE_INT_ARGB)
    renderer.render(captcha, image)
    val bytes = new ByteArrayOutputStream()
    ImageIO.write(image, "png", bytes)
    Map(
      "title"->"Entry title",  // todo
      "feedback_url"->"post url", // todo
      "captcha"->("data:image/png;base64," + Base64.encodeBase64String(bytes.toByteArray))
    )
  }

  override def post(request:HttpServletRequest, headers:ResponseHeader):AnyRef = {
    val path = "path/to/content" // TODO: impl
    val attrName = captchaAttrName(path)
    val session = request.getSession()
    val (savedCaptcha,givenCaptcha) = (
      Option(session.getAttribute(attrName)).map(_.asInstanceOf[String]).getOrElse(raiseForbidden("no valid session")),
      Option(request.getParameter("captcha")).getOrElse(raiseBadRequest("captcha is not given"))
    )
    session.removeAttribute(attrName)

    if (savedCaptcha != givenCaptcha) raiseForbidden("Captcha doesn't match")
    Map("success"->false, "info"->"Not implemented yet")
  }
}

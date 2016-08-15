package com.walbrix.cms.servlet

import javax.servlet._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

import com.walbrix.cms.Authentication
import org.jsoup.Jsoup

/**
 * Created by shimarin on 14/11/23.
 */
class SPAFilter extends Filter {
  private var actualPage:String = "/index.html"
  private var servletContext:ServletContext = _

  def setActualPage(actualPage:String):Unit = this.actualPage = actualPage

  private def getContextURL(request:HttpServletRequest):String = {
    val buf = new StringBuilder
    val scheme = request.isSecure match {
      case true => ("https", 443)
      case false => ("http", 80)
    }
    buf.append(scheme._1)
    buf.append("://")
    buf.append(request.getServerName)
    buf.append(
      request.getServerPort match {
        case port if port != scheme._2 => ":%d".format(port)
        case _ => ""
      }
    )
    buf.append(request.getContextPath)
    buf.toString()
  }

  override def init(filterConfig: FilterConfig): Unit = {
    servletContext = filterConfig.getServletContext
    Option(servletContext.getResourceAsStream("/META-INF/MANIFEST.MF")).foreach { is =>
      try {
        val manifest = new java.util.jar.Manifest(is)
        val attributes = manifest.getMainAttributes
        servletContext.setAttribute("appVersion", attributes.getValue("Implementation-Version"))
      }
      finally {
        is.close()
      }
    }
  }

  override def doFilter(request: ServletRequest, response: ServletResponse, filterChain: FilterChain): Unit = {
    val httpRequest = request.asInstanceOf[HttpServletRequest]
    Option(servletContext.getResourceAsStream(actualPage)).foreach { actualPage =>
      request.setAttribute("actualPage", Jsoup.parse(actualPage,"UTF-8",getContextURL(httpRequest)).getElementsByTag("body"))
    }
    filterChain.doFilter(request, response)
  }

  override def destroy(): Unit = {}

  protected def forward(to:String, request:ServletRequest, response:ServletResponse):Unit = {
    val requestDispatcher = servletContext.getRequestDispatcher(to)
    requestDispatcher.forward(request, response)
  }

  protected def redirect(to:String, response:ServletResponse):Unit = {
    response.asInstanceOf[HttpServletResponse].sendRedirect(to)
  }

}

class AuthenticationRequiredSPAFilter extends SPAFilter with Authentication {
  override def doFilter(request: ServletRequest, response: ServletResponse, filterChain: FilterChain): Unit = {
    this.getUserOpt() match {
      case Some(user) =>
        super.doFilter(request, response, filterChain)
      case None =>
        notAuthenticated(request, response, filterChain)
    }
  }

  def notAuthenticated(request: ServletRequest, response: ServletResponse, filterChain: FilterChain): Unit = {
    forward("/login.jsp", request, response)
  }
}
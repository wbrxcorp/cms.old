package modules.scalatra

trait LogErrors extends org.scalatra.ScalatraServlet with com.typesafe.scalalogging.slf4j.LazyLogging {
  error { case e =>
    logger.error("error", e)
    throw e
  }
}

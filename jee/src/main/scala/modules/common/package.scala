package modules

import scala.language.reflectiveCalls

package object common {
  def hello():Unit = println("Hello, World!")
  def using[A <: { def close():Unit },B]( resource:A )( f:A => B ) = try(f(resource)) finally(resource.close)
}

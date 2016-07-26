package modules.common

trait Using {
  type Closable = { def close():Unit }
  def using[A <: Closable,B]( resource:A )( f:A => B ) = try(f(resource)) finally(resource.close)
}

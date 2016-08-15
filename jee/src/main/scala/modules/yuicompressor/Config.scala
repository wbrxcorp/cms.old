package modules.yuicompressor

case class Config(
  lineBreak:Int = -1,
  noMunge:Boolean = false,
  preserveSemi:Boolean = false,
  disableOptimizations:Boolean = false
)

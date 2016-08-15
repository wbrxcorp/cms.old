package modules

package object hash {
  def sha1(bytes:Array[Byte]):String = {
    org.apache.commons.codec.binary.Hex.encodeHexString(java.security.MessageDigest.getInstance("SHA-1").digest(bytes))
  }
  def sha1(in:java.io.InputStream):String = {
    sha1(org.apache.commons.io.IOUtils.toByteArray(in))
  }
  def sha1(str:String, charset:String = "UTF-8"):String = {
    sha1(str.getBytes(charset))
  }
}

package modules

import java.io.{OutputStream, InputStream, InputStreamReader, Writer, OutputStreamWriter}
import com.yahoo.platform.yui.compressor.{CssCompressor, JavaScriptCompressor}
import modules.common.using

package object yuicompressor {
  def compressJs(in:InputStream, out:Writer, config:Config = Config()):Unit = {
    //"application/javascript"
    using (new InputStreamReader(in)) { reader =>
      val compressor = new JavaScriptCompressor(reader, null);
      compressor.compress(out, config.lineBreak, !config.noMunge, false, config.preserveSemi, config.disableOptimizations);
    }

  }
  def compressCss(in:InputStream, out:Writer, config:Config = Config()):Unit = {
    // "text/css"
    using (new InputStreamReader(in)) { reader =>
      val compressor = new CssCompressor(reader);
      compressor.compress(out, config.lineBreak);
    }
  }
}

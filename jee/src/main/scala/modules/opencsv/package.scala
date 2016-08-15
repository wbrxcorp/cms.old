package modules

import com.opencsv.{CSVWriter, CSVReader}
import modules.common.using

package object opencsv {
  def withCSVWriter[T](out:java.io.OutputStream, charset:String="UTF-8")(f:CSVWriter=>T):T = {
    using (new java.io.OutputStreamWriter(out, charset)) { osw =>
      using (new CSVWriter(osw)) { writer =>
        f(writer)
      }
    }
  }

  def withCSVWriterToFile[T](filename:String, charset:String="UTF-8")(f:CSVWriter=>T):T = {
    using (new java.io.FileOutputStream(filename)) { out =>
      withCSVWriter(out, charset)(f)
    }
  }

  def withCSVReader[T](in:java.io.InputStream, charset:String="UTF-8")(f:CSVReader=>T):T = {
    using (new java.io.InputStreamReader(in, charset)) { isr =>
      using (new CSVReader(isr)) { reader =>
        f(reader)
      }
    }
  }

  def withCSVReaderFromFile[T](filename:String, charset:String="UTF-8")(f:CSVReader=>T):T = {
    using (new java.io.FileInputStream(filename)) { in =>
      withCSVReader(in, charset)(f)
    }
  }
}

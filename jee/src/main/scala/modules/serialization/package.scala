package modules
import modules.common.using

package object serialization {
  def saveObject(obj:Serializable, filename:String):Unit = {
    using(new java.io.FileOutputStream(filename)) { fos =>
      using(new java.io.ObjectOutputStream(fos)) { oos =>
        oos.writeObject(obj)
      }
    }
  }

  def loadObject(filename:String):AnyRef = {
    using(new java.io.FileInputStream(filename)) { fis =>
      using(new java.io.ObjectInputStream(fis)) { ois =>
        ois.readObject
      }
    }
  }

}

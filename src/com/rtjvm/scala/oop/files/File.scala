package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, val contents: String)
  extends DirEntry(parentPath, name) {

  override def asDirectory: Directory = throw new FileSystemException("A file cannot be converted to a directory")

  override def getType: String = "File"

  override def asFile: File = this

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true

  def setContent(newContents: String): File = {
    new File(parentPath, name, newContents)
  }

  def appendContent(newContents: String): File = {
    setContent(contents + "\n" + newContents);
  }

}

object File {
  def empty(parentPath: String, name: String): File = {
    new File(parentPath, name, "")
  }
}

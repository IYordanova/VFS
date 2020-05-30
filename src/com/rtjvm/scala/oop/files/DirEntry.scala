package com.rtjvm.scala.oop.files

abstract class DirEntry (val parentPath: String, val name: String){

  def path: String = {
    val separator =
      if(Directory.ROOT_PATH.equals(parentPath)) ""
      else Directory.SEPARATOR
    parentPath + separator + name
  }

  def asDirectory: Directory

  def asFile: File

  def getType: String

  def isDirectory: Boolean

  def isFile: Boolean

}

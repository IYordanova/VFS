package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.{FileSystemException, State}

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    val root = state.root
    val wd = state.wd
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir
    val destDir = doFindEntry(root, absolutePath)
    if (destDir == null || !destDir.isDirectory) {
      state.setMessage(s"$dir: no such directory")
    } else {
      State(root, destDir.asDirectory)
    }
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDir: Directory, path: List[String]): DirEntry = {
      if(path.isEmpty || path.head.isEmpty) currentDir
      else if(path.tail.isEmpty) currentDir.findEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if(nextDir == null) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if(path.isEmpty) result
      else if(".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if("..".equals(path.head)) {
        if(result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val collapsedTokens = collapseRelativeTokens(tokens, List())
    if(collapsedTokens == null) null
    else findEntryHelper(root, collapsedTokens)
  }

}
package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State


class Rm(name: String) extends Command {

  def doRm(state: State, absolutePath: String): State = {
    def rmHelper(currentDir: Directory, path: List[String]): Directory = {
      if(path.isEmpty) currentDir
      else if (path.tail.isEmpty) currentDir.removeEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if(!nextDir.isDirectory) currentDir
        else {
          val newNextDirectory = rmHelper(nextDir.asDirectory, path.tail)
          if(newNextDirectory == nextDir) currentDir
          else currentDir.replaceEntry(path.head, newNextDirectory)
        }
      }
    }

    val tokens = absolutePath.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)
    if(newRoot == state.root) {
      state.setMessage(s"$absolutePath: no such file or directory")
    } else {
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
    }
  }

  override def apply(state: State): State = {
    val wd = state.wd
    val absolutePath =
      if(name.startsWith(Directory.SEPARATOR)) name
      else if(wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name
    if(Directory.ROOT_PATH.equals(absolutePath)) {
      state.setMessage("Cannot delete root!!!!")
    } else {
      doRm(state, absolutePath)
    }
  }

}

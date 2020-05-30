package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: List[String]) extends Command {

  def createContent(args: List[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if (currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex + 1, accumulator + " " + args(currentIndex))
    }

    createContentHelper(0, "")
  }

  def doEcho(state: State, content: String, fileName: String, append: Boolean): State = {
    if (fileName.contains(Directory.SEPARATOR)) {
      state.setMessage("Echo: file name must not contain separators")
    } else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ fileName, content, append)
      if (newRoot == state.root) {
        state.setMessage(s"$fileName:  no such file")
      } else {
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
      }
    }
  }

  def getRootAfterEcho(currentDir: Directory, path: List[String], content: String, append: Boolean): Directory = {
    if (path.isEmpty) currentDir
    else if (path.tail.isEmpty) {
      val dirEntry = currentDir.findEntry(path.head)
      if (dirEntry == null) currentDir.addEntry(new File(currentDir.path, path.head, content))
      else if (dirEntry.isDirectory) currentDir
      else if(append) currentDir.replaceEntry(path.head, dirEntry.asFile.appendContent(content))
      else currentDir.replaceEntry(path.head, dirEntry.asFile.setContent(content))
    } else {
      val nextDir = currentDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, content, append)
      if(newNextDir == nextDir) currentDir
      else currentDir.replaceEntry(path.head, newNextDir)
    }
  }

  override def apply(state: State): State = {
    if (args.isEmpty) {
      state
    } else if (args.length == 1) {
      state.setMessage(args.head)
    } else {
      val operator = args(args.length - 2)
      val fileName = args.last
      val content = createContent(args, args.length - 2)
      if (">>".equals(operator)) {
        doEcho(state, content, fileName, append = true)
      } else if (">".equals(operator)) {
        doEcho(state, content, fileName, append = false)
      } else {
        state.setMessage(createContent(args, args.length))
      }
    }
  }

}

package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(entryName: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(entryName)) {
      state.setMessage(s"Entry $entryName already exists")
    } else if (entryName.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$entryName must not contain separators")
    } else if (checkIllegal(entryName)) {
      state.setMessage(s"$entryName is illegal")
    } else {
      doCreateEntry(state)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(state: State): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd
    val allDirsInPath = wd.getAllFoldersInPath
    val newEntry = createSpecificEntry(state)
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    val newWd = newRoot.findDescendant(allDirsInPath)
    State(newRoot, newWd.asDirectory)
  }

  def createSpecificEntry(state: State): DirEntry

}

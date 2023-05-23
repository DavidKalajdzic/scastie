package com.olegych.scastie.api

import play.api.libs.json.{Json, OFormat}

sealed trait FileOrFolder {
  val name: String
  val path: String

  def isFolder: Boolean
}

object FileOrFolder {
  implicit val format: OFormat[FileOrFolder] = Json.format[FileOrFolder]
}


case class File(override val name: String, content: String = "", override val path: String = "") extends FileOrFolder {
  def isFolder: Boolean = false
}

object File {
  implicit val format: OFormat[File] = Json.format[File]
}

case class Folder(override val name: String, children: List[FileOrFolder] = List(), override val path: String = "") extends FileOrFolder {
  def isFolder: Boolean = true

  def isEmpty: Boolean = {
    children.isEmpty
  }

  def summary: String = {
    import System.{lineSeparator => nl}
    children.headOption match {
      case Some(File(_, content, _)) => content.split(nl).take(3).mkString(nl)
      case _ => this.toString.take(200) + "..."
    }
  }

  def childHeadFileContent: String = {
    children.head.asInstanceOf[File].content
  }
}

object Folder {
  implicit val format: OFormat[Folder] = Json.format[Folder]

  def singleton(code: String): Folder = {
    Folder("", List(File("code.scala", code, "/code.scala")), "/")
  }
}

object FileOrFolderUtils {
  def find(root: Folder, path: String): Option[FileOrFolder] = {
    if (root.path == path) {
      Some(root)
    } else {
      root.children.foldLeft[Option[FileOrFolder]](None) { (acc, fileOrFolder) =>
        acc.orElse(fileOrFolder match {
          case f: File if f.path == path => Some(f)
          case l: Folder => find(l, path)
          case _ => None
        })
      }
    }
  }


  def remove(root: Folder, path: String): Folder = {
    root.copy(children = root.children
      .map {
        case f: File => f
        case l: Folder => remove(l, path)
      }
      .filterNot(_.path == path)
    )
  }

  // place a new item inside the folder in dstFolder
  // newItem's path is ignored, it will be computed as follows : dstFolder + "/" + newItem.name
  def insert(root: Folder, newItem: FileOrFolder, dstFolder: String): Folder = {
    recomputePaths(
      root.copy(children =
        if (root.path.equals(dstFolder)) {
          root.children.filterNot(_.name == newItem.name) :+ newItem
        } else
          root.children.map {
            case f: File =>
              if (f.path.equals(dstFolder)) throw new IllegalArgumentException("BRUH")
              else f
            case l: Folder => insert(l, newItem, dstFolder)
          }
      ))
  }

  def insert(root: Folder, item: FileOrFolder): Folder = {
    val idx = item.path.lastIndexOf("/")
    // find the folder location
    val dst = if (idx <= 0) "/" else item.path.substring(0, idx)
    insert(root, item, dst)
  }

  def move(root: Folder, srcPath: String, dstPath: String): Folder = {
    if (dstPath.startsWith(srcPath)) {
      root //refuse to drop a folder inside of himself as it's done in intellij
    } else {
      val movingFolder = find(root, srcPath).get
      try {
        recomputePaths(
          insert(
            remove(root, srcPath),
            movingFolder,
            dstPath)
        )
      } catch {
        case x: IllegalArgumentException =>
          Console.println("Cannot drop into a file :)")
          root
      }
    }
  }

  def recomputePaths(f: Folder, prefix: String = ""): Folder = {
    val isRoot = f.name.isEmpty
    val currFolderPath = if (isRoot) prefix else prefix + "/" + f.name //don't put double // if the root folder has no name
    f.copy(
      path = if (isRoot) "/" else currFolderPath,
      children = f.children.map {
        case folder: Folder => recomputePaths(folder, prefix = currFolderPath)
        case file: File => file.copy(path = currFolderPath + "/" + file.name)
      })
  }

  def rename(root: Folder, fileOrFolder: FileOrFolder, newName: String): Folder = {
    recomputePaths(
      updateFile(
        root,
        fileOrFolder match {
          case f: File => f.copy(name = newName)
          case f: Folder => f.copy(name = newName)
        }))
  }

  def allFiles(root: Folder): List[File] = {
    root.children.flatMap {
      case f: File => List(f)
      case l: Folder => allFiles(l)
    }
  }

  def updateFile(root: Folder, newFile: FileOrFolder): Folder = {
    root.copy(children = root.children.map {
      case f: FileOrFolder if f.path == newFile.path => newFile
      case f: File => f
      case l: Folder => updateFile(l, newFile)
    })
  }
}

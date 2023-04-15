package com.olegych.scastie.api

import com.olegych.scastie.api.FileOrFolderUtils.prependPath
import play.api.libs.json.{Json, OFormat}

sealed trait FileOrFolder {
  val name: String
  val path: String
  val isFolder: Boolean
  val isRoot: Boolean
}

object FileOrFolder {
  implicit val format: OFormat[FileOrFolder] = Json.format[FileOrFolder]
}


case class File(override val name: String, content: String = "", override val path: String = "") extends FileOrFolder {
  override val isFolder: Boolean = false
  override val isRoot: Boolean = false
}

object File {
  implicit val format: OFormat[File] = Json.format[File]
}

case class Folder(override val name: String, children: List[FileOrFolder] = List(), override val path: String = "", override val isRoot: Boolean = false) extends FileOrFolder {
  override val isFolder: Boolean = true


  def add(ff: FileOrFolder): Folder = {
    if (this.path.nonEmpty) {
      val f = prependPath(this.path, ff)
      this.copy(children = this.children :+ f)
    } else {
      val rootName = name
      val f = if (isRoot) prependPath(rootName, ff) else ff
      this.copy(children = this.children :+ f)
    }
  }

  def add2(path: String, isFolder: Boolean = false): Folder = { //builds intermediate folders
    path.split("/").toList match {
      case Nil => this
      case head :: Nil =>
        val newFilePath = this.path + "/" + head
        val newFile = if (isFolder) Folder(head, path = newFilePath) else File(head, newFilePath)
        copy(children = children :+ newFile)
      case head :: tail =>
        val newIntermediate = Folder(head, path = this.path + "/" + head) // new folder if intermediary 'head' folder does not exist
        val folder = children.find(_.name.equals(head)).getOrElse(newIntermediate).asInstanceOf[Folder]
        copy(children = children :+ folder.add2(tail.mkString("/"), isFolder))
    }
  }

  def isEmpty: Boolean = {
    children.isEmpty
  }

  def take(i: Int): String = {
    children.head.asInstanceOf[File].content.take(i)
  }

  def split(nl: String): Array[String] = {
    children.head.asInstanceOf[File].content.split(nl)
  }

  def childHeadFileContent: String = {
    children.head.asInstanceOf[File].content
  }
}

object Folder {
  implicit val format: OFormat[Folder] = Json.format[Folder]

  def singleton(code: String): Folder = {
    Folder("root", List(File("code.scala", code, "/root/code.scala")), "/root", isRoot = true)
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

  def insert(root: Folder, newFolder: FileOrFolder, path: String): Folder = {
    recomputePaths(
      root.copy(children =
        if (root.path.equals(path)) {
          root.children.filterNot(_.name == newFolder.name) :+ newFolder
        } else
          root.children.map {
            case f: File =>
              if (f.path.equals(path)) throw new IllegalArgumentException("BRUH")
              else f
            case l: Folder => insert(l, newFolder, path)
          }
      ))
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
    f.copy(
      path = prefix + "/" + f.name,
      children = f.children.map {
        case folder: Folder => recomputePaths(folder, prefix + "/" + f.name)
        case file: File => file.copy(path = prefix + "/" + f.name + "/" + file.name)
      })
  }


  // used only for the .add() function
  // move the whole
  def prependPath(p: String, fileOrFolder: FileOrFolder): FileOrFolder = {
    fileOrFolder match {
      case File(name, content, path) =>
        val nonEmptyPath = if (path.isEmpty) name else path // start building the path from the name then put p's one by one to the left with / separators
        File(name, content, p + "/" + nonEmptyPath)
      case Folder(name, files, path, ir) =>
        val nonEmptyPath = if (path.isEmpty) name else path
        Folder(name, files.map(x => prependPath(p + "/" + name, x)), p + "/" + nonEmptyPath)
    }
  }

  def allFiles(root: Folder): List[File] = {
    root.children.flatMap {
      case f: File => List(f)
      case l: Folder => allFiles(l)
    }
  }

  def setFileContent(root: Folder, newFileContent: File): Folder = {
    root.copy(children = root.children.map {
      case f: File if f.path == newFileContent.path => f.copy(content = newFileContent.content)
      case f: File => f
      case l: Folder => setFileContent(l, newFileContent)
    })
  }

  def updateFile(root: Folder, newFile: File): Folder = {
    root.copy(children = root.children.map {
      case f: File if f.path == newFile.path => newFile
      case f: File => f
      case l: Folder => updateFile(l, newFile)
    })
  }
}

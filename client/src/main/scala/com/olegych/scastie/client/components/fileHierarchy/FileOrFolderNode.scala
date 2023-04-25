package com.olegych.scastie.client.components.fileHierarchy

import com.olegych.scastie.api.{File, FileOrFolder, Folder}
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import org.scalajs.dom.window._

/**
 * Provides information about drag over or end.
 *
 * @param end          true if drag ended
 * @param fileOrFolder file or folder that is being dragged (end is true) or dragged over (end is false)
 */
case class DragInfo(end: Boolean, fileOrFolder: FileOrFolder)

/**
 * Node in the file hierarchy view that represents a file or a folder.
 * In case of a folder we can collapse or expand the children
 * It can be dragged around and dropped on another folder
 *
 * @param fileOrFolder       node that we display
 * @param selectedFile       selected file's path in the view hirerarchy
 * @param depth              depth of the node in the hierarchy from root (used to shift the node to the right the right amount)
 * @param selectFile         callback when user clicks on a file
 * @param dragOverOrEnd      callback when user starts making a dragOver a folder or ends dragging a file or folder
 * @param deleteFileOrFolder callback when user deletes a file or folder
 * @param createFileOrFolder callback when user creates a file or folder
 * @param renameFileOrFolder callback when user renames a file or folder (original fileOrFolder, new name)
 */
final case class FileOrFolderNode(fileOrFolder: FileOrFolder,
                                  selectedFile: String,
                                  depth: Int,
                                  selectFile: File => Callback,
                                  dragOverOrEnd: DragInfo => Callback,
                                  deleteFileOrFolder: FileOrFolder => Callback,
                                  createFileOrFolder: FileOrFolder => Callback,
                                  renameFileOrFolder: (FileOrFolder, String) => Callback) {

  @inline def render: VdomElement = FileOrFolderNode.component((fileOrFolder, selectedFile, depth, selectFile, dragOverOrEnd, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder))
}

object FileOrFolderNode {


  val component = ScalaFnComponent.withHooks[(FileOrFolder, String, Int, File => Callback, DragInfo => Callback, FileOrFolder => Callback, FileOrFolder => Callback, (FileOrFolder, String) => Callback)]

    .useState(true) //isExpanded
    .useState(false) //isMouseOver

    .render((props, isExpanded: UseState[Boolean], isMouseOver: UseState[Boolean]) => {
      val (fileOrFolder: FileOrFolder, s, depth, selectFile, dragStartOrEnd, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder) = props

      val fafa = fileOrFolder match {
        case _: File => "file-o"
        case _: Folder => if (isExpanded.value) "folder-open" else "folder"
      }

      val handleClick = (e: ReactMouseEvent) => {
        e.stopPropagation()
        fileOrFolder match {
          case f: File => selectFile(f)
          case f: Folder => isExpanded.modState(x => !x)
        }
      }

      val onDragOver = (e: ReactDragEvent) => {
        if (fileOrFolder.isFolder) {
          isMouseOver.setState(true) >>
            dragStartOrEnd(DragInfo(end = false, fileOrFolder))
        } else Callback.empty
      }

      val onDragEnd = (e: ReactDragEvent) => {
        dragStartOrEnd(DragInfo(end = true, fileOrFolder))
      }

      def handleRename(e: ReactMouseEvent): Callback = {
        e.stopPropagation()
        window.prompt("Enter a new name", fileOrFolder.name) match {
          case null => Callback.empty
          case name => renameFileOrFolder(fileOrFolder, name)
        }
      }

      def handleDelete(e: ReactMouseEvent): Callback = {
        e.stopPropagation()
        window.confirm(s"Are you sure you want to delete ${fileOrFolder.name}?") match {
          case true => deleteFileOrFolder(fileOrFolder)
          case false => Callback.empty
        }
      }

      def handleCreate(e: ReactMouseEvent): Callback = {
        e.stopPropagation()
        val unauthorizedNames = fileOrFolder.asInstanceOf[Folder].children.map(_.name)
        val defaultName = if (unauthorizedNames.contains("file.scala")) {
          var i = 1
          while (unauthorizedNames.contains(s"file${i}.scala")) {
            i = i + 1
          }
          s"file${i}.scala"
        } else {
          "file.scala"
        }
        window.prompt("Enter a name for the new file or folder", defaultName) match {
          case null => Callback.empty
          case name =>
            if (unauthorizedNames.contains(name)) {
              window.alert(s"Name $name is already taken")
              Callback.empty
            } else if (name.contains(".")) {
              createFileOrFolder(File(name, "", fileOrFolder.path + "/" + name))
            } else {
              createFileOrFolder(Folder(name, List(), fileOrFolder.path + "/" + name))
            }
        }
      }

      <.div(
        <.div(
          ^.cls := s"hierarchy-list-row",
          ^.cls := s"${if (fileOrFolder.path.equals(s)) " file-selected" else ""}",
          ^.cls := s"${if (isMouseOver.value) "file-mouse-over" else ""}",
          ^.onClick ==> handleClick,
          ^.draggable := true, // makes the div draggable (it shows a little preview as well)
          ^.onDragEnd ==> onDragEnd,
          ^.onDragOver ==> onDragOver,
          ^.onDragLeave --> isMouseOver.setState(false),

          ^.onMouseOver --> isMouseOver.setState(true),
          ^.onMouseLeave --> isMouseOver.setState(false),
          ^.key := fileOrFolder.path,
          <.div(
            ^.paddingLeft := s"${16 * depth}px",
            <.i(^.className := s"fa fa-${fafa}"),
            fileOrFolder.name
          ),
          <.div(^.cls := "hierarchy-list-row-buttons",
            <.button("✎", ^.onClick ==> handleRename).when(fileOrFolder.path != "/"),
            <.button("⌫", ^.onClick ==> handleDelete).when(fileOrFolder.path != "/"),
            <.button("+", ^.onClick ==> handleCreate).when(fileOrFolder.isFolder)
          )
        ),

        <.div(
          if (isExpanded.value) {
            fileOrFolder match {
              case folder: Folder =>
                folder.children.map {
                  f: FileOrFolder => FileOrFolderNode(f, s, depth + 1, selectFile, dragStartOrEnd, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder).render
                }.toVdomArray
              case _: File => japgolly.scalajs.react.vdom.all.EmptyVdom
            }
          } else {
            japgolly.scalajs.react.vdom.all.EmptyVdom
          }
        )
      )
    })
}
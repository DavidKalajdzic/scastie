package com.olegych.scastie.client.components.fileHierarchy

import com.olegych.scastie.api.{File, FileOrFolder, Folder}
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}

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
 * @param fileOrFolder  node that we display
 * @param selectedFile  selected file's path in the view hirerarchy
 * @param depth         depth of the node in the hierarchy from root (used to shift the node to the right the right amount)
 * @param selectFile    callback when user clicks on a file
 * @param dragOverOrEnd callback when user starts making a dragOver a folder or ends dragging a file or folder
 */
final case class FileOrFolderNode(fileOrFolder: FileOrFolder, selectedFile: String, depth: Int, selectFile: File => Callback, dragOverOrEnd: DragInfo => Callback) {

  @inline def render: VdomElement = FileOrFolderNode.component((fileOrFolder, selectedFile, depth, selectFile, dragOverOrEnd))
}

object FileOrFolderNode {


  val component = ScalaFnComponent.withHooks[(FileOrFolder, String, Int, File => Callback, DragInfo => Callback)]

    .useState(true) //isExpanded
    .useState(false) //isMouseOver

    .render((props, isExpanded: UseState[Boolean], isMouseOver: UseState[Boolean]) => {
      val (fileOrFolder: FileOrFolder, s, depth, selectFile, dragStartOrEnd) = props

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
          )
        ),

        <.div(
          if (isExpanded.value) {
            fileOrFolder match {
              case folder: Folder =>
                folder.children.map {
                  f: FileOrFolder => FileOrFolderNode(f, s, depth + 1, selectFile, dragStartOrEnd).render
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
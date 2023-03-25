package com.olegych.scastie.client.components.fileHierarchy

import japgolly.scalajs.react.{Callback, CtorType, callback, _}
import japgolly.scalajs.react.component.ScalaFn.Component
import japgolly.scalajs.react.hooks.HookCtx
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.all.cls
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document

case class DragInfo(start: Boolean, end: Boolean, f: FileOrFolder)

final case class FileOrFolderNode(file: FileOrFolder, selectedFile: String, depth: Int, selectFile: String => Callback, dragStartOrEnd: DragInfo => Callback) {

  @inline def render: VdomElement = FileOrFolderNode.component((file, selectedFile, depth, selectFile, dragStartOrEnd))
}

object FileOrFolderNode {


  val component = ScalaFnComponent.withHooks[(FileOrFolder, String, Int, String => Callback, DragInfo => Callback)]

    .useState(true) //isExpanded
    .useState(false) //isMouseOver

    .render((props, isExpanded, isMouseOver) => {
      val (file, s, depth, selectFile, dragStartOrEnd) = props

      var fafa = "file-o"
      if (file.isFolder) {
        fafa = "folder"
        if (isExpanded.value) {
          fafa = "folder-open"
        }
      }

      val handleClick = (e: ReactMouseEvent) => {
        e.stopPropagation()
        selectFile(file.name).runNow()
        if (file.isFolder) {
          isExpanded.modState(x => !x).runNow()
        }
        Callback.empty
      }

      val onDragStart = (e: ReactDragEvent) => {
        dragStartOrEnd(DragInfo(start = true, end = false, file))
      }

      val onDragOver = (e: ReactDragEvent) => {
        isMouseOver.setState(true).when(file.isFolder) >>
          dragStartOrEnd(DragInfo(start = false, end = false, file))
      }

      val onDragEnd = (e: ReactDragEvent) => {
        dragStartOrEnd(DragInfo(start = false, end = true, file))
      }

      <.div(
        <.div(
          ^.cls := s"hierarchy-list-row",
          ^.cls := s"${if (file.name.equals(s)) " file-selected" else ""}",
          ^.cls := s"${if (isMouseOver.value) "file-mouse-over" else ""}",
          ^.onClick ==> handleClick,
          ^.draggable := true, // makes the div draggable (it shows a little preview as well)
          ^.onDragStart ==> onDragStart,
          ^.onDragEnd ==> onDragEnd,
          ^.onDragOver ==> onDragOver,
          ^.onDragLeave --> isMouseOver.setState(false),

          ^.onMouseOver --> isMouseOver.setState(true),
          ^.onMouseLeave --> isMouseOver.setState(false),
          ^.key := file.name,
          <.div(
            ^.paddingLeft := s"${16 * depth}px",
            <.i(^.className := s"fa fa-${fafa}"),
            file.name
          )
        ),

        <.div(
          if (isExpanded.value) {
            file match {
              case folder: Folder =>
                folder.files.map {
                  f: FileOrFolder => FileOrFolderNode(f, s, depth + 1, selectFile, dragStartOrEnd).render
                }.toVdomArray
              case _: File => EmptyVdom
            }
          } else {
            EmptyVdom
          }
        )
      )

    })
}
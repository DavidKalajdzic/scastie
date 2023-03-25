package com.olegych.scastie.client.components.fileHierarchy

import japgolly.scalajs.react.{Callback, CtorType, callback, _}
import japgolly.scalajs.react.component.ScalaFn.Component
import japgolly.scalajs.react.hooks.HookCtx
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document


trait FileOrFolder {
  val name: String = ""
  val isFolder: Boolean
}

case class File(override val name: String, content: String = "<empty content>") extends FileOrFolder {
  override val isFolder: Boolean = false
}

case class Folder(override val name: String, files: List[FileOrFolder] = List()) extends FileOrFolder {
  override val isFolder: Boolean = true
}


final case class FileHierarchy(rootFolder: Folder) {
  @inline def render: VdomElement = FileHierarchy.component()
}

object FileHierarchy {

  val initialState: (Folder, String) = {
    (
      Folder("Folder A",
        List(
          File("File A.1"),
          Folder("Folder A.B", List(
            File("File A.B.1"),
            File("File A.B.2"),
            File("FA.B.2"),
            File("File A.B.3")
          )),
          File("File A.2"),
          File("File A.3"),
          Folder("File A.C", List(
            File("File A.C.1")
          ))
        )
      ),
      "File A.B.1") //selection file name
  }


  val component =
    ScalaFnComponent.withHooks[Unit]
      .useState(initialState)
      .render($ => {

        val fn: String => Callback = {
          s => $.hook1.setState($.hook1.value._1, s)
        }
        <.div(
          FileOrFolderNode($.hook1.value._1, $.hook1.value._2, 0, fn).render
        )
      })
}
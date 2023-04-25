package com.olegych.scastie.client.components.fileHierarchy

import com.olegych.scastie.api.{File, FileOrFolder, Folder}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._


/**
 * @param rootFolder         hierarchy to display
 * @param openFile           when user click on a file in this hierarchy this function is called
 * @param moveFileOrFolder   when user moves file or folder this function is called, the string is the destination folder path
 * @param deleteFileOrFolder when user deletes file or folder this function is called
 * @param createFileOrFolder when user creates file or folder this function is called
 * @param renameFileOrFolder when user renames file or folder this function is called, the string is the new name
 */
final case class FileHierarchy(rootFolder: Folder, openFile: File => Callback, moveFileOrFolder: (FileOrFolder, String) => Callback, deleteFileOrFolder: FileOrFolder => Callback, createFileOrFolder: FileOrFolder => Callback, renameFileOrFolder: (FileOrFolder, String) => Callback) {
  @inline def render: VdomElement = FileHierarchy.component((rootFolder, openFile, moveFileOrFolder, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder))
}

object FileHierarchy {

  /**
   * @param selectedFile currently selected file in the hierarchy view
   * @param dragOver     FileOrFolder's path that the user could drop to
   */
  private case class FileHierarchyState(selectedFile: String, dragOver: String)

  private val initialFhs = FileHierarchyState(selectedFile = "", dragOver = "")


  private val component = ScalaFnComponent.withHooks[(Folder, File => Callback, (FileOrFolder, String) => Callback, FileOrFolder => Callback, FileOrFolder => Callback, (FileOrFolder, String) => Callback)]
    .useState(initialFhs)
    .render((props, fhs) => {
      val rootFolder: Folder = props._1
      val openFile: File => Callback = props._2
      val moveFileOrFolder: (FileOrFolder, String) => Callback = props._3
      val deleteFileOrFolder: FileOrFolder => Callback = props._4
      val createFileOrFolder: FileOrFolder => Callback = props._5
      val renameFileOrFolder: (FileOrFolder, String) => Callback = props._6

      val selectFile: File => Callback = {
        (f: File) => openFile(f) >> fhs.modState(_.copy(selectedFile = f.path))
      }

      val dragInfoUpdate: DragInfo => Callback = {
        // Continuously save the dragOver folder's path, so when the user drops the fileOrFolder we know where to move it.
        dragInfo =>
          if (dragInfo.end) {
            val src = dragInfo.fileOrFolder
            val dstPath = fhs.value.dragOver
            if (dstPath.isEmpty) {
              // do nothing if the user never dragged over a folder and just dropped the fileOrFolder on a file
              Callback.empty
            } else {
              moveFileOrFolder(src, dstPath)
              val newSelectedFile = dstPath.stripSuffix("/") + "/" + src.name
              fhs.setState(FileHierarchyState(selectedFile = newSelectedFile, dragOver = ""))
            }
          } else {
            // update the potential destination of the drag
            fhs.modState(_.copy(dragOver = dragInfo.fileOrFolder.path))
          }
      }
      <.div(
        FileOrFolderNode(rootFolder, fhs.value.selectedFile, 0, selectFile, dragInfoUpdate, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder).render
      )
    })
}
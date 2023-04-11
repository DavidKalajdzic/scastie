package com.olegych.scastie.client.components

import com.olegych.scastie.api.Inputs
import com.olegych.scastie.client.View
import com.olegych.scastie.client.components.editor.CodeEditor
import com.olegych.scastie.client.components.fileHierarchy.FileOrFolderUtils.recomputePaths
import com.olegych.scastie.client.components.fileHierarchy.{File, FileHierarchy, FileOrFolder, FileOrFolderUtils, Folder}
import com.olegych.scastie.client.components.mainComp.{MetalsStatus, Scastie, ScastieBackend, ScastieState}
import com.olegych.scastie.client.components.sideBar.SideBar
import com.olegych.scastie.client.components.tabStrip.TabStrip
import com.olegych.scastie.client.components.tabStrip.TabStrip.TabStripState
import japgolly.scalajs.react.{Callback, CtorType, callback, _}
import japgolly.scalajs.react.component.ScalaFn.Component
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.hooks.HookCtx
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.all.{cls, div, p}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import typings.std.global.{length_=, status}

final case class CentralPanel(scope: RenderScope[Scastie, ScastieState, ScastieBackend],
                              props: Scastie,
                              state: ScastieState) {
  @inline def render: VdomElement = CentralPanel.component(scope, props, state)
}


object CentralPanel {
  private val initialRoot: Folder = (
    Folder("root", isRoot = true)
      .add(File("ClientMain.scala", "client main content"))
      .add(File("LocalStorage.scala", "local storage content"))
      .add(Folder("awesomeFolder")
        .add(File("Routing.scala", "rounting content"))
        .add(File("data.txt", "data content"))
      )
      .add(Folder("other"))
    )

  private val initialTabStripState: TabStripState = TabStripState(File("null file"), List())

  val component = ScalaFnComponent.withHooks[(RenderScope[Scastie, ScastieState, ScastieBackend], Scastie, ScastieState)]

    .useState(initialRoot)
    .useState(initialTabStripState)
    .useState("testTEXT")
    .render($ => {
      val (scope: RenderScope[Scastie, ScastieState, ScastieBackend], props: Scastie, state: ScastieState) = $.props.asInstanceOf[(RenderScope[Scastie, ScastieState, ScastieBackend], Scastie, ScastieState)]
      val backend = scope.backend
      val rootFolder: UseState[Folder] = $.hook1
      val tabStrip: UseState[TabStripState] = $.hook2
      val testTEXT: UseState[String] = $.hook3

      val tabStripSelectionChange: File => Callback = {
        (newSelection: File) =>
          tabStrip.modState { case TabStripState(prevSelection, activeTabs) => TabStripState(newSelection, activeTabs) }
      }

      val tabStripCloseTab: File => Callback = {
        (closeAtIndex: File) =>
          tabStrip.modState {
            case TabStripState(prevSelection, activeTabs) =>
              val closeIdx = activeTabs.indexOf(closeAtIndex)
              val newTabs = activeTabs.filterNot(_.path.equals(closeAtIndex.path))
              val newSelection = {
                if (newTabs.isEmpty) {
                  prevSelection
                } else if (newTabs.size <= closeIdx) {
                  newTabs.last
                } else {
                  newTabs(closeIdx)
                }
              }
              TabStripState(newSelection, newTabs)
          }
      }

      val openFile: File => Callback = {
        (f: File) =>
          testTEXT.modState { case s => f.content }.runNow() //TODO continue
          tabStrip.modState { case TabStripState(_, activeTabs) =>
            if (activeTabs.contains(f)) {
              TabStripState(f, activeTabs)
            } else {
              TabStripState(f, activeTabs ::: List(f))
            }
          }
      }


      <.div(cls := "main-grid-central",
        <.div(cls := "side-bar-thin",
          SideBar(
            isDarkTheme = state.isDarkTheme,
            status = state.status,
            inputs = state.inputs,
            toggleTheme = scope.backend.toggleTheme,
            view = scope.backend.viewSnapshot(state.view),
            openHelpModal = scope.backend.openHelpModal,
            openPrivacyPolicyModal = scope.backend.openPrivacyPolicyModal
          ).render.unless(props.isEmbedded || state.isPresentationMode)
        ),
        <.div(cls := "side-pane",
          FileHierarchy(rootFolder.value, openFile).render
        ),
        <.div(cls := "central-pane",
          TabStrip(tabStrip.value, tabStripSelectionChange, tabStripCloseTab).render,
          CodeEditor(
            visible = true, //TODO use backend
            isDarkTheme = state.isDarkTheme,
            isPresentationMode = state.isPresentationMode,
            isWorksheetMode = state.inputs.isWorksheetMode,
            isEmbedded = props.isEmbedded,
            showLineNumbers = state.showLineNumbers,
            value = testTEXT.value, //state.inputs.code,
            attachedDoms = state.attachedDoms,
            instrumentations = state.outputs.instrumentations,
            compilationInfos = state.outputs.compilationInfos,
            runtimeError = state.outputs.runtimeError,
            saveOrUpdate = Reusable.always(Callback.empty), //backend.saveOrUpdate,
            clear = Reusable.always(Callback.empty), //backend.clear,
            openNewSnippetModal = backend.openNewSnippetModal,
            toggleHelp = backend.toggleHelpModal,
            toggleConsole = backend.toggleConsole,
            toggleLineNumbers = backend.toggleLineNumbers,
            togglePresentationMode = backend.togglePresentationMode,
            formatCode = Reusable.always(Callback.empty), //backend.formatCode,
            codeChange = Reusable.always(_ => Callback.empty), //backend.codeChange,
            target = state.inputs.target,
            metalsStatus = state.metalsStatus,
            setMetalsStatus = Reusable.always(_ => Callback.empty), //backend.setMetalsStatus,
            dependencies = state.inputs.libraries
          ).render
        )
      )

    })
}

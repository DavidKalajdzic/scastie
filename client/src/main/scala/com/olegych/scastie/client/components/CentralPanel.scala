package com.olegych.scastie.client.components

import com.olegych.scastie.api.{File, FileOrFolder, FileOrFolderUtils, Folder, Inputs, SnippetId}
import com.olegych.scastie.client.View
import com.olegych.scastie.client.components.editor.CodeEditor
import com.olegych.scastie.api.FileOrFolderUtils.recomputePaths
import com.olegych.scastie.client.components.console.Console
import com.olegych.scastie.client.components.fileHierarchy.FileHierarchy
import com.olegych.scastie.client.components.mainComp.{MetalsStatus, Scastie, ScastieBackend, ScastieState}
import com.olegych.scastie.client.components.projectSettings.BuildSettings
import com.olegych.scastie.client.components.runnersStatus.Status
import com.olegych.scastie.client.components.sideBar.SideBar
import com.olegych.scastie.client.components.snippets.CodeSnippets
import com.olegych.scastie.client.components.tabStrip.TabStrip
import com.olegych.scastie.client.components.tabStrip.TabStrip._
import com.olegych.scastie.client.components.topBarEditor.{EditorTopBar, MobileBar}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.ScalaFn.Component
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.hooks.HookCtx
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.{HtmlStyles, html_<^}
import japgolly.scalajs.react.vdom.all.{cls, div, p}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import typings.std.global.{length_=, status}

final case class CentralPanel(scope: RenderScope[Scastie, ScastieState, ScastieBackend],
                              props: Scastie,
                              state: ScastieState) {
  @inline def render: VdomElement = CentralPanel.component(scope, props, state)
}


object CentralPanel {

  val component = ScalaFnComponent.withHooks[(RenderScope[Scastie, ScastieState, ScastieBackend], Scastie, ScastieState)]
    .render($ => {
      val (scope: RenderScope[Scastie, ScastieState, ScastieBackend], props: Scastie, state: ScastieState) = $
      val backend = scope.backend

      val tabStripSelectionChange: Tab => Callback = {
        (newSelection: Tab) =>
          scope.modState(_.copy(tabStripState = TabStripState(Some(newSelection), state.tabStripState.activeTabs)))
      }

      val tabStripCloseTab: Tab => Callback = {
        (tabToClose: Tab) =>
          scope.modState(_.copy(tabStripState = scope.state.tabStripState match {
            case TabStripState(prevSelection, activeTabs) =>
              val closeIdx = activeTabs.indexOf(tabToClose)
              val newTabs = activeTabs.filterNot(_.equals(tabToClose))
              val newSelection = {
                if (newTabs.isEmpty) None
                else if (newTabs.size <= closeIdx) Some(newTabs.last)
                else Some(newTabs(closeIdx))
              }
              TabStripState(newSelection, newTabs)
          }
          ))
      }

      val openFile: File => Callback = {
        (f: File) =>
          val tab: Tab = Tab.fromFile(f)
          scope.modState((ss: ScastieState, s: Scastie) => {
            val activeTabs = ss.tabStripState.activeTabs
            ss.copy(tabStripState =
              if (activeTabs.contains(tab)) {
                TabStripState(Some(tab), activeTabs)
              } else {
                TabStripState(Some(tab), activeTabs ::: List(tab))
              }
            )
          })
      }

      val moveFileOrFolder: (FileOrFolder, String) => Callback = {
        (f, dstFolderPath) =>
          // move f
          scope.modState((ss: ScastieState, s: Scastie) => {
            val currRoot = ss.inputs.code
            val newRoot = FileOrFolderUtils.move(currRoot, f.path, dstFolderPath)
            ss.setRootFolder(newRoot)
          }).runNow()

          // update tab that had file f opened, or a child in folder f opened
          scope.modState((ss: ScastieState, s: Scastie) => {
            def updateTab(tab: Tab): Tab = {
              if (tab.tabId.equals(f.path))
                tab.copy(tabId = dstFolderPath.stripSuffix("/") + "/" + f.name)
              else tab
            }

            def isInF(tab: Tab): Boolean =
              f.isFolder && FileOrFolderUtils.find(f.asInstanceOf[Folder], tab.tabId).nonEmpty

            ss.copy(tabStripState = ss.tabStripState match {
              case TabStripState(selectedTab, activeTabs) =>
                TabStripState(selectedTab.filterNot(isInF).map(updateTab), activeTabs.filterNot(isInF).map(updateTab))
            })
          }).runNow()
          Callback.empty
      }

      val deleteFileOrFolder: FileOrFolder => Callback = {
        f =>
          // remove f
          scope.modState((ss: ScastieState, s: Scastie) => {
            ss.setRootFolder(FileOrFolderUtils.remove(ss.inputs.code, f.path))
          }).runNow()

          // update tab that had file f opened, or a child in folder f opened
          scope.modState((ss: ScastieState, s: Scastie) => {
            def isForInF(tab: Tab): Boolean =
              f.path.equals(tab.tabId) ||
                f.isFolder && FileOrFolderUtils.find(f.asInstanceOf[Folder], tab.tabId).nonEmpty

            ss.copy(tabStripState = ss.tabStripState match {
              case TabStripState(selectedTab, activeTabs) =>
                TabStripState(selectedTab.filterNot(isForInF), activeTabs.filterNot(isForInF))
            })
          }).runNow()
          Callback.empty
      }

      val createFileOrFolder: FileOrFolder => Callback = {
        f =>
          scope.modState((ss: ScastieState, s: Scastie) => {
            val folder = FileOrFolderUtils.insert(ss.inputs.code, f)
            println(folder.toString)
            ss.setRootFolder(folder)
          }).runNow()

          f match {
            case f: File => openFile(f)
            case _ => Callback.empty
          }
      }

      val renameFileOrFolder: (FileOrFolder, String) => Callback = {
        (f, newName) =>
          scope.modState((ss: ScastieState, s: Scastie) => {
            ss.copy(tabStripState = ss.tabStripState match {
              case TabStripState(selectedTab, activeTabs) =>
                def updateTab(tab: Tab): Tab = {
                  if (tab.tabId.equals(f.path))
                    tab.copy(tabId = f.path.stripSuffix("/" + f.name) + "/" + newName, title = newName)
                  else tab
                }

                TabStripState(selectedTab.map(updateTab), activeTabs.map(updateTab))
            })
          }).runNow()

          scope.modState((ss: ScastieState, s: Scastie) => {
            ss.setRootFolder(FileOrFolderUtils.rename(ss.inputs.code, f, newName))
          }).runNow()

          Callback.empty
      }

      def visible(view: View): Boolean = view == state.view

      def show(view: View) = if (visible(view)) HtmlStyles.display.block else HtmlStyles.display.none

      val codeSnippets = {
        (props.router, state.user) match {
          case (Some(router), Some(user)) if state.view == View.CodeSnippets =>
            <.div(cls := "snippets-container inner-container")(
              CodeSnippets(
                isDarkTheme = state.isDarkTheme,
                view = state.view,
                user = user,
                router = router,
                isShareModalClosed = state.modalState.isShareModalClosed,
                closeShareModal = backend.closeShareModal,
                openShareModal = backend.openShareModal,
                loadProfile = backend.loadProfile,
                deleteSnippet = backend.deleteSnippet
              ).render
            )
          case _ => EmptyVdom
        }
      }

      val statusView = {
        <.div(cls := "status-container inner-container", show(View.Status))(
          props.router match {
            case Some(router) =>
              Status(
                state = state.status,
                router = router,
                isAdmin = state.user.exists(_.isAdmin),
                inputs = state.inputs
              ).render
            case _ => EmptyVdom
          })
      }

      val buildSettings = {
        <.div(cls := "settings-container inner-container", show(View.BuildSettings))(
          BuildSettings(
            visible = visible(View.BuildSettings),
            librariesFrom = state.inputs.librariesFrom,
            isDarkTheme = state.isDarkTheme,
            isBuildDefault = state.isBuildDefault,
            isResetModalClosed = state.modalState.isResetModalClosed,
            scalaTarget = state.inputs.target,
            sbtConfigExtra = state.inputs.sbtConfigExtra,
            sbtConfig = state.inputs.sbtConfigGenerated,
            sbtPluginsConfig = state.inputs.sbtPluginsConfigGenerated,
            setTarget = backend.setTarget,
            closeResetModal = backend.closeResetModal,
            resetBuild = backend.resetBuild,
            openResetModal = backend.openResetModal,
            sbtConfigChange = backend.sbtConfigChange,
            removeScalaDependency = backend.removeScalaDependency,
            updateDependencyVersion = backend.updateDependencyVersion,
            addScalaDependency = backend.addScalaDependency
          ).render
        )
      }

      <.div(cls := "main-grid-central",
        <.div(cls := "centralHFlex",
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
          <.div(cls := "side-pane")(
            if (visible(View.Editor)) {
              FileHierarchy(scope.state.inputs.code, openFile, moveFileOrFolder, deleteFileOrFolder, createFileOrFolder, renameFileOrFolder).render
            } else if (visible(View.BuildSettings)) {
              buildSettings
            } else if (visible(View.Status)) {
              statusView
            } else {
              EmptyVdom
            }
          ).when(visible(View.Editor) || visible(View.BuildSettings) || visible(View.Status))
            .when(if (state.isEmbedded) state.inputs.code.children.length > 1 else true),
          <.div(cls := "central-pane")(
            TabStrip(state.tabStripState, tabStripSelectionChange, tabStripCloseTab).render
              .unless(state.isEmbedded),

            EditorTopBar(
              clear = backend.clear,
              closeNewSnippetModal = backend.closeNewSnippetModal,
              closeEmbeddedModal = backend.closeEmbeddedModal,
              openEmbeddedModal = backend.openEmbeddedModal,
              formatCode = backend.formatCode,
              newSnippet = backend.newSnippet,
              openNewSnippetModal = backend.openNewSnippetModal,
              save = backend.saveOrUpdate,
              toggleWorksheetMode = backend.toggleWorksheetMode,
              router = props.router,
              inputsHasChanged = state.inputsHasChanged,
              isDarkTheme = state.isDarkTheme,
              isNewSnippetModalClosed = state.modalState.isNewSnippetModalClosed,
              isEmbeddedModalClosed = state.modalState.isEmbeddedClosed,
              isRunning = state.isRunning,
              isStatusOk = false,
              snippetId = state.snippetId,
              user = state.user,
              view = backend.viewSnapshot(state.view),
              isWorksheetMode = state.inputs.isWorksheetMode,
              metalsStatus = state.metalsStatus,
              toggleMetalsStatus = backend.toggleMetalsStatus,
              scalaTarget = state.inputs.target
            ).render.unless(props.isEmbedded || state.isPresentationMode),

            CodeEditor(
              visible = visible(View.Editor),
              isDarkTheme = state.isDarkTheme,
              isPresentationMode = state.isPresentationMode,
              isWorksheetMode = state.inputs.isWorksheetMode,
              isEmbedded = props.isEmbedded,
              showLineNumbers = state.showLineNumbers,
              value = backend.selectedFileCode.runNow(),
              attachedDoms = state.attachedDoms,
              instrumentations = state.outputs.instrumentations,
              compilationInfos = backend.compilationInfos.runNow(),
              runtimeError = backend.runtimeError.runNow(),
              saveOrUpdate = backend.saveOrUpdate,
              clear = backend.clear,
              openNewSnippetModal = backend.openNewSnippetModal,
              toggleHelp = backend.toggleHelpModal,
              toggleConsole = backend.toggleConsole,
              toggleLineNumbers = backend.toggleLineNumbers,
              togglePresentationMode = backend.togglePresentationMode,
              formatCode = backend.formatCode,
              codeChange = backend.selectedFileCodeChange,
              target = state.inputs.target,
              metalsStatus = state.metalsStatus,
              setMetalsStatus = backend.setMetalsStatus,
              dependencies = state.inputs.libraries
            ).render,

            Console(
              isOpen = state.consoleState.consoleIsOpen,
              isRunning = state.isRunning,
              isEmbedded = props.isEmbedded,
              consoleOutputs = state.outputs.consoleOutputs,
              run = backend.run,
              setView = backend.setViewReused,
              close = backend.closeConsole,
              open = backend.openConsole
            ).render,

            MobileBar(
              isRunning = state.isRunning,
              isStatusOk = state.status.isSbtOk,
              isDarkTheme = state.isDarkTheme,
              save = backend.saveOrUpdate,
              setView = backend.setViewReused,
              clear = backend.clear,
              isNewSnippetModalClosed = state.modalState.isNewSnippetModalClosed,
              openNewSnippetModal = backend.openNewSnippetModal,
              closeNewSnippetModal = backend.closeNewSnippetModal,
              newSnippet = backend.newSnippet,
              forceDesktop = backend.forceDesktop
            ).render
          ).when(visible(View.Editor) || visible(View.BuildSettings) || visible(View.Status))

          , codeSnippets.when(visible(View.CodeSnippets))
          // , <.p(state.inputs.code.toString)
        )
      )
    })
}

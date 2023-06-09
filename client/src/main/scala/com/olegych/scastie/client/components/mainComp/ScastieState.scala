package com.olegych.scastie.client.components.mainComp

import com.olegych.scastie.api._
import com.olegych.scastie.client._
import com.olegych.scastie.client.components.tabStrip.TabStrip
import com.olegych.scastie.client.components.tabStrip.TabStrip._
import com.olegych.scastie.client.utils.{EventStream, LocalStorage}
import org.scalajs.dom.{HTMLElement, Position => _}
import play.api.libs.json._

sealed trait MetalsStatus {
  val info: String
}

case object MetalsLoading extends MetalsStatus {
  val info: String = "Compiler is loading"
}

case object MetalsReady extends MetalsStatus {
  val info: String = "Metals is ready"
}

case object MetalsDisabled extends MetalsStatus {
  val info: String = "Metals Disabled"
}

case class MetalsConfigurationError(msg: String) extends MetalsStatus {
  val info: String = s"Unsupported Configuration: \n  $msg"
}

case class NetworkError(msg: String) extends MetalsStatus {
  val info: String = s"Network Error: \n  $msg"
}

object SnippetState {
  implicit val formatSnippetState: OFormat[SnippetState] =
    Json.format[SnippetState]
}

case class SnippetState(
                         snippetId: Option[SnippetId],
                         loadSnippet: Boolean,
                         scalaJsContent: Option[String],
                       )

object ScastieState {
  def default(isEmbedded: Boolean): ScastieState = {
    ScastieState(
      view = View.Editor,
      isRunning = false,
      statusStream = None,
      progressStream = None,
      modalState =
        if (isEmbedded) ModalState.allClosed
        else ModalState.default,
      isSidePaneVisible = true,
      isDarkTheme = false,
      isDesktopForced = false,
      isPresentationMode = false,
      showLineNumbers = true,
      tabStripState = TabStripState(None, List()),
      consoleState = ConsoleState.default,
      inputsHasChanged = false,
      snippetState = SnippetState(
        snippetId = None,
        loadSnippet = true,
        scalaJsContent = None,
      ),
      user = None,
      attachedDoms = Map(),
      inputs = Inputs.default,
      outputs = Outputs.default,
      status = StatusState.empty,
      isEmbedded = isEmbedded
    )
  }

  implicit val dontSerializeAttachedDoms: Format[Map[String, HTMLElement]] =
    dontSerialize[Map[String, HTMLElement]](Map())

  implicit val dontSerializeStatusState: Format[StatusState] =
    dontSerialize[StatusState](StatusState.empty)

  implicit val dontSerializeEventStream: Format[EventStream[StatusProgress]] =
    dontSerializeOption[EventStream[StatusProgress]]

  implicit val dontSerializeProgressStream: Format[EventStream[SnippetProgress]] =
    dontSerializeOption[EventStream[SnippetProgress]]

  implicit val dontSerializeMetalsStatus: Format[MetalsStatus] =
    dontSerialize[MetalsStatus](MetalsLoading)

  implicit val formatTabStripState: OFormat[TabStripState] =
    Json.format[TabStripState]

  implicit val formatScastieState: OFormat[ScastieState] =
    Json.format[ScastieState]

}

case class ScastieState(view: View, // which view to display
                        isRunning: Boolean,
                        statusStream: Option[EventStream[StatusProgress]],
                        progressStream: Option[EventStream[SnippetProgress]],
                        modalState: ModalState,
                        isSidePaneVisible: Boolean,
                        isDarkTheme: Boolean,
                        isDesktopForced: Boolean,
                        isPresentationMode: Boolean,
                        showLineNumbers: Boolean,
                        tabStripState: TabStripState,
                        consoleState: ConsoleState,
                        inputsHasChanged: Boolean, // adding a little start in the browser title to indicate that the inputs have changed and is not saved
                        snippetState: SnippetState,
                        user: Option[User],
                        attachedDoms: Map[String, HTMLElement],
                        inputs: Inputs,
                        outputs: Outputs,
                        status: StatusState,
                        metalsStatus: MetalsStatus = MetalsLoading,
                        isEmbedded: Boolean = false,
                        transient: Boolean = false) {
  def snippetId: Option[SnippetId] = snippetState.snippetId

  def loadSnippet: Boolean = snippetState.loadSnippet

  def copyAndSave(attachedDoms: Map[String, HTMLElement] = attachedDoms,
                  view: View = view,
                  isRunning: Boolean = isRunning,
                  statusStream: Option[EventStream[StatusProgress]] = statusStream,
                  progressStream: Option[EventStream[SnippetProgress]] = progressStream,
                  modalState: ModalState = modalState,
                  isDarkTheme: Boolean = isDarkTheme,
                  isPresentationMode: Boolean = isPresentationMode,
                  isDesktopForced: Boolean = isDesktopForced,
                  showLineNumbers: Boolean = showLineNumbers,
                  tabStripState: TabStripState = tabStripState,
                  consoleState: ConsoleState = consoleState,
                  inputsHasChanged: Boolean = inputsHasChanged,
                  snippetId: Option[SnippetId] = snippetId,
                  loadSnippet: Boolean = loadSnippet,
                  scalaJsContent: Option[String] = snippetState.scalaJsContent,
                  user: Option[User] = user,
                  inputs: Inputs = inputs,
                  outputs: Outputs = outputs,
                  status: StatusState = status,
                  metalsStatus: MetalsStatus = metalsStatus,
                  transient: Boolean = transient): ScastieState = {
    val state0 =
      copy(
        view = view,
        isRunning = isRunning,
        statusStream = statusStream,
        progressStream = progressStream,
        modalState = modalState,
        isDarkTheme = isDarkTheme,
        isDesktopForced = isDesktopForced,
        isPresentationMode = isPresentationMode,
        showLineNumbers = showLineNumbers,
        tabStripState = tabStripState,
        consoleState = consoleState,
        inputsHasChanged = inputsHasChanged,
        snippetState = SnippetState(
          snippetId = snippetId,
          loadSnippet = loadSnippet,
          scalaJsContent = scalaJsContent,
        ),
        user = user,
        attachedDoms = attachedDoms,
        inputs = inputs.copy(
          isShowingInUserProfile = false,
          forked = None
        ),
        outputs = outputs,
        status = status,
        metalsStatus = metalsStatus,
        isEmbedded = isEmbedded,
        transient = transient,
      )

    if (!isEmbedded && !transient) {
      LocalStorage.save(state0)
    }

    state0
  }

  private def coalesceUpdates(update: ScastieState => ScastieState) = {
    if (transient) update(this) else update(this.copy(transient = true)).copyAndSave(transient = false)
  }

  def isBuildDefault: Boolean = inputs.isDefault

  def isClearable: Boolean =
    outputs.isClearable

  def run(snippetId: SnippetId): ScastieState = {
    clearOutputs.resetScalajs
      .setRunning(true)
      .logSystem("Sending task to the server.")
      .setSnippetId(snippetId)
  }

  def setRunning(isRunning: Boolean): ScastieState = {
    val openConsole = consoleState.consoleHasUserOutput || outputs.sbtError
    copyAndSave(isRunning = isRunning).autoOpen(openConsole)
  }

  def toggleTheme: ScastieState =
    copyAndSave(isDarkTheme = !isDarkTheme)

  def setTheme(dark: Boolean): ScastieState =
    copyAndSave(isDarkTheme = dark)

  def setMetalsStatus(status: MetalsStatus): ScastieState =
    copyAndSave(metalsStatus = status)

  def toggleMetalsStatus: ScastieState =
    copyAndSave(metalsStatus = if (metalsStatus != MetalsDisabled) MetalsDisabled else MetalsLoading)

  def toggleLineNumbers: ScastieState =
    copyAndSave(showLineNumbers = !showLineNumbers)

  def togglePresentationMode: ScastieState =
    copyAndSave(isPresentationMode = !isPresentationMode)

  def toggleWorksheetMode: ScastieState =
    copyAndSave(
      inputs = inputs.copy(_isWorksheetMode = !inputs.isWorksheetMode),
      inputsHasChanged = true
    )

  def toggleHelpModal: ScastieState =
    copyAndSave(
      modalState = modalState.copy(isHelpModalClosed = !modalState.isHelpModalClosed)
    )

  def togglePrivacyPolicyModal: ScastieState =
    copyAndSave(
      modalState = modalState.copy(isPrivacyPolicyModalClosed = !modalState.isPrivacyPolicyModalClosed)
    )

  def setPrivacyPolicyPromptClosed(status: Boolean): ScastieState =
    copyAndSave(
      modalState = modalState.copy(isPrivacyPolicyPromptClosed = status)
    )

  def setLoginModalClosed(status: Boolean): ScastieState =
    copyAndSave(
      modalState = modalState.copy(isLoginModalClosed = status)
    )

  def openHelpModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isHelpModalClosed = false))

  def openPrivacyPolicyModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isPrivacyPolicyModalClosed = false))

  def closeHelpModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isHelpModalClosed = true))

  def openResetModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isResetModalClosed = false))

  def closeResetModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isResetModalClosed = true))

  def openNewSnippetModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isNewSnippetModalClosed = false))

  def closeNewSnippetModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isNewSnippetModalClosed = true))

  def openShareModal(snippetId: Option[SnippetId]): ScastieState =
    copyAndSave(modalState = modalState.copy(shareModalSnippetId = snippetId))

  def closeShareModal: ScastieState =
    copyAndSave(modalState = modalState.copy(shareModalSnippetId = None))

  def openEmbeddedModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isEmbeddedClosed = false))

  def closeEmbeddedModal: ScastieState =
    copyAndSave(modalState = modalState.copy(isEmbeddedClosed = true))

  def forceDesktop: ScastieState = copyAndSave(isDesktopForced = true)

  def openConsole: ScastieState = {
    copyAndSave(
      consoleState = consoleState.copy(
        consoleIsOpen = true,
        userOpenedConsole = true
      )
    )
  }

  def closeConsole: ScastieState = {
    copyAndSave(
      consoleState = consoleState.copy(
        consoleIsOpen = false,
        userOpenedConsole = false
      )
    )
  }

  def autoOpen(isOpen: Boolean): ScastieState = {
    copyAndSave(
      consoleState = consoleState.copy(
        consoleIsOpen = isOpen || consoleState.consoleIsOpen
      )
    )
  }

  def toggleConsole: ScastieState = {
    copyAndSave(
      consoleState =
        if (consoleState.consoleIsOpen)
          consoleState.copy(
            consoleIsOpen = false,
            userOpenedConsole = false
          )
        else
          consoleState.copy(
            consoleIsOpen = true,
            userOpenedConsole = true
          )
    )
  }

  def setUserOutput: ScastieState = {
    copyAndSave(consoleState = consoleState.copy(consoleHasUserOutput = true))
  }

  def setLoadSnippet(value: Boolean): ScastieState =
    copy(snippetState = snippetState.copy(loadSnippet = value))

  def setUser(user: Option[User]): ScastieState =
    copyAndSave(user = user)

  def changeSelectedFileContent(newContent: String): ScastieState = {
    tabStripState.selectedTab match {
      case Some(Tab(tabId, title)) =>
        val selectedFile = FileOrFolderUtils.find(inputs.code, tabId)
        selectedFile match {
          case Some(file:File) => replaceFile(file.copy(content = newContent))
          case _ => this
        }
      case None =>
        this
    }
  }

  private def replaceFile(f: File): ScastieState = {
    val updatedCode: Folder = FileOrFolderUtils.updateFile(inputs.code, f)
    setRootFolder(updatedCode)
  }

  def setRootFolder(code: Folder): ScastieState = {
    if (inputs.code != code) {
      copyAndSave(
        inputs = inputs.copy(code = code),
        inputsHasChanged = true
      )
    } else {
      this
    }
  }

  def setInputs(inputs: Inputs): ScastieState = {
    copyAndSave(
      inputs = inputs
    )
  }

  def setSbtConfigExtra(config: String): ScastieState =
    copyAndSave(
      inputs = inputs.copy(sbtConfigExtra = config),
      inputsHasChanged = true
    )

  def setChangedInputs: ScastieState =
    copyAndSave(inputsHasChanged = true)

  def setCleanInputs: ScastieState =
    copyAndSave(inputsHasChanged = false)

  def setView(newView: View): ScastieState =
    copyAndSave(view = newView)

  def setTarget(target: ScalaTarget): ScastieState =
    copyAndSave(
      inputs = inputs.modifyConfig(_.copy(target = target)),
      inputsHasChanged = true
    )

  def clearDependencies: ScastieState =
    copyAndSave(
      inputs = inputs.clearDependencies,
      inputsHasChanged = true
    )

  def addScalaDependency(scalaDependency: ScalaDependency, project: Project): ScastieState = {
    val newInputs = inputs.addScalaDependency(scalaDependency, project)
    copyAndSave(
      inputs = newInputs,
      inputsHasChanged = newInputs != inputs,
    )
  }

  def removeScalaDependency(scalaDependency: ScalaDependency): ScastieState =
    copyAndSave(
      inputs = inputs.removeScalaDependency(scalaDependency),
      inputsHasChanged = true
    )

  def updateDependencyVersion(scalaDependency: ScalaDependency, version: String): ScastieState = {
    copyAndSave(
      inputs = inputs.updateScalaDependency(scalaDependency, version),
      inputsHasChanged = true
    )
  }

  def scalaJsScriptLoaded: ScastieState = copyAndSave(scalaJsContent = None)

  def resetScalajs: ScastieState = copyAndSave(attachedDoms = Map())

  def clearOutputs: ScastieState = {
    copyAndSave(
      outputs = Outputs.default,
      consoleState = consoleState.copy(
        consoleHasUserOutput = false
      )
    )
  }

  def clearOutputsPreserveConsole: ScastieState = {
    copyAndSave(
      outputs = Outputs.default.copy(consoleOutputs = outputs.consoleOutputs),
    )
  }

  def closeModals: ScastieState =
    copyAndSave(modalState = ModalState.allClosed)

  def setRuntimeError(runtimeError: Option[RuntimeError]): ScastieState =
    if (runtimeError.isEmpty) this
    else copyAndSave(outputs = outputs.copy(runtimeError = runtimeError))

  def setSbtError(err: Boolean): ScastieState =
    copyAndSave(outputs = outputs.copy(sbtError = err))

  def logOutput(line: Option[ProcessOutput], wrap: ProcessOutput => ConsoleOutput): ScastieState = {
    line match {
      case Some(l) =>
        copyAndSave(
          outputs = outputs.copy(
            consoleOutputs = outputs.consoleOutputs ++ Vector(wrap(l))
          )
        )
      case _ => this
    }
  }

  def logSystem(line: String): ScastieState = {
    copyAndSave(
      outputs = outputs.copy(
        consoleOutputs = outputs.consoleOutputs ++ Vector(
          ConsoleOutput.ScastieOutput(line)
        )
      )
    )
  }

  def addProgress(progress: SnippetProgress): ScastieState = coalesceUpdates { self =>
    val state = self
      .addOutputs(progress.compilationInfos, progress.instrumentations)
      .logOutput(progress.userOutput, ConsoleOutput.UserOutput.apply _)
      .logOutput(progress.sbtOutput, ConsoleOutput.SbtOutput.apply _)
      .setForcedProgramMode(progress.isForcedProgramMode)
      .setRuntimeError(progress.runtimeError)
      .setSbtError(progress.isSbtError)
      .setRunning(!progress.isDone)
      .copyAndSave(scalaJsContent = progress.scalaJsContent.orElse(self.snippetState.scalaJsContent))

    if (progress.userOutput.exists(_.line.nonEmpty)) state.setUserOutput
    else state
  }

  def addStatus(statusUpdate: StatusProgress): ScastieState = {
    statusUpdate match {
      case StatusProgress.KeepAlive =>
        this
      case StatusProgress.Sbt(sbtRunners) =>
        copyAndSave(status = status.copy(sbtRunners = Some(sbtRunners)))
    }
  }

  def removeStatus: ScastieState = {
    copyAndSave(status = StatusState.empty)
  }

  def setProgresses(progresses: List[SnippetProgress]): ScastieState = coalesceUpdates { self =>
    progresses.foldLeft(self) {
      case (state, progress) => state.addProgress(progress)
    }
  }

  def setSnippetId(snippetId: SnippetId): ScastieState = copyAndSave(snippetId = Some(snippetId))

  def selectFirstFile(): ScastieState = {
    val headFile = inputs.code.children.find(!_.isFolder).map(x => Tab.fromFile(x.asInstanceOf[File]))
    copyAndSave(tabStripState = TabStripState(headFile, List(headFile).filter(_.isDefined).map(_.get)))
  }

  def clearSnippetId: ScastieState = copyAndSave(snippetId = None)

  private def info(message: String) = Problem(Info, None, None, message)

  def setForcedProgramMode(forcedProgramMode: Boolean): ScastieState = {
    if (!forcedProgramMode) this
    else {
      copyAndSave(
        outputs = outputs.copy(
          compilationInfos = outputs.compilationInfos +
            info(
              "You don't need a main method (or extends Scastie) in Worksheet Mode"
            )
        )
      )
    }
  }

  def addOutputs(compilationInfos: List[Problem], instrumentations: List[Instrumentation]): ScastieState = {

    def topDef(problem: Problem): Boolean = {
      problem.severity == Error &&
        problem.message == "expected class or object definition"
    }

    val useWorksheetModeTip =
      if (compilationInfos.exists(ci => topDef(ci)))
        if (inputs.target.hasWorksheetMode)
          Set(
            info(
              """|It seems you're writing code without an enclosing class/object.
                 |Switch to Worksheet mode if you want to use scastie more like a REPL.""".stripMargin
            )
          )
        else
          Set(
            info(
              """|It seems you're writing code without an enclosing class/object.
                 |This configuration does not support Worksheet mode.""".stripMargin
            )
          )
      else Set()

    copyAndSave(
      outputs = outputs.copy(
        compilationInfos = outputs.compilationInfos ++ compilationInfos.toSet ++ useWorksheetModeTip,
        instrumentations = outputs.instrumentations ++ instrumentations.toSet
      )
    )
  }

  override def toString: String = Json.toJson(this).toString()
}
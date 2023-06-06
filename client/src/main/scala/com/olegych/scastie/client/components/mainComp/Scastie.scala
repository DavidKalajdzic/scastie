package com.olegych.scastie.client.components.mainComp

import com.olegych.scastie.api._
import com.olegych.scastie.client._
import com.olegych.scastie.client.components.editor.CodeEditor
import com.olegych.scastie.client.components._
import com.olegych.scastie.client.components.fileHierarchy.FileHierarchy
import com.olegych.scastie.client.components.footerBar.FooterBar
import com.olegych.scastie.client.components.modals.{HelpModal, LoginModal, PrivacyPolicyModal, PrivacyPolicyPrompt}
import com.olegych.scastie.client.components.sideBar.SideBar
import com.olegych.scastie.client.components.tabStrip.TabStrip
import com.olegych.scastie.client.components.topBar.TopBar
import com.olegych.scastie.client.utils.LocalStorage
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.HTMLScriptElement

import java.util.UUID

final case class Scastie(router: Option[RouterCtl[Page]],
                         private val scastieId: UUID,
                         private val snippetId: Option[SnippetId],
                         private val oldSnippetId: Option[Int],
                         private val embedded: Option[EmbeddedOptions],
                         private val targetType: Option[ScalaTargetType], // target user can pass from url
                         private val tryLibrary: Option[(ScalaDependency, Project)],
                         private val code: Option[String], // code user can pass from url
                         private val inputs: Option[Inputs] //  inputs user can pass from url // TODO accept old inputs as well, so links are not broken
                        ) {

  @inline def render = Scastie.component(serverUrl, scastieId)(this)

  def serverUrl: Option[String] = embedded.map(_.serverUrl)

  def isEmbedded: Boolean = embedded.isDefined

  //todo not sure how is it different from regular snippet id
  def embeddedSnippetId: Option[SnippetId] = embedded.flatMap(_.snippetId)
}

object Scastie {
  implicit val scastieReuse: Reusability[Scastie] =
    Reusability.derive[Scastie]

  def default(router: RouterCtl[Page]): Scastie =
    Scastie(
      scastieId = UUID.randomUUID(),
      router = Some(router),
      snippetId = None,
      oldSnippetId = None,
      embedded = None,
      targetType = None,
      tryLibrary = None,
      code = None,
      inputs = None,
    )

  private def setTitle(state: ScastieState, props: Scastie) = {
    def scastieCode = if (state.inputs.code.isEmpty) "Scastie" else state.inputs.code + " - Scastie"

    if (!props.isEmbedded) {
      if (state.inputsHasChanged) {
        Callback(dom.document.title = "* " + scastieCode)
      } else {
        Callback(dom.document.title = scastieCode)
      }
    } else {
      Callback(())
    }
  }

  private def render(scope: RenderScope[Scastie, ScastieState, ScastieBackend],
                     props: Scastie,
                     state: ScastieState): VdomElement = {

    val theme = if (state.isDarkTheme) "dark" else "light"

    val forceDesktopClass =
      (cls := "force-desktop").when(state.isDesktopForced)

    val presentationModeClass =
      (cls := "presentation-mode").when(state.isPresentationMode)

    val topBar = TopBar(
      scope.backend.viewSnapshot(state.view),
      state.user,
      scope.backend.openLoginModal
    ).render.unless(props.isEmbedded || state.isPresentationMode)

    val footBar = FooterBar(
      scope.backend.viewSnapshot(state.view),
      scope.backend.openHelpModal,
      scope.backend.openPrivacyPolicyModal
    ).render.unless(props.isEmbedded || state.isPresentationMode)

    div(cls := s"app $theme", forceDesktopClass, presentationModeClass)(
      div(cls := "main-grid-container")(
        div(cls := "main-grid-header")(
          topBar
        ),
        CentralPanel(scope, props, state).render,
        div(cls := "main-grid-footer")(
          footBar
        )
      ),
      EmbeddedOverlay(
        inputsHasChanged = state.inputsHasChanged,
        embeddedSnippetId = props.embeddedSnippetId,
        serverUrl = props.serverUrl,
        save = scope.backend.saveBlocking,
      ).render.when(props.isEmbedded),
      HelpModal(
        isDarkTheme = state.isDarkTheme,
        isClosed = state.modalState.isHelpModalClosed,
        close = scope.backend.closeHelpModal
      ).render,
      LoginModal(
        isDarkTheme = state.isDarkTheme,
        isClosed = state.modalState.isLoginModalClosed,
        close = scope.backend.closeLoginModal,
        openPrivacyPolicyModal = scope.backend.openPrivacyPolicyModal,
      ).render,
      PrivacyPolicyPrompt(
        isDarkTheme = state.isDarkTheme,
        isClosed = state.modalState.isPrivacyPolicyPromptClosed,
        acceptPrivacyPolicy = scope.backend.acceptPolicy,
        refusePrivacyPolicy = scope.backend.refusePrivacyPolicy,
        openPrivacyPolicyModal = scope.backend.openPrivacyPolicyModal,
      ).render,
      PrivacyPolicyModal(
        isDarkTheme = state.isDarkTheme,
        isClosed = state.modalState.isPrivacyPolicyModalClosed,
        close = scope.backend.closePrivacyPolicyModal
      ).render,
    )
  }

  private def start(props: Scastie, backend: ScastieBackend): Callback = {
    val initialState =
      props.embedded match {
        case None => {
          props.snippetId match {
            case Some(snippetId) =>
              backend.loadSnippet(snippetId)

            case None =>
              props.oldSnippetId match {
                case Some(id) =>
                  backend.loadOldSnippet(id)

                case None =>
                  Callback.traverseOption(LocalStorage.load) { state =>
                    backend.scope.modState { _ =>
                      state
                        .setRunning(false)
                        .setCleanInputs
                        .resetScalajs
                    }
                  }
              }
          }
        }
        case Some(embededOptions) => {
          val setInputs =
            (embededOptions.snippetId, embededOptions.inputs) match {
              case (Some(snippetId), _) =>
                backend.loadSnippet(snippetId)

              case (_, Some(inputs)) =>
                backend.scope.modState(_.setInputs(inputs))

              case _ => Callback.empty
            }

          val setTheme =
            embededOptions.theme match {
              case Some("dark") => backend.scope.modState(_.setTheme(dark = true))
              case Some("light") => backend.scope.modState(_.setTheme(dark = false))
              case _ => Callback(())
            }

          setInputs >> setTheme >> backend.scope.modState(_.selectFirstFile())
        }
      }

    initialState >> backend.loadUser
  }

  private def executeScalaJs(scastieId: UUID, state: ScastieState): CallbackTo[Unit] = {
    val scalaJsRunId = "scastie-scalajs-playground-run"

    def createScript(id: String): HTMLScriptElement = {
      val newScript = dom.document
        .createElement("script")
        .asInstanceOf[HTMLScriptElement]
      newScript.id = id
      dom.document.body.appendChild(newScript)
      newScript
    }

    def removeIfExist(id: String): Unit = {
      Option(dom.document.getElementById(id)).foreach { element =>
        element.parentNode.removeChild(element)
      }
    }

    def runScalaJs(): Unit = {
      removeIfExist(scalaJsRunId)
      val scalaJsRunScriptElement = createScript(scalaJsRunId)
      println("== Running Scala.js ==")

      val scalaJsScript =
        s"""|try {
            |  var main = new ScastiePlaygroundMain();
            |  scastie.ClientMain.signal(
            |    main.result,
            |    main.attachedElements,
            |    "$scastieId"
            |  );
            |} catch (e) {
            |  scastie.ClientMain.error(
            |    e,
            |    "$scastieId"
            |  );
            |}""".stripMargin

      scalaJsRunScriptElement.innerHTML = scalaJsScript
    }

    Callback.when(state.snippetId.nonEmpty && !state.isRunning) {
      Callback {
        val scalaJsId = "scastie-scalajs-playground-target"
        removeIfExist(scalaJsId)
        state.snippetState.scalaJsContent.foreach { content =>
          println("== Loading Scala.js! ==")
          val scalaJsScriptElement = createScript(scalaJsId)
          val fixedContent = content.replace("let ScastiePlaygroundMain;", "var ScastiePlaygroundMain;")
          val scriptTextNode = dom.document.createTextNode(fixedContent)
          scalaJsScriptElement.appendChild(scriptTextNode)
          runScalaJs()
        }
      }
    }
  }

  private def component(serverUrl: Option[String], scastieId: UUID) =
    ScalaComponent
      .builder[Scastie]("Scastie")
      .initialStateFromProps { props: Scastie =>
        // we can set those props from the url
        // for example /?target=DOTTY&code=println(42)
        // or /try?g=com.typesafe.play&a=play&v=2.8.12&t=JVM&sv=2.13&o=playframework&r=playframework
        // for more, see TargetTypePage, TryLibraryPage, ...

        // start with the default state
        val state0 = {
          val loadedState = ScastieState.default(props.isEmbedded).copy(inputs = Inputs.default)
          if (props.isEmbedded) {
            loadedState.setCleanInputs.clearOutputs
          } else {
            loadedState
          }
        }

        // modify target
        val state1 = {
          props.targetType match {
            case Some(targetType) => {
              val s = state0.setTarget(targetType.defaultScalaTarget)

              if (targetType == ScalaTargetType.Scala3) {
                s.setRootFolder(Folder.singleton(ScalaTarget.Scala3.defaultCode))
              } else {
                s
              }
            }
            case _ => state0
          }
        }

        // set library dependencies
        val state2 = props.tryLibrary match {
          case Some(dependency) =>
            state1
              .setTarget(dependency._1.target)
              .addScalaDependency(dependency._1, dependency._2)
          case _ => state1
        }

        // modify initial code
        val state3 = props.code match {
          case Some(code) => state2.setRootFolder(Folder.singleton(code))
          case _ => state2
        }

        // modify inputs
        props.inputs match { // the inputs passed from url (in routing see : InputsPage)
          case Some(inputs) => state3.setInputs(inputs)
          case _ => state3
        }
      }
      .backend(ScastieBackend(scastieId, serverUrl, _))
      .renderPS(render)
      .componentWillMount { current =>
        start(current.props, current.backend) >>
          setTitle(current.state, current.props) >>
          current.backend.closeNewSnippetModal >>
          current.backend.closeResetModal >>
          current.backend.connectStatus.when_(!current.props.isEmbedded)
      }
      .componentWillUnmount { current =>
        current.backend.disconnectStatus.when_(!current.props.isEmbedded) >>
          current.backend.unsubscribeGlobal
      }
      .componentDidUpdate { scope =>
        setTitle(scope.prevState, scope.currentProps) >>
          scope.modState(_.scalaJsScriptLoaded) >>
          executeScalaJs(scastieId, scope.currentState)
      }
      .componentWillReceiveProps { scope =>
        val next = scope.nextProps.snippetId
        val current = scope.currentProps.snippetId
        val state = scope.state
        val backend = scope.backend

        val loadSnippet: CallbackOption[Unit] =
          for {
            snippetId <- CallbackOption.option(next)
            _ <- CallbackOption.require(next != current)
            _ <- backend.loadSnippet(snippetId).toCBO >> backend.setView(View.Editor)
          } yield ()

        setTitle(state, scope.nextProps) >> loadSnippet.toCallback
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}

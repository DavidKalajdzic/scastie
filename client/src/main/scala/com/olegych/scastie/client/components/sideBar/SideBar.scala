package com.olegych.scastie.client.components.sideBar

import com.olegych.scastie.api._
import com.olegych.scastie.client.components._
import com.olegych.scastie.client.components.reusableEmpty
import com.olegych.scastie.client.View
import com.olegych.scastie.client.components.mainComp.StatusState
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.all._

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSImport("@resources/images/icon-scastie.png", JSImport.Default)
@js.native
object ScastieLogo extends js.Any

@JSImport("@resources/images/icon-scastie.png", JSImport.Default)
@js.native
object Placeholder extends js.Any

object Assets {
  def logo: String = ScastieLogo.asInstanceOf[String]

  def placeholder: String = Placeholder.asInstanceOf[String]
}

final case class SideBar(isDarkTheme: Boolean,
                         status: StatusState,
                         inputs: Inputs,
                         toggleTheme: Reusable[Callback],
                         view: StateSnapshot[View],
                         openHelpModal: Reusable[Callback],
                         openPrivacyPolicyModal: Reusable[Callback],
                         openSidePane: Reusable[Boolean => Callback]) {
  @inline def render: VdomElement = SideBar.component(this)
}

object SideBar {

  implicit val reusability: Reusability[SideBar] =
    Reusability.derive[SideBar]

  private def render(props: SideBar): VdomElement = {

    val toggleThemeLabel = if (props.isDarkTheme) "Light" else "Dark"
    val selectedIcon = if (props.isDarkTheme) "fa fa-sun-o" else "fa fa-moon-o"

    val themeButton =
      li(onClick --> props.toggleTheme, role := "button", id := "okok", title := s"Select $toggleThemeLabel Theme (F2)", cls := "btn")(
        i(cls := s"fa $selectedIcon")
      )

    val privacyPolicyButton =
      li(onClick --> props.openPrivacyPolicyModal, role := "button", title := "Show privacy policy", cls := "btn")(
        i(cls := "fa fa-user-secret")
      )

    val helpButton =
      li(onClick --> props.openHelpModal, role := "button", title := "Show help Menu", cls := "btn")(
        i(cls := "fa fa-question-circle")
      )

    def buttonsOnClick(v: View): Reusable[Callback] = Reusable.always {
      Callback {
        val toggle = props.view.value == v
        props.openSidePane(toggle).runNow()
      }
    }

    val runnersStatusButton = {
      val (statusIcon, statusClass, statusLabel) =
        props.status.sbtRunnerCount match {
          case None =>
            ("fa-times-circle", "status-unknown", "Unknown")

          case Some(0) =>
            ("fa-times-circle", "status-down", "Down")

          case Some(_) =>
            ("fa-check-circle", "status-up", "Up")
        }

      li(onClick --> props.view.setState(View.Status), role := "button", title := "Show runners status", cls := s"btn $statusClass")(
        i(cls := s"fa $statusIcon")
      )
    }

    val editorButton = ViewToggleButton(
      currentView = props.view,
      forView = View.Editor,
      buttonTitle = "Editor",
      faIcon = "fa-edit",
      onClick = buttonsOnClick(View.Editor)
    ).render

    val buildSettingsButton = ViewToggleButton(
      currentView = props.view,
      forView = View.BuildSettings,
      buttonTitle = "Build Settings",
      faIcon = "fa-gear",
      onClick = buttonsOnClick(View.BuildSettings)
    ).render

    nav(cls := "sidebar")(
      editorButton,
      buildSettingsButton,
      themeButton,
      privacyPolicyButton,
      helpButton,
      runnersStatusButton
    )
  }

  private val component =
    ScalaComponent
      .builder[SideBar]("SideBar")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
}

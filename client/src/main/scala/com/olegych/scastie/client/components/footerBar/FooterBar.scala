package com.olegych.scastie.client.components.footerBar

import com.olegych.scastie.api.User
import com.olegych.scastie.client.View
import com.olegych.scastie.client.components._
import com.olegych.scastie.client.components.sideBar.Assets
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.all.{ul, _}
import org.scalajs.dom

final case class FooterBar(view: StateSnapshot[View],
                           openHelpModal: Reusable[Callback],
                           openPrivacyPolicyModal: Reusable[Callback]) {
  @inline def render: VdomElement = FooterBar.component(this)
}

object FooterBar {

  implicit val reusability: Reusability[FooterBar] =
    Reusability.derive[FooterBar]

  private def render(props: FooterBar): VdomElement = {
    def openInNewTab(link: String): Callback =
      Callback {
        dom.window.open(link, "_blank").focus()
      }

    def github: Callback =
      openInNewTab("https://github.com/scalacenter/scastie")

    val privacyPolicyButton =
      p(onClick --> props.openPrivacyPolicyModal, role := "button", title := "Show privacy policy", cls := "btn")(
        span("Privacy Policy")
      )

    val helpButton =
      p(onClick --> props.openHelpModal, role := "button", title := "Show help Menu", cls := "btn")(
        span("Help")
      )

    val githubButton =
      p(onClick --> github, role := "button", title := "2022 ScalaCenter, Github", cls := "btn")(
        span("2023 ScalaCenter, Github")
      )

    div(cls := "footer-bar")(
      p("About"),
      privacyPolicyButton,
      helpButton,
      githubButton
    )
  }

  private val component = ScalaComponent
    .builder[FooterBar]("FooterBar")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}

package com.olegych.scastie.client.components.topBarEditor

import com.olegych.scastie.client.components._
import com.olegych.scastie.client.components.editor.EditorKeymaps
import com.olegych.scastie.client.components.modals.PromptModal
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._

final case class NewButton(isDarkTheme: Boolean,
                           isNewSnippetModalClosed: Boolean,
                           openNewSnippetModal: Reusable[Callback],
                           closeNewSnippetModal: Reusable[Callback],
                           newSnippet: Reusable[Callback]) {
  @inline def render: VdomElement = NewButton.component(this)
}

object NewButton {
  implicit val reusability: Reusability[NewButton] =
    Reusability.derive[NewButton]

  def render(props: NewButton): VdomElement = {

    li(
      title := s"New code snippet (${EditorKeymaps.openNewSnippetModal.getName})",
      role := "button",
      onClick --> props.openNewSnippetModal,
      cls := "btn"
    )(
      i(cls := "fa fa-file-o"),
      span("New"),
      PromptModal(
        isDarkTheme = props.isDarkTheme,
        modalText = "New Snippet",
        modalId = "new-snippet-modal",
        isClosed = props.isNewSnippetModalClosed,
        close = props.closeNewSnippetModal,
        actionText = "Are you sure you want to create a new snippet ?",
        actionLabel = "New",
        action = props.newSnippet
      ).render
    )
  }

  private val component =
    ScalaComponent
      .builder[NewButton]("NewButton")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build
}

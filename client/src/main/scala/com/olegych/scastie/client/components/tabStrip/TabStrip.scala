package com.olegych.scastie.client.components.tabStrip

import com.olegych.scastie.api.File
import com.olegych.scastie.client.components.tabStrip.TabStrip.TabStripState
import japgolly.scalajs.react.{Callback, CtorType, callback, _}
import japgolly.scalajs.react.component.ScalaFn.Component
import japgolly.scalajs.react.facade.React.useCallback
import japgolly.scalajs.react.hooks.HookCtx
import japgolly.scalajs.react.hooks.Hooks.UseState
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.document
import japgolly.scalajs.react._
import japgolly.scalajs.react.hooks._
import japgolly.scalajs.react.vdom.html_<^._
import play.api.libs.json.{Json, OFormat}

final case class TabStrip(tabStripState: TabStripState, changeSelection: TabStrip.Tab => Callback, closeTab: TabStrip.Tab => Callback) {
  @inline def render: VdomElement = TabStrip.TabStripComponent(tabStripState, changeSelection, closeTab)
}

object TabStrip {

  case class Tab(tabId: String, title: String)

  object Tab {
    def fromFile(file: File): Tab = Tab(file.path, file.name)

    implicit val format: OFormat[Tab] = Json.format[Tab]
  }

  case class TabStripState(selectedTab: Option[Tab], activeTabs: List[Tab])

  private val TabStripComponent = ScalaFnComponent.withHooks[(TabStripState, Tab => Callback, Tab => Callback)]
    .render($ => {
      val tabs: List[Tab] = $._1.activeTabs
      val selectedTab: Option[Tab] = $._1.selectedTab
      val changeSelection = $._2
      val closeTab = $._3

      val handleTabClickCb: Tab => Callback = {
        file => changeSelection(file)
      }

      <.div()(

        <.div(
          ^.className := "tab-strip",
          tabs.map { tab: Tab =>
            renderTab(
              tab.title,
              tab.tabId,
              tab.tabId.equals(selectedTab.getOrElse(Tab("", "")).tabId)
            )(handleTabClickCb(tab), closeTab(tab))
          }.toVdomArray
        ),
        <.p($._1.toString)
      )
    }

    )

  private def renderTab(tabText: String, tabKey: String, isActive: Boolean)(onTabClick: Callback, onTabClose: Callback): VdomElement = {
    <.div(
      ^.key := tabKey,
      ^.className := "tab-strip-item",
      ^.className := (if (isActive) "active" else ""),
      ^.onClick --> onTabClick,
      <.span(tabText),
      <.p(
        ^.onClick ==> { (e: ReactMouseEvent) =>
          e.stopPropagation()
          onTabClose
        },
        ^.className := "close-btn",
        "x")
    )
  }
}

package com.olegych.scastie.client.components.tabStrip

import com.olegych.scastie.client.components.fileHierarchy.File
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

final case class TabStrip(tabs: TabStripState, changeSelection: File => Callback, closeTab: File => Callback) {
  @inline def render: VdomElement = TabStrip.TabStripComponent(tabs, changeSelection, closeTab)
}

object TabStrip {

  case class TabStripState(selectedTab: File, activeTabs: List[File])

  private val TabStripComponent = ScalaFnComponent.withHooks[(TabStripState, File => Callback, File => Callback)]
    .render($ => {
      val tabs: List[File] = $._1.activeTabs
      val selectedTab: File = $._1.selectedTab
      val changeSelection = $._2
      val closeTab = $._3

      val handleTabClickCb: File => Callback = {
        file => changeSelection(file)
      }

      <.div(
        ^.className := "tab-strip",
        tabs.map { file: File =>
          renderTab(
            file.name,
            file.path,
            file.path.equals(selectedTab.path)
          )(handleTabClickCb(file), closeTab(file))
        }.toVdomArray
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

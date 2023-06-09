package com.olegych.scastie.client.utils

import com.olegych.scastie.client.components.mainComp.ScastieState
import org.scalajs.dom
import org.scalajs.dom.window.localStorage
import play.api.libs.json.Json

object LocalStorage {
  private val stateKey = "state"

  def save(state: ScastieState): Unit = {
    localStorage.setItem(stateKey, Json.stringify(Json.toJson(state)))
  }

  def load: Option[ScastieState] = {
    try {
      Option(localStorage.getItem(stateKey))
        .flatMap(raw => Json.fromJson[ScastieState](Json.parse(raw)).asOpt)
    } catch {
      case e: Exception =>
        dom.console.log(e.toString)
        None
    }
  }
}
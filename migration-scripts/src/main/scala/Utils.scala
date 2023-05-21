import com.olegych.scastie.api.{Folder, Project, ScalaDependency, ScalaTarget, SnippetId, SnippetProgress}
import play.api.libs.json.{JsObject, Json, OFormat, Reads}

/**
 * For migration use only.
 * This file holds the data structures that are stored in the database.
 * V1 is the first version of the database, V2 is the second version.
 * V1 had only a String for the inputs, V2 has a proper data structure (Folder hierarchy).
 * */


case class MongoSnippetV1(simpleSnippetId: String,
                          user: Option[String],
                          snippetId: SnippetId,
                          oldId: Long,
                          inputs: InputsV1,
                          progresses: List[SnippetProgress],
                          scalaJsContent: String,
                          scalaJsSourceMapContent: String,
                          time: Long)

object MongoSnippetV1 {
  implicit val formatMongoSnippetV1: OFormat[MongoSnippetV1] = Json.format[MongoSnippetV1]
}

case class MongoSnippetV2(simpleSnippetId: String,
                          user: Option[String],
                          snippetId: SnippetId,
                          oldId: Long,
                          inputs: InputsV2,
                          progresses: List[SnippetProgress],
                          scalaJsContent: String,
                          scalaJsSourceMapContent: String,
                          time: Long)

object MongoSnippetV2 {
  implicit val formatMongoSnippetV2: OFormat[MongoSnippetV2] = Json.format[MongoSnippetV2]
}

object InputsV1 {
  implicit val formatInputsV1: OFormat[InputsV1] = {
    val f = Json.format[InputsV1]
    OFormat(
      Reads { v =>
        f.reads(
          v.asOpt[JsObject]
            .fold(Json.obj())(
              _ ++ Json.obj(
                "_isWorksheetMode" -> (v \ "isWorksheetMode")
                  .asOpt[Boolean]
                  .orElse((v \ "_isWorksheetMode").asOpt[Boolean])
              )
            )
        )
      },
      f
    )
  }
}

case class InputsV1(_isWorksheetMode: Boolean,
                    code: String,
                    target: ScalaTarget,
                    libraries: Set[ScalaDependency],
                    librariesFromList: List[(ScalaDependency, Project)],
                    sbtConfigExtra: String,
                    sbtConfigSaved: Option[String],
                    sbtPluginsConfigExtra: String,
                    sbtPluginsConfigSaved: Option[String],
                    isShowingInUserProfile: Boolean,
                    forked: Option[SnippetId] = None)

object InputsV2 {
  implicit val formatInputsV2: OFormat[InputsV2] = {
    val f = Json.format[InputsV2]
    OFormat(
      Reads { v =>
        f.reads(
          v.asOpt[JsObject]
            .fold(Json.obj())(
              _ ++ Json.obj(
                "_isWorksheetMode" -> (v \ "isWorksheetMode")
                  .asOpt[Boolean]
                  .orElse((v \ "_isWorksheetMode").asOpt[Boolean])
              )
            )
        )
      },
      f
    )
  }
}

case class InputsV2(_isWorksheetMode: Boolean,
                    code: Folder,
                    target: ScalaTarget,
                    libraries: Set[ScalaDependency],
                    librariesFromList: List[(ScalaDependency, Project)],
                    sbtConfigExtra: String,
                    sbtConfigSaved: Option[String],
                    sbtPluginsConfigExtra: String,
                    sbtPluginsConfigSaved: Option[String],
                    isShowingInUserProfile: Boolean,
                    forked: Option[SnippetId] = None)

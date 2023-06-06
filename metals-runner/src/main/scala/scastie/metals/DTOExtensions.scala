package scastie.metals

import java.nio.file.Path
import scala.meta.internal.metals.CompilerOffsetParams
import scala.reflect.internal.util.NoSourceFile

import com.olegych.scastie.api.ScastieOffsetParams

object DTOExtensions {

  extension (offsetParams: ScastieOffsetParams)

    def toOffsetParams: CompilerOffsetParams = {
      val uri = offsetParams.filePath match {
        case "" => Path.of(NoSourceFile.path).toUri
        case path => Path.of(path.substring(1)).toUri
      }


      val ident = "  "
      val wrapperObject = s"""|object worksheet {
                              |$ident""".stripMargin

      val contentToOffset = offsetParams.content.take(offsetParams.offset).linesWithSeparators
      val line            = contentToOffset.size - 1

      val (content, position) =
        if offsetParams.isWorksheetMode then
          val adjustedContent = s"""$wrapperObject${offsetParams.content.replace("\n", "\n" + ident)}}"""
          val adjustedPosition = wrapperObject.length + line * 2 + offsetParams.offset
          (adjustedContent, adjustedPosition)
        else (offsetParams.content, offsetParams.offset)

      new CompilerOffsetParams(uri, content, position)
    }

}

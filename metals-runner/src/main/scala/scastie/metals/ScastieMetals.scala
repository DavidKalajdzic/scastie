package scastie.metals

import cats.Applicative
import cats.syntax.all.*
import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all.*
import com.evolutiongaming.scache.Cache
import com.olegych.scastie.api.*
import org.eclipse.lsp4j.*

import java.nio.file.Path
import scala.concurrent.Await
import scala.meta.internal.metals.CompilerOffsetParams

trait ScastieMetals[F[_]]:

  def setup(request: SetupRequest): EitherT[F, FailureType, List[CompletionList]]

  def complete(request: LSPRequestDTO): EitherT[F, FailureType, CompletionList]

  def completionInfo(request: CompletionInfoRequest): EitherT[F, FailureType, String]

  def hover(request: LSPRequestDTO): EitherT[F, FailureType, Hover]

  def signatureHelp(request: LSPRequestDTO): EitherT[F, FailureType, SignatureHelp]

  def isConfigurationSupported(config: ScastieMetalsOptions): EitherT[F, FailureType, Boolean]

object ScastieMetalsImpl:

  def instance[F[_] : Async](cache: Cache[F, ScastieMetalsOptions, ScastiePresentationCompiler]): ScastieMetals[F] =
    new ScastieMetals[F] {
      private val dispatcher: MetalsDispatcher[F] = new MetalsDispatcher[F](cache)

      def setup(request: SetupRequest): EitherT[F, FailureType, List[CompletionList]] = {
        for {
          compiler <- dispatcher.getCompiler(request.options)
          results <- EitherT.right(request.files.traverse(file => compiler.complete(fileToOffsetParams(file))))
        } yield results
      }

      private def fileToOffsetParams(file: File): ScastieOffsetParams = {
        ScastieOffsetParams(file.content, 0, false, file.path)
      }

      def complete(request: LSPRequestDTO): EitherT[F, FailureType, CompletionList] =
        (dispatcher.getCompiler(request.options) >>= (_.complete(request.offsetParams)))

      def completionInfo(request: CompletionInfoRequest): EitherT[F, FailureType, String] =
        dispatcher.getCompiler(request.options) >>= (_.completionItemResolve(request.completionItem))

      def hover(request: LSPRequestDTO): EitherT[F, FailureType, Hover] =
        dispatcher.getCompiler(request.options).flatMapF(_.hover(request.offsetParams))

      def signatureHelp(request: LSPRequestDTO): EitherT[F, FailureType, SignatureHelp] =
        dispatcher.getCompiler(request.options) >>= (_.signatureHelp(request.offsetParams))

      def isConfigurationSupported(config: ScastieMetalsOptions): EitherT[F, FailureType, Boolean] =
        dispatcher.areDependenciesSupported(config) >>=
          (_ => dispatcher.getCompiler(config).map(_ => true))

    }

package scala.meta.internal.metals

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalafixEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}
import ch.epfl.scala.{bsp4j => b}

import scalafix.interfaces.ScalafixFileEvaluation
import scala.meta.internal.parsing.Trees
import JsonParser._

class ScalafixDiagnosticsProvider(
    val buffers: Buffers,
    val scalafixProvider: () => ScalafixProvider,
    val buildTargets: BuildTargets,
    val trees: Trees,
    val diagnostics: Diagnostics,
    val userConfig: () => UserConfiguration,
) {

  private val ignoredRules =
    Set(ScalafixProvider.organizeImportRuleName, "RemoveUnused")
  // Used for diagnostic.source
  val source = "scalafix"

  def diagnostics(
      paths: Set[AbsolutePath]
  ): Future[Map[AbsolutePath, List[l.Diagnostic]]] =
    if (userConfig().showScalafixDiagnostics) getDiagnostics(paths)
    else Future.successful(Map.empty)

  private def getDiagnostics(
      paths: Set[AbsolutePath]
  ): Future[Map[AbsolutePath, List[l.Diagnostic]]] = {
    val rules =
      scalafixProvider().rulesFromScalafixConf().toList.filterNot(ignoredRules)
    scribe.debug(
      s"Publishing scalafix diagnostics for rules: ${rules.toString}"
    )
    Future
      .traverse(paths) { path =>
        val fut = Future(generateDiagnostics(path, rules))
        fut
          .logError(s"producing scalafix diagnostics for $path")
          .map(diagnostics => (path, diagnostics))
      }
      .map(_.toMap)
  }

  private def generateDiagnostics(
      path: AbsolutePath,
      rules: List[String],
  ): List[l.Diagnostic] =
    if (rules.nonEmpty) {
      val timer = new Timer(Time.system)
      val fileEvals = for {
        buildTarget <- buildTargets.inverseSources(path).toList
        scalaTarget <- buildTargets.scalaTarget(buildTarget).toList
        inBuffers = path.toInputFromBuffers(buffers)
        eval <- scalafixProvider()
          .scalafixEvaluate(
            path,
            scalaTarget,
            inBuffers.value,
            false,
            rules,
          )
          .toOption
          .toList
        fileEval <- eval.getFileEvaluations()
      } yield fileEval

      val allDiagnostics =
        fileEvals.flatMap(actionableDiagnostics) ++ fileEvals
          .flatMap(_.getDiagnostics)
          .flatMap(_.toLsp(source))

      val time = timer.elapsedMillis
      scribe.debug(
        s"Scalafix took ${time}ms for ${path.filename} finding ${allDiagnostics.length} diagnostics"
      )
      allDiagnostics
    } else List.empty

  private def actionableDiagnostics(
      fileEval: ScalafixFileEvaluation
  ): List[l.Diagnostic] =
    fileEval
      .getPatches()
      .toList
      .filter(_.isAtomic)
      .flatMap { patch =>
        patch.toLspDiagnostic.map { diagnostic =>
          val edit = new b.ScalaWorkspaceEdit(
            patch.textEdits().map(_.toScalaTextEdit).toList.asJava
          )
          val bspAction = new b.ScalaAction(diagnostic.getMessage())
          bspAction.setEdit(edit)
          bspAction.setTitle(s"Apply Suggestion: ${diagnostic.getMessage()}")
          val bspDiagnostic = new b.ScalaDiagnostic()
          bspDiagnostic.setActions(List(bspAction).asJava)
          diagnostic.setData(bspDiagnostic.toJsonObject)
          diagnostic
        }
      }
}

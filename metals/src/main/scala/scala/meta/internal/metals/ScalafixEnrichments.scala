package scala.meta.internal.metals

import org.eclipse.{lsp4j => l}
import ch.epfl.scala.{bsp4j => b}

import scalafix.interfaces._
import scala.meta.internal.metals.MetalsEnrichments._

object ScalafixEnrichments {

  implicit class XtensionScalafixEdit(edit: ScalafixTextEdit) {
    def range: l.Range = {
      val startPos =
        new l.Position(
          edit.position().startLine(),
          edit.position().startColumn(),
        )
      val endPos =
        new l.Position(edit.position().endLine(), edit.position().endColumn())
      new l.Range(startPos, endPos)
    }

    def rangeBsp: b.Range = {
      val startPos =
        new b.Position(
          edit.position().startLine(),
          edit.position().startColumn(),
        )
      val endPos =
        new b.Position(edit.position().endLine(), edit.position().endColumn())
      new b.Range(startPos, endPos)
    }

    def toLsp: l.TextEdit = new l.TextEdit(range, edit.newText())
    def toScalaTextEdit: b.ScalaTextEdit =
      new b.ScalaTextEdit(rangeBsp, edit.newText())
  }

  implicit class XtensionScalafixPatch(patch: ScalafixPatch) {
    private val maxTextlength = 50
    private val source = "scalafix"

    def toLspDiagnostic: Option[l.Diagnostic] = {
      val edits = patch.textEdits().toList
      for {
        text <- edits.map(_.newText()).maxByOption(_.length)
        extra = if (text.length > maxTextlength) "..." else ""
        message =
          if (text.isEmpty) "Remove snippet"
          else s"`${text.take(maxTextlength - extra.length) + extra}`"
        // Try not to include imports when calculating the scope of the diagnostic
        importsRemoved = edits.filterNot(_.newText().startsWith("import"))
        filteredEdits = if (importsRemoved.isEmpty) edits else importsRemoved
        (startLine, startCol) <- filteredEdits
          .map(e => (e.position().startLine(), e.position().startColumn()))
          .minOption
        (endLine, endCol) <- filteredEdits
          .map(e => (e.position().endLine(), e.position().endColumn()))
          .maxOption

        diagnostic = new l.Diagnostic(
          new l.Range(
            new l.Position(startLine, startCol),
            new l.Position(endLine, endCol),
          ),
          s"[Scalafix] $message",
          l.DiagnosticSeverity.Warning,
          source,
        )
      } yield diagnostic
    }
  }

  implicit class XtensionScalafixDiagnostic(diagnostic: ScalafixDiagnostic) {
    def toLsp(source: String): Option[l.Diagnostic] =
      // A Scalafix diagnostic may not be associated with a position
      diagnostic.position().asScala.map { pos =>
        val startPos = new l.Position(pos.startLine, pos.startColumn)
        val endPos = new l.Position(pos.endLine, pos.endColumn)
        val range = new l.Range(startPos, endPos)
        val message =
          if (diagnostic.explanation().nonEmpty) diagnostic.explanation()
          else diagnostic.message()
        val identifier =
          diagnostic.lintID().asScala.fold("")(" " + _.ruleName())
        new l.Diagnostic(
          range,
          s"[Scalafix$identifier]: $identifier$message",
          diagnostic.severity() match {
            case ScalafixSeverity.ERROR => l.DiagnosticSeverity.Error
            case ScalafixSeverity.INFO => l.DiagnosticSeverity.Information
            case ScalafixSeverity.WARNING => l.DiagnosticSeverity.Warning
          },
          source,
        )
      }
  }
}

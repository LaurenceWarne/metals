package scala.meta.internal.metals

import scala.concurrent.Future

import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}
import ch.epfl.scala.bsp4j.BuildTargetIdentifier

trait ExternalDiagnosticsProvider {
  def source: String

  def diagnostics(
      buildTargetId: BuildTargetIdentifier
  ): Future[Map[AbsolutePath, List[l.Diagnostic]]]
}

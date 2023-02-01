package tests.scalafix

import scala.concurrent.Future

import scala.meta.internal.metals.ScalafixProvider
import scala.meta.internal.metals.ServerCommands

import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentPositionParams
import tests.BaseLspSuite

class ScalafixDiagnosticsProviderLspSuite
    extends BaseLspSuite("scalafix-diagnostics-provider") {

  def scalafixConf(path: String = "/.scalafix.conf"): String =
    s"""|$path
        |rules = [
        |  OrganizeImports,
        |  ExplicitResultTypes,
        |  RemoveUnused,
        |  RedundantSyntax,
        |  LeakingImplicitClassVal
        |]
        |
        |ExplicitResultTypes.rewriteStructuralTypesToNamedSubclass = false
        |
        |RemoveUnused.imports = false
        |
        |OrganizeImports.groupedImports = Explode
        |OrganizeImports.expandRelative = true
        |OrganizeImports.removeUnused = true
        |OrganizeImports.groups = [
        |  "scala."
        |  "re:javax?\\\\."
        |  "*"
        |]
        |
        |""".stripMargin

  val path = "a/src/main/scala/Main.scala"

  test("base-all") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""/metals.json
           |{"a":{"scalacOptions": ["-Wunused"] }}
           |${scalafixConf()}
           |/$path
           |
           |object Main {
           |  implicit class FooOps(val x: Int) {
           |    def id = s"x"
           |  }
           |}
           |""".stripMargin
      )
      _ <- server.didOpen(path)
      _ <- server.didChange(path)(identity)
      _ <- server.waitForCompile()
      _ <- server.assertCodeAction(
        path,
        """|object Main {
           |  implicit class FooOps(val x: Int) {
           |    <<def id = s"x">>
           |  }
           |}""",
        "[Scalafix] Remove snippet",
        List.empty,
      )
      _ = println(client.workspaceDiagnostics)
      // _ = assertNoDiff(
      //   // we need warnings for scalafix rules
      //   client.workspaceDiagnostics,
      //   """|a/src/main/scala/Main.scala:2:1: warning: Unused import
      //      |import java.io.File
      //      |^^^^^^^^^^^^^^^^^^^
      //      |a/src/main/scala/Main.scala:7:15: warning: private val notUsed in object Main is never used
      //      |  private val notUsed = 123
      //      |              ^^^^^^^
      //      |""".stripMargin,
      // )
      textParams =
        new TextDocumentPositionParams(
          new TextDocumentIdentifier(
            workspace.resolve("a/src/main/scala/Main.scala").toURI.toString()
          ),
          new Position(0, 0),
        )

    } yield ()
  }
}

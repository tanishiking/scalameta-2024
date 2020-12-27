import scala.tools.nsc.{Global, Phase, Settings, SubComponent}
import scala.tools.nsc.reporters.StoreReporter

object Main extends App {
  def compile(
      code: String,
      global: Global = newGlobal()
  ): global.CompilationUnit = {
    val run = new global.Run
    global.reporter.reset()
    val source = global.newSourceFile(code)
    run.compileSources(source :: Nil)
    val unit = run.units.toList.head
    unit
  }

  def newGlobal(): Global = {
    val settings = new Settings()
    // https://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programmatica
    settings.processArgumentString("-usejavacp")
    val reporter = new StoreReporter(settings)
    val g = new Global(settings, reporter)
    new g.Run
    g
  }

  val global = newGlobal()
  import global._

  val code =
    """|object root {
       |  object impl
       |  val f: impl.type => Unit = {}
       |}  
    """.stripMargin

  val unit = compile(code, global)

  object Traverser extends global.Traverser {
    override def traverse(gtree: global.Tree): Unit = {
      gtree match {
        case tt: global.TypeTree if tt.original != null =>
          traverse(tt.original)
        case st: global.SingletonTypeTree =>
          pprint.pprintln(
            (
              global.showRaw(st),
              st.symbol,
              st.ref.symbol
            )
          )
        case t =>
          super.traverse(gtree)
      }
    }
  }
  Traverser.traverse(unit.body)
  // ("SingletonTypeTree(Ident(TermName(\"impl\")))", null, <none>)
  // ("SingletonTypeTree(Ident(TermName(\"impl\")))", null, <none>)
}

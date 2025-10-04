import scala.scalajs.js.annotation.JSExportTopLevel
import typings.bitburnerTypeDefinition.mod.NS

@JSExportTopLevel("main")
def main(ns: NS): Unit = {
  val v = common.squared(10)
  ns.alert(s"Hello World! ${v} piyo")
}

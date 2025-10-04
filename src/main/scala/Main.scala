import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("main")
def hey(args: Any): Unit = {
  println("Hello World")
}


@JSExportTopLevel("Hello")
object Hello {
  @JSExport

  def hello(): Unit = println("hello")
}

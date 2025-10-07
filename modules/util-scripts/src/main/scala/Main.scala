import scala.scalajs.js.annotation.JSExportTopLevel
import typings.bitburnerTypeDefinition.mod.NS
import common.NSWrapper.toast
import util.scanAll
import util.home
import common.NSWrapper.ls
import common.NSWrapper.rm

def deleteAllScripts()(using NS) = {
  for {
    server <- scanAll().filter(_ != home)
    file <- ls(server).filter(_.endsWith(".js"))
  } {
    rm(server, file)
    toast(s"delete file(server: ${server}, file: ${file})")
  }
}

@JSExportTopLevel("main")
def main(ns: NS) = {
  given NS = ns
  toast("Hello")

  ns.args.map(_.toString()).toSeq match {
    case Seq("delete-all-scripts") => deleteAllScripts()
  }
}

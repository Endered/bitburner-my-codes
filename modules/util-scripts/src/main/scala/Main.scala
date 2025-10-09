import common.NSWrapper.alert
import common.NSWrapper.ls
import common.NSWrapper.rm
import common.NSWrapper.toast
import typings.bitburnerTypeDefinition.mod.NS
import util.home
import util.scanAll

import scala.scalajs.js.annotation.JSExportTopLevel
import common.types.HostName
import common.NSWrapper.scan

def deleteAllScripts()(using NS) = {
  for {
    server <- scanAll().filter(_ != home)
    file <- ls(server).filter(_.endsWith(".js"))
  } {
    rm(server, file)
    toast(s"delete file(server: ${server}, file: ${file})")
  }
}

def listAllServers()(using ns: NS) = {
  val servers = scanAll().sortBy(_.value)
  alert(servers.map(_.value).mkString("\n"))
}

def superConnect(target: HostName)(using NS) = {
  val servers = collection.mutable.Map(home -> List("home"))
  val queue = collection.mutable.Queue(home)

  while (!queue.isEmpty) {
    val host = queue.dequeue()
    scan(host).foreach { nhost =>
      if (!servers.contains(nhost)) {
        servers(nhost) = s"connect ${nhost.value}" :: servers(host)
        queue.enqueue(nhost)
      }
    }
  }

  val msg = servers.get(target).map(_.reverse.mkString(";\n")).getOrElse("Not Found!")
  alert(msg)
}

def findCodingContract()(using NS) = {
  val servers = scanAll().filter(ls(_).exists(_.endsWith(".cct")))
  toast(s"found servers: ${servers.length}")
  val msg = servers.mkString("\n")
  alert(msg)
}

@JSExportTopLevel("main")
def main(ns: NS) = {
  given NS = ns

  ns.args.map(_.toString()).toSeq match {
    case Seq("delete-all-scripts")    => deleteAllScripts()
    case Seq("list-all-servers")      => listAllServers()
    case Seq("super-connect", target) => superConnect(HostName(target))
    case Seq("find-coding-contract")  => findCodingContract()
  }
}

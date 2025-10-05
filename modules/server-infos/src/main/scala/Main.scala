import common.types.HostName
import common.types.ServerScore
import typings.bitburnerTypeDefinition.mod.NS
import util.makeServerScore
import util.maxAttackTime
import util.scanAll

import scala.math.Ordering.Reverse
import scala.scalajs.js.annotation.JSExportTopLevel

def isAttackTarget(host: HostName)(using NS) = maxAttackTime(host) < 300 * 1000

@JSExportTopLevel("main")
def main(ns: NS) = {
  given NS = ns
  val servers = scanAll()

  val text = servers
    .map(server => server -> makeServerScore(server))
    .sortBy(x => x._2.totalScore)
    .reverse
    .map { case (server, score) =>
      s"""|this is ${server}
          |money score is: ${score.moneyScore}
          |time score is: ${score.timeScore}
          |total score is: ${score.totalScore}
          |thread is: ${score.threads}
          |
       """.stripMargin
    }
    .mkString("\n")

  ns.alert(text)
}

import Batch.batchAttack
import Batch.filterByBatchable
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.kernel.Monoid
import cats.syntax.all.given
import common.NSWrapper.getHostName
import common.NSWrapper.getPurchasedServers
import common.NSWrapper.getServerMaxMoney
import common.NSWrapper.getServerMinSecurityLevel
import common.NSWrapper.getServerMoneyAvailable
import common.NSWrapper.getServerNumPortsRequired
import common.NSWrapper.getServerSecurityLevel
import common.NSWrapper.growthAnalyze
import common.NSWrapper.hackAnalyzeThreads
import common.NSWrapper.nuke
import common.NSWrapper.scan
import common.NSWrapper.toast
import common.ServerBuyer
import common.ServerEvaluator
import common.ServerHacker
import common.ServerSearcher
import common.cct.CodingContractManager
import common.types.Attack
import common.types.AttackElement
import common.types.Attackings
import common.types.HostName
import common.types.ServerScore
import typings.bitburnerTypeDefinition.mod.NS
import util.allocateThreadsToRequires
import util.alreadyBuyAllServer
import util.attackTo
import util.availableThreadsAt
import util.crackServer
import util.filterByScore
import util.getAttackings
import util.getAvailableRam
import util.getRequiredWeakenThreads
import util.getRequiredWeakenThreadsTo
import util.home
import util.makeServerScore
import util.max
import util.maxAttackTime
import util.maxAttackTimeLessThan
import util.purchaseNewServer
import util.retrieveByKeys
import util.scanAll
import util.upgradeAllPurchasedServerBy2

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.given
import scala.scalajs.js
import scala.scalajs.js.JSConverters.given
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.chaining.given

def backdooredAll()(using NS): Seq[HostName] =
  scanAll()
    .filter(_ != home)
    .filter(crackServer(_))

def makeBackdooredsIO(
    updateInterval: FiniteDuration
)(using NS) = {
  for {
    ref <- Resource.eval(Ref.of[IO, Seq[HostName]](Seq()))
    _ <- (for {
      hosts <- IO(backdooredAll())
      _ <- ref.set(hosts)
      _ <- IO.sleep(updateInterval)
    } yield ()).foreverM.background
  } yield ref.get
}

def isAttackTarget(host: HostName)(using NS) = maxAttackTime(host) < 1200 * 1000

def plannigRequiredAttacks(host: HostName, attack: Attack)(using NS): Attack = {
  def weaken = {
    val threads = getRequiredWeakenThreadsTo(host)
    Attack.withWeaken(max(threads - attack.weaken, 0))
  }

  def hack = {
    val money = Math.max(
      getServerMoneyAvailable(host) - getServerMaxMoney(host) * 0.51,
      0
    )
    val threads = hackAnalyzeThreads(host, money)
    Attack.withHack(max(threads - attack.hack, 0))
  }

  def grow = {
    val threads = growthAnalyze(host, 2)
    Attack.withGrow(max(threads - attack.grow, 0))
  }
  () match {
    case _ if getServerSecurityLevel(host) > getServerMinSecurityLevel(host) + 1 =>
      weaken
    case _ if getServerMoneyAvailable(host) < getServerMaxMoney(host) * 0.95 =>
      grow
    case _ =>
      hack
  }
}

def planningAttack(
    attackers: Seq[HostName],
    hostWithScores: Map[HostName, ServerScore]
)(using NS): List[(HostName, HostName, AttackElement)] = {
  val filteredTargets = hostWithScores.filterKeys(maxAttackTimeLessThan(1200 * 1000)).toMap
  val minScoreToAttack = filteredTargets.values.map(_.totalScore).maxOption.getOrElse(1.0) * 0.1
  val attackTargets =
    filteredTargets.view
      .filterKeys(maxAttackTimeLessThan(1200 * 1000))
      .filter(_._2.totalScore > minScoreToAttack)
      .keys
      .toSeq
  val currentAttackings = attackers.map(getAttackings).combineAll.value
  val availableThreads = attackers.map(host => host -> availableThreadsAt(host))
  val requiredAttacks = for {
    host <- attackTargets
    attack <- plannigRequiredAttacks(
      host,
      currentAttackings.getOrElse(host, Monoid[Attack].empty)
    ).toElements
  } yield (host -> attack, attack.thread)

  allocateThreadsToRequires(availableThreads, requiredAttacks)
    .map { case (attacker, (target, attack), thread) => (attacker, target, attack.withThread(thread)) }
}

def attackOnce(hacker: ServerHacker, evaluator: ServerEvaluator, buyer: ServerBuyer)(using NS) = for {
  attackers <- (hacker.hackedServers, buyer.boughts).tupled.map { case (hacked, boughts) => hacked.toSeq ++ boughts }
  hostWithScores <- evaluator.serverScores
  attacks = planningAttack(attackers, hostWithScores)
  _ = attacks.foreach { case (attacker, target, attackElement) =>
    attackTo(target, attacker, attackElement)
  }
} yield ()

def buyAllServer(using NS): IO[Unit] = {
  def buyOnce = for {
    _ <- IO(purchaseNewServer())
    _ <- IO.sleep(10.seconds)
  } yield ()

  buyOnce.whileM_(IO(!alreadyBuyAllServer()))
}

def upgradeServer(using NS): IO[Nothing] = {
  def upgradeOnce = for {
    _ <- IO(upgradeAllPurchasedServerBy2())
    _ <- IO.sleep(10.seconds)
  } yield ()

  upgradeOnce.foreverM
}

def withInterval(duration: FiniteDuration)[T](io: IO[T]): IO[Nothing] =
  (io >> IO.sleep(duration)).foreverM

def batchAttackOnce(
    backdooredsIO: IO[Seq[HostName]],
    serverHacker: ServerHacker,
    serverBuyer: ServerBuyer,
    serverEvaluator: ServerEvaluator
)(using NS) = for {
  backdooreds <- backdooredsIO
  attackers <- (serverHacker.hackedServers, serverBuyer.boughts).tupled
    .map { case (hackeds, boughts) => hackeds.toSeq ++ boughts }
  hostWithScores <- serverEvaluator.serverScores
  filteredTargets = hostWithScores.filterKeys(maxAttackTimeLessThan(1200 * 1000)).toMap.pipe { hws =>
    retrieveByKeys(
      hws,
      filterByBatchable(attackers, hws.map(_._1).toSeq)
    )
  }
  minScoreToAttack = filteredTargets.map(_._2.totalScore).maxOption.getOrElse(1.0) * 0.1
  targets = filteredTargets.filter(_._2.totalScore > minScoreToAttack).keys.toSeq
  _ = batchAttack(attackers, targets)
} yield ()

def run(using NS): IO[Unit] = (for {
  backdooredsIO <- makeBackdooredsIO(10.seconds)
  serverSearcher <- ServerSearcher.withAutoUpdate(10.seconds)
  serverHacker <- ServerHacker.withAutoUpdate(serverSearcher, 10.seconds)
  serverEvaluator <- ServerEvaluator.withAutoUpdate(serverHacker, 1.seconds)
  serverBuyer <- ServerBuyer.withAutoUpdate(10.seconds)
  codingContractManager <- CodingContractManager.withAutoUpdate(10.seconds)
} yield for {
  _ <- (for
    j1 <- withInterval(1.seconds)(
      attackOnce(serverHacker, serverEvaluator, serverBuyer) >> batchAttackOnce(
        backdooredsIO,
        serverHacker,
        serverBuyer,
        serverEvaluator
      )
    ).start
    _ <- j1.join
  yield ())
} yield ()).use(identity).void

@JSExportTopLevel("main")
def main(ns: NS): js.Promise[Unit] =
  run(using ns).unsafeRunSyncToFuture().toJSPromise

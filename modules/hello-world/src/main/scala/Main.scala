import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.std.Random
import scala.util.chaining.given
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
import common.types.Attack
import common.types.AttackElement
import common.types.Attackings
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import util.alreadyBuyAllServer
import util.attackTo
import util.availableThreadsAt
import util.crackServer
import util.getAttackings
import util.getAvailableRam
import util.getRequiredWeakenThreads
import util.getRequiredWeakenThreadsTo
import util.home
import util.makeServerScore
import util.max
import util.maxAttackTime
import util.purchaseNewServer
import util.scanAll
import util.upgradeAllPurchasedServerBy2

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.given
import scala.scalajs.js
import scala.scalajs.js.JSConverters.given
import scala.scalajs.js.annotation.JSExportTopLevel
import util.filterByScore
import Batch.batchAttack
import Batch.filterByBatchable

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
    hosts: Seq[HostName]
)(using NS): List[(HostName, HostName, AttackElement)] = {
  val attackTargets = hosts.filter(isAttackTarget).pipe(filterByScore(_))
  val attackerHosts = hosts ++ getPurchasedServers()
  val currentAttackings = attackerHosts.map(getAttackings).combineAll.value
  val availableThreads =
    attackerHosts.map(host => host -> availableThreadsAt(host))
  val requiredAttacks = for {
    host <- attackTargets
    attack <- plannigRequiredAttacks(
      host,
      currentAttackings.getOrElse(host, Monoid[Attack].empty)
    ).toElements
  } yield (host -> attack)

  def rec(
      attacker: List[(HostName, Long)],
      targets: List[(HostName, AttackElement)]
  ): List[(HostName, HostName, AttackElement)] = {
    (attacker, targets) match {
      case (Nil, _)                                    => Nil
      case (_, Nil)                                    => Nil
      case ((from, available) :: xs, (to, need) :: ys) =>
        if (available < need.thread) {
          (from, to, need.withThread(available)) :: rec(
            xs,
            (to, need.withThread(need.thread - available)) :: ys
          )
        } else if (available > need.thread) {
          (from, to, need) :: rec((from, available - need.thread) :: xs, ys)
        } else { (from, to, need) :: rec(xs, ys) }
    }
  }

  rec(availableThreads.toList, requiredAttacks.toList)
}

def attackOnce(getHosts: IO[Seq[HostName]])(using NS) = for {
  hosts <- getHosts
  attacks = planningAttack(hosts)
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

def batchAttackOnce(backdooredsIO: IO[Seq[HostName]])(using NS) = for {
  backdooreds <- backdooredsIO
  attackers = backdooreds ++ getPurchasedServers()
  targets = backdooreds.filter(isAttackTarget).pipe(filterByBatchable(attackers, _)).pipe(filterByScore)
  _ = batchAttack(attackers, targets)
} yield ()

def run(using NS): IO[Unit] = (for {
  backdooredsIO <- makeBackdooredsIO(10.seconds)
} yield for {
  _ <- (for
    j1 <- withInterval(10.seconds)(attackOnce(backdooredsIO)).start
    j2 <- buyAllServer.start
    j3 <- upgradeServer.start
    j4 <- withInterval(1.seconds)(batchAttackOnce(backdooredsIO)).start
    _ <- j1.join
    _ <- j2.join
    _ <- j3.join
    _ <- j4.join
  yield ())
} yield ()).use(identity).void

@JSExportTopLevel("main")
def main(ns: NS): js.Promise[Unit] =
  run(using ns).unsafeRunSyncToFuture().toJSPromise

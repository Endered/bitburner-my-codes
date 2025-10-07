import common.NSWrapper.getGrowTime
import common.NSWrapper.getHackTime
import common.NSWrapper.getServerMaxMoney
import common.NSWrapper.getServerMoneyAvailable
import common.NSWrapper.getWeakenTime
import common.NSWrapper.growthAnalyze
import scala.util.chaining.given
import common.NSWrapper.growthAnalyzeSecurity
import common.NSWrapper.hackAnalyzeSecurity
import common.NSWrapper.hackAnalyzeThreads
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import util.getRequiredWeakenThreads
import util.growWithDelayTo
import util.hackWithDelayTo
import util.weakenWithDelayTo
import util.availableThreadsAt
import util.allocateThreadsToRequires
import util.makeServerScore
import common.NSWrapper.toast
import common.NSWrapper.log

object Batch {
  val BatchDelay: Double = 100

  sealed trait DelayedAttack {
    def thread: Long
    def delay: Double

    def withThread(thread: Long) = this match {
      case x @ DelayedHack(_, _)   => x.copy(thread = thread)
      case x @ DelayedGrow(_, _)   => x.copy(thread = thread)
      case x @ DelayedWeaken(_, _) => x.copy(thread = thread)
    }
  }

  case class DelayedHack(override val thread: Long, override val delay: Double) extends DelayedAttack
  case class DelayedGrow(override val thread: Long, override val delay: Double) extends DelayedAttack
  case class DelayedWeaken(
      override val thread: Long,
      override val delay: Double
  ) extends DelayedAttack

  case class BatchAttack(
      grow: DelayedGrow,
      weaken1: DelayedWeaken,
      hack: DelayedHack,
      weaken2: DelayedWeaken
  ) {
    def toElements: Seq[DelayedAttack] = Seq(grow, weaken1, hack, weaken2)
  }

  case class BatchRequiredThreads(
      grow: Long,
      weaken1: Long,
      hack: Long,
      weaken2: Long
  ) {
    def total: Long = grow + weaken1 + hack + weaken2
  }

  def makeBatchRequired(host: HostName)(using NS): BatchRequiredThreads = {
    val currentMoney = getServerMoneyAvailable(host)
    val maxMoney = getServerMaxMoney(host)

    val growThreads = growthAnalyze(host, 2)
    val growSecurity = growthAnalyzeSecurity(host, growThreads)

    val weaken1Threads = getRequiredWeakenThreads(growSecurity * 1.2)

    val growedMoney = Math.min(currentMoney * 2, maxMoney)
    val hackMoney = Math.max(growedMoney - maxMoney * 0.55, 0)
    val hackThreads = hackAnalyzeThreads(host, hackMoney)
    val hackSecurity = hackAnalyzeSecurity(host, hackThreads)

    val weaken2Threads = getRequiredWeakenThreads(hackSecurity * 1.2)

    BatchRequiredThreads(
      grow = growThreads,
      weaken1 = weaken1Threads,
      hack = hackThreads,
      weaken2 = weaken2Threads
    )
  }

  def planingBatchAttack(
      host: HostName,
      requiredThreads: BatchRequiredThreads
  )(using NS): BatchAttack = {
    val hackTime = getHackTime(host)
    val growTime = getGrowTime(host)
    val weakenTime = getWeakenTime(host)

    var growAt = growTime
    var weaken1At = weakenTime
    var hackAt = hackTime
    var weaken2At = weakenTime

    weaken1At = Math.max(weaken1At, growAt + BatchDelay)
    hackAt = Math.max(hackAt, weaken1At + BatchDelay)
    weaken2At = Math.max(weaken2At, hackAt + BatchDelay)

    hackAt = Math.max(hackAt, weaken2At - BatchDelay)
    weaken1At = Math.max(weaken1At, hackAt - BatchDelay)
    growAt = Math.max(growAt, weaken1At - BatchDelay)

    BatchAttack(
      grow = DelayedGrow(thread = requiredThreads.grow, delay = growAt - growTime),
      weaken1 = DelayedWeaken(thread = requiredThreads.weaken1, delay = weaken1At - weakenTime),
      hack = DelayedHack(thread = requiredThreads.hack, delay = hackAt - weakenTime),
      weaken2 = DelayedWeaken(thread = requiredThreads.weaken2, delay = weaken2At - weakenTime)
    )
  }

  def attack(
      target: HostName,
      attacker: HostName,
      batchAttack: BatchAttack
  )(using NS): Unit = {
    hackWithDelayTo(target, attacker, batchAttack.hack.thread, batchAttack.hack.delay)
    weakenWithDelayTo(target, attacker, batchAttack.weaken1.thread, batchAttack.weaken1.delay)
    growWithDelayTo(target, attacker, batchAttack.grow.thread, batchAttack.grow.delay)
    weakenWithDelayTo(target, attacker, batchAttack.weaken2.thread, batchAttack.weaken2.delay)
  }

  def attackTo(target: HostName, attacker: HostName, attack: DelayedAttack)(using NS) =
    attack match {
      case DelayedHack(thread, delay)   => hackWithDelayTo(target, attacker, thread, delay)
      case DelayedGrow(thread, delay)   => growWithDelayTo(target, attacker, thread, delay)
      case DelayedWeaken(thread, delay) => weakenWithDelayTo(target, attacker, thread, delay)
    }

  def allocateThreadsToRequest(availableThreads: Long, requiredThreads: Seq[(HostName, BatchRequiredThreads)])(using
      NS
  ): List[(HostName, BatchRequiredThreads)] =
    requiredThreads
      .foldLeft((availableThreads, Nil): (Long, List[(HostName, BatchRequiredThreads)])) {
        case ((thread, acc), request) =>
          if (thread >= request._2.total) {
            (thread - request._2.total, request :: acc)
          } else {
            (thread, acc)
          }
      }
      ._2

  def batchAttack(attackers: Seq[HostName], targets: Seq[HostName])(using NS) = {
    val attackerThreads = attackers.map(attacker => (attacker -> availableThreadsAt(attacker)))
    val sumAvailableThreads = attackerThreads.map(x => x._2).sum

    val batchRequests = targets
      .map(target => target -> makeBatchRequired(target))
      .pipe(allocateThreadsToRequest(sumAvailableThreads, _))

    val attacks = for {
      (host, request) <- batchRequests
      attack <- planingBatchAttack(host, request).toElements
    } yield ((host, attack), attack.thread)

    val distributedAttacks = allocateThreadsToRequires(attackerThreads, attacks)

    distributedAttacks.foreach { case (attacker, (target, attack), allocatedThreads) =>
      attackTo(target, attacker, attack.withThread(allocatedThreads))
    }
  }

  def filterByBatchable(attackers: Seq[HostName], targets: Seq[HostName])(using NS): Seq[HostName] = {
    val availableThreads = attackers.map(availableThreadsAt).sum
    targets.filter(target => makeBatchRequired(target).total <= availableThreads)
  }
}

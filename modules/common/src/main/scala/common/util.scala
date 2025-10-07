import cats.syntax.all.given
import common.NSWrapper.brutessh
import common.NSWrapper.exec
import common.NSWrapper.fileExists
import common.NSWrapper.ftpcrack
import common.NSWrapper.getGrowTime
import common.NSWrapper.getHackTime
import common.NSWrapper.getHostName
import common.NSWrapper.getPurchasedServerLimit
import common.NSWrapper.getPurchasedServers
import common.NSWrapper.getServerMaxMoney
import common.NSWrapper.getServerMaxRam
import common.NSWrapper.getServerMinSecurityLevel
import common.NSWrapper.getServerNumPortsRequired
import common.NSWrapper.getServerSecurityLevel
import common.NSWrapper.getServerUsedRam
import common.NSWrapper.getWeakenTime
import common.NSWrapper.growthAnalyze
import common.NSWrapper.growthAnalyzeSecurity
import common.NSWrapper.hackAnalyzeSecurity
import common.NSWrapper.hackAnalyzeThreads
import common.NSWrapper.httpworm
import common.NSWrapper.nuke
import common.NSWrapper.ps
import common.NSWrapper.purchaseServer
import common.NSWrapper.relaysmtp
import common.NSWrapper.scan
import common.NSWrapper.scp
import common.NSWrapper.sqlinject
import common.NSWrapper.upgradePurchasedServer
import common.NSWrapper.weakenAnalyze
import common.types.Attack
import common.types.AttackElement
import common.types.AttackElement.GrowElement
import common.types.AttackElement.HackElement
import common.types.AttackElement.WeakenElement
import common.types.Attackings
import common.types.HostName
import common.types.ServerScore
import typings.bitburnerTypeDefinition.mod.NS

import scala.util.chaining.given
import common.NSWrapper.toast

object util {

  val home: HostName = HostName("home")

  val HackScript: String = "hack.js"
  val GrowScript: String = "grow.js"
  val WeakenScript: String = "weaken.js"

  val HackWithDelayScript: String = "hack-with-delay.js"
  val GrowWithDelayScript: String = "grow-with-delay.js"
  val WeakenWithDelayScript: String = "weaken-with-delay.js"

  val purchaseServerBaseName: String = "purchase-my-server"
  def purchaseServerName(id: Long): String = s"${purchaseServerBaseName}-${id}"

  val purchaseServerInitialRam: Double = 4

  // The actual value is 1.75, but I'll make a little margin for error.
  val AttackRamUsage: Double = 1.76

  val crackPrograms: Seq[(String, HostName => NS ?=> Boolean)] = Seq(
    "BruteSSH.exe" -> brutessh,
    "FTPCrack.exe" -> ftpcrack,
    "HTTPWorm.exe" -> httpworm,
    "relaySMTP.exe" -> relaysmtp,
    "SQLInject.exe" -> sqlinject
  )

  def ensureFile(host: HostName, file: String)(using NS): Unit = {
    if (!fileExists(file, host)) {
      scp(file, host, home)
    }
  }

  def executeAttackScript(
      attacker: HostName,
      threads: Long,
      script: String,
      args: Seq[String]
  )(using NS) = {
    if (threads > 0) {
      ensureFile(attacker, script)
      exec(script, attacker, threads, args)
    }
  }

  def hackTo(target: HostName, attacker: HostName, threads: Long)(using NS) =
    executeAttackScript(attacker, threads, HackScript, Seq(target.value))

  def growTo(target: HostName, attacker: HostName, threads: Long)(using NS) =
    executeAttackScript(attacker, threads, GrowScript, Seq(target.value))

  def weakenTo(target: HostName, attacker: HostName, threads: Long)(using NS) =
    executeAttackScript(attacker, threads, WeakenScript, Seq(target.value))

  def attackTo(
      target: HostName,
      attacker: HostName,
      attackElement: AttackElement
  )(using NS) = {
    attackElement match {
      case HackElement(thread)   => hackTo(target, attacker, thread)
      case GrowElement(thread)   => growTo(target, attacker, thread)
      case WeakenElement(thread) => weakenTo(target, attacker, thread)
    }
  }

  def hackWithDelayTo(
      target: HostName,
      attacker: HostName,
      threads: Long,
      delay: Double
  )(using NS) =
    executeAttackScript(
      attacker,
      threads,
      HackWithDelayScript,
      Seq(target.value, delay.toString())
    )

  def growWithDelayTo(
      target: HostName,
      attacker: HostName,
      threads: Long,
      delay: Double
  )(using NS) =
    executeAttackScript(
      attacker,
      threads,
      GrowWithDelayScript,
      Seq(target.value, delay.toString())
    )

  def weakenWithDelayTo(
      target: HostName,
      attacker: HostName,
      threads: Long,
      delay: Double
  )(using NS) =
    executeAttackScript(
      attacker,
      threads,
      WeakenWithDelayScript,
      Seq(target.value, delay.toString())
    )

  def getAvailableRam(target: HostName)(using NS) =
    getServerMaxRam(target) - getServerUsedRam(target)

  def availableThreads(ram: Double): Long =
    Math.floor(ram / AttackRamUsage).toLong

  def availableThreadsAt(hostname: HostName)(using NS): Long =
    getAvailableRam(hostname).pipe(availableThreads)

  def getAttackings(target: HostName)(using NS): Attackings = {
    ps(target)
      .map(x => (x.filename, x.args.toSeq, x.threads.toLong))
      .collect { case (a, Seq(b: String), c) => (a, HostName(b), c) }
      .collect {
        case (HackScript, target, threads) =>
          Attackings.fromAttack(target, Attack.withHack(threads))
        case (GrowScript, target, threads) =>
          Attackings.fromAttack(target, Attack.withGrow(threads))
        case (WeakenScript, target, threads) =>
          Attackings.fromAttack(target, Attack.withWeaken(threads))
      }
      .combineAll
  }

  def bfs[T](initialState: Seq[T])(nextState: T => Seq[T]): Seq[T] = {
    val finds = initialState.to(collection.mutable.Set)
    val queue = initialState.to(collection.mutable.Queue)
    while (queue.size > 0) {
      val p = queue.dequeue()
      nextState(p)
        .filter(!finds.contains(_))
        .foreach { q =>
          finds += q
          queue.enqueue(q)
        }
    }
    finds.toSeq
  }

  def scanAll()(using NS) = bfs(Seq(getHostName())) { scan(_) }

  def getRequiredWeakenThreads(securityLevel: Double)(using NS): Long =
    (securityLevel / weakenAnalyze(1)).pipe(Math.ceil(_)).toLong

  def getRequiredWeakenThreadsTo(host: HostName)(using NS): Long =
    (getServerSecurityLevel(host) - getServerMinSecurityLevel(host))
      .pipe(getRequiredWeakenThreads(_))

  def maxAttackTime(hostname: HostName)(using NS) = {
    getHackTime(hostname)
      .pipe(Math.max(_, getGrowTime(hostname)))
      .pipe(Math.max(_, getWeakenTime(hostname)))
  }

  def min(x: Long, y: Long): Long = if (x < y) { x }
  else { y }

  def max(x: Long, y: Long) = if (x < y) { y }
  else { x }

  def alreadyBuyAllServer()(using NS): Boolean = {
    getPurchasedServers().length == getPurchasedServerLimit()
  }

  def nextServerName()(using NS) =
    HostName(purchaseServerName(getPurchasedServers().length.toLong + 1))

  def purchaseNewServer()(using NS) = {
    val hostname = nextServerName()
    purchaseServer(hostname, purchaseServerInitialRam)
  }

  def upgradePurchasedServerBy2(hostname: HostName)(using NS): Boolean = {
    val upgradedRam = getServerMaxRam(hostname) * 2
    upgradePurchasedServer(hostname, upgradedRam)
  }

  def upgradeAllPurchasedServerBy2()(using NS): Unit = {
    getPurchasedServers().foreach(upgradePurchasedServerBy2)
  }

  def crackServer(host: HostName)(using NS): Boolean = {
    val availables =
      crackPrograms
        .flatMap { case (file, fn) => Option.when(fileExists(file, home))(fn) }

    if (getServerNumPortsRequired(host) <= availables.length) {
      availables.foreach(fn => fn(host))
      nuke(host)
    } else {
      false
    }
  }

  def makeServerScore(hostname: HostName)(using NS) = {
    val money = getServerMaxMoney(hostname) / 2

    val hackThreads = hackAnalyzeThreads(hostname, money / 2)
    val growThreads = growthAnalyze(hostname, 2)

    val increaseSecurityLevel = hackAnalyzeSecurity(
      hostname,
      hackThreads
    ) + growthAnalyzeSecurity(hostname, growThreads)

    val weakenThreads = getRequiredWeakenThreads(increaseSecurityLevel)

    val hackTime = hackThreads * getHackTime(hostname)
    val growTime = growThreads * getGrowTime(hostname)
    val weakenTime = weakenThreads * getWeakenTime(hostname)

    ServerScore(
      money,
      hackTime + growTime + weakenTime,
      hackThreads + growThreads + weakenThreads
    )
  }

  def filterByScore(hosts: Seq[HostName])(using NS): Seq[HostName] = {
    val hostsWithScore = hosts.map(host => host -> makeServerScore(host)).sortBy(x => x._2.totalScore).reverse
    hostsWithScore
      .filter { case (host, score) =>
        hostsWithScore(0)._2.totalScore * 0.1 < score.totalScore
      }
      .map(x => x._1)
  }

  def allocateThreadsToRequires[T](
      availableThreads: Seq[(HostName, Long)],
      threadRequires: Seq[(T, Long)]
  ): List[(HostName, T, Long)] = {
    def rec(availables: List[(HostName, Long)], requires: List[(T, Long)]): List[(HostName, T, Long)] = {
      (availables, requires) match {
        case (Nil, _)                                      => Nil
        case (_, Nil)                                      => Nil
        case ((host, available) :: xs, (t, require) :: ys) =>
          if (available < require) {
            (host, t, available) :: rec(xs, (t, require - available) :: ys)
          } else if (available > require) {
            (host, t, require) :: rec((host, available - require) :: xs, ys)
          } else {
            (host, t, available) :: rec(xs, ys)
          }
      }
    }

    rec(availableThreads.toList, threadRequires.toList)
  }
}

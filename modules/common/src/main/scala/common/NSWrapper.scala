package common

import cats.effect.IO
import cats.effect.IO.fromFuture
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import typings.bitburnerTypeDefinition.mod.ProcessInfo

import scala.util.chaining.given
import common.types.CodingContract
import scala.scalajs.js

object NSWrapper {
  def toast(msg: String)(using ns: NS): Unit = ns.toast(msg)

  def alert(msg: String)(using ns: NS): Unit = ns.alert(msg)

  def getHostName()(using ns: NS): HostName = HostName(ns.getHostname())

  def scan(hostname: HostName)(using ns: NS): Seq[HostName] =
    ns.scan(hostname.value).toSeq.map(HostName(_))

  def hack(hostname: HostName)(using ns: NS): IO[Double] = fromFuture(
    IO(ns.hack(hostname.value).toFuture)
  )

  def weaken(hostname: HostName)(using ns: NS): IO[Double] = fromFuture(
    IO(ns.weaken(hostname.value).toFuture)
  )

  def grow(hostname: HostName)(using ns: NS): IO[Double] = fromFuture(
    IO(ns.grow(hostname.value).toFuture)
  )

  def exec(
      script: String,
      hostname: HostName,
      threads: Long,
      args: Seq[String]
  )(using ns: NS): Double = {
    ns.exec(script, hostname.value, threads.toDouble, args*).toLong
  }

  def scp(file: String, destination: HostName, source: HostName)(using
      ns: NS
  ): Boolean =
    ns.scp(file, destination.value, source.value)

  def fileExists(file: String, hostname: HostName)(using ns: NS): Boolean =
    ns.fileExists(file, hostname.value)

  def getScriptRam(script: String, hostname: HostName)(using ns: NS): Double =
    ns.getScriptRam(script, hostname.value)

  def getServerMaxRam(hostname: HostName)(using ns: NS): Double =
    ns.getServerMaxRam(hostname.value)

  def getServerUsedRam(hostname: HostName)(using ns: NS): Double =
    ns.getServerUsedRam(hostname.value)

  def ps(hostname: HostName)(using ns: NS): Seq[ProcessInfo] =
    ns.ps(hostname.value).toSeq

  def getServerNumPortsRequired(hostname: HostName)(using ns: NS): Long =
    ns.getServerNumPortsRequired(hostname.value).toLong

  def nuke(hostname: HostName)(using ns: NS): Boolean = ns.nuke(hostname.value)

  def growthAnalyze(hostname: HostName, multiplier: Long)(using
      ns: NS
  ): Long =
    ns.growthAnalyze(hostname.value, multiplier.toDouble)
      .pipe(Math.ceil)
      .toLong

  def growthAnalyzeSecurity(hostname: HostName, threads: Long)(using
      ns: NS
  ): Double =
    ns.growthAnalyzeSecurity(threads, hostname.value)

  def hackAnalyze(hostname: HostName)(using ns: NS): Double =
    ns.hackAnalyze(hostname.value)

  def hackAnalyzeChance(hostname: HostName)(using ns: NS): Double =
    ns.hackAnalyzeChance(hostname.value)

  def hackAnalyzeSecurity(hostname: HostName, threads: Long)(using ns: NS) =
    ns.hackAnalyzeSecurity(threads.toDouble, hostname.value)

  def hackAnalyzeThreads(hostname: HostName, hackAmount: Double)(using ns: NS) =
    ns.hackAnalyzeThreads(hostname.value, hackAmount).pipe(Math.ceil).toLong

  def weakenAnalyze(threads: Long)(using ns: NS): Double =
    ns.weakenAnalyze(threads.toLong)

  def getHackTime(hostname: HostName)(using ns: NS): Double =
    ns.getHackTime(hostname.value)

  def getGrowTime(hostname: HostName)(using ns: NS): Double =
    ns.getGrowTime(hostname.value)

  def getWeakenTime(hostname: HostName)(using ns: NS): Double =
    ns.getWeakenTime(hostname.value)

  def getServerMinSecurityLevel(hostname: HostName)(using ns: NS): Double =
    ns.getServerMinSecurityLevel(hostname.value)

  def getServerSecurityLevel(hostname: HostName)(using ns: NS): Double =
    ns.getServerSecurityLevel(hostname.value)

  def getServerMaxMoney(hostname: HostName)(using ns: NS): Double =
    ns.getServerMaxMoney(hostname.value)

  def getServerMoneyAvailable(hostname: HostName)(using ns: NS): Double =
    ns.getServerMoneyAvailable(hostname.value)

  def getPurchasedServerCost(ram: Double)(using ns: NS): Double =
    ns.getPurchasedServerCost(ram)

  def getPurchasedServerLimit()(using ns: NS): Long =
    ns.getPurchasedServerLimit().toLong

  def getPurchasedServerMaxRam()(using ns: NS): Double =
    ns.getPurchasedServerMaxRam()

  def getPurchasedServers()(using ns: NS): Seq[HostName] =
    ns.getPurchasedServers().map(HostName(_)).toSeq

  def getPurchasedServerUpgradeCost(hostname: HostName, ram: Double)(using
      ns: NS
  ): Double = ns.getPurchasedServerUpgradeCost(hostname.value, ram)

  def purchaseServer(hostname: HostName, ram: Double)(using ns: NS): String =
    ns.purchaseServer(hostname.value, ram)

  def upgradePurchasedServer(hostname: HostName, ram: Double)(using
      ns: NS
  ): Boolean =
    ns.upgradePurchasedServer(hostname.value, ram)

  def ftpcrack(hostname: HostName)(using ns: NS): Boolean =
    ns.ftpcrack(hostname.value)

  def httpworm(hostname: HostName)(using ns: NS): Boolean =
    ns.httpworm(hostname.value)

  def relaysmtp(hostname: HostName)(using ns: NS): Boolean =
    ns.relaysmtp(hostname.value)

  def sqlinject(hostname: HostName)(using ns: NS): Boolean =
    ns.sqlinject(hostname.value)

  def brutessh(hostname: HostName)(using ns: NS): Boolean =
    ns.brutessh(hostname.value)

  def ls(hostname: HostName)(using ns: NS): Seq[String] = ns.ls(hostname.value).toSeq

  def rm(hostname: HostName, file: String)(using ns: NS): Boolean = ns.rm(file, hostname.value)

  def log(message: String)(using ns: NS) = ns.print(message)

  def getContractType(contract: CodingContract)(using ns: NS): String =
    ns.codingcontract.getContractType(contract.file, contract.server.value)

  def getData(contract: CodingContract)(using ns: NS): Any =
    ns.codingcontract.getData(contract.file, contract.server.value)

  def attempt(answer: Any, contract: CodingContract)(using ns: NS): Option[String] =
    ns.codingcontract.attempt(answer, contract.file, contract.server.value).pipe(Some.apply).filter(_ != "")

  def createDummyContract(typ: String)(using ns: NS): String =
    ns.codingcontract.createDummyContract(typ)
}

package common

import typings.bitburnerTypeDefinition.mod.NS
import cats.effect.IO
import common.NSWrapper.getPurchasedServers
import common.types.HostName
import util.purchaseNewServer
import scala.util.chaining.given
import util.upgradePurchasedServerBy2
import util.alreadyBuyAllServer
import cats.effect.kernel.Resource
import scala.concurrent.duration.FiniteDuration

class ServerBuyer()(using NS) {
  def boughts: IO[Seq[HostName]] = IO(getPurchasedServers())

  def buyNew: IO[HostName] = IO(purchaseNewServer().pipe(HostName.apply))

  def boughtAllServers: IO[Boolean] = IO(alreadyBuyAllServer())

  def upgrade: IO[Unit] = for {
    servers <- boughts
    _ = servers.foreach(upgradePurchasedServerBy2)
  } yield ()
}

object ServerBuyer {

  def withoutAutoUpdate(using NS) = for {
    buyer <- Resource.pure[IO, ServerBuyer](ServerBuyer())
  } yield buyer

  def withAutoUpdate(delay: FiniteDuration)(using NS) = for {
    buyer <- withoutAutoUpdate
    _ <- (buyer.buyNew >> IO.sleep(delay)).whileM_(buyer.boughtAllServers.map(!_)).background
    _ <- (buyer.upgrade >> IO.sleep(delay)).foreverM.background
  } yield buyer
}

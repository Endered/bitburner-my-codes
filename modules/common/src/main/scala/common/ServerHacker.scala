package common

import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import util.crackServer
import scala.concurrent.duration.FiniteDuration

class ServerHacker(searcher: ServerSearcher, hacked: Ref[IO, Set[HostName]])(using NS) {
  def hackedServers: IO[Set[HostName]] = hacked.get

  def hacking: IO[Set[HostName]] = for {
    servers <- searcher.servers
    currentHacked <- hacked.get
    unhacked = servers -- currentHacked
    additionalHacked = unhacked.filter(crackServer)
    res <- hacked.updateAndGet(_ ++ additionalHacked)
  } yield res
}

object ServerHacker {
  def withoutAutoUpdate(searcher: ServerSearcher)(using NS): Resource[IO, ServerHacker] = for {
    hacked <- Resource.eval(Ref.of[IO, Set[HostName]](Set.empty))
    hacker = ServerHacker(searcher, hacked)
  } yield hacker

  def withAutoUpdate(searcher: ServerSearcher, delay: FiniteDuration)(using NS) = for {
    hacker <- withoutAutoUpdate(searcher)
    _ <- Resource.eval(hacker.hacking)
    _ <- (hacker.hacking >> IO.sleep(delay)).foreverM.background
  } yield hacker
}

package common

import _root_.util.scanAll
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS

import scala.concurrent.duration.FiniteDuration

class ServerSearcher private (foundedServers: Ref[IO, Set[HostName]])(using NS) {
  def servers: IO[Set[HostName]] = foundedServers.get

  def updateServers: IO[Set[HostName]] = {
    val founded = scanAll()
    foundedServers.updateAndGet(_ ++ founded)
  }
}

object ServerSearcher {
  def withoutAutoUpdate(using NS): Resource[IO, ServerSearcher] = for {
    foundedServers <- Resource.eval(Ref.of[IO, Set[HostName]](Set.empty))
    searcher = ServerSearcher(foundedServers)
  } yield searcher

  def withAutoUpdate(delay: FiniteDuration)(using NS): Resource[IO, ServerSearcher] = for {
    searcher <- withoutAutoUpdate
    _ <- Resource.eval(searcher.updateServers)
    _ <- (searcher.updateServers >> IO.sleep(delay)).foreverM.background
  } yield searcher
}

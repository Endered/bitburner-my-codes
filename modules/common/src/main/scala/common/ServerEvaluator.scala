package common

import cats.Functor
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.kernel.Monoid
import cats.kernel.Semigroup
import cats.syntax.all.given
import common.ServerEvaluator.GreaterCombineServerScore
import common.ServerEvaluator.fromCombineable
import common.ServerEvaluator.toCombineable
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import util.makeServerScore

import scala.util.chaining.given

import types.ServerScore
import scala.concurrent.duration.FiniteDuration

class ServerEvaluator(hacker: ServerHacker, scores: Ref[IO, Map[HostName, GreaterCombineServerScore]])(using NS) {

  def serverScores: IO[Map[HostName, ServerScore]] = scores.get.map(_.fmap(fromCombineable))

  def updateScores = for {
    servers <- hacker.hackedServers
    currentScores = servers.map(server => server -> makeServerScore(server)).toMap
    res <- scores.updateAndGet(_.combine(currentScores.fmap(toCombineable)))
  } yield res.fmap(fromCombineable)
}

object ServerEvaluator {
  def withoutAutoUpdate(hacker: ServerHacker)(using NS): Resource[IO, ServerEvaluator] = for {
    scores <- Resource.eval(Ref.of[IO, Map[HostName, GreaterCombineServerScore]](Map.empty))
    evaluator = ServerEvaluator(hacker, scores)
  } yield evaluator

  def withAutoUpdate(hacker: ServerHacker, delay: FiniteDuration)(using NS): Resource[IO, ServerEvaluator] = for {
    evaluator <- withoutAutoUpdate(hacker)
    _ <- Resource.eval(evaluator.updateScores)
    _ <- (evaluator.updateScores >> IO.sleep(delay)).foreverM.background
  } yield evaluator

  opaque type GreaterCombineServerScore = ServerScore
  given Semigroup[GreaterCombineServerScore] {
    def combine(x: GreaterCombineServerScore, y: GreaterCombineServerScore): GreaterCombineServerScore =
      if (x.totalScore < y.totalScore) {
        y
      } else {
        x
      }
  }

  def toCombineable(score: ServerScore): GreaterCombineServerScore = score
  def fromCombineable(score: GreaterCombineServerScore): ServerScore = score
}

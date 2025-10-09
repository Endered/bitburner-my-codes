package cct

import cats.effect.IO
import common.NSWrapper.ls
import common.types.CodingContract
import common.types.HostName
import typings.bitburnerTypeDefinition.mod.NS
import util.scanAll
import common.NSWrapper.getContractType
import common.NSWrapper.getData
import common.NSWrapper.attempt
import common.NSWrapper.toast
import cats.syntax.all.given
import cats.effect.Resource
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import common.NSWrapper.alert

class CodingContractManager()(using NS) {
  def findContracts: IO[Seq[CodingContract]] = IO {
    for {
      server <- scanAll()
      file <- ls(server)
      if file.endsWith(".cct")
    } yield CodingContract(server, file)
  }

  def solveTo(contract: CodingContract): IO[Unit] = IO {
    def solver: PartialFunction[(String, Any), Any] = { case ("Array Jumping Game II", TryConvert[Array[Int]](data)) =>
      ArrayJumpingGameII(data)
    }

    val typ = getContractType(contract)
    val input = getData(contract)

    solver.lift(typ, input).foreach { answer =>
      attempt(answer, contract) match {
        case Some(response) => toast(s"solve a problem: ${response}")
        case None           => alert(s"Failed to solve... type: \"${typ}\" input: ${input}, output: ${answer}")
      }
    }
  }

  def solveAll: IO[Unit] = for {
    contracts <- findContracts
    _ <- contracts.traverse(solveTo)
  } yield ()
}

object CodingContractManager {
  def withoutAutoUpdate(using NS): Resource[IO, CodingContractManager] = Resource.pure(CodingContractManager())

  def withAutoUpdate(delay: FiniteDuration)(using NS) = for {
    solver <- withoutAutoUpdate
    _ <- Resource.eval(solver.solveAll)
    _ <- (solver.solveAll >> IO.sleep(delay)).foreverM.background
  } yield ()
}

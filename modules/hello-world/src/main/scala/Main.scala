import scala.scalajs.js.annotation.JSExportTopLevel
import typings.bitburnerTypeDefinition.mod.NS
import cats.effect.IO
import scala.scalajs.js.JSConverters.given
import cats.effect.unsafe.implicits.global
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.given

def toast(msg: String)(using ns: NS): IO[Unit] = IO.blocking{ns.toast(msg)}

def run(using NS): IO[Unit] = for {
  _ <- (toast("Hello World") >> IO.sleep(500.milliseconds)).replicateA(10)
} yield ()

@JSExportTopLevel("main")
def main(ns: NS):js.Promise[Unit] = run(using ns).unsafeRunSyncToFuture().toJSPromise

import java.io.IOException

import zio._
import zio.Console._

def getUserInput(message: String): IO[IOException, String] =  
  Console.readLine(message)

object RunZIOEffectUsingUnsafeRun extends scala.App {
  val program: ZIO[Any, IOException, Unit] = for {
    name <- getUserInput("What's your name? ")
    _    <- printLine(s"Hello $name, welcome to ZIO!")
  } yield ()
  

  Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(
        program
      ).getOrThrowFiberFailure()
  }
}
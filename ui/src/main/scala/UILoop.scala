import java.lang.Thread.State
import java.io.IOException

import zio._


def gameLoop(oldState: State): IO[IOException, Unit] =
  for {
    guess       <- renderState(oldState) <*> getGuess
    newState    = oldState.addGuess(guess)
    guessResult = analyzeNewGuess(oldState, newState, guess)
    _ <- guessResult match {
          case GuessResult.Won =>
            Console.printLine(s"Congratulations ${newState.name.name}! You won!") <*> renderState(newState)
          case GuessResult.Lost =>
            Console.printLine(s"Sorry ${newState.name.name}! You Lost! Word was: ${newState.word.word}") <*>
              renderState(newState)
          case GuessResult.Correct =>
            Console.printLine(s"Good guess, ${newState.name.name}!") <*> gameLoop(newState)
          case GuessResult.Incorrect =>
            Console.printLine(s"Bad guess, ${newState.name.name}!") <*> gameLoop(newState)
          case GuessResult.Unchanged =>
            Console.printLine(s"${newState.name.name}, You've already tried that letter!") <*> gameLoop(newState)
        }
  } yield ()
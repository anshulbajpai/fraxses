package problems

import org.scalacheck.{ Gen, Shrink }
import org.scalactic.anyvals.PosInt
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.Ordering.Float.TotalOrdering

class MontyHallProblemSpecs
    extends AnyWordSpecLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  val numberOfSimulations = PosInt(100000)

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = numberOfSimulations, workers = 1)

  "monty hall simulation" should {
    "have win rate more than 66% if player switch choice" in {
      var simulationCount = 0f
      var successCount    = 0f
      forAll((montyHallProblemGen, "montyHallProblemGen")) {
        case MontyHallProblemGen(carDoor, originalChoice) =>
          val montyHall     = MontyHallProblem.init(carDoor).chooseDoor(originalChoice)
          val goatDoor      = montyHall.revealGoat()
          val changedChoice = (1 + 2 + 3) - originalChoice - goatDoor
          val hasWon = montyHall.finalChoice(changedChoice).hasWon
          simulationCount = simulationCount + 1
          if (hasWon) {
            successCount = successCount + 1
          }
      }

      val winningProbability = successCount / simulationCount * 100
      winningProbability should be >= 66f
    }

    "have win rate less than 34% if player does not switch choice" in {
      var simulationCount = 0f
      var successCount    = 0f
      forAll((montyHallProblemGen, "montyHallProblemGen")) {
        case MontyHallProblemGen(carDoor, originalChoice) =>
          val montyHall = MontyHallProblem.init(carDoor).chooseDoor(originalChoice)
          val hasWon = montyHall.finalChoice(originalChoice).hasWon
          simulationCount = simulationCount + 1
          if (hasWon) {
            successCount = successCount + 1
          }
      }
      val winningPercentage = successCount / simulationCount * 100
      winningPercentage should be <= 34f
    }
  }

  case class MontyHallProblemGen(carDoor: Int, choosenDoor: Int)

  val doorNumberGen: Gen[Int] = Gen.oneOf(1, 2, 3)
  lazy val montyHallProblemGen: Gen[MontyHallProblemGen] = for {
    carDoor     <- doorNumberGen
    choosenDoor <- doorNumberGen
  } yield MontyHallProblemGen(carDoor, choosenDoor)

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

}

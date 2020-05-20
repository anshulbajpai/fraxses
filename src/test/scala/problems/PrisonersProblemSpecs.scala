package problems

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import problems.PrisonersProblem._

import scala.Ordering.Float.TotalOrdering
import scala.util.Random

class PrisonersProblemSpecs extends AnyWordSpecLike with Matchers {

  "prisoners" should {
    "have around more than 30% chances to survive if they choose index based strategy" in {
      strategyPercentage(IndexStrategy) should be >= 30f
    }

    "have 0% chance to survive if they choose random strategy" in {
      strategyPercentage(RandomStrategy) should be(0f)
    }

    def strategyPercentage(prisonerStrategy: PrisonerStrategy): Float = {
      val prisonersCount  = 100
      val maxChoices      = 50
      val simulationCount = 100000
      var successCount    = 0f
      (1 to simulationCount).map(_ => Random.shuffle((1 to prisonersCount).toList)).foreach {
        drawers =>
          val strategicChoiceResult = executeStrategy(drawers.toArray, maxChoices, prisonerStrategy)

          if (strategicChoiceResult) {
            successCount = successCount + 1
          }
      }
      successCount / simulationCount * 100
    }
  }

}

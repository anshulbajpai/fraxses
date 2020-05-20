package problems

import scala.annotation.tailrec
import scala.util.Random

object PrisonersProblem {

  sealed trait PrisonerStrategy {
    def apply(drawers: Array[Int], prisonerNumber: Int, maxChoices: Int): Boolean
  }

  case object RandomStrategy extends PrisonerStrategy {
    override def apply(drawers: Array[Int], prisonerNumber: Int, maxChoices: Int): Boolean = {
      val shuffledDrawers = Random.shuffle(drawers)
      @tailrec
      def loop(nextChoiceIndex: Int): Boolean =
        if (nextChoiceIndex == maxChoices) false
        else if (shuffledDrawers(nextChoiceIndex) == prisonerNumber) true
        else loop(nextChoiceIndex + 1)
      loop(nextChoiceIndex = 0)
    }
  }

  case object IndexStrategy extends PrisonerStrategy {
    override def apply(drawers: Array[Int], prisonerNumber: Int, maxChoices: Int): Boolean = {
      @tailrec
      def loop(nextChoiceIndex: Int, choiceCount: Int): Boolean =
        if (choiceCount > maxChoices) false
        else if (drawers(nextChoiceIndex) == prisonerNumber) true
        else loop(drawers(nextChoiceIndex) - 1, choiceCount + 1)
      loop(prisonerNumber - 1, choiceCount = 1)
    }
  }

  def executeStrategy(drawers: Array[Int], maxChoices: Int, strategy: PrisonerStrategy): Boolean =
    (1 to drawers.length).toList
      .forall(strategy(drawers, _, maxChoices))

}

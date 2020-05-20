package problems

import problems.MontyHallProblem.MontyHall.Init

import scala.util.Random

object MontyHallProblem {

  sealed trait MontyHall

  object MontyHall {
    class Init(carDoor: Int) extends MontyHall {
      def chooseDoor(chosenDoor: Int) = new DoorChosen(carDoor, chosenDoor)
    }
    class DoorChosen(carDoor: Int, chosenDoor: Int) extends MontyHall {
      private val allDoors = Set(1, 2, 3)
      def revealGoat(): Int =
        if (carDoor == chosenDoor) {
          Random.shuffle(allDoors - carDoor).head
        } else {
          (allDoors - carDoor - chosenDoor).head
        }
      def finalChoice(chosenDoor: Int) = new Final(carDoor, chosenDoor)
    }
    class Final(carDoor: Int, chosenDoor: Int) extends MontyHall {
      lazy val hasWon: Boolean = carDoor == chosenDoor
    }
  }

  def init(carDoorNumber: Int): Init = new Init(carDoorNumber)
}

import scala.util.Random
import time.Time; import Time._

class Elevator1 extends Simulation {

  type ListBuffer = scala.collection.mutable.ListBuffer[Person]

  var num = 20;

  def initialize():Unit = {
    val elevator = new Elevator(num)
    //val intervals = new PoissonDistribution(5.0).scale(SEC).toInt.map(Time(_))
    for (i <- 1 to num) {
      elevator.waitingFloors(i) = new ListBuffer()
      elevator.clickedFloors(i) = new ListBuffer()
    }
    def butler(person:Int):Unit = {
        //after(intervals.next) {
        after(secs(5)) {
          val start = Random.nextInt(num)+1
          val target = Random.nextInt(num)+1
          wait(elevator, new Person(s"Person$person", start, target))
          butler(person+1)
        }
    }
    butler(1)
    elevator.runElevator(1)
  }

  override def log(message: Any): Unit = println(s"@$now: $message")

  class Elevator(numFloors:Int) {
    var waitingFloors = new Array[ListBuffer](numFloors+1)
    var clickedFloors = new Array[ListBuffer](numFloors+1)
    var direction = 1
    var floor = 1

    def runElevator(f:Int):Unit = {
      log(s"Elevator is at floor $f")


      var move = false

      var f1 = f; var d = direction; var f2 = f
      if (f==1) d = 1
      if (f==numFloors) d = -1
      //check if any commands in that direction
      while (f1 >=1 && f1 <= numFloors && clickedFloors(f1).isEmpty && waitingFloors(f1).isEmpty) f1 += d
      while (f2 >=1 && f2 <= numFloors && clickedFloors(f2).isEmpty && waitingFloors(f2).isEmpty) f2 -= d

      // there exist commands and at end
      if ((f==1 || f==numFloors) && (f1>=1 && f1 <= numFloors)) {direction *= -1; move = true}
      else if (f1>=1 && f1 <= numFloors) move = true

      // not at end and no commands in direction, exist commands in other direction
      else if (f2>=1 && f2 <=numFloors) {direction *= -1; move = true}

      else {
        direction = -1; move = true
      }

      // if at end, move only if there exist commands
      // else if there are no commands in direction, change direction

      if (!clickedFloors(f).isEmpty) exit(this, f) // there are people to exit
      if (!waitingFloors(f).isEmpty) enter(this, f) // there are people to enter


      if (move && (f+direction) >= 1 && (f+direction) <= numFloors) {
        floor = f+direction
      }


      after(secs(1)) (runElevator(floor))


    }
  }


  class Person(name:String, _start:Int, _target:Int) {
    var start = _start
    var target = _target
    var direction = if (_target-_start>0) 1 else -1
    override def toString:String = s"$name (start: $start, target: $target)"
  }

  def wait(elevator:Elevator, p:Person):Unit = {
    val f = p.start
    elevator.waitingFloors(f) = elevator.waitingFloors(f) += p
    log(s"$p is waiting at floor $f")
  }

  def enter(elevator:Elevator, f:Int):Unit = {
    for (p <- elevator.waitingFloors(f)) {
      if (p.direction == elevator.direction) {
        elevator.clickedFloors(p.target) += p
        elevator.waitingFloors(p.target) -= p
        val t = p.target
        log(s"$p has entered and pressed floor $t")
      }
    }
  }

  def exit(elevator:Elevator, f:Int):Unit = {
    for (p <- elevator.clickedFloors(f)) {
      log(s"$p has exited on floor $f")
    }
    elevator.clickedFloors(f) = new ListBuffer()
  }


}

object E1 {
  def main(args:Array[String]):Unit = {
    var e = new Elevator1
    e.run(Some(secs(50)))
  }
}

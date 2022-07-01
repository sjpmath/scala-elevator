object Simulation
{       import time.Time
        import Time._

        type  Effect = () => Unit

        class Event(val time: Time, val effect: Effect) extends Ordered[Event] {
          def compare(that: Event): Int = -time.compare(that.time)
          override def toString: String = s"@$time"
        }

}

abstract class Simulation  {
   import Simulation._
   import time.Time
   import Time._

   private   var _now: Time = Time(0) // Not updateable by subclasses
   protected def now:  Time = _now    // Accessible only by subclasses

   private
   val agenda = new scala.collection.mutable.PriorityQueue[Event]()

   protected
   def after(delay: Time)(action: => Unit): Unit = {
       require(delay.nonNegative, s"Simulation.after($delay): Non-negative delay.")
       agenda.addOne(new Event(_now+delay, { () => action }))
   }

   //
   //
   def initialize(): Unit

   /** A single step in running the simulation */
   protected def makeNextEventHappen(): Unit = {
       val event = agenda.dequeue()
        _now = event.time
        event.effect()
   }

   /** Initialize and run the simulation until either the agenda is empty,
    *  or there is an end time given by stopAfter, and it has been reached. */
   def run(stopAfter: Option[Time]=None): Unit = {
       val end     = stopAfter.getOrElse(Time(0))
       val nonStop = stopAfter.isEmpty
       initialize()
       while (agenda.nonEmpty && (nonStop || _now<end)) makeNextEventHappen()
       finalReport()
   }

  protected def log(message: Any): Unit = println(s"$this @$now: $message")

  protected def finalReport(): Unit = log("Terminated")
}

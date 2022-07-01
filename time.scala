package time {
    /** An instant or duration in time (textualized as if in nanoseconds)  */
    class Time(val time: Long) extends AnyVal with Ordered [Time] {
      def /(that: Long): Time = Time(time/that)      // scale
      def *(that: Long): Time = Time(time*that)      // scale
      def -(that: Time): Time = Time(time-that.time)
      def +(that: Time): Time = Time(time+that.time)
      def compare(that: Time): Int =
        if (time<that.time) -1 else
        if (time>that.time) +1 else 0
      def isPositive: Boolean  = time>0
      def nonNegative: Boolean = time>=0

      override def toString: String = {
        var t  = time
        // generate the mixed-radix digits (little-endian order)
        val ds = for { base <- Time.radix } yield { val d = t % base; t = t / base; d }
        val fracSecs =
            if (ds(0)+ds(1)+ds(2)!=0) f".${ds(2)}%03d,${ds(1)}%03d,${ds(0)}%03d" else ""
        val fracDays =
            if (ds(3)+ds(4)+ds(5)!=0) f"${ds(5)}%02d:${ds(4)}%02d:${ds(3)}%02d$fracSecs"  else s"00$fracSecs"
        if (t!=0) s"$t $fracDays" else s"$fracDays"
      }
     }
  object Time {
   private val radix = Array(1000L,1000L,1000L,60L,60L,24L)
   val NS:   Long=1L
   val US:   Long=radix(0)*NS
   val MS:   Long=radix(1)*US
   val SEC:  Long=radix(2)*MS
   val MIN:  Long=radix(3)*SEC
   val HOUR: Long=radix(4)*MIN
   val DAY:  Long=radix(5)*HOUR
   def apply(ns: Long): Time = new Time(ns)
   def ns(n: Long): Time     = new Time(n*NS)
   def us(n: Long): Time     = new Time(n*US)
   def ms(n: Long): Time     = new Time(n*MS)
   def secs(n: Long): Time   = new Time(n*SEC)
   def mins(n: Long): Time   = new Time(n*MIN)
   def hours(n: Long): Time  = new Time(n*HOUR)
   def days(n: Long): Time   = new Time(n*DAY)

   /** Extractor for use in match expressions */
   def unApply(t: Time): Option[Long] = Some(t.time)

   def apply(string: String): Time =
   /**
       Specify a time with a string.
       This method is ``forgiving'', in the sense that it permits fields
       specifying fractional seconds, minutes, hours, days to exceed the
       proper number of their fractional units. For example 24:63:62 is
       treated as 24 hours, 63 minutes, 62 seconds, and ``normalised'' to
       1 day 1 hour 4 minutes, and 2 seconds.
   */
   try string match {
      case s"$time.$mss,$uss,$nss" => apply(time) + ms(mss.toLong) + us(uss.toLong) + ns(nss.toLong)
      case s"$time.$mss,$uss"      => apply(time) + ms(mss.toLong) + us(uss.toLong)
      case s"$time.$mss"           => apply(time) + ms(mss.toLong*(mss.length match { case 1 => 100; case 2 => 10; case _ => 1}))
      case s"$dd:$hh:$mm:$secs" => new Time(secs.toLong*SEC+mm.toLong*MIN+hh.toLong*HOUR+dd.toLong*DAY)
      case s"$dd $hh:$mm:$secs" => new Time(secs.toLong*SEC+mm.toLong*MIN+hh.toLong*HOUR+dd.toLong*DAY)
      case s"$hh:$mm:$secs"     => new Time(secs.toLong*SEC+mm.toLong*MIN+hh.toLong*HOUR)
      case s"$mm:$secs"         => new Time(secs.toLong*SEC+mm.toLong*MIN)
      case s"$secs"             => new Time(secs.toLong*SEC)
    }
    catch { case exn: Exception => throw new TimeSpecificationError(string + " " + exn.toString) }

    case class TimeSpecificationError(spec: String) extends Exception
   /* The most general form is $dd:$hh:$mm:$secs.$ms,$us,$ns
      Here are some examples. Note the shortcuts for multiples of
      milliseconds.
      time.Time("5:12:0:0.5")     //5:12:00:00.500,000,000
      time.Time("5:12:0:0.50")    //5:12:00:00.500,000,000
      time.Time("12:0:0.503")     //12:00:00.503,000,000
      time.Time("12:0:0.5,6,7")   //12:00:00.005,006,007
      time.Time("12:1.5")         //12:1.500,000,000
   */

   }

} // time package

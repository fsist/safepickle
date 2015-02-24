package com.fsist.safepickle.joda

import com.fsist.safepickle._
import org.joda.time.{DateTime, Interval, Duration, Instant}

/** Picklers for joda-time types which, by default, pickle them as Long values counting milliseconds. */
object JodaTimePicklers {
  implicit object InstantPickler extends Pickler[Instant, PicklingBackend] with ConvertPickler[Instant, Long, PicklingBackend] {
    override implicit def otherPickler: Pickler[Long, PicklingBackend] = PrimitivePicklers.LongPickler
    override def convertFrom(other: Long): Instant = new Instant(other)
    override def convertTo(t: Instant): Long = t.getMillis
  }

  implicit object DateTimePickler extends Pickler[DateTime, PicklingBackend] with ConvertPickler[DateTime, Long, PicklingBackend] {
    override implicit def otherPickler: Pickler[Long, PicklingBackend] = PrimitivePicklers.LongPickler
    override def convertFrom(other: Long): DateTime = new DateTime(other)
    override def convertTo(t: DateTime): Long = t.getMillis
  }

  implicit object DurationPickler extends Pickler[Duration, PicklingBackend] with ConvertPickler[Duration, Long, PicklingBackend] {
    override implicit def otherPickler: Pickler[Long, PicklingBackend] = PrimitivePicklers.LongPickler
    override def convertFrom(other: Long): Duration = new Duration(other)
    override def convertTo(t: Duration): Long = t.getMillis
  }

  /** Pickles an interval as an array of start and end dates. */
  implicit object IntervalPickler extends Pickler[Interval, PicklingBackend] with ConvertPickler[Interval, (Long, Long), PicklingBackend] with TuplePicklers {
    import PrimitivePicklers._

    override implicit def otherPickler: Pickler[(Long, Long), PicklingBackend] = tuple2
    override def convertFrom(other: (Long, Long)): Interval = new Interval(other._1, other._2)
    override def convertTo(t: Interval): (Long, Long) = (t.getStartMillis, t.getEndMillis)
  }
}

package com.fsist.safepickle.joda

import com.fsist.safepickle.Schema.Desc
import com.fsist.safepickle._
import org.joda.time._

/** Picklers for joda-time types which, by default, pickle them as Long values counting milliseconds. */
object JodaTimePicklers {
  implicit object InstantPickler extends ConvertPickler[Instant, Long] {
    override def convertFrom(other: Long): Instant = new Instant(other)
    override def convertTo(t: Instant): Long = t.getMillis
    override val schema: Schema = Schema.SLong(Desc(typeHint = Some("date-time")))
  }

  implicit object DateTimePickler extends ConvertPickler[DateTime, Long] {
    override def convertFrom(other: Long): DateTime = new DateTime(other)
    override def convertTo(t: DateTime): Long = t.getMillis
    override val schema: Schema = Schema.SLong(Desc(typeHint = Some("date-time")))
  }

  implicit object DurationPickler extends ConvertPickler[Duration, Long] {
    override def convertFrom(other: Long): Duration = new Duration(other)
    override def convertTo(t: Duration): Long = t.getMillis
  }

  /** Pickles an interval as an array of start and end dates. */
  implicit object IntervalPickler extends ConvertPickler[Interval, (Long, Long)] with TuplePicklers {
    override def convertFrom(other: (Long, Long)): Interval = new Interval(other._1, other._2)
    override def convertTo(t: Interval): (Long, Long) = (t.getStartMillis, t.getEndMillis)
  }
}

package com.fsist.safepickle.joda

import com.fsist.safepickle._
import org.joda.time.{DateTime, Interval, Duration, Instant}
import org.scalatest.FunSuite

class JodaTimePicklersTest extends FunSuite with WrapperTester {
  import JodaTimePicklers._

  test("Pickling") {
    roundtrip(new Instant(12345L), LongWrapper(12345L))
    roundtrip(new DateTime(12345L), LongWrapper(12345L))
    roundtrip(new Duration(12345L), LongWrapper(12345L))

    roundtrip(new Interval(0, 12345L), ArrayWrapper(Seq(LongWrapper(0), LongWrapper(12345L))))
  }
}

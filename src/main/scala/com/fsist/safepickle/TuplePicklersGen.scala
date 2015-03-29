package com.fsist.safepickle


/** Generates the Scala code in TuplePicklers for all 22 tuple widths. */
object TuplePicklersGen {
  def generatePickler(size: Int): String = {
    val ts = (1 to size) map (i => s"T$i") mkString (", ")
    val picklerParams = (1 to size) map (i => s"tpickler$i: Pickler[T$i]") mkString (", ")
    val tuple = s"Tuple$size[$ts]"
    val schemas = (1 to size) map (i => s"tpickler$i.schema") mkString (", ")

    s"""implicit def tuple$size[$ts](implicit $picklerParams, tag: TypeTag[$tuple]): Pickler[$tuple] =
    new Pickler[$tuple] {
      final override val ttag = tag

      override def pickle(t: $tuple, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        ${(1 to size) map (i => s"writer.write[T$i](t._$i)(tpickler$i)") mkString ("\n        ")}
        writer.writeArrayEnd()
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): $tuple = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        ${(1 to size) map (i => s"val t$i = reader.read[T$i]()(tpickler$i); reader.nextInArray") mkString ("\n        ")}

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        Tuple$size(${(1 to size) map (i => s"t$i") mkString (", ")})
      }

      override val schema: Schema = Schema.STuple(List($schemas))
    }
    """
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 22) {
      val pickler = generatePickler(i)
      println(pickler)
      println()
    }
  }
}

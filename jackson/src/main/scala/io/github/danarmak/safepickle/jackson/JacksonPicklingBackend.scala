package io.github.danarmak.safepickle.jackson

import java.io.{StringWriter, ByteArrayOutputStream, IOException}

import io.github.danarmak.safepickle._

import com.fasterxml.jackson.core._

object JacksonPicklingBackend {
  private lazy val factory = (new JsonFactory)
    .configure(JsonParser.Feature.AUTO_CLOSE_SOURCE, false)
    .configure(JsonParser.Feature.ALLOW_COMMENTS, true)
    .configure(JsonParser.Feature.ALLOW_YAML_COMMENTS, true)
    .configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    .configure(JsonParser.Feature.ALLOW_UNQUOTED_CONTROL_CHARS, true)
    .configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false)
    .configure(JsonGenerator.Feature.AUTO_CLOSE_JSON_CONTENT, false)
    .configure(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN, true)

  object Array extends PicklingBackend[Array[Byte]] {

    override def reader(repr: Array[Byte]): Reader = new JacksonReader(factory.createParser(repr))

    override def writer(): Writer[Array[Byte]] = {
      val bos = new ByteArrayOutputStream()
      new JacksonWriter(factory.createGenerator(bos), bos.toByteArray)
    }
  }

  object String extends PicklingBackend[String] {

    override def reader(repr: String): Reader = new JacksonReader(factory.createParser(repr))

    override def writer(): Writer[String] = {
      val buf = new StringWriter()
      new JacksonWriter(factory.createGenerator(buf), buf.toString)
    }
  }
}

class JacksonReader(parser: JsonParser) extends Reader {

  import com.fasterxml.jackson.core.JsonTokenId._

  override def next(): Boolean = try {
    parser.nextToken() != null
  }
  catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def hasToken(): Boolean = {
    val id = parser.getCurrentTokenId
    id != ID_NO_TOKEN && id != ID_NOT_AVAILABLE
  }

  override def isInt: Boolean = parser.getCurrentTokenId == ID_NUMBER_INT

  override def isBoolean: Boolean = {
    val id = parser.getCurrentTokenId
    id == ID_TRUE || id == ID_FALSE
  }

  override def isString: Boolean = parser.getCurrentTokenId == ID_STRING

  override def isLong: Boolean = parser.getCurrentTokenId == ID_NUMBER_INT

  override def isNull: Boolean = parser.getCurrentTokenId == ID_NULL

  override def isArrayStart: Boolean = parser.getCurrentTokenId == ID_START_ARRAY

  override def isArrayEnd: Boolean = parser.getCurrentTokenId == ID_END_ARRAY

  override def isObjectStart: Boolean = parser.getCurrentTokenId == ID_START_OBJECT

  override def isObjectEnd: Boolean = parser.getCurrentTokenId == ID_END_OBJECT

  override def isAttributeName: Boolean = parser.getCurrentTokenId == ID_FIELD_NAME

  override def boolean: Boolean = try {
    parser.getBooleanValue
  }
  catch {
    case e: IOException =>
      throw new IllegalStateException(e)
  }

  override def int: Int = try {
    parser.getIntValue
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def long: Long = try {
    parser.getLongValue
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def string: String = try {
    if (parser.getCurrentTokenId == ID_STRING) {
      parser.getText
    }
    else throw new IllegalStateException(s"Expected string but found ${parser.getCurrentToken} at ${parser.getCurrentLocation}")
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def attributeName: String = try {
    if (parser.getCurrentTokenId == ID_FIELD_NAME) {
      parser.getText
    }
    else throw new IllegalStateException(s"Expected attribute name but found ${parser.getCurrentToken} at ${parser.getCurrentLocation}")
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }
}

class JacksonWriter[Repr](generator: JsonGenerator, makeResult: => Repr) extends Writer[Repr] {
  override def result(): Repr = makeResult

  override def writeString(string: String): Unit = try {
    generator.writeString(string)
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeObjectStart(): Unit = try {
    generator.writeStartObject()
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeArrayEnd(): Unit = try {
    generator.writeEndArray()
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeInt(int: Int): Unit = try {
    generator.writeNumber(int)
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeArrayStart(): Unit = try {
    generator.writeStartArray()

  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeBoolean(boolean: Boolean): Unit = try {
    generator.writeBoolean(boolean)
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeObjectEnd(): Unit = try {
    generator.writeEndObject()
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeLong(long: Long): Unit = try {
    generator.writeNumber(long)
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeNull(): Unit = try {
    generator.writeNull()
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def writeAttributeName(name: String): Unit = try {
    generator.writeFieldName(name)
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }
}

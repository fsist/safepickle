package io.github.danarmak.safepickle.jackson

import java.io.{StringWriter, ByteArrayOutputStream, IOException}

import com.fasterxml.jackson.core.JsonParser.NumberType
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

  override def atEof(): Boolean = {
    val id = parser.getCurrentTokenId
    id != ID_NO_TOKEN && id != ID_NOT_AVAILABLE
  }

  override def tokenType: TokenType = parser.getCurrentTokenId match {
    case ID_NUMBER_INT => parser.getNumberType match {
      case NumberType.INT => TokenType.Int
      case NumberType.LONG => TokenType.Long
      case other => TokenType.String
    }
    case ID_STRING => TokenType.String
    case ID_TRUE | ID_FALSE => TokenType.Boolean
    case ID_NULL => TokenType.Null
    case ID_START_ARRAY => TokenType.ArrayStart
    case ID_END_ARRAY => TokenType.ArrayEnd
    case ID_START_OBJECT => TokenType.ObjectStart
    case ID_END_OBJECT => TokenType.ObjectEnd
    case ID_FIELD_NAME => TokenType.AttributeName
    case other => TokenType.String // Everything else will be rendered as a string, which may not be parsed but is at least supported
  }

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
    parser.getText
  } catch {
    case e: IOException => throw new IllegalStateException(e)
  }

  override def attributeName: String = try {
    parser.getText
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

import scodec.{bits => _, _}
import scodec.bits._
import scodec.codecs._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

def listWithTerminator[A](elem: Codec[A], term: Codec[Unit]): Codec[List[A]] = Codec(l => (list(elem) :: term).encode((l, ())), bits => term.decode(bits).map(res => res.map(_ => List())).orElse(
    elem.decode(bits).flatMap{case DecodeResult(elemVal, rem) => listWithTerminator(elem, term).decode(rem).map(res => res.map(elemVal +: _))}
  ))

val objectCodec: Codec[Value.Object] = fallback((uint32 :: listWithTerminator(
    (uint32 :: lazily(valueCodec)).as[Value.Object.Field], constant(hex"00000000")
  )).as[Value.Object], constant(hex"00000001") :: uint32).xmap(
    d => d.swap.toOption.getOrElse(Value.Object(1, List())),
    obj => if(obj.objType == 1) then Right(((), 0)) else Left(obj)
  )

val objectListCodec: Codec[List[Value.Object]] = listWithTerminator(lazily(objectCodec), constant(hex"00000003"))

val valueCodec: Codec[Value] = discriminated[Value].by(uint8)
  .typecase(0x01, uint8.as[Value.U8])
  .typecase(0x02, uint16.as[Value.Un02])
  .typecase(0x03, uint32.as[Value.U32])
  .typecase(0x05, bool(8).as[Value.Bool])
  .typecase(0x07, double.as[Value.Double])
  .typecase(0x08, bool(1).flatZip(if _ then variableSizeBytes(uint(31).xmap(_ * 2, _ / 2), string(StandardCharsets.UTF_16)) else variableSizeBytes(uint(31), string(StandardCharsets.US_ASCII))).xmap(d => d._2, e => (false, e)).as[Value.Str])
  .typecase(0x09, lazily(objectCodec).as[Value.Object])
  .typecase(0x0A, constant(hex"").as[Value.Un0A])
  .typecase(0x0B, bytes(4).as[Value.Un0B])
  .typecase(0x0D, variableSizeBytesLong(uint32, bytes).as[Value.Un0D])
  .typecase(0x0F, listOfN(int32, uint32).as[Value.U32Arr])
  .typecase(0x12, lazily(objectListCodec).as[Value.ObjectList])
  .typecase(0x15, uuid.as[Value.UUID])
  .typecase(0x16, bytes(16).as[Value.Un16])
  .typecase(0x17, listOfN(int32, float).as[Value.FloatArr])
  .typecase(0x19, listOfN(int32, ascii32).as[Value.StrArr])

val asciiHexCodec = filtered(ascii, bits(32)).xmap(
  Integer.parseUnsignedInt(_, 16),
  _.toHexString
)

sealed abstract class Value
object Value:
  case class U8(value: scala.Int) extends Value
  case class Un02(value: scala.Int) extends Value
  case class U32(value: scala.Long) extends Value
  case class Bool(value: scala.Boolean) extends Value
  case class Double(value: scala.Double) extends Value
  case class Str(value: java.lang.String) extends Value
  case class Object(objType: scala.Long, values: List[Object.Field]) extends Value
  object Object:
    case class Field(id: scala.Long, value: Value)
  case class Un0A(value: Unit) extends Value
  case class Un0B(value: ByteVector) extends Value
  case class Un0D(value: ByteVector) extends Value
  case class U32Arr(values: List[scala.Long]) extends Value
  case class ObjectList(value: List[Object]) extends Value
  case class UUID(value: java.util.UUID) extends Value
  case class Un16(value: ByteVector) extends Value
  case class FloatArr(value: List[Float]) extends Value
  case class StrArr(value: List[java.lang.String]) extends Value


val prefix = constant(asciiBytes"BtWg") ~> bytes(16).unit(ByteVector.fill(16)(0)) ~> asciiHexCodec
val bwpreset = peek(prefix).flatZip(start => bytes(start).unit(ByteVector.fill(start)(0)) ~> objectCodec)


@main def hello: Unit =
  val presets = os.walk(os.home / "Music" / "bitwig" / "library" / "Presets").filter(_.ext == "bwpreset")

  val objs = presets.map(os.read.bytes).map(BitVector(_)).map(bwpreset.decode)
  val results = presets.zip(objs).filter{(_, res) => res.isFailure}.map{(path, res) => (path, res.toString())}
  results.foreach(println(_))
  // println(s"success: ${objs.count(_.isSuccessful)}, failure: ${objs.count(_.isFailure)}")

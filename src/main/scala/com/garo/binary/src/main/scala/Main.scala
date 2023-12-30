import scodec.{bits => _, _}
import scodec.bits._
import scodec.codecs._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import cats.Monoid
import cats.syntax.all._

def listWithTerminator[A](elem: Codec[A], term: Codec[Unit]): Codec[List[A]] = Codec(
  l => (list(elem) :: term).encode((l, ())),
  bits =>
    term
      .decode(bits)
      .map(res => res.map(_ => List()))
      .orElse(
        elem.decode(bits).flatMap { case DecodeResult(elemVal, rem) =>
          listWithTerminator(elem, term).decode(rem).map(res => res.map(elemVal +: _))
        }
      )
)

val asciiHexCodec = filtered(ascii, bits(32)).xmap(
  Integer.parseUnsignedInt(_, 16),
  i => "0" * (4 - i.toHexString.length) + i.toHexString
)

sealed abstract class Value:
  def walk[T: Monoid](f: Value => T): T =
    Monoid.combine(
      f(this),
      this match {
        case Value.Object(objType, values) => Monoid.combineAll(values.map(_.value.walk(f)))
        case Value.ObjectList(value)       => Monoid.combineAll(value.map(_.walk(f)))
        case _                             => Monoid.empty
      }
    )

  def transform(f: Value => Value): Value =
    f(this match {
      case Value.Object(objType, values) =>
        Value.Object(
          objType,
          values.map { case Value.Object.Field(name, value) => Value.Object.Field(name, value.transform(f)) }
        )
      case Value.ObjectList(value) => Value.ObjectList(value.map(_.transform(f).asInstanceOf[Value.Object]))
      case _                       => this
    })

  def /(field: Field) = this match {
    case Value.Object(tpe, values) => values.find(_.id == field.tag).map(_.value)
    case _                         => Option.empty
  }
  def /!(field: Field) = this.asInstanceOf[Value.Object].values.find(_.id == field.tag).get.value
end Value

object Value:
  case class U8(value: scala.Int) extends Value:
    override def toString() = s"$value"
  case class Un02(value: scala.Int) extends Value
  case class U32(value: scala.Long) extends Value:
    override def toString() = s"$value"
  case class Bool(value: scala.Boolean) extends Value:
    override def toString() = s"$value"
  case class Double(value: scala.Double) extends Value:
    override def toString() = s"$value"
  case class Str(value: java.lang.String) extends Value:
    override def toString() = s"\"$value\""
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

  val objectCodec: Codec[Value.Object] = fallback(
    (uint32 :: listWithTerminator(
      (uint32 :: lazily(valueCodec)).as[Value.Object.Field],
      constant(hex"00000000")
    )).as[Value.Object],
    constant(hex"00000001") :: uint32
  ).xmap(
    d => d.swap.toOption.getOrElse(Value.Object(1, List())),
    obj => if (obj.objType == 1) then Right(((), 0)) else Left(obj)
  )

  val objectListCodec: Codec[List[Value.Object]] = listWithTerminator(lazily(objectCodec), constant(hex"00000003"))

  val valueCodec: Codec[Value] = discriminated[Value]
    .by(uint8)
    .typecase(0x01, uint8.as[Value.U8])
    .typecase(0x02, uint16.as[Value.Un02])
    .typecase(0x03, uint32.as[Value.U32])
    .typecase(0x05, bool(8).as[Value.Bool])
    .typecase(0x07, double.as[Value.Double])
    .typecase(
      0x08,
      bool(1)
        .flatZip(
          if _ then variableSizeBytes(uint(31).xmap(_ * 2, _ / 2), string(StandardCharsets.UTF_16))
          else variableSizeBytes(uint(31), string(StandardCharsets.US_ASCII))
        )
        .xmap(d => d._2, e => (!StandardCharsets.US_ASCII.newEncoder().canEncode(e), e))
        .as[Value.Str]
    )
    .typecase(0x09, lazily(objectCodec).as[Value.Object])
    .typecase(0x0a, constant(hex"").as[Value.Un0A])
    .typecase(0x0b, bytes(4).as[Value.Un0B])
    .typecase(0x0d, variableSizeBytesLong(uint32, bytes).as[Value.Un0D])
    .typecase(0x0f, listOfN(int32, uint32).as[Value.U32Arr])
    .typecase(0x12, lazily(objectListCodec).as[Value.ObjectList])
    .typecase(0x15, uuid.as[Value.UUID])
    .typecase(0x16, bytes(16).as[Value.Un16])
    .typecase(0x17, listOfN(int32, float).as[Value.FloatArr])
    .typecase(0x19, listOfN(int32, ascii32).as[Value.StrArr])
end Value

sealed abstract class Field(val tag: Long)

object Field:
  object RCName extends Field(0x1b72)

  object DeviceOpen extends Field(0x1a8c)
  object DeviceRemoteControlsOpen extends Field(0x1a8d)
  object DeviceModsOpen extends Field(0x1aa9)

  object DeviceID extends Field(0x99)
  object Device extends Field(0x1421)
  object RCPagesPages extends Field(0x1a7e)
  object UUID extends Field(0x18c6)

sealed abstract class Obj(val tag: Long)
object Obj:
  trait ID:
    val ID = Obj.ID
  object ID extends Field(0x02b9)

  trait Module:
    val PresetName = Module.PresetName
    val DeviceName = Module.DeviceName
    val UserDefinedName = Module.UserDefinedName
    val DeviceCreator = Module.DeviceCreator
    val DeviceCat = Module.DeviceCat
    val PresetAuthor = Module.PresetAuthor
    val PresetDesc = Module.PresetDesc
    val PresetCat = Module.PresetCat
    val PresetTags = Module.PresetTags
    val On = Module.On
  object Module:
    object PresetName extends Field(0x12de)
    object DeviceName extends Field(0x009a)
    object UserDefinedName extends Field(0x1559)
    object DeviceCreator extends Field(0x009b)
    object DeviceCat extends Field(0x009c)
    object PresetAuthor extends Field(0x009e)
    object PresetDesc extends Field(0x009f)
    object PresetCat extends Field(0x00a1)
    object PresetTags extends Field(0x00a2)
    object On extends Field(0x00a3)

  object Modulators extends Obj(0x075f) with ID:
    object List extends Field(0x1a46)

  object Modulator extends Obj(0x06c9) with Module:
    object Contents extends Field(0x18c7)

  object ModParam extends Obj(0x06db)

  object Label extends Obj(0x06d9)
  object Value extends Obj(0x85)
  object Preset extends Obj(0x561)

  trait Device extends Module:
    val Active = Device.Active
    val Modulators = Device.Modulators
    val RCPages = Device.RCPages
    val RCPage = Device.RCPage
  object Device:
    object Active extends Field(0x137e)
    object Modulators extends Field(0x18f5)
    object RCPages extends Field(0x1a85)
    object RCPage extends Field(0x1b75)

  object BitwigDevice extends Obj(0x0040) with Device:
    object Contents extends Field(0x00a4)
  object DeviceContents extends Obj(0x00d3):
    object List extends Field(0x020c)

  object VSTDevice extends Obj(0x0728) with Device

  object DeviceChain extends Obj(0x24e)

  trait Param extends ID:
    def Value: Field
  object Param extends ID:
    val all = List(DoubleParam, IntParam, BoolParam, U8Param)

  object DoubleParam extends Obj(0x85) with Param:
    override object Value extends Field(0x0136)
  object IntParam extends Obj(0xf7) with Param:
    override object Value extends Field(0x0330)
  object BoolParam extends Obj(0x7f) with Param:
    override object Value extends Field(0x012f)
  object U8Param extends Obj(0x189) with Param:
    override object Value extends Field(0x0273)

  object GridPolyParams extends Obj(0xd94)
  object GridModules extends Obj(0x771)

  object RCPage extends Obj(0x077b):
    object RCPageName extends Field(0x1b69)
    object RCPageTags extends Field(0x1b79)
    object RCPageRCs extends Field(0x1a7a)

  object RemoteControl extends Obj(0x077c):
    object RCName extends Field(0x1a7b)
    object RCTarget extends Field(0x1a7c)
    object RCIndex extends Field(0x1a88)
    object RCMeta extends Field(0x1a7d)

  object ModSource extends Obj(0x2fc):
    object ModSourceTargets extends Field(0x0e20)

  object ModTarget extends Obj(0x02fd):
    object ModTargetTarget extends Field(0x0e3d)
    object ModTargetMeta extends Field(0x1334)
    object ModTargetAmt extends Field(0x0e32)

def printParams(value: Value) = value.walk {
  case obj @ Value.Object(objType, values) if Obj.Param.all.map(_.tag).contains(objType) =>
    List(s"${obj /! Obj.Param.ID}: ${obj.values(1).value}")
  case _ => List()
}

def doublesToOne(value: Value) = value
  .transform {
    case obj @ Value.Object(Obj.DoubleParam.tag, _) =>
      obj.transform { case _: Value.Double => Value.Double(1.0); case value => value }
    case value => value
  }
  .asInstanceOf[Value.Object]

case class Prefix(bytes: ByteVector, start: Long)
val prefix = constant(asciiBytes"BtWg") ~> bytes(16) :: asciiHexCodec
val bwpreset = (prefix.flatZip { (_, start) => bytes(start - 24) :: Value.objectCodec :: bytes }).complete

@main def hello: Unit =
  val presets = os
    .walk(os.home / "Music" / "bitwig" / "library" / "Presets" / "Flanger+")
    .filter(_.ext == "bwpreset")
    .filter(!_.baseName.endsWith("-test"))

  val objs = presets.map(os.read.bytes).map(BitVector(_)).map(bwpreset.decode).map(_.require)
  val results = presets.zip(objs).map { case (path, res) => (path, printParams(res.value._2._2)) }
  results.foreach(println(_))
  presets.zip(objs).foreach { (path, res) =>
    {
      val newRes = res.value.copy(_2 = res.value._2.copy(_2 = doublesToOne(res.value._2._2)))
      os.write.over(
        os.Path(path.toNIO.getParent()) / s"${path.baseName}-test.${path.ext}",
        bwpreset.encode(newRes).require.toByteArray
      )
    }
  }

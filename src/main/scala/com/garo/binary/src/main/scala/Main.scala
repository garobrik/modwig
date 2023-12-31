import scodec.{bits => _, _}
import scodec.bits._
import scodec.codecs._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import cats.Monoid
import cats.syntax.all._
import scala.collection.mutable

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
  def walk[T: Monoid](f: PartialFunction[Value, T]): T =
    Monoid.combine(
      f.applyOrElse(this, _ => Monoid.empty),
      this match {
        case Value.Object(objType, values) => Monoid.combineAll(values.map(_.value.walk(f)))
        case Value.ObjectList(value)       => Monoid.combineAll(value.map(_.walk(f)))
        case _                             => Monoid.empty
      }
    )

  def walkObjs[T: Monoid](tag: Long, f: PartialFunction[Value.Object, T]): T = walk{
    case obj@Value.Object(`tag`, _) => f.applyOrElse(obj, Monoid.empty)
  }
  def walkObjs[T: Monoid](obj: Obj, f: PartialFunction[Value.Object, T]): T = walkObjs(obj.tag, f)

  def transform(f: PartialFunction[Value, Value]): Value =
    f.applyOrElse(this match {
      case Value.Object(objType, values) =>
        Value.Object(
          objType,
          values.map { case Value.Object.Field(name, value) => Value.Object.Field(name, value.transform(f)) }
        )
      case Value.ObjectList(value) => Value.ObjectList(value.map(_.transform(f).asInstanceOf[Value.Object]))
      case _                       => this
    }, identity)
  
  def transformObjs(obj: Obj, f: PartialFunction[Value.Object, Value.Object]): Value = transform{
    case obj@Value.Object(obj.tag, _) => f(obj)
  }

  def /[V <: Value](field: Field[V]) = this match {
    case Value.Object(tpe, values) => values.find(_.id == field.tag).map(_.value.asInstanceOf[V])
    case _                         => Option.empty
  }
  def /![V <: Value](field: Field[V]) = this.asInstanceOf[Value.Object].values.find(_.id == field.tag).get.value.asInstanceOf[V]
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
  case class Object(objType: scala.Long, values: List[Object.Field]) extends Value:
    override def toString() = s"${objType.toString()}(${String.join(", ", values.map(_.toString()):_*)})"
  object Object:
    case class Field(id: scala.Long, value: Value):
      override def toString() = s"${id.toHexString}: ${value}"
  case class Null(value: Unit) extends Value
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
    .typecase(0x0a, constant(hex"").as[Value.Null])
    .typecase(0x0b, bytes(4).as[Value.Un0B])
    .typecase(0x0d, variableSizeBytesLong(uint32, bytes).as[Value.Un0D])
    .typecase(0x0f, listOfN(int32, uint32).as[Value.U32Arr])
    .typecase(0x12, lazily(objectListCodec).as[Value.ObjectList])
    .typecase(0x15, uuid.as[Value.UUID])
    .typecase(0x16, bytes(16).as[Value.Un16])
    .typecase(0x17, listOfN(int32, float).as[Value.FloatArr])
    .typecase(0x19, listOfN(int32, ascii32).as[Value.StrArr])
end Value

sealed abstract class Field[V <: Value](val tag: Long):
  if (Field.fieldSet.exists(_.tag == tag)) throw Exception()
  Field.fieldSet.add(this)

object Field:
  private val fieldSet = mutable.Set[Field[_]]()
  def all = List(fieldSet)
  object RCName extends Field(0x1b72)

  object DeviceOpen extends Field(0x1a8c)
  object DeviceRemoteControlsOpen extends Field(0x1a8d)
  object DeviceModsOpen extends Field(0x1aa9)

  object DeviceID extends Field(0x99)
  object Device extends Field(0x1421)
  object UUID extends Field(0x18c6)

sealed abstract class Obj(val tag: Long):
  if (Obj.objSet.exists(_.tag == tag)) throw Exception()
  Obj.objSet.add(this)

object Obj:
  private val objSet = mutable.Set[Obj]()
  def all = List(objSet)

  import Value._
  trait ID:
    val ID = Obj.ID
  object ID extends Field[Str](0x02b9)

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
    object PresetName extends Field[Str](0x12de)
    object DeviceName extends Field[Str](0x009a)
    object UserDefinedName extends Field[Str](0x1559)
    object DeviceCreator extends Field[Str](0x009b)
    object DeviceCat extends Field[Str](0x009c)
    object PresetAuthor extends Field[Str](0x009e)
    object PresetDesc extends Field[Str](0x009f)
    object PresetCat extends Field[Str](0x00a1)
    object PresetTags extends Field[Str](0x00a2)
    object On extends Field[Bool](0x00a3)

  object Modulators extends Obj(0x075f) with ID:
    object List extends Field[ObjectList](0x1a46)

  object Modulator extends Obj(0x06c9) with Module:
    object Contents extends Field[Object](0x18c7)
    object Index extends Field[U8](0x1a1b)

  object ModParam extends Obj(0x06db)

  object ValueTmp extends Obj(0x85)
  object Preset extends Obj(0x561)

  trait Device extends Module:
    val Active = Device.Active
    val Modulators = Device.Modulators
    val RCPages = Device.RCPages
    val RCPage = Device.RCPage
  object Device:
    object Active extends Field[Bool](0x137e)
    object Modulators extends Field[Object](0x18f5)
    object RCPages extends Field[Object](0x1a85)
    object RCPage extends Field[Object](0x1b75)

  object BitwigDevice extends Obj(0x0040) with Device:
    object Contents extends Field[Object](0x00a4)
  object DeviceContents extends Obj(0x00d3):
    object List extends Field[ObjectList](0x020c)

  object VSTDevice extends Obj(0x0728) with Device

  object DeviceChain extends Obj(0x24e)

  trait Param[V <: Value] extends ID:
    def Value: Field[V]
  object Param extends ID:
    val all = List(DoubleParam, IntParam, BoolParam, U8Param)

  object DoubleParam extends Obj(0x0085) with Param[Double]:
    override object Value extends Field[Double](0x0136)
  object IntParam extends Obj(0x00f7) with Param[U32]:
    override object Value extends Field[U32](0x0330)
  object BoolParam extends Obj(0x007f) with Param[Bool]:
    override object Value extends Field[Bool](0x012f)
  object U8Param extends Obj(0x0189) with Param[U8]:
    override object Value extends Field[U8](0x0273)

  object BoolSetting extends Obj(0x06db) with ID:
    object Value extends Field[Bool](0x18f9)
  
  object Label extends Obj(0x06d9) with ID:
    object Value extends Field[Str](0x18f6)

  object GridPolyParams extends Obj(0xd94)
  object GridModules extends Obj(0x771)

  object RCPages extends Obj(0x077d):
    object Pages extends Field[ObjectList](0x1a7e)

  object RCPage extends Obj(0x077b):
    object Name extends Field[Str](0x1b69)
    object Tags extends Field[Str](0x1b79)
    object RCs extends Field[ObjectList](0x1a7a)

  object CurrentRCPage extends Obj(0x1b75):
    object Name extends Field[Str](0x1b72)
    object Index extends Field[U8](0x1b73)

  object RemoteControl extends Obj(0x077c):
    object Name extends Field[Str](0x1a7b)
    object Target extends Field[Str](0x1a7c)
    object Index extends Field[U8](0x1a88)
    object Meta extends Field[Object](0x1a7d)

  object ModSource extends Obj(0x2fc):
    object Targets extends Field[ObjectList](0x0e20)

  object ModTarget extends Obj(0x02fd):
    object Target extends Field[Str](0x0e3d)
    object Meta extends Field[Object](0x1334)
    object Amount extends Field[Double](0x0e32)
    object Enabled extends Field[Bool](0x2d3b)
    object ResponseCurve extends Field[U8](0x2c4c)
    object ScaledBy extends Field[Str](0x2d2c)

def printParams(value: Value) = value.walk {
  case obj @ Value.Object(objType, values) if Obj.Param.all.map(_.tag).contains(objType) =>
    List(s"${obj /! Obj.Param.ID}: ${obj.values(1).value}")
}

def doublesToOne(value: Value) = value
  .transformObjs(Obj.DoubleParam, _.transform{ case Value.Double(_) => Value.Double(1.0) }.asInstanceOf[Value.Object])
  .asInstanceOf[Value.Object]

def rcTargetsWithTag(tag: String, value: Value) = value.walkObjs(Obj.RCPage, {
  case obj if (obj /! Obj.RCPage.Tags).value.split(",").contains(tag) => (obj /! Obj.RCPage.RCs).value.map(_ /! Obj.RemoteControl.Target).map(_.value)
})

case class Prefix(bytes: ByteVector, start: Long)
val prefix = constant(asciiBytes"BtWg") ~> bytes(16) :: asciiHexCodec

case class BWPreset(prefix: ByteVector, start: Int, presuffix: ByteVector, value: Value.Object, suffix: ByteVector)
object BWPreset:
  val codec = (prefix.flatConcat { (_, start) => bytes(start - 24) :: Value.objectCodec :: bytes }).complete.as[BWPreset]

  def fromFile(path: os.Path) = codec.decode(BitVector(os.read.bytes(path))).require.value

@main def hello: Unit =
  val presets = os
    .walk(os.home / "Music" / "bitwig" / "library" / "Presets")
    .filter(_.ext == "bwpreset")
    .filter(!_.baseName.endsWith("-test"))

  val objs = presets.map(BWPreset.fromFile)
  val results = presets.zip(objs).map { case (path, res) => (path, rcTargetsWithTag("xy", res.value)) }
  results.foreach(println(_))
  // presets.zip(objs).foreach { (path, res) =>
  //   {
  //     val newRes = res.value.copy(_2 = res.value._2.copy(_2 = doublesToOne(res.value._2._2)))
  //     os.write.over(
  //       os.Path(path.toNIO.getParent()) / s"${path.baseName}-test.${path.ext}",
  //       bwpreset.encode(newRes).require.toByteArray
  //     )
  //   }
  // }

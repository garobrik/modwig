package bwfile

import scodec.{bits => _, _}
import scodec.bits._
import scodec.codecs._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import cats.Monoid
import cats.syntax.all._
import scala.collection.mutable
import scala.util.chaining._

// object Reflect:
//   import scala.compiletime._
//   inline def enumerateSealed[T](using
//       m: scala.deriving.Mirror.SumOf[T]
//   ) = allInstances[m.MirroredElemTypes, m.MirroredMonoType]

//   inline def allInstances[ET <: Tuple, T]: List[T] =
//     import scala.compiletime.*

//     inline erasedValue[ET] match
//       case _: EmptyTuple => Nil
//       case _: (t1 *: t2 *: ts)  => summonInline[ValueOf[t1]].value.asInstanceOf[T] :: summonInline[ValueOf[t2]].value.asInstanceOf[T] :: allInstances[ts, T]
//       case _: (t *: ts)  => summonInline[ValueOf[t]].value.asInstanceOf[T] :: allInstances[ts, T]
// end Reflect

trait Lens[T, S]:
  def get(t: T): S
  def mut(t: T, f: S => S): T

  def apply[S2](other: Lens[S, S2]) = new Lens[T, S2] {
    def get(t: T) = other.get(Lens.this.get(t))
    def mut(t: T, f: S2 => S2) = Lens.this.mut(t, other.mut(_, f))
  }

  def equates(t1: T, t2: T) = get(t1) == get(t2)

sealed abstract class Obj(val tag: Long):
  override def toString() = this.getClass().getSimpleName().filter(_ != '$')

  def apply(fields: (Obj.Field[_], Value)*) =
    Value.Object(tag, fields.map { (f, v) => Value.Object.Field(f.tag, v) }.toList)

object Obj:
  // val all = Reflect.enumerateSealed[Obj]

  import Value._
  trait ID:
    val ID = Obj.ID
  object ID extends Field[Str](0x02b9):
    def find(id: String) = new Lens[ObjectList, Object] {
      def get(t: ObjectList) = t.value.find(ID.this.get(_).value == id).get
      def mut(t: ObjectList, f: Object => Object) = ObjectList(t.value.map {
        case obj if ID.this.get(obj).value == id => f(obj)
        case obj                                 => obj
      })
    }

  trait Module extends ID:
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
    object ContentsField extends Field[Object](0x18c7)
    object Column extends Field[U8](0x1a1a)
    object Row extends Field[U8](0x1a1b)
    object UUID extends Field[UUID](0x18c6)

    val Contents = ContentsField(Obj.Contents.List)

  object ModParam extends Obj(0x06db)

  object ValueTmp extends Obj(0x85)

  object Preset extends Obj(0x561):
    object Device extends Field[Object](0x1421)

  trait Device extends Module:
    val Active = Device.Active
    val Modulators = Device.Modulators
    val RCPages = Device.RCPages
    val RCPage = Device.RCPage
    val Open = Device.Open
    val RemoteControlsOpen = Device.RemoteControlsOpen
    val ModsOpen = Device.ModsOpen
  object Device:
    object Active extends Field[Bool](0x137e)
    object Modulators extends Field[Object](0x18f5)
    object RCPages extends Field[Object](0x1a85)
    object RCPage extends Field[Object](0x1b75)
    object Open extends Field[Bool](0x1a8c)
    object RemoteControlsOpen extends Field[Bool](0x1a8d)
    object ModsOpen extends Field[Bool](0x1aa9)

    val ModList = Modulators(Obj.Modulators.List)

  object BitwigDevice extends Obj(0x0040) with Device:
    object ContentsField extends Field[Object](0x00a4)
    object UUID extends Field[UUID](0x0099)

    val Contents = ContentsField(Obj.Contents.List)

  object Contents extends Obj(0x00d3) with ID:
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

  object CurrentRCPage extends Obj(0x07b1):
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
    val default = Value.objectCodec
      .decode(
        hex"000002FD000002B9080000000000000E3D0800000028434F4E54454E54532F524F4F545F47454E455249435F4D4F44554C452F504944313466623932393800001334090000007B0000012407000000000000000000000125073FF00000000000000000037B07000000000000000000000126010000000127010000000BC60100000007C407BFF00000000000000000012801010000115105010000115205010000000000000E32073FD851EB851EB85700002D3B050100002C4C010000002D2C080000000000000000".toBitVector
      )
      .require
      .value

    def apply(target: String, amount: scala.Double = 1.0, scaledBy: String = "") =
      default.pipe(Target.mut(_, _ => target)).pipe(Amount.mut(_, _ => amount)).pipe(ScaledBy.mut(_, _ => scaledBy))

    object Target extends Field[Str](0x0e3d)
    object Meta extends Field[Object](0x1334)
    object Amount extends Field[Double](0x0e32)
    object Enabled extends Field[Bool](0x2d3b)
    object ResponseCurve extends Field[U8](0x2c4c)
    object ScaledBy extends Field[Str](0x2d2c)

  sealed abstract class Field[V <: Value](val tag: Long) extends Lens[Object, V]:
    override def toString() = this.getClass().getSimpleName().filter(_ != '$')

    def get(t: Object) = t.values.find(_.id == tag).get.value.asInstanceOf[V]
    def mut(t: Object, f: V => V) = t.copy(values = t.values.map {
      case Object.Field(`tag`, value) => Object.Field(tag, f(value.asInstanceOf[V]))
      case field                      => field
    })

  object Field
  // val all = Reflect.enumerateSealed[Obj.Field[_]]

end Obj

def listWithTerminator[A](elem: Codec[A], term: Codec[Unit], index: Int = 0): Codec[List[A]] = Codec(
  l => (list(elem) :: term).encode((l, ())),
  bits =>
    term
      .decode(bits)
      .map(res => res.map(_ => List()))
      .orElse(
        (index.toString | elem).decode(bits).flatMap { case DecodeResult(elemVal, rem) =>
          listWithTerminator(elem, term, index + 1).decode(rem).map(res => res.map(elemVal +: _))
        }
      )
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

  def walkObjs[T: Monoid](tag: Long, f: PartialFunction[Value.Object, T]): T = walk {
    case obj @ Value.Object(`tag`, _) => f.applyOrElse(obj, Monoid.empty)
  }
  def walkObjs[T: Monoid](obj: Obj, f: PartialFunction[Value.Object, T]): T = walkObjs(obj.tag, f)

  def transform(f: PartialFunction[Value, Value]): Value =
    f.applyOrElse(
      this match {
        case Value.Object(objType, values) =>
          Value.Object(
            objType,
            values.map { case Value.Object.Field(name, value) => Value.Object.Field(name, value.transform(f)) }
          )
        case Value.ObjectList(value) => Value.ObjectList(value.map(_.transform(f).asInstanceOf[Value.Object]))
        case _                       => this
      },
      identity
    )

  def transformObjs(obj: Obj, f: PartialFunction[Value.Object, Value.Object]): this.type = transform {
    case obj @ Value.Object(obj.tag, _) => f(obj)
  }.asInstanceOf[this.type]
end Value

given Conversion[Int, Value.U8] = Value.U8(_)
given Conversion[String, Value.Str] = Value.Str(_)
given Conversion[Double, Value.Double] = Value.Double(_)

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
    // override def toString() =
    //   s"${Obj.all.find(_.tag == objType).getOrElse(objType.toHexString)}(${String.join(", ", values.map(_.toString()): _*)})"
    override def toString() =
      s"${objType.toHexString}(${String.join(", ", values.map(_.toString()): _*)})"

    def apply[V <: Value](field: Obj.Field[V]) = field.get(this)

  object Object:
    case class Field(id: scala.Long, value: Value):
      // override def toString() = s"${Obj.Field.all.find(_.tag == id).getOrElse(id.toHexString)}: ${value}"
      override def toString() = s"${id.toHexString}: ${value}"

  case class Null(value: Unit) extends Value:
    override def toString() = "Null"
  case class Un0B(value: ByteVector) extends Value
  case class Un0D(value: ByteVector) extends Value
  case class U32Arr(values: List[scala.Long]) extends Value

  case class ObjectList(value: List[Object]) extends Value:
    override def toString() = s"[${String.join(", ", value.map(_.toString()): _*)}]"

    def appended = value.appended.andThen(ObjectList(_))

  case class UUID(value: java.util.UUID) extends Value
  case class Un16(value: ByteVector) extends Value
  case class FloatArr(value: List[Float]) extends Value
  case class StrArr(value: List[java.lang.String]) extends Value

  val objectCodec: Codec[Value.Object] = fallback(
    (uint32 :: listWithTerminator(
      uint32.flatZip(_.toHexString | lazily(codec)).as[Value.Object.Field],
      constant(hex"00000000")
    )).as[Value.Object],
    constant(hex"00000001") :: uint32
  ).xmap(
    d => d.swap.toOption.getOrElse(Value.Object(1, List())),
    obj => if (obj.objType == 1) then Right(((), 0)) else Left(obj)
  )

  val objectListCodec: Codec[List[Value.Object]] = listWithTerminator(lazily(objectCodec), constant(hex"00000003"))

  val codec: Codec[Value] = discriminated[Value]
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

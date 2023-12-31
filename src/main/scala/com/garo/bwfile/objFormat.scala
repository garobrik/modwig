package com.garo.bwfile.objFormat

import com.garo.bwfile.Value

object Reflect:
  import scala.compiletime._
  inline def enumerateSealed[T](using
      m: scala.deriving.Mirror.SumOf[T]
  ) = allInstances[m.MirroredElemTypes, m.MirroredMonoType]

  inline def allInstances[ET <: Tuple, T]: List[T] =
    import scala.compiletime.*

    inline erasedValue[ET] match
      case _: EmptyTuple => Nil
      case _: (t1 *: t2 *: ts)  => summonInline[ValueOf[t1]].value.asInstanceOf[T] :: summonInline[ValueOf[t2]].value.asInstanceOf[T] :: allInstances[ts, T]
      case _: (t *: ts)  => summonInline[ValueOf[t]].value.asInstanceOf[T] :: allInstances[ts, T]
end Reflect

sealed abstract class Obj(val tag: Long):
  override def toString() = this.getClass().getSimpleName().filter(_ != '$')

object Obj:
  val all = Reflect.enumerateSealed[Obj]

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
    object Open extends Field[Bool](0x1a8c)
    object RemoteControlsOpen extends Field[Bool](0x1a8d)
    object ModsOpen extends Field[Bool](0x1aa9)

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
  
  sealed abstract class Field[V <: Value](val tag: Long):
    override def toString() = this.getClass().getSimpleName().filter(_ != '$')

  object Field:
    val all = Reflect.enumerateSealed[Obj.Field[_]]

    object DeviceID extends Field(0x99)
    object Device extends Field(0x1421)
    object UUID extends Field(0x18c6)

end Obj

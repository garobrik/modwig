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

def rcTargetsWithTag(tag: String, value: Value) = value.walkObjs(
  Obj.RCPage,
  {
    case obj if obj(Obj.RCPage.Tags).value.split(",").contains(tag) =>
      obj(Obj.RCPage.RCs).value.map(_(Obj.RemoteControl.Target).value)
  }
)

case class BWPreset(prefix: ByteVector, start: Int, presuffix: ByteVector, value: Value.Object, suffix: ByteVector):
  import Obj._

  def addModulator(modulator: Modulator) =
    copy(value = Preset.Device.mut(value, modulator.add(_)))

  def addBinding(modulatorID: String, sourceID: String, target: String) =
    if (
      Preset
        .Device(Device.ModList)(ID.find(modulatorID)(Modulator.Contents)(ID.find(sourceID)))(ModSource.Targets)
        .get(value)
        .value
        .exists(_(ModTarget.Target).value == target)
    ) {
      this
    } else {
      copy(value =
        Preset.Device.mut(
          value,
          _.pipe(
            Device
              .ModList(ID.find(modulatorID))(Modulator.Contents)(ID.find(sourceID))(ModSource.Targets)
              .mut(
                _,
                targets => {
                  targets.appended(
                    ModTarget(
                      target,
                      scaledBy =
                        s"MODULATORS/${Preset.Device(Device.ModList).get(value).value.length}/CONTENTS/${Macro.source}"
                    )
                  )
                }
              )
          ).pipe(
            Macro.addWithoutRCs(_)
          ).pipe { device =>
            {
              def rcPage(index: Int) =
                RemoteControl("", s"MODULATORS/${Device.ModList.get(device).length - 1}/CONTENTS/VALUE", index)
              if (device(Device.RCPages)(RCPages.Pages).value.exists(_(RCPage.Name).value == target)) {
                Device
                  .RCPages(RCPages.Pages)(Value.ObjectList.find(_(RCPage.Name).value == target)(RCPage.RCs))
                  .mut(device, list => list.appended(rcPage(list.length)))
              } else {
                Device
                  .RCPages(RCPages.Pages)
                  .mut(
                    device,
                    _.appended(
                      RCPage(
                        RCPage.Name -> target,
                        RCPage.Tags -> target,
                        RCPage.RCs -> Value.ObjectList(List(rcPage(0)))
                      )
                    )
                  )
              }
            }
          }
        )
      )
    }

  def toBytes = BWPreset.codec.encode(this).require.toByteArray

object BWPreset:
  val asciiHexCodec = filtered(ascii, bits(32)).xmap(
    Integer.parseUnsignedInt(_, 16),
    i => "0" * (4 - i.toHexString.length) + i.toHexString
  )

  val prefix = constant(asciiBytes"BtWg") ~> bytes(16) :: asciiHexCodec
  val codec =
    (prefix.flatConcat { (_, start) => bytes(start - 24) :: Value.objectCodec :: bytes }).complete.as[BWPreset]

  def fromFile(path: os.Path) = codec.decode(BitVector(os.read.bytes(path))).require.value

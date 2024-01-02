package bwfile

import scodec.{bits => _, _}
import scodec.bits._
import scodec.codecs._
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import cats.Monoid
import cats.syntax.all._
import scala.collection.mutable

def rcTargetsWithTag(tag: String, value: Value) = value.walkObjs(
  Obj.RCPage,
  {
    case obj if obj(Obj.RCPage.Tags).value.split(",").contains(tag) =>
      obj(Obj.RCPage.RCs).value.map(_(Obj.RemoteControl.Target).value)
  }
)

case class BWPreset(prefix: ByteVector, start: Int, presuffix: ByteVector, value: Value.Object, suffix: ByteVector):
  def addModulator(modulator: Modulator) = 
    copy(value = Obj.Preset.Device.mut(value, modulator.addTo(_)))

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

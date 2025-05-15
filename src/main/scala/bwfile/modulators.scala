package bwfile

import scala.io.Source
import scodec.bits.BitVector
import scala.util.chaining._

abstract class Modulator(name: String):
  import Obj.{Modulator => _, _}

  lazy val default = {
    val stream = getClass.getClassLoader.getResourceAsStream(s"modulators/${name}.bwmodulator")
    val bytes = stream.readAllBytes()
    stream.close()
    Value.codec.decode(BitVector(bytes)).require.value.asInstanceOf[Value.Object]
  }

  def params: List[String] = List()
  def source: String = ""

  def add(device: Value.Object) = {
    val list = Device.ModList.get(device).value

    device
      .pipe(
        Device
          .RCPages(RCPages.Pages)
          .mut(
            _,
            _.appended(
              RCPage(
                RCPage.Name -> name,
                RCPage.Tags -> s"mod:${list.filter(Obj.Modulator.DeviceName.get(_).value != "Macro").length}",
                RCPage.RCs -> Value.ObjectList(params.take(8).zipWithIndex.map { (p, i) =>
                  RemoteControl(
                    "",
                    s"MODULATORS/${list.length}/CONTENTS/${p}",
                    i
                  )
                })
              )
            )
          )
      )
      .pipe(addWithoutRCs(_))
  }

  def addWithoutRCs(device: Value.Object) = {
    val list = Device.ModList.get(device).value
    Device.ModList.mut(
      device,
      _.appended(
        default
          .pipe(Obj.Modulator.Column.mut(_, _ => list.length / 3))
          .pipe(Obj.Modulator.Row.mut(_, _ => list.length % 3))
          .pipe(Obj.Modulator.ID.mut(_, _ => list.length.toString))
      )
    )
  }

object AHDSR extends Modulator("AHDSR"):
  override def params = List(
    "ATTACK_TIME",
    "HOLD_TIME",
    "DECAY_TIME",
    "SUSTAIN_LEVEL",
    "ATTACK_SHAPE",
    "RELEASE_SHAPE",
    "RELEASE_TIME",
    "DEPTH",
    "DECAY_SHAPE",
    "SINGLE_TRIGGER"
  )
  override def source = "EG"

object LFO extends Modulator("LFO"):
  override def params =
    List("RATE", "TIMEBASE", "DEPTH", "BIPOLAR", "SHAPE", "FORM", "PHASE", "TRIGGER", "DELAY", "FADE_IN")
  override def source = "LFO"

object RelativeKeytrack extends Modulator("Relative Keytrack"):
  override def params = List("ROOT_KEY", "SPREAD", "DEPTH")
  override def source = "KEYTRACK"

object Random extends Modulator("Random"):
  override def params = List("FEEDBACK", "RATE", "BIPOLAR", "SMOOTH", "TRIGGER", "TIMEBASE", "AMOUNT")
  override def source = "RANDOM"

object AudioSidechain extends Modulator("Audio Sidechain"):
  override def params = List("GAIN", "LOW_CUT", "HIGH_CUT", "AMOUNT", "MODE", "RELEASE", "ATTACK")
  override def source = "OUT"

object Macro extends Modulator("Macro"):
  override def source = "SOURCE"

// object Expressions extends Modulator("Expressions")
// object Macro4 extends Modulator("Macro-4")
// object XY extends Modulator("XY")

// object FourStage extends Modulator("4-Stage")
// object AHDonRelease extends Modulator("AHD on Release")
// object AudioRate extends Modulator("Audio Rate")
// object Curves extends Modulator("Curves")
// object Keytrack extends Modulator("Keytrack+")
// object ClassicLFO extends Modulator("Classic LFO")
// object ParSeq8 extends Modulator("ParSeq-8")
// object Quantize extends Modulator("Quantize")
// object Segments extends Modulator("Segments")
// object Steps extends Modulator("Steps")
// object Vibrato extends Modulator("Vibrato")
// object WavetableLFO extends Modulator("Wavetable LFO")

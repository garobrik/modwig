package bwfile

import scala.io.Source
import scodec.bits.BitVector
import scala.util.chaining._

class Modulator(name: String):
  import Obj.{Modulator => _, _}

  lazy val default = {
    val stream = getClass.getClassLoader.getResourceAsStream(s"modulators/${name}.bwmodulator")
    val bytes = stream.readAllBytes()
    stream.close()
    Value.codec.decode(BitVector(bytes)) //.require.value.asInstanceOf[Value.Object]
  }

  def params: List[String] = List()

  def addTo(device: Value.Object) = {
    val list = Device.Modulators(Modulators.List).get(device)
    val existing = list.value.filter(Obj.Modulator.UUID.equates(_, default.require.value.asInstanceOf[Value.Object]))
    val id = s"${name}${existing.length}"
    
    device.pipe(Device.RCPages(RCPages.Pages).mut(_, _.appended(RCPage(
      RCPage.Name -> id,
      RCPage.Tags -> id,
      RCPage.RCs -> Value.ObjectList(params.zipWithIndex.map{(p, i) => RemoteControl(
        RemoteControl.Name -> "",
        RemoteControl.Index -> i,
        RemoteControl.Target -> s"MODULATORS/${list.value.length}/CONTENTS/${p}"
      )})
    )))).pipe(Device.Modulators(Modulators.List).mut(_, _.appended(
      default.require.value.asInstanceOf[Value.Object]
        .pipe(Obj.Modulator.Column.mut(_, _ => list.value.length / 3))
        .pipe(Obj.Modulator.Row.mut(_, _ => list.value.length % 3))
        .pipe(Obj.Modulator.ID.mut(_, _ => list.value.length.toString))
    )))
  }

object FourStage extends Modulator("4-Stage")
object AHDonRelease extends Modulator("AHD on Release")
object AHDSR extends Modulator("AHDSR")
object AudioRate extends Modulator("Audio Rate")
object AudioSidechain extends Modulator("Audio Sidechain")
object ClassicLFO extends Modulator("Classic LFO")
object Curves extends Modulator("Curves")
object Expressions extends Modulator("Expressions")
object Keytrack extends Modulator("Keytrack+")
object LFO extends Modulator("LFO")
object Macro4 extends Modulator("Macro-4")
object Macro extends Modulator("Macro")
object ParSeq8 extends Modulator("ParSeq-8")
object Quantize extends Modulator("Quantize")
object Random extends Modulator("Random")
object RelativeKeytrack extends Modulator("Relative Keytrack"):
  override def params = List("ROOT_KEY", "DEPTH", "SPREAD")
object Segments extends Modulator("Segments")
object Steps extends Modulator("Steps")
object Vibrato extends Modulator("Vibrato")
object WavetableLFO extends Modulator("Wavetable LFO")
object XY extends Modulator("XY")

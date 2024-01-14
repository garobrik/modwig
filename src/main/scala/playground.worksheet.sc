import bwfile._
import bwfile.objFormat._
import scala.util.chaining._

val presets = os
  .walk(os.home / "Music" / "bitwig" / "library" / "Presets")
  .filter(_.ext == "bwpreset")
  .filter(!_.baseName.endsWith("-test"))

val objs = presets.map(BWPreset.fromFile(_).value)

val obj = objs(0)

os.walk(os.home / "Music" / "bitwig" / "library" / ".settings" / "devices").filter(_.last == "Default.bwpreset").map(os.mtime)
System.currentTimeMillis()

val flanger = BWPreset.fromFile(os.home / "Music" / "bitwig" / "library" / "Presets" / "Flanger+" / "flanger.bwpreset")

val wKeytrack = flanger.addModulator(RelativeKeytrack).addModulator(RelativeKeytrack).addModulator(LFO).addBinding("0", RelativeKeytrack.source, "CONTENTS/DEPTH")
os.write.over(os.home / "Music" / "bitwig" / "library" / "Presets" / "Flanger+" / "wkeytrack.bwpreset", wKeytrack.toBytes)

Macro.default//.pipe(Obj.Modulator.Contents.get)
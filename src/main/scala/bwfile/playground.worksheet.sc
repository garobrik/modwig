import bwfile._
import bwfile.objFormat._

val presets = os
  .walk(os.home / "Music" / "bitwig" / "library" / "Presets")
  .filter(_.ext == "bwpreset")
  .filter(!_.baseName.endsWith("-test"))

val objs = presets.map(BWPreset.fromFile(_).value)

val obj = objs(0)

os.walk(os.home / "Music" / "bitwig" / "library" / ".settings" / "devices").filter(_.last == "Default.bwpreset").map(os.mtime)
System.currentTimeMillis()

import com.garo.bwfile._
import com.garo.bwfile.objFormat._

val presets = os
  .walk(os.home / "Music" / "bitwig" / "library" / "Presets")
  .filter(_.ext == "bwpreset")
  .filter(!_.baseName.endsWith("-test"))

val objs = presets.map(BWPreset.fromFile(_).value)

val obj = objs(0)

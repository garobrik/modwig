package com.garo

import com.garo._;
import com.bitwig.extension.controller.api.{
  Device => _,
  Track => _,
  Parameter => _,
  InsertionPoint => _,
  Action => _,
  _
};
import com.bitwig.extension.controller.{api => bitwig};
import java.util.UUID

abstract class SpecificDeviceSpec {
  def insert(insertionPoint: InsertionPoint): Unit
}

trait SpecificDevice {
  val exists: BoolValue
  val device: com.garo.Device
}

class SpecificVST3Device(id: String, defaultParamID: Int) extends SpecificDeviceSpec {
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertVST3Device(id)

  abstract class BaseDevice(val device: com.garo.Device)(using ControllerExtension) extends SpecificDevice {
    val specificDevice = device.device.createSpecificVst3Device(id)
    def param(id: Int) = Parameter(specificDevice.createParameter(id))
    def param(id: Int, name: String) = Parameter(specificDevice.createParameter(id), Some(name))
    val exists = {
      val defaultParam = param(defaultParamID)
      defaultParam.exists
    }
  }
}

class SpecificBitwigDevice(idString: String, defaultParamID: String) extends SpecificDeviceSpec {
  val id = UUID.fromString(idString)
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertBitwigDevice(id)

  abstract class BaseDevice(val device: com.garo.Device)(using ControllerExtension) extends SpecificDevice {
    val specificDevice = device.device.createSpecificBitwigDevice(id)
    def param(id: String) = Parameter(specificDevice.createParameter(id))
    val exists = {
      val defaultParam = param(defaultParamID)
      defaultParam.exists
    }
  }
}

object Enso extends SpecificVST3Device("565354456E736F656E736F0000000000", 50) {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val lengthMult = param(50)
  }
}

object InstrumentSelector extends SpecificBitwigDevice("9588fbcf-721a-438b-8555-97e4231f7d2c", "INDEX") {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val index = param("INDEX")
  }
}

object Chain extends SpecificBitwigDevice("c86d21fb-d544-4daf-a1bf-57de22aa320c", "MIX") {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val gain = param("GAIN")
    val mix = param("MIX")
  }
}

object DrumSynth extends SpecificVST3Device("ABCDEF019182FAEB4149526D4D755379", 48) {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val instrument = List(1572, 1724, 48634, 48786, 49589, 49741, 49872, 50675).map(param)
    val model = List(48, 1667, 1819, 48750, 48881, 49684, 49836, 50618).map(param)
    val velocity = List(50, 1669, 1821, 48752, 48904, 49686, 49838, 50641).map(param)
    val gain = List(1666, 1818, 48749, 48880, 49683, 49835, 50617, 50769).map(param)
    val params = List(
      List(55, 56, 57, 1567, 1568, 1569, 1570, 1571),
      List(1695, 1696, 1697, 1698, 1699, 1700, 1722, 1723),
      List(48626, 48627, 48628, 48629, 48630, 48631, 48632, 48633),
      List(48757, 48758, 48780, 48781, 48782, 48783, 48784, 48785),
      List(48909, 48910, 48911, 48912, 48913, 49586, 49587, 49588),
      List(49712, 49713, 49714, 49715, 49716, 49717, 49718, 49719),
      List(49843, 49865, 49866, 49867, 49868, 49869, 49870, 49871),
      List(50646, 50647, 50648, 50649, 50671, 50672, 50673, 50674)
    ).map(_.map(param))
  }
}

object Diva extends SpecificVST3Device("D39D5B69D6AF42FA1234567844695661", 126) {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val oscModel = param(85)
    val hpfModel = param(139)
    val filterModel = param(147)
    val env1Model = param(38)
    val env2Model = param(49)

    class VCO(i: Int) {
      val tune = param(i, "Tune")
      val shape = param(i + 5, "Shape")
      val volume = param(i + 11, "Volume")
      val tuneMod = param(i + 38, "Tune Mod")
      val shapeMod = param(i + 41, "Shape Mod")
    }

    object vco1 extends VCO(86) { val fm = param(94, "FM") }
    object vco2 extends VCO(87) { val sync = param(95, "Sync") }
    object vco3 extends VCO(88) { val sync = param(121, "Sync") }
    val vcos = List(vco1, vco2, vco3)

    val voiceCount = param(46)
    val voiceStack = param(5)
    val polyMode = param(6)

    val glide = param(8)
    val glide2 = param(9)
    val vibrato = param(89)
    val feedback = param(160, "Feedback")
    val noise = param(122, "Noise Vol")
    val pinkWhite = param(123, "Noise Color")
    val drive = param(168)

    val filterFreq = param(148, "Freq")
    val filterResonance = param(149, "Res")
    val filterKeyFollow = param(154, "KeyTrack")
    val filterFM = param(155)

    val tuneModSrc = param(103, "Tune Source")
    val tuneModDepth = param(104, "Tune Depth")
    val shapeModSrc = param(109, "Shape Source")
    val shapeModDepth = param(110, "Shape Depth")
    val fmModSrc = param(135, "FM Mod Source")
    val fmModDepth = param(136, "FM Mod Depth")
    val filtModSrc = param(150, "Freq Source 1")
    val filtModDepth = param(151, "Freq Depth 1")
    val filtMod2Src = param(152, "Freq Source 2")
    val filtMod2Depth = param(153, "Freq Depth 1")
    val resModSrc = param(161, "Res Source")
    val resModDepth = param(162, "Res Depth")
    val filtFMModSrc = param(163, "Filt FM Mod Src")
    val filtFMModDepth = param(164, "Filt FM Mod Src")

    class Env(val idx: Int) {
      private val offset = 33 + idx * 11
      val attack = param(offset)
      val decay = param(offset + 1)
      val sustain = param(offset + 2)
      val release = param(offset + 3)
      val velocity = param(offset + 4)
      val keyFollow = param(offset + 10, "KeyTrack")
    }
    val env = List(0, 1).map(Env(_))

    class LFO(val idx: Int) {
      val offset = 10 * idx
      val sync = param(55 + offset)
      val restart = param(56 + offset)
      val waveform = param(57 + offset)
      val phase = param(58 + offset)
      val delay = param(59 + offset)
      val depthSrc = param(60 + offset)
      val depthMod = param(61 + offset)
      val rate = param(62 + offset)
      val rateSrc = param(63 + offset)
      val rateMod = param(64 + offset)
      val polarity = param(272 + idx)
    }
    val lfo = List(0, 1).map(LFO(_))
  }
}

object Chromaphone extends SpecificVST3Device("56535443424D566368726F6D6170686F", 188) {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    class Layer(l: Int) {
      val gain = param(5 + l * 5)
      val balance = param(200 + l * 470)
      val balanceKeyFollow = param(201 + l * 470)
      val coupled = param(202 + l * 470)

      val unisonOn = param(39 + l * 470)
      val unisonVoices = param(38 + l * 470)

      val vibratoRate = param(76 + l * 470)
      val vibratoAmount = param(77 + l * 470)
      val vibratoDelay = param(78 + l * 470)
      val vibratoFade = param(79 + l * 470)

      val malletStiffness = param(105 + l * 470)
      val malletStiffnessKeyTrack = param(106 + l * 470)
      val malletStiffnessVeloTrack = param(107 + l * 470)
      val malletNoise = param(108 + l * 470)
      val malletNoiseKeyTrack = param(109 + l * 470)
      val malletNoiseVeloTrack = param(110 + l * 470)
      val malletColor = param(111 + l * 470)
      val malletOn = param(112 + l * 470)

      val noiseFiltType = param(113 + l * 470)
      val noiseFiltFreq = param(114 + l * 470)
      val noiseFiltFreqKey = param(115 + l * 470)
      val noiseFiltFreqVelo = param(116 + l * 470)
      val noiseFiltFreqLFO = param(117 + l * 470)
      val noiseFiltFreqEnv = param(118 + l * 470)
      val noiseFiltWidth = param(119 + l * 470)
      val noiseFiltQ = param(120 + l * 470)
      val noiseDensity = param(121 + l * 470)
      val noiseDensityKey = param(122 + l * 470)
      val noiseDensityVelo = param(123 + l * 470)
      val noiseDensityLFO = param(124 + l * 470)
      val noiseDensityEnv = param(125 + l * 470)
      val noiseFiltGraphic100hz = param(127 + l * 470)
      val noiseFiltGraphic160hz = param(128 + l * 470)
      val noiseFiltGraphic260hz = param(129 + l * 470)
      val noiseFiltGraphic410hz = param(130 + l * 470)
      val noiseFiltGraphic660hz = param(131 + l * 470)
      val noiseFiltGraphic1p1khz = param(132 + l * 470)
      val noiseFiltGraphic1p7khz = param(133 + l * 470)
      val noiseFiltGraphic2p7khz = param(134 + l * 470)
      val noiseFiltGraphic4p4khz = param(135 + l * 470)
      val noiseFiltGraphic7khz = param(136 + l * 470)

      val noiseOn = param(137 + l * 470)
      val noiseEnvType = param(138 + l * 470)
      val noiseEnvDelay = param(139 + l * 470)
      val noiseEnvA = param(140 + l * 470)
      val noiseEnvH = param(141 + l * 470)
      val noiseEnvD = param(142 + l * 470)
      val noiseEnvS = param(143 + l * 470)
      val noiseEnvR = param(144 + l * 470)

      val malletVol = param(145 + l * 470)
      val malletVolKeyTrack = param(146 + l * 470)
      val malletVolVeloTrack = param(147 + l * 470)
      val malletVolDirect = param(148 + l * 470)

      val noiseVol = param(149 + l * 470)
      val noiseVolKeyTrack = param(150 + l * 470)
      val noiseVolVeloTrack = param(151 + l * 470)
      val noiseVolLFO = param(152 + l * 470)
      val noiseVolDirect = param(153 + l * 470)

      val lfoType = param(203 + l * 470)
      val lfoRate = param(204 + l * 470)
      val lfoSync = param(205 + l * 470)
      val lfoSyncRate = param(206 + l * 470)
      val lfoDelay = param(207 + l * 470)
      val lfoOffset = param(208 + l * 470)
      val lfoPulseWidth = param(209 + l * 470)
      val lfoOn = param(210 + l * 470)

      val detuneLFO = param(157 + l * 470)
      val detuneEnvLevel = param(158 + l * 470)
      val detuneEnvVeloTrack = param(159 + l * 470)
      val detuneEnvAttack = param(160 + l * 470)

      class Resonator(r: Int) {
        val name = if r == 0 then "A" else "B"
        val objectType = param(154 + l * 470 + r * 25)
        val detune = param(155 + l * 470 + +r * 25)
        val detuneKeyTrack = param(156 + l * 470 + r * 25)
        val density = param(161 + l * 470 + r * 21)
        val decay = param(162 + l * 470 + r * 21)
        val decayKeyTrack = param(163 + l * 470 + r * 21)
        val decayVeloTrack = param(164 + l * 470 + r * 21)
        val release = param(165 + l * 470 + r * 21)
        val material = param(166 + l * 470 + r * 21)
        val radius = param(167 + l * 470 + r * 21)
        val tone = param(168 + l * 470 + r * 21)
        val hitPos = param(169 + l * 470 + r * 21)
        val hitPosKeyTrack = param(170 + l * 470 + r * 21)
        val hitPosVeloTrack = param(171 + l * 470 + r * 21)
        val hitPosRand = param(172 + l * 470 + r * 21)
        val lowCut = param(173 + l * 470 + r * 21)
        val partial1 = param(174 + l * 470 + r * 21)
        val partial2 = param(175 + l * 470 + r * 21)
        val partial3 = param(176 + l * 470 + r * 21)
        val partial4 = param(177 + l * 470 + r * 21)
        val onSwitch = param(178 + l * 470 + r * 21)

        def isTube = List("Closed Tube", "Open Tube").contains(objectType.displayedValue())
      }
      val resonator = List(0, 1).map(Resonator(_))
    }
    val layer = List(0, 1).map(Layer(_))
  }
}

object Latch extends SpecificBitwigDevice("93c9d566-4cc9-4895-bf5b-475cab44eba9", "TRIGGER") {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val on = device.remoteControls.controls(0)
  }
}

object OtherDesertCities extends SpecificVST3Device("ABCDEF019182FAEB417544614F444331", 48) {
  class Device(device: com.garo.Device)(using ControllerExtension) extends super.BaseDevice(device) {
    val time1 = param(48)
    val time2 = param(49)
    val regen = param(50)
    val mix = param(51)
    val sync1 = param(52)
    val sync2 = param(53)
    val loop = param(54)
    val ducking = device.remoteControls.controls(0)

    object algorithm {
      val p = param(55)
      val setActions = List(
        ("Stereo", 0.0),
        ("Reverse", 0.2),
        ("Tape", 0.4),
        ("Multi Tap", 0.6),
        ("Multi Head", 0.8),
        ("Granular", 1.0)
      ).map { (name, value) =>
        Action(name, () => p.set(value))
      }
    }

    object algo1 {
      val locut = param(1565)
      val hicut = param(1566)
      val saturation = param(1567)
      val spread = param(1568)
      val crossfeed = param(1569)
    }
    algo1
    object algo2 {
      val locut = param(1570)
      val hicut = param(1571)
      val smoothing = param(1572)
      val crossfeed = param(1573)
      val reflect = param(1574)
    }
    algo2
    object algo3 {
      val speed1 = param(1575)
      val speed2 = param(1576)
      val color = param(1598)
      val random = param(1599)
      val smoothing = param(1600)
      val crossfeed = param(1601)
      val respeed = param(1602)
      val quantize = param(1603)
    }
    algo3
    object algo4 {
      val numTaps1 = param(1604)
      val numTaps2 = param(1605)
      val locut = param(1606)
      val hicut = param(1607)
      val spread = param(1629)
      val crossfeed = param(1630)
    }
    algo4
    object algo5 {
      val speed = param(1726)
      val window = param(1727)
      val heads = param(1728)
      val color = param(1729)
      val repitch = param(1730)
      val quantize = param(1731)
    }
    algo5
    object algo6 {
      val pitch = param(1753)
      val pitchRandom = param(1754)
      val size = param(1755)
      val scatter = param(1756)
      val locut = param(1757)
      val hicut = param(1758)
      val regrain = param(1759)
      val quantize = param(1760)
    }
    algo6
    object diffuser {
      val size = param(1818)
      val diffusion = param(1819)
      val modRate = param(1820)
      val modDepth = param(1821)
      val mix = param(1822)
      val locut = param(1823)
    }
    diffuser

    val inputLevel = param(1824)
    val inputBalance = param(1825)
    val delay1pan = param(48626)
    val delay2pan = param(48627)
    val width = param(48628)
    val regenBalance = param(48629)
    val wetBalance = param(48630)
    val output = param(48631)
    val mute = param(48632)
  }
}

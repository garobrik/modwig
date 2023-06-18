package com.garo

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._
import com.bitwig.extension.controller.{api => bitwig}

import com.garo.ControllerExtensionProxy
import java.util.UUID

case class MPKminiExtension(_host: ControllerHost) extends MyControllerExtension()(_host) {
  implicit val modeCtx = ModeCtx()
  implicit val MPKin = host.getMidiInPort(0)
  implicit val surface = host.createHardwareSurface()
  surface.setPhysicalSize(600, 200)

  val MPKout = host.getMidiOutPort(0)

  val noteInput = MPKin.createNoteInput("Keys", "80????", "90????")
  val padInput = MPKin.createNoteInput("Pads", "81????", "91????", "A1????")

  val knobs =
    List
      .range(0, 8)
      .map(idx =>
        RelativeHardwareControl(
          s"K${idx + 1}",
          20 + idx,
          channel = Some(0),
          indexInGroup = Some(idx),
          bounds = Some(Bounds(300 + ((idx % 4) + 1) * 220 / 4, ((idx / 4) * 2 + 1) * 20, 20, 20))
        )
      )

  val ccPads =
    List(20, 30).flatMap(idx => (idx + 4 until idx + 8) ++ (idx until idx + 4)).zipWithIndex.map { case (cc, idx) =>
      HardwareButton(
        s"CC${idx + 1}",
        ActionMatcher(ActionMatcher.CC, cc, channel = Some(1)),
        indexInGroup = Some(idx % 8),
        bounds = Some(Bounds(((idx % 4) + 1) * 220 / 4, ((idx / 4) * 2 + 1) * 20, 20, 20))
      )
    }

  val pcPads =
    List(1, 9).flatMap(idx => (idx + 4 until idx + 8) ++ (idx until idx + 4)).zipWithIndex.map { case (pc, idx) =>
      HardwareButton(
        s"PC${idx + 1}",
        ActionMatcher(ActionMatcher.PC, pc, channel = Some(1)),
        indexInGroup = Some(idx % 8)
      )
    }

  object joystick {
    val leftMod = AbsoluteHardwareKnob(s"JLMod", 30, channel = Some(0))
    val rightMod = AbsoluteHardwareKnob(s"JRMod", 31, channel = Some(0))
    val upMod = AbsoluteHardwareKnob(s"JUMod", 32, channel = Some(0))
    val downMod = AbsoluteHardwareKnob(s"JDMod", 33, channel = Some(0))
    val mods = List(leftMod, rightMod, upMod, downMod)

    val leftButton =
      HardwareButton(s"JLButton", ActionMatcher(ActionMatcher.CC, 30, channel = Some(0), value = Some(127)))
    val rightButton =
      HardwareButton(s"JRButton", ActionMatcher(ActionMatcher.CC, 31, channel = Some(0), value = Some(127)))
    val upButton =
      HardwareButton(s"JUButton", ActionMatcher(ActionMatcher.CC, 32, channel = Some(0), value = Some(127)))
    val downButton =
      HardwareButton(s"JDButton", ActionMatcher(ActionMatcher.CC, 33, channel = Some(0), value = Some(127)))
  }
  joystick

  object pedal {
    val control = HardwareButton("Pedal", ActionMatcher(ActionMatcher.CC, 1, channel = Some(0)))
    implicit val modeCtx = ModeCtx()
    val modes = List(
      new Mode {
        val kickAction = createAction(
          () => "Kick",
          () => {
            padInput.sendRawMidiEvent(ShortMidiMessage.NOTE_ON + 1, 0, 127)
            padInput.sendRawMidiEvent(ShortMidiMessage.NOTE_OFF + 1, 0, 0)
          }
        )
        override def name = "Kick"
        override def bindings = List(control -> kickAction)
      }
    )

    val toggleModeAction = createAction(
      () => "Toggle Pedal Function",
      () => {
        val currentModeIdx = modes.indexOf(modeCtx.current.getOrElse(modes.head))
        val nextModeIdx = (currentModeIdx + 1) % modes.length
        modes(nextModeIdx).setAction.invoke()
      }
    )
  }
  pedal

  abstract class ModeWithSelector extends Mode {
    def bindingsWithoutSelector: Mode.Bindings
    def extraSelectorBindings: Mode.Bindings = List()

    val modeSelect = {
      val outer = this
      new Mode {
        override def name = "Select"
        override def bindings = {
          assert(
            !extraSelectorBindings.exists { case (binding, _) => (0 until 3).map(ccPads).contains(binding) },
            s"${outer.name} should not bind to selector pads 1 to 4!"
          )
          extraSelectorBindings ++ modes.globals.bindingsWithoutSelector.filter(
            _._1.isInstanceOf[RelativeHardwareControl]
          ) ++ List(
            ccPads(0) -> modes.globalModeSelector.replaceAction,
            ccPads(1) -> modes.trackSelect.replaceAction,
            ccPads(2) -> modes.loopSelect.replaceAction,
            ccPads(3) -> popAction
          )
        }
      }
    }

    final override def bindings = {
      assert(!bindingsWithoutSelector.exists(_._1 == ccPads(3)), s"$name should not bind to pad 4!")
      bindingsWithoutSelector ++ List(
        ccPads(3) -> modeSelect.pushAction
      )
    }
  }

  object modes {
    val globals: ModeWithSelector = new ModeWithSelector {
      def name = "Globals"
      def bindingsWithoutSelector = List(
        joystick.leftButton -> selectedTrack.selectPreviousAction,
        joystick.rightButton -> selectedTrack.selectNextAction,
        knobs(3) -> selectedTrack.volume.param,
        knobs(6) -> transport.tempo.param,
        knobs(7) -> masterTrack.volume.param,
        ccPads(4) -> transport.playAction,
        ccPads(5) -> transport.recordAction,
        ccPads(6) -> transport.tapTempoAction,
        ccPads(0) -> transport.stopAction,
        ccPads(1) -> selectedTrack.arm.toggleAction,
        ccPads(2) -> selectedTrack.active.toggleAction,
        ccPads(7) -> modes.browse.replaceAction
      )
    }

    val browse = new ModeWithSelector {
      def name = "Browse"
      def bindingsWithoutSelector = List(
        joystick.leftButton -> selectedTrack.selectPreviousAction,
        joystick.rightButton -> selectedTrack.selectNextAction,
        joystick.upButton -> browser.prevFileAction,
        joystick.downButton -> browser.nextFileAction,
        knobs(3) -> selectedTrack.volume.param,
        knobs(7) -> masterTrack.volume.param,
        ccPads(0) -> selectedTrack.deviceCursor.replaceAction,
        ccPads(4) -> selectedTrack.arm.toggleAction,
        ccPads(5) -> selectedTrack.active.toggleAction,
        ccPads(7) -> modes.globals.replaceAction
      ) ++ (List.range(0, 3) ++ List.range(4, 7)).map(idx =>
        knobs(idx) -> selectedTrack.deviceCursor.remoteControls(idx)
      )
    }

    case class TrackMixer(name: String, tracks: List[Track]) extends ModeWithSelector {
      assert(tracks.length == 7)

      val padCtx = ModeCtx()
      case class MixerPadMode(name: String, fn: Track => HardwareActionBindable) extends Mode()(extension, padCtx) {
        override def bindings = ccPads.zip(tracks.map(fn))
      }

      val knobCtx = ModeCtx()
      case class MixerKnobMode(name: String, fn: Track => RelativeHardwarControlBindable)
          extends Mode()(extension, knobCtx) {
        override def bindings = knobs.zip(tracks.map(fn)) :+ (knobs.last -> fn(masterTrack))
      }

      val padModes = List(
        MixerPadMode("Arm", _.arm.toggleAction),
        MixerPadMode("Solo", _.solo.toggleAction),
        MixerPadMode("Mute", _.mute.toggleAction),
        MixerPadMode("Active", _.active.toggleAction)
      )
      val knobModes = List(
        MixerKnobMode("Volume", _.volume.param),
        MixerKnobMode("Pan", _.pan.param)
      )

      val mixerPadSelectMode = new Mode {
        val switchActions = padModes.map(mode =>
          createAction(
            () => s"Select ${mode.name} Mode",
            () => {
              mode.setAction.invoke()
              this.popAction.invoke()
            }
          )
        )
        override def name = "Mixer Select"
        override def bindings = ccPads.zip(switchActions)
      }

      val mixerKnobSelectMode = new Mode {
        val switchActions = knobModes.map(mode =>
          createAction(
            () => s"Select ${mode.name} Mode",
            () => {
              mode.setAction.invoke()
              this.popAction.invoke()
            }
          )
        )
        override def name = "Mixer Select"
        override def bindings = ccPads.zip(switchActions)
      }

      override def extraSelectorBindings = List(
        ccPads(6) -> mixerPadSelectMode.pushAction,
        ccPads(7) -> mixerKnobSelectMode.pushAction
      )
      override def bindingsWithoutSelector = List()
      override def submodes = List(padCtx, knobCtx)

      override def onEnable() = {
        if (padCtx.current.isEmpty) {
          padCtx.set(padModes(0))
        }
        if (knobCtx.current.isEmpty) {
          knobCtx.set(knobModes(0))
        }
      }
    }

    val trackMixer = TrackMixer("Track Mixer", mainTrackBank.tracks.take(7))
    val loopMixer = TrackMixer("Loop Mixer", fxTrackBank.tracks.take(7))

    val goodies = new ModeWithSelector {
      override def name = "Goodies"
      override def bindingsWithoutSelector = List(
        ccPads(0) -> pedal.toggleModeAction
      )
    }

    val globalModeSelector = new Mode {
      override def name = "Global Mode Select"
      override def bindings = ccPads.zip(
        List(
          goodies,
          goodies,
          goodies,
          goodies,
          globals,
          browse,
          trackMixer,
          loopMixer
        ).map(_.replaceAction)
      )
    }

    val stompTemplates = {
      val stompTrack = host.createCursorTrack("STOMP TEMPLATE", "Stomp Template", 0, 0, true)
      val stompDevice = Device(
        stompTrack.createCursorDevice("STOM_TEMPLATE", "Stomp Template", 0, CursorDeviceFollowMode.FIRST_DEVICE)
      )
      stompDevice.layers.map(_.fxDevice)
    }

    case class StompBoxMode(track: Track, associated: Mode) extends ModeWithSelector {
      override def name = s"${track.name()} Stomp"
      val mainDevice = track.firstDeviceMatching(Matchers.instrumentOrEffect)
      val maybeStompDevice = track.firstDeviceMatching(Matchers.or(Matchers.lastAndEffect, Chain.matcher))
      val chainDevice = Chain.of(maybeStompDevice)
      val chain = maybeStompDevice.slot
      val chainDevices = chain.createBank(6)
      val stompEqualsFirst = mainDevice.equalsValue(maybeStompDevice)
      def stompDevice = if (maybeStompDevice.exists() && !stompEqualsFirst()) Some(maybeStompDevice) else None

      case class StompMode(stompIdx: Int) extends ModeWithSelector {
        val device = chainDevices(stompIdx)

        override def name = s"Stomp ${stompIdx + 1}"

        val stompPickMode = {
          val outer = this
          new Mode {
            override def name = s"Pick Stomp ${stompIdx + 1}"
            val actions =
              (0 until 8).map(fxIdx =>
                createAction(
                  () => s"Pick ${stompTemplates(fxIdx).name()}",
                  () => {
                    val insertionPoint =
                      if (device.exists()) device.device.replaceDeviceInsertionPoint()
                      else chain.chain.endOfDeviceChainInsertionPoint()
                    insertionPoint.copyDevices(stompTemplates(fxIdx).device)
                    outer.replaceAction.invoke()
                  }
                )
              )
            def bindings = ccPads.zip(actions)
          }
        }

        val enableAction = createAction(
          () => s"Enable Stomp $stompIdx",
          () => if (device.exists()) this.replaceAction.invoke() else stompPickMode.replaceAction.invoke()
        )

        override def extraSelectorBindings = List(
          ccPads(7) -> associated.replaceAction
        )
        override def bindingsWithoutSelector =
          (ccPads.slice(0, 3) ++ ccPads.slice(4, 7)).zip(stomps.map(_.enableAction)) ++
            knobs.zip(device.remoteControls.controls) ++ List(
              ccPads(7) -> stompPickMode.replaceAction
            )
      }
      val stomps = (0 until 6).map(StompMode)

      override def extraSelectorBindings = List(
        ccPads(7) -> associated.replaceAction
      )
      override def bindingsWithoutSelector = (ccPads.slice(0, 3) ++ ccPads.slice(4, 7)).zip(stomps.map(_.enableAction))
    }

    case class TrackMode(track: Track) extends ModeWithSelector {
      case class MPKDevice(device: Device) {
        val joystickPage = device.createTaggedRemoteControlsPage("XY", "xy")
      }

      val firstDevice = MPKDevice(
        track.firstDeviceMatching(Matchers.or(Matchers.instrumentOrEffect, InstrumentSelector.matcher))
      )

      override def name = track.name()
      override def onEnable() = {
        track.select()
        firstDevice.device.remoteControls.visible.set(true)
        host.println(firstDevice.device.name())
        host.println(stomp.chainDevices.itemCount().toString())
      }

      val stomp = StompBoxMode(track, this)
      val selector = firstDevice.device.selector

      val selectInstrumentMode = new Mode {
        override def name = "Instrument Selector"
        val actions = (0 until 8).map(idx =>
          createAction(
            () => s"Select Instrument $idx",
            () => {
              selector.activeChainIndex.set(idx)
              popAction.invoke()
            }
          )
        )

        override def bindings = ccPads.zip(actions)

        val maybeReplaceAction = createAction(
          () => "Maybe Push",
          () => if (selector.exists()) replaceAction.invoke()
        )
      }

      override def bindingsWithoutSelector = List(
        knobs(7) -> track.volume.param,
        ccPads(7) -> track.arm.toggleAction
      ) ++ knobs.take(7).zip(firstDevice.device.remoteControls.controls) ++
        (ccPads.slice(0, 3) ++ ccPads.slice(4, 7)).zip(firstDevice.device.remoteControls.selectPageAction) ++
        joystick.mods.zip(firstDevice.joystickPage.controls)

      override def extraSelectorBindings = List(
        ccPads(7) -> stomp.replaceAction,
        ccPads(4) -> selectInstrumentMode.maybeReplaceAction
      )
    }
    val tracks = List.range(0, ccPads.length).map(mainTrackBank.apply).map(TrackMode)

    val trackSelect = new Mode {
      def name = "Track Select"
      def bindings = ccPads.zip(tracks.map(_.replaceAction))
    }

    case class LoopMode(idx: Int) extends ModeWithSelector {
      val track = fxTrackBank(idx)
      override def name = s"Loop ${idx + 1}"

      val sendMidiActions = (20 until 28).map(idx =>
        createAction(
          () => s"Send Midi CC ${idx}",
          () => track.track.sendMidi(ShortMidiMessage.CONTROL_CHANGE, idx, 127)
        )
      )

      override def onEnable() = {
        track.select()
        enso.device.windowOpen.set(true)
      }

      override def onDisable() = {
        track.select()
        enso.device.windowOpen.set(false)
      }

      val enso = Enso.of(track)
      val stomp = StompBoxMode(track, this)

      val setLengthMode = new Mode {
        override def name = "Set Length"

        val actions = List(1, 2, 3, 4, 6, 8, 12, 16).map(length =>
          createAction(
            () => s"Set Length To $length",
            () => { enso.lengthMult.setImmediately((length - 1) / 32); popAction.invoke() }
          )
        )

        override def bindings = ccPads.zip(actions)
      }

      override def bindingsWithoutSelector = List(
        ccPads(4) -> sendMidiActions(0),
        ccPads(5) -> sendMidiActions(1),
        ccPads(6) -> sendMidiActions(2),
        ccPads(7) -> sendMidiActions(3),
        ccPads(0) -> track.mute.toggleAction,
        ccPads(1) -> track.solo.toggleAction,
        ccPads(2) -> setLengthMode.pushAction,
        knobs(7) -> track.volume.param
      ) ++ knobs.take(7).zip(track.fxDevice.remoteControls.controls)
    }
    val loops = List.range(0, ccPads.length).map(LoopMode)

    val loopSelect = new Mode {
      def name = "Loop Select"
      def bindings = ccPads.zip(loops.map(_.replaceAction))
    }
  }
  modes

  host.scheduleTask(
    new Runnable {
      override def run() = {
        pedal.modeCtx.set(pedal.modes(0))
        modeCtx.set(modes.globals)
      }
    },
    1000
  )

  documentState
    .getSignalSetting("do it", "do it", "do it")
    .addSignalObserver(() => {
      host.showPopupNotification("Doing it")
      host.println(
        MPKmini.program(List("K1", "K2", "Kebab3", "K4", "K5", "K6", "K7", "K8")).map(_.toString).mkString(", ")
      )
      MPKout.sendSysex(MPKmini.program(List("K1", "K2", "Kebab3", "K4", "K5", "K6", "K7", "K8")))
    })
}

object MPKmini {
  def program(knobs: List[String]): Array[Byte] = Array[Byte](
    0x47, 0x7f, 0x49, 0x64, 0x01, 0x76, 0x00 // last byte is program number
  ) ++ Array("garos instrument".toCharArray().map(_.toByte): _*) ++ // must be length 16
    Array[Byte](
      0x01, // pad midi channel
      0x02, // poly aftertouch
      0x00, // keys & controls midi channel
      0x04, // octave
      0x00, // arp on/off
      0x00, // arp mode.
      0x00, // arp timediv
      0x01, // arp clock (ext)
      0x00, // arp latch
      0x00, // arp swing
      0x02, // arp tempo taps
      0x00, // unknown
      0x78, // tempo (default 120)
      0x00, // arp octave
      0x02, // joystick horizontal mode (dual CC)
      30, 31, 0x02, 32, 33
    ) ++ Array.range(0, 16).flatMap(idx => Array[Byte](idx.toByte, (27 - idx).toByte, (idx + 1).toByte)) ++ Array
      .range(
        0,
        8
      )
      .flatMap(idx =>
        Array[Byte](
          0x01, // relative
          (20 + idx).toByte, // cc num
          0x00,
          0x7f
        ) ++ Array(knobs(idx).take(16).padTo(16, ' ').toCharArray().map(_.toByte): _*)
      ) ++ Array[Byte](12) // transpose
}

abstract class Serializable {
  def toBytes: Array[Byte]
}

sealed abstract class Aftertouch extends Serializable {}
object Aftertouch {
  case object Off extends Aftertouch { override def toBytes = Array(0) }
  case object Channel extends Aftertouch { override def toBytes = Array(1) }
  case object Polyphonic extends Aftertouch { override def toBytes = Array(2) }
}

sealed abstract class Program extends Serializable {}
object Program {
  case object RAM extends Program { override def toBytes = Array(0) }
  case class Number(num: Byte) extends Program { override def toBytes = Array(num) }
}

class MPKminiExtensionDefinition extends ControllerExtensionDefinition {
  val DRIVER_ID = UUID.fromString("aa0399c3-409c-4901-957f-cb7f214d2de9")

  override def getName = "MPK mini"

  override def getAuthor = "garo"

  override def getVersion = "0.1"

  override def getId = DRIVER_ID

  override def getHardwareVendor = "garo"

  override def getHardwareModel = "MPK mini"

  override def getRequiredAPIVersion = 17

  override def getNumMidiInPorts = 1

  override def getNumMidiOutPorts = 1

  override def listAutoDetectionMidiPortNames(
      list: AutoDetectionMidiPortNamesList,
      platformType: PlatformType
  ) = {
    if (platformType == PlatformType.WINDOWS) {
      // TODO: Set the correct names of the ports for auto detection on Windows
      // platform here
      // and uncomment this when port names are correct.
      // list.add(new String[]{"Input Port 0"}, new String[]{"Output Port 0", "Output
      // Port -1"});
    } else if (platformType == PlatformType.MAC) {
      // TODO: Set the correct names of the ports for auto detection on Windows
      // platform here
      // and uncomment this when port names are correct.
      // list.add(new String[]{"Input Port 0"}, new String[]{"Output Port 0", "Output
      // Port -1"});
    } else if (platformType == PlatformType.LINUX) {
      // TODO: Set the correct names of the ports for auto detection on Windows
      // platform here
      // and uncomment this when port names are correct.
      // list.add(new String[]{"Input Port 0"}, new String[]{"Output Port 0", "Output
      // Port -1"});
      list.add(Array("MPK mini 3 MIDI 1"), Array("MPK mini 3 MIDI 1"))
    }
  }

  override def createInstance(host: ControllerHost) = ControllerExtensionProxy(MPKminiExtension.apply, this, host)
}

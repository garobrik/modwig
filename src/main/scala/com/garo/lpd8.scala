package com.garo

import java.util.UUID

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._

import com.garo.ControllerExtensionProxy

case class LPD8Extension(_host: ControllerHost)
    extends MyControllerExtension()(_host)
    with ShortMidiMessageReceivedCallback {
  val midiIn = host.getMidiInPort(0)
  val midiOut = host.getMidiOutPort(0)

  midiIn.setMidiCallback(this)
  val noteInput = midiIn.createNoteInput("LPD8 Drums", "000000")

  val surface = host.createHardwareSurface

  val lowKnob = 20
  val knobs = Seq
    .range(0, 8)
    .map(knobNum => {
      val knob = surface.createAbsoluteHardwareKnob(s"K${knobNum + 1}")
      knob.setAdjustValueMatcher(midiIn.createAbsoluteCCValueMatcher(20 + knobNum))
      knob
    })

  val lowPad = 12
  val ccPads = Seq
    .range(0, 8)
    .map(padNum => {
      val button = surface.createHardwareButton(s"CCPAD${padNum + 1}")
      button.pressedAction.setActionMatcher(midiIn.createCCActionMatcher(0, 12 + padNum, 25))
      button.pressedAction
    })

  val lowNote = 36
  val notes = Seq
    .range(0, 8)
    .map(noteNum => {
      val button = surface.createHardwareButton(s"NOTE${noteNum + 1}")
      button.pressedAction.setActionMatcher(midiIn.createNoteOnActionMatcher(0, 12 + noteNum))
      button.pressedAction
    })

  transport.onTransportTick((bar, beat, isPlaying) => {
    for ((statusOn, statusOff, data1, data2on, data2off) <- Seq((176, 176, 12, 1, 0), (144, 128, 36, 1, 127))) {
      for (barButton <- Seq.range(0, 4)) {
        val isOn = isPlaying && barButton == (bar % 4)
        midiOut.sendMidi(if (isOn) statusOn else statusOff, data1 + 4 + barButton, if (isOn) data2on else data2off)
      }
      for (beatButton <- Seq.range(0, 4)) {
        val isOn = isPlaying && beatButton == beat
        midiOut.sendMidi(if (isOn) statusOn else statusOff, data1 + beatButton, if (isOn) data2on else data2off)
      }
    }
  })

  val modes = List(
    new Mode {
      override def name() = "Main"
      override def actions = Map(
        LPD8Button(0) -> MyAction(() => transport.transport.play),
        LPD8Button(1) -> MyAction(() => transport.transport.stop),
        LPD8Button(2) -> MyAction(() => transport.transport.record),
        LPD8Button(3) -> MyAction(() => transport.transport.tapTempo),
        LPD8Button(4) -> MyAction(() => track.cycleNext),
        LPD8Button(7) -> MyAction(() => track.loopLastNBarsOfRecordingClip(4)),
        LPD8Knob(4) -> BitwigBinding(transport.transport.tempo),
        LPD8Knob(7) -> BitwigBinding(masterTrack.volume),
      )
    },
    new Mode {
      override def name() = "Track"
      override def actions = Map()
    },
    new Mode {
      override def name() = "Mixer"
      override def actions = Map()
    },
    new Mode {
      override def name() = "Looper"
      override def actions = Map()
    }
  )
  var mode: Mode = modes(0)
  val modeUI = documentState.getEnumSetting("Mode", "Mode", modes.map(_.name).toArray, mode.name)
  modeUI.addValueObserver(new EnumValueChangedCallback {
    override def valueChanged(newMode: String) = setMode(modes.find(_.name == newMode).get)
  })

  def hardwareTranslation(lpd8: LPD8Msg) = lpd8 match {
    case LPD8Button(number) => ccPads(number)
    case LPD8Knob(number)   => knobs(number)
    case LPD8Note(number)   => notes(number)
  }
  def setMode(newMode: Mode) {
    mode.actions.foreach({
      case (msg, BitwigBinding(_)) => hardwareTranslation(msg).clearBindings
      case _                       =>
    })
    mode = newMode
    mode.actions.foreach({
      case (msg, BitwigBinding(target)) => hardwareTranslation(msg).addBinding(target)
      case _                       =>
    })
    modeUI.set(newMode.name)
    host.showPopupNotification(s"${mode.name} Mode")
  }
  setMode(mode)

  override def midiReceived(msg: ShortMidiMessage) = {
    host.println(msg.toString())
    if (msg.isNoteOn || msg.isNoteOff) {
      noteInput.sendRawMidiEvent(msg.getStatusByte, msg.getData1, msg.getData2)
    } else if (msg.isProgramChange) {
      if (msg.getData1 < modes.length) {
        setMode(modes(msg.getData1))
      }
    } else {
      if (msg.isControlChange) {
        val lpd8Msg = if (12 <= msg.getData1 && msg.getData1 <= 19 && msg.getData2 > 0) {
          LPD8Button(msg.getData1 - 12)
        } else {
          LPD8Knob(msg.getData1 - 20)
        }
        mode.actions.getOrElse(lpd8Msg, MyAction(() => {})) match {
          case MyAction(action) => action()
          case _                =>
        }
      }
    }
  }
}

sealed trait LPD8Msg

case class LPD8Button(number: Int) extends LPD8Msg
case class LPD8Knob(number: Int) extends LPD8Msg
case class LPD8Note(number: Int) extends LPD8Msg

abstract class Mode {
  def name(): String
  def actions: Map[LPD8Msg, ModeAction]
}

sealed trait ModeAction

case class MyAction(action: () => Unit) extends ModeAction
case class BitwigBinding(binding: HardwareBindable) extends ModeAction

class LPD8ExtensionDefinition extends ControllerExtensionDefinition {
  val DRIVER_ID = UUID.fromString("aa0399c3-409c-4901-957e-cb7f214d6de9")

  override def getName = "lpd8"

  override def getAuthor = "garo"

  override def getVersion = "0.1"

  override def getId = DRIVER_ID

  override def getHardwareVendor = "garo"

  override def getHardwareModel = "lpd8"

  override def getRequiredAPIVersion = 17

  override def getNumMidiInPorts = 2

  override def getNumMidiOutPorts = 2

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
      //   list.add(Array("LPD8 Midi 1"), Array("LPD8 MIDI 1"))
    }
  }

  override def createInstance(host: ControllerHost) = ControllerExtensionProxy(LPD8Extension.apply, this, host)
}

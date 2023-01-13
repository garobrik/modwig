package com.garo

import java.util.UUID

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._

import com.garo.ControllerExtensionProxy
import _root_.java.util.function.Supplier

case class LPD8Extension(_host: ControllerHost) extends MyControllerExtension()(_host) {
  val lpd8In = host.getMidiInPort(0)
  val lpd8Out = host.getMidiOutPort(0)

  val noteInput = lpd8In.createNoteInput("LPD8 Drums", "000000")

  val actions = Map(
    "Toggle Play" -> transport.playAction,
    "Stop/Reset" -> transport.stopAction,
    "Toggle Record" -> transport.recordAction,
    "Tap Tempo" -> transport.tapTempoAction,
    "Cycle Next Track" -> track.cycleNextAction,
    "Loop Track's Clip's Last 2 Bars" -> track.loopLast2BarsOfRecordingClipAction
  )

  val parameters = Map(
    "Transport Tempo" -> transport.transport.tempo,
    "Master Volume" -> masterTrack.volume
  )

  case class ActionPreferences(
      cat: String,
      name: String,
      possibleBindings: Map[String, HardwareBindable],
      init: Option[HardwareBindable]
  ) {
    val action = documentState.getEnumSetting(
      name,
      cat,
      (Seq("None") ++ possibleBindings.keys).toArray,
      possibleBindings.find({ case (str, act) => Option(act) == init }).getOrElse(("None", doNothingAction))._1
    )
  }

  case class BindingSetting(
      possibleBindings: Map[String, HardwareBindable],
      name: String,
      initialBinding: Option[HardwareBindable]
  )(implicit settingCtx: SettingCtx)
      extends Setting[Option[HardwareBindable]] {
    val binding =
      EnumSetting(name, List(BindingSetting.noneBinding) ++ possibleBindings.keys, findBinding(initialBinding))

    override def get(): Option[HardwareBindable] = possibleBindings.get(binding.get)
    override def set(bindable: Option[HardwareBindable]) = binding.set(findBinding(bindable))

    def findBinding(binding: Option[HardwareBindable]) = possibleBindings
      .find({ case (str, act) => Option(act) == initialBinding })
      .getOrElse((BindingSetting.noneBinding, doNothingAction))
      ._1

    override def setEnabled(enabled: Boolean) = binding.setEnabled(enabled)
    override def setShown(shown: Boolean) = { binding.setShown(shown); }
    override def onChanged(callback: Option[HardwareBindable] => Unit) = {
      binding.onChanged(value => callback(possibleBindings.get(value)))
    }
  }

  object BindingSetting {
    val noneBinding = "None"
  }

  case class ModeSetting(modeCreateCtx: SettingCtx, modeConfigCtx: SettingCtx, idx: Int, initialName: String)
      extends Setting[Mode] {
    val modeListNameDisplay = StringSetting(s"Mode ${idx + 1}", initialName)(modeCreateCtx)
    modeListNameDisplay.setEnabled(false)

    implicit val settingCtx = modeConfigCtx
    val name = StringSetting(s"Mode ${idx + 1}", initialName)
    name.onChanged(name => modeListNameDisplay.set(name))
    val shownSetting = EnumSetting(s"Bindings${settingCtx.getBlankLabel}", List("Show", "Hide"), "Show")
    var changedAlready = false
    shownSetting.onChanged(shown => {
      if (changedAlready) {
        bindings.values.foreach(_.setShown(shown == "Show"))
      }
      changedAlready = true
    })
    val bindings = {
      val builder = Map.newBuilder[LPD8Msg, BindingSetting];
      builder ++= LPD8Msg.allMsgs.map(msg =>
        msg -> BindingSetting(
          msg match { case LPD8Knob(_) => parameters; case _ => actions },
          s"${msg.name}${settingCtx.getBlankLabel}",
          Option.empty
        )
      );
      builder.result
    }

    def get() = Mode(idx, name.get, bindings.flatMap({ case (msg, binding) => Seq(msg).zip(binding.get) }))
    def set(mode: Mode) = {
      name.set(mode.name)
      bindings.foreach({ case (msg, setting) => { setting.set(mode.actions.get(msg)) } })
    }

    override def setEnabled(enabled: Boolean) = (List(name) ++ bindings.values).foreach(_.setEnabled(enabled))

    override def setShown(shown: Boolean) = {
      List(modeListNameDisplay, name, shownSetting).foreach(_.setShown(shown))
      bindings.values.foreach(_.setShown(shown && shownSetting.get == "Show"))
    }

    override def onChanged(callback: Mode => Unit) = {
      (List(name) ++ bindings.values).foreach(_.onChanged(_ => callback(get)))
    }
  }

  val maxModes = 16
  val modeCreateCtx = SettingCtx("Create Modes")(documentState, extension)
  val modeConfigCtx = SettingCtx("Configure Modes")(documentState, extension)
  val modes = ListSetting(
    1,
    maxModes,
    Mode(0, "New Mode", Map()),
    idx => ModeSetting(modeCreateCtx, modeConfigCtx, idx, if (idx == 0) "Main" else s"Name $idx")
  )(modeCreateCtx)
  modes.onChanged(_ => update())

  val currentMode = IntSetting("Current Mode", "", 0, maxModes, 1, 0)(modeCreateCtx)
  currentMode.setShown(false)
  currentMode.onChanged(_ => update())

  var mode: Mode = modes.get()(currentMode.get())

  lpd8In.setMidiCallback(new ShortMidiMessageReceivedCallback {
    override def midiReceived(msg: ShortMidiMessage) = {
      if (msg.isNoteOn || msg.isNoteOff) {
        noteInput.sendRawMidiEvent(msg.getStatusByte, msg.getData1, msg.getData2)
      } else if (msg.isProgramChange) {
        if (msg.getData1 < modes.get().length) {
          currentMode.set(msg.getData1())
          update()
        }
      } else if (msg.isControlChange) {}
    }
  })

  sealed trait MyHardware {
    def enable(): Unit
    def disable(): Unit

    def bindingSource: HardwareBindingSource[_ <: HardwareBinding]
    def hardwareControl: HardwareControl

    def clearBindings() = bindingSource.clearBindings()
    def addBinding(binding: HardwareBindable) = bindingSource.addBinding(binding)
    def setName(name: String) = hardwareControl.setName(name)
  }

  case class MyButton(modeIdx: Int, padNum: Int, namePrefix: String, actionMatcher: String) extends MyHardware {
    val button = surface.createHardwareButton(s"$namePrefix ${modeIdx}-${padNum + 1}")
    override def bindingSource = button.pressedAction
    override def hardwareControl = button
    button.setIndexInGroup(padNum)
    val onMatcher = lpd8In.createActionMatcher(actionMatcher)
    val nullMatcher = lpd8In.createCCActionMatcher(15, 0, 0)

    def enable() = button.pressedAction.setActionMatcher(onMatcher)
    def disable() = button.pressedAction.setActionMatcher(nullMatcher)
  }

  case class MyAbsoluteKnob(modeIdx: Int, knobNum: Int) extends MyHardware {
    val knob = surface.createAbsoluteHardwareKnob(s"Knob $modeIdx-${knobNum + 1}")
    override def bindingSource = knob
    override def hardwareControl = knob
    knob.setIndexInGroup(knobNum)
    val onMatcher = lpd8In.createAbsoluteCCValueMatcher(20 + knobNum)
    val nullMatcher = lpd8In.createAbsoluteCCValueMatcher(3)

    def enable() = knob.setAdjustValueMatcher(onMatcher)
    def disable() = knob.setAdjustValueMatcher(nullMatcher)
  }

  val surface = host.createHardwareSurface

  val lowKnob = 20
  val knobs = Seq.range(0, maxModes).map(mode => Seq.range(0, 8).map(knobNum => MyAbsoluteKnob(mode, knobNum)))

  val lowPad = 12

  val ccPads =
    Seq
      .range(0, maxModes)
      .map(mode =>
        Seq
          .range(0, 8)
          .map(padNum =>
            MyButton(
              mode,
              padNum,
              "CC Pad",
              s"${ShortMidiMessage.CONTROL_CHANGE} <= status && status < ${ShortMidiMessage.CONTROL_CHANGE + 4} && data1 == ${12 + padNum} && data2 > 0"
            )
          )
      )

  val lowNote = 36
  val notePads =
    Seq
      .range(0, maxModes)
      .map(mode =>
        Seq
          .range(0, 8)
          .map(noteNum =>
            MyButton(
              mode,
              noteNum,
              "Note Pad",
              s"${ShortMidiMessage.NOTE_ON} <= status && status < ${ShortMidiMessage.NOTE_ON} + 4 && data1 == ${36 + noteNum}"
            )
          )
      )

  transport.onTransportTick((bar, beat, isPlaying) => {
    for ((statusOn, statusOff, data1, data2on, data2off) <- Seq((176, 176, 12, 1, 0), (144, 128, 36, 1, 127))) {
      for (barButton <- Seq.range(0, 4)) {
        val isOn = isPlaying && barButton == (bar % 4)
        lpd8Out.sendMidi(if (isOn) statusOn else statusOff, data1 + 4 + barButton, if (isOn) data2on else data2off)
      }
      for (beatButton <- Seq.range(0, 4)) {
        val isOn = isPlaying && beatButton == beat
        lpd8Out.sendMidi(if (isOn) statusOn else statusOff, data1 + beatButton, if (isOn) data2on else data2off)
      }
    }
  })

  def hardwareTranslation(mode: Mode, lpd8: LPD8Msg) = lpd8 match {
    case LPD8Button(number) => ccPads(mode.index)(number)
    case LPD8Knob(number)   => knobs(mode.index)(number)
    case LPD8Note(number)   => notePads(mode.index)(number)
  }

  def update() {
    (ccPads(mode.index) ++ knobs(mode.index) ++ notePads(mode.index)).foreach((h) => { h.disable; h.clearBindings() })
    if (currentMode.get() >= modes.get.length) currentMode.set(modes.get.length - 1)
    mode = modes.get()(currentMode.get())
    (ccPads(mode.index) ++ knobs(mode.index) ++ notePads(mode.index)).foreach(_.enable)
    LPD8Msg.allMsgs
      .map((msg) => (msg, mode.actions.get(msg)))
      .foreach({ case (msg, Some(target)) => hardwareTranslation(mode, msg).addBinding(target); case _ => })

    host.showPopupNotification(s"${mode.name} Mode")
  }

  update()
}

sealed trait LPD8Msg {
  def name: String
}

object LPD8Msg {
  def numIndexes = 24
  val numPads = 8
  val numKnobs = 8

  def allMsgs: Seq[LPD8Msg] = allCCPads ++ allKnobs ++ allNotes
  def allCCPads = Seq.range(0, numPads).map(LPD8Button(_))
  def allKnobs = Seq.range(0, numKnobs).map(LPD8Knob(_))
  def allNotes = Seq.range(0, numPads).map(LPD8Note(_))
}

case class LPD8Button(number: Int) extends LPD8Msg {
  override val name = s"CC Pad ${number + 1}"
}
case class LPD8Knob(number: Int) extends LPD8Msg {
  override val name = s"Knob ${number + 1}"
}
case class LPD8Note(number: Int) extends LPD8Msg {
  override val name = s"Note ${number + 1}"
}

case class Mode(index: Int, name: String, actions: Map[LPD8Msg, HardwareBindable]) {}

object Mode {
  def makeList(list: Map[String, Map[LPD8Msg, HardwareBindable]]) =
    list.zipWithIndex.map({ case ((name, actions), index) => Mode(index, name, actions) }).toList
}

class LPD8ExtensionDefinition extends ControllerExtensionDefinition {
  val DRIVER_ID = UUID.fromString("aa0399c3-409c-4901-957e-cb7f214d6de9")

  override def getName = "lpd8"

  override def getAuthor = "garo"

  override def getVersion = "0.1"

  override def getId = DRIVER_ID

  override def getHardwareVendor = "garo"

  override def getHardwareModel = "lpd8"

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
      //   list.add(Array("LPD8 Midi 1"), Array("LPD8 MIDI 1"))
    }
  }

  override def createInstance(host: ControllerHost) = ControllerExtensionProxy(LPD8Extension.apply, this, host)
}

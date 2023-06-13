package com.garo

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._
import com.bitwig.extension.controller.{api => bitwig}
import java.util.function.Supplier
import _root_.java.sql.Wrapper
import javax.sound.midi.MidiMessage

abstract class MyControllerExtension(implicit val host: ControllerHost) {
  implicit val extension = this

  val transport = Transport(host.createTransport())
  val arranger = host.createArranger()
  val application = host.createApplication()
  val preferences = host.getPreferences()
  val documentState = host.getDocumentState()
  val browser = Browser(host.createPopupBrowser())

  def numSends = 4
  def numScenes = 1
  def trackCursorDefinitions: List[(String, String, Boolean)] = List()
  val trackCursors =
    (("SELECTION", "Selection", true) +: trackCursorDefinitions).map(t =>
      TrackCursor(host.createCursorTrack(t._1, t._2, numSends, numScenes, t._3))
    )
  val selectedTrack = trackCursors(0)
  val mainTrackBank = TrackBank(host.createMainTrackBank(128, numSends, 1))
  val fxTrackBank = TrackBank(host.createEffectTrackBank(128, 1))
  val masterTrack = new Track(host.createMasterTrack(1))

  val bindings = List(transport, Track).flatMap(_.createBindings())
  if (bindings.map(_.name).length != bindings.map(_.name).toSet.size)
    throw new Exception("bindings require unique names!")

  def macroSettings = {
    implicit val settingCtx = SettingCtx("Macros")(documentState, this)
    bindings
      .filter(_.isInstanceOf[CallbackBinding])
      .foreach(binding => {
        SignalSetting(binding.name).onChanged(_ => binding.asInstanceOf[CallbackBinding].action())
      })
  }
  macroSettings

  val blockSize = preferences.getNumberSetting("Max Block Size", "Audio", 1, 50, 1, "ms", 24)

  val doNothingAction = createAction(() => "None", () => {})

  def exit() = {}
  def flush() = {}

  def postFrame(action: () => Unit, ms: Long = blockSize.getRaw.round) =
    new java.util.Timer().schedule(
      new java.util.TimerTask {
        def run() = action()
      },
      ms
    )

  // def adjust(param: Parameter, value: Double, range: (Double, Double)) = {
  //   val (rangeLow, rangeHigh) = range
  //   val rangeSize = rangeHigh - rangeLow
  //   val scaledValue = value / 127 * rangeSize + rangeLow
  //   if (127 * (param.getRaw - scaledValue).abs / rangeSize <= 2) param.setRaw(scaledValue)
  // }

  def createAction(name: () => String, action: () => Unit): HardwareActionBindable =
    host.createAction(
      new Runnable {
        override def run() = action()
      },
      new Supplier[String] {
        override def get() = name()
      }
    )

  def createAction(name: String, action: () => Unit): HardwareActionBindable = createAction(() => name, action)

  object Matchers {
    val effect = host.createAudioEffectMatcher()
    val instrument = host.createInstrumentMatcher()
    val lastDevice = host.createLastDeviceInChainMatcher()
    val instrumentOrEffect = or(instrument, effect)
    val lastAndEffect = and(lastDevice, effect)
    def or(matchers: DeviceMatcher*) = host.createOrDeviceMatcher(matchers: _*)
    def and(matchers: DeviceMatcher*) = host.createAndDeviceMatcher(matchers: _*)
    def not(matcher: DeviceMatcher) = host.createNotDeviceMatcher(matcher)
  }
  Matchers
}

case class ModeCtx()(implicit ext: MyControllerExtension) {
  private var stack = Seq[Mode]();

  def push(mode: Mode) = update(mode +: _)
  def pop(mode: Mode) = update {
    case current +: tail if current == mode => tail
    case stack                              => stack
  }
  def pop() = update(_.drop(1))
  def replace(mode: Mode) = update(stack => mode +: stack.drop(1))
  def set(modes: Mode*) = update(_ => modes)
  def current = stack.headOption

  def enable() = current.foreach(_.enable())
  def disable() = current.foreach(_.disable())

  def update(fn: Seq[Mode] => Seq[Mode]) = {
    val oldStack = stack
    stack = fn(oldStack)
    if (stack.headOption != oldStack.headOption) {
      oldStack.headOption.foreach(_.disable())
      stack.headOption.foreach(mode => {
        mode.enable()
        ext.host.showPopupNotification(s"${mode.name} Mode")
      })
    }
  }
}

abstract class Mode(implicit ext: MyControllerExtension, ctx: ModeCtx) {
  def name: String
  def bindings: Mode.Bindings
  def submodes: List[ModeCtx] = List()

  def onEnable() = {}
  private var currentBindings: List[HardwareBinding] = List()
  private var currentlyBound: List[HardwareBindable] = List()
  final def enable(): Unit = {
    if (!currentBindings.isEmpty || !currentlyBound.isEmpty) return
    onEnable()
    currentBindings = bindings.map {
      case (control, bindable) => {
        if (bindable.isInstanceOf[bitwig.Parameter]) bindable.asInstanceOf[bitwig.Parameter].setIndication(true)
        control.addBinding(bindable)
      }
    }
    currentlyBound = bindings.map(_._2)
    submodes.foreach(_.enable())
    ext.host.println(s"enable mode $name")
  }

  def onDisable() = {}
  final def disable(): Unit = {
    ext.host.println(s"disable mode $name")
    submodes.foreach(_.disable())
    currentBindings.foreach(_.removeBinding())
    currentlyBound.foreach(bindable => {
      if (bindable.isInstanceOf[bitwig.Parameter]) bindable.asInstanceOf[bitwig.Parameter].setIndication(false)
    })
    currentlyBound = List()
    currentBindings = List()
    onDisable()
  }

  def refreshBindings(): Unit = {
    if (!currentBindings.isEmpty) {
      currentBindings.foreach(_.removeBinding())
      currentlyBound.foreach(bindable => {
        if (bindable.isInstanceOf[bitwig.Parameter]) bindable.asInstanceOf[bitwig.Parameter].setIndication(false)
      })
      currentBindings = bindings.map {
        case (control, bindable) => {
          if (bindable.isInstanceOf[bitwig.Parameter]) bindable.asInstanceOf[bitwig.Parameter].setIndication(true)
          control.addBinding(bindable)
        }
      }
      currentlyBound = bindings.map(_._2)
    }
  }

  val setAction = ext.createAction(
    () => s"Set $name Mode",
    () => ctx.set(this)
  )

  val pushAction = ext.createAction(
    () => s"Push $name Mode",
    () => ctx.push(this)
  )

  val popAction = ext.createAction(
    () => s"Pop $name Mode",
    () => ctx.pop(this)
  )

  val replaceAction = ext.createAction(
    () => s"Replace With $name Mode",
    () => ctx.replace(this)
  )
}

object Mode {
  type Bindings = List[(HardwareControl[_], HardwareBindable)]

}

case class Browser(browser: bitwig.PopupBrowser) {
  val commitAction = browser.commitAction()
  val cancelAction = browser.cancelAction()
  val nextFileAction = browser.selectNextFileAction()
  val prevFileAction = browser.selectPreviousFileAction()
  browser.shouldAudition().set(false)
}

case class Bounds(x: Double, y: Double, w: Double, h: Double)
sealed abstract class HardwareControl[T <: bitwig.HardwareControl](
    controlFn: String => T,
    name: String,
    indexInGroup: Option[Int],
    bounds: Option[Bounds]
) {
  def bindingSource: HardwareBindingSource[_ <: HardwareBinding]
  val control = controlFn(name)

  def clearBindings() = bindingSource.clearBindings()
  def addBinding(binding: HardwareBindable) = bindingSource.addBinding(binding)
  def setName(name: String) = control.setName(name)

  indexInGroup.foreach(control.setIndexInGroup)
  control.setName(name)
  control.setLabel(name)
  bounds.foreach(bounds => control.setBounds(bounds.x, bounds.y, bounds.w, bounds.h))
}

case class ActionMatcher(
    kind: ActionMatcher.ActionKind,
    number: Int,
    channel: Option[Int] = None,
    value: Option[Int] = None
) {
  override def toString() = {
    val statusByte = kind match {
      case ActionMatcher.Note => ShortMidiMessage.NOTE_ON
      case ActionMatcher.CC   => ShortMidiMessage.CONTROL_CHANGE
      case ActionMatcher.PC   => ShortMidiMessage.PROGRAM_CHANGE
    }
    val statusCond = channel match {
      case Some(channel) => s"${statusByte + channel} == status"
      case None          => s"$statusByte <= status && status < ${statusByte + 16}"
    }
    val data2Cond = (kind, value) match {
      case (ActionMatcher.PC, _) => "true"
      case (_, Some(value))      => s"data2 == $value"
      case _                     => "data2 > 0"
    }

    s"$statusCond && data1 == $number && $data2Cond"
  }
}

object ActionMatcher {
  sealed trait ActionKind
  case object Note extends ActionKind
  case object CC extends ActionKind
  case object PC extends ActionKind
}

case class HardwareButton(
    name: String,
    actionMatcher: ActionMatcher,
    pressureActionMatcher: Option[String] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn
) extends HardwareControl(surface.createHardwareButton, name, indexInGroup, bounds) {
  override def bindingSource = control.pressedAction

  val onMatcher = midiPort.createActionMatcher(actionMatcher.toString)
  val nullMatcher = midiPort.createCCActionMatcher(15, 0, 0)

  control.pressedAction.setActionMatcher(onMatcher)
}

case class AbsoluteHardwareKnob(
    name: String,
    ccNum: Int,
    channel: Option[Int] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn
) extends HardwareControl(surface.createAbsoluteHardwareKnob, name, indexInGroup, bounds) {
  override def bindingSource = control

  val onMatcher = channel match {
    case Some(channel) => midiPort.createAbsoluteCCValueMatcher(channel, ccNum)
    case None          => midiPort.createAbsoluteCCValueMatcher(ccNum)
  }
  val nullMatcher = midiPort.createAbsoluteCCValueMatcher(0, 0)

  control.setAdjustValueMatcher(onMatcher)
}

case class RelativeHardwareKnob(
    name: String,
    ccNum: Int,
    channel: Option[Int] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn
) extends HardwareControl(surface.createRelativeHardwareKnob, name, indexInGroup, bounds) {
  override def bindingSource = control

  val onMatcher = midiPort.createRelative2sComplementValueMatcher(
    channel match {
      case Some(channel) => midiPort.createAbsoluteCCValueMatcher(channel, ccNum)
      case None          => midiPort.createAbsoluteCCValueMatcher(ccNum)
    },
    127
  )
  val nullMatcher = midiPort.createRelative2sComplementValueMatcher(midiPort.createAbsoluteCCValueMatcher(0, 0), 127)

  control.setAdjustValueMatcher(onMatcher)
}

trait Setting[+T] {
  def setEnabled(enabled: Boolean)
  def setShown(shown: Boolean)
  def get(): T
  def onChanged(callback: (T) => Unit)

  def setEnabledAndShown(enabledAndShown: Boolean) = { setEnabled(enabledAndShown); setShown(enabledAndShown) }
  def enableAndShow() = setEnabledAndShown(true)
  def disableAndHide() = setEnabledAndShown(false)

  def map[T2](fn: T => T2) = {
    val orig = this
    new ProxySetting[T2] {
      override def get() = fn(orig.get)
      override val proxied = List(orig)
    }
  }
}

trait WritableSetting[T] extends Setting[T] {
  def set(value: T)
}

abstract class ProxySetting[T] extends Setting[T] {
  def proxied: List[Setting[_ <: Any]]
  override def setEnabled(enabled: Boolean) = proxied.foreach(_.setEnabled(enabled))
  override def setShown(shown: Boolean) = proxied.foreach(_.setShown(shown))
  override def onChanged(callback: T => Unit) = proxied.foreach(_.onChanged(_ => callback(get)))
}

case class PairSetting[A, B](a: Setting[A], b: Setting[B])(implicit settingCtx: SettingCtx)
    extends ProxySetting[(A, B)] {
  override def proxied: List[Setting[_]] = List(a, b)
  override def get() = (a.get, b.get)
}

case class SettingCtx(
    val category: String,
    indentationLevel: Int = 0,
    blankLabelMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map().withDefaultValue(0)
)(implicit val settings: Settings, val extension: MyControllerExtension) {
  def indented = SettingCtx(category, indentationLevel + 1, blankLabelMap)

  def uniqueify(name: String) = {
    blankLabelMap.update(name, blankLabelMap(name) + 1)
    (" " * indentationLevel) + name + (" " * blankLabelMap(name))
  }
}

case class ConstantSetting[T](value: T) extends Setting[T] {
  override def setEnabled(enabled: Boolean) = {}
  override def setShown(shown: Boolean) = {}

  override def get() = value
  override def onChanged(callback: T => Unit) = {}
}

abstract class WrapperSetting[T] extends WritableSetting[T] {
  def setting: bitwig.Setting

  def setEnabled(enabled: Boolean) =
    if (enabled) setting.enable else setting.disable
  def setShown(shown: Boolean) =
    if (shown) setting.show else setting.hide
}

case class EnumSetting(name: String, possibleValues: List[String], initialValue: String)(implicit
    settingCtx: SettingCtx
) extends WrapperSetting[String] {
  val enumValue = settingCtx.settings.getEnumSetting(
    settingCtx.uniqueify(name),
    settingCtx.category,
    possibleValues.toArray,
    initialValue
  )
  val setting = enumValue.asInstanceOf[bitwig.Setting]

  override def get() = enumValue.get()
  override def set(s: String) = enumValue.set(s)
  override def onChanged(callback: String => Unit) = enumValue.addValueObserver(new EnumValueChangedCallback {
    override def valueChanged(newValue: String) = callback(newValue)
  })
}

abstract class NumberSetting[T](
    name: String,
    unit: String,
    min: Double,
    max: Double,
    stepSize: Double,
    init: Double
)(implicit settingCtx: SettingCtx)
    extends WrapperSetting[T] {
  val rangedValue = settingCtx.settings.getNumberSetting(
    settingCtx.uniqueify(name),
    settingCtx.category,
    min,
    max,
    stepSize,
    unit,
    init
  )
  val setting = rangedValue.asInstanceOf[bitwig.Setting]

  def convert(raw: Double): T
  def invert(processed: T): Double
  def get() = convert(rangedValue.getRaw)
  def set(v: T) = rangedValue.setRaw(invert(v))

  override def onChanged(callback: T => Unit) = rangedValue.addRawValueObserver(new DoubleValueChangedCallback {
    override def valueChanged(newValue: Double) = callback(convert(newValue))
  })
}

case class IntSetting(name: String, unit: String, min: Int, max: Int, stepSize: Int, init: Int)(implicit
    settingCtx: SettingCtx
) extends NumberSetting[Int](name, unit, min, max, stepSize, init) {
  override def convert(raw: Double): Int = raw.round.toInt
  override def invert(processed: Int): Double = processed
}

case class BooleanSetting(name: String, init: Boolean)(implicit settingCtx: SettingCtx)
    extends WrapperSetting[Boolean] {
  val booleanValue = settingCtx.settings.getBooleanSetting(name, settingCtx.category, init)
  val setting = booleanValue.asInstanceOf[bitwig.Setting]

  override def get() = booleanValue.get
  override def set(value: Boolean) = booleanValue.set(value)
  override def onChanged(callback: Boolean => Unit) = booleanValue.addValueObserver(new BooleanValueChangedCallback {
    override def valueChanged(newValue: Boolean) = callback(newValue)
  })
}

case class StringSetting(name: String, init: String)(implicit settingCtx: SettingCtx) extends WrapperSetting[String] {
  val stringValue = settingCtx.settings.getStringSetting(name, settingCtx.category, 128, init)
  val setting = stringValue.asInstanceOf[bitwig.Setting]

  def get() = stringValue.get()
  def set(s: String) = stringValue.set(s)
  override def onChanged(callback: String => Unit) = stringValue.addValueObserver(new StringValueChangedCallback {
    override def valueChanged(newValue: String) = callback(newValue)
  })
}

case class SignalSetting(name: String)(implicit settingCtx: SettingCtx) extends WrapperSetting[Unit] {
  val signal = settingCtx.settings.getSignalSetting(settingCtx.uniqueify(" "), settingCtx.category, name)
  val setting = signal.asInstanceOf[bitwig.Setting]

  def get() = {}
  def set(v: Unit) = {}
  override def onChanged(callback: Unit => Unit) = signal.addSignalObserver(new NoArgsCallback {
    override def call() = callback()
  })
}

case class HeaderSetting(text: String)(implicit settingCtx: SettingCtx) extends Setting[Unit] {
  val signal = SignalSetting(text)
  signal.setEnabled(false)

  override def get() = {}
  override def onChanged(callback: Unit => Unit) = {}
  override def setEnabled(enabled: Boolean) = {}
  override def setShown(shown: Boolean) = signal.setShown(shown)
}

abstract class Value[T](value: bitwig.Value[_ <: ValueChangedCallback]) {
  value.markInterested()

  protected def addValueObserver(callback: T => Unit)
  def onChanged(callback: T => Unit) = addValueObserver(callback)
  def get(): T
  def apply() = get()
}

trait SettableValue[T] extends Value[T] {
  def set(newValue: T)
}

case class Parameter(param: bitwig.Parameter) extends Value[Double](param) {
  override def addValueObserver(callback: Double => Unit) = param.addRawValueObserver(
    new DoubleValueChangedCallback {
      override def valueChanged(newValue: Double) = callback(newValue)
    }
  )
  override def get() = param.get
}

class IntValue(value: bitwig.IntegerValue) extends Value[Int](value) {
  override def get() = value.get
  override def addValueObserver(callback: Int => Unit) = value.addValueObserver(
    new IntegerValueChangedCallback {
      override def valueChanged(newValue: Int) = callback(newValue)
    }
  )
}

case class SettableIntValue(value: bitwig.SettableIntegerValue) extends IntValue(value) with SettableValue[Int] {
  override def set(newValue: Int) = value.set(newValue)
}

class BoolValue(value: bitwig.BooleanValue) extends Value[Boolean](value) {
  override def get() = value.get
  override def addValueObserver(callback: Boolean => Unit) = value.addValueObserver(
    new BooleanValueChangedCallback {
      override def valueChanged(newValue: Boolean) = callback(newValue)
    }
  )
}

case class SettableBoolValue(value: bitwig.SettableBooleanValue) extends BoolValue(value) with SettableValue[Boolean] {
  override def set(newValue: Boolean) = value.set(newValue)
  val toggleAction = value.toggleAction()
}

class StringValue(value: bitwig.StringValue) extends Value[String](value) {
  override def get() = value.get
  override def addValueObserver(callback: String => Unit) = value.addValueObserver(
    new StringValueChangedCallback {
      override def valueChanged(newValue: String) = callback(newValue)
    }
  )
}

case class SettableStringValue(value: bitwig.SettableStringValue)
    extends StringValue(value)
    with SettableValue[String] {
  override def set(newValue: String) = value.set(newValue)
}

case class TrackBank(bank: bitwig.TrackBank)(implicit ext: MyControllerExtension) {
  val tracks = List.range(0, bank.getSizeOfBank).map(idx => new Track(bank.getItemAt(idx)))

  def bankSize = tracks.size

  val channelCount = new IntValue(bank.channelCount)

  def currentNumberOfTracks = channelCount.get
}

class Channel(val channel: bitwig.Channel)(implicit ext: MyControllerExtension) {
  val volume = Parameter(channel.volume)
  val pan = Parameter(channel.pan)
  val mute = SettableBoolValue(channel.mute)
  val solo = SettableBoolValue(channel.solo)
  val name = SettableStringValue(channel.name)
  val active = SettableBoolValue(channel.isActivated)

  def select() = channel.selectInMixer()
  val selectAction = ext.createAction(() => name.get(), select)

  def createBank(size: Int, matcher: Option[DeviceMatcher] = None) = DeviceBank(channel.createDeviceBank(1), matcher)
  def firstDeviceMatching(matcher: DeviceMatcher) = createBank(1, Some(matcher)).deviceAt(0)

  val instrumentDevice = firstDeviceMatching(ext.host.createInstrumentMatcher())
  val fxDevice = firstDeviceMatching(ext.host.createAudioEffectMatcher())
}

class Track(val track: bitwig.Track)(implicit ext: MyControllerExtension) extends Channel(track) {
  val trackType = new StringValue(track.trackType)
  val arm = SettableBoolValue(track.arm)
  val clipSlot = ClipSlot(track.clipLauncherSlotBank().getItemAt(0))
  def sendBank = if (!Set("Master", "Effect").contains(trackType.get())) track.sendBank() else null

  val cursorDevice = Device(track.createCursorDevice())
}

case class ClipSlot(clipSlot: bitwig.ClipLauncherSlot)(implicit extension: MyControllerExtension) {
  val isRecording = new BoolValue(clipSlot.isRecording)
  val exists = new BoolValue(clipSlot.exists)

  private var hasContentCallback = Option.empty[Boolean => Unit]
  clipSlot.hasContent.addValueObserver(new BooleanValueChangedCallback {
    override def valueChanged(newValue: Boolean): Unit = {
      val oldCallback = hasContentCallback
      hasContentCallback = Option.empty
      oldCallback.foreach(_(newValue))
    }
  })
  def doAfterHasContentChanges(callback: Boolean => Unit) {
    hasContentCallback = Option(hasContentCallback match {
      case Some(prevCallback) => (hasContent) => { prevCallback(hasContent); callback(hasContent) }
      case None               => callback
    })
  }

  private var isPlayingCallback = Option.empty[Boolean => Unit]
  clipSlot.isPlaying.addValueObserver(new BooleanValueChangedCallback {
    override def valueChanged(newValue: Boolean): Unit = {
      val oldCallback = isPlayingCallback
      isPlayingCallback = Option.empty
      oldCallback.foreach(_(newValue))
    }
  })
  def doAfterPlayingChanges(callback: Boolean => Unit) {
    isPlayingCallback = Option(isPlayingCallback match {
      case Some(prevCallback) => (isPlaying) => { prevCallback(isPlaying); callback(isPlaying) }
      case None               => callback
    })
  }
}

case class TrackCursor(override val track: CursorTrack)(implicit extension: MyControllerExtension)
    extends Track(track) {
  val hasNext = new BoolValue(track.hasNext)
  val isGroup = new BoolValue(track.isGroup)
  val isGroupExpanded = new SettableBoolValue(track.isGroupExpanded)
  val position = new IntValue(track.position)
  val isPinned = SettableBoolValue(track.isPinned)

  val selectPreviousAction = track.selectPreviousAction()
  val selectNextAction = track.selectNextAction()

  val parent = new Track(track.createParentTrack(extension.numSends, 1))

  val clipCursor = track.createLauncherCursorClip("CLIP", "Clip", 0, 0)
  clipCursor.getLoopStart.markInterested
  clipCursor.getLoopLength.markInterested

  val clipCursorSlot = ClipSlot(clipCursor.clipLauncherSlot)

  def loopLastNBarsOfRecordingClip(bars: Int): Unit = {
    if (!clipCursorSlot.exists()) {
      clipCursor.selectFirst()
    }
    if (!clipCursorSlot.isRecording.get) return
    val beatsPerBar = extension.transport.beatsPerBar

    val loopLength = clipCursor.getLoopLength.get + extension.transport.remainder
    val endTime = (loopLength / beatsPerBar).round * beatsPerBar
    val startTime = endTime - bars * beatsPerBar
    clipCursor.getLoopStart.set(startTime)
    clipCursor.getPlayStart.set(startTime + loopLength - endTime)
    clipCursor.getLoopLength.set(endTime - startTime)
    clipCursorSlot.doAfterPlayingChanges(_ => {
      clipCursor.getLoopLength.set(endTime - startTime)
    })
    if (endTime > loopLength) {
      clipCursorSlot.clipSlot.launchWithOptions("1", "play_with_quantization")
    } else {
      clipCursorSlot.clipSlot.launchWithOptions("none", "continue_with_quantization")
    }
  }

  def cycleNextTrack() = {
    if (track.hasNext.get) track.selectNext() else track.selectFirst()
  }
}

case class DeviceBank(bank: bitwig.DeviceBank, matcher: Option[bitwig.DeviceMatcher] = None)(implicit
    ext: MyControllerExtension
) {
  matcher.foreach(bank.setDeviceMatcher(_))

  def deviceAt(index: Int) = Device(bank.getDevice(index))
}

case class Device(device: bitwig.Device)(implicit ext: MyControllerExtension) {
  val remoteControls = RemoteControls(device.createCursorRemoteControlsPage(8))
  val replaceAction = device.replaceDeviceInsertionPoint.browseAction()
  val exists = new BoolValue(device.exists())

  lazy val selector = Selector(device.createChainSelector(), this)
  lazy val layers = {
    val bank = device.createLayerBank(8)
    (0 until 8).map(idx => new Channel(bank.getItemAt(0)))
  }

  def createTaggedRemoteControlsPage(name: String, tag: String) = RemoteControls(
    device.createCursorRemoteControlsPage(name, 8, tag)
  )
}

case class DeviceLayer(layer: bitwig.DeviceLayer)(implicit ext: MyControllerExtension) extends Channel(layer)

case class Selector(selector: bitwig.ChainSelector, device: Device)(implicit ext: MyControllerExtension) {
  val activeChain = DeviceLayer(selector.activeChain())
  val activeChainIndex = SettableIntValue(selector.activeChainIndex())
  val activeIndexParam = InstrumentSelector.of(device).index
  val chainCount = new IntValue(selector.chainCount())
  val exists = new BoolValue(selector.exists())
}

case class RemoteControls(remoteControls: CursorRemoteControlsPage)(implicit ext: MyControllerExtension) {
  val controls = (0 until 8).map(remoteControls.getParameter)
  val selectedPage = SettableIntValue(remoteControls.selectedPageIndex())

  def apply(idx: Int) = controls(idx)

  val selectPageAction =
    (0 until 8).map(idx => ext.createAction(() => s"Select Page ${idx + 1}", () => selectedPage.set(idx)))
}

object Device extends BindingProvider {
  override def createBindings()(implicit ext: MyControllerExtension) = List(
    ParameterizedParamBinding[(Device, Int)](
      "Device - Remote Control",
      ctx => PairSetting(createSetting(ctx), IntSetting("Control Number", "", 1, 8, 1, 1)(ctx))(ctx),
      { case (device, number) => device.remoteControls(number) }
    )
  )

  def createSetting(ctx: SettingCtx) = {
    new ProxySetting[Device] {
      val track = Track.trackSelectorSetting("Track")(ctx)
      override def get() = track.get.cursorDevice
      override def proxied = List(track)
      override def onChanged(callback: Device => Unit) = {
        track.onChanged(_ => callback(get))
        track.get.trackType.onChanged(_ => callback(get))
      }
    }
  }
}

object Track extends BindingProvider {
  sealed trait PostAction { def name: String; def setting = (settingCtx: SettingCtx) => ConstantSetting(this) }
  case object None extends PostAction { def name = "None" }
  case object Duplicate extends PostAction { def name = "Record Into New Track" }

  def createBindings()(implicit ext: MyControllerExtension) = List(
    ParameterizedCallbackBinding[TrackCursor](
      "Track - Cycle Next",
      ctx => trackCursorSelectorSetting("Track")(ctx),
      _.cycleNextTrack
    ),
    ParameterizedCallbackBinding[(((TrackCursor, Int), PostAction), Int)](
      "Track - Loop Recording Clip",
      (ctx) => {
        implicit val settingCtx = ctx
        PairSetting(
          PairSetting(
            PairSetting(trackCursorSelectorSetting("Track Cursor"), IntSetting("Num Bars", "", 1, 16, 1, 1)),
            SelectorSetting[PostAction](
              "Post Action",
              List(None, Duplicate).map(a => a.name -> a.setting).toMap,
              None.name
            )
          ),
          IntSetting("FX Send", "", 0, ctx.extension.numSends, 1, 0)
        )
      },
      {
        case (((track, bars), action), fxSend) => {
          track.loopLastNBarsOfRecordingClip(bars)
          if (fxSend > 0) {
            val send = track.sendBank.getItemAt(fxSend - 1)
            send.setImmediately(track.volume.get)
            send.sendMode().set("PRE")
            track.mute.set(true)
          }
          action match {
            case None =>
            case Duplicate => {
              val pinnedBefore = track.track.isPinned.get
              track.track.isPinned.set(true)
              track.track.arm().set(false)
              track.clipCursorSlot.doAfterPlayingChanges(_ => {
                track.track.duplicate()
                track.track.selectNext()
                track.name.set(s"${track.name.get.dropRight(2)} ${track.name.get.takeRight(2).trim.toInt + 1}")
                track.arm.set(true)
                track.mute.set(false)
                List.range(0, ext.numSends).map(track.sendBank.getItemAt(_)).foreach(_.reset())
                track.clipCursorSlot.clipSlot.deleteObject()
                track.clipCursorSlot.doAfterHasContentChanges(_ => {
                  track.clipSlot.clipSlot.record()
                  track.clipSlot.doAfterHasContentChanges(_ => {
                    track.clipSlot.doAfterHasContentChanges(_ => {
                      track.clipCursor.selectFirst()
                    })
                  })
                })
                track.track.isPinned.set(pinnedBefore)
              })
            }
          }
        }
      }
    ),
    ParameterizedParamBinding[Track](
      "Track - Volume",
      trackSelectorSetting("Track"),
      track => track.volume.param
    ),
    CallbackBinding(
      "Global - Record Clips For Armed Tracks",
      () => {
        ext.mainTrackBank.tracks.foreach(track => {
          if (track.arm.get) {
            track.clipSlot.clipSlot.record()
          }
        })
      }
    )
  )

  def trackSelectorSetting(name: String) = (settingCtx: SettingCtx) =>
    SelectorSetting[Track](
      name,
      Map(
        "Track Cursor" -> (ctx => trackCursorSelectorSetting("Cursor #")(ctx)),
        "Track Cursor Parent" -> (ctx => trackCursorSelectorSetting("Cursor #")(ctx).map(_.parent)),
        "Master Track" -> ((_: SettingCtx) => ConstantSetting(settingCtx.extension.masterTrack)),
        "Selected Track" -> ((_: SettingCtx) => ConstantSetting(settingCtx.extension.trackCursors(0)))
      ),
      "Track Cursor"
    )(settingCtx)

  def trackCursorSelectorSetting(name: String)(implicit settingCtx: SettingCtx) =
    IntSetting(name, "", 1, settingCtx.extension.trackCursors.length, 1, 1).map[TrackCursor](idx =>
      settingCtx.extension.trackCursors(idx - 1)
    )
}

case class SelectorSetting[T](
    name: String,
    possibleSelections: Map[String, SettingCtx => Setting[T]],
    initialSelection: String
)(implicit settingCtx: SettingCtx)
    extends WritableSetting[T] {
  val selection = EnumSetting(name, possibleSelections.keys.toList.sorted, initialSelection)
  val selectionConfigs = possibleSelections.map({ case (name, ctor) => name -> ctor(settingCtx.indented) }).toMap
  selectionConfigs.values.foreach(_.disableAndHide)
  selectionConfigs.get(selection.get).foreach(_.enableAndShow)

  var firstTime = true
  selection.onChanged(newselection => {
    if (!firstTime) {
      selectionConfigs.values.foreach(_.disableAndHide)
      selectionConfigs.get(newselection).foreach(_.enableAndShow)
    }
    firstTime = false
  })

  override def get() = selectionConfigs(selection.get).get
  override def set(newSelection: T) = selection.set(
    selectionConfigs
      .find({ case (name, config) => config.get == selection })
      .getOrElse(throw new Exception(s"couldn't find $newSelection!!"))
      ._1
  )

  override def setEnabled(enabled: Boolean) = {
    selection.setEnabled(enabled)
    selectionConfigs.values.foreach(_.setEnabled(false))
    selectionConfigs.get(selection.get).foreach(_.setEnabled(enabled))
  }
  override def setShown(shown: Boolean) = {
    selection.setShown(shown)
    selectionConfigs.values.foreach(_.setShown(false))
    selectionConfigs.get(selection.get).foreach(_.setShown(shown))
  }
  override def onChanged(callback: T => Unit) = {
    (List(selection) ++ selectionConfigs.values).foreach(_.onChanged(_ => callback(get)))
  }
}

sealed trait BindableSpec {
  def isAction: Boolean
  def createSetting()(implicit settingCtx: SettingCtx): Setting[HardwareBindable]
  def name: String
}

object BindableSpec {
  def selectorSetting(name: String, possibleBindings: List[BindableSpec], initialBinding: Option[BindableSpec])(implicit
      settingCtx: SettingCtx
  ): WritableSetting[Option[HardwareBindable]] = {
    SelectorSetting(
      name,
      possibleBindings
        .map(binding => (binding.name -> ((ctx: SettingCtx) => binding.createSetting()(ctx).map(Option(_)))))
        .toMap ++ Map(
        noneBinding -> ((ctx: SettingCtx) => ConstantSetting(Option.empty[HardwareBindable]))
      ),
      noneBinding
    )

  }

  val noneBinding = "None"

  // var numActionListBindings = 0
  // def actionListBinding(possibleBindings: List[Binding], initialBinding: Option[Binding], depth: Int) = (ctx: SettingCtx) => {
  //   ParameterizedCallbackBinding[List[Option[HardwareBindable]]](
  //   "Meta - Action List",
  //   ctx => ListSetting(1, 10, Option.empty[HardwareBindable], (_) => selectorSetting(" ", possibleBindings, initialBinding, depth + 1)(ctx))(ctx),
  //   (bindings) => bindings.foreach(_.foreach(_.asInstanceOf[HardwareActionBindable].invoke()))
  //   )(ctx.extension)
  // }
}

trait BindingProvider {
  def createBindings()(implicit ext: MyControllerExtension): List[BindableSpec]
}

case class WrapperActionBinding(override val name: String, binding: bitwig.HardwareActionBindable)
    extends BindableSpec {
  override def isAction = true
  override def createSetting()(implicit ctx: SettingCtx) = ConstantSetting(binding)
}

case class WrapperParamBinding(override val name: String, binding: bitwig.Parameter) extends BindableSpec {
  override def isAction = false
  binding.markInterested

  override def createSetting()(implicit ctx: SettingCtx) = ConstantSetting(binding)
}

case class CallbackBinding(override val name: String, action: () => Unit)(implicit ext: MyControllerExtension)
    extends BindableSpec {
  override def isAction = true

  val hardwareAction = ext.createAction(() => name, action)
  override def createSetting()(implicit ctx: SettingCtx) = ConstantSetting(hardwareAction)
}

case class ParameterizedCallbackBinding[P](
    override val name: String,
    createParamSetting: SettingCtx => Setting[P],
    action: P => Unit
)(implicit ext: MyControllerExtension)
    extends BindableSpec {
  override def isAction = true

  override def createSetting()(implicit settingCtx: SettingCtx) = {
    new Setting[HardwareBindable] {
      val configSetting = createParamSetting(settingCtx)
      val rawAction = ext.createAction(() => name, () => action(configSetting.get))
      override def get() = rawAction
      override def setEnabled(enabled: Boolean) = configSetting.setEnabled(enabled)
      override def setShown(shown: Boolean) = configSetting.setShown(shown)
      override def onChanged(callback: HardwareBindable => Unit) = configSetting.onChanged(_ => callback(rawAction))
    }
  }
}

case class ParameterizedParamBinding[P](
    override val name: String,
    createParamSetting: SettingCtx => Setting[P],
    param: P => HardwareBindable
)(implicit ext: MyControllerExtension)
    extends BindableSpec {
  override def isAction = true

  override def createSetting()(implicit settingCtx: SettingCtx) =
    createParamSetting(settingCtx).map(param)
}

case class Transport(transport: bitwig.Transport) extends BindingProvider {
  val tempo = Parameter(transport.tempo)
  val playAction = transport.playAction()
  val stopAction = transport.stopAction()
  val recordAction = transport.recordAction()
  val tapTempoAction = transport.tapTempoAction()

  def createBindings()(implicit ext: MyControllerExtension): List[BindableSpec] = List(
    WrapperActionBinding("Transport - Toggle Play", playAction),
    WrapperActionBinding("Transport - Stop/Reset", stopAction),
    WrapperActionBinding("Transport - Toggle Record", recordAction),
    WrapperActionBinding("Transport - Tap Tempo", tapTempoAction),
    WrapperParamBinding("Transport - Tempo", transport.tempo)
  )

  def beatsPerBar = transport.timeSignature.numerator.get
  transport.timeSignature.numerator.markInterested

  def remainder = transport.playPosition.get - (transport.playPosition.get * 4).floor / 4

  var isPlaying = false
  var currentBar = 0
  var currentBeat = 0

  def onTransportTick(callback: (Int, Int, Boolean) => Unit) {
    transport.isPlaying.addValueObserver(new BooleanValueChangedCallback {
      override def valueChanged(newValue: Boolean) = {
        isPlaying = newValue; callback(currentBar, currentBeat, isPlaying)
      }
    })
    transport.playPosition.addValueObserver(new DoubleValueChangedCallback {
      override def valueChanged(newValue: Double) = {
        currentBar = (newValue / transport.timeSignature.numerator.get).floor.toInt
        currentBeat = newValue.floor.toInt - currentBar * transport.timeSignature.numerator.get
        callback(currentBar, currentBeat, isPlaying)
      }
    })
  }
}

case class ControllerExtensionProxy(
    extCtor: ControllerHost => MyControllerExtension,
    defn: ControllerExtensionDefinition,
    host: ControllerHost
) extends ControllerExtension(defn, host) {
  lazy val ext = extCtor(host)
  override def init() = {
    ext
  }

  override def exit() = {
    ext.exit()
  }

  override def flush() = {
    ext.flush()
  }
}

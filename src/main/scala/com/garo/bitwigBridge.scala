package com.garo

import scala.language.implicitConversions

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._
import com.bitwig.extension.controller.{api => bitwig}
import java.util.function.Supplier
import _root_.java.sql.Wrapper

abstract class MyControllerExtension(implicit val host: ControllerHost) {
  implicit val extension = this
  val transport = MyTransport(host.createTransport())
  val arranger = host.createArranger()
  val application = host.createApplication()
  val preferences = host.getPreferences()
  val documentState = host.getDocumentState()
  val mainTrackBank = host.createMainTrackBank(2, 0, 1)
  mainTrackBank.channelCount.markInterested
  val track = MyTrack(host.createCursorTrack("TRACK", "Track", 0, 0, true))
  val masterTrack = host.createMasterTrack(0)
  masterTrack.volume.markInterested

  val blockSize = preferences.getNumberSetting("Max Block Size", "Audio", 1, 50, 1, "ms", 24)

  val doNothingAction = createAction("None", () => {})

  def exit() = {}
  def flush() = {}

  def postFrame(action: () => Unit) =
    new java.util.Timer().schedule(
      new java.util.TimerTask {
        def run() = action()
      },
      blockSize.getRaw.round
    )

  def adjust(param: Parameter, value: Double, range: (Double, Double)) = {
    val (rangeLow, rangeHigh) = range
    val rangeSize = rangeHigh - rangeLow
    val scaledValue = value / 127 * rangeSize + rangeLow
    if (127 * (param.getRaw - scaledValue).abs / rangeSize <= 2) param.setRaw(scaledValue)
  }

  def createAction(name: String, action: () => Unit) =
    host.createAction(
      new Runnable {
        override def run() = action()
      },
      new Supplier[String] {
        override def get(): String = name
      }
    )
}

trait Setting[T] {
  def setEnabled(enabled: Boolean)
  def setShown(shown: Boolean)
  def get(): T
  def set(value: T)
  def onChanged(callback: (T) => Unit)

  def setEnabledAndShown(enabledAndShown: Boolean) = { setEnabled(enabledAndShown); setShown(enabledAndShown) }
  def enableAndShow() = setEnabledAndShown(true)
  def disableAndHide() = setEnabledAndShown(false)
}

case class SettingCtx(val category: String)(implicit val settings: Settings, val extension: MyControllerExtension) {
  var numBlankLabels = 0
  def getBlankLabel() = {
    numBlankLabels = numBlankLabels + 1
    " " * numBlankLabels
  }
}

abstract class WrapperSetting[T] extends Setting[T] {
  def setting: bitwig.Setting

  def setEnabled(enabled: Boolean) =
    if (enabled) setting.enable else setting.disable
  def setShown(shown: Boolean) =
    if (shown) setting.show else setting.hide
}

case class EnumSetting(name: String, possibleValues: List[String], initialValue: String)(implicit
    settingCtx: SettingCtx
) extends WrapperSetting[String] {
  val enumValue = settingCtx.settings.getEnumSetting(name, settingCtx.category, possibleValues.toArray, initialValue)
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
  val rangedValue = settingCtx.settings.getNumberSetting(name, settingCtx.category, min, max, stepSize, unit, init)
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
  val signal = settingCtx.settings.getSignalSetting(settingCtx.getBlankLabel(), settingCtx.category, name)
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
  override def set(value: Unit) = {}
  override def onChanged(callback: Unit => Unit) = {}
  override def setEnabled(enabled: Boolean) = {}
  override def setShown(shown: Boolean) = signal.setShown(shown)
}

case class ListSetting[T, V <: Setting[T]](
    initialLength: Int,
    maxSize: Int,
    defaultElementValue: T,
    ctor: (Int) => V
)(implicit settingCtx: SettingCtx)
    extends Setting[List[T]] {
  val length = IntSetting("Length", "", 0, maxSize, 1, initialLength)
  length.disableAndHide()

  val settingList = List.range(0, maxSize).map(ctor)
  settingList.drop(length.get).foreach(_.disableAndHide)

  val addNewButton = SignalSetting("Add Element")
  addNewButton.onChanged((_) => {
    set((get().take(itemToAddOrRemove.get - 1) :+ defaultElementValue) ++ get().drop(itemToAddOrRemove.get - 1))
  })
  val removeButton = SignalSetting("Remove Element")
  removeButton.onChanged((_) => {
    set(get().take(itemToAddOrRemove.get - 1) ++ get().drop(itemToAddOrRemove.get))
  })
  val itemToAddOrRemove = IntSetting("Item To Add Or Remove", "", 1, maxSize, 1, 1)

  def get() = settingList.take(length.get).map(_.get)
  def set(list: List[T]) = {
    if (list.length > maxSize) {
      throw new Exception()
    }

    settingList.zipWithIndex.foreach({
      case (setting, index) => { setting.setEnabledAndShown(index < list.length) }
    })
    settingList
      .zip(list)
      .foreach({
        case (setting, newValue) => {
          setting.set(newValue)
        }
      })

    length.set(list.length)
    addNewButton.setEnabledAndShown(list.length < maxSize)
    removeButton.setEnabledAndShown(list.length > 0)
    itemToAddOrRemove.setEnabledAndShown(list.length > 0)
  }

  def setEnabled(enabled: Boolean): Unit = {
    settingList.zipWithIndex.foreach({
      case (setting, index) => { setting.setEnabled(enabled && index < length.get) }
    })
  }

  def setShown(shown: Boolean): Unit = {
    settingList.zipWithIndex.foreach({
      case (setting, index) => { setting.setShown(shown && index < length.get) }
    })
  }

  override def onChanged(callback: List[T] => Unit) = {
    (List(length) ++ settingList).foreach(_.onChanged(_ => callback(get())))
  }
}

case class MyTrack(track: CursorTrack)(implicit extension: MyControllerExtension) {
  track.hasNext.markInterested
  track.position.markInterested

  val clip = track.createLauncherCursorClip("CLIP", "Clip", 8, 1)
  clip.getLoopStart.markInterested
  clip.getLoopLength.markInterested

  val clipSlot = clip.clipLauncherSlot
  clipSlot.isRecording.markInterested

  var clipLengthToSet = Option.empty[Double]
  clipSlot.isPlaying.addValueObserver(new BooleanValueChangedCallback {
    override def valueChanged(newValue: Boolean): Unit = {
      clipLengthToSet match {
        case Some(length) => {
          clip.getLoopLength.set(length)
          clipLengthToSet = Option.empty
        }
        case _ => {}
      }
    }
  })

  val loopLast2BarsOfRecordingClipAction =
    extension.createAction("Loop Last 2 Bars of Recording Clip", () => loopLastNBarsOfRecordingClip(2))
  def loopLastNBarsOfRecordingClip(bars: Int): Unit = {
    if (!clipSlot.isRecording.get) return
    val beatsPerBar = extension.transport.beatsPerBar

    val loopLength = clip.getLoopLength.get
    val endTime = (loopLength / beatsPerBar).round * beatsPerBar
    val startTime = endTime - bars * extension.transport.beatsPerBar
    clip.getLoopStart.set(startTime)
    clipLengthToSet = Option(endTime - startTime)
    clipSlot.launchWithOptions("none", "continue_with_quantization")
  }

  val cycleNextAction = extension.createAction(
    "Cycle Next Track",
    () => {
      if (track.position.get < extension.mainTrackBank.channelCount.get - 1) track.selectNext else track.selectFirst
    }
  )
}

case class MyTransport(transport: Transport)(implicit host: ControllerHost) {
  val tempo = transport.tempo
  tempo.markInterested

  val playAction = transport.playAction
  val stopAction = transport.stopAction
  val recordAction = transport.recordAction
  val tapTempoAction = transport.tapTempoAction

  def beatsPerBar = transport.timeSignature.numerator.get
  transport.timeSignature.numerator.markInterested

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

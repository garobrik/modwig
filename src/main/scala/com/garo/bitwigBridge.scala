package com.garo

import scala.language.implicitConversions

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._

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

  def exit() = {}
  def flush() = {}

  def postFrame(action: () => Unit) =
    new java.util.Timer().schedule(
      new java.util.TimerTask {
        def run() = action()
      },
      { host.println(s"${blockSize.getRaw.round}"); blockSize.getRaw.round }
    )

  def adjust(param: Parameter, value: Double, range: (Double, Double)) = {
    val (rangeLow, rangeHigh) = range
    val rangeSize = rangeHigh - rangeLow
    val scaledValue = value / 127 * rangeSize + rangeLow
    host.println(s"$rangeSize, $rangeLow, $scaledValue, ${param.getRaw}")
    if (127 * (param.getRaw - scaledValue).abs / rangeSize <= 2) param.setRaw(scaledValue)
  }

  implicit class HideDisableSettings(val enumValue: Value[_ <: ValueChangedCallback]) {
    def setEnabled(enabled: Boolean) =
      if (enabled) enumValue.asInstanceOf[Setting].enable else enumValue.asInstanceOf[Setting].disable
    def setShown(shown: Boolean) = 
      if (shown) enumValue.asInstanceOf[Setting].show else enumValue.asInstanceOf[Setting].hide
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
      extension.host.println(s"clip playing $newValue")
      clipLengthToSet match {
        case Some(length) => {
          clip.getLoopLength.set(length)
          clipLengthToSet = Option.empty
        }
        case _ => {}
      }
    }
  })

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

  def cycleNext() = {
    extension.host.println(s"${extension.mainTrackBank.channelCount.get}, ${extension.mainTrackBank.getSizeOfBank}")
    if (track.position.get < extension.mainTrackBank.channelCount.get - 1) track.selectNext else track.selectFirst
  }
}

case class MyTransport(transport: Transport)(implicit host: ControllerHost) {
  transport.tempo.markInterested

  val playAction = transport.playAction

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

package bwbridge

import java.util.UUID

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._
import com.bitwig.extension.controller.{api => bitwig}

import bwbridge.ControllerExtension

class ChordalControllerExtension(using _host: ControllerHost) extends ControllerExtension {
  // lazy val noteInput = host.getMidiInPort(0).createNoteInput("Instrument", "000000")
  // lazy val doc = host.getDocumentState()

  // lazy val midiLowSetting = doc.getNumberSetting("Lowest Midi Note", "Keyboard Config", 0, 127, 1, "", 36)
  // def midiLow() = (midiLowSetting.get() * 128).round.intValue()

  // var transposeDevice: MyBitwigDevice = null
  // var instrumentTrackDevice: MyDevice = null
  // override def init() = {
  //   host.getMidiInPort(0).setMidiCallback(this)

  //   noteInput
  //   browser

  //   selectedTrack.clipCursor.slot.isRecording.onChanged(isRecording => {
  //     if (!isRecording) {
  //       val q = transport.postRecordTime()
  //       val loopLength = selectedTrack.clipCursor.loopLength()
  //       val endTime = (loopLength / q).floor * q
  //       val startTime = endTime - q
  //       selectedTrack.clipCursor.loopStart.set(startTime)
  //       selectedTrack.clipCursor.loopLength.set(q)
  //       postFrame(() => { selectedTrack.clipCursor.slot.launch() })
  //     }
  //   })

  //   midiLowSetting.addValueObserver(new DoubleValueChangedCallback {
  //     override def valueChanged(newValue: Double) = reconfigure()
  //   })
  //   transposeDevice = MyDevice.findFirstBitwigDevice(track, DeviceSpec.NoteTranspose)
  //   instrumentTrackDevice = MyDevice.deviceAt(track, 1)

  //   // TODO: Perform your driver initialization here.
  //   // For now just show a popup notification for verification that it is running.
  //   host.showPopupNotification("Welcome to The Instrument")
  // }

  // override def exit() = {
  //   // TODO: Perform any cleanup once the driver exits
  //   // For now just show a popup notification for verification that it is no longer
  //   // running.
  //   getHost().showPopupNotification("instrument Exited")
  // }

  // override def flush() = {
  //   // TODO Send any updates you need here.
  // }

  // /** Called when we receive short MIDI message on port 0. */

  // private var keyState = Seq.fill(128)(0)
  // override def midiReceived(msg: ShortMidiMessage) = {
  //   if (msg.isNoteOn || msg.isNoteOff) {
  //     val oldState = keyState
  //     keyState = oldState.updated(msg.getData1(), if (msg.isNoteOn) 1 else 0)
  //     controlStep(oldState, keyState)

  //     if (msg.getData1() >= midiLow() + chanLen + adjLen) {
  //       noteInput.sendRawMidiEvent(msg.getStatusByte(), msg.getData1(), msg.getData2())
  //     }
  //   } else {
  //     noteInput.sendRawMidiEvent(msg.getStatusByte(), msg.getData1(), msg.getData2())
  //   }
  // }

  // private val chanLen = 5
  // private val adjLen = 7
  // private def controlStep(oldState: Seq[Int], newState: Seq[Int]) = {
  //   def sliceChan(state: Seq[Int]) = state.slice(midiLow(), midiLow() + chanLen)
  //   val adjustChan = updateChans(sliceChan(oldState), sliceChan(newState))

  //   def sliceAdj(state: Seq[Int]) = state.slice(midiLow() + chanLen, midiLow() + chanLen + adjLen)
  //   doActions(adjustChan, sliceAdj(oldState), sliceAdj(newState))

  // }

  // private var chanChord = Seq.fill(chanLen)(0)
  // private var adjustChan = 0

  // def updateChans(oldChan: Seq[Int], newChan: Seq[Int]) = {
  //   chanChord = newChan.zip(chanChord).map { case (a, b) => math.max(a, b) }
  //   if (!newChan.contains(1) && oldChan.contains(1)) {
  //     val chanIdx = toBin(chanChord.takeRight(1))
  //     val chanVal = toBin(chanChord.dropRight(1)) - (if (chanIdx == 0) 1 else 0)
  //     if (chanIdx == 0) {
  //       if (track.position.get() != chanVal) {
  //         val origPos = track.position.get()
  //         track.arm().set(false)
  //         val numMoves = math.abs(chanVal - track.position().get())
  //         val move = if (track.position().get() < chanVal) () => track.selectNext() else () => track.selectPrevious()
  //         (0 until numMoves) map ((_) => move())
  //         track.arm().set(true)
  //       }
  //     } else if (chanIdx == 1) {
  //       adjustChan = chanVal
  //       host.println(s"adj: $adjustChan")
  //     }

  //     chanChord = Seq.fill(chanLen)(0)
  //   }

  //   adjustChan
  // }

  // def doActions(adjustChan: Int, oldActs: Seq[Int], newActs: Seq[Int]) = {
  //   def press(idx: Int) = oldActs(idx) == 0 && newActs(idx) > 0

  //   def onPress(idx: Int, action: () => Unit) = if (press(idx)) action()

  //   def incrRanged(
  //       idx: Int,
  //       param: SettableRangedValue,
  //       amount: Double = 0.1,
  //       pre: () => Unit = () => {},
  //       post: () => Unit = () => {}
  //   ) = {
  //     pre()
  //     onPress(idx, () => param.inc(-amount))
  //     onPress(idx + 1, () => param.inc(amount))
  //     post()
  //   }

  //   def incrDouble(
  //       idx: Int,
  //       param: SettableDoubleValue,
  //       amount: Double = 0.1,
  //       pre: () => Unit = () => {},
  //       post: () => Unit = () => {}
  //   ) = {
  //     pre()
  //     onPress(idx, () => param.inc(-amount))
  //     onPress(idx + 1, () => param.inc(amount))
  //     post()
  //   }

  //   val isInstrumentTrack = track.trackType().get() == "Instrument"
  //   val isAudioTrack = track.trackType().get() == "Audio"

  //   if (adjustChan == 0) {
  //     onPress(
  //       0,
  //       () => {
  //         if (!clipSlot.isRecording().get() && !clipSlot.isRecordingQueued().get()) {
  //           track.arm().set(true)
  //           clipSlot.record()
  //         } else {
  //           track.stop()
  //         }
  //       }
  //     )
  //     onPress(1, () => { clipSlot.deleteObject() })
  //     onPress(2, () => { track.stop() })
  //     onPress(3, () => { clipSlot.launch() })
  //     incrDouble(4, transport.getClipLauncherPostRecordingTimeOffset(), 4.0)
  //     onPress(6, () => { clip.getLoopLength.set(2) })
  //   }
  //   if (adjustChan == 1) {
  //     incrRanged(0, track.volume())
  //     if (isInstrumentTrack) {
  //       onPress(2, () => instrumentTrackDevice.prevPreset())
  //       onPress(3, () => instrumentTrackDevice.nextPreset())
  //       onPress(4, () => instrumentTrackDevice.prevCollection())
  //       onPress(5, () => instrumentTrackDevice.nextCollection())
  //       onPress(6, () => instrumentTrackDevice.commitPreset())
  //     }
  //   }
  //   // if adjust_chan == 1:
  //   //    incr_pair(0, "inst~preset", 1.0)
  //   if (adjustChan == 2) {
  //     if (isInstrumentTrack) {
  //       incrRanged(
  //         0,
  //         transposeDevice.getParameter(DeviceSpec.NoteTranspose.octaves),
  //         1.0 / 7.0,
  //         pre = () => {
  //           if (!transposeDevice.exists()) transposeDevice.insert(track.startOfDeviceChainInsertionPoint())
  //         }
  //       )
  //     }
  //   }
  // }

  // private def reconfigure() = {}

  // def postFrame(action: () => Unit)(implicit host: ControllerHost) =
  //   new java.util.Timer().schedule(
  //     new java.util.TimerTask {
  //       def run() = action()
  //     },
  //     { host.println(s"$blockSize"); blockSize }
  //   )
}

class ChordalControllerExtensionDefinition extends ControllerExtensionDefinition {
  val DRIVER_ID = UUID.fromString("aa0399c3-409a-4901-957e-cb7f214d6de9")

  override def getName = "chordal controller"

  override def getAuthor = "garo"

  override def getVersion = "0.1"

  override def getId = DRIVER_ID

  override def getHardwareVendor = "garo"

  override def getHardwareModel = "any midi keyboard"

  override def getRequiredAPIVersion = 17

  override def getNumMidiInPorts = 1

  override def getNumMidiOutPorts = 0

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
    }
  }

  override def createInstance(host: ControllerHost) =
    ControllerExtensionProxy(ChordalControllerExtension(using _), this, host)
}

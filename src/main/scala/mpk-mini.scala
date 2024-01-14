package mpkmini

import bwbridge._
import bwbridge.given
import plugins._
import com.bitwig.extension.api._
import com.bitwig.extension.controller.{ControllerExtension => _, _}
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api.{
  AbsoluteHardwareKnob => _,
  HardwareButton => _,
  RelativeHardwareKnob => _,
  Transport => _,
  Device => _,
  DeviceChain => _,
  Track => _,
  ControllerHost => _,
  SpecificDevice => _,
  Parameter => _,
  HardwareActionBindable => _,
  Action => _,
  _
}
import com.bitwig.extension.controller.{api => bitwig}
import org.http4s.blaze.server._

import java.util.UUID
import org.http4s.HttpRoutes
import org.http4s._
import org.http4s.dsl.io._
import cats.effect._
import concurrent.duration._
import org.http4s.websocket.WebSocketFrame
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import java.io.StringWriter
import java.io.PrintWriter
import bwbridge.Mode.Bindings

class MPKminiExtension(using _host: bitwig.ControllerHost) extends ControllerExtension {
  implicit val modeCtx: ModeCtx = ModeCtx()
  implicit val MPKin: MidiIn = host.getMidiInPort(0)
  implicit val surface: HardwareSurface = host.createHardwareSurface()
  surface.setPhysicalSize(600, 100)

  val MPKout = host.getMidiOutPort(0)

  val noteInput = MPKin.createNoteInput("Keys", "80????", "90????")
  val padInput = MPKin.createNoteInput("Pads", "81????", "91????", "A1????")

  def leftSix[T](l: List[T]) = l.slice(0, 3) ++ l.slice(4, 7)
  def rightSix[T](l: List[T], flip: Boolean = true) =
    if flip then l.slice(5, 8) ++ l.slice(1, 4) else l.slice(1, 4) ++ l.slice(5, 8)

  val knobs =
    List
      .range(0, 8)
      .map(idx =>
        RelativeHardwareKnob(
          s"K${idx + 1}",
          20 + idx,
          channel = Some(0),
          indexInGroup = Some(idx),
          bounds = Some(Bounds(300 + ((idx % 4) + 1) * 220 / 4, ((idx / 4) * 2 + 1) * 20, 20, 20))
        )
      )

  val bKnobs = List
    .range(8, 16)
    .map(idx => RelativeHardwareKnob(s"K${idx + 1}", 20 + idx, channel = Some(0), indexInGroup = Some(idx - 8)))

  val ccPads =
    List(20, 30).flatMap(idx => (idx + 4 until idx + 8) ++ (idx until idx + 4)).zipWithIndex.map { case (cc, idx) =>
      HardwareButton(
        s"CC${idx + 1}",
        ActionMatcher(ActionMatcher.CC, cc, channel = Some(1)),
        Some(ActionMatcher(ActionMatcher.CC, cc, value = Some(0), channel = Some(1))),
        Some(ActionMatcher(ActionMatcher.CCKnob, cc, channel = Some(1))),
        indexInGroup = Some(idx % 8),
        bounds = if (idx < 8) Some(Bounds(((idx % 4) + 1) * 220 / 4, ((idx / 4) * 2 + 1) * 20, 20, 20)) else None
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

  val notePads =
    List(0, 8).flatMap(idx => (idx + 4 until idx + 8) ++ (idx until idx + 4)).zipWithIndex.map { case (note, idx) =>
      HardwareButton(
        s"Note${idx + 1}",
        ActionMatcher(ActionMatcher.NoteOn, idx, channel = Some(1)),
        Some(ActionMatcher(ActionMatcher.NoteOff, idx, channel = Some(1))),
        indexInGroup = Some(idx % 8)
      )
    }

  val keys =
    List.range(0, 128).map { note =>
      HardwareButton(
        s"Key${note + 1}",
        ActionMatcher(ActionMatcher.NoteOn, note, channel = Some(0)),
        Some(ActionMatcher(ActionMatcher.NoteOff, note, channel = Some(0)))
      )
    }

  object joystick {
    given NoteInput = noteInput
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
    val control = HardwareButton("Pedal", ActionMatcher(ActionMatcher.CC, 64, channel = Some(0)))
    implicit val modeCtx: ModeCtx = ModeCtx()
    val modes = List(
      new Mode("Loop") {
        override def bindings = List(control -> mainModes.loops(0).triggerAction)
      },
      new Mode("Kick") {
        val kickAction = Action(
          "Kick",
          () => padInput.sendRawMidiEvent(ShortMidiMessage.NOTE_ON + 1, 0, 127)
        )
        override def bindings = List(control -> kickAction)
      }
    )

    modes.foreach(_.setAction)
    val toggleModeAction = Action(
      "Toggle Pedal Function",
      () => {
        val currentModeIdx = modes.indexOf(modeCtx.current.getOrElse(modes.head))
        val nextModeIdx = (currentModeIdx + 1) % modes.length
        modes(nextModeIdx).setAction()
      }
    )
  }
  pedal

  object metronome {
    val track = TrackCursor("METRONOME", "Metronome", true)
    val toggle = Action(
      "Metronome",
      () => {
        track.clipBank(0).launchWith(Some(Transport.LaunchQ.None), Some(Transport.LaunchMode.Synced))
        track.mute.toggle()
      }
    )
  }
  metronome

  val stompTemplates = {
    val stompTrack = host.createCursorTrack("STOMP TEMPLATE", "Stomp Template", 0, 0, true)
    val stompDevice = new Device(
      stompTrack.createCursorDevice("STOMP_TEMPLATE", "Stomp Template", 0, CursorDeviceFollowMode.FIRST_DEVICE)
    )
    stompDevice.layers.map(_.fxDevice)
  }

  object mainModes {
    val joystickBindings = AbsoluteBinding.list(
      joystick.mods.zip(selectedTrack.deviceCursor.createTaggedRemoteControlsPage("XY", "xy").controls)
    )

    object trackMode extends Mode("Track Control") {
      val pickTrackMode = new Mode("Pick Track") {
        val pickActions = mainTrackBank.tracks.map(t =>
          Action(
            t.name,
            () => { selectedTrack.selectChannel(t); trackMode.replaceAction() },
            t.isSelected.map(if _ then 1.0 else 0.0)
          )
        )

        override def dependents = super.dependents :+ mainTrackBank.channelCount
        override def bindings = ccPads
          .take(8)
          .zip(mainTrackBank.currentItems)
          .zip(pickActions)
          .map { case ((pad, track), pick) =>
            ButtonBinding(pad, pick, onLongPress = track.duplicateAndUnarm)
          }
      }

      override def bindings =
        joystickBindings ++ List(
          ButtonBinding(ccPads(0), pickTrackMode.replaceAction, onLongPress = deviceMode.replaceAction),
          ccPads(1) -> application.undo,
          ccPads(2) -> application.redo,
          ButtonBinding(ccPads(3), transport.addCue, onLongPress = transport.record.toggle),
          ButtonBinding(
            ccPads(4),
            loops(0).triggerAction,
            onLongPress = loopsMode.replaceAction
          ),
          ccPads(5) -> selectedTrack.arm.toggle,
          ButtonBinding(ccPads(6), transport.tapTempo, onLongPress = metronome.toggle),
          ButtonBinding(ccPads(7), transport.play, onLongPress = transport.playAndRecordOrStop),
          knobs(2) -> selectedTrack.pan,
          knobs(3) -> selectedTrack.volume,
          RelativeBinding(knobs(4), transport.tempo, sensitivity = 0.1),
          knobs(7) -> masterTrack.volume
        )
    }
    trackMode

    object loopsMode extends Mode("Loop Mixer") {
      override def dependents = List(loops(0).loops.currentSize)
      override def bindings =
        joystickBindings ++
          rightSix(ccPads, flip = false).zip(loops(0).loops.currentItems.dropRight(1)).map { (button, loop) =>
            ButtonBinding(button, loop.clipBank.clips(0).toggle, onLongPress = loop.delete)
          }
          ++ RelativeBinding.list(
            leftSix(knobs).zip(loops(0).loops.currentItems.dropRight(1).map(_.volume))
          ) ++ List(
            ccPads(0) -> deviceMode.replaceAction,
            ccPads(4) -> loops(0).triggerAction,
            RelativeBinding(knobs(3), transport.postRecordTime, sensitivity = 4.0),
            knobs(7) -> masterTrack.volume
          )
    }
    loopsMode

    val deviceMode: Mode = new Mode("Device Control") {
      val device = selectedTrack.deviceCursor
      val joystickPage = device.createTaggedRemoteControlsPage("XY", "xy")

      override def onEnable() = {
        device.remoteControls.visible.set(true)
      }

      override def dependents = deviceModes.map(_.specificDevice.exists)

      val pickDeviceMode = new Mode("Pick Device") {
        val selectActions =
          browser.results.itemBank.items.map(_.selectAndCommit.andThen(deviceMode.replaceAction.apply))

        val browserBindings: Mode.Bindings =
          ButtonBinding.list(ccPads.take(7).zip(selectActions)) ++ List(
            ccPads(7) -> browser.cancel.andThen(() => deviceMode.replaceAction())
          )

        val pickActions = selectedTrack.devices.devices.map(d =>
          Action(
            d.name,
            () => { device.selectDevice(d); deviceMode.replaceAction() },
            d.isSelected.map(if _ then 1.0 else 0.0)
          )
        )

        def normalBindings = ccPads
          .take(6)
          .zip(selectedTrack.devices.currentItems)
          .zip(pickActions)
          .map { case ((pad, device), pick) =>
            ButtonBinding(pad, pick, onLongPress = device.before.browseAction)
          } ++ List(
          ButtonBinding(ccPads(6), selectedTrack.start.browseAction, onLongPress = device.replace.browseAction),
          ButtonBinding(ccPads(7), selectedTrack.end.browseAction, onLongPress = device.delete)
        )

        override def dependents = super.dependents ++ selectedTrack.devices.devices.map(_.exists) ++ List(
          browser.isOpen,
          browser.results.entryCount
        )
        override def bindings = if browser.isOpen() then browserBindings else normalBindings
      }

      override def bindings = List(deviceModes.find(_.specificDevice.exists()).getOrElse(genericMode)) ++ List(
        ButtonBinding(ccPads(0), pickDeviceMode.replaceAction, onLongPress = trackMode.replaceAction),
        ButtonBinding(ccPads(4), loops(0).triggerAction, onLongPress = loopsMode.replaceAction),
        knobs(3) -> selectedTrack.volume,
        knobs(7) -> masterTrack.volume
      )

      val genericMode = new Mode {
        override def dependents =
          List(device.remoteControls.pageCount, device.remoteControls.pageNames) ++ device.remoteControls.controls.map(
            _.exists
          )
        override def bindings =
          joystickBindings ++ leftSix(knobs)
            .zip(leftSix(device.remoteControls.controls))
            .map { case (knob, control) =>
              if control.exists() then RelativeBinding(knob, control)
              else ButtonBinding(knob.asButton, control.startMapping)
            } ++ ButtonBinding.list(
            rightSix(ccPads).zip(
              device.remoteControls.selectPageAction.take(device.remoteControls.pageCount()).zipWithIndex.flatMap {
                (select, idx) =>
                  if idx < device.remoteControls.pageNames().length && device.remoteControls.pageNames()(idx) == "xy"
                  then List()
                  else List(select)
              }
            )
          )
      }

      abstract class DeviceMode[T <: SpecificDevice](val specificDevice: T) extends Mode

      val deviceModes = List[DeviceMode[_]](
        new DeviceMode(Diva.Device(device)) {
          import this.specificDevice._

          override val bindings =
            List(
              new Mode.Paged(rightSix(ccPads)) {
                override def dependents =
                  super.dependents ++ List(oscModel, hpfModel, filterModel, env1Model, env2Model)

                def pages = Mode.pages(
                  leftSix(knobs),
                  List(
                    "VCOs" -> (vcos.map(_.shape) ++ vcos.map(_.volume)),
                    "Tune" -> (vcos.map(_.tune) ++ List(vco2.sync, vco3.sync))
                  ),
                  List(
                    "Filter" -> List(filterFreq, filterResonance, filterModel, filterKeyFollow),
                    "Mod" -> List(filtModSrc, filtMod2Src, resModSrc, filtModDepth, filtMod2Depth, resModDepth)
                  ),
                  List(
                    "FM" -> List(vco1.fm, fmModSrc, fmModDepth, filterFM, filtFMModSrc, filtFMModDepth),
                    "Noise" -> List(drive, feedback, noise, voiceStack, polyMode, glide)
                  ),
                  List(
                    "Tune Mod" -> (vcos.map(_.shapeMod) ++ List(shapeModDepth, shapeModSrc)),
                    "Shape Mod" -> (vcos.map(_.tuneMod) ++ List(tuneModDepth, tuneModSrc))
                  ),
                  env.map(env =>
                    s"Env ${env.idx + 1}" -> List(
                      env.attack,
                      env.decay,
                      env.sustain,
                      env.release,
                      env.velocity,
                      env.keyFollow
                    )
                  ),
                  lfo.map(lfo =>
                    s"LFO ${lfo.idx + 1}" -> List(
                      lfo.waveform,
                      lfo.delay,
                      lfo.restart,
                      lfo.sync,
                      lfo.rate,
                      if lfo.idx == 0 then vibrato else lfo.phase
                    )
                  )
                )
              },
              joystick.upMod.CCBinding(1),
              joystick.leftMod.CCBinding(2),
              joystick.rightMod.CCBinding(11)
              // joystick.leftMod.PitchBinding(-1),
              // joystick.rightMod.PitchBinding(1),
            )
        },
        new DeviceMode(Chromaphone.Device(device)) {
          val layer = this.specificDevice.layer(0)
          import layer._

          override val bindings = List(
            new Mode.Paged(rightSix(ccPads)) {
              override def dependents = super.dependents ++ resonator
                .map(_.objectType) ++ List(
                noiseFiltType,
                noiseEnvType,
                lfoType
              )

              def pages = Mode.pages(
                leftSix(knobs),
                resonator.map(res =>
                  List(
                    s"Object ${res.name}" -> List(
                      res.objectType,
                      res.release,
                      res.decay,
                      if res.isTube then res.radius else res.density,
                      res.tone,
                      res.material
                    ),
                    "Kbd/HitPos" -> List(
                      res.lowCut,
                      res.decayKeyTrack,
                      res.decayVeloTrack,
                      res.hitPos,
                      res.hitPosVeloTrack,
                      res.hitPosRand
                    )
                  ),
                ) ++ List(
                  List(
                    "Mallet" -> List(malletStiffness, malletNoise, malletVol, malletColor, malletVolDirect),
                    "Kbd" -> List(
                      malletStiffnessKeyTrack,
                      malletNoiseKeyTrack,
                      malletVolKeyTrack,
                      malletStiffnessVeloTrack,
                      malletNoiseVeloTrack,
                      malletVolVeloTrack
                    )
                  ),
                  List(
                    "Noise" -> List(
                      noiseFiltFreq,
                      noiseDensity,
                      noiseVol,
                      noiseFiltType,
                      if noiseFiltType.displayedValue() == "HP+LP" then noiseFiltWidth else noiseFiltQ,
                      noiseVolDirect
                    ),
                    "Kbd" -> List(
                      noiseFiltFreqKey,
                      noiseDensityKey,
                      noiseVolKeyTrack,
                      noiseFiltFreqVelo,
                      noiseDensityVelo,
                      noiseVolVeloTrack
                    )
                  ),
                  List(
                    "LFO" -> List(lfoType, lfoSyncRate, lfoDelay, noiseVolLFO, noiseDensityLFO, noiseFiltFreqLFO),
                    "Env" -> List(noiseEnvA, noiseEnvD, noiseEnvS, noiseEnvR, noiseFiltFreqEnv, noiseDensityEnv)
                  ),
                  List(
                    "Balance" -> List(balance, unisonOn, coupled, balanceKeyFollow, vibratoAmount, gain),
                    "" -> List()
                  )
                )
              )
            }
          )
        },
        new DeviceMode(DrumSynth.Device(device)) {
          import this.specificDevice._

          override val bindings = List(
            new Mode.Paged(notePads.take(8)) {
              override def pages = List.range(0, 8).map { idx =>
                Mode.Page(
                  instrument(idx).displayedValue,
                  RelativeBinding.list(
                    leftSix(knobs)
                      .zip(List(instrument(idx), params(idx)(0), velocity(idx), model(idx), params(idx)(1), gain(idx)))
                  ),
                  instrument(idx).displayedValue,
                  RelativeBinding.list(leftSix(knobs).zip(List.range(2, 8).map(params(idx)(_))))
                )
              }
            }
          )
        }
      )
    }

    val tryLoopDelete = preferences.getBooleanSetting("Try Loop Delete", "Experimental", false)
    val loopsTrack = TrackCursor("LOOPS_GROUP", "Loops Group", true)
    val loopCursor = TrackCursor("LOOP_CURSOR", "Loop Cursor", true)
    loopCursor.clipCursor
    val loopTracks = loopsTrack.children(ccPads.length)
    case class LoopMode(idx: Int) extends Mode(loopTracks(idx).name) {
      val track = loopTracks(idx)
      val fxTrack = fxTrackBank(idx)

      val loops = track.children(128)

      def currentLoop = loops(Math.max(loops.currentSize() - 2, 0))
      def nextLoop = loops(loops.currentSize() - 1)
      def nextNextLoop = loops(loops.currentSize())

      override def onEnable() = {
        track.select()
      }

      val setLengthMode = new Mode("Set Length") {
        val actions = List(1, 2, 3, 4, 6, 8, 12, 16).map(length =>
          Action(
            s"Set Length To $length",
            () => { transport.postRecordTime.set(BeatTime.bars(length)); popAction() }
          )
        )

        override def bindings = ButtonBinding.list(ccPads.zip(actions))
      }

      var triggerCancel = Option.empty[() => Unit]
      var currentIsSilent = Option.empty[() => Boolean]
      val triggerAction = Action(
        "Trigger",
        () => {
          if (transport.isPlaying()) {
            val currentCurrentLoop = currentLoop
            val currentCurrentClip = currentCurrentLoop.clipBank(0)
            val currentNextLoop = nextLoop
            val currentNextClip = currentNextLoop.clipBank(0)
            if (transport.postRecordTime.get().beats == 0 && currentCurrentClip.isRecording()) {
              currentCurrentLoop.clipBank(0).launch()
              transport.postRecordTime.set(loopCursor.clipCursor.loopLength().ceilBar)
            } else {
              if (transport.postRecordTime.get().beats == 0) {
                loopCursor.isPinned.set(true)
                loopCursor.selectChannel(currentNextLoop)
              }
              currentNextClip.record()
              currentNextLoop.duplicate()
              nextNextLoop.name.set(
                currentNextLoop.name().split(" ").last.toIntOption match {
                  case Some(num) => (currentNextLoop.name().split(" ").dropRight(1) :+ (num + 1).toString).mkString(" ")
                  case None      => currentNextLoop.name()
                }
              )
            }
          }
        }
      )

      override def bindings = RelativeBinding.list(knobs.take(7).zip(track.fxDevice.remoteControls.controls)) ++ List(
        ccPads(4) -> triggerAction,
        ccPads(0) -> track.mute.toggle,
        ccPads(1) -> track.solo.toggle,
        ccPads(2) -> setLengthMode.pushAction,
        knobs(7) -> track.volume
      )
    }
    val loops = List.range(0, ccPads.length).map(LoopMode.apply)

    val loopSelect = new Mode("Loop Select") {
      override val bindings = ButtonBinding.list(ccPads.zip(loops.map(_.replaceAction)))
    }
  }
  mainModes

  host.scheduleTask(
    new Runnable {
      override def run() = {
        pedal.modeCtx.set(pedal.modes(0))
        modeCtx.set(mainModes.trackMode)
      }
    },
    1000
  )

  def uiState() = try
    ujson
      .Obj(
        "pads" -> ujson.Arr(ccPads.take(8).map(_.toJson()): _*),
        "knobs" -> ujson.Arr(knobs.take(8).map(_.toJson()): _*),
        "mode" -> modeCtx.current.map(_.name()).getOrElse(""),
        "browser" -> browser.toJson(),
        "tracks" -> allTracks.toJson(),
        "loops" -> ujson.Arr(mainModes.loops(0).loops.currentItems.dropRight(1).map(_.clipBank(0).toJson()): _*)
      )
      .toString
  catch
    case e =>
      val stringWriter = new StringWriter()
      e.printStackTrace(new PrintWriter(stringWriter))
      stringWriter.toString()

  val (server, shutdown) = BlazeServerBuilder[IO]
    .bindHttp(8080, "0.0.0.0")
    .withHttpWebSocketApp(ws =>
      HttpRoutes
        .of[IO] {
          case _ -> Root / "ws" =>
            val send: fs2.Stream[IO, WebSocketFrame] =
              fs2.Stream
                .awakeEvery[IO](10.milliseconds)
                .evalMap(_ => IO(WebSocketFrame.Text(uiState())))
            val receive: fs2.Pipe[IO, WebSocketFrame, Unit] =
              in => in.evalMap(frameIn => IO(println("in " + frameIn.length)))

            ws.build(send, receive)
          case request @ GET -> path =>
            StaticFile
              .fromResource(
                "/web/" + (if path.toString == "/" then "/index.html" else path.toString),
                req = Some(request)
              )
              .getOrElseF(NotFound())
        }
        .orNotFound
    )
    .allocated
    .unsafeRunSync()

  override def exit() = {
    host.println("shutting down!")
    shutdown.unsafeRunAsync {
      case Right(_) => host.println("shut down!")
      case _        =>
    }

  }

  // documentState
  //   .getSignalSetting("do it", "do it", "do it")
  //   .addSignalObserver(() => {
  //     host.showPopupNotification("Doing it")
  //     host.println(
  //       MPKmini.program(List("K1", "K2", "Kebab3", "K4", "K5", "K6", "K7", "K8")).map(_.toString).mkString(", ")
  //     )
  //     MPKout.sendSysex(MPKmini.program(List("K1", "K2", "Kebab3", "K4", "K5", "K6", "K7", "K8")))
  //   })

  documentState
    .getSignalSetting("Update Cursors", "Update Cursors", "Update Cursors")
    .addSignalObserver(() => {
      allTracks.tracks.find(_.name() == "Metronome").foreach { track =>
        metronome.track.isPinned.set(true)
        metronome.track.selectChannel(track)
      }
      allTracks.tracks.find(_.name() == "Loops").foreach { track =>
        mainModes.loopsTrack.isPinned.set(true)
        mainModes.loopsTrack.selectChannel(track)
      }
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

abstract class Bytes {
  def toBytes: Array[Byte]
}

sealed abstract class Aftertouch extends Bytes {}
object Aftertouch {
  case object Off extends Aftertouch { override def toBytes = Array(0) }
  case object Channel extends Aftertouch { override def toBytes = Array(1) }
  case object Polyphonic extends Aftertouch { override def toBytes = Array(2) }
}

sealed abstract class Program extends Bytes {}
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
      // and   uncomment this when port names are correct.
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

  override def createInstance(host: bitwig.ControllerHost) =
    ControllerExtensionProxy(MPKminiExtension.apply(using _), this, host)
}

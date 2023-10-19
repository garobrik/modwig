package com.garo

import com.garo._
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
import com.garo.Mode.Bindings

case class MPKminiExtension(_host: bitwig.ControllerHost) extends ControllerExtension()(_host) {
  implicit val modeCtx: ModeCtx = ModeCtx()
  implicit val MPKin: MidiIn = host.getMidiInPort(0)
  implicit val surface: HardwareSurface = host.createHardwareSurface()
  surface.setPhysicalSize(600, 100)

  val MPKout = host.getMidiOutPort(0)

  val noteInput = MPKin.createNoteInput("Keys", "80????", "90????")
  val padInput = MPKin.createNoteInput("Pads", "81????", "91????", "A1????")
  MPKin.setMidiCallback(new ShortMidiDataReceivedCallback {
    override def midiReceived(statusByte: Int, data1: Int, data2: Int) =
      host.println(s"${statusByte}:${data1}:${data2}")
  })

  def leftSix[T](l: List[T]) = l.slice(0, 3) ++ l.slice(4, 7)

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
      new Mode {
        override def name = "Loop"
        override def bindings = List(control -> mainModes.loops(0).triggerAction)
      },
      new Mode {
        val kickAction = createAction(
          "Kick",
          () => padInput.sendRawMidiEvent(ShortMidiMessage.NOTE_ON + 1, 0, 127)
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
        modes(nextModeIdx).setAction()
      }
    )
  }
  pedal

  object metronome {
    val track = TrackCursor("METRONOME", "Metronome", true)
    val toggle = createAction(
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
    val default: Mode = new Mode {
      def name = "Session Control"

      override def dependents = List(browser.isOpen)

      override def bindings =
        List(
          ccPads(0) -> (if !browser.isOpen() then selectedTrackMode.replaceAction
                        else selectedTrack.start.browseAction),
          ccPads(1) -> selectedTrack.deviceCursor.before.browseAction,
          ccPads(2) -> selectedTrack.deviceCursor.after.browseAction,
          ccPads(3) -> (if !browser.isOpen() then selectedTrack.duplicate else selectedTrack.end.browseAction),
          ccPads(4) -> loops(0).triggerAction,
          ccPads(5) -> metronome.toggle,
          ccPads(6) -> (if !browser.isOpen() then transport.tapTempo else browser.commitAction),
          ccPads(7) -> (if !browser.isOpen() then transport.playAction else browser.cancelAction),
          knobs(0) -> selectedTrack,
          knobs(1) -> selectedTrack.deviceCursor,
          knobs(2).asButton -> selectedTrack.deviceCursor.replace.browseIfClosedAction,
          knobs(2) -> browser,
          knobs(3) -> selectedTrack.volume,
          RelativeBinding(knobs(4), transport.tempo, sensitivity = 0.1),
          RelativeBinding(knobs(5), transport.postRecordTime, sensitivity = 4.0),
          knobs(7) -> masterTrack.volume
        )

    }

    val selectedTrackMode = new Mode {
      val device = selectedTrack.deviceCursor
      val joystickPage = device.createTaggedRemoteControlsPage("XY", "xy")

      override def name = "Device Control"
      override def onEnable() = {
        device.remoteControls.visible.set(true)
      }

      override def dependents = deviceModes.map(_.device.exists)

      override def submodes = List(deviceModes.find(_.device.exists()).getOrElse(genericMode))

      override def bindings = List(
        ccPads(0) -> default.replaceAction,
        knobs(3) -> selectedTrack.volume,
        knobs(7) -> masterTrack.volume
      )

      val genericMode = new Mode {
        override def name = "Generic Device"
        override def dependents = device.remoteControls.pageCount +: device.remoteControls.controls.map(_.exists)
        override def bindings =
          AbsoluteBinding.list(joystick.mods.zip(joystickPage.controls)) ++ leftSix(knobs)
            .zip(leftSix(device.remoteControls.controls))
            .map { case (knob, control) =>
              if control.exists() then RelativeBinding(knob, control)
              else ButtonBinding(knob.asButton, control.startMapping)
            } ++ ButtonBinding.list(
            (ccPads.slice(4, 8) ++ ccPads.slice(2, 4))
              .zip(
                device.remoteControls.selectPageAction.take(device.remoteControls.pageCount()) ++ (0 until 8).map(_ =>
                  device.remoteControls.createPage
                )
              )
          )
      }

      abstract class DeviceMode[T <: SpecificDevice](val device: T) extends Mode {
        def name = device.device.name()
      }
      val deviceModes = List[DeviceMode[_]](
        new DeviceMode(Diva.Device(device)) {
          import this.device._
          override def dependents =
            List(pageSwitch, pageIdxs(0), pageIdxs(1), oscModel, hpfModel, vcfModel, env1Model, env2Model)
          val pageSwitch = Settable(0)
          val pageIdxs = List(Settable(0), Settable(0))

          def oscKnobs = oscModel.displayedValue() match {
            case "Triple VCO" =>
              List(
                "VCO1" -> List(vco1.shape, vco1.volume, vco1.tune, vco1.fm, filterFM),
                "VCO2" -> List(vco2.shape, vco2.volume, vco2.tune, vco2.sync),
                "VCO3" -> List(vco3.shape, vco3.volume, vco3.tune, vco3.sync),
                "Filter" -> List(feedback, noise, pinkWhite, filterFreq, filterResonance, filterKeyFollow)
              )
            case _ => host.println(oscModel.displayedValue()); List()
          }

          def envKnobs = List
            .range(0, 2)
            .map(idx =>
              s"Env ${idx + 1}" -> List(
                env(idx).attack,
                env(idx).decay,
                env(idx).sustain,
                env(idx).release,
                env(idx).velocity,
                env(idx).keyFollow
              )
            )

          def modKnobs = List(
            "Tune Mod" -> (vcos.map(_.tuneMod) ++ List(
              tuneModDepth,
              tuneModSrc
            )),
            "Shape Mod" -> (vcos.map(_.shapeMod) ++ List(
              shapeModDepth,
              shapeModSrc
            )),
            "Filt Mod" -> List(
              filtModSrc,
              filtMod2Src,
              resModSrc,
              filtModDepth,
              filtMod2Depth,
              resModDepth
            )
          )

          def keyboardKnobs = "Keyboard" -> List(polyMode, voiceStack, drive, glide, vibrato, filterKeyFollow)

          def lfoKnobs = List
            .range(0, 2)
            .map(idx =>
              s"LFO ${idx + 1}" -> List(
                lfo(idx).waveform,
                lfo(idx).delay,
                lfo(idx).restart,
                lfo(idx).sync,
                lfo(idx).rate,
                lfo(idx).phase
              )
            )

          def allKnobs = List(oscKnobs :+ envKnobs(0) :+ keyboardKnobs, modKnobs ++ List(envKnobs(1)) ++ lfoKnobs)
          val togglePageAction = createAction("Switch Page", () => pageSwitch.set(1 - pageSwitch()))
          val selectPageActions = (0 until 2).map(switchIdx =>
            (0 until 6).map(idx =>
              createAction(
                () => allKnobs(switchIdx)(idx)._1,
                () => pageIdxs(switchIdx).set(idx),
                Gettable(() => if pageIdxs(switchIdx)().min(allKnobs(switchIdx).length - 1) == idx then 1.0 else 0.0)
              )
            )
          )

          def bindings = RelativeBinding.list(
            leftSix(knobs).zip(
              allKnobs(pageSwitch())(pageIdxs(pageSwitch())().min(allKnobs(pageSwitch()).length - 1))._2
            )
          ) ++ ButtonBinding.list(
            (ccPads(4) -> togglePageAction) +: (ccPads.slice(5, 8) ++ ccPads.slice(1, 4))
              .zip(selectPageActions(pageSwitch()).take(allKnobs(pageSwitch()).length))
          ) ++ List(
            joystick.upMod.CCBinding(1),
            joystick.leftMod.CCBinding(2),
            joystick.rightMod.CCBinding(11)
            // joystick.leftMod.PitchBinding(-1),
            // joystick.rightMod.PitchBinding(1),
          )
        },
        new DeviceMode(Chromaphone.Device(device)) {
          val layer = this.device.layer(0)
          import layer._
          override def dependents = resonator
            .map(_.objectType) ++ resonatorHit ++ List(
            noiseFiltType,
            noiseEnvType,
            lfoType,
            malletMod,
            noiseMod,
            pageIdx
          )

          val pageIdx = Settable(0)
          val malletMod = SettableBool(false, "Mallet", "Mod")
          val noiseMod = SettableBool(false, "Noise", "Mod")
          val resonatorHit = resonator.map(_ => SettableBool(false, "Object", "Hit Pos"))

          def knobPages: List[(String, (Option[HardwareActionBindable], List[Parameter]))] = resonator
            .zip(resonatorHit)
            .map { (res, hit) =>
              s"Object ${res.name}" -> (
                Some(hit.toggle),
                if !hit() then
                  List(
                    res.objectType,
                    if res.isTube then res.radius else res.density,
                    res.release,
                    res.decay,
                    res.decayKeyTrack,
                    res.decayVeloTrack
                  )
                else
                  List(
                    res.hitPos,
                    res.tone,
                    res.material,
                    res.lowCut,
                    res.hitPosVeloTrack,
                    res.hitPosRand
                  )
              )
            } ++ List(
            "Mallet" -> (
              Some(malletMod.toggle),
              if !malletMod() then
                List(
                  malletStiffness,
                  malletNoise,
                  malletVol,
                  malletColor,
                  malletVolDirect
                )
              else
                List(
                  malletStiffnessKeyTrack,
                  malletNoiseKeyTrack,
                  malletVolKeyTrack,
                  malletStiffnessVeloTrack,
                  malletNoiseVeloTrack,
                  malletVolVeloTrack
                )
            ),
            "Noise" -> (
              Some(noiseMod.toggle),
              if !noiseMod() then
                List(
                  noiseFiltFreq,
                  noiseDensity,
                  noiseVol,
                  noiseFiltType,
                  if noiseFiltType.displayedValue() == "HP+LP" then noiseFiltWidth else noiseFiltQ,
                  noiseVolDirect
                )
              else
                List(
                  noiseFiltFreqKey,
                  noiseDensityKey,
                  noiseVolKeyTrack,
                  noiseFiltFreqVelo,
                  noiseDensityVelo,
                  noiseVolVeloTrack
                )
            ),
            "LFO" -> (Option.empty, List(
              lfoType,
              lfoSyncRate,
              lfoDelay,
              noiseVolLFO,
              noiseDensityLFO,
              noiseFiltFreqLFO
            )),
            "Env" -> (Option.empty, List(
              noiseEnvA,
              noiseEnvD,
              noiseEnvS,
              noiseEnvR,
              noiseFiltFreqEnv,
              noiseDensityEnv
            )),
            "Balance" -> (Option.empty, List(balance, unisonOn, coupled, balanceKeyFollow, vibratoAmount, gain))
          )

          def pads = ccPads.slice(4, 8) ++ ccPads.slice(1, 4)
          val selectPageAction = pads.zipWithIndex.map { (_, idx) =>
            createAction(
              () => knobPages(idx)._1,
              () => pageIdx.set(idx),
              Gettable(() => if pageIdx() == idx then 1.0 else 0.0)
            )
          }

          override def bindings = RelativeBinding.list(knobPages(pageIdx()) match {
            case (name, (_, params)) => leftSix(knobs).zip(params)
          }) ++ ButtonBinding.list(pads.zip(knobPages.zipWithIndex.map {
            case ((_, (Some(action), _)), idx) if idx == pageIdx() => action
            case (_, idx)                                          => selectPageAction(idx)
          }))
        },
        new DeviceMode(DrumSynth.Device(device)) {
          import this.device._

          val instIdx = Settable[Either[Int, Int]](Left(0))
          val setInst =
            (0 until 8).map(idx => createAction(() => model(idx).displayedValue(), () => instIdx.set(Left(idx))))
          val setInstB =
            (0 until 8).map(idx => createAction(() => model(idx).displayedValue(), () => instIdx.set(Right(idx))))

          override def dependents = List(instIdx)

          override def bindings =
            RelativeBinding.list(
              leftSix(knobs).zip(
                instIdx() match {
                  case Left(idx) =>
                    List(instrument(idx), params(idx)(0), velocity(idx), model(idx), params(idx)(1), gain(idx))
                  case Right(idx) => List.range(2, 8).map(params(idx)(_))
                }
              )
            ) ++ List.range(0, 8).map(idx => ButtonBinding(notePads(idx), setInstB(idx), setInst(idx)))
        }
      )
    }

    val tryLoopDelete = preferences.getBooleanSetting("Try Loop Delete", "Experimental", false)
    val loopsTrack = TrackCursor("LOOPS_GROUP", "Loops Group", true)
    val loopCursor = TrackCursor("LOOP_CURSOR", "Loop Cursor", true)
    loopCursor.clipCursor
    val loopTracks = loopsTrack.children(ccPads.length)
    case class LoopMode(idx: Int) extends Mode {
      val track = loopTracks(idx)
      val fxTrack = fxTrackBank(idx)
      override def name = track.name()

      val loops = track.children(128)

      def currentLoop = loops(Math.max(loops.currentSize() - 2, 0))
      def nextLoop = loops(loops.currentSize() - 1)

      override def onEnable() = {
        track.select()
      }

      val setLengthMode = new Mode {
        override def name = "Set Length"

        val actions = List(1, 2, 3, 4, 6, 8, 12, 16).map(length =>
          createAction(
            () => s"Set Length To $length",
            () => { transport.postRecordTime.set(BeatTime.bars(length)); popAction() }
          )
        )

        override def bindings = ButtonBinding.list(ccPads.zip(actions))
      }

      var triggerCancel = Option.empty[() => Unit]
      var currentIsSilent = Option.empty[() => Boolean]
      val triggerAction = createAction(
        "Trigger",
        () => {
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
            currentNextClip.isPlaying.doAfterChanged(isPlaying =>
              if (isPlaying) {
                currentNextLoop.arm.set(false)
              }
            )
            currentNextLoop.duplicate()
          }

          // def triggerClip(offset: Int = 0): Unit = {
          //   var vuMeterReadings = Seq.empty[Int]
          //   currentIsSilent = Option(() => vuMeterReadings.sum.toDouble / vuMeterReadings.size.toDouble < 5)
          //   var vuMeterCancel = Option.empty[() => Unit]
          //   val currentNextLoop = loops((loops.currentSize() - 1) + offset)
          //   host.println(s"next loop pos: ${currentNextLoop.position()}")
          //   currentNextLoop.duplicate()

          //   transport.cancelOnPause(
          //     currentNextLoop
          //       .clipBank(0)
          //       .isRecording
          //       .doAfterChanged(isRecording => {
          //         if (isRecording) {
          //           vuMeterCancel = Some(transport.onTick(_ => {
          //             vuMeterReadings = fxTrack.vuMeterRMS() +: vuMeterReadings
          //           }))
          //         }
          //       })
          //   )
          //   currentNextLoop.clipBank(0).record()
          //   val transportCancel =
          //     transport.doBeforeOffset(
          //       transport.postRecordTime() + transport.tilNextBar,
          //       () => {
          //         vuMeterCancel.foreach(_())
          //         val offset = if tryLoopDelete.get() && currentIsSilent.getOrElse(() => false)() then {
          //           host.println(s"deleting ${currentNextLoop.position()}")
          //           currentNextLoop.delete()
          //           -1
          //         } else 0
          //         triggerClip(offset)
          //       }
          //     )
          //   triggerCancel = Some(() => { vuMeterCancel.foreach(_()); transportCancel(); })
          // }

          // if (currentLoop.clipBank(0).isRecording()) {
          //   triggerCancel.foreach(_())
          //   nextLoop.clipBank.stop()
          //   nextLoop.clipBank(0).delete()
          //   if (currentIsSilent.getOrElse(() => false)()) {
          //     currentLoop.delete()
          //   } else if (transport.postRecordAction.get() == Transport.PostRecordAction.Off) {
          //     currentLoop.clipBank(0).launch()
          //   }
          // } else {
          //   triggerClip()
          // }

          // transport.playAction()
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

    val loopSelect = new Mode {
      def name = "Loop Select"
      def bindings = ButtonBinding.list(ccPads.zip(loops.map(_.replaceAction)))
    }
  }
  mainModes

  host.scheduleTask(
    new Runnable {
      override def run() = {
        pedal.modeCtx.set(pedal.modes(0))
        modeCtx.set(mainModes.default)
      }
    },
    1000
  )

  def uiState() = {
    try
      ujson
        .Obj(
          "pads" -> ujson.Arr(ccPads.take(8).map(_.toJson()): _*),
          "knobs" -> ujson.Arr(knobs.take(8).map(_.toJson()): _*),
          "mode" -> modeCtx.current.map(_.name).getOrElse(""),
          "browserResults" -> (if !browser.isOpen() then ujson.Null
                               else browser.resultsColumn.toJson()),
          "tracks" -> allTracks.toJson()
        )
        .toString
    catch
      case e =>
        val stringWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stringWriter))
        stringWriter.toString()
  }

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
    ControllerExtensionProxy(MPKminiExtension.apply, this, host)
}

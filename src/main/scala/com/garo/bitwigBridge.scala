package com.garo

import com.bitwig.extension.api._
import com.bitwig.extension.{controller => bitwigController}
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._
import com.bitwig.extension.controller.{api => bitwig}
import java.util.function.Supplier
import _root_.java.sql.Wrapper
import javax.sound.midi.MidiMessage
import com.garo
import java.net.Inet4Address
import java.net.InetAddress
import scala.annotation.targetName

trait Serializable {
  def toJson(): ujson.Value
}
given Conversion[Serializable, ujson.Value] = _.toJson()

abstract class ControllerExtension(implicit val host: ControllerHost) {
  implicit val extension: ControllerExtension = this

  var _isInitializaing = true
  def isInitializaing = _isInitializaing

  val transport = Transport(host.createTransport())
  val arranger = host.createArranger()
  val application = Application(host.createApplication())
  val preferences = host.getPreferences()
  val documentState = host.getDocumentState()
  val browser = Browser(host.createPopupBrowser())

  val debug = SettableBoolValue(preferences.getBooleanSetting("Log Enabled", "Debug", false))

  def numSends = 4
  def numScenes = 1
  val selectedTrack = TrackCursor("SELECTION", "Track", true)
  selectedTrack.clipCursor
  selectedTrack.deviceCursor
  selectedTrack.devices
  val allTracks = TrackBank(host.createTrackBank(32, numSends, numScenes))
  allTracks.tracks.map(_.devices.devices.map(_.isSelected))
  val mainTrackBank = TrackBank(host.createMainTrackBank(32, numSends, numScenes))
  val fxTrackBank = TrackBank(host.createEffectTrackBank(32, numScenes))
  val masterTrack = new Track(host.createMasterTrack(numScenes))

  val blockSize = preferences.getNumberSetting("Max Block Size", "Audio", 1, 50, 1, "ms", 24)
  // val addr = preferences.getStringSetting("Local IP", "Network", 30, InetAddress.getLocalHost().getHostAddress())

  val doNothingAction = Action("None", () => {})

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

class Application(application: bitwig.Application)(implicit ext: ControllerExtension) {
  def createAction(id: String) =
    Action(application.getAction(id).getName(), application.getAction(id).invoke)
  val minimize = createAction("Minimize window")
  val undo = Action("Undo", application.undoAction())
  val redo = Action("Redo", application.redoAction())
}

case class ModeCtx()(implicit ext: ControllerExtension) {
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
        ext.host.showPopupNotification(s"${mode.name()} Mode")
      })
    }
  }
}

abstract class Mode(val name: Gettable[String] = "Unnamed")(implicit ext: ControllerExtension, val ctx: ModeCtx) {
  def bindings: Mode.Bindings

  def dependents = List[Gettable[_]]()

  def onEnable() = {}
  private var currentBindings: List[() => Unit] = List()
  final def enable(): Unit = {
    if (!currentBindings.isEmpty) return
    onEnable()
    refreshBindings()
    if (ext.debug()) ext.host.println(s"enable mode ${name()}")
  }

  def onDisable() = {}
  final def disable(): Unit = {
    if (ext.debug()) ext.host.println(s"disable mode ${name()}")
    currentBindings.foreach(_.apply())
    currentBindings = List()
    onDisable()
  }

  def refreshBindings(): Unit = {
    currentBindings.foreach(_.apply())
    currentBindings = bindings.map {
      case ctx: ModeCtx           => ctx.enable(); () => ctx.disable()
      case mode: Mode             => mode.enable(); () => mode.disable()
      case binding: Binding[_, _] => binding.create()
    } ++ dependents.map(_.onChanged(_ => refreshBindings()))
  }

  lazy val setAction = Action(name, () => ctx.set(this))
  lazy val pushAction = Action(name, () => ctx.push(this))
  lazy val popAction = Action(name, () => ctx.pop(this))
  lazy val replaceAction = Action(name, () => ctx.replace(this))

  if (ext.isInitializaing) { setAction; pushAction; popAction; replaceAction }
}

object Mode {
  type Bindings = List[ModeCtx | Mode | Binding[_, _]]

  case class Page(pressedName: Gettable[String], onPressed: Bindings, heldName: Gettable[String], onHeld: Bindings)
  object Page {
    def from(page: List[(Gettable[String], Bindings)]) = Page(page(0)._1, page(0)._2, page(1)._1, page(1)._2)
  }
  def pages(knobs: List[RelativeHardwareKnob], pages: List[List[(Gettable[String], List[Parameter | Binding[_, _]])]]) =
    pages
      .map(_.map { (name, params) =>
        (
          name,
          knobs.zip(params).map {
            case (_, binding: Binding[_, _]) => binding
            case (knob, param: Parameter)    => RelativeBinding(knob, param)
          }
        )
      })
      .map(Page.from)
  @targetName("pagesString")
  def pages(
      knobs: List[RelativeHardwareKnob],
      pages: List[List[(String, List[Parameter | Binding[_, _]])]]
  ): List[Page] =
    Mode.pages(knobs, pages.map(_.map { (name, params) => (Gettable(name), params) }))
  def pages(
      knobs: List[RelativeHardwareKnob],
      pages: List[(Gettable[String], List[Parameter | Binding[_, _]])]*
  ): List[Page] =
    Mode.pages(knobs, pages.toList)
  @targetName("pagesString")
  def pages(knobs: List[RelativeHardwareKnob], pages: List[(String, List[Parameter | Binding[_, _]])]*): List[Page] =
    Mode.pages(knobs, pages.toList)
  abstract class Paged(buttons: List[HardwareButton])(using ctx: ModeCtx, ext: ControllerExtension) extends Mode {
    def pages: List[Page]

    val page = Settable(0)
    val pageHeld = buttons.map(_ => Settable(false))
    val pageSwapBindings = buttons.zip(pageHeld).zipWithIndex.map { case ((button, pageHeld), idx) =>
      ButtonBinding(
        button,
        Action(
          pages.map(_.pressedName).applyOrElse(idx, _ => Gettable("")),
          () => { if (page() != idx) pageHeld.set(false); page.set(idx); },
          page.map(p => if p == idx then 1.0 else 0.0)
        ),
        onLongPress = Action(
          pages.map(_.heldName).applyOrElse(idx, _ => Gettable("")),
          () => { pageHeld.set(page() != idx || !pageHeld()); page.set(idx); },
          page.map(p => if p == idx then 1.0 else 0.0)
        )
      )
    }

    override def dependents = page +: pageHeld
    override def bindings = {
      val curPage = pages.zipWithIndex.filter(_._2 == page()).map(_._1)
      val curBindings = curPage.flatMap(curPage =>
        if (pageHeld(page())() && !curPage.onHeld.isEmpty) then curPage.onHeld else curPage.onPressed
      )
      curBindings ++ pageSwapBindings
    }
  }
}

abstract class Binding[Bindable <: HardwareBindable[_], Control <: HardwareControl[_, Bindable, _]](
    val control: Control
) {
  def create(): () => Unit
}

class ButtonBinding(
    control: HardwareButton,
    onPressed: Action,
    onReleased: Option[Action] = None,
    onLongPress: Option[Action] = None
) extends Binding(control) {
  def create() = {
    val pairs = (onReleased, onLongPress) match {
      case (Some(onReleased), Some(onLongPress)) =>
        List(control.onTap -> onPressed, control.onLongPress -> onLongPress, control.onReleased.get -> onReleased)
      case (Some(onReleased), None)  => List(control.onPressed -> onPressed, control.onReleased.get -> onReleased)
      case (None, Some(onLongPress)) => List(control.onTap -> onPressed, control.onLongPress -> onLongPress)
      case _                         => List(control.onPressed -> onPressed)
    }
    val disposals = pairs.flatMap { (source, bindable) =>
      List(source.addBinding(bindable), control.addBindable(bindable))
    }
    () => disposals.foreach(_())
  }
}

given Conversion[Action, Option[Action]] = Option.apply

object ButtonBinding {
  def list(list: List[(HardwareButton, Action)]) = list.map { (button, bindable) =>
    ButtonBinding(button, bindable)
  }
}

given Conversion[(HardwareButton, Action), ButtonBinding] = { (button, bindable) =>
  ButtonBinding(button, bindable)
}

case class RelativeBinding(
    override val control: RelativeHardwareKnob,
    bindable: RelativeHardwareControlBindable[_ <: RelativeHardwarControlBindable],
    sensitivity: Double = 1
) extends Binding(control) {
  def create() = {
    bindable match { case param: bitwig.Parameter => param.setIndication(true); case _ => () }
    val disposals =
      List(
        {
          val binding = bindable.bindable.addBindingWithSensitivity(control.control, sensitivity);
          () => binding.removeBinding()
        },
        control.addBindable(bindable)
      )
    () => {
      bindable match { case param: bitwig.Parameter => param.setIndication(false); case _ => () }
      disposals.foreach(_())
    }
  }
}

object RelativeBinding {
  def list(list: List[(RelativeHardwareKnob, RelativeHardwareControlBindable[_])]) = list.map { case (knob, bindable) =>
    RelativeBinding(knob, bindable)
  }
}

given Conversion[(RelativeHardwareKnob, RelativeHardwareControlBindable[_]), RelativeBinding] = (tup) =>
  RelativeBinding(tup(0), tup(1))

case class AbsoluteBinding(
    override val control: AbsoluteHardwareKnob,
    bindable: AbsoluteHardwareControlBindable[_ <: AbsoluteHardwarControlBindable]
) extends Binding(control) {
  def create() = {
    bindable match { case param: bitwig.Parameter => param.setIndication(true); case _ => () }
    val disposals =
      List(
        { val binding = bindable.bindable.addBinding(control.control); () => binding.removeBinding() },
        control.addBindable(bindable)
      )
    () => {
      bindable match { case param: bitwig.Parameter => param.setIndication(false); case _ => () }
      disposals.foreach(_())
    }
  }
}

object AbsoluteBinding {
  def list(list: List[(AbsoluteHardwareKnob, AbsoluteHardwareControlBindable[_])]) = list.map { case (knob, bindable) =>
    AbsoluteBinding(knob, bindable)
  }
}

given Conversion[(AbsoluteHardwareKnob, AbsoluteHardwareControlBindable[_]), AbsoluteBinding] = (tup) =>
  AbsoluteBinding.apply.tupled(tup)

abstract class ObjectProxy(obj: bitwig.ObjectProxy) {
  val exists = BoolValue(obj.exists())
}

case class Browser(browser: bitwig.PopupBrowser)(implicit ext: ControllerExtension)
    extends HardwareBindable(browser),
      RelativeHardwareControlBindable[bitwig.PopupBrowser] {
  val bindableName = "Browse"
  val rawValue = 0.0
  val displayedValue = ""

  val commit = Action("Commit", browser.commitAction())
  val cancel = Action("Cancel", browser.cancelAction())
  val nextFile = Action("Next", browser.selectNextFileAction())
  val prevFile = Action("Prev", browser.selectPreviousFileAction())

  val isOpen = BoolValue(browser.exists())
  val shouldAudtion = SettableBoolValue(browser.shouldAudition())
  shouldAudtion.set(false)

  val results = BrowserResultsColumn(browser.resultsColumn())
  val tags = BrowserFilterColumn(browser.tagColumn())
  val categories = BrowserFilterColumn(browser.categoryColumn())
  val deviceTypes = BrowserFilterColumn(browser.deviceTypeColumn())
  val creators = BrowserFilterColumn(browser.creatorColumn())

  override def toJson() = super.toJson().obj ++ ujson
    .Obj(
      "isOpen" -> isOpen(),
      "results" -> results.toJson(),
      "tags" -> tags.toJson(),
      "categories" -> categories.toJson(),
      "deviceTypes" -> deviceTypes.toJson(),
      "creators" -> creators.toJson()
    )
    .obj
}

class BrowserColumn[T <: bitwig.BrowserColumn, BR <: bitwig.BrowserItem, R <: BrowserItem](
    column: bitwig.BrowserColumn,
    ctor: BR => R
)(using ControllerExtension) {
  val itemBank = Bank(column.createItemBank(100).asInstanceOf[bitwig.BrowserItemBank[BR]], ctor)
  val itemCursor = ctor(column.createCursorItem().asInstanceOf[BR])
  val entryCount = IntValue(column.entryCount())

  def toJson() = ujson.Arr(List.range(0, 100.min(entryCount())).map(itemBank.apply).map(_.toJson()): _*)
}

class BrowserResultsColumn(column: bitwig.BrowserResultsColumn)(using ControllerExtension)
    extends BrowserColumn(column, item => BrowserItem(item))

class BrowserFilterColumn(column: bitwig.BrowserFilterColumn)(using ControllerExtension)
    extends BrowserColumn(column, item => BrowserFilterItem(item))

class BrowserItem(item: bitwig.BrowserItem)(using ext: ControllerExtension) extends Serializable {
  val name = StringValue(item.name())
  val isSelected = SettableBoolValue(item.isSelected())
  val selectAndCommit = Action(name, () => { isSelected.set(true); ext.browser.commit() })

  def toJson() = ujson.Obj(
    "name" -> name(),
    "isSelected" -> isSelected()
  )
}

class BrowserFilterItem(item: bitwig.BrowserFilterItem)(using ControllerExtension) extends BrowserItem(item) {
  val hitCount = IntValue(item.hitCount())
}

case class Bounds(x: Double, y: Double, w: Double, h: Double)
sealed abstract class HardwareControl[
    ControlType <: bitwig.HardwareControl,
    BindableType <: HardwareBindable[_],
    BindingType <: bitwig.HardwareBinding
](
    controlFn: String => ControlType,
    name: String,
    indexInGroup: Option[Int],
    bounds: Option[Bounds]
) {
  val control = controlFn(name)

  var bindings = List[BindableType]()
  def addBindable(bindable: BindableType) = {
    bindings = bindings :+ bindable
    () => {
      bindings = bindings.filter(_ != bindable)
    }
  }

  def setName(name: String) = control.setName(name)

  indexInGroup.foreach(control.setIndexInGroup)
  control.setName(name)
  control.setLabel(name)
  bounds.foreach(bounds => control.setBounds(bounds.x, bounds.y, bounds.w, bounds.h))

  def toJson() = ujson.Obj(
    "name" -> name,
    "type" -> (this match {
      case _: HardwareButton       => "Button"
      case _: RelativeHardwareKnob => "RelativeKnob"
      case _: AbsoluteHardwareKnob => "AbsoluteKnob"
    }),
    "bindings" -> bindings.map(_.toJson())
  )
}

case class ActionMatcher(
    kind: ActionMatcher.ActionKind,
    number: Int,
    channel: Option[Int] = None,
    value: Option[Int] = None
) {
  override def toString() = {
    val statusByte = kind match {
      case ActionMatcher.NoteOn  => ShortMidiMessage.NOTE_ON
      case ActionMatcher.NoteOff => ShortMidiMessage.NOTE_OFF
      case ActionMatcher.CC      => ShortMidiMessage.CONTROL_CHANGE
      case ActionMatcher.CCKnob  => ShortMidiMessage.CONTROL_CHANGE
      case ActionMatcher.PC      => ShortMidiMessage.PROGRAM_CHANGE
    }
    val statusCond = channel match {
      case Some(channel) => s"${statusByte + channel} == status"
      case None          => s"$statusByte <= status && status < ${statusByte + 16}"
    }
    val data2Cond = (kind, value) match {
      case (ActionMatcher.PC | ActionMatcher.CCKnob | ActionMatcher.NoteOff, _) => "true"
      case (_, Some(value))                                                     => s"data2 == $value"
      case _                                                                    => "data2 > 0"
    }

    s"$statusCond && data1 == $number && $data2Cond"
  }
}

object ActionMatcher {
  sealed trait ActionKind
  case object NoteOn extends ActionKind
  case object NoteOff extends ActionKind
  case object CC extends ActionKind
  case object PC extends ActionKind
  case object CCKnob extends ActionKind
}

trait HardwareActionSource {
  def addBinding(bindable: Action): () => Unit
}

case class HardwareButton(
    name: String,
    actionMatcher: ActionMatcher,
    offActionMatcher: Option[ActionMatcher] = None,
    pressureActionMatcher: Option[ActionMatcher] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    ext: ControllerExtension,
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn
) extends HardwareControl(surface.createHardwareButton, name, indexInGroup, bounds) {
  val isPressed = BoolValue(control.isPressed())

  private class WrapperActionSource(val action: bitwig.HardwareAction, matcher: ActionMatcher)
      extends HardwareActionSource {
    action.setShouldFireEvenWhenUsedAsNoteInput(true)
    action.setActionMatcher(midiPort.createActionMatcher(matcher.toString))

    override def addBinding(bindable: Action) = {
      val binding = action.addBinding(bindable.bindable)
      () => binding.removeBinding()
    }
  }
  val onPressed: HardwareActionSource = WrapperActionSource(control.pressedAction(), actionMatcher)
  val onReleased: Option[HardwareActionSource] = offActionMatcher.map(WrapperActionSource(control.releasedAction(), _))

  val onTap = onReleased match {
    case Some(onReleased) =>
      new HardwareActionSource {
        var lastPress = Option.empty[Long]
        var bindings = List[Action]()
        onPressed.addBinding(Action("", () => lastPress = Some(System.currentTimeMillis())))
        onReleased.addBinding(
          Action(
            "",
            () => if lastPress.isDefined && System.currentTimeMillis() - lastPress.get <= 200 then bindings.foreach(_())
          )
        )

        def addBinding(bindable: com.garo.Action) = {
          bindings = bindings :+ bindable
          lastPress = None
          () => { bindings = bindings.filter(_ != bindable); lastPress = None }
        }
      }
    case None =>
      new HardwareActionSource {
        override def addBinding(bindable: Action) = onPressed.addBinding(bindable)
      }
  }

  val onLongPress = onReleased match {
    case Some(onReleased) =>
      new HardwareActionSource {
        var lastPress = Option.empty[Long]
        var bindings = List[Action]()
        onPressed.addBinding(Action("", () => lastPress = Some(System.currentTimeMillis())))
        onReleased.addBinding(
          Action(
            "",
            () => if lastPress.isDefined && System.currentTimeMillis() - lastPress.get > 200 then bindings.foreach(_())
          )
        )

        def addBinding(bindable: com.garo.Action) = {
          bindings = bindings :+ bindable
          lastPress = None
          () => { bindings = bindings.filter(_ != bindable); lastPress = None }
        }
      }
    case None =>
      new HardwareActionSource {
        override def addBinding(bindable: Action) = () => {}
      }
  }

  override def toJson() = {
    val sup = super.toJson()
    sup.update("isPressed", isPressed())
    sup
  }
}

case class AbsoluteHardwareKnob(
    name: String,
    ccNum: Int,
    channel: Option[Int] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    ext: ControllerExtension,
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn,
    midiInput: bitwig.NoteInput
) extends HardwareControl(surface.createAbsoluteHardwareKnob, name, indexInGroup, bounds) {
  val value = new DoubleValue(control.value())

  val onMatcher = channel match {
    case Some(channel) => midiPort.createAbsoluteCCValueMatcher(channel, ccNum)
    case None          => midiPort.createAbsoluteCCValueMatcher(ccNum)
  }

  control.setAdjustValueMatcher(onMatcher)

  class CCBinding(ccNum: Int) extends Binding(AbsoluteHardwareKnob.this) {
    override def create(): () => Unit =
      value.onChanged(value => midiInput.sendRawMidiEvent(ShortMidiMessage.CONTROL_CHANGE, ccNum, (value * 127).toInt))
  }

  class PitchBinding(factor: Int) extends Binding(AbsoluteHardwareKnob.this) {
    override def create(): () => Unit =
      value.onChanged(value => {
        val result = (8192 * (1 + factor * value / 127)).toInt
        midiInput.sendRawMidiEvent(ShortMidiMessage.PITCH_BEND, result / 128, result % 128)
      })
  }

  val asButton = HardwareButton(name + "_AS_BUTTON", ActionMatcher(ActionMatcher.CCKnob, ccNum, channel, indexInGroup))
}

case class RelativeHardwareKnob(
    name: String,
    ccNum: Int,
    channel: Option[Int] = None,
    indexInGroup: Option[Int] = None,
    bounds: Option[Bounds] = None
)(implicit
    ext: ControllerExtension,
    surface: bitwig.HardwareSurface,
    midiPort: bitwig.MidiIn
) extends HardwareControl[
      bitwig.RelativeHardwareKnob,
      RelativeHardwareControlBindable[_],
      bitwig.RelativeHardwareControlBinding
    ](surface.createRelativeHardwareKnob, name, indexInGroup, bounds) {

  val onMatcher = midiPort.createRelative2sComplementValueMatcher(
    channel match {
      case Some(channel) => midiPort.createAbsoluteCCValueMatcher(channel, ccNum)
      case None          => midiPort.createAbsoluteCCValueMatcher(ccNum)
    },
    127
  )

  control.setAdjustValueMatcher(onMatcher)

  val asButton = HardwareButton(name + "_AS_BUTTON", ActionMatcher(ActionMatcher.CCKnob, ccNum, channel, indexInGroup))

  override def toJson() = {
    val sup = super.toJson()
    sup.update("bindings", (bindings ++ asButton.bindings).map(_.toJson()))
    sup
  }
}

trait HardwareBindable[T <: bitwig.HardwareBindable](val bindable: T) extends Serializable {
  val bindableName: Gettable[String]
  val rawValue: Gettable[Double]
  val displayedValue: Gettable[String]

  def toJson() =
    ujson.Obj(
      "name" -> bindableName(),
      "value" -> rawValue(),
      "displayedValue" -> displayedValue()
    )

}

trait RelativeHardwareControlBindable[T <: bitwig.RelativeHardwarControlBindable] extends HardwareBindable[T]
trait AbsoluteHardwareControlBindable[T <: bitwig.AbsoluteHardwarControlBindable] extends HardwareBindable[T]

trait Cursor[T <: bitwig.Cursor](using ControllerExtension) extends RelativeHardwareControlBindable[T] {
  val selectNext = Action("Select Next", bindable.selectNextAction())
}

given Conversion[bitwig.HardwareActionBindable, () => Unit] = _.invoke

class Action(
    name: Gettable[String],
    action: () => Unit,
    value: Gettable[Double] = Gettable(0.0)
)(using ext: ControllerExtension)
    extends HardwareBindable(
      ext.host.createAction(
        new Runnable {
          override def run() = action()
        },
        new Supplier[String] {
          override def get() = name()
        }
      )
    ) {

  if (name == null) throw new Exception()
  override val bindableName = name
  override val rawValue = value
  override val displayedValue = ""

  def apply() = action()

  def andThen(next: () => Unit, nameFn: String => String = identity) =
    Action(name.map(nameFn), () => { action(); next(); })
}

trait Setting[+T] {
  def setEnabled(enabled: Boolean): Unit
  def setShown(shown: Boolean): Unit
  def get(): T
  def onChanged(callback: (T) => Unit): Unit

  def setEnabledAndShown(enabledAndShown: Boolean) = { setEnabled(enabledAndShown); setShown(enabledAndShown) }
  def enableAndShow() = setEnabledAndShown(true)
  def disableAndHide() = setEnabledAndShown(false)

  def map[T2](fn: T => T2) = {
    val orig = this
    new ProxySetting[T2] {
      override def get() = fn(orig.get())
      override val proxied = List(orig)
    }
  }
}

given Conversion[Action, () => Unit] = _.apply

trait WritableSetting[T] extends Setting[T] {
  def set(value: T): Unit
}

abstract class ProxySetting[T] extends Setting[T] {
  def proxied: List[Setting[_ <: Any]]
  override def setEnabled(enabled: Boolean) = proxied.foreach(_.setEnabled(enabled))
  override def setShown(shown: Boolean) = proxied.foreach(_.setShown(shown))
  override def onChanged(callback: T => Unit) = proxied.foreach(_.onChanged(_ => callback(get())))
}

case class PairSetting[A, B](a: Setting[A], b: Setting[B])(implicit settingCtx: SettingCtx)
    extends ProxySetting[(A, B)] {
  override def proxied: List[Setting[_]] = List(a, b)
  override def get() = (a.get(), b.get())
}

case class SettingCtx(
    val category: String,
    indentationLevel: Int = 0,
    blankLabelMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map().withDefaultValue(0)
)(implicit val settings: Settings, val extension: ControllerExtension) {
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
    override def call() = callback(())
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

abstract class Gettable[T] {
  def get(): T
  def apply() = get()
  def onChanged(callback: T => Unit): () => Unit

  def doAfterChanged(callback: T => Unit) = {
    var cancel: () => Unit = null
    cancel = onChanged(t => {
      callback(t)
      cancel()
    })
    cancel
  }

  def map[T2](f: T => T2) = Gettable(() => f(get()), List(this))
}

object Gettable {
  def apply[T](f: () => T, dependents: List[Gettable[_]]): Gettable[T] =
    new Observable[T] {
      override def get() = f()
      override def addValueObserver(callback: T => Unit) = {
        dependents.foreach(_.onChanged(_ => callback(f())))
      }
    }

  def apply[T](f: () => T, dependent: Gettable[_]): Gettable[T] =
    Gettable(f, List(dependent))

  def apply[T](t: T) =
    new Gettable[T] {
      override def get() = t
      override def onChanged(callback: T => Unit) = () => {}
    }
}

given Conversion[String, Gettable[String]] = t => Gettable(t)
given Conversion[Double, Gettable[Double]] = t => Gettable(t)
given getString2Opt: Conversion[Gettable[String], Option[Gettable[String]]] = Option.apply
given Conversion[Gettable[Double], Option[Gettable[Double]]] = Option.apply
given Conversion[(String, List[Parameter]), (Gettable[String], List[Parameter])] = { (name, params) =>
  (Gettable(name), params)
}
given Conversion[String, Option[Gettable[String]]] = Option.apply
given Conversion[Double, Option[Gettable[Double]]] = Option.apply

abstract class Observable[T] extends Gettable[T] {
  protected def addValueObserver(callback: T => Unit): Unit
  addValueObserver(t => {
    callbacks.foreach(_(t))
  })
  var callbacks = Set.empty[T => Unit]

  override def onChanged(callback: T => Unit) = {
    callbacks = callbacks + callback
    () => callbacks = callbacks - callback
  }
}

class Settable[T](private var value: T)(implicit ext: ControllerExtension) extends Observable[T] {
  override protected def addValueObserver(callback: T => Unit) = {}

  def get() = value
  def set(newValue: T): Unit = {
    value = newValue
    callbacks.foreach(_(newValue))
  }
}

class SettableBool(t: Boolean, trueName: String, falseName: String)(implicit ext: ControllerExtension)
    extends Settable(t) {
  val toggle =
    Action(
      this.map(if _ then trueName else falseName),
      () => set(!get()),
      this.map(if _ then 1.0 else 0.5)
    )
}

abstract class Value[T](value: bitwig.Value[_ <: ValueChangedCallback]) extends Observable[T] {
  value.markInterested()
}

object Observable {
  def apply[T](t: T) =
    new Observable[T] {
      override def get() = t
      override protected def addValueObserver(callback: T => Unit) = {}
    }
}

case class ValueFromLastObserved[T](initial: T, valueObserverCallback: (T => Unit) => Unit) extends Observable[T] {
  var lastVal = initial
  override def get() = lastVal
  override protected def addValueObserver(callback: T => Unit) = valueObserverCallback(callback)
  onChanged(newVal => lastVal = newVal)
}

trait SettableValue[T] extends Value[T] {
  def set(newValue: T): Unit
  def update(f: T => T) = set(f(get()))
}

class DoubleValue(value: bitwig.DoubleValue) extends Value[Double](value) {
  override def addValueObserver(callback: Double => Unit) = value.addValueObserver(
    new DoubleValueChangedCallback {
      override def valueChanged(newValue: Double) = callback(newValue)
    }
  )
  override def get() = value.get()
}

case class SettableDoubleValue(value: bitwig.SettableDoubleValue)
    extends DoubleValue(value)
    with SettableValue[Double] {
  override def set(double: Double) = value.set(double)
  def inc(double: Double) = value.inc(double)
}

case class BeatTime(beats: Double) {
  def bars(implicit ext: ControllerExtension) = beats / ext.transport.beatsPerBar()
  def beatInBar(implicit ext: ControllerExtension) = (this - floorBar).beats.floor

  def +(other: BeatTime) = BeatTime(beats + other.beats)
  def -(other: BeatTime) = BeatTime(beats - other.beats)
  def /(other: BeatTime) = BeatTime(beats / other.beats)
  def /(other: Int) = BeatTime(beats / other)
  def *(other: Int) = BeatTime(beats * other)

  def round = BeatTime(beats.round)

  def floorBar(implicit ext: ControllerExtension) = BeatTime(bars.floor * ext.transport.beatsPerBar())
  def ceilBar(implicit ext: ControllerExtension) = BeatTime(bars.ceil * ext.transport.beatsPerBar())

  def >=(other: BeatTime) = beats >= other.beats
  def >(other: BeatTime) = beats > other.beats
  def <(other: BeatTime) = beats < other.beats
  def <=(other: BeatTime) = beats <= other.beats
}

object BeatTime {
  def bars(numBars: Int)(implicit ext: ControllerExtension) = BeatTime(numBars * ext.transport.beatsPerBar())
  def beats(numBeats: Int) = BeatTime(numBeats)
  def eights(numEights: Int) = BeatTime(numEights.toDouble / 2.0)
  def sixteenths(numEights: Int) = BeatTime(numEights.toDouble / 4.0)
  def thirtySeconds(numEights: Int) = BeatTime(numEights.toDouble / 8.0)
}

class BeatTimeValue(value: bitwig.BeatTimeValue)(implicit ext: ControllerExtension) extends Value[BeatTime](value) {
  override def addValueObserver(callback: BeatTime => Unit) = value.addValueObserver(
    new DoubleValueChangedCallback {
      override def valueChanged(newValue: Double) = callback(BeatTime(newValue))
    }
  )

  override def get() = BeatTime(value.get())

  override def onChanged(callback: BeatTime => Unit) = {
    super.onChanged(callback)
  }
}

case class SettableBeatTimeValue(value: bitwig.SettableBeatTimeValue, name: String)(implicit ext: ControllerExtension)
    extends BeatTimeValue(value),
      SettableValue[BeatTime],
      HardwareBindable(value.beatStepper()),
      RelativeHardwareControlBindable {

  override val bindableName = name
  override val displayedValue = this.map(v => s"${v.bars.floor.toInt}.${v.beatInBar.round.toInt}")
  override val rawValue = 0.0

  override def set(beatTime: BeatTime) = value.set(beatTime.beats)
}

class Parameter(param: bitwig.Parameter, name: Option[Gettable[String]] = None)(using ControllerExtension)
    extends Value[Double](param),
      HardwareBindable(param),
      SettableValue[Double],
      RelativeHardwareControlBindable[bitwig.Parameter],
      AbsoluteHardwareControlBindable[bitwig.Parameter] {

  override def addValueObserver(callback: Double => Unit) = param
    .value()
    .addValueObserver(
      new DoubleValueChangedCallback {
        override def valueChanged(newValue: Double) = callback(newValue)
      }
    )
  override def get() = param.get
  override def set(newValue: Double): Unit = param.set(newValue)

  override val bindableName = name.getOrElse(StringValue(param.name()))
  override val rawValue = this
  override val displayedValue = StringValue(param.displayedValue())
  val modulatedValue = param.modulatedValue()

  val exists = BoolValue(param.exists())

  def copy(name: Gettable[String]) = Parameter(param, Some(name))
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

case class SettableBoolValue(value: bitwig.SettableBooleanValue, name: Option[Gettable[String]] = None)(implicit
    ext: ControllerExtension
) extends BoolValue(value)
    with SettableValue[Boolean] {
  override def set(newValue: Boolean) = value.set(newValue)
  val toggle = Action(name.getOrElse("Toggle"), value.toggleAction(), this.map(if _ then 1.0 else 0.0))
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

class StringArrayValue(value: bitwig.StringArrayValue) extends Value[Array[String]](value) {
  override def get() = value.get
  override def addValueObserver(callback: Array[String] => Unit) = value.addValueObserver(
    new ObjectValueChangedCallback {
      override def valueChanged(newValue: Array[String]) = callback(newValue)
    }
  )
}

case class SettableStringArrayValue(value: bitwig.SettableStringArrayValue)
    extends StringArrayValue(value)
    with SettableValue[Array[String]] {
  override def set(newValue: Array[String]) = value.set(newValue)
}

case class TrackBank(bank: bitwig.TrackBank)(implicit ext: ControllerExtension) extends Bank(bank, t => Track(t)) {
  def tracks = items
  val channelCount = new IntValue(bank.channelCount)
}

class DeviceChain[T <: bitwig.DeviceChain](val chain: T)(implicit ext: ControllerExtension) {
  val name = SettableStringValue(chain.name)
  val exists = new BoolValue(chain.exists())

  def createBank(size: Int, matcher: Option[DeviceMatcher] = None) = DeviceBank(chain.createDeviceBank(size), matcher)
  def firstDeviceMatching(matcher: DeviceMatcher) = createBank(1, Some(matcher))(0)

  val instrumentDevice = firstDeviceMatching(ext.Matchers.instrument)
  val fxDevice = firstDeviceMatching(ext.Matchers.effect)
  val primaryDevice = firstDeviceMatching(ext.Matchers.instrumentOrEffect)

  val start = InsertionPoint(chain.startOfDeviceChainInsertionPoint(), "At Start")
  val end = InsertionPoint(chain.endOfDeviceChainInsertionPoint(), "At End")
}

class Channel(val channel: bitwig.Channel)(using ControllerExtension) extends DeviceChain(channel) {
  val volume = Parameter(channel.volume, name = name.map(_ + " volume"))
  val pan = Parameter(channel.pan, name = name.map(_ + " pan"))
  val mute = SettableBoolValue(channel.mute)
  val solo = SettableBoolValue(channel.solo)
  val active = SettableBoolValue(channel.isActivated)

  def select() = channel.selectInMixer()
  val selectAction = Action(name, () => select())

  object vuMeter extends Serializable {
    class VuMeterChannels(rms: Boolean) extends Serializable {
      private def vuMeter(chan: Int, rms: Boolean) = ValueFromLastObserved(
        0,
        callback =>
          channel.addVuMeterObserver(
            100,
            -1,
            true,
            new IntegerValueChangedCallback { override def valueChanged(newValue: Int): Unit = callback(newValue) }
          )
      )

      val left = vuMeter(0, rms)
      val right = vuMeter(1, rms)
      val sum = vuMeter(-1, rms)

      override def toJson() = ujson.Obj(
        "left" -> left(),
        "right" -> right(),
        "sum" -> sum()
      )
    }

    val rms = VuMeterChannels(false)
    val peak = VuMeterChannels(true)
    override def toJson() = ujson.Obj(
      "rms" -> rms,
      "peak" -> peak
    )
  }
  vuMeter

  val delete = Action("Delete", channel.deleteObjectAction())

}

class Track[T <: bitwig.Track](val track: T)(implicit ext: ControllerExtension) extends Channel(track), Serializable {
  val trackType = StringValue(track.trackType)
  val arm = SettableBoolValue(track.arm, "Arm")
  val clipBank = ClipBank(track.clipLauncherSlotBank, track)
  val isGroup = BoolValue(track.isGroup)
  val groupExpanded = SettableBoolValue(track.isGroupExpanded)
  val position = IntValue(track.position)
  val duplicate = Action(name.map("Copy " + _), () => track.duplicate())
  val duplicateAndUnarm = Action(
    name.map("Copy " + _),
    () => {
      // TODO: different behavior for cursor/non-cursor
      track.duplicate()
      arm.set(false)
    }
  )
  val isSelected =
    if ext.selectedTrack == null then Observable(false) else BoolValue(track.createEqualsValue(ext.selectedTrack.track))
  lazy val devices = createBank(10)

  def children(size: Int) = TrackBank(track.createTrackBank(size, ext.numSends, ext.numScenes, false))

  def sendBank = if (!Set("Master", "Effect").contains(trackType)) track.sendBank() else null

  def toJson() = ujson.Obj(
    "name" -> name(),
    "arm" -> arm(),
    "mute" -> mute(),
    "type" -> trackType(),
    "isSelected" -> isSelected(),
    "devices" -> devices,
    "vuMeter" -> vuMeter
  )
}

case class ClipBank(bank: ClipLauncherSlotBank, track: bitwig.Track)(implicit ext: ControllerExtension)
    extends Bank(bank, slot => ClipSlot(slot, bank, track)) {
  def stop() = bank.stop()
  val stopAction = bank.stopAction()

  def clips = items
}

class ClipSlot(clipSlot: bitwig.ClipLauncherSlot, bank: bitwig.ClipLauncherSlotBank, track: bitwig.Track)(using
    ControllerExtension
) extends Serializable {
  val hasClip = BoolValue(clipSlot.hasContent())
  val isPlaying = BoolValue(clipSlot.isPlaying())
  val isPlayingQueued = BoolValue(clipSlot.isPlaybackQueued())
  val isRecording = BoolValue(clipSlot.isRecording())
  val isRecordingQueued = BoolValue(clipSlot.isRecordingQueued())
  val exists = BoolValue(clipSlot.exists())
  val launch = Action(hasClip.map(if _ then "launch" else "loop"), clipSlot.launchAction())
  def launchWith(quantization: Option[Transport.LaunchQ.Value], mode: Option[Transport.LaunchMode.Value]) =
    clipSlot.launchWithOptions(
      quantization.map(_.toString()).getOrElse("default"),
      mode.map(_.toString()).getOrElse("default")
    )
  val trackName = SettableStringValue(track.name())
  val toggle =
    Action(
      Gettable(() => s"Toggle ${trackName()}", trackName),
      () => if isPlaying() || isPlayingQueued() then bank.stop() else launch(),
      Gettable(() => if isPlaying() then 1.0 else if isRecording() then 0.5 else 0.0, List(isPlaying, isRecording))
    )
  def delete() = clipSlot.deleteObject()
  def record() = clipSlot.record()

  def toJson() = ujson.Obj(
    "name" -> trackName(),
    "hasClip" -> hasClip(),
    "playing" -> isPlaying(),
    "recording" -> isRecording(),
    "playingQueued" -> isPlayingQueued(),
    "recordingQueued" -> isRecordingQueued()
  )
}

class TrackCursor(cursorName: String, track: bitwig.CursorTrack)(using ext: ControllerExtension)
    extends Track(track),
      HardwareBindable(track),
      RelativeHardwareControlBindable[bitwig.CursorTrack] {
  def this(id: String, cursorName: String, follow: Boolean)(using ext: ControllerExtension) =
    this(cursorName, ext.host.createCursorTrack(id, cursorName, ext.numSends, ext.numScenes, follow))

  override val bindableName = cursorName
  override val rawValue =
    Gettable(() => position().toDouble / ext.allTracks.currentSize(), List(position)) //
  override val displayedValue = name

  val hasNext = new BoolValue(track.hasNext)
  val isPinned = SettableBoolValue(track.isPinned)

  val selectPrevious = Action("Select Prev", track.selectPreviousAction())
  val selectNext = Action("Select Next", track.selectNextAction())
  def selectChannel(channel: Channel) = track.selectChannel(channel.channel)

  val parent = new Track(track.createParentTrack(ext.numSends, 1))

  lazy val clipCursor = ClipCursor(track.createLauncherCursorClip("CLIP", "Clip", 0, 0))
  lazy val deviceCursor = DeviceCursor("Device", track.createCursorDevice())
  override val duplicateAndUnarm = Action(
    "Copy",
    () => {
      // TODO: different behavior for cursor/non-cursor
      arm.set(false)
      track.duplicate()
      selectNext()
      arm.set(true)
    }
  )

  def loopLastNBarsOfRecordingClip(bars: BeatTime): Unit = {
    if (!clipCursor.exists()) {
      clipCursor.selectFirst()
    }
    if (!clipCursor.slot.isRecording()) return
    val beatsPerBar = ext.transport.beatsPerBar()

    val loopLength = clipCursor.loopLength() + ext.transport.tilNextBar
    val endTime = (loopLength / beatsPerBar).round * beatsPerBar
    val startTime = endTime - bars * beatsPerBar
    clipCursor.loopStart.set(startTime)
    clipCursor.playStart.set(startTime + loopLength - endTime)
    clipCursor.loopLength.set(endTime - startTime)
    clipCursor.slot.isPlaying.doAfterChanged(_ => {
      clipCursor.loopLength.set(endTime - startTime)
    })
    if (endTime > loopLength) {
      clipCursor.slot.launchWith(Some(Transport.LaunchQ.One), Some(Transport.LaunchMode.Synced))
    } else {
      clipCursor.slot.launchWith(Some(Transport.LaunchQ.None), Some(Transport.LaunchMode.ContinueOrSynced))
    }
  }

  def cycleNextTrack() = {
    if (track.hasNext.get) track.selectNext() else track.selectFirst()
  }

  override def toJson() = super[RelativeHardwareControlBindable].toJson()
}

case class ClipCursor(clipCursor: PinnableCursorClip)(implicit ext: ControllerExtension) {
  val exists = new BoolValue(clipCursor.exists())
  val loopStart = SettableBeatTimeValue(clipCursor.getLoopStart, "Loop Start")
  val loopLength = SettableBeatTimeValue(clipCursor.getLoopLength, "Loop Length")
  val playStart = SettableBeatTimeValue(clipCursor.getPlayStart(), "Play Start")
  val slot =
    ClipSlot(clipCursor.clipLauncherSlot(), clipCursor.getTrack().clipLauncherSlotBank(), clipCursor.getTrack())

  def selectFirst() = clipCursor.selectFirst()
}

class Bank[T <: Serializable, BT <: bitwig.ObjectProxy](bank: bitwig.Bank[BT], itemCtor: BT => T)(using
    ControllerExtension
) extends Serializable {
  val currentSize = new IntValue(bank.itemCount())
  def currentItems = items.slice(0, currentSize())
  def maxSize = bank.getSizeOfBank()

  val items = List.range(0, maxSize).map(idx => itemCtor(bank.getItemAt(idx)))

  def apply(idx: Int) = items(idx)

  def toJson() = ujson.Arr((0 until currentSize()).map(apply(_).toJson()): _*)
}

case class DeviceBank(bank: bitwig.DeviceBank, matcher: Option[bitwig.DeviceMatcher] = None)(using ControllerExtension)
    extends Bank(bank, device => Device(device)) {
  def devices = items
  matcher.foreach(bank.setDeviceMatcher(_))
}

class Device(val device: bitwig.Device)(implicit ext: ControllerExtension) extends Serializable {
  val name = StringValue(device.name())
  val presetName = StringValue(device.presetName())
  val remoteControls = RemoteControls(device.createCursorRemoteControlsPage(8), device)
  val exists = BoolValue(device.exists())
  val windowOpen = SettableBoolValue(device.isWindowOpen())
  val enabled = SettableBoolValue(device.isEnabled())
  val position = IntValue(device.position())
  val delete = Action("Delete", device.deleteObjectAction())
  def equalsValue(other: Device) = BoolValue(device.createEqualsValue(other.device))
  lazy val isSelected =
    if ext.selectedTrack == null then Observable(false) else equalsValue(ext.selectedTrack.deviceCursor)

  lazy val selector = Selector(device.createChainSelector(), this)
  lazy val layers = {
    val bank = device.createLayerBank(8)
    (0 until 8).map(idx => new Channel(bank.getItemAt(idx)))
  }
  lazy val slot = new DeviceChain(device.getCursorSlot())

  def createTaggedRemoteControlsPage(name: String, tag: String) = RemoteControls(
    device.createCursorRemoteControlsPage(name, 8, tag),
    device
  )

  val before = InsertionPoint(device.beforeDeviceInsertionPoint(), "Before")
  val replace = InsertionPoint(device.replaceDeviceInsertionPoint(), "")
  val after = InsertionPoint(device.afterDeviceInsertionPoint(), "After")

  def toJson() = ujson.Obj(
    "deviceName" -> name(),
    "presetName" -> presetName(),
    "enabled" -> enabled(),
    "isSelected" -> isSelected()
  )
}

class DeviceCursor(cursorName: String, override val device: bitwig.CursorDevice)(using ControllerExtension)
    extends Device(device),
      HardwareBindable(device),
      Cursor[bitwig.CursorDevice] {
  override val bindableName = cursorName
  override val rawValue = Gettable(0.0)
  override val displayedValue = name
  def selectDevice(other: Device) = device.selectDevice(other.device)
  def selectFirstInSlot(name: String) = device.selectFirstInSlot(name)

  override def toJson() = super[Cursor].toJson()
}

case class DeviceLayer(layer: bitwig.DeviceLayer)(implicit ext: ControllerExtension) extends Channel(layer)

case class Selector(selector: bitwig.ChainSelector, device: Device)(implicit ext: ControllerExtension) {
  val activeChain = DeviceLayer(selector.activeChain())
  val activeChainIndex = SettableIntValue(selector.activeChainIndex())
  val chainCount = new IntValue(selector.chainCount())
  val exists = new BoolValue(selector.exists())
}

class RemoteControl(remoteControl: bitwig.RemoteControl)(implicit ext: ControllerExtension)
    extends Parameter(remoteControl) {
  val name = SettableStringValue(remoteControl.name())
  val isBeingMapped = SettableBoolValue(remoteControl.isBeingMapped())

  val startMapping = Action("Start Mapping", () => isBeingMapped.set(true))
}

class RemoteControls(remoteControls: CursorRemoteControlsPage, device: bitwig.Device)(implicit
    ext: ControllerExtension
) {
  val controls = List.range(0, 8).map(remoteControls.getParameter).map(ctl => RemoteControl(ctl))
  val selectedPage = SettableIntValue(remoteControls.selectedPageIndex())
  val visible = SettableBoolValue(device.isRemoteControlsSectionVisible())
  val pageNames = StringArrayValue(remoteControls.pageNames())
  val pageCount = IntValue(remoteControls.pageCount())

  def apply(idx: Int) = controls(idx)

  val selectPageAction =
    (0 until 8).map(idx =>
      Action(
        pageNames.map(pageNames => if idx < pageNames.length then pageNames(idx) else s"Select Page ${idx + 1}"),
        () => selectedPage.set(idx)
      )
    )

  val createPage = Action("Create Page", remoteControls.createPresetPageAction())
}

case class InsertionPoint(insertionPoint: bitwig.InsertionPoint, name: String)(implicit ext: ControllerExtension) {
  val browseAction = Action(s"Insert ${name}", insertionPoint.browseAction())
  val browseIfClosedAction = Action(
    s"Insert ${name}",
    () => {
      if !ext.browser.isOpen() then browseAction()
    }
  )
  def insert(device: SpecificDeviceSpec) = device.insert(this)
}

case class SelectorSetting[T](
    name: String,
    possibleSelections: Map[String, SettingCtx => Setting[T]],
    initialSelection: String
)(implicit settingCtx: SettingCtx)
    extends WritableSetting[T] {
  val selection = EnumSetting(name, possibleSelections.keys.toList.sorted, initialSelection)
  val selectionConfigs = possibleSelections.map({ case (name, ctor) => name -> ctor(settingCtx.indented) }).toMap
  selectionConfigs.values.foreach(_.disableAndHide())
  selectionConfigs.get(selection.get()).foreach(_.enableAndShow())

  var firstTime = true
  selection.onChanged(newselection => {
    if (!firstTime) {
      selectionConfigs.values.foreach(_.disableAndHide())
      selectionConfigs.get(newselection).foreach(_.enableAndShow())
    }
    firstTime = false
  })

  override def get() = selectionConfigs(selection.get()).get()
  override def set(newSelection: T) = selection.set(
    selectionConfigs
      .find({ case (name, config) => config.get() == selection })
      .getOrElse(throw new Exception(s"couldn't find $newSelection!!"))
      ._1
  )

  override def setEnabled(enabled: Boolean) = {
    selection.setEnabled(enabled)
    selectionConfigs.values.foreach(_.setEnabled(false))
    selectionConfigs.get(selection.get()).foreach(_.setEnabled(enabled))
  }
  override def setShown(shown: Boolean) = {
    selection.setShown(shown)
    selectionConfigs.values.foreach(_.setShown(false))
    selectionConfigs.get(selection.get()).foreach(_.setShown(shown))
  }
  override def onChanged(callback: T => Unit) = {
    (List(selection) ++ selectionConfigs.values).foreach(_.onChanged(_ => callback(get())))
  }
}

abstract class Enumeration extends scala.Enumeration {
  class EnumValue(value: bitwig.EnumValue) extends com.garo.Value[Value](value) {
    protected def addValueObserver(callback: Value => Unit) =
      value.addValueObserver(new EnumValueChangedCallback {
        override def valueChanged(newValue: String) = callback(withName(value.get()))
      })
    override def get() = withName(value.get())
  }

  case class SettableEnumValue(value: bitwig.SettableEnumValue) extends EnumValue(value) with SettableValue[Value] {
    override def set(newValue: Value) = value.set(newValue.toString())
  }
}

class Transport(transport: bitwig.Transport)(implicit ext: ControllerExtension) {
  val isPlaying = SettableBoolValue(transport.isPlaying())
  val playPosition = BeatTimeValue(transport.playPosition())
  val record = SettableBoolValue(transport.isArrangerRecordEnabled(), "record")
  // TODO: properly enable values from multi-bindings and remove this hack
  val addCue = Action("add cue", transport.addCueMarkerAtPlaybackPositionAction(), value = record.toggle.rawValue)
  val tempo = Parameter(transport.tempo)

  val defaultLaunchQ = Transport.LaunchQ.SettableEnumValue(transport.defaultLaunchQuantization())

  val postRecordAction = Transport.PostRecordAction.SettableEnumValue(transport.clipLauncherPostRecordingAction())
  val postRecordTime = SettableBeatTimeValue(transport.getClipLauncherPostRecordingTimeOffset(), "Loop Length")

  val play = Action(isPlaying.map(if _ then "pause" else "play"), transport.playAction())
  val playAndRecordOrStop = Action(
    isPlaying.map(if _ then "stop" else "w record"),
    () => if isPlaying() then stop() else { record.toggle(); play() }
  )
  val stop = Action("stop", transport.stopAction())
  val tapTempo = Action("tap tempo", transport.tapTempoAction())

  val beatsPerBar = SettableIntValue(transport.timeSignature.numerator)

  def cancelOnPause(cancel: () => Unit) = {
    var thisCancel = Option.empty[() => Unit]
    thisCancel = Some(isPlaying.onChanged(isPlaying => {
      if (!isPlaying) {
        cancel()
        thisCancel.foreach(_())
      }
    }))
    cancel
  }

  def onTick(callback: BeatTime => Unit) = {
    var cancel: () => Unit = null
    cancel = playPosition.onChanged(playPosition => {
      if (!isPlaying()) cancel() else callback(playPosition)
    })
    cancel
  }

  def doAt(time: BeatTime, action: () => Unit) = {
    ext.host.println(s"do at: $time, current: ${playPosition()}")
    if (time <= playPosition()) { () =>
      {}
    } else {
      var cancel: () => Unit = null
      cancel = onTick(currentTime => {
        if (currentTime >= time) {
          action()
          cancel()
        }
      })
      cancel
    }
  }

  def doBefore(time: BeatTime, action: () => Unit) = {
    doAt(time - BeatTime.sixteenths(1), action)
  }

  def doBeforeOffset(time: BeatTime, action: () => Unit) = {
    doBefore(playPosition() + time, action)
  }

  def tilNextBar = playPosition().ceilBar - playPosition()

}

object Transport {
  object LaunchQ extends Enumeration {
    val None = Value("none")
    val Eight = Value("8")
    val Four = Value("4")
    val Two = Value("2")
    val One = Value("1")
    val Half = Value("1/2")
    val Quarter = Value("1/4")
    val Eigth = Value("1/8")
    val Sixteenth = Value("1/16")
  }

  object LaunchMode extends Enumeration {
    val FromStart = Value("from_start")
    val ContinueOrFromStart = Value("continue_or_from_start")
    val ContinueOrSynced = Value("continue_or_synced")
    val Synced = Value("synced")
  }

  object PostRecordAction extends Enumeration {
    val Off = Value("off")
    val PlayRecorded = Value("play_recorded")
    val RecordNextFreeSlot = Value("record_next_free_slot")
    val Stop = Value("stop")
    val ReturnToArrangement = Value("return_to_arrangement")
    val ReturnToPreviousClip = Value("return_to_previous_clip")
    val PlayRandom = Value("play_random")
  }
}

case class ControllerExtensionProxy(
    extCtor: ControllerHost => ControllerExtension,
    defn: bitwigController.ControllerExtensionDefinition,
    host: ControllerHost
) extends bitwigController.ControllerExtension(defn, host) {
  lazy val ext = extCtor(host)
  override def init() = {
    // TODO: iz ugly
    ext._isInitializaing = false
  }

  override def exit() = {
    host.println("exiting!")
    ext.exit()
  }

  override def flush() = {
    ext.flush()
  }
}

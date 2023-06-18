package com.garo

import java.util.UUID
import java.lang.reflect._;

import com.bitwig.extension.api._
import com.bitwig.extension.controller._
import com.bitwig.extension.api.util.midi._
import com.bitwig.extension.callback._
import com.bitwig.extension.controller.api._

import com.garo.ControllerExtensionProxy
import java.{util => ju}

case class TestExtension(_host: ControllerHost) extends MyControllerExtension()(_host) {
  def supers(cls: Class[_]): Seq[Class[_]] = {
    val sup = cls.getSuperclass
    if (sup != null) sup +: supers(sup) else Seq()
  }
  def clsInfo(value: Option[Object] = None, clsArg: Option[Class[_]] = None) = {
    val cls = clsArg.getOrElse(value.get.getClass)
    host.println(s"\n${cls.toString} Info:\n")
    host.println("  superclasses: " + supers(cls).toString)
    host.println("  interfaces: " + (cls +: supers(cls)).flatMap(_.getInterfaces).toSeq.toString)

    if (
      supers(cls)
        .exists(_.toString == "class com.bitwig.flt.control_surface.proxy.ControlSurfaceObject") && value.isDefined
    ) {
      val path = cls.getMethod("getPath").invoke(value.get)
      host.println("  path: " + path.toString())
      val parent = cls.getMethod("getParent").invoke(value.get)
      host.println("  parent: " + parent.getClass.toString)
      val children = cls.getMethod("getChildren").invoke(value.get)
      if (children != null) {
        host.println("\n  children: \n")
        children.asInstanceOf[ju.Set[_]].forEach(o => host.println("    " + o.getClass.toString))
      }
    }

    host.println(s"\n  constructors:\n")
    cls.getConstructors().foreach(c => host.println("    " + c.toString))
    host.println(s"\n  Fields:\n")
    cls.getDeclaredFields.foreach(f => host.println("    " + f.toString))
    host.println(s"\n  Methods:\n")
    cls.getMethods.foreach(m => host.println("    " + m.toString))
    host.println(s"\n  Declared Methods:\n")
    cls.getMethods.foreach(m => host.println("    " + m.toString))
  }

  // postFrame(() => {
  //   val clipSlot = mainTrackBank.tracks.head.clipSlot.clipSlot
  //   clipSlot.createEmptyClip(4)
  //   val cls = clipSlot.getClass
  //   clsInfo(Some(clipSlot))
  //   val getClip = cls.getDeclaredMethod("getClip")
  //   val clip = getClip.invoke(clipSlot)
  //   val clipCls = clip.getClass
  //   // clsInfo(Some(clip))

  //   clsInfo(Some(trackCursors.head.clipCursor))

  //   clsInfo(Some(mainTrackBank.tracks.head.track))
  //   clsInfo(clsArg = Some(Class.forName("com.bitwig.flt.control_surface.proxy.ClipProxy")))

  //   clsInfo(clsArg = Some(Class.forName("com.bitwig.flt.control_surface.proxy.DelegatingClipProxy")))
  // })
  val testSerialize = preferences.getStringSetting("Test", "Test", 10000000, "init")
  host.println(testSerialize.get.toString)

  val controls = host.createUserControls(2)
  controls.getControl(0).setLabel("hallo")
  clsInfo(Some(mainTrackBank(0).track.sourceSelector))

  documentState.getSignalSetting("Do it", "Do it", "Do it").addSignalObserver(() => controls.getControl(0).touch(true))
}

class TestExtensionDefinition extends ControllerExtensionDefinition {
  val DRIVER_ID = UUID.fromString("aa0399c3-409c-4901-957f-cb7f214d6de9")

  override def getName = "test"

  override def getAuthor = "garo"

  override def getVersion = "0.1"

  override def getId = DRIVER_ID

  override def getHardwareVendor = "garo"

  override def getHardwareModel = "test"

  override def getRequiredAPIVersion = 17

  override def getNumMidiInPorts = 0

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
      //   list.add(Array("LPD8 Midi 1"), Array("LPD8 MIDI 1"))
    }
  }

  override def createInstance(host: ControllerHost) = ControllerExtensionProxy(TestExtension.apply, this, host)
}

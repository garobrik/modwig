package com.garo

import com.bitwig.extension.controller.api._;
import com.bitwig.extension.controller.{api => bitwig};
import java.util.UUID

class SpecificVST3Device[T](ctor: SpecificPluginDevice => T, val id: String) {
  def matcher(implicit ext: MyControllerExtension) = ext.host.createVST3DeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificVst3Device(id))
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
}

class SpecificBitwigDevice[T](ctor: bitwig.SpecificBitwigDevice => T, idString: String) {
  val id = UUID.fromString(idString)
  def matcher(implicit ext: MyControllerExtension) = ext.host.createBitwigDeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificBitwigDevice(id))
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
}

case class EnsoDevice(device: SpecificPluginDevice) {
  val lengthMult = device.createParameter(50)
}

object Enso extends SpecificVST3Device(EnsoDevice, "565354456E736F656E736F0000000000")

case class InstrumentSelectorDevice(device: bitwig.SpecificBitwigDevice) {
  val index = Parameter(device.createParameter("INDEX"))
}

object InstrumentSelector extends SpecificBitwigDevice(InstrumentSelectorDevice, "9588fbcf-721a-438b-8555-97e4231f7d2c")

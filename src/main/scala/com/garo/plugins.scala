package com.garo

import com.garo._;
import com.bitwig.extension.controller.api.{Device => _, Track => _, Parameter => _, InsertionPoint => _, _};
import com.bitwig.extension.controller.{api => bitwig};
import java.util.UUID

abstract class SpecificDevice[T] {
  def matcher(implicit ext: MyControllerExtension): DeviceMatcher
  def of(device: Device): T
  def of(track: Track)(implicit ext: MyControllerExtension): T
  def insert(insertionPoint: InsertionPoint): Unit
}

class SpecificVST3Device[T](ctor: (SpecificPluginDevice, Device) => T, val id: String) extends SpecificDevice[T] {
  def matcher(implicit ext: MyControllerExtension) = ext.host.createVST3DeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificVst3Device(id), device)
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertVST3Device(id)
}

class SpecificBitwigDevice[T](ctor: (bitwig.SpecificBitwigDevice, Device) => T, idString: String)
    extends SpecificDevice[T] {
  val id = UUID.fromString(idString)
  def matcher(implicit ext: MyControllerExtension) = ext.host.createBitwigDeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificBitwigDevice(id), device)
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertBitwigDevice(id)
}

case class EnsoDevice(specificDevice: SpecificPluginDevice, device: Device) {
  val lengthMult = specificDevice.createParameter(50)
}

object Enso extends SpecificVST3Device(EnsoDevice.apply, "565354456E736F656E736F0000000000")

case class InstrumentSelectorDevice(specificDevice: bitwig.SpecificBitwigDevice, device: Device) {
  val index = Parameter(specificDevice.createParameter("INDEX"))
}

object InstrumentSelector
    extends SpecificBitwigDevice(InstrumentSelectorDevice.apply, "9588fbcf-721a-438b-8555-97e4231f7d2c")

case class ChainDevice(specificDevice: bitwig.SpecificBitwigDevice, device: Device) {
  val gain = Parameter(specificDevice.createParameter("GAIN"))
  val mix = Parameter(specificDevice.createParameter("MIX"))
}

object Chain extends SpecificBitwigDevice(ChainDevice.apply, "c86d21fb-d544-4daf-a1bf-57de22aa320c")

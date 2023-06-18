package com.garo

import com.bitwig.extension.controller.api._;
import com.bitwig.extension.controller.{api => bitwig};
import java.util.UUID

abstract class SpecificDevice[T] {
  def matcher(implicit ext: MyControllerExtension): DeviceMatcher
  def of(device: Device): T
  def of(track: Track)(implicit ext: MyControllerExtension): T
  def insert(insertionPoint: InsertionPoint)
}

class SpecificVST3Device[T](ctor: SpecificPluginDevice => T, val id: String) extends SpecificDevice[T] {
  def matcher(implicit ext: MyControllerExtension) = ext.host.createVST3DeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificVst3Device(id))
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertVST3Device(id)
}

class SpecificBitwigDevice[T](ctor: bitwig.SpecificBitwigDevice => T, idString: String) extends SpecificDevice[T] {
  val id = UUID.fromString(idString)
  def matcher(implicit ext: MyControllerExtension) = ext.host.createBitwigDeviceMatcher(id)
  def of(device: Device): T = ctor(device.device.createSpecificBitwigDevice(id))
  def of(track: Track)(implicit ext: MyControllerExtension): T = of(track.firstDeviceMatching(matcher))
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertBitwigDevice(id)
}

case class EnsoDevice(device: SpecificPluginDevice) {
  val lengthMult = device.createParameter(50)
}

object Enso extends SpecificVST3Device(EnsoDevice, "565354456E736F656E736F0000000000")

case class InstrumentSelectorDevice(device: bitwig.SpecificBitwigDevice) {
  val index = Parameter(device.createParameter("INDEX"))
}

object InstrumentSelector extends SpecificBitwigDevice(InstrumentSelectorDevice, "9588fbcf-721a-438b-8555-97e4231f7d2c")

case class ChainDevice(device: bitwig.SpecificBitwigDevice) {
  val gain = Parameter(device.createParameter("GAIN"))
  val mix = Parameter(device.createParameter("MIX"))
}

object Chain extends SpecificBitwigDevice(ChainDevice, "c86d21fb-d544-4daf-a1bf-57de22aa320c")

package com.garo

import com.garo._;
import com.bitwig.extension.controller.api.{Device => _, Track => _, Parameter => _, InsertionPoint => _, _};
import com.bitwig.extension.controller.{api => bitwig};
import java.util.UUID

abstract class SpecificDevice[T] {
  def matcher(implicit ext: ControllerExtension): DeviceMatcher
  def insert(insertionPoint: InsertionPoint): Unit
}

class SpecificVST3Device[T](id: String, defaultParam: Int) extends SpecificDevice[T] {
  def matcher(implicit ext: ControllerExtension) = ext.host.createVST3DeviceMatcher(id)
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertVST3Device(id)

  abstract class BaseDevice(device: com.garo.Device) {
    val specificDevice = device.device.createSpecificVst3Device(id)
    def createParameter(id: Int) = Parameter(specificDevice.createParameter(id))
    val exists = {
      val param = createParameter(defaultParam)
      param.exists
    }
  }
}

class SpecificBitwigDevice[T](idString: String, defaultParam: String) extends SpecificDevice[T] {
  val id = UUID.fromString(idString)
  def matcher(implicit ext: ControllerExtension) = ext.host.createBitwigDeviceMatcher(id)
  def insert(insertionPoint: InsertionPoint) = insertionPoint.insertionPoint.insertBitwigDevice(id)

  abstract class BaseDevice(device: com.garo.Device) {
    val specificDevice = device.device.createSpecificBitwigDevice(id)
    def createParameter(id: String) = Parameter(specificDevice.createParameter(id))
    val exists = {
      val param = createParameter(defaultParam)
      param.exists
    }
  }
}

object Enso extends SpecificVST3Device("565354456E736F656E736F0000000000", 50) {
  class Device(device: com.garo.Device) extends super.BaseDevice(device) {
    val lengthMult = createParameter(50)
  }
}

object InstrumentSelector extends SpecificBitwigDevice("9588fbcf-721a-438b-8555-97e4231f7d2c", "INDEX") {
  class Device(device: com.garo.Device) extends super.BaseDevice(device) {
    val index = Parameter(specificDevice.createParameter("INDEX"))
  }
}

object Chain extends SpecificBitwigDevice("c86d21fb-d544-4daf-a1bf-57de22aa320c", "MIX") {
  class Device(device: com.garo.Device) extends super.BaseDevice(device) {
    val gain = Parameter(specificDevice.createParameter("GAIN"))
    val mix = Parameter(specificDevice.createParameter("MIX"))
  }
}

object DrumSynth extends SpecificVST3Device("ABCDEF019182FAEB4149526D4D755379", 48) {
  class Device(device: com.garo.Device) extends super.BaseDevice(device)
}

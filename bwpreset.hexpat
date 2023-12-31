#pragma eval_depth 50
#pragma endian big
#include "type/magic.pat"
#include "type/guid.pat"
#include "type/base.pat"
#include "std/mem.pat"

using BObj;
using BField;

namespace Decimal {
  struct Conv {
    u8 values[4];
  };
  
  fn toHex(Conv conv) {
    u64 base = 1;
    u64 acc = 0;
    for (u8 i = 0, i < 4, i = i + 1) {
      u8 val = conv.values[3 - i];
      if (val < 0x4A) val = val - 0x30;
      else val = val - 87;
      acc = acc + base * val;
      base = base * 16;
    }
    return acc;
  };

  struct U32 {
    Conv values;
    u64 value = Decimal::toHex(values);
  };
}

enum ValueType : u8 {
  U8 = 0x01, // S8?
  Un02 = 0x02,
  Int = 0x03,
  Bool = 0x05,
  Double = 0x07,
  Str = 0x08,
  BObj = 0x09,
  Null = 0x0A,
  Un0B = 0x0B,
  Un0D = 0x0D,
  U32Arr = 0x0F,
  BObjList = 0x12,
  UUID = 0x15,
  Un16 = 0x16,
  FloatArr = 0x17,
  StrArr = 0x19,
};
fn formatValueType(ValueType type) {
  return std::string::substr(std::string::to_string(type), 11, 30);
};

struct String {
  bool utf = std::mem::read_unsigned($, 1) == 0x80;
  u32 sz;
  if (utf) {
        char16 value[sz & 0x0FFFFFFF];
      } else {
        char value[sz];
        }
}[[format("formatString")]];
fn formatString(String string) {
  return std::format("\"{}\"", std::string::to_string(string.value));
};


struct Value {
  ValueType type [[color("00FF00"), hidden]];
  std::print("{} {:x}", type, std::mem::read_unsigned($, 8, std::mem::Endian::Big));
  
  match (type) {
    (ValueType::U8): u8 value;
    (ValueType::Un02): u16 value;
    (ValueType::Str): String value [[color("004400")]];
    (ValueType::Int): u32 value;
    (ValueType::Double): double value;
    (ValueType::UUID): type::Hex<u128> value;
    (ValueType::StrArr): {
      u32 sz;
        String value[sz];
    }
    (ValueType::Bool): bool value;
    (ValueType::BObj): BObj value [[inline]];
    (ValueType::Null): {
      str value = "";
    }
    (ValueType::Un0B): u32;
    (ValueType::Un0D): std::string::SizedString<u32> value;
    (ValueType::U32Arr): {
      u32 sz;
      u32 values[sz];
    }
    (ValueType::Un16): u128 value;
    (ValueType::FloatArr): {
      u32 sz;
      float value[sz];
    }
    (ValueType::BObjList): {
      if (std::mem::read_unsigned($, 4, std::mem::Endian::Big) == 0x1) {
        u64 value;
      } else {
        BObj value[while(std::mem::read_unsigned($, 4, std::mem::Endian::Big) != 0x03)] [[inline]];
      }
      type::Magic<"\x00\x00\x00\x03"> end_list [[color("000088"), hidden]];
    }
  }
};

struct SField {
  std::string::SizedString<u32> name;
  Value value;
};

enum ObjType : u32 {
  Field = 0x01,
  Dict = 0x04,
};

struct Obj {
  ObjType type;
  
  match (type) {
    (ObjType::Field): SField field;
    (ObjType::Dict): {
      std::string::SizedString<u32> name;
      Obj entries[while(std::mem::read_unsigned($, 4) != 0x0)];
      type::Magic<"\x00\x00\x00\x00"> end_dict [[color("000088"), hidden]];
    }
  }
};

struct prefix {
  type::Magic<"BtWg">;
  u32;
  Decimal::U32 format;
  u32;
  u32;
  Decimal::U32 preset_start;
  u32;
  u32;
  u32;
  Decimal::U32 vst_start;
  u16;
};

enum BFieldName: u32 {
  ID = 0x02B9,
  PresetName = 0x12DE,
  DeviceName = 0x9A,
  UserDefinedName = 0x1559,
  DeviceCreator = 0x9B,
  DeviceCat = 0x9C,
  PresetAuthor = 0x9E,
  PresetDesc = 0x9F,
  PresetCat = 0xA1,
  PresetTags = 0xA2,
  DeviceOn = 0xA3,
  DeviceActive = 0x137E,
  DeviceModulators = 0x18F5,
  PresetRCPages = 0x1A85,
  DeviceRCPage = 0x1B75,
  RCName = 0x1B72,
  
  DeviceOpen = 0x1A8C,
  DeviceRemoteControlsOpen = 0x1A8D,
  DeviceModsOpen = 0x1AA9,
  
  DeviceID = 0x99,
  DeviceContents = 0xA4,
  ParamList = 0x020C,
  ModList = 0x1A46,
  ModContents = 0x18C7,
  Value = 0x136,
  Device = 0x1421,
  
  // RCPages
  RCPagesPages = 0x1A7E,
  
  // RCPage
  RCPageName = 0x1B69,
  RCPageTags = 0x1B79,
  RCPageRCs = 0x1A7A,
  
  // RemoteControl
  RCName = 0x1A7B,
  RCTarget = 0x1A7C,
  RCIndex = 0x1A88,
  RCMeta = 0x1A7D,
  
  // ModSource
  ModSourceTargets = 0x0E20,
  
  // ModTarget
  ModTargetTarget = 0x0E3D,
  ModTargetMeta = 0x1334,
  ModTargetAmt = 0x0E32,
  
  UUID = 0x18C6,
};
fn formatFieldName(BFieldName type) {
  return std::string::substr(std::string::to_string(type), 12, 30);
};


fn formatField(BField field) {
  str typePart = "";
  str valuePart = "";
  if (field.value.type > 0x08) {
    if (field.value.type == ValueType::BObj) {
      typePart = std::format(": {}", formatObjType(field.value.value.type));
    } else {
      typePart = std::format(": {}", formatValueType(field.value.type));
    }
  } else {
    valuePart = std::format(" = {}", field.value.value);
  }
  return std::format("{}{}{}", formatFieldName(field.name), typePart, valuePart);
};

struct BField {
  BFieldName name [[color("FF0000"), hidden]];
  Value value [[inline]];
} [[format("formatField")]];


enum BObjType : u32 {
  Mod = 0x06C9, 
  ModParam = 0x06DB,
  Label = 0x06D9,
  Value = 0x85,
  ModSource = 0x2FC,
  Preset = 0x561,
  BitwigDevice = 0x40,
  VSTDevice = 0x0728,
  DeviceChain = 0x24E,
  DoubleParam = 0x85,
  IntParam = 0xF7,
  BoolParam = 0x7F,
  U8Param = 0x189,
  GridPolyParams = 0xD94,
  GridModules = 0x771,
  RCPage = 0x077B,
  RemoteControl = 0x077C,
  ModTarget = 0x02FD,
};
fn formatObjType(BObjType type) {
  return std::string::substr(std::string::to_string(type), 10, 30);
};

fn formatObj(BObj obj) {
  return std::format("{}", formatObjType(obj.type));
};
struct BObj {
  BObjType type [[color("0000FF"), hidden]];
  match (type) {
    (_): {
      BField fields[while(std::mem::read_unsigned($, 4) != 0x0)] [[inline]];
    }
  }
  type::Magic<"\x00\x00\x00\x00"> end_object [[color("000088"), hidden]];
} [[format("formatObj")]];

struct preset {
  BObj;
};

struct vst {

};

struct bwpreset {
  prefix prefix;
  if (prefix.format.value == 0x02) {
    Obj meta;
    preset preset @ prefix.preset_start.value;
  }
  if (prefix.vst_start.value != 0) {
    vst vst @ prefix.vst_start.value;
  }
};

bwpreset preset @ 0x0;
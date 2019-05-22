unit Ntapi.ntmmapi;

interface

type
  TSectionImageInformation = record
    TransferAddress: Pointer;
    ZeroBits: Cardinal;
    MaximumStackSize: NativeUInt;
    CommittedStackSize: NativeUInt;
    SubSystemType: Cardinal;
    SubSystemVersion: Cardinal;
    OperatingSystemVersion: Cardinal;
    ImageCharacteristics: Word;
    DllCharacteristics: Word;
    Machine: Word;
    ImageContainsCode: Boolean;
    ImageFlags: Byte;
    LoaderFlags: Cardinal;
    ImageFileSize: Cardinal;
    CheckSum: Cardinal;
  end;
  PSectionImageInformation = ^TSectionImageInformation;

implementation

end.

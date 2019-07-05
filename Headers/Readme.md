## Header files

These files contain **Windows API** and **Native API** definitions adapted to use with Delphi. They include information from different sources, i.e.:

 - [Windows SDK](https://en.wikipedia.org/wiki/Microsoft_Windows_SDK) — for most of _Winapi.\*_ modules;
 - [phnt](https://github.com/processhacker/phnt) — for most of _Ntapi.\*_ modules;
 - [Windows DDK](https://en.wikipedia.org/wiki/Windows_Driver_Kit) — for some _Ntapi.\*_ modules (like _Ntapi.ntsam_);
 - [\[MS-WINPROTLP\]](https://docs.microsoft.com/en-us/openspecs/windows_protocols/) specifications — for _Lsa\*_, _Sam\*_, and _WinStation\*_ functions;
 - [Microsoft Docs](https://docs.microsoft.com/en-us/windows/) & [MSDN](https://msdn.microsoft.com/) — for some functions that I can't find in headers (like _LogonUserExExW_);
 - Reverse engineering and Internet forums — for things like _Winapi.Wdc_.
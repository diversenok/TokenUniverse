unit TU.Credentials;

interface
{$WARN SYMBOL_PLATFORM OFF}
// TODO: check delayed import

uses
  Winapi.Windows, TU.Tokens;

type
  TCredentialsCallback = procedure(Domain, User: String; Password: PWideChar)
    of object;

procedure PromptCredentialsUI(ParentWindow: HWND;
  Callback: TCredentialsCallback; AllowNoPassword: Boolean = False);

implementation

uses
  System.SysUtils, Winapi.Ole2, TU.Tokens.Types, NtUtils,
  NtUtils.Security.Sid, NtUtils.Lsa.Sid, NtUiLib.Exceptions;

type
  TCredUIInfoW = record
    cbSize: Cardinal;
    hwndParent: HWND;
    pszMessageText: PWideChar;
    pszCaptionText: PWideChar;
    hbmBanner: HBITMAP;
  end;

  PCredUIInfoW = ^TCredUIInfoW;

const
  CREDUIWIN_GENERIC = $00000001;

const
  credui = 'credui.dll';

function CredUIPromptForWindowsCredentialsW(const UiInfo: TCredUIInfoW;
  dwAuthError: Cardinal; var ulAuthPackage: Cardinal; pvInAuthBuffer: Pointer;
  ulInAuthBufferSize: Cardinal; out pvOutAuthBuffer: Pointer;
  out ulOutAuthBufferSize: Cardinal; pfSave: PLongBool; dwFlags: Cardinal)
  : Cardinal; stdcall; external credui delayed;

function CredUnPackAuthenticationBufferW(dwFlags: Cardinal;
  pAuthBuffer: Pointer; cbAuthBuffer: Cardinal; pszUserName: PWideChar;
  var cchMaxUserName: Cardinal; pszDomainName: PWideChar;
  var cchMaxDomainname: Cardinal; pszPassword: PWideChar;
  var cchMaxPassword: Cardinal): LongBool; stdcall; external credui delayed;

procedure PromptCredentialsUI(ParentWindow: HWND;
  Callback: TCredentialsCallback; AllowNoPassword: Boolean = False);
var
  CredInfo: TCredUIInfoW;
  ErrorCode, AuthPackage: Cardinal;
  LastAuthError: Cardinal;
  AuthBuffer: Pointer;
  AuthBufferSize: Cardinal;
  UserBuffer, DomainBuffer, PasswordBuffer: PWideChar;
  UserLength, DomainLength, PasswordLength: Cardinal;
  Sid: ISid;
  AccountName: TTranslatedName;
begin
  LastAuthError := 0;
  while True do
  begin
    FillChar(CredInfo, SizeOf(CredInfo), 0);
    CredInfo.cbSize := SizeOf(CredInfo);
    CredInfo.hwndParent := ParentWindow;
    CredInfo.pszCaptionText := 'Logon a user';
    if AllowNoPassword then
      CredInfo.pszMessageText := 'Note: password is not required.'
    else
      CredInfo.pszMessageText := 'Please enter credentials:';

    AuthPackage := 0;
    ErrorCode := CredUIPromptForWindowsCredentialsW(CredInfo, LastAuthError,
      AuthPackage, nil, 0, AuthBuffer, AuthBufferSize, nil, CREDUIWIN_GENERIC);

    if ErrorCode = ERROR_CANCELLED then
      Abort;

    if ErrorCode <> ERROR_SUCCESS then
      RaiseLastOSError(ErrorCode);

    UserBuffer := nil;
    DomainBuffer := nil;
    PasswordBuffer := nil;

    try
      UserLength := 0;
      DomainLength := 0;
      PasswordLength := 0;

      CredUnPackAuthenticationBufferW(0, AuthBuffer, AuthBufferSize, nil,
        UserLength, nil, DomainLength, nil, PasswordLength);

      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        RaiseLastOSError;

      UserBuffer := AllocMem(UserLength * SizeOf(Char));
      DomainBuffer := AllocMem(DomainLength * SizeOf(Char));
      PasswordBuffer := AllocMem(PasswordLength * SizeOf(Char));

      if not CredUnPackAuthenticationBufferW(0, AuthBuffer, AuthBufferSize,
        UserBuffer, UserLength, DomainBuffer, DomainLength, PasswordBuffer,
        PasswordLength) then
        RaiseLastOSError;

      try
        LsaxLookupNameOrSddl(UserBuffer, Sid).RaiseOnError;
        LsaxLookupSid(Sid.Data, AccountName).RaiseOnError;

        if Assigned(Callback) then
          Callback(AccountName.DomainName, AccountName.UserName, PasswordBuffer);

        Exit;
      except
        on E: ENtError do
          LastAuthError := ENtError(E).NtxStatus.Win32Error;
        on E: EOSError do
          LastAuthError := E.ErrorCode
      end;

    finally
      FillChar(AuthBuffer^, AuthBufferSize, 0);
      FreeMem(PasswordBuffer);
      FreeMem(DomainBuffer);
      FreeMem(UserBuffer);
      CoTaskMemFree(AuthBuffer);
    end;
  end;
end;

end.

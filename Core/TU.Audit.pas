unit TU.Audit;

interface

type
  TAuditEntitiy = record
    Value: TGuid;
    Name: String;
  end;

  TAuditCategories = record
    Categories: array of TAuditEntitiy;
    SubCategories: array of array of TAuditEntitiy;
  end;

/// <summary>
///  Enumerates all auditing categories and their subcategories.
/// </summary>
/// <returns>
///  <see cref="ERROR_SUCCESS"/>, or any other error occured in the process.
/// </returns>
function EnumerateAuditCategiries(out Items: TAuditCategories): Cardinal;

implementation

uses
  Winapi.WinBase, Winapi.WinError, Winapi.NtSecApi, System.SysUtils;

function EnumerateAuditCategiries(out Items: TAuditCategories): Cardinal;
var
  Guids, SubGuids: PGuidArray;
  Count, SubCount: Cardinal;
  Ind, SubInd: Integer;
  Buffer: PWideChar;
begin
  Result := ERROR_SUCCESS;
  SetLength(Items.Categories, 0);
  SetLength(Items.SubCategories, 0, 0);

  // Query categories
  if not AuditEnumerateCategories(Guids, Count) then
    Exit(GetLastError);

  SetLength(Items.Categories, Count);
  SetLength(Items.SubCategories, Count, 0);

  // Go through all categories
  for Ind := 0 to High(Items.Categories) do
  begin
    Items.Categories[Ind].Value := Guids[Ind];

    // Query category name
    if AuditLookupCategoryNameW(Guids[Ind], Buffer) then
    begin
       Items.Categories[Ind].Name := String(Buffer);
       AuditFree(Buffer);
    end
    else
      Items.Categories[Ind].Name := GUIDToString(Guids[Ind]);

    // Query subcategories of this category
    if not AuditEnumerateSubCategories(Guids[Ind], False, SubGuids, SubCount)
      then
      Exit(GetLastError);

    SetLength(Items.SubCategories[Ind], SubCount);

    // Go through all subcategories
    for SubInd := 0 to High(Items.SubCategories[Ind]) do
    begin
      Items.SubCategories[Ind, SubInd].Value := SubGuids[SubInd];

      // Query subcategory name
      if AuditLookupSubCategoryNameW(SubGuids[SubInd], Buffer) then
      begin
        Items.SubCategories[Ind, SubInd].Name := String(Buffer);
        AuditFree(Buffer);
      end
      else
        Items.SubCategories[Ind, SubInd].Name := GUIDToString(SubGuids[SubInd]);
    end;

    AuditFree(SubGuids);
  end;

  AuditFree(Guids);
end;

end.

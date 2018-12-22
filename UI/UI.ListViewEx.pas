unit UI.ListViewEx;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls,
  System.UITypes, Winapi.Messages, Winapi.Windows, Winapi.CommCtrl;

type
  TListItemsEx = class;
  TListItemEx = class(TListItem)
  private
    FColor: TColor;
    FColorEnabled: Boolean;
    FHint: String;
    FOwnedData: TObject;
    procedure SetColor(const Value: TColor);
    procedure SetColorEnabled(const Value: Boolean);
    function GetOwnerItems: TListItemsEx;
  public
    constructor Create(AOwner: TListItems); override;
    destructor Destroy; override;
    function ToString: String; override;
    property Color: TColor read FColor write SetColor;
    property ColorEnabled: Boolean read FColorEnabled write SetColorEnabled;
    property Hint: String read FHint write FHint;
    property OwnedData: TObject read FOwnedData write FOwnedData;
    property Owner: TListItemsEx read GetOwnerItems;
    function Matches(SearchPattern: String; OnlyColumn: Integer = -1): Boolean;
  end;

  TListViewEx = class;
  TListItemsEx = class(TListItems)
  private
    FSelectionSnapshot: array of Boolean;
    function GetOwnerListView: TListViewEx;
  protected
    function GetItem(Index: Integer): TListItemEx;
    procedure SetItem(Index: Integer; Value: TListItemEx);
    procedure CreateSelectionSnapshot;
    function ApplySelectionSnapshot: Boolean;
    function InheritedAddItem(Item: TListItemEx; Index: Integer = -1): TListItemEx;
  public
    function Add: TListItemEx;
    function AddItem(Item: TListItemEx; Index: Integer = -1): TListItemEx;
    function Insert(Index: Integer): TListItemEx;
    property Item[Index: Integer]: TListItemEx read GetItem write SetItem; default;
    property Owner: TListViewEx read GetOwnerListView;
    procedure BeginUpdate(MakeSelectionSnapshot: Boolean = False);
    procedure EndUpdate(ApplySnapshot: Boolean = False);
  end;

  TListViewEx = class(TListView)
  private
    FColoringItems: Boolean;
    FPopupOnItemsOnly: Boolean;
    FClipboardColumn: Integer;
    FOnEditingEnd: TNotifyEvent;
    function GetItems: TListItemsEx;
    procedure SetItems(const Value: TListItemsEx);
    procedure SetItemsColoring(const Value: Boolean);
    function GetSelected: TListItemEx;
    procedure SetSelected(const Value: TListItemEx);
    procedure CNNotify(var Message: TWMNotifyLV); message CN_NOTIFY;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure SetSelectedCheckboxesState(State: Boolean);
    procedure ShowItemsHint(Sender: TObject; Item: TListItem; var InfoTip: string);
    function ItemToStringEx(Item: TListItemEx; AllColumns: Boolean): String;
  protected
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function CreateListItem: TListItem; override;
    function CreateListItems: TListItems; override;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage):
      Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Items: TListItemsEx read GetItems write SetItems;
    procedure Clear; override;
    property Selected: TListItemEx read GetSelected write SetSelected;
    procedure CopySelectedToClipboard(AllColumns: Boolean); virtual;
  published
    property ClipboardSourceColumn: Integer read FClipboardColumn write FClipboardColumn default -1;
    property ColoringItems: Boolean read FColoringItems write SetItemsColoring default False;
    property PopupOnItemsOnly: Boolean read FPopupOnItemsOnly write FPopupOnItemsOnly default False;

    /// <summary>
    ///  Each <see cref="OnEditing"/> event have a corresponding OnEditingEnd
    ///  event.
    /// </summary>
    /// <remarks>
    ///  Do not confuse this event with <see cref="OnEdited"/> that is called
    ///  only if the caption has actually changed.
    /// </remarks>
    property OnEditingEnd: TNotifyEvent read FOnEditingEnd write FOnEditingEnd;
  end;

procedure Register;

implementation

uses
  Vcl.Graphics, Vcl.Clipbrd, Vcl.Forms;

procedure Register;
begin
  RegisterComponents('Token Universe', [TListViewEx]);
end;

{ TListViewEx }

procedure TListViewEx.Clear;
begin
  // FIX: Clear doesn't deselect items before deleting them
  // so we don't get OnSelectItem event.
  ClearSelection;
  inherited;
end;

procedure TListViewEx.CMHintShow(var Message: TCMHintShow);
begin
  if not Assigned(OnInfoTip) then
  begin
    OnInfoTip := ShowItemsHint;
    inherited;
    OnInfoTip := nil;
  end
  else
    inherited;
end;

procedure TListViewEx.CNNotify(var Message: TWMNotifyLV);
begin
  inherited;

  // Since there is no event that states that editing is over we introduce one.
  // Note: do not confuse it with OnEdited that is called only if the caption
  // has actually changed.
  case Message.NMHdr.code of
    LVN_ENDLABELEDITA, LVN_ENDLABELEDITW:
      if Assigned(FOnEditingEnd) then
        FOnEditingEnd(Self);
  end;
end;

procedure TListViewEx.CopySelectedToClipboard(AllColumns: Boolean);
var
  i, j: integer;
  Texts: array of String;
  Text: String;
begin
  if SelCount = 0 then
    Exit;

  if MultiSelect and (SelCount > 1) then
  begin
    SetLength(Texts, SelCount);
    j := 0;
    for i := 0 to Items.Count - 1 do
    if Items[i].Selected then
      begin
        Texts[j] := ItemToStringEx(Items[i], AllColumns);
        Inc(j);
      end;
    Text := String.Join(#$D#$A, Texts);
  end
  else if Assigned(Selected) then
  begin
    Text := ItemToStringEx(Selected, AllColumns);
    UniqueString(Text);
  end;

  Clipboard.SetTextBuf(PWideChar(Text));
end;

constructor TListViewEx.Create(AOwner: TComponent);
begin
  inherited;
  FClipboardColumn := -1;
end;

function TListViewEx.CreateListItem: TListItem;
var
  LClass: TListItemClass;
begin
  LClass := TListItemEx;
  if Assigned(OnCreateItemClass) then
    OnCreateItemClass(Self, LClass);
  Result := LClass.Create(Items);
end;

function TListViewEx.CreateListItems: TListItems;
begin
  Result := TListItemsEx.Create(Self);
end;

function TListViewEx.CustomDrawItem(Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage): Boolean;
begin
  if FColoringItems then
  begin
    if (Item as TListItemEx).FColorEnabled then
      Canvas.Brush.Color := (Item as TListItemEx).FColor
    else
      Canvas.Brush.Color := Color;
  end;
  Result := inherited;
end;

procedure TListViewEx.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := FPopupOnItemsOnly and (SelCount = 0);
  if not Handled then
    inherited;
end;

function TListViewEx.GetItems: TListItemsEx;
begin
  Result := inherited Items as TListItemsEx;
end;

function TListViewEx.GetSelected: TListItemEx;
begin
  Result := inherited Selected as TListItemEx;
end;

function TListViewEx.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  if Target = dtItem then
    Result := FColoringItems or inherited
  else
    Result := inherited;
end;

function TListViewEx.ItemToStringEx(Item: TListItemEx; AllColumns: Boolean):
  String;
begin
  if AllColumns or (FClipboardColumn < 0) then
    Result := Item.ToString
  else if FClipboardColumn = 0 then
    Result := Item.Caption
  else if Item.SubItems.Count >= FClipboardColumn then
    Result := Item.SubItems[FClipboardColumn - 1]
  else
    Result := '';
end;

procedure TListViewEx.SetItems(const Value: TListItemsEx);
begin
  inherited Items := Value;
end;

procedure TListViewEx.SetItemsColoring(const Value: Boolean);
begin
  FColoringItems := Value;
  if FColoringItems then
    Repaint;
end;

procedure TListViewEx.SetSelected(const Value: TListItemEx);
begin
  inherited Selected := Value;
end;

procedure TListViewEx.SetSelectedCheckboxesState(State: Boolean);
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if Items[i].Selected then
      Items[i].Checked := State;
end;

procedure TListViewEx.ShowItemsHint(Sender: TObject; Item: TListItem;
  var InfoTip: string);
begin
  InfoTip := (Item as TListItemEx).Hint;
end;

procedure TListViewEx.WMKeyDown(var Message: TWMKeyDown);
var
  State: TShiftState;
begin
  State := KeyDataToShiftState(Message.KeyData);

  // <Ctrl + A> to select everyting
  if MultiSelect and (State = [ssCtrl]) and (Message.CharCode = Ord('A')) then
    SelectAll

  // <Ctrl + C> to copy only the main column
  else if (State = [ssCtrl]) and (Message.CharCode = Ord('C')) then
    CopySelectedToClipboard(False)

  // <Ctrl + Shift + C> to copy everything
  else if (State = [ssCtrl, ssShift]) and (Message.CharCode = Ord('C')) then
    CopySelectedToClipboard(True)

  // <Space> to check multiple items
  else if Checkboxes and MultiSelect and Assigned(Selected) and
      (State = []) and (Message.CharCode = VK_SPACE) then
  begin
    SetSelectedCheckboxesState(not Selected.Checked);
    Exit; // do not call inherited;
  end;

  inherited;
end;

{ TListItemsEx }

function TListItemsEx.Add: TListItemEx;
begin
  Result := AddItem(nil, -1);
end;

function TListItemsEx.AddItem(Item: TListItemEx; Index: Integer): TListItemEx;
var
  FixCaption: String;
begin
  Result := InheritedAddItem(Item, Index);

  // FIX: For some reason it draws empty caption until it wouldn't be updated
  if Item <> nil then
  begin
    FixCaption := Result.Caption;
    Result.Caption := '';
    Result.Caption := FixCaption;
  end;
end;

function TListItemsEx.ApplySelectionSnapshot: Boolean;
var
  i: Integer;
begin
  Result := Length(FSelectionSnapshot) = Count;
  if not Result then
    Exit;

  BeginUpdate;
  for i := 0 to High(FSelectionSnapshot) do
    Item[i].Selected := FSelectionSnapshot[i];
  EndUpdate;
end;

procedure TListItemsEx.BeginUpdate(MakeSelectionSnapshot: Boolean);
begin
  if MakeSelectionSnapshot then
    CreateSelectionSnapshot;
  (Self as TListItems).BeginUpdate;
end;

procedure TListItemsEx.CreateSelectionSnapshot;
var
  i: integer;
begin
  SetLength(FSelectionSnapshot, Count);
  for i := 0 to High(FSelectionSnapshot) do
    FSelectionSnapshot[i] := Item[i].Selected;
end;

procedure TListItemsEx.EndUpdate(ApplySnapshot: Boolean);
begin
  if ApplySnapshot then
  begin
    ApplySelectionSnapshot;
    SetLength(FSelectionSnapshot, 0);
  end;
  (Self as TListItems).EndUpdate;
end;

function TListItemsEx.GetItem(Index: Integer): TListItemEx;
begin
  Result := inherited GetItem(Index) as TListItemEx;
end;

function TListItemsEx.GetOwnerListView: TListViewEx;
begin
  Result := inherited Owner as TListViewEx;
end;

function TListItemsEx.InheritedAddItem(Item: TListItemEx;
  Index: Integer): TListItemEx;
begin
  Result := (Self as TListItems).AddItem(Item, Index) as TListItemEx;
end;

function TListItemsEx.Insert(Index: Integer): TListItemEx;
begin
  Result := AddItem(nil, Index);
end;

procedure TListItemsEx.SetItem(Index: Integer; Value: TListItemEx);
begin
  inherited SetItem(Index, Value);
end;

{ TListItemEx }

constructor TListItemEx.Create(AOwner: TListItems);
begin
  inherited;
  FColor := clWindow;
end;

destructor TListItemEx.Destroy;
begin
  OwnedData.Free;
  inherited;
end;

function TListItemEx.GetOwnerItems: TListItemsEx;
begin
  Result := inherited Owner as TListItemsEx;
end;

function TListItemEx.Matches(SearchPattern: String;
  OnlyColumn: Integer): Boolean;
var
  sub: Integer;
begin
  if SearchPattern = '' then
    Exit(True);

  if OnlyColumn < 0 then
  begin
    if Self.Caption.ToLower.Contains(SearchPattern) then
      Exit(True);

    for sub := 0 to Self.SubItems.Count - 1 do
      if Self.SubItems[sub].ToLower.Contains(SearchPattern) then
        Exit(True);

    if (Hint <> '') and Hint.ToLower.Contains(SearchPattern) then
      Exit(True);

    Result := False;
  end
  else if OnlyColumn = 0 then
    Result := Self.Caption.ToLower.Contains(SearchPattern)
  else if OnlyColumn <= Self.SubItems.Count then
    Result := Self.SubItems[OnlyColumn - 1].ToLower.Contains(SearchPattern)
  else
    Result := False;
end;

procedure TListItemEx.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColorEnabled := True;
    FColor := Value;
    if Owner.Owner.ColoringItems then
      Owner.Owner.Repaint;
  end;
end;

procedure TListItemEx.SetColorEnabled(const Value: Boolean);
begin
  if FColorEnabled <> Value then
  begin
    FColorEnabled := Value;
    if Owner.Owner.ColoringItems then
      Owner.Owner.Repaint;
  end;
end;

function TListItemEx.ToString: String;
begin
  if SubItems.Count = 0 then
    Result := Caption
  else
    Result := AnsiQuotedStr(Caption, '"') + ',' + SubItems.CommaText;
end;

end.

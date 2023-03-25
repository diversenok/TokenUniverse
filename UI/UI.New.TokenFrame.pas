unit UI.New.TokenFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  VirtualTreesEx, DevirtualizedTree, DevirtualizedTree.Provider,
  NtUtils, TU.Tokens;

type
  ITokenNode = interface (INodeProvider)
    ['{756BC6F1-77F9-4BDA-BEB8-0789E418278D}']
    function GetToken: IToken;
    procedure UpdateColumnVisibiliy(Column: TTokenStringClass; Visible: Boolean);
    property Token: IToken read GetToken;
  end;

  TTokenNode = class (TNodeProvider, ITokenNode)
    FToken: IToken;
    FSubscriptions: array [TTokenStringClass] of IAutoReleasable;
    procedure UpdateColumnVisibiliy(Column: TTokenStringClass; Visible: Boolean);
    procedure ColumnUpdated(const Column: TTokenStringClass; const NewValue: String);
    function GetToken: IToken;
    function GetColumnText(Index: Integer): String; override;
    constructor Create(const Source: IToken; Columns: TVirtualTreeColumns);
  end;

  TFrameTokens = class(TFrame)
    VST: TDevirtualizedTree;
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VSTColumnVisibilityChanged(const Sender: TBaseVirtualTree;
      const Column: TColumnIndex; Visible: Boolean);
  private
    function GetSelectedToken: IToken;
    function GetAllTokens: TArray<IToken>;
    function GetHasSelectedToken: Boolean;
  public
    function AddRoot(const Caption: String; Root: PVirtualNode = nil): PVirtualNode;
    procedure AddMany(const Tokens: TArray<IToken>; Root: PVirtualNode = nil);
    function Add(
      const Token: IToken;
      Root: PVirtualNode = nil;
      CaptureFocus: Boolean = True
    ): PVirtualNode;
    procedure DeleteSelected;
    property HasSelectedToken: Boolean read GetHasSelectedToken;
    property Selected: IToken read GetSelectedToken;
    property Tokens: TArray<IToken> read GetAllTokens;
    constructor Create(Owner: TComponent); override;
  end;

implementation

uses
  UI.Helper, UI.Settings, VirtualTrees.Header, VirtualTrees.Types,
  DelphiUtils.Arrays, NtUtils.Errors;

{$R *.dfm}

{ TTokenNode }

procedure TTokenNode.ColumnUpdated;
begin
  Self.Invalidate;
end;

constructor TTokenNode.Create;
var
  ColumnId: TColumnIndex;
begin
  inherited Create(0);
  FToken := Source;

  // Subscribe to changes for all currently visible columns
  ColumnId := Columns.GetFirstVisibleColumn;

  while ColumnId <> InvalidColumn do
  begin
    UpdateColumnVisibiliy(TTokenStringClass(ColumnId), True);
    ColumnId := Columns.GetNextVisibleColumn(ColumnId);
  end;
end;

function TTokenNode.GetColumnText;
var
  InfoClass: TTokenStringClass absolute Index;
begin
  if (InfoClass < Low(TTokenStringClass)) or
    (InfoClass > High(TTokenStringClass)) then
    raise EArgumentException.Create('Invalid index in TTokenNode.GetColumn');

  Result := FToken.QueryString(InfoClass);
end;

function TTokenNode.GetToken;
begin
  Result := FToken;
end;

procedure TTokenNode.UpdateColumnVisibiliy;
begin
  if (Column < Low(TTokenStringClass)) or
    (Column > High(TTokenStringClass)) then
    raise EArgumentException.Create('Invalid index in TTokenNode.UpdateColumnVisibiliy');

  // Either subscribe for updates or clear the existing subscription
  if Visible then
    FSubscriptions[Column] := FToken.ObserveString(Column,
      ColumnUpdated)
  else
    FSubscriptions[Column] := nil;
end;

{ TFrameTokens }

function TFrameTokens.Add;
begin
  if not Assigned(Root) then
    Root := VST.RootNode;

  VST.BeginUpdateAuto;
  Result := VST.AddChild(Root);
  Result.Provider := TTokenNode.Create(Token, VST.Header.Columns);

  if CaptureFocus then
  begin
    VST.ClearSelection;
    VST.FocusedNode := Result;
    VST.Selected[Result] := True;
  end;
end;

procedure TFrameTokens.AddMany;
var
  Token: IToken;
begin
  VST.BeginUpdateAuto;

  for Token in Tokens do
    Add(Token, Root, False);

  VST.SelectSometing;
end;

function TFrameTokens.AddRoot;
var
  Provider: IEditableNodeProvider;
begin
  if not Assigned(Root) then
    Root := VST.RootNode;

  VST.BeginUpdateAuto;

  VST.TreeOptions.PaintOptions := VST.TreeOptions.PaintOptions +
    [TVTPaintOption.toShowRoot];

  Provider := TEditableNodeProvider.Create;
  Provider.ColumnText[0] := Caption;
  Provider.FontStyle := [TFontStyle.fsBold];

  Result := VST.AddChild(Root, Provider);
end;

constructor TFrameTokens.Create;
const
  DEFAULT_COLUMN_OPTIONS = [
    TVTColumnOption.coAllowClick,
    TVTColumnOption.coDraggable,
    TVTColumnOption.coEnabled,
    TVTColumnOption.coParentBidiMode,
    TVTColumnOption.coParentColor,
    TVTColumnOption.coResizable,
    TVTColumnOption.coAutoSpring,
    TVTColumnOption.coSmartResize,
    TVTColumnOption.coAllowFocus,
    TVTColumnOption.coDisableAnimatedResize,
    TVTColumnOption.coStyleColor
  ];
var
  InfoClass: TTokenStringClass;
begin
  inherited;

  VST.Header.Columns.BeginUpdateAuto;

  // Populate columns based on string info classes
  for InfoClass := Low(TTokenStringClass) to High(TTokenStringClass) do
    with VST.Header.Columns.Add do
    begin
      Text := ColumsInfo[InfoClass].Caption;
      Width := ColumsInfo[InfoClass].Width;
      Alignment := ColumsInfo[InfoClass].Alignment;
      Options := DEFAULT_COLUMN_OPTIONS;
      MinWidth := 30;

      // Only show selected columns by default
      if InfoClass in TSettings.SelectedColumns then
        Options := Options + [TVTColumnOption.coVisible];

      // The caption column is specal
      if InfoClass = tsCaption then
        Options := Options - [TVTColumnOption.coDraggable] +
          [TVTColumnOption.coEditable, TVTColumnOption.coFixed];
    end;
end;

procedure TFrameTokens.DeleteSelected;
begin
  VST.DeleteSelectedNodesEx;
end;

function TFrameTokens.GetAllTokens;
begin
  Result := TArray.Convert<PVirtualNode, IToken>(VST.Nodes.ToArray,
    function (const Node: PVirtualNode; out Token: IToken): Boolean
    var
      TokenNode: ITokenNode;
    begin
      Result := Node.TryGetProvider(ITokenNode, TokenNode);

      if Result then
        Token := TokenNode.Token;
    end
  );
end;

function TFrameTokens.GetHasSelectedToken;
begin
  Result := (VST.SelectedCount > 0) and
    VST.FocusedNode.HasProvider(ITokenNode);
end;

function TFrameTokens.GetSelectedToken;
var
  TokenNode: ITokenNode;
begin
  if (VST.SelectedCount > 0) and VST.FocusedNode.TryGetProvider(ITokenNode,
    TokenNode) then
    Result := TokenNode.Token
  else
    Abort;
end;

procedure TFrameTokens.VSTColumnVisibilityChanged;
var
  Node: PVirtualNode;
  TokenNode: ITokenNode;
begin
  if (Column = Integer(tsCaption)) and not Visible then
  begin
    // Prevent hiding the caption column
    VST.Header.Columns[Column].Options := VST.Header.Columns[Column].Options +
      [TVTColumnOption.coVisible];

    Exit;
  end;

  // Notify each node that it needs to [un]subscribe an info class
  for Node in VST.Nodes do
    if Node.TryGetProvider(ITokenNode, TokenNode) then
      TokenNode.UpdateColumnVisibiliy(TTokenStringClass(Column), Visible);
end;

procedure TFrameTokens.VSTEditing;
begin
  Allowed := (Column = Integer(tsCaption)) and HasSelectedToken;
end;

procedure TFrameTokens.VSTNewText;
var
  TokenNode: ITokenNode;
begin
  if (Column = Integer(tsCaption)) and
    VST.FocusedNode.TryGetProvider(ITokenNode, TokenNode) then
    TokenNode.Token.Caption := NewText;
end;

end.

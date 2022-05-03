unit UI.New.TokenFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  VirtualTreesEx, DevirtualizedTree, DevirtualizedTree.Provider,
  NtUtils, TU.Tokens, TU.Tokens3;

type
  ITokenNode = interface (INodeProvider)
    ['{6F211C2D-3D96-41B0-A14F-55C74939D37D}']
    function GetToken: IToken;
    property Token: IToken read GetToken;
  end;

  TTokenNode = class (TCustomNodeProvider, ITokenNode)
    FToken: IToken;
    function GetToken: IToken;
    function GetColumn(Index: Integer): String; override;
    constructor Create(const Source: IToken);
  end;

  TFrameTokens = class(TFrame)
    VST: TDevirtualizedTree;
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
  private
    function GetSelectedToken: IToken;
    function GetAllTokens: TArray<IToken>;
    function GetHasSelectedToken: Boolean;
    { Private declarations }
  public
    function AddRoot(Root: PVirtualNode; const Name: String): PVirtualNode;
    procedure AddMany(const Tokens: TArray<IToken>; Root: PVirtualNode = nil);
    procedure Add(const Token: IToken; Root: PVirtualNode = nil);
    procedure DeleteSelected;

    property HasSelectedToken: Boolean read GetHasSelectedToken;
    property Selected: IToken read GetSelectedToken;
    property Tokens: TArray<IToken> read GetAllTokens;

    constructor Create(Owner: TComponent); override;
  end;

implementation

uses
  UI.Helper, UI.Settings, VirtualTrees.Header,
  DelphiUtils.Arrays, NtUtils.Errors;

{$R *.dfm}

{ TTokenNode }

constructor TTokenNode.Create;
begin
  inherited Create(0);
  FToken := Source;
end;

function TTokenNode.GetColumn;
var
  InfoClass: TTokenStringClass absolute Index;
begin
  if (InfoClass < Low(TTokenStringClass)) or
    (InfoClass > High(TTokenStringClass)) then
    Exit('');

  Result := (FToken as IToken3).QueryString(InfoClass);
end;

function TTokenNode.GetToken;
begin
  Result := FToken;
end;

{ TFrameTokens }

procedure TFrameTokens.Add;
begin
  AddMany([Token], Root);
end;

procedure TFrameTokens.AddMany;
var
  i: Integer;
begin
  if not Assigned(Root) then
    Root := VST.RootNode;

  VST.BeginUpdateAuto;

  for i := 0 to High(Tokens) do
    VST.AddChild(Root).Provider := TTokenNode.Create(Tokens[i]);
end;

function TFrameTokens.AddRoot;
var
  Provider: INodeProvider;
begin
  Provider := TCustomNodeProvider.Create;
  Provider.Column[0] := Caption;
  Result := VST.AddChild(Root, Provider);
end;

constructor TFrameTokens.Create(Owner: TComponent);
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
    TVTColumnOption.coEditable,
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

      // Only show selected columns by default
      if InfoClass in TSettings.SelectedColumns then
        Options := Options + [TVTColumnOption.coVisible];
    end;
end;

procedure TFrameTokens.DeleteSelected;
var
  Node: PVirtualNode;
begin
  VST.BeginUpdateAuto;

  for Node in VST.SelectedNodes.ToArray do
    VST.DeleteNode(Node);
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

procedure TFrameTokens.VSTEditing;
begin
  Allowed := (Column = Integer(tsCaption));
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

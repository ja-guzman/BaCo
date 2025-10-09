unit SMap.About;


INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, shellAPI,System.UITypes,
  OLE2,
  XMLIntf,
  XMLDoc,
  IdHTTP;


TYPE
  TSpellInfo = class(TForm)
    Timer1: TTimer;
    Label10: TLabel;
    Wizard: TImage;
    Label2: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Timer2: TTimer;
    Memo1: TMemo;
    procedure WizardClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer2Timer(Sender: TObject);
  private
    ii      : shortInt;
    authors : Tstrings;
    procedure getVersion(URL: string);
  public
    banner  : boolean;
    function  getAppVersionStr: string;
    procedure spellUpdateCheck;
  end;


VAR
SPELLinfo: TSpellInfo;



IMPLEMENTATION


{$R *.dfm}



USES
SMap.Types;



procedure TSpellInfo.FormCreate(Sender: TObject);
begin
  ii:= 0;
  banner := FALSE;
  authors:= TstringList.Create;
  SpellInfo.Width  := 600;
  SpellInfo.Height := 480;
  SpellInfo.Left   := (Screen.Width - 600) div 2;
  SpellInfo.Top    := (Screen.Height- 308) div 2;
  SpellInfo.Label1.Caption := 'SPELLmap';
  SpellInfo.Label2.Caption:= GetAppVersionStr;
  authors.Add('Jorge A. GUZMAN');
  authors.Add('Daniel N. MORIASI');
  authors.Add('Maria L. CHU');
  authors.Add('Patrick J. STARKS');
  authors.Add('Jean L. STEINER');
  authors.Add('Prasanna H. GOWDA');
  SpellInfo.Visible:= FALSE;
  label10.Caption:= authors.strings[ii];
end;



procedure TSpellInfo.FormShow(Sender: TObject);
begin
  timer1.Enabled:= TRUE;
  if not banner then label10.Visible:= FALSE else label10.Visible:= TRUE;
end;



procedure TSpellInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  timer1.Enabled:= FALSE;
end;



procedure TSpellInfo.Timer1Timer(Sender: TObject);
begin
  if not banner then exit;
  if label10.Left > -100 then label10.Left:= label10.Left - 1
  else
  begin
    inc(ii);
    if ii < 6 then
    begin
      label10.Caption:= authors.strings[ii];
      label10.left:= 240;
    end else ii:= -1;
  end;
end;



procedure TSpellInfo.Timer2Timer(Sender: TObject);
begin
  timer2.Enabled:= FALSE;
  SpellInfo.ModalResult:= mrOK;
end;



procedure TSpellInfo.WizardClick(Sender: TObject);
begin
  timer1.Enabled:= FALSE;
  SpellInfo.ModalResult:= mrOK;
end;



function TSpellInfo.GetAppVersionStr: string;
var
exe        : string;
size,Handle: DWORD;
buffer     : TBytes;
fixedPtr   : PVSFixedFileInfo;
begin
  Exe:= ParamStr(0);
  Size:= getFileVersionInfoSize(PChar(Exe),Handle);
  if Size = 0 then raiseLastOSError;
  SetLength(Buffer,Size);
  if not GetFileVersionInfo(PChar(Exe),Handle,Size,Buffer) then raiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then raiseLastOSError;
  result:= Format('%d.%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi,  //release
     LongRec(FixedPtr.dwFileVersionLS).Lo]) //build
end;



// -----------------------------------------------------------------------------



procedure TSpellInfo.getVersion(URL: string);
var
stream : TMemoryStream;
http   : TIdHTTP;
doc    : iXMLDocument;

node   : iXMLnode;
vStr   : Tstrings;
mStr   : Tstrings;
bttn   : integer;

begin
  try
    vStr:= TstringList.Create;
    mStr:= TstringList.Create;

    http:= TIdHTTP.Create(self);
    stream:= TMemoryStream.Create;

    coInitialize(nil); // must use OLE2 unit
    doc:= TXMLDocument.Create(self);
    doc.Active:= True;
    doc.Encoding:= 'utf-8';
    doc.Options:= [doNodeAutoIndent]; // looks better in Editor ;)

    http.Request.ContentType:= 'text/xml; charset=utf-8';
    http.Request.ContentEncoding:= 'utf-8';
    http.HTTPOptions:= [hoForceEncodeParams];
    http.Request.CharSet:= 'utf-8';
    http.Request.UserAgent:= 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/49.0.1';
    try
      http.Get(url,stream);
    except
      on E : Exception do exit;
    end;

    doc.LoadFromStream(stream);
    with doc.ChildNodes.Nodes['SPELLmap'] do begin
      node:= ChildNodes.FindNode('version');
      extractStrings([#9,#32,#44],[#32],PChar(node.Text),vStr);

      if not (GetAppVersionStr = vStr[0]) then begin
        node:= ChildNodes.FindNode('msg');
        extractStrings([#13],[#32],PChar(node.Text),mStr);
        bttn:= messageDlg(mStr[0],mtInformation,[mbYes, mbNo],0,mbYes);
      end;
      if bttn = 6 then
      begin
        URL := 'http://jguzman.info/SPELLmap/download/download.html';
        ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
      end;
    end;

  finally
    try
      http.Disconnect;
    except
    end;
    mStr.Free;
    vStr.Free;
    stream.Free;
    http.Free;
  end;
end;



procedure TSpellInfo.spellUpdateCheck;
begin
//  getVersion('http://jguzman.info/SPELLmap/download/sm_version.xml');
end;



END.

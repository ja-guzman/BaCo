unit about;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm2 = class(TForm)
    Image1: TImage;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image2: TImage;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Image3: TImage;
    Label9: TLabel;
    procedure Rectangle1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    function GetAppVersionStr: string;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;



IMPLEMENTATION

{$R *.fmx}

uses
  FMX.Platform;


procedure TForm2.Image1Click(Sender: TObject);
begin
  Form2.Visible:= FALSE;
end;

procedure TForm2.Rectangle1Click(Sender: TObject);
begin
  Form2.Visible:= FALSE;
end;



procedure TForm2.FormActivate(Sender: TObject);
begin
  label9.Text:= GetAppVersionStr;
end;



function TForm2.GetAppVersionStr: string;
var
  AppService: IFMXApplicationService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(AppService)) then
    Result:= 'Version: ' + AppService.AppVersion
  else Result:= 'Version unAvailable';
end;


end.

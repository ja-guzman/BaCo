program BaCo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  main,
  metrics,
  core.reanalysis,
  core.mssg,
  about;

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

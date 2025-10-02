program GPFuncOrder;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  SynEditHelper in 'SynEditHelper.pas',
  Vcl.Themes,
  Vcl.Styles,
  HelperFunctions in 'HelperFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  { The default theme:          }
  { TODO: Load this externally? }
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

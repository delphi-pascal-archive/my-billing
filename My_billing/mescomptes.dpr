program mescomptes;

uses
  QForms,
  main in 'main.pas' {FrmMain},
  chslang in 'chslang.pas' {FrmChooseLang};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

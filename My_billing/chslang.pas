unit chslang;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls;

type
  TFrmChooseLang = class(TForm)
    GroupBox1: TGroupBox;
    RadFr: TRadioButton;
    Button1: TButton;
    RadEn: TRadioButton;
    RadEs: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmChooseLang: TFrmChooseLang;


implementation

{$R *.xfm}
uses main;

{*   FileCopy: Portable CopyFile function
 *
 * No error handling; Delphi finds errors and writes an error box, cancelling
 * the application, eg if the source file does not exist. Console applications
 * just halt.
 * Tested and works. Get c. 600-830 kB/sec over 10 Meg E'net. Average-750,000
 * Bytes/sec.
 *}

procedure FileCopy( Const SourceFn, TargetFn : String );
Var
  S, T: TFileStream;
  SourceHand, DestHand: Integer;
Begin
  S := TFileStream.Create( SourceFn, fmOpenRead );
  try
    T := TFileStream.Create( TargetFn, fmOpenWrite or fmCreate );
    try
      T.CopyFrom(S, S.Size ) ;
    finally
      T.Free;
    end;
  finally
    S.Free;
  end;
{Now set the new file's date/time to that of the source }
  SourceHand := FileOpen(SourceFn, fmOpenRead);  // open source file
  DestHand := FileOpen(TargetFn, fmOpenWrite);   // open dest file
  {$IfDef MSWINDOWS}
  FileSetDate(DestHand , FileGetDate(SourceHand));// get/set date
  {$Else}
  FileSetDate(TargetFn , FileGetDate(SourceHand));// get/set date
  {$EndIf}
  FileClose(SourceHand);                         // close source file
  FileClose(DestHand);                           // close dest file
end; { Procedure FileCopy }


procedure TFrmChooseLang.FormCreate(Sender: TObject);
var
  f : TfrmMain;
begin
  selfDir := ExtractFilePath(Application.ExeName);
  if FileExists(selfDir + '/lng.ini') then
  begin
    //f := TfrmMain.Create(self);
    //f.ShowModal;
    //Close;
  end;
end;

procedure TFrmChooseLang.Button1Click(Sender: TObject);
begin
  if RadFr.Checked and  FileExists(selfDir + '/lng_fr.ini') then
     FileCopy(selfDir + '/lng_fr.ini', selfDir + '/lng.ini');
  if RadEn.Checked and  FileExists(selfDir + '/lng_en.ini') then
     FileCopy(selfDir + '/lng_en.ini', selfDir + '/lng.ini');
  if RadEs.Checked and  FileExists(selfDir + '/lng_es.ini') then
     FileCopy(selfDir + '/lng_es.ini', selfDir + '/lng.ini');
  self.Close;
end;

end.

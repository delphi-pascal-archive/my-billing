unit main;

interface

uses
  SysUtils, Types, Classes, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QGrids, QMask, ravageidb, ravagedb, QComCtrls,dateutils,
  inifiles2;

type
  TFrmMain = class(TForm)
    StringGrid2: TStringGrid;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    LblCompte: TLabel;
    LblMontant: TLabel;
    txtMontant: TEdit;
    LblTousles: TLabel;
    seNbMois: TSpinEdit;
    LblMois: TLabel;
    ComboBox2: TComboBox;
    LblLibelle: TLabel;
    LblDate: TLabel;
    Mskdate: TMaskEdit;
    cmdAjout: TButton;
    cmdSuppr: TButton;
    GroupBox2: TGroupBox;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmdAjoutClick(Sender: TObject);
    procedure cmdSupprClick(Sender: TObject);
    procedure MajListe(Sender: TObject);
    procedure StringGrid2DblClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    function Translation(msg: string; Sender: TObject):string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;
  db,dbplan : TRavageidb;
//  rdb: TRavageDB;
  selfDir : string;

implementation

uses chslang;

{$R *.xfm}

function TFrmMain.Translation(msg: string; Sender: TObject):string;
var
  ini : Tinifile2;
begin
  ini := Tinifile2.Create(selfDir + '/lng.ini');
  result := ini.ReadString('LNG',msg, msg);
  ini.Free;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  y,m,d,jp,nbm,dm:word;
  index,i: LongWord;
  tmp:string;
  dec : Double;
  ini : Tinifile2;
  f : TfrmChooseLang;
begin
  selfDir := ExtractFilePath(Application.ExeName);
  if not FileExists(selfDir + '/lng.ini') then
  begin
    f:= TfrmChooseLang.Create(self);
    f.Showmodal;
  end;
  ini := Tinifile2.Create(selfDir + '/lng.ini');
  for i:=0 to ComponentCount-1 do
  begin
    if components[i] is Tbutton then
      Tbutton(Components[i]).Caption := ini.ReadString('LNG',Tbutton(Components[i]).Caption, Tbutton(Components[i]).Caption);
    if components[i] is TLabel then
      TLabel(Components[i]).Caption := ini.ReadString('LNG',TLabel(Components[i]).Caption, TLabel(Components[i]).Caption);
    if components[i] is TComboBox then
    begin
      for index := 0 to TComboBox(Components[i]).Items.Count-1 do
        TComboBox(Components[i]).Items[index] := ini.ReadString('LNG',TComboBox(Components[i]).items[index], TComboBox(Components[i]).items[index]);
    end;


  end;
  ini.Free;

  Combobox1.Text := ComboBox1.Items[0];
  StringGrid1.Cells[0, 0]:= '0';
  StringGrid1.ColWidths[0] := 0;
  StringGrid1.Cells[1, 0]:= LblDate.Caption;
  StringGrid1.ColWidths[1] := 80;
  StringGrid1.Cells[2, 0]:= LblLibelle.Caption;
  StringGrid1.ColWidths[2] := 250;
  StringGrid1.Cells[3, 0]:= 'Débit';
  StringGrid1.ColWidths[3] := 80;
  StringGrid1.Cells[4, 0]:= 'Crédit';
  StringGrid1.ColWidths[4] := 80;
  StringGrid1.Cells[5, 0]:= 'Solde';
  StringGrid1.ColWidths[5] := 80;

  StringGrid2.Cells[0, 0] := '0';
  StringGrid2.ColWidths[0] := 0;
  StringGrid2.Cells[1, 0] := 'J/M';
  StringGrid2.ColWidths[1] := 40;
  StringGrid2.Cells[2, 0] := 'NB';
  StringGrid2.ColWidths[2] := 40;
  StringGrid2.Cells[3, 0] := LblLibelle.Caption;
  StringGrid2.ColWidths[3] := 210;
  StringGrid2.Cells[4, 0] := LblMontant.Caption;
  StringGrid2.ColWidths[4] := 80;

  MskDate.Text:=datetostr(date);
  db := TRavageiDB.create;
  If Not FileExists('courant.cpt') then    //120 + 3 152
    db.init('courant.cpt', 'j,m,aa,lib,type,debit,credit,solde',
                 '1,1,2,60,1,4,4,4',
                 'I,I,I,s,I,I2,I2,i2'   );
  db.load('courant.cpt');

  dbplan :=  TRavageiDB.create;
  If Not FileExists('courantp.cpt') then    //120 + 3 152
    dbplan.init('courantp.cpt', 'j,m,nbm,dm,lib,montant',
                 '1,1,1,1,68,4',
                 'I,I,I,I,s,i2'   );
  dbplan.load('courantp.cpt');
  MajListe(Sender);
  DecodeDate(date,y,m,d);
  for index := 1 to dbplan.MaxEntries do
  begin
    tmp := dbplan.get(index,'dm');
    if tmp <> '' then
    begin
      tmp := dbplan.get(index,'j');
      jp := strtoint(tmp);
      tmp := dbplan.get(index,'nbm');
      nbm := strtoint(tmp);
      tmp := dbplan.get(index,'dm');
      dm := strtoint(tmp);
      tmp := dbplan.get(index,'montant');
      dec := StrToFloat(tmp);
      if jp < d then
        if ((dm+nbm > 12) and ((m=12) or (dm + nbm - 12 <= m))) or
           ((dm+nbm <= 12) and (dm + nbm <= m)) then
          if InputQuery('Ajout d''ecriture planifiee', dbplan.get(index,'lib'), dec)
          then
          begin
            txtMontant.Text := FloatToStr(dec);
            ComboBox2.Text := dbplan.get(index,'lib');
            CmdAjoutClick(Sender);
            txtMontant.Text := '';
            ComboBox2.Text := '';
            dbplan.update(index,'dm',IntToStr(m))
          end;
    end;
  end; 
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  db := TRavageiDB.create;
  If Not FileExists('courant.cpt') then    //120 + 3 152
    db.init('courant.cpt', 'j,m,aa,lib,type,debit,credit,solde',
                 '1,1,2,68,1,4,4,4',
                 'I,I,I,s,I,I2,I2,i2'   );
  db.load('courant.cpt');
  MajListe(Sender);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  db.close;
  dbplan.close;
end;

procedure TFrmMain.cmdAjoutClick(Sender: TObject);
var
  erreur:integer;
  nb,solde: Extended;
  id0: LongWord;
  tmp,tmp2: String;
  date1 : TDateTime;
  y,m,d:Word;
begin
  val(txtMontant.Text,nb,erreur);
  if erreur <> 0 then
  begin
    ShowMessage(Translation('Valeur numérique requise',Sender));
    exit;
  end;

  cmdAjout.Enabled := False;
  if seNbMois.Value = 0 then
  begin
    tmp2 := StringGrid1.Cells[0,StringGrid1.rowcount-1];
    if (tmp2 = '') Or (tmp2 = '0') then solde := 0 else
    begin
      tmp := db.get(strtoint(StringGrid1.Cells[0,StringGrid1.rowcount-1]),'solde');
      solde := StrToFloat(tmp);
    end;
    id0 := db.add;
    date1 := strtodate(Mskdate.Text) ;
    DecodeDate(date1,y,m,d);
    db.update(id0,'j',IntToStr(d));
    db.update(id0,'m',IntToStr(m));
    db.update(id0,'aa',IntToStr(y));
    db.update(id0,'lib',ComboBox2.text);
    if (nb < 0) and (erreur = 0) then
    begin
      db.update(id0,'debit',Copy(txtMontant.Text,2,100));
      db.update(id0,'credit','0');
    end;
    if (nb > 0) and (erreur = 0) then
    begin
      db.update(id0,'debit','0');
      db.update(id0,'credit',txtMontant.Text);
    end;
    db.update(id0,'solde',FloatToStr(solde+nb));
  end
  else
  begin
    id0 := dbplan.add;
    date1 := strtodate(Mskdate.Text) ;
    DecodeDate(date1,y,m,d);
    dbplan.update(id0,'j',IntToStr(d));
    dbplan.update(id0,'m',IntToStr(m));
    dbplan.update(id0,'nbm',IntToStr(seNbMois.Value));
    dbplan.update(id0,'dm','0');
    dbplan.update(id0,'lib',ComboBox2.text);
    dbplan.update(id0,'montant',txtMontant.Text);
  end;
  MajListe(sender);
  cmdAjout.Enabled := True;
end;

procedure TFrmMain.cmdSupprClick(Sender: TObject);
var
  i: longword;
begin
  i := strtoint(StringGrid1.Cells[0,StringGrid1.rowcount-1]);
  if i > 0 then
  begin
  cmdSuppr.Enabled := False;
  db.remove(i);
  MajListe(Sender);
  cmdSuppr.Enabled := True;
  end;
end;

procedure TFrmMain.MajListe(Sender: TObject);
var
  id0,index: LongWord;
  tmp : string;
  index2 : integer;
  d,m,y : Word;
  date1 : TDateTime;
begin
  if not db.IsLoaded then exit;
  //if db.MaxEntries = 0 then exit;

  StringGrid1.rowcount := 1;
  if db.MaxEntries <= 50 then id0 := 1
  else id0 := db.MaxEntries - 50;
  index2 := 1;
  //showMessage(inttostr(id0));
  //id0 := db.MaxEntries;
  for index := id0 to db.MaxEntries do
  begin
    tmp := db.get(index,'j');
    if tmp <> '' then
    begin
      StringGrid1.Cells[0,index2] := inttostr(index);
      d := StrToInt(db.get(index,'j'));
      m := StrToInt(db.get(index,'m'));
      y := StrToInt(db.get(index,'aa'));
      // un peut de maintenance (cas de plantage ...)
      if (d<1) or (d>31) or (m<1) or (m>12) or (y<2006) or (y>9999) then
      begin
        db.remove(index);
        continue;
      end;
      date1 := EncodeDate(y,m,d);
      StringGrid1.Cells[1,index2] := DateTimeToStr(date1);
      StringGrid1.Cells[2,index2] := db.get(index,'lib');

      if db.get(index,'debit')  <> '0' then
        StringGrid1.Cells[3,index2] := '-' + db.get(index,'debit')
      else
        StringGrid1.Cells[3,index2] := '';
      if db.get(index,'credit')  <> '0' then
        StringGrid1.Cells[4,index2] := db.get(index,'credit')
      else
        StringGrid1.Cells[4,index2] := '';
      StringGrid1.Cells[5,index2] := db.get(index,'solde');
      inc(index2);
    end;
  end;
  StringGrid1.rowcount := index2;
  if StringGrid1.rowcount > 1 then
    StringGrid1.FixedRows := 1;
  StringGrid1.row := StringGrid1.rowcount-1;

  if not dbplan.IsLoaded then exit;
  if dbplan.MaxEntries = 0 then exit;

  StringGrid2.rowcount := 1;
  if dbplan.MaxEntries <= 50 then id0 := 1
  else id0 := dbplan.MaxEntries - 50;
  index2 := 1;
  for index := id0 to dbplan.MaxEntries do
  begin
    tmp := dbplan.get(index,'j');
    if tmp <> '' then
    begin
      StringGrid2.Cells[0,index2] := inttostr(index);
      StringGrid2.Cells[1,index2] := dbplan.get(index,'j') + '/' + dbplan.get(index,'m');
      StringGrid2.Cells[2,index2] := dbplan.get(index,'nbm');
      StringGrid2.Cells[3,index2] := dbplan.get(index,'lib');
      StringGrid2.Cells[4,index2] := dbplan.get(index,'montant');
      inc(index2);
    end;
  end;

  StringGrid2.rowcount := index2;
  if StringGrid2.rowcount > 1 then
    StringGrid2.FixedRows := 1;


end;

procedure TFrmMain.StringGrid2DblClick(Sender: TObject);
var
  tmp : Double;
  id : integer;
begin

    id := StrToInt(StringGrid2.Cells[0,StringGrid2.row]);
    tmp := StrToFloat(dbplan.get(id ,'montant'));
    if InputQuery(Translation('Modification d''ecriture planifiee (0 pour effacer)',sender), dbplan.get(id,'lib'), tmp)
    then
    begin
      if tmp = 0 Then
        dbplan.remove(id)
      else
        dbplan.update(id,'montant',FloatToStr(tmp));
    end;
    MajListe(Sender);

end;

procedure TFrmMain.StringGrid1DblClick(Sender: TObject);
var
  tmp : string;
  id : integer;
begin

    id := StrToInt(StringGrid1.Cells[0,StringGrid1.row]);
    tmp := db.get(id ,'lib');
    if InputQuery(Translation('Modification d''ecriture',sender), db.get(id,'lib'), tmp)
    then
    begin
        db.update(id,'lib',tmp);
    end;
    MajListe(Sender);

end;

end.

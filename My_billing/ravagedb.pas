{******************************************************************************
 * ravagedb.pas v0.2 for Kylix3 and Delphi7
 * Copyright (c) 12th july 2007 by alvaro Hermo <alvaro.h@ifrance.com>
 *
 * ravagedb database is under the terms of the GNU General Public License
 * please see licence.txt
 *
 * depend of ravagelib.pas
 * 
 *
 * exemple:
 *
 * procedure TForm1.FormCreate(Sender: TObject);
 * Var
 *   db : TRavageDB;       // ravage database
 *   good : Integer;
 * begin
 *   db := TRavageDB.create;
 *   if Not FileExists('database.dat') then
 *     db.init('database.dat');
 *   db.load('database.dat', 'driver,car,track,version,conditions,date',
 *                  '19,13,4,19,1,6',
 *                  's,s,s,s,I,i2'   );
 * 
 *   // to add an element
 *   db.add;
 *   db.update('driver','gaston');
 *   db.update('car','mac laren');
 * 
 *   // to find an element
 *   db.add_finditem('driver','gaston','=');
 *   db.add_finditem('car','mac laren*','like');
 *
 *   good := db.findfirst();
 *   while good <> 0 do
 *   begin
 *     Lst3Find.Items.Add(db.get('driver') + ',' + db.get('car'));
 *     good := db.findnext();
 *   end;
 *
 *   db.close;
 * end;
 *
 * NB: The Source Code is commented in french
 *****************************************************************************}

unit ravagedb;

interface

uses
   SysUtils, StrUtils,
   QDialogs,QControls,   // pour application CLX
   //Dialogs,Controls,     // pour application windows
   ravagelib;

const
  SEEK_CUR = 0;
  SEEK_SET = 1;
  RESERVE_SPACE = 2048; // de 0 a 65535 espace RAM de reserve.
  MAX_FIELDS = 64; // de 1 a 65535
  MAX_FIELD_NAME = 64; // de 1 a 65535 MAX_FIELDS * MAX_FIELD_SIZE < 65535


type

Tfields = record        // represente la structure de la base.
   fnames : Array[0..MAX_FIELDS] of String;  // Liste des noms des champs
   fsize : Array[0..MAX_FIELDS] of Word;     // Liste des taille des champs.
   ftype : Array[0..MAX_FIELDS] of String;   // Liste des types des champs.
end;


Tfinditems = record     // represente une reherche sur la base.
   fields : Array[0..MAX_FIELDS] of Word;     // numero des champs
   values : Array[0..MAX_FIELDS] of String;   // Liste des valeurs des champs.
   operat : Array[0..MAX_FIELDS] of String;   // Liste d'operations
end;


TRavageDB=class
  private
    finditems : Tfinditems;
    ronly : Boolean;
    position : LongWord;    // position du curseur dans la base de donnee

    function find(): LongWord;
    function data2str(out,str : PAnsiChar; octets : Word; datatype : PAnsiChar):
      PAnsiChar;
    function str2data(out,str : PAnsiChar; Octets : Word; datatype : PAnsiChar):
      PAnsiChar;
  protected
    dbfields : Tfields;     // recoit les infos sur la structure de la base

    allocsize : LongWord;   // taille de la memoire allouee
    caseflag : Boolean;     // drapeaud de distinstion des majuscules
    ramsize : LongWord;     // taille de la base de donnee
    ram,ramp : PChar;   // pointeur de debut, et variable sur la ram
    eskip : LongWord;       // taille d'une entree moins KEY_BYTES
    //autosave : Word;        // pas encore implementee
    //lastsave : Word;        // pas encore implementee

    function Struct (database_file,fields,sizes,types : String) : Boolean;
    procedure write_idata(var realram:PChar;
                          value : LongWord; size : Byte);
    procedure write_data(var realram:PChar;
                         data : AnsiString; size : Word; datatype : PAnsiChar);
    procedure reset1(var realram:PChar);
    function read_idata(size: Byte) : LongWord;
    function read_data(data : PAnsiChar; size: Word; datatype : PAnsiChar) :
      PAnsiChar;
    procedure move_cursor(number : Integer; whence : Byte);
    function dbeof(): Boolean;
    function find1(): Boolean;
    
  public
    IsLoaded : Boolean;
    Name : String;          // nom de la base de donnee
    constructor create();
    function init(database_file : string): Boolean;
    function load (database_file,fields,sizes,types : String) : Boolean;
    procedure close ();
    function add(): LongWord;
    function update(field,str : string): Boolean;
    procedure add_finditem(field, value,operat : string);
    procedure clear_finditem();
    function findnext(): LongWord;
    function findfirst(): LongWord;
    function get(field : string): string;
    procedure reset();
    property CaseSensitive : Boolean read caseflag write caseflag;
    property Reading : Boolean read ronly write ronly;
    property EntrySize : LongWord read eskip;
    function GetEntryNumber() : LongWord;
    function GetMaxEntries() : LongWord;
    procedure ConvertToCSV (fname : String);
  end;
implementation

uses Classes;


{******************************************************************************
 *     create   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Constructeur de la classe. Initialise le drapeau de sensibilite a la case
 *
 * Boolean   rCaseSensitive  drapeau de sensibilite a la case
 *}

constructor TRavageDB.create();
begin
  DecimalSeparator := '.';   // pour FloatToStr et StrToFloat
  caseflag := false;
  IsLoaded := false;
  ronly := false;
  eskip := 0;
end;


{******************************************************************************
 *     str2data   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Code une chaine en memoire suivant le type desiree.
 * Le type peut etre i pour un entier, s pour une chaine et iN 1<N<9
 *
 * PAnsiChar out        sera affectee par la section codee
 * PAnsiChar str        pointeur sur une chaine de caracteres
 * Word  octets         nombre d'octets a affecter
 * PAnsiChar datatype   type de donnee
 *
 * Retourne un pointeur sur la chaine codee "out"
 *}

function TRavageDB.str2data(out,str : PAnsiChar; octets : Word;
                            datatype : PAnsiChar): PAnsiChar;
var
  neg : Boolean;
  val,octpow : Extended;
  i : Byte;
  p : PAnsiChar;
  idx:Word;
begin
  result := out;
  if (datatype^ = 's') Or (datatype^ = 'S') then
  begin      // traitement d'une chaine
    for idx := 0 to octets-1 do
      (out+idx)^ := #0;
    StrPLCopy(out,str, octets);
    exit;    // fin du traitement
  end;
  val := 0;           // initialise la valeur numerique a 0
  neg := false;       // initialise le drapeau neg a faux
  while str^ = ' ' do // saute les blancs du debut
    inc(str);
  if str^ = '-' then  // si negatif ------------
  begin
    if datatype^ = 'i' then   // si de type numerique:
    begin                     // ---------------------------------------
      inc(str);               // ignore le caractere '-'
      neg := true;            // initialise le drapeau neg a vrai
      val := 1;               // val prend la valeur numerique de "str"
    end;
  end
  else                // sinon ------------------
    val := 1;
  if val = 1 then
  begin
    p := str;
    repeat
      if ((p^ <> ' ') And (p^ <> DecimalSeparator)) And
         ((p^ < '0') Or  (p^ > '9')) then
        val := 0;
      inc(p);                   // verifie le format
    until p^ = #0;
    if val <> 0 then
        val := StrToFloat(str);   // val prend la valeur numerique de "str"
  end;
  inc(datatype);      // si type decimal (datatype sous forme iN)
  if (datatype^ >= '1') And (datatype^ <= '9') then
    val := val * IntPower(10, Ord(datatype^) - Ord('0'));
  val := Round(val);  // ignore la virgule

  out := out + octets;
  for i:=octets-1 downto 0 do   // effectue la transformation octets par octets
  begin                         // -------------------------------
    octpow := Trunc(IntPower(2,8*i));
    dec(out);
    out^ := Chr(Trunc(val / octpow));
    val := val - (Ord(out^) * octpow);
    if neg then
      out^ := Chr(Not Ord(out^));   // si neg prend le complement
  end;                          // ------------------------------
  if neg then
    inc(out^);                  // si neg incremente l'octect de poid faible
end;



{******************************************************************************
 *     data2str   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Decode une chaine en memoire suivant le type desiree.
 * Le type peut etre i pour un entier, s pour une chaine et iN 1<N<9
 *
 * PAnsiChar out        sera affectee par la section decodee
 * PAnsiChar str        pointeur sur une chaine de caracteres
 * Word  octets         nombre d'octets a lire en memoire
 * PAnsiChar datatype   type de donnee
 *
 * Retourne un pointeur sur la chaine decodee "out"
 *}

function TRavageDB.data2str(out,str : PAnsiChar; octets : Word;
                             datatype : PAnsiChar): PAnsiChar;
var
  res : Extended;
  i : Byte;
  neg,issigned : Boolean;
begin
  result := out;
  issigned := false;
  if datatype^ = 'i' then  // initialise le drapeau de signe
    issigned := true;
  if (datatype^ = 's') Or (datatype^ = 'S') then
  begin      // traitement d'une chaine
    {for idx := 0 to octets-1 do
      (out+idx)^ := #0;}
    StrPLCopy(out,str, octets);
    exit;    // fin du traitement
  end;
  res := 0;                 // initialise a 0 "res"
  neg := false;             // initialise a faux le drapeau "neg"
  for i:=0 to octets-1 do
    (out+i)^ := (str+i)^;
  if ((Ord((out+octets-1)^) And $80) = $80) And issigned then
  begin                     // si nombre negatif en donnee
    out^ := Chr(Ord(out^) - 1); // decremente de 1 l'octet de poid faible
    neg := true;                // initialise a vrai le drapeau "neg"
  end;
  for i:=0 to octets-1 do
  begin                      // effectue la transformation octets par octets
    if neg then (out+i)^ := Chr(Not Ord((out+i)^));// si neg prend le complement
    res := res + Ord((out+i)^) * Trunc(IntPower(2, 8*i));
  end;
  if neg then res := -res;   // si neg inverse le resultat
  inc(datatype);             // si type decimal (datatype sous forme iN)
  if (datatype^ >= '1') And (datatype^ <= '9') then
    res := res / IntPower(10, Ord(datatype^) - Ord('0'));  // rajoute la virgule
  StrPLCopy(out,FloatToStr(res),20);
end;


{******************************************************************************
 *     write_data   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Ecrit les donnees dans la base de donnee suivant le type desiree.
 * Le type peut etre i pour un entier, s pour une chaine et iN 1<N<9
 *
 * AnsiString data      donnees a ecrire
 * Word  size           nombre d'octets a affecter dans la base
 * PAnsiChar datatype   type de donnee
 *
 *}

procedure TRavageDB.write_data(var realram:PChar;
          data : AnsiString; size : Word; datatype : PAnsiChar);
var
  curs: LongWord;
  skip:Word;
begin
  skip := LongWord(ram - realram);
  position := position + size;     // mise a jour de la position
  if position >= allocsize-skip then    // si la position depasse l'espace allouee
  begin
    allocsize := position + skip + RESERVE_SPACE;  // augmente l'espace allouee
    curs := LongWord(ramp - ram);  // get ramp from last ram adress
    ReallocMem(realram, allocsize);    // make the block larger
    ram := realram+skip;
    ramp := ram + curs;            // set ramp in the new ram adress
  end;
  if position > ramsize then       // si la position depasse l'espace de la bdd
    ramsize := ramsize + size;     // l'espace bdd est augmenter

  str2data (ramp, PAnsiChar(data), size, datatype);  // ecrit les donnees.
  ramp := ramp + size;             // deplace le pointeur sur bdd
end;


{******************************************************************************
 *     write_idata   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Ecrit les donnees de type numerique entiere et positive dans la base.
 * Utilise pour accelerer la gestion de la base de donnee.
 *
 * LongWord value       valeur numerique entiere et positive
 * Byte  size           nombre d'octets a affecter dans la base < 5
 *
 *}

procedure TRavageDB.write_idata(var realram:PChar;
                                value : LongWord; size : Byte);
var
  octpow : Extended;
  i : Byte;
  out : PAnsiChar;
  curs: LongWord;
  skp:Word;
begin
  skp := LongWord(ram - realram);
  position := position + size;     // mise a jour de la position
  if position >= allocsize-skp then    // si la position depasse l'espace allouee
  begin
    allocsize := position + skp + RESERVE_SPACE;  // augmente l'espace allouee
    curs := LongWord(ramp - ram);  // get ramp from last ram adress
    ReallocMem(realram, allocsize);    // make the block larger
    ram := realram+skp;
    ramp := ram + curs;            // set ramp in the new ram adress
  end;
  if position > ramsize then       // si la position depasse l'espace de la bdd
    ramsize := ramsize + size;     // l'espace bdd est augmenter

  out := ramp + size;             // ecrit les donnees numeriques.
  for i:=size-1 downto 0 do
  begin
    octpow := Trunc(IntPower(2,8*i));
    dec(out);
    out^ := Chr(Trunc(value / octpow));
    value := value - Trunc(Ord(out^) * octpow);
  end;

  ramp := ramp + size;             // deplace le pointeur sur bdd
end;


{******************************************************************************
 *     read_data   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Lit les donnees dans la base de donnee suivant le type desiree.
 * Le type peut etre i pour un entier, s pour une chaine et iN 1<N<9
 *
 * AnsiString data      sera affectee par les donnees lues
 * Word  size           nombre d'octets a lire en memoire
 * PAnsiChar datatype   type de donnee
 *
 * Retourne un pointeur sur la chaine de donnees "data"
 *
 *}

function TRavageDB.read_data(data : PAnsiChar; size: Word;
         datatype : PAnsiChar) : PAnsiChar;
begin
  result := data;
  if (position + size) >= ramsize then
    size := ramsize - position;
  if size = 0 then exit;
  position := position + size;            // mise a jour de la position
  data2str (data, ramp, size, datatype);  // lit les donnees
  ramp := ramp + size;                    // deplace le pointeur sur bdd
end;


{******************************************************************************
 *     read_idata   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Lit les donnees de type numerique entiere et positive dans la base de donnee.
 * Utilise pour accelerer la gestion de la base de donnee.
 *
 * Byte  size           nombre d'octets a lire en memoire
 *
 * Retourne la valeur lue
 *}

function TRavageDB.read_idata(size: Byte) : LongWord;
var
  res : LongWord;
  i : Byte;
begin
  if (position + size) >= ramsize then
    size := ramsize - position;
  result := 0;
  if size = 0 then exit;
  position := position + size;            // mise a jour de la position
  res := 0;                               // lit les donnees numerique
  for i:=0 to size-1 do
  begin
    res := res + (Ord((ramp+i)^) * Trunc(IntPower(2, 8*i)));
  end;
  result := res;

  ramp := ramp + size;                    // deplace le pointeur sur bdd
end;


{******************************************************************************
 *     move_cursor   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Deplace le curseur de la base de donnee
 *
 * Integer number       offcet du deplacement
 * Byte  whence         a partir d'ou ? 0: debut, 1: position courrante
 *
 *}

procedure TRavageDB.move_cursor(number : Integer; whence : Byte);
begin
  if whence = SEEK_CUR then           // par raport a la position courante
  begin
    if (Int64(position)+number) < 0 then
      number := -position;
    if Int64(position)+number >= ramsize then
      number := ramsize - position;
    ramp := ramp + number;                 // deplace le pointeur sur bdd
    position := Int64(position) + number;  // mise a jour de la position
  end;
  if (whence = SEEK_SET) And (number >= 0) then   // par raport au debut
  begin
    ramp := ram+number;                    // deplace le pointeur sur bdd
    position := number;                    // mise a jour de la position
  end;
end;


{******************************************************************************
 *     GetEntryNumber   par alvaro Hermo (alvaro.h@ifrance.com) 30/06/2005
 *
 * Retourne le numéro de l'entrée courante.
 *}

function TRavageDB.GetEntryNumber() : LongWord;
begin
  result := trunc(position / eskip);
end;


{******************************************************************************
 *     GetMaxEntries   par alvaro Hermo (alvaro.h@ifrance.com) 30/06/2005
 *
 * Retourne le nombre maximal d'entrées de la base de donnée.
 *}
 
function TRavageDB.GetMaxEntries() : LongWord;
begin
  result := trunc(ramsize / eskip);
end;


{******************************************************************************
 *     dbeof   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Retourne true si la fin de la base est atteinte, sinon false.
 *}

function TRavageDB.dbeof(): Boolean;
begin
  result := position >= ramsize;
end;



{******************************************************************************
 *     init   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Cree une nouvelle base de donnee.
 *
 * string database_file nom de la base de donnee
 *
 * Retourne vrai si l'opperation c'est bien derouler, et sinon faux.
 *}

function TRavageDB.init(database_file : string): Boolean;
var
  fh : file;
begin
  result := false;
  if ronly then
  begin
    ShowMessage('Cannot create RavageDB: database is set to reading mode !');
    exit;
  end;
  if FileExists(database_file) And FileIsReadOnly(database_file) then
  begin
    ShowMessage('Cannot create RavageDB: target is write Protected !');
    exit;
  end;

  try     // ouvre ou efface le fichier en ecriture
    AssignFile(fh, database_file);
    Rewrite(fh);
  except
    exit;
  end;
  
  CloseFile(fh);
  result := True;
end;


{******************************************************************************
 *     struct   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *}

function TRavageDB.struct (database_file,fields,sizes,types : String) : Boolean;
var
  space : String[MAX_FIELD_NAME];
  buf,fieldsp,sizesp,typesp : PAnsiChar;
  nb : Word;
begin
  result := false;
  buf := Addr(space);

  fieldsp:= PAnsiChar(fields);
  sizesp:= PAnsiChar(sizes);
  typesp:= PAnsiChar(types);

  nb := 0;
  // remplissage de dbfields avec les donnees fields sizes et types
  while (fieldsp^ <> #0) And  (sizesp^ <> #0) And (typesp^ <> #0) And
        (nb < MAX_FIELDS) do          // tan que les pointeurs sont pas a la fin
  begin                                // recupere la prochaine section de sizes
    sizesp := get_section(buf, sizesp, MAX_FIELD_NAME,',');
    dbfields.fsize[nb] := 0;                   // initialise fsize
    if (buf^ <> #0) then dbfields.fsize[nb] := Trunc(StrToFloat(buf));
    typesp := get_section(buf, typesp, 3,','); // initialise ftype
    if buf^ = 'S' then buf^ := 's';
    dbfields.ftype[nb] := buf;
    fieldsp := get_section(buf, fieldsp, MAX_FIELD_NAME, ',');
    if dbfields.fsize[nb] > 0 then             // initialise fnames
    begin
      dbfields.fnames[nb] := buf;
      eskip := eskip + dbfields.fsize[nb];     // mise a jour de eskip
    end;
    inc(nb);
  end;
  dbfields.fnames[nb] := '';    // termine les tableaux fnames, fsize, ftype
  dbfields.fsize[nb] := 0;
  dbfields.ftype[nb] := '';

  clear_finditem();   // vide la liste de recherche
end;


{******************************************************************************
 *     load   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Charge une base de donnee.
 *
 * string database_file nom de la base de donnee
 * string fields        liste des noms de champs separes par des virgules
 * string sizes         liste des tailles de champs separes par des virgules
 * string types         liste des types de champs separes par des virgules
 *
 * Un type peut etre:
 * - "s" une chaîne de caractères de longueur "sizes"
 * - "I" un entier non signé de "sizes" octets
 * - "i" un entier signé de "sizes" octets
 * - "In" un entier non signé avec n décimales de "sizes" octets
 * - "in" un entier signé avec n décimales de "sizes" octets
 * - "c" une chaîne de caractères crypté de longueur "sizes" (non implem)
 *
 * Retourne vrai si l'operation c'est bien derouler, et sinon faux.
 *}

function TRavageDB.load (database_file,fields,sizes,types : String) : Boolean;
begin
  result := false;

  if Not FileExists(database_file) then exit;
  if FileIsReadOnly(database_file) And Not ronly then
  begin
    if MessageDlg('RavageDB is already loaded.'+#10+'load it anyway ?',
      mtConfirmation,[mbYes,mbNo],0) <> mrYes then exit;
  end;
  if Not ronly then FileSetReadOnly(database_file, true);
  ram := ram_load(ramsize,PAnsiChar(database_file),0,  // charge la bdd
         LongWord(-1),RESERVE_SPACE);    // alloue RESERVE_SPACE en plus
  if ram = nil then exit;
  allocsize := ramsize + RESERVE_SPACE;  // initialize "allocsize"
  move_cursor(0, SEEK_SET);              // initialise la position
  Name := database_file;                 // initialise le nom
  struct (database_file,fields,sizes,types);
  IsLoaded := True;
end;


{******************************************************************************
 *     close   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Ferme la base de donnee et libere l'espace memoire.
 *}

procedure TRavageDB.close ();
begin
  if Not IsLoaded then exit;
  if Not ronly then
  begin
    FileSetReadOnly(Name, false);
    ram_save(ram,PAnsiChar(Name),ramsize);
  end;
  FreeMem(ram);                    // libere l'espace memoire
  IsLoaded := false;
end;


{******************************************************************************
 *     add   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Ajoute une entree  dans la base.  Elle est inserer dans un  emplacement vide;
 * c'est a dire dans une entree dont le numero d'identifiant est egal a 0, ou a
 * la fin de la base de donnees. La recherche s' effectue de haut en en bas,  si
 * le numero d'id de l'entree actuelle differe de 0.
 *
 * Retourne le nouveau numero d' identifiant de l'entree.  Il correspond au plus
 * grand numero de la base.
 *}

function TRavageDB.add(): LongWord;
begin
  result := 0;
  if Not IsLoaded then
  begin
    ShowMessage('RavageDB cannot add in a closed database !');
    exit;
  end;
  if ronly then exit;
  // se place a la fin
  while Not dbeof() do move_cursor(eskip, SEEK_CUR);
  reset();
  result := 0;
end;


{******************************************************************************
 *     reset   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Vide l'entree courante (sur la position courante du curseur).
 *}

procedure TRavageDB.reset();
begin
  reset1(ram);
end;

procedure TRavageDB.reset1(var realram:PChar);
var
  i,skp : SmallInt;
begin
  i := 0;
  skp := 0;
  while (dbfields.fsize[i] <> 0) do
  begin
    skp := skp - dbfields.fsize[i];
    if UpperCase(LeftStr(dbfields.ftype[i],1)) = 'I' then
      write_data(realram,'0',
         dbfields.fsize[i], PAnsiChar(dbfields.ftype[i] + #0))
    else
      write_data(realram,'' + #0, dbfields.fsize[i],
                 PAnsiChar(dbfields.ftype[i] + #0));
    inc(i);
  end;
  move_cursor(skp, SEEK_CUR);  // se positionne au debut de l'entree
end;


{******************************************************************************
 *     update   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * met a jour une entree de la base de donnee
 *
 * string    field      nom du champ
 * string    str        donnees a rentrer
 *}

function TRavageDB.update(field,str : string): Boolean;
var
  i,skp : SmallInt;
begin
  result := false;
  if Not IsLoaded then exit;
  if ronly then exit;
  if Not caseflag then                              // si pas de case
    field := AnsiStrLower(PAnsiChar(field + #0));  // ignore la case
  if dbeof() then exit;

  skp := 0;
  i := 0;
  while dbfields.fsize[i] <> 0 do
  begin
    skp := skp - dbfields.fsize[i];
    if AnsiCompareStr(dbfields.fnames[i], field) = 0 then
    begin                  // mise a jour d'un champ
      move_cursor(0, SEEK_CUR);  // pour la methode fseek
      write_data(ram,str, dbfields.fsize[i], PAnsiChar(dbfields.ftype[i]));
      move_cursor(skp, SEEK_CUR);// se positionne au debut de l'entree
      result := true;
      exit;                      // sort et retourne vrai
    end
    else
      move_cursor(dbfields.fsize[i], SEEK_CUR); // sur le prochain champ
    inc(i);
  end;

end;


{******************************************************************************
 *     add_finditem   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Rajoute un element de recherche dans la liste de recherche.
 * Si l'element demandé est déjà dans la liste, sa valeur sera modifiée.
 *
 * string    field      nom du champ
 * string    value      valeur du champ
 * string    operat     operand (=,<>,<,>,like)
 *}

procedure TRavageDB.add_finditem(field, value, operat : string);
var
  nb,i : Word;
begin
  if Not IsLoaded then exit;

  if Not caseflag then                              // si pas de case
    field := AnsiStrLower(PAnsiChar(field + #0));  // ignore la case
  for i := 0 to MAX_FIELDS do
    if (dbfields.fnames[i] = field) Or (dbfields.fsize[i] = 0) then break;
  if dbfields.fsize[i] <> 0 then
  begin
    if Not caseflag then value := AnsiStrLower(PAnsiChar(value + #0));
    for nb := 0 to MAX_FIELDS do
    begin
      if finditems.fields[nb] = i+1 Then
      begin
        finditems.values[nb] := value;
        finditems.operat[nb] := operat;
        exit;
      end;
      if finditems.fields[nb] = 0 then break;
    end;
    finditems.fields[nb] := i+1;
    finditems.values[nb] := value;
    finditems.operat[nb] := operat;
    finditems.fields[nb+1] := 0;
  end;
end;


{******************************************************************************
 *     clear_finditem   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Efface la liste des elements de recherche.
 *}

procedure TRavageDB.clear_finditem();
begin
  finditems.fields[0] := 0;
end;


{******************************************************************************
 *     find1   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 *}

function TRavageDB.find1(): Boolean;
var
  i,nb,found,expect,skp : Word;
  buf : PAnsiChar;
begin
  result := False;

    for expect := 0 to MAX_FIELDS do
      if finditems.fields[expect] = 0 then break;

      found := 0;
      skp := 0;
      i := 0;
      while dbfields.fsize[i] <> 0 do
      begin
        skp := skp + dbfields.fsize[i];
        GetMem(buf,dbfields.fsize[i]+20);
        read_data(buf,dbfields.fsize[i],PAnsiChar(dbfields.ftype[i]));
        if dbfields.ftype[i] = 's' then (buf+dbfields.fsize[i])^ := #0;
        if (Not caseflag) and (dbfields.ftype[i] = 's') then
          buf := AnsiStrLower(buf);
        for nb := 0 to MAX_FIELDS do
        begin
          if finditems.fields[nb] = 0 then break;
          if finditems.fields[nb] = i+1 then
          begin

            if ((finditems.operat[nb] = '<>') And
               (AnsiCompareStr(buf, finditems.values[nb]) <> 0)) Or
               ((finditems.operat[nb] = '=') And
               (AnsiCompareStr(buf, finditems.values[nb]) = 0)) Or
               ((finditems.operat[nb] = '>') And
               (AnsiCompareStr(buf, finditems.values[nb]) > 0)) Or
               ((finditems.operat[nb] = '<') And
               (AnsiCompareStr(buf, finditems.values[nb]) < 0)) Or
               ((finditems.operat[nb] = 'like') And
               RavageMatch(buf,PAnsiChar(finditems.values[nb])))
            then
            begin
              inc(found);
              if found = expect then
              begin
                move_cursor( -skp, SEEK_CUR); // entree courante
                result := True;
                FreeMem(buf);
                exit;          // Sort sur l'entree courante. permet d'effectuer
              end;             // des operations durant une recherche.
            end;
          end;
        end; // for
        inc(i);
        FreeMem(buf);
      end;  // while
end;

{******************************************************************************
 *     find   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Trouve l'entree suivante qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageDB.find(): LongWord;
begin
  result := 0;
  while Not dbeof() do
  begin
    if find1() Then
    begin
      result := 1;
      exit;  // Sort sur l'entree courante. permet d'effectuer
    end;
    //move_cursor(eskip,SEEK_CUR);
  end;
end;


{******************************************************************************
 *     findnext   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Trouve l'entree suivante qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageDB.findnext(): LongWord;
begin
  result := 0;
  if Not IsLoaded then exit;
  move_cursor(eskip,SEEK_CUR);  // saute a l'entree suivante
  result := find();
end;


{******************************************************************************
 *     findfirst   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Trouve la premiere entree qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageDB.findfirst(): LongWord;
begin
  result := 0;
  if Not IsLoaded then
  begin
    ShowMessage('RavageDB cannot find from a closed database !');
    exit;
  end;
  move_cursor(0, SEEK_SET);
  result := find();
end;


{******************************************************************************
 *     get   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Recupere un element d'une entree de la base de donnee
 *
 * string    field      nom du champ a recuperer
 *
 * Retourne la chaine de l'element.
 *}

function TRavageDB.get(field : string): string;
var
  i,skp : SmallInt;
  str : PAnsiChar;
begin
  result := '';
      skp := 0;
      i := 0;
      while dbfields.fsize[i] <> 0 do
      begin
        skp := skp - dbfields.fsize[i];
        if AnsiCompareStr(dbfields.fnames[i], field) = 0 then
        begin
          move_cursor(0, SEEK_CUR);  // need fseek
          GetMem(str,dbfields.fsize[i]+20);   // interger
          read_data(str, dbfields.fsize[i], PAnsiChar(dbfields.ftype[i]));
          if dbfields.ftype[i] = 's' then (str+dbfields.fsize[i])^ := #0;
          result := str;
          FreeMem(str);
          move_cursor(skp, SEEK_CUR);
          exit;
        end
        else
          move_cursor(dbfields.fsize[i], SEEK_CUR);
        inc(i);
      end;
end;




// ex fichier CSV
//"CLE_DIXI","CLE_DIXIE"
//"3993","3992"

procedure TRavageDB.ConvertToCSV (fname : String);
var
  f : textfile;
  i: SmallInt;
  buf,debut: String;
begin
  if Not DirectoryExists(ExtractFilePath(fname)) Then exit;
  try
    AssignFile(f, fname);
    Rewrite(f);
  except
    exit;
  end;
  debut := '';
  i := 0;
  while dbfields.fnames[i] <> '' do
  begin
    buf := buf + debut + '"' + dbfields.fnames[i] + '"';
    debut := ',';
    inc(i);
  end;
  writeln(f,buf);
  if findfirst() <> 0 Then
  begin
  repeat
    i := 0;
    debut := '';
    buf := '';
    while dbfields.fnames[i] <> '' do
    begin
      buf := buf + debut + '"' + get(dbfields.fnames[i]) + '"';
      debut := ',';
      inc(i);
    end;
    writeln(f,buf);
  until findnext() = 0;
  end;
  CloseFile(f);
end;


end.




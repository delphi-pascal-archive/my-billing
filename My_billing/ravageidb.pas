{******************************************************************************
 * ravageidb.pas v0.1 for Kylix3 and Delphi7
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
 *   db : TRavageiDB;       // ravage database
 *   id,id0 : Integer;
 * begin
 *   db := TRavageiDB.create;
 *   if Not FileExists('database.dat') then
 *     db.init('database.dat', 'driver,car,track,version,conditions,date',
 *                  '19,13,4,19,1,6',
 *                  's,s,s,s,I,i'   );
 *   db.load('database.dat');
 *
 *   // to add an element
 *   id0 := db.add;
 *   db.update(id0,'driver','gaston');
 *   db.update(id0,'car','mac laren');
 * 
 *   // to find an element
 *   db.add_finditem('driver','gaston','=');
 *   db.add_finditem('car','mac laren*','like');
 *  
 *   id := db.findfirst();
 *   while id <> 0 do
 *   begin
 *     Lst3Find.Items.Add(FloatToStr(id) + ' ' + db.get(id,'driver') + ',' +
 *                        db.get(id,'car'));
 *     id := db.findnext();
 *   end;
 * 
 *   // to remove an element
 *   db.remove(id0);
 *   
 *   db.close;
 * end;
 *
 * Note: On crash the base can contain invalid data, dont forget to check data
 * Like that:
 *
 * for index := 1 to db.MaxEntries do
 * begin
 *   tmp := db.get(index,'j');
 *   if tmp <> '' then
 *   begin
 *     d := StrToInt(db.get(index,'j'));
 *     m := StrToInt(db.get(index,'m'));
 *     y := StrToInt(db.get(index,'aa'));
 *    db.get
 *    if (d<1) or (d>31) or (m<1) or
        (m>12) or (y<2006) or (y>9999) then
 *    begin
 *      db.remove(index);
 *      continue;
 *    end;
 * end;
 *
 * NB: The Source Code is commented in french
 *****************************************************************************}

unit ravageidb;

interface

uses
   SysUtils, StrUtils,
   QDialogs,QControls,   // pour application CLX
   //Dialogs,Controls,     // pour application windows
   ravagedb,ravagelib,pc1;

const
  KEY_BYTES = 3; // de 1 a 4   1:256 2:65536 3:8388608 4:4294967296

type

TRavageiDB=class(TRavageDB)
  private
    lastram : PChar;
    skip : Word;            // taille de l'entete de la ram
    emptyrm : Boolean;
    maxid : LongWord;       // plus grand numero d'identification assigne
    function find(): LongWord;
  public
    constructor create();
    function init(database_file,fields,sizes,types : string): Boolean;
    function load (database_file : string) : Boolean;
    function add(): LongWord;
    function remove(id : LongWord): Boolean;
    procedure close ();
    function update(id : LongWord; field,str : string): Boolean;
    function findfirst(): LongWord;
    function findnext(): LongWord;
    function get(id : LongWord; field : string): string;
    property EmptyOnRemove : Boolean read emptyrm write emptyrm;
    property MaxEntries: LongWord read maxid;
  end;

implementation


{**
 *     create   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Constructeur de la classe. Initialise le drapeau de sensibilite a la case
 *
 * Boolean   rCaseSensitive  drapeau de sensibilite a la case
 *}

constructor TRavageiDB.create();
begin
  inherited create();
  emptyrm := False;
end;



{******************************************************************************
 *     init   par alvaro Hermo (alvaro.h@ifrance.com) 30/06/2005
 *
 * Cree une nouvelle base de donnee.
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
 * Retourne vrai si l'opperation c'est bien derouler, et sinon faux.
 *}

function TRavageiDB.init(database_file,fields,sizes,types : string): Boolean;
var
  f,s,t : Word;
begin
  result := false;
  if not Inherited init(database_file) then exit;
  // [clef max][Taille fields][Taille sizes][Taille types][fields][sizes][types]
  {
  pour masquer la structure dans le fichier (adapter le load):

    ramsize := KEY_BYTES;
    GetMem(ram, KEY_BYTES+2);
    ramp := ram;
    move_cursor(0, SEEK_SET);
    write_idata(0, KEY_BYTES);
    ram_save(ram,PAnsiChar(database_file),KEY_BYTES);
    FreeMem(ram);
  }
  f := Length(fields)+1;
  s := Length(sizes)+1;
  t := Length(types)+1;
  ramsize := KEY_BYTES+6+f+s+t;  // taille des donnees a ecrire
  allocsize := ramsize + 2;      // evite la reallocation automatique
  GetMem(ram, allocsize);        // initialise l'espace memoire
  move_cursor(0, SEEK_SET);      // initialise la base
  write_idata(ram,0, KEY_BYTES);     // ecrit la clef maximale de 0
  if ram^ <> #0 Then
  begin
    ShowMessage('Unable to create RavageDB');
    DeleteFile(database_file);
    exit;
  end;
  write_idata(ram,f, 2);             // ecrit la taille de la liste fields
  write_idata(ram,s, 2);             // ecrit la taille de la liste sizes
  write_idata(ram,t, 2);             // ecrit la taille de la liste types

  write_data(ram,fields, f, 's');    // ecrit la liste fields
  write_data(ram,sizes, s, 's');     // ecrit la liste sizes
  write_data(ram,types, t, 's');     // ecrit la liste types

  ram_save(ram,PAnsiChar(database_file),ramsize);  // cree le fichier bdd
  freemem(ram);                  // libere l'espace memoire
  result := true;
end;


{**
 *     load   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Charge une base de donnee.
 *
 * string   database_file   nom de la base de donnee
 *
 * Retourne vrai si l'opperation c'est bien derouler, et sinon faux.
 *}

function TRavageiDB.load (database_file : string) : Boolean;
var
  fields,sizes,types : PAnsiChar;
  f, s, t : Word;
begin
  result := false;

  // charge l'en tete du fichier
  if Not FileExists(database_file) then exit;
  ram := ram_load(ramsize,PAnsiChar(database_file),0,  // charge l'entete de bdd
         6+KEY_BYTES+1,0);    // alloue RESERVE_SPACE en plus
  if ram = nil then exit;
  move_cursor(0, SEEK_SET);              // initialise la position
  Name := database_file;                 // initialise le nom
  if ramsize < (6 + KEY_BYTES) Then
  begin
    ShowMessage('invalid format');
    exit;
  end;
  maxid := read_idata(KEY_BYTES);        // lit la clef maximale
  f := read_idata(2);          // lit la taille des listes fields, sizes, types
  s := read_idata(2);
  t := read_idata(2);
  freeMem(ram);

  // charge la structure de la base en en-tete du fichier
  skip := f+s+t+KEY_BYTES+6;       // initialise la taille de l'entete de la ram
  ram := ram_load(ramsize,PAnsiChar(database_file),0,  // charge la struct bdd
         skip+1,0);
  move_cursor(KEY_BYTES+6,SEEK_SET);
  if ramsize < skip Then
  begin
    ShowMessage('invalid format');
    exit;
  end;

  GetMem(fields, f);       // alloue de la memoire pour fields, sizes, types
  GetMem(sizes, s);
  GetMem(types, t);
  read_data(fields, f, 's');           // charge les listes fields, sizes, types
  read_data(sizes, s, 's');
  read_data(types, t, 's');
  freeMem(ram);
  (fields + f-1)^ := #0;
  (sizes + s-1)^ := #0;
  (types + t-1)^ := #0;

  If Not caseflag then
  begin
    fields := AnsiStrLower(fields);
    sizes  := AnsiStrLower(sizes);
  end;

  // chargement terminé
  Inherited load (database_file,fields,sizes,types);
  lastram := ram;
  ram := ram+skip;
  ramsize := ramsize - skip;
  move_cursor(0,SEEK_SET);

  FreeMem(fields);    // libere les espaces memoires de fields, sizes, types
  FreeMem(sizes);
  FreeMem(types);
end;


{**
 *     close   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Ferme la base de donnee et libere l'espace memoire.
 *}

procedure TRavageiDB.close ();
begin
  if IsLoaded then
  begin
    ram := lastram;
    ramsize := ramsize+skip;
    move_cursor(0, SEEK_SET);        // se positionne au tout debut de la base
    write_idata(ram,maxid, KEY_BYTES);   // Sauvegarde le plus grand no d'id
  end;
  inherited close;
end;


{**
 *     add   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Ajoute une entree  dans la base.  Elle est inserer dans un  emplacement vide;
 * c'est a dire dans une entree dont le numero d'identifiant est egal a 0, ou a
 * la fin de la base de donnees. La recherche s' effectue de haut en en bas,  si
 * le numero d'id de l'entree actuelle differe de 0.
 *
 * Retourne le nouveau numero d' identifiant de l'entree.  Il correspond au plus
 * grand numero de la base.
 *}

function TRavageiDB.add(): LongWord;
var
  id : Int64;
begin
  result := 0;
  if Not IsLoaded then
  begin
    ShowMessage('RavageDB cannot add in a closed database !');
    exit;
  end;

  id := read_idata(KEY_BYTES);             // lit l'id

  if dbeof() Or (id <> 0) then             // si fin de bdd ou entree occupee
  begin
    move_cursor(0, SEEK_SET);           // se place sur le premiere entree
    id := read_idata(KEY_BYTES);           // lit l'id
  end;

  while Not dbeof() do
  begin
    if id = 0 then          // si l'entree est inocupee
    begin
       move_cursor(-KEY_BYTES, SEEK_CUR);  // se positionne au debut de l'entree
       Break;                              // quitte la boucle
    end
    else                   // sinon ----------
      move_cursor(eskip, SEEK_CUR);        // se place sur l'entree suivante
    id := read_idata(KEY_BYTES);
  end;

  inc(maxid);             // mise a jour de maxid
  write_idata(lastram,maxid, KEY_BYTES);   // ecriture d'une entree vierge
  reset1(lastram);
  move_cursor(-KEY_BYTES, SEEK_CUR);

  result := maxid;
end;


{**
 *     remove   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Efface l'entree qui correspond au numero d'id donnee. 0 est attribuer  a l'id
 * de l'entree.
 *
 * LongWord  id         numero d'identifiant de l'entree a supprimer
 *
 * Retourne vrai si la suppression a ete realisee correctement, et sinon; faux
 *}

function TRavageiDB.remove(id : LongWord): Boolean;
var
  lastid : Int64;
begin
  result := false;
  if Not IsLoaded then
  begin
    ShowMessage('RavageDB cannot remove from a closed database !');
    exit;
  end;
  lastid := read_idata(KEY_BYTES);             // lit l'id dans lastid
  if dbeof() Or (lastid <> id) then
  begin    // si c'est la fin de la base ou que l'id courrant difere de 0
    move_cursor(0, SEEK_SET);  // se repositionne au debut de la base
    lastid := read_idata(KEY_BYTES);
  end;
  while Not dbeof() do
  begin
    if lastid = id then  // si l'id correspond
    begin
      move_cursor(-KEY_BYTES,SEEK_CUR);
      write_idata(lastram,0, KEY_BYTES);          // affecte 0 a l'id
      if emptyrm Then reset1(lastram);
      move_cursor(-KEY_BYTES,SEEK_CUR);   // se positionne au debut de l'entree
      result := true;
      break;                              // quitte la boucle
    end
    else // 1 entree <=> eskip + 4
      move_cursor(eskip,SEEK_CUR);// se positionne au debut de l'entree suivante
    lastid := read_idata(KEY_BYTES); // recupere l'id dans buf
  end;
end;


{**
 *     update   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * met a jour une entree de la base de donnee
 *
 * LongWord  id         numero d'identifiant de l'entree a mettre a jour
 * string    field      nom du champ
 * string    str        donnees a rentrer
 *}

function TRavageiDB.update(id : LongWord; field,str : string): Boolean;
var
  lastid : Int64;
begin
  result := false;
  if Not IsLoaded then exit;
  if Not caseflag then                              // si pas de case
    field := AnsiStrLower(PAnsiChar(field + #0));  // ignore la case
  lastid := read_idata(KEY_BYTES);                // lit l'id dans lastid
  if dbeof() Or (lastid <> id) then
  begin    // si c'est la fin de la base ou que l'id courrant difere de 0
    move_cursor(0, SEEK_SET);  // se repositionne au debut de la base
    lastid := read_idata(KEY_BYTES);
  end;
  while Not dbeof() do
  begin
    if lastid = id then  // si l'id correspond
    begin
      inherited update(field,str);
      move_cursor(-KEY_BYTES, SEEK_CUR);
      exit;
    end
    else
      move_cursor(eskip,SEEK_CUR);// se positionne au debut de l'entree suivante
    lastid := read_idata(KEY_BYTES); // recupere l'id dans buf
  end;
end;


{**
 *     find   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Trouve l'entree suivante qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageiDB.find(): LongWord;
var
  id : Int64;
begin
  result := 0;

  while Not dbeof() do
  begin
    id := read_idata(KEY_BYTES);
    if id <> 0 then
    begin
      if inherited find1() Then
      begin
        move_cursor( -KEY_BYTES, SEEK_CUR); // entree courante
        result := id;
        exit;          // Sort sur l'entree courante. permet d'effectuer
      end;
    end
    else
      move_cursor(eskip,SEEK_CUR);
  end;
end;


{**
 *     findnext   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Trouve l'entree suivante qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageiDB.findnext(): LongWord;
begin
  result := 0;
  if Not IsLoaded then exit;
  move_cursor(eskip + KEY_BYTES,SEEK_CUR);  // saute a l'entree suivante
  result := find();
end;


{**
 *     findfirst   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Trouve la premiere entree qui correspond a la liste de recherche
 *
 * Retourne le numero d'identifiant de l'entree trouvee ou 0 si non trouve
 *}

function TRavageiDB.findfirst(): LongWord;
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


{**
 *     get   par alvaro Hermo (alvaro.h@ifrance.com) 18/06/2005
 *
 * Recupere un element d'une entree de la base de donnee
 *
 * LongWord  id         numero d'identifiant de l'entree
 * string    field      nom du champ a recuperer
 *
 * Retourne la chaine de l'element.
 *}

function TRavageiDB.get(id : LongWord; field : string): string;
var
  lastid : Int64;
begin
  result := '';
  if Not IsLoaded then
  begin
    ShowMessage('RavageDB cannot read from a closed database !');
    exit;
  end;
  if Not caseflag then  field := AnsiStrLower(PAnsiChar(field + #0));
  lastid := read_idata(KEY_BYTES);
  if dbeof() Or (lastid <> id) then
  begin
    move_cursor(0, SEEK_SET);
    lastid := read_idata(KEY_BYTES);
  end;
  while Not dbeof() do
  begin
    if lastid = id then
    begin
      result := inherited get(field);
      move_cursor(-KEY_BYTES, SEEK_CUR);
      exit;
    end
    else
      move_cursor(eskip,SEEK_CUR);
    lastid := read_idata(KEY_BYTES);
  end;
end;


end.

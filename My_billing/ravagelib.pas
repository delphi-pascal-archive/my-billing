{******************************************************************************
 * ravagelib.pas v0.3 for Kylix3 and Delphi7
 * Copyright (c) 16th june 2007 by alvaro Hermo <alvaro.h@ifrance.com>
 *
 * ravage function library under the terms of the GNU General Public License
 * please see licence.txt
 *
 * get_section      Recupere la prochaine section, delimitee par un caractere
 *                  donnee.
 *
 * ram_load         Alloue un espace donnee en RAM, puis charge une section du
 *                  fichier donnee.
 *
 * ram_reload       Charge une section du fichier dans un espace déja alloué.
 *
 * ram_save         Sauvegarde une partie de la memoire RAM, dans un fichier.
 *
 * ram_add          Ajoute a la fin du fichier le contenu de la memoire.
 *
 * ram_ReadLn       Lit une ligne d'un fichier texte chargé en ram.
 *
 * ram_WriteLn
 *
 * IntPower
 *
 * RavageMatch
 *
 *
 ******************************************************************************}


unit ravagelib;

interface

uses SysUtils,StrUtils,QDialogs,Types;

//type

function get_section(section,str: PAnsiChar; max: LongWord; separ: Char): PChar;
function ram_get(section,str,separ:PChar; max: LongWord;
                 endsepar:PChar): PChar;
function ram_load(var ramsize: LongWord; fname: PChar;
                  begsec, endsec : LongWord; overroom : Word): PChar;
function ram_reload(var ramsize : LongWord; fname : PChar;
                 begsec, endsec : LongWord; maxsize : LongWord;
                 var ram : PChar): PChar;
function ram_save(var rams:PAnsiChar; fname : PAnsiChar; size : LongWord):
                  Boolean;
function ram_add(var rams: PAnsiChar; fname : PAnsiChar; size : LongWord):
         Boolean;
procedure ram_ReadLn(ram:PChar; var pram: PChar; line:PChar; MaxSize:LongWord;
                     endsepar:PChar);
function IntPower(x : Extended; y : Integer) : Extended;
function RavageMatch (Str, Pattern: PChar) : Boolean;

implementation


{******************************************************************************
 *     get_section   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Recupere la prochaine section, delimitee par un caractere donnee.
 *
 * PChar *result   sera affectee par la section trouvee
 * PChar *str      pointeur sur une chaine de caracteres
 * Word  max      longueur maximale de la section
 * Char separ     caractere delimiteur
 *
 * Retourne un pointeur sur le debut de la prochaine section.
 * En cas d'erreur sur la taille *result recoit NULL.
 *}

function get_section(section,str: PAnsiChar; max: LongWord; separ: Char): PChar;
var
  p : PAnsiChar;
begin
  result := #0;
  while str^ = separ do // saute les blancs du debut
    inc(str);

  p := str;   // p est positionne au debut d'une section
  while (str^ <> separ) and (str^ <> #0) do
    inc(str); // positionne str a la fin de la section + 1
  if ( LongWord(str - p - 1) > max) then
    exit;
  if str > p then
    while p < str do
    begin     // recopie les caracteres de p a str dans 'section'
      section^ := p^;
      inc(p);
      inc(section);
    end;
  section^ := #0; // Termine la section
  while str^ = separ do // saute les blancs de fin
    inc(str);
  result := str; // retourne la nouvelle position de str
end;

{******************************************************************************
 *     ram_get   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Recupere la prochaine section, delimitee par un caractere donnee.
 *
 * PChar *result   sera affectee par la section trouvee
 * PChar *str      pointeur sur une chaine de caracteres
 * Char separ     liste caractere delimiteur
 * Word  max      longueur maximale de la chaine
 *
 * Retourne un pointeur sur le debut de la prochaine section.
 *}

function ram_get(section,str,separ:PChar; max: LongWord;
                 endsepar:PChar): PChar;
var
  p : PAnsiChar;
  cmp: array[0..1] of Char;
  lastcr : Boolean;
  endsep1 : PChar;
begin
  endsep1 := endsepar;
  cmp[1] := #0;
  cmp[0] := str^;
  p := str;   // p est positionne au debut
  while (strpos(separ,cmp) <> nil) do // saute les blancs du debut
  begin
    inc(str);
    cmp[0] := str^;
  end;

  while (strpos(separ,cmp) = nil) and (LongWord(str - p) < max) do
  begin  // recopie les caracteres de p a str dans 'section'
    section^ := str^;
    inc(str);
    inc(section);
    cmp[0] := str^;
  end;

  endsep1^ := str^;
  section^ := #0; // Termine la section

  lastcr := false;
  while (strpos(separ,cmp) <> nil) do // saute les blancs de fin
  begin
    inc(str);
    inc(endsep1);
    endsep1^ := str^;
    cmp[0] := str^;
    //if lastcr and (cmp[0] <> #13) and (cmp[0] <> #10) then
    //  break;
    //if (cmp[0] = #13) or (cmp[0] = #10) then lastcr := true;
  end;
  endsep1^ := #0;
  (endsep1+1)^ := #0;

  result := str; // retourne la nouvelle position de str
end;


{******************************************************************************
 *     ram_load   par alvaro Hermo (alvaro.h@ifrance.com) 2/02/2005
 *
 * Alloue un espace donnee en RAM, puis charge une section du fichier donnee.
 *
 * LongWord  ramsize       ramsize est assigné a la longueur de la section.
 * PChar     fname         nom du fichier a charger.
 * LongWord  begsec        debut de la section (0 a 4,294,967,295).
 * LongWord  endsec        fin de la section (LongWord(-1) pour la fin).
 * Word      overroom      espace a allouee en plus de la taille de la
 *                         section (0 a 65,535).
 *
 * Retourne le pointeur sur la mémoire allouée, ou NULL en cas d'erreur.
 *
 *   Example:
 *   ~~~~~~~
 *      ram: PChar;
 *      ramsize: LongWorld;
 *
 *      ram := ram_load(ramsize,PChar('file.txt'),0,LongWord(-1),0);
 *      FreeMem(ram);
 *}

function ram_load(var ramsize : LongWord; fname : PChar;
                  begsec, endsec : LongWord; overroom : Word): PChar;
var
  f: file;
  Length : LongWord;
  Buffer : PChar;
begin
  result := nil;
  try
    AssignFile(f, fname);
    FileMode := 0;
    Reset(f, 1); { Record size = 1 }
    FileMode := 2;
  except
    exit;
  end;
  Length := FileSize(f);
  Seek(f,begsec);
  if  (endsec <= begsec) Or (endsec > length) then
    endsec := length;
  length := endsec-begsec+1;
  ramsize := length-1;
  GetMem(Buffer, length+overroom);
  result := Buffer;
  try
    BlockRead(f, Buffer^, ramsize);
  except
    exit;
  end;
  CloseFile(f);
end;


{******************************************************************************
 *     ram_reload   par alvaro Hermo (alvaro.h@ifrance.com) 2/02/2005
 *
 * Charge une section du fichier dans un espace déja alloué.
 *
 * LongWord  ramsize       ramsize recoit la taille des données copiées.
 * PChar     fname         nom du fichier a charger.
 * LongWord  begsec        debut de la section (0 a 4,294,967,295).
 * LongWord  endsec        fin de la section (-1 pour la fin du fichier).
 * Word      maxsize       taille de l'espace allouee - 1
 * PChar     ram           espace alloué.
 *
 * Retourne le pointeur sur la mémoire allouée, ou NULL en cas d'erreur.
 * Si maxsize est dépassé, il y a une réalocation
 *
 *   Example:
 *   ~~~~~~~
 *      ram: PChar;
 *      ramsize: LongWorld;
 *
 *      ram := GetMem(ram, 204801);
 *      ram := ram_reload(ramsize,PChar('file.txt'),0,LongWord(-1),204800,ram);
 *      FreeMem(ram);
 *}

function ram_reload(var ramsize : LongWord; fname : PChar;
                 begsec, endsec : LongWord; maxsize : LongWord;
                 var ram : PChar): PChar;
var
  f: file;
  Length : LongWord;
begin
  result := nil;
  try
    AssignFile(f, fname);
    Reset(f, 1); { Record size = 1 }
  except
    exit;
  end;
  Length := FileSize(f);
  Seek(f,begsec);
  if  (endsec <= begsec) Or (endsec > length) then
    endsec := length;
  length := endsec-begsec+1;
  ramsize := length-1;
  if ramsize > maxsize Then
  begin
    FreeMem(ram);
    GetMem(ram, length);
  end;
  result := ram;
  try
    BlockRead(f, ram^, ramsize);
  except
    exit;
  end;
  CloseFile(f);
end;


{******************************************************************************
 *     ram_save   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Sauvegarde une partie de la memoire RAM, dans un fichier.
 *
 * PChar ram            pointeur sur la memoire
 * PChar fname          nom du fichier
 * LongWord size        taille a sauvegarder  (0 a 4,294,967,295)
 *
 * Retourne true si l'operation s'est bien deroulee, false en cas d'erreur.
 *}

function ram_save(var rams: PAnsiChar; fname : PAnsiChar; size : LongWord):
         Boolean;
var
  f: file;
begin
  result := false;
  try
    AssignFile(f, fname);
    Rewrite(f,1);
  except
    exit;
  end;
  try
    BlockWrite(f, rams^, size);
  except
    CloseFile(f);
    exit;
  end;
  CloseFile(f);
  result := true;

end;


{******************************************************************************
 *     ram_add   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Ajoute a la fin du fichier le contenu de la memoire.
 *}

function ram_add(var rams: PAnsiChar; fname : PAnsiChar; size : LongWord):
         Boolean;
var
  f: file;
begin
  result := false;
  try
    AssignFile(f, fname);
    Reset(f);
    FileMode := 2;
  except
    exit;
  end;
  seek(f,filesize(f));
  try
    BlockWrite(f, rams^, size);
  except
    CloseFile(f);
    exit;
  end;
  CloseFile(f);
  result := true;

end;


{******************************************************************************
 *     ram_ReadLn   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Lit une ligne d'un fichier texte chargé en ram.
 * ram se positionne sur le début de la ligne suivante
 *}

procedure ram_ReadLn(ram:PChar; var pram: PChar; line:PChar; MaxSize:LongWord;
                     endsepar:PChar);
var
  separ: array [0..2]of Char;
begin
  separ[0] := #13;
  separ[1] := #10;
  separ[2] := #0;
  pram := ram_get(line,pram,separ, MaxSize-LongWord(pram-ram),endsepar);
end;


{******************************************************************************
 *     IntPower   par alvaro Hermo (alvaro.h@ifrance.com) 16/07/2004
 *
 * Calcule x puissance y.
 *
 * Extended x           nombre
 * Integer y            puissance entiere
 *}

function IntPower(x : Extended; y : Integer) : Extended;
var
  invert : Boolean;
  i : Integer;
  res : Extended;
begin
  res := 1;
  invert := false;
  if y < 0 then
  begin
    invert := true;
    y := -y;
  end;
  for i:=0 to y-1 do
    res := res * x;
  if invert then
    result := 1/res
  else
    result := res;
end;

{******************************************************************************
 *     RavageMatch   par alvaro Hermo (alvaro.h@ifrance.com) 28/11/2004
 *
 * Compare une chaine avec un Pattern pouvant contenir des wilcards ('*' & '?')
 * Delphi fournit,  dans l'unité Masks,  la fonction MatchesMask. Mais elle est
 * moins rapide.
 *
 * PChar            Str        chaine a tester
 * PChar            Pattern    chaine de comparaison
 *
 * Retourne la valeur vrai si la chaine correspond avec le Pattern
 *}

function RavageMatch (Str, Pattern: PChar) : Boolean;
var
  p1 : PChar;  // pointeur de sauvegarde de position
begin
  p1 := nil;
  result := False;
  if Pattern = #0 then exit;
  while (Str^<>#0) And ((Pattern^='?') Or (Pattern^='*') Or (Pattern^=Str^))
  do  // Le patern correspond à la chaine à tester
  begin
    if Pattern^ <> '*' then
      Inc(Pattern) // caractère du pattern validé: passe au caractère suivant.
    else           // chaine du pattern vérouillé sur '*'
      while  (Pattern+1)^ = '*' do Inc(Pattern); // étoiles redondantes zapées
    Inc(Str); // passe au caractère suivant pour la chaine à tester.
    
    if (Pattern^ ='*') And (Str^ = (Pattern+1)^) then
    begin     // ici possibilité de déverouillage du pattern.
      p1 := Pattern;  // sauvegarde de la position de l'étoile
      Inc(Pattern);   // déverouillage du pattern
    end;
    if (nil <> p1) And (Pattern^ <> Str^) And (Pattern^ <> '?')
                   And (Pattern^ <> '*')  then
    begin  // déverouillage anticipé => reverouillage à la position p1
      Pattern := p1;  // restauration de la position du Pattern
      if Str^ = (Pattern+1)^  // teste le caractère dans la nouvelle position
      then
        Inc(Pattern)  // si le caractere correspond, on redéverouille.
      else
        p1 := Nil;    // sinon, on reste vérouillé.
    end;
  end;
  // à la fin si tout est ok, les deux chaines sont terminées.
  if (Str^ = #0) And (Pattern^=#0) then result := True;
end;


end.


unit pc1;

// PC1 CIPHER with 128-bit keys //
// (c) Alexander PUKALL 1991 //
//  Freeware. Can be freely use even for commercial //
// purposes //
// This stream cipher algorithm can be used to //
// encrypt texts or binary files //
// Very high security //

interface

uses
	SysUtils, StrUtils,QDialogs,QControls;

var
  si,x1a2,i,k : Word;
  x1a0  : array[0..7] of Word;  // put 15 for 256 bits
  cle   : array[0..15] of char; // put 31 for 256 bits

Procedure Crypt(ThisCle : PChar; var Buffer: PChar; BufferLength: Integer);
Procedure Decrypt(ThisCle :PChar; var Buffer: PChar; BufferLength: Integer);

implementation


function code : Word;
var
  tmp,ax,bx,cx,dx : Word;
begin
  dx:= x1a2+i;
	ax:= x1a0[i];
  cx:= $015a;  // remplacer par un calcul sur le password
  bx:= $4e35;  // remplacer par un calcul sur le password
  tmp:= ax;
	ax:= si;
  si:= tmp;
	tmp:= ax;
	ax:= dx;
	dx:= tmp;
  if (ax <> 0) then ax:= ax*bx;
  tmp:= ax;
  ax:= cx;
  cx:= tmp;
  if (ax <> 0) then
  begin
    ax:= ax*si;
    cx:= ax+cx;
  end;
	tmp:= ax;
  ax:= si;
  si:= tmp;
  ax:= ax*bx;
  dx:= cx+dx;
	ax:= ax+1;
  x1a2:= dx;
	x1a0[i]:= ax;
  result:= ax xor dx;
	i:= i+1;
end;

function Assemble : Word;
var
  i1, i2 : Byte;
  inter : Word;
begin
	x1a0[0]:= ( ord(cle[0])*256 ) + ord(cle[1]);
  inter:= code;
  i2 := 2;
  for i1:=0 To 6  do // put 14 for 256 bits
  begin
    x1a0[i1+1]:= x1a0[i1] xor ( (ord(cle[i2])*256) + ord(cle[i2+1]) );
    inter := inter xor code;
    inc(i2,2);
  end;
	i:= 0;
  result := inter;
end;

Procedure Crypt(ThisCle : PChar; var Buffer: PChar; BufferLength: Integer);
// The buffer contains the message to encrypt. No need to be null-termindated,
// since its length is explicitly specified.
// ThisCle contains the password, 16 characters at max.
var
  c, compte: Byte;
  cfc,cfd,inter,j : word;
begin
  // Some initializations
  StrCopy(Cle, ThisCle);
  si:=0;
  x1a2:=0;
  i:=0;

  for j:=0 to BufferLength-1 do begin
    c:= ord(Buffer[j]);      { c = first byte to crypt}
    inter := Assemble;
    cfc:= inter shr 8;
    cfd:= inter and $FF;
    for compte:= 0 to 15 do // put 31 for 256 bits
      cle[compte]:= chr(ord(cle[compte]) xor c);
    c:= c xor (cfc xor cfd);
    Buffer[j]:=chr(c);
  end;
end;

Procedure Decrypt(ThisCle :PChar; var Buffer: PChar; BufferLength: Integer);
var
  c, compte: Byte;
  cfc,cfd,inter,j : word;
begin
  // Some initializations
  fillchar(Cle, SizeOf(Cle),#0);
  StrCopy(Cle, ThisCle);
  si:= 0; x1a2:=0; i:=0; j:=0;

  while j<BufferLength do begin
    c:= ord(Buffer[j]);
    inter := Assemble;
    cfc:= inter shr 8;
    cfd:= inter and $FF;
    c:= c xor (cfc xor cfd);
    
    for compte:= 0 to 15 do // put 31 for 256 bits
      cle[compte]:= chr(ord(cle[compte]) xor c);

    // Note : c contains the decrypted byte
    Buffer[j] := chr(c);
    j:=j+1;
  end;

end;

end.

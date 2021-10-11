{*
 * inifile2 par alvaro Hermo (alvaro.h@ifrance.com) 30/05/2004
 *
 * amelioration de la classe Tinifile pour la gestion des fichiers INI
 * Sous XP la methode ReadString plante si le paramettre Section est vide.  Sous
 * Linux ou Windows 9x, cette methode retourne le paramettre Default.
 * Utilisez cette classe pour avoir le meme comportement suivant la plateforme.
 *}

unit inifiles2;

interface

uses inifiles; // pour la classe Tinifile

type
Tinifile2=class(Tinifile) // surdefinition de la classe Tinifile
  private
  public
    function ReadString(const Section, Ident, Default: String): String;
      override;  // Surdefinition de la methode ReadString
  end;

implementation

{*
 * Surdefinition de la methode ReadString
 *
 * Sous XP la fonction ReadString plante si Section = ''
 *}
function Tinifile2.ReadString(const Section, Ident, Default: String): String;
var
  s : String;
begin
  if Section = '' then
    s := Default
  else // on peut lancer ReadString de la classe mere sans danger.
    s := inherited ReadString(Section,Ident,Default);
  if s = '' then s := Default;
  result := s;
end;


end.

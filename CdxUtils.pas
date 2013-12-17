unit CdxUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxUtils
Version:    1.0
Purpose:    Set of additional helper functions
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   16.12.2013    Initial version
                    function GetFileVersion()
                    function UnicodeStringReplace()
                    function UTF8Chr()
                    function Split()
                    function SubnetFromIP()
1.0b  17.12.2013    added Github URL
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Windows;

function GetFileVersion(Filename: String; const ShortForm: Boolean = false): String;
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
function UTF8Chr(Unicode: Cardinal): UTF8String;
function Split(Delimiter: Char; Text: String): TStrings;
function SubnetFromIPv4(IP: String): String;

implementation

function GetFileVersion(Filename: String; const ShortForm: Boolean = false): String;
//Retrieve the file version from an EXE file
var
  aFilename: array [0..MAX_PATH] of Char;
  pFileInfo: Pointer;
  pFixFInfo: PVSFixedFileInfo;
  nFixFInfo: DWORD;
  pdwHandle: DWORD;
  nInfoSize: DWORD;

begin
  if ShortForm=true then
    result:='0.0'
  else
    result:='0.0.0.0';
  StrPCopy(aFilename, Filename);
  pdwHandle:=0;
  nInfoSize:=GetFileVersionInfoSize(aFilename, pdwHandle);
  if nInfoSize<>0 then
    pFileInfo:=GetMemory(nInfoSize)
  else
    pFileInfo:=nil;
  if Assigned(pFileInfo) then
    begin
      try
        if GetFileVersionInfo(aFilename, pdwHandle, nInfoSize, pFileInfo) then
          begin
            pFixFInfo:=nil;
            nFixFInfo:=0;
            if VerQueryValue(pFileInfo, '\', Pointer(pFixFInfo), nFixFInfo) then
              begin
                if ShortForm=false then
                  result:=Format('%d.%d.%d.%d',[HiWord(pFixFInfo^.dwFileVersionMS),LoWord(pFixFInfo^.dwFileVersionMS),HiWord(pFixFInfo^.dwFileVersionLS),LoWord(pFixFInfo^.dwFileVersionLS)])
                else
                  result:=Format('%d.%d',[HiWord(pFixFInfo^.dwFileVersionMS),LoWord(pFixFInfo^.dwFileVersionLS)]);
              end;
          end;
      finally
        FreeMemory(pFileInfo);
      end;
    end;
end;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
//Unicode StringReplace() variant based on original SysUtil function
var
  Srch,OldP,RemS: UnicodeString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=WideUpperCase(Srch);
    OldP:=WideUpperCase(OldP);
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

function UTF8Chr(Unicode: Cardinal): UTF8String;
//UTF8 compatible Chr() function
var
  UTF8Char: UTF8String;

begin
  if Unicode>$FFFF then
    Unicode:=$FFFF;
  UTF8Char:=WideChar(Unicode);
  result:=Utf8Encode(UTF8Char);
end;

function Split(Delimiter: Char; Text: String): TStrings;
//Simple String Split function
var
  TextParts: TStringList;

begin
  TextParts:=TStringList.Create;
  TextParts.Clear;
  TextParts.StrictDelimiter:=true;
  TextParts.Delimiter:=Delimiter;
  TextParts.DelimitedText:=Text;
  result:=TextParts;
end;

function SubnetFromIPv4(IP: String): String;
//extract Subnet from a given IPv4 String
begin
  if AnsiPos('.',IP)=0 then
    Result:=''
  else
    Result:=AnsiLeftStr(IP,LastDelimiter('.',IP));
end;

end.


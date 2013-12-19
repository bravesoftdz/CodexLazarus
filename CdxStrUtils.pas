unit CdxStrUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxStrUtils
Version:    1.0
Purpose:    Set of additional String helper functions
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   19.12.2013    Initial version
                    function UnicodeStringReplace() moved from CdxUtils unit
                    function UTF8Chr() moved from CdxUtils unit
                    function Split() moved from CdxUtils unit
                    function SubnetFromIPv4() moved from CdxUtils unit
                    function HexToBinStr() moved from CdxUtils unit
                    function IntToBinStr() moved from CdxUtils unit
                    function SecondsToTimeString() moved from CdxUtils unit
                    function BinStrToHex()
                    function BinStrToInt()

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math;

function BinStrToHex(BinString: String): String;
function BinStrToInt(BinString: String): Integer;
function HexToBinStr(HexString: String): String;
function IntToBinStr(Value: Integer): String;
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
function UTF8Chr(Unicode: Cardinal): UTF8String;
function SecondsToTimeString(Seconds: Integer; SecondsAsMilliseconds: Boolean = false): String;
function Split(Delimiter: Char; Text: String): TStrings;
function SubnetFromIPv4(IP: String): String;


implementation

function BinStrToHex(BinString: String): String;
//Converts a binary String back to a hexadecimal String
begin
  result:=IntToHex(BinStrToInt(BinString),1);
end;

function BinStrToInt(BinString: String): Integer;
//Converts a binary String back to an Integer
var
  Index: Integer;
  Int: Integer;

begin
  Result:=0;
  for Index:=0 To Length(BinString)-1 do //start conversion with MSB (Most Significat Bit = most left Bit)
    begin
      if BinString[Index+1]='1' then
        Result:=Result+Trunc(IntPower(2,(Length(BinString)-Index-1)));
    end;
end;

function HexToBinStr(HexString: String): String;
//Converts a hexadecimal String to a binary String
const
  //String array for the binary value Bits from 0 to 15
  HexBits: array [0..15] of String =
    ('0000', '0001', '0010', '0011',
     '0100', '0101', '0110', '0111',
     '1000', '1001', '1010', '1011',
     '1100', '1101', '1110', '1111');

var
  Index: Integer;

begin
  //remove trailing $ in case of
  if LeftStr(HexString,1)='$' then
    HexString:=RightStr(HexString, Length(HexString)-1);
  //remove trailing # in case of
  if LeftStr(HexString,1)='#' then
    HexString:=RightStr(HexString, Length(HexString)-1);
  //remove trailing 0x in case of
  if LeftStr(HexString,2)='0x' then
    HexString:=RightStr(HexString, Length(HexString)-2);

  //add for every Hexadecimal char the according binary string part
  Result:='';
  for Index:=Length(HexString) DownTo 1 do //start String creation with MSB (Most Significat Bit = most left Bit)
    Result:=HexBits[StrToInt('$'+HexString[Index])]+Result;

  //remove trailing Zero´s
  Index:=0;
  while Index<Length(result) do
    begin
      if result[Index]='1' then
        begin
          result:=RightStr(result,Length(result)-Index+1);
          break;
        end;
      inc(Index);
    end;
end;

function IntToBinStr(Value: Integer): String;
//Converts a decimal String to a binary String
begin
  result:=HexToBinStr(IntToHex(Value,1));
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
  //make sure Unicode does not exceed 2 bytes
  if Unicode>$FFFF then
    Unicode:=$FFFF;
  //do conversion
  UTF8Char:=WideChar(Unicode);
  result:=Utf8Encode(UTF8Char);
end;

function SecondsToTimeString(Seconds: Integer; SecondsAsMilliseconds: Boolean = false): String;
//Converts (milli)seconds numerical value to a time string with format "xx:xx:xx"
var
  Hours, Minutes: Integer;

begin
  try
    //Convert milliseconds to seconds
    if SecondsAsMilliseconds=true then
      Seconds:=round(Seconds/1000);
    //do not allow more seconds than the maximum value of "99:59:59"
    if Seconds>359999 then
      Seconds:=359999;
    //do string conversion
    Hours:=(Seconds div 3600);
    Seconds:=Seconds-(Hours*3600);
    Minutes:=(Seconds div 60);
    Seconds:=Seconds-(Minutes*60);
    result:= FormatFloat('00', Hours)+':'+FormatFloat('00', Minutes)+':'+FormatFloat('00', Seconds);
  except
    //default fallback result
    result:='00:00:00';
  end;
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


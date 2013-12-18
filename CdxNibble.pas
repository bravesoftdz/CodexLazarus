unit CdxNibble;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxNibble
Version:    1.0
Purpose:    Provides functions and types for a Nibble (4 Bit data type)
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   18.12.2013    Initial version

------------------------------------------------------------------------
Additional Technical Information:
------------------------------------------------------------------------

A Nibble is a data type with the size of 4 Bits and a value range from 0..15:
15..................0
 4     3     2     1
2^3 + 2^2 + 2^1 + 2^0

In comparision a regular Byte has the size of 8 bits and a value range from 0..255:
255.........................................0
 8     7     6     5     4     3     2     1
2^7 + 2^6 + 2^5 + 2^4 + 2^3 + 2^2 + 2^1 + 2^0

So a regular Byte (DoubleNibbles) with 8 Bits can contain 2 Nibbles x 4 Bits:
15...................0|15...................0
 4     3     2     1  |  4     3     2     1
2^3 + 2^2 + 2^1 + 2^0 | 2^3 + 2^2 + 2^1 + 2^0

With DoubleNibbles (Byte) it is for e.g. possible to create small "16 x 16" arrays
based on a "High Nibble x Low Nibble" matrix.

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  //define a Nibble as a High and Low part of a regular Byte type
  PNibble = ^Nibble;
  Nibble = record
    HiNibble:  Byte;
    LoNibble:  Byte;
  end;

  //wrapper for a DoubleNibble Byte type but is is possible to use also regular
  //Byte type instead of DblNibble in CdxNibble functions
  DblNibble = Byte;
  PDblNibble = ^DblNibble;

  function BinToDblNibble(EightBitString: ShortString): DblNibble;
  function BinToNibble(FourBitString: ShortString): Byte;
  function DblNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
  function HiNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
  function LoNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
  function NibbleToBinaryStr(NibbleByte: Byte): ShortString;
  function NibbleToDblNibble(NibblesBytes: Nibble): DblNibble;
  function SplitDblNibble(DblNibbleByte: DblNibble): Nibble;

implementation

function BinToDblNibble(EightBitString: ShortString): DblNibble;
//Creates a Double Nibble from a binary String
var
  NewDblNibble: DblNibble;
  BitIndex: Byte;

begin
  //make sure String contains 8 Bits
  if Length(EightBitString)>8 then
     EightBitString:=RightStr(EightBitString,8);
  if Length(EightBitString)<8 then
    EightBitString:=StringOfChar('0',8-Length(EightBitString))+EightBitString;

  //convert 8 Bits from String to a new DblNibble
  NewDblNibble:=0;
  for BitIndex:=0 to 7 do
    begin
      if EightBitString[8-BitIndex]='1' then  //important: binary Strings start from the right side
        NewDblNibble:=NewDblNibble+Trunc((Power(2,BitIndex)));
    end;
  result:=NewDblNibble;
end;

function BinToNibble(FourBitString: ShortString): Byte;
//essentially the same as BinToDblNibble() so just a wrapper
begin
  result:=BinToDblNibble(FourBitString);
end;

function DblNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
//Creates a 8 Bit binary String from a Double Nibble
begin
  result:=HiNibbleToBinaryStr(NibblesBytes)+LoNibbleToBinaryStr(NibblesBytes);
end;

function HiNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
//Creates a 4 Bit binary String from a High Nibble
var
  BinaryStr: ShortString;
  BitIndex: Byte;

begin
  BinaryStr:='';
  for BitIndex:=7 downto 4 do // from the given 8 Bits of a Byte use only the Bits of the High Nibble
    if (NibblesBytes and(1 shl BitIndex))>0 then
      BinaryStr:=BinaryStr+'1'
    else
      BinaryStr:=BinaryStr+'0';
  result:=BinaryStr;
end;

function LoNibbleToBinaryStr(NibblesBytes: DblNibble): ShortString;
//Creates a 4 Bit binary String from a Low Nibble
var
  BinaryStr: ShortString;
  BitIndex: Byte;

begin
  BinaryStr:='';
  for BitIndex:=3 downto 0 do // from the given 8 Bits of a Byte use only the Bits of the low Nibble
    if (NibblesBytes and(1 shl BitIndex))>0 then
      BinaryStr:=BinaryStr+'1'
    else
      BinaryStr:=BinaryStr+'0';
  result:=BinaryStr;
end;

function NibbleToBinaryStr(NibbleByte: Byte): ShortString;
//Creates a 4 Bit binary String from a single Nibble byte
begin
  result:=RightStr(LoNibbleToBinaryStr(NibbleByte),4);
end;

function NibbleToDblNibble(NibblesBytes: Nibble): DblNibble;
//Combines two Nibbles into a Byte
begin
  //make sure value of Nibble Hi/Lo Bytes does not exceed 15 (4 Bits)
  if NibblesBytes.HiNibble>15 then
     NibblesBytes.HiNibble:=15;
  if NibblesBytes.LoNibble>15 then
     NibblesBytes.LoNibble:=15;
  result:=(NibblesBytes.HiNibble << 4)+NibblesBytes.LoNibble;
end;

function SplitDblNibble(DblNibbleByte: DblNibble): Nibble;
//Splits a Byte into two Nibbles
var
  Buffer: Nibble;

begin
  Buffer.HiNibble:=((DblNibbleByte) >> 4) and $0F;
  Buffer.LoNibble:=DblNibbleByte and $0F;
  result:=Buffer;
end;

end.


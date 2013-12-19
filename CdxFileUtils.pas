unit CdxFileUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxFileUtils
Version:    1.0
Purpose:    Set of additional file and filesystem helper functions
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   19.12.2013    Initial version
                    function IsDriveFat() moved from CdxUtils unit
                    function IsDriveNTFS() moved from CdxUtils unit
                    function GetDriveFormat() moved from CdxUtils unit
                    function GetFileSize() moved from CdxUtils unit
                    function GetFileSize() moved from CdxUtils unit

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

function GetDriveFormat(const DriveLetter: Char = 'C'): String;
function GetFileSize(FileName: String): Int64;
function GetFileVersion(Filename: String; const ShortForm: Boolean = false): String;
function IsDriveFAT(const DriveLetter: Char = 'C'): Boolean;
function IsDriveNTFS(const DriveLetter: Char = 'C'): Boolean;


implementation

function GetDriveFormat(const DriveLetter: Char = 'C'): String;
//Get filesystem format
var
  lpVolumeNameBuffer: PChar;
  lpVolumeSerialNumber: PDWORD;
  lpFileSystemNameBuffer: PChar;
  lpFileSystemFlags: DWORD;
  lpMaximumComponentLength: DWORD;

begin
  //default return value
  result:='';
  try
    //reserve memory for variables
    GetMem(lpVolumeNameBuffer, MAX_PATH);
    GetMem(lpVolumeSerialNumber, MAX_PATH);
    GetMem(lpFileSystemNameBuffer, MAX_PATH);
    //read volume information
    GetVolumeInformation(PChar(DriveLetter+':\'), lpVolumeNameBuffer, MAX_PATH, lpVolumeSerialNumber, lpMaximumComponentLength, lpFileSystemFlags, lpFileSystemNameBuffer, MAX_PATH);
    //return filesystem name
    result:=StrPas(lpFileSystemNameBuffer);
  finally
    //free up reserved memory
    FreeMem(lpVolumeNameBuffer, MAX_PATH);
    FreeMem(lpVolumeSerialNumber, MAX_PATH);
    FreeMem(lpFileSystemNameBuffer, MAX_PATH);
  end;
end;

function GetFileSize(FileName: String): Int64;
//Get size of a file in Bytes
var
  FileStream: TFileStream;

begin
  //default return value
  result:=0;
  //check if file does exist
  if FileExists(FileName)=false then
    exit;
  //create a filestream and read its size
  try
    FileStream:=TFileStream.Create(FileName,fmOpenRead OR fmShareDenyWrite);
    result:=FileStream.Size;
  finally
    FileStream.Free;
  end;
end;

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
  //create default results
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
        //read file version info from binary
        if GetFileVersionInfo(aFilename, pdwHandle, nInfoSize, pFileInfo) then
          begin
            pFixFInfo:=nil;
            nFixFInfo:=0;
            if VerQueryValue(pFileInfo, '\', Pointer(pFixFInfo), nFixFInfo) then
              begin
                if ShortForm=false then
                  //Create a full version string with MAIN.SUBVERSION.REVISION.BUILD
                  result:=Format('%d.%d.%d.%d',[HiWord(pFixFInfo^.dwFileVersionMS),LoWord(pFixFInfo^.dwFileVersionMS),HiWord(pFixFInfo^.dwFileVersionLS),LoWord(pFixFInfo^.dwFileVersionLS)])
                else
                  //Create a short form of the version string with MAIN.BUILD
                  result:=Format('%d.%d',[HiWord(pFixFInfo^.dwFileVersionMS),LoWord(pFixFInfo^.dwFileVersionLS)]);
              end;
          end;
      finally
        FreeMemory(pFileInfo);
      end;
    end;
end;

function IsDriveFAT(const DriveLetter: Char = 'C'): Boolean;
//Check if a drive partition is FAT/FAT32 formatted
begin
  if Pos('FAT',GetDriveFormat(DriveLetter))>0 then
    result:=true
  else
    result:=false;
end;

function IsDriveNTFS(const DriveLetter: Char = 'C'): Boolean;
//Check if a drive partition is NTFS formatted
begin
  if Pos('NTFS',GetDriveFormat(DriveLetter))>0 then
    result:=true
  else
    result:=false;
end;

end.


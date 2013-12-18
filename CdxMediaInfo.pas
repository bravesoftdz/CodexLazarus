unit CdxMediaInfo;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxMediaInfo
Version:    1.1
Purpose:    Header for dynamic loading of functions from the MediaInfo DLL
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   11.03.2008    Initial version written in D7
1.1   18.12.2013    Rework as FPC/Lazarus version

------------------------------------------------------------------------
Additional Technical Information:
------------------------------------------------------------------------

MediaInfo is a convenient unified display of the most relevant technical
and tag data for video and audio files. The "MediaInfo.dll" is the
library DLL of this project.
This header is adapted for the Version "v0.7.65" of this library DLL,
but should work in general also with newer (and probably older) versions.

Homepage:
http://mediaarea.net/de/MediaInfo   (german site)
http://mediaarea.net/en/MediaInfo   (english site)

}
//enables in FPC/Lazarus Delphi compatibility which makes porting easier since original code was written in Delphi
{$ifdef fpc}
{$mode delphi}
{$endif}   

interface

uses
  Windows, SysUtils;

type
  {$warnings off}
  TMediaInfo_New=function(): LongWord; cdecl stdcall;
  TMediaInfo_Delete=procedure(Handle: Cardinal) cdecl stdcall;
  TMediaInfo_Open=function(Handle: Cardinal; File__: PWideChar): Cardinal cdecl stdcall;
  TMediaInfo_Close=procedure(Handle: Cardinal) cdecl stdcall;
  TMediaInfo_Inform=function (Handle: Cardinal; Reserved: Integer): PWideChar cdecl stdcall;
  TMediaInfo_GetI=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer; Parameter: Integer; KindOfInfo: Integer): PWideChar cdecl stdcall;
  TMediaInfo_Get=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer; Parameter: PWideChar; KindOfInfo: Integer; KindOfSearch: Integer): PWideChar cdecl stdcall;
  TMediaInfo_Option=function (Handle: Cardinal; Option: PWideChar; Value: PWideChar): PWideChar cdecl stdcall;
  TMediaInfo_State_Get=function (Handle: Cardinal): Integer cdecl stdcall;
  TMediaInfo_Count_Get=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer): Integer cdecl stdcall;
  TMediaInfoA_New=function (): Cardinal cdecl stdcall;
  TMediaInfoA_Delete=procedure (Handle: Cardinal) cdecl stdcall;
  TMediaInfoA_Open=function (Handle: Cardinal; File__: PChar): Cardinal cdecl stdcall;
  TMediaInfoA_Close=procedure (Handle: Cardinal) cdecl stdcall;
  TMediaInfoA_Inform=function (Handle: Cardinal; Reserved: Integer): PChar cdecl stdcall;
  TMediaInfoA_GetI=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer; Parameter: Integer; KindOfInfo: Integer): PChar cdecl stdcall;
  TMediaInfoA_Get=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer; Parameter: PChar; KindOfInfo: Integer; KindOfSearch: Integer): PChar cdecl stdcall;
  TMediaInfoA_Option=function (Handle: Cardinal; Option: PChar; Value: PChar): PChar cdecl stdcall;
  TMediaInfoA_State_Get=function (Handle: Cardinal): Integer cdecl stdcall;
  TMediaInfoA_Count_Get=function (Handle: Cardinal; StreamKind: Integer; StreamNumber: Integer): Integer cdecl stdcall;
  {$warnings on}

  function MilliSecondsToString(MilliSeconds: Int64): string;

  function MediaInfoDLLVersion(): String;
  function MediaInfoFileSize(FilePath: String): Int64;
  function MediaInfoFileFormat(FilePath: String): String;
  function MediaInfoFileCodec(FilePath: String): String;
  function MediaInfoVideoCount(FilePath: String): String;
  function MediaInfoFileVideoWidth(FilePath: String): Integer;
  function MediaInfoFileVideoHeight(FilePath: String): Integer;
  function MediaInfoFileVideoAspect(FilePath: String): String;
  function MediaInfoFileBitrate(FilePath: String): Integer;
  function MediaInfoFilePlayTime(FilePath: String): Int64;

var
  MediaFile: LongWord;
  PMediaFile: PWideChar;
  StreamKind: Integer;
  MediaInfoString: WideString;

  MediaInfo_DLLHandle: THandle;
  MediaInfo_DLL_OK: Boolean;
  MediaInfo_New: TMediaInfo_New;
  MediaInfo_Delete: TMediaInfo_Delete;
  MediaInfo_Open: TMediaInfo_Open;
  MediaInfo_Close: TMediaInfo_Close;
  MediaInfo_Inform: TMediaInfo_Inform;
  MediaInfo_GetI: TMediaInfo_GetI;
  MediaInfo_Get: TMediaInfo_Get;
  MediaInfo_Option: TMediaInfo_Option;
  MediaInfo_State_Get: TMediaInfo_State_Get;
  MediaInfo_Count_Get: TMediaInfo_Count_Get;
  MediaInfoA_New: TMediaInfoA_New;
  MediaInfoA_Delete: TMediaInfoA_Delete;
  MediaInfoA_Open: TMediaInfoA_Open;
  MediaInfoA_Close: TMediaInfoA_Close;
  MediaInfoA_Inform: TMediaInfoA_Inform;
  MediaInfoA_GetI: TMediaInfoA_GetI;
  MediaInfoA_Get: TMediaInfoA_Get;
  MediaInfoA_Option: TMediaInfoA_Option;
  MediaInfoA_State_Get: TMediaInfoA_State_Get;
  MediaInfoA_Count_Get: TMediaInfoA_Count_Get;

Const
  Stream_General: Cardinal = 0;
  Stream_Video: Cardinal = 1;
  Stream_Audio: Cardinal = 2;
  Stream_Text: Cardinal = 3;
  Stream_Other: Cardinal = 4;
  Stream_Image: Cardinal = 5;
  Stream_Menu: Cardinal = 6;
  Stream_Max: Cardinal = 7;
  Info_Name: Cardinal = 0;
  Info_Text: Cardinal = 1;
  Info_Measure: Cardinal = 2;
  Info_Options: Cardinal = 3;
  Info_Name_Text: Cardinal = 4;
  Info_Measure_Text: Cardinal = 5;
  Info_Info: Cardinal = 6;
  Info_HowTo: Cardinal = 7;
  Info_Max: Cardinal = 8;
  InfoOption_ShowInInform: Cardinal = 0;
  InfoOption_Reserved: Cardinal = 1;
  InfoOption_ShowInSupported: Cardinal = 2;
  InfoOption_TypeOfValue: Cardinal = 3;
  InfoOption_Max: Cardinal = 4;


implementation

function MilliSecondsToString(MilliSeconds: Int64): string;
//Milliseconds to String
var
  Hours: Integer;
  Minutes: Integer;
  Seconds: Integer;

begin
  try
    Seconds:=MilliSeconds div 1000;
    Hours:=(Seconds div 3600);
    Seconds:=Seconds-(Hours*3600);
    Minutes:=(Seconds div 60);
    Seconds:=Seconds-(Minutes*60);
    MilliSecondsToString:= FormatFloat('00', Hours)+':'+FormatFloat('00', Minutes)+':'+FormatFloat('00', Seconds);
  except
    MilliSecondsToString:= '00:00:00';
  end;
end;

function MediaInfoDLLVersion(): String;
//MediaInfo DLL Version
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:='';
      exit;
    end;
  try
   MediaInfoString:=MediaInfo_Option(0, 'Info_Version', '');
   Result:=MediaInfoString;
  except
    Result:='';
  end;
end;

function MediaInfoFileSize(FilePath: String): Int64;
//Media File Filesize
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:=0;
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 0, 0, 'FileSize', 1, 0);
  Result:=StrToInt64Def(MediaInfoString,0);
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:=0;
  end;
end;

function MediaInfoFileFormat(FilePath: String): String;
//Media File Format
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:='';
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, Stream_General, 0, 'Format', 1, 0);
  Result:=MediaInfoString;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:='';
  end;
end;

function MediaInfoFileCodec(FilePath: String): String;
//Media File Codec
var
  Buffer: String;

begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:='';
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  Buffer:=MediaInfo_Get(MediaFile, Stream_Video, 0, 'Format', 1, 0);
  if Buffer<>'' then
    MediaInfoString:=Buffer;
  Buffer:=MediaInfo_Get(MediaFile, Stream_Audio, 0, 'Format', 1, 0);
  if (Buffer<>'') and (Buffer<>MediaInfoString) then
    MediaInfoString:=MediaInfoString+' / '+Buffer;
  Result:=MediaInfoString;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:='';
  end;
end;

function MediaInfoFilePlayTime(FilePath: String): Int64;
//Media File Playtime
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:=0;
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 0, 0, 'PlayTime', 1, 0);
  Result:=StrToInt64Def(MediaInfoString,0);
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:=0;
  end;
end;

function MediaInfoVideoCount(FilePath: String): String;
//Media File Video Track available
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:='';
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, Stream_General, 0, 'VideoCount', 1, 0);
  Result:=MediaInfoString;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:='';
  end;
end;

function MediaInfoFileVideoWidth(FilePath: String): Integer;
//Media File Video Width
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:=0;
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 1, 0, 'Width', 1, 0);
  if MediaInfoString<>'' then
    Result:=StrToInt(MediaInfoString)
  else
    Result:=0;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:=0;
  end;
end;

function MediaInfoFileVideoHeight(FilePath: String): Integer;
//Media File Video Width
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:=0;
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 1, 0, 'Height', 1, 0);
  if MediaInfoString<>'' then
    Result:=StrToInt(MediaInfoString)
  else
    Result:=0;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:=0;
  end;
end;

function MediaInfoFileVideoAspect(FilePath: String): String;
//Media File Video Aspect
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:='';
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 1, 0, 'AspectRatio/String', 1, 0);
  Result:=MediaInfoString;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:='';
  end;
end;

function MediaInfoFileBitrate(FilePath: String): Integer;
//Media File Bitrate
begin
  if MediaInfo_DLL_OK=false then
    begin
      Result:=0;
      exit;
    end;
  try
  MediaFile:=MediaInfo_New;
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  MediaInfo_Open(MediaFile,PMediaFile);
  MediaInfoString:=MediaInfo_Get(MediaFile, 0, 0, 'BitRate', 1, 0);
  if MediaInfoString<>'' then
      Result:=StrToInt(MediaInfoString)
  else
    begin
      MediaInfoString:=MediaInfo_Get(MediaFile, 1, 0, 'BitRate', 1, 0);
      if MediaInfoString<>'' then
        Result:=StrToInt(MediaInfoString)
      else
        begin
          MediaInfoString:=MediaInfo_Get(MediaFile, 2, 0, 'BitRate', 1, 0);
          if MediaInfoString<>'' then
            Result:=StrToInt(MediaInfoString)
          else
            Result:=0;
        end;
    end;
  MediaInfo_Close(MediaFile);
  MediaInfo_Delete(MediaFile);
  except
  Result:=0;
  end;
end;

initialization
begin
  MediaInfo_DLL_OK:=false;
  try
  //DLL laden und Funktionen initialisieren
  if (MediaInfo_DLLHandle=0) and (FileExists('MediaInfo.dll')=true) then
    begin
      MediaInfo_DLLHandle:=LoadLibrary('MediaInfo.dll');
      if MediaInfo_DLLHandle<>0 then
        begin
          //MediaInfo_New
          MediaInfo_New:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_New');
          if not Assigned(MediaInfo_New) then Exit;
          //MediaInfo_Delete
          MediaInfo_Delete:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Delete');
          if not Assigned(MediaInfo_Delete) then Exit;
          //MediaInfo_Open
          MediaInfo_Open:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Open');
          if not Assigned(MediaInfo_Open) then Exit;
          //TMediaInfo_Close
          MediaInfo_Close:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Close');
          if not Assigned(MediaInfo_Close) then Exit;
          //MediaInfo_Inform
          MediaInfo_Inform:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Inform');
          if not Assigned(MediaInfo_Inform) then Exit;
          //MediaInfo_GetI
          MediaInfo_GetI:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_GetI');
          if not Assigned(MediaInfo_GetI) then Exit;
          //MediaInfo_Get
          MediaInfo_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Get');
          if not Assigned(MediaInfo_Get) then Exit;
          //MediaInfo_Option
          MediaInfo_Option:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Option');
          if not Assigned(MediaInfo_Option) then Exit;
          //MediaInfo_State_Get
          MediaInfo_State_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_State_Get');
          if not Assigned(MediaInfo_State_Get) then Exit;
          //MediaInfo_Count_Get
          MediaInfo_Count_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfo_Count_Get');
          if not Assigned(MediaInfo_Count_Get) then Exit;
          //TMediaInfoA_New
          MediaInfoA_New:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_New');
          if not Assigned(MediaInfoA_New) then Exit;
          //MediaInfoA_Delete
          MediaInfoA_Delete:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Delete');
          if not Assigned(MediaInfoA_Delete) then Exit;
          //MediaInfoA_Open
          MediaInfoA_Open:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Open');
          if not Assigned(MediaInfoA_Open) then Exit;
          //MediaInfoA_Close
          MediaInfoA_Close:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Close');
          if not Assigned(MediaInfoA_Close) then Exit;
          //MediaInfoA_Inform
          MediaInfoA_Inform:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Inform');
          if not Assigned(MediaInfoA_Inform) then Exit;
          //MediaInfoA_GetI
          MediaInfoA_GetI:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_GetI');
          if not Assigned(MediaInfoA_GetI) then Exit;
          //MediaInfoA_Get
          MediaInfoA_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Get');
          if not Assigned(MediaInfoA_Get) then Exit;
          //MediaInfoA_Option
          MediaInfoA_Option:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Option');
          if not Assigned(MediaInfoA_Option) then Exit;
          //MediaInfoA_State_Get
          MediaInfoA_State_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_State_Get');
          if not Assigned(MediaInfoA_State_Get) then Exit;
          //MediaInfoA_Count_Get
          MediaInfoA_Count_Get:=GetProcAddress(MediaInfo_DLLHandle,'MediaInfoA_Count_Get');
          if not Assigned(MediaInfoA_Count_Get) then Exit;
          //alle DLL Funktion ohne Fehler geladen
          MediaInfo_DLL_OK:=true;
        end;
    end;
    except
    MediaInfo_DLL_OK:=false;
    end;
end;

finalization
begin
  //DLL freigeben
  if MediaInfo_DLLHandle<>0 then
    begin
      FreeLibrary(MediaInfo_DLLHandle);
      MediaInfo_DLLHandle:=0;
    end;
end;

end.

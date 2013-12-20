unit CdxMediaInfo;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxMediaInfo
Version:    1.2
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
1.2   20.12.2013    again a complete rework based on older source
                    function DLLVersion()
                    function FileFormat()
                    function FileFormatInfo()
                    function FileSize()
                    function FilePlayTime()
                    function StreamCount()
                    function VideoCount()
                    function AudioCount()
                    function FileCodecs()
                    function FileVideoCodecs()
                    function FileAudioCodecs()
                    function FileFirstVideoCodec()
                    function FileFirstAudioCodec()
                    function MilliSecondsToString() removed
                      -> use SecondsToTimeString() from CdxStrUtils unit instead
                    function FileVideoWidth()
                    function FileVideoHeight()
                    function StreamBitRateString()
                    function VideoBitRateString()
                    function AudioBitRateString()

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
  SysUtils, Classes, Windows;

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

  procedure LoadDLL;
  procedure UnloadDLL;

  function DLLVersion: String;
  function FileFormat(FilePath: String): String;
  function FileFormatInfo(FilePath: String): String;
  function FileSize(FilePath: String): Int64;
  function FilePlayTime(FilePath: String): Int64;
  function StreamCount(FilePath: String): Integer;
  function VideoCount(FilePath: String): Integer;
  function AudioCount(FilePath: String): Integer;
  function FileCodecs(FilePath: String): TStrings;
  function FileVideoCodecs(FilePath: String): TStrings;
  function FileAudioCodecs(FilePath: String): TStrings;
  function FileFirstVideoCodec(FilePath: String): String;
  function FileFirstAudioCodec(FilePath: String): String;
  function VideoWidth(FilePath: String; const StreamNumber: Integer = 0): Integer;
  function VideoHeight(FilePath: String; const StreamNumber: Integer = 0): Integer;
  function StreamBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
  function VideoBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
  function AudioBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;

var
  MediaFile: LongWord;
  PMediaFile: PWideChar;
  StreamKind: Integer;
  MediaInfo_DLLHandle: THandle = 0;
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

const
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
  InformOption_Nothing: Cardinal = 0;


implementation

function OpenMediaFile(FilePath: String): Boolean;
//Open the file which shall be examined
begin
  GetMem(PMediaFile, 512);
  PMediaFile:=StringToWideChar(FilePath, PMediaFile, 256);
  if MediaInfo_Open(MediaFile,PMediaFile)=1 then
    result:=true
  else
    result:=false;
end;

function DLLVersion: String;
//MediaInfo DLL version
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  result:=MediaInfo_Option(0, 'Info_Version', '');
  except
  result:='';
  end;
end;

function FileFormat(FilePath: String): String;
//Media file general stream format
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read general stream format
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_General, 0, 'Format', 1, 0);
  except
  result:='';
  end;
end;

function FileFormatInfo(FilePath: String): String;
//Info about stream format
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read general stream format info
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_General, 0, 'Format/Info', 1, 0);
  except
  result:='';
  end;
end;

function FileSize(FilePath: String): Int64;
//Media filesize
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read filesize
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, 0, 0, 'FileSize', 1, 0),0);
  except
  Result:=0;
  end;
end;

function FilePlayTime(FilePath: String): Int64;
//Media file duration
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read duration from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, 0, 0, 'PlayTime', 1, 0),0);
  except
  Result:=0;
  end;
end;

function StreamCount(FilePath: String): Integer;
//Media file stream count
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read stream count from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, Stream_General, 0, 'StreamCount', 1, 0),0);
  except
  Result:=0;
  end;
end;

function VideoCount(FilePath: String): Integer;
//Media file video stream count
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read video stream count from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, Stream_General, 0, 'VideoCount', 1, 0),0);
  except
  Result:=0;
  end;
end;

function AudioCount(FilePath: String): Integer;
//Media file audio stream count
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read audio stream count from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, Stream_General, 0, 'AudioCount', 1, 0),0);
  except
  Result:=0;
  end;
end;

function FileCodecs(FilePath: String): TStrings;
//Media file codecs
var
  Count: Integer;
  VideoStreamCount: Integer;
  AudioStreamCount: Integer;

begin
  //empty list as default return value
  result:=TStringList.Create;
  result.Clear;

  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;

  try
  //get stream counts
  VideoStreamCount:=VideoCount(FilePath);
  AudioStreamCount:=AudioCount(FilePath);

  //list video codecs at first followed by all used audio codecs
  if OpenMediaFile(FilePath)=true then
    begin
      //list all video codecs
      for Count:=0 to VideoStreamCount-1 do
        result.Add(MediaInfo_Get(MediaFile, Stream_Video, Count, 'Format', 1, 0));
      //list all audio codecs
      for Count:=0 to AudioStreamCount-1 do
        result.Add(MediaInfo_Get(MediaFile, Stream_Audio, Count, 'Format', 1, 0));
    end;
  except
  result.clear;
  end;
end;

function FileVideoCodecs(FilePath: String): TStrings;
//Media file video codecs
var
  Count: Integer;
  VideoStreamCount: Integer;

begin
  //empty list as default return value
  result:=TStringList.Create;
  result.Clear;

  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;

  try
  //get video stream count
  VideoStreamCount:=VideoCount(FilePath);

  //list video codecs
  if OpenMediaFile(FilePath)=true then
    begin
      //list all video codecs
      for Count:=0 to VideoStreamCount-1 do
        result.Add(MediaInfo_Get(MediaFile, Stream_Video, Count, 'Format', 1, 0));
    end;
  except
  result.clear;
  end;
end;

function FileAudioCodecs(FilePath: String): TStrings;
//Media file audio codecs
var
  Count: Integer;
  AudioStreamCount: Integer;

begin
  //empty list as default return value
  result:=TStringList.Create;
  result.Clear;

  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;

  try
  //get audio stream count
  AudioStreamCount:=AudioCount(FilePath);

  //list audio codecs
  if OpenMediaFile(FilePath)=true then
    begin
      //list all audio codecs
      for Count:=0 to AudioStreamCount-1 do
        result.Add(MediaInfo_Get(MediaFile, Stream_Audio, Count, 'Format', 1, 0));
    end;
  except
  result.clear;
  end;
end;

function FileFirstVideoCodec(FilePath: String): String;
//Media file first available video codec
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;
  try
  //list first video codec
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_Video, 0, 'Format', 1, 0);
  except
  result:='';
  end;
end;

function FileFirstAudioCodec(FilePath: String): String;
//Media file first available audio codec
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;
  try
  //list first audio codec
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_Audio, 0, 'Format', 1, 0);
  except
  result:='';
  end;
end;

function VideoWidth(FilePath: String; const StreamNumber: Integer = 0): Integer;
//Media file video width
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read video width from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, Stream_Video, StreamNumber, 'Width', 1, 0),0);
  except
  Result:=0;
  end;
end;

function VideoHeight(FilePath: String; const StreamNumber: Integer = 0): Integer;
//Media file video height
begin
  Result:=0;
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read video width from file
  if OpenMediaFile(FilePath)=true then
    result:=StrToInt64Def(MediaInfo_Get(MediaFile, Stream_Video, StreamNumber, 'Height', 1, 0),0);
  except
  Result:=0;
  end;
end;

function StreamBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//General stream bitrate
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read general stream bitrate
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_General, StreamNumber, 'BitRate/String', 1, 0);
  except
  result:='';
  end;
end;

function VideoBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//Video stream bitrate
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read video stream bitrate
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_Video, StreamNumber, 'BitRate/String', 1, 0);
  except
  result:='';
  end;
end;

function AudioBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//Audio stream bitrate
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  //read audio stream bitrate
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, Stream_Audio, StreamNumber, 'BitRate/String', 1, 0);
  except
  result:='';
  end;
end;

procedure LoadDLL;
//Load DLL and initialize functions
begin
  MediaInfo_DLL_OK:=false;
  try
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
          //MediaInfoLib tries to connect to an Internet server for availability of newer software,
          //anonymous statistics and retrieving information about a file (Later... To be done)
          //If for some reasons you don't want this connection, deactivate it.
          //http://mediaarea.net/en/MediaInfo/Support/SDK/Quick_Start#Init
          MediaInfo_Option(MediaInfo_DLLHandle,'Internet','No');
          //Handle
          MediaFile:=MediaInfo_New;
          //all DLL functions loaded without error
          MediaInfo_DLL_OK:=true;
        end;
    end;
    except
    MediaInfo_DLL_OK:=false;
    end;
end;

procedure UnloadDLL;
//Free DLL
begin
if MediaInfo_DLLHandle<>0 then
  begin
    MediaInfo_Delete(MediaFile);
    FreeLibrary(MediaInfo_DLLHandle);
    MediaInfo_DLLHandle:=0;
  end;
end;

initialization
//Unit initalization
begin
  //initialize DLL with unit
  LoadDLL;
end;

finalization
//Unit finalization
begin
  //Unload and free DLL
  UnloadDLL;
end;

end.

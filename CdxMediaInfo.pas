unit CdxMediaInfo;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxMediaInfo
Version:    1.3
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
                    function DisplayAspectRatioString()
                    function PixelAspectRatioString()
                    function VideoFrameRate()
                    function AudioSamplingRateString()
1.3   21.12.2013    function PictureFormat()
                    function PictureWidth()
                    function PictureHeight()
                    function MIMEType()
                    function DLLSupportedCodecs()
                    function MediaInfoGet()
                    function MediaInfoGetInt()
                    function MediaInfoGetInt64()
                    all functions cleaned up


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
  function DLLSupportedCodecs: String;
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
  function DisplayAspectRatioString(FilePath: String; const StreamNumber: Integer = 0): String;
  function PixelAspectRatioString(FilePath: String; const StreamNumber: Integer = 0): String;
  function VideoFrameRate(FilePath: String; const StreamNumber: Integer = 0): String;
  function AudioSamplingRateString(FilePath: String; const StreamNumber: Integer = 0): String;
  function PictureFormat(FilePath: String): String;
  function PictureWidth(FilePath: String): Integer;
  function PictureHeight(FilePath: String): Integer;
  function MIMEType(FilePath: String): String;
  function MediaInfoGet(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): String;
  function MediaInfoGetInt(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): Integer;
  function MediaInfoGetInt64(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): Int64;

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
    Stream_General:   Cardinal = 0;
    Stream_Video:     Cardinal = 1;
    Stream_Audio:     Cardinal = 2;
    Stream_Text:      Cardinal = 3;
    Stream_Other:     Cardinal = 4;
    Stream_Image:     Cardinal = 5;
    Stream_Menu:      Cardinal = 6;
    Stream_Max:       Cardinal = 7;
    Info_Name:        Cardinal = 0;
    Info_Text:        Cardinal = 1;
    Info_Measure:     Cardinal = 2;
    Info_Options:     Cardinal = 3;
    Info_Name_Text:   Cardinal = 4;
    Info_Measure_Text:Cardinal = 5;
    Info_Info:        Cardinal = 6;
    Info_HowTo:       Cardinal = 7;
    Info_Max:         Cardinal = 8;
    InfoOption_ShowInInform:    Cardinal = 0;
    InfoOption_Reserved:        Cardinal = 1;
    InfoOption_ShowInSupported: Cardinal = 2;
    InfoOption_TypeOfValue:     Cardinal = 3;
    InfoOption_Max:             Cardinal = 4;
    InformOption_Nothing:       Cardinal = 0;

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

function MediaInfoGet(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): String;
//General MediaInfo_Get() wrapper
var
  PParameter: PWideChar;

begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
      exit;
  try
  //get parameter
  GetMem(PParameter, 512);
  PParameter:=StringToWideChar(Parameter, PParameter, 256);
  if OpenMediaFile(FilePath)=true then
    result:=MediaInfo_Get(MediaFile, StreamKind, StreamNumber, PParameter, 1, 0);
  except
  result:='';
  end;
end;

function MediaInfoGetInt(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): Integer;
//General MediaInfo_Get() wrapper
begin
  result:=StrToIntDef(MediaInfoGet(FilePath, StreamKind, StreamNumber, Parameter),0);
end;

function MediaInfoGetInt64(FilePath: String; StreamKind: Integer; StreamNumber: Integer; Parameter: String): Int64;
//General MediaInfo_Get() wrapper
begin
  result:=StrToInt64Def(MediaInfoGet(FilePath, StreamKind, StreamNumber, Parameter),0);
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

function DLLSupportedCodecs: String;
//MediaInfo DLL supported codecs
begin
  result:='';
  //check if DLL is loaded
  if MediaInfo_DLL_OK=false then
    exit;
  try
  result:=MediaInfo_Option(0, 'Info_Codecs', '');
  except
  result:='';
  end;
end;

function FileFormat(FilePath: String): String;
//Info about general stream
begin
  result:=MediaInfoGet(FilePath, Stream_General, 0, 'Format');
end;

function FileFormatInfo(FilePath: String): String;
//Info about stream format
begin
  result:=MediaInfoGet(FilePath, Stream_General, 0, 'Format/Info');
end;

function FileSize(FilePath: String): Int64;
//Filesize
begin
  result:=MediaInfoGetInt64(FilePath, Stream_General, 0, 'FileSize');
end;

function FilePlayTime(FilePath: String): Int64;
//Media file duration
begin
  result:=MediaInfoGetInt64(FilePath, Stream_General, 0, 'PlayTime');
end;

function StreamCount(FilePath: String): Integer;
//Media file stream count
begin
  result:=MediaInfoGetInt(FilePath, Stream_General, 0, 'StreamCount');
end;

function VideoCount(FilePath: String): Integer;
//Media file video stream count
begin
  result:=MediaInfoGetInt(FilePath, Stream_General, 0, 'VideoCount');
end;

function AudioCount(FilePath: String): Integer;
//Media file audio stream count
begin
  result:=MediaInfoGetInt(FilePath, Stream_General, 0, 'AudioCount');
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
//Codec of the first available video stream
begin
  result:=MediaInfoGet(FilePath, Stream_Video, 0, 'Format');
end;

function FileFirstAudioCodec(FilePath: String): String;
//Codec of the first available audio stream
begin
  result:=MediaInfoGet(FilePath, Stream_Audio, 0, 'Format');
end;

function VideoWidth(FilePath: String; const StreamNumber: Integer = 0): Integer;
//Video width
begin
  result:=MediaInfoGetInt(FilePath, Stream_Video, StreamNumber, 'Width');
end;

function VideoHeight(FilePath: String; const StreamNumber: Integer = 0): Integer;
//Video height
begin
  result:=MediaInfoGetInt(FilePath, Stream_Video, StreamNumber, 'Height');
end;

function StreamBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//General stream bitrate
begin
  result:=MediaInfoGet(FilePath, Stream_General, StreamNumber, 'BitRate/String');
end;

function VideoBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//Video stream bitrate
begin
  result:=MediaInfoGet(FilePath, Stream_Video, StreamNumber, 'BitRate/String');
end;

function AudioBitRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//Audio stream bitrate
begin
  result:=MediaInfoGet(FilePath, Stream_Audio, StreamNumber, 'BitRate/String');
end;

function DisplayAspectRatioString(FilePath: String; const StreamNumber: Integer = 0): String;
//Video display aspect ratio
begin
  result:=MediaInfoGet(FilePath, Stream_Video, StreamNumber, 'DisplayAspectRatio/String');
end;

function PixelAspectRatioString(FilePath: String; const StreamNumber: Integer = 0): String;
//Pixel aspect ratio
begin
  result:=MediaInfoGet(FilePath, Stream_Video, StreamNumber, 'PixelAspectRatio/String');
end;

function VideoFrameRate(FilePath: String; const StreamNumber: Integer = 0): String;
//Video frame rate
begin
  result:=MediaInfoGet(FilePath, Stream_Video, StreamNumber, 'FrameRate/String');
end;

function AudioSamplingRateString(FilePath: String; const StreamNumber: Integer = 0): String;
//Audio stream sampling rate
begin
  result:=MediaInfoGet(FilePath, Stream_Audio, StreamNumber, 'SamplingRate/String');
end;

function PictureFormat(FilePath: String): String;
//Picture file format
begin
  result:=MediaInfoGet(FilePath, Stream_General, 0, 'Format');
end;

function PictureWidth(FilePath: String): Integer;
//Picture width
begin
  result:=MediaInfoGetInt(FilePath, Stream_Image, 0, 'Width');
end;

function PictureHeight(FilePath: String): Integer;
//Picture height
begin
  result:=MediaInfoGetInt(FilePath, Stream_Image, 0, 'Height');
end;

function MIMEType(FilePath: String): String;
//File MIME type
begin
  result:=MediaInfoGet(FilePath, Stream_General, 0, 'InternetMediaType');
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

unit CdxWinamp;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxWinamp
Version:    1.1
Purpose:    Remote access to Winamp for playback of files and URLs
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   08.11.2008    Initial version written in D7
1.1   19.12.2013    Rework as FPC/Lazarus version
                    removed additional functionality which was intended for use in another project
                    function WinampSearchAndPlay()

}

interface
  procedure WinampClose;
  function WinampNextTitle: Boolean;
  function WinampPause: Boolean;
  function WinampPlay(WinampPath: String; FileOrURL: String): Boolean;
  function WinampPreviousTitle: Boolean;
  function WinampSearchAndPlay(FileOrURL: String; const ShowSearchProgress: Boolean = true; const SearchDriveLetter: Char = 'C'): Boolean;
  function WinampStop: Boolean;
  function WinampVolumeUp: Boolean;
  function WinampVolumeDown: Boolean;

const
  WINAMP_BUTTON1   : integer = 40044; // previous title
  WINAMP_BUTTON2   : integer = 40045; // play
  WINAMP_BUTTON3   : integer = 40046; // pause
  WINAMP_BUTTON4   : integer = 40047; // stop
  WINAMP_BUTTON5   : integer = 40048; // next title
  WINAMP_VOLUMEUP  : integer = 40058; // volume up
  WINAMP_VOLUMEDOWN: integer = 40059; // volume down

var
  WinampHandle : THandle;

implementation

Uses
  Windows, Messages, ShellApi, SysUtils, CdxFileFinder;

function WinampCommand(Command: Integer): Boolean;
//send a remote command to the Winamp window
begin
  //find Winamp window handle
  WinampHandle:=FindWindow('Winamp v1.x',nil);
  if WinampHandle=0 then
    begin
      result:=false;
      exit;
    end
  else
    begin
      //send command to Winamp window handle
      if Windows.SendMessage(WinampHandle, WM_COMMAND, Command, 0)=0 then
        result:=true
      else
        result:=false;
    end;
end;

procedure WinampClose;
//Close Winamp (does also quit playback)
begin
  WinampStop;
  WinampHandle:=FindWindow('Winamp v1.x',nil);
  if WinampHandle=0 then
    exit;
  PostMessage(WinampHandle,WM_CLOSE, 0, 0);
end;

function WinampNextTitle: Boolean;
//Play next title
begin
  result:=WinampCommand(WINAMP_BUTTON1);
end;

function WinampPause: Boolean;
//pause Winamp playback
begin
  result:=WinampCommand(WINAMP_BUTTON3);
end;

function WinampPlay(WinampPath: String; FileOrURL: String): Boolean;
//Play URL or file in Winamp spielen
begin
  result:=false;
  //exit if no file or URL is given
  if FileOrURL='' then
    exit;

  //find Winamp window handle
  WinampHandle:=FindWindow('Winamp v1.x',nil);
  if WinampHandle<>0 then
    begin
      WinampStop;
    end;

  //start Winamp
  if ShellExecute(GetCurrentProcess,'open',PChar('"'+ExtractFilename(WinampPath)+'"'),PChar('"'+FileOrURL+'"'),PChar('"'+ExtractFilePath(WinampPath)+'"'),sw_ShowNormal)<>0 then
    begin
      WinampHandle:=FindWindow('Winamp v1.x',nil);
      if WinampHandle<>0 then
        Windows.SendMessage(WinampHandle, WM_COMMAND, WINAMP_BUTTON2, 0);
      result:=true
    end
  else
    result:=false;
end;

function WinampPreviousTitle: Boolean;
//Play previous title
begin
  result:=WinampCommand(WINAMP_BUTTON1);
end;

function WinampSearchAndPlay(FileOrURL: String; const ShowSearchProgress: Boolean = true; const SearchDriveLetter: Char = 'C'): Boolean;
//Automatic search for Winamp path and play the given file or URL
var
  WinampPath: String;

begin
  result:=false;
  //exit if no file or URL is given
  if FileOrURL='' then
    exit;

  //Search for Winamp.exe
  WinampPath:=FindFileEx(SearchDriveLetter+':','Winamp.exe',ShowSearchProgress);
  if WinampPath<>'' then
    result:=WinampPlay(WinampPath,FileOrURL);
end;

function WinampStop: Boolean;
//Stop Winamp playback
begin
  result:=WinampCommand(WINAMP_BUTTON4);
end;

function WinampVolumeDown: Boolean;
//Volume down in Winamp
begin
  result:=WinampCommand(WINAMP_VOLUMEDOWN);
end;

function WinampVolumeUp: Boolean;
//Volume up in Winamp
begin
  result:=WinampCommand(WINAMP_VOLUMEUP);
end;

end.

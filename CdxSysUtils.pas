unit CdxSysUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxSysUtils
Version:    1.0
Purpose:    Set of additional system and application helper functions
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   19.12.2013    Initial version
                    procedure WindowsLogoff() moved from CdxUtils unit
                    procedure WindowsRestart() moved from CdxUtils unit
                    procedure WindowsShutdown() moved from CdxUtils unit
                    procedure ApplicationRestart() moved from CdxUtils unit
                    function ApplicationVersion() moved from CdxUtils unit
                    function ComPortExists() moved from CdxUtils unit

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Process, CdxFileUtils;

procedure ApplicationRestart;
function ApplicationVersion(const ShortForm: Boolean = false): String;
function ComPortExists(COM: Integer): Boolean;
procedure WindowsLogoff;
procedure WindowsRestart(const forced: Boolean = true; const Delay: Byte = 1; const Comment: String ='');
procedure WindowsShutdown(const forced: Boolean = true; const Delay: Byte = 1; const Comment: String ='');


implementation

procedure ApplicationRestart;
//Restart the application
var
  Process: TProcess;

begin
  //Create the process for the new instance of the application
  Process:=TProcess.Create(nil);
  Process.CommandLine:=GetCommandLine;
  Process.Options:=Process.Options+[];
  Process.ShowWindow:=swoShow;
  Process.Priority:=ppNormal;

  //run and free process
  Process.Execute;
  Process.Free;

  //close old instance of application
  TerminateProcess(GetCurrentProcess, 1);
end;

function ApplicationVersion(const ShortForm: Boolean = false): String;
//Retrieve the file version of the actual application
begin
  result:=GetFileVersion(StringReplace(ExtractFilePath(GetCommandLine)+ExtractFilename(GetCommandLine),'"','',[rfReplaceAll, rfIgnoreCase]),ShortForm);
end;

function ComPortExists(COM: Integer): Boolean;
//Check if a COM port does exist
var
  DeviceHandle: THandle;

begin
  DeviceHandle:=0;
  result:=false;
  try
    //create COM port handle
    DeviceHandle:=CreateFile(PChar('COM'+IntToStr(COM)), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
    //if handle can be created COM port does exist (attention: an existing but already opened COM port will also not be available!)
    if DeviceHandle<>INVALID_HANDLE_VALUE then
      result:=true;
  finally
    CloseHandle(DeviceHandle);
  end;
end;

procedure Call_Shutdown_Exe(Mode: Byte; const forced: Boolean = true; const Delay: Byte = 1; const Comment: String ='');
//Function calls of Windows commandline tool "shutdown.exe"
var
  Process: TProcess;
  Parameter: String;
  CommentStr: String;

begin
  //Use given comment or create a default comment (only for restart/shutdown!)
  if Comment<>'' then
    CommentStr:=Comment
  else
    begin
      if Mode=2 then
        CommentStr:='Windows Reboot'
      else
        CommentStr:='Windows Shutdown';
      if Delay=0 then
        CommentStr:=CommentStr+' now'
      else
        begin
          if Delay=1 then
            CommentStr:=CommentStr+' in 1 second!'
          else
            CommentStr:=CommentStr+' in '+IntToStr(Delay)+ ' seconds!';
        end;
    end;

  //Create a process for Windows "shutdown.exe"
  Process:=TProcess.Create(nil);
  Process.Executable:='cmd.exe';                  //Windows commandline shell
  Parameter:='/C shutdown.exe ';                  //Windows shutdown.exe commandline tool
  if Mode=1 then
    Parameter:=Parameter+'-l'                     //logoff
  else if Mode=2 then
    Parameter:=Parameter+'-r -c "'+CommentStr+'"' //restart
  else
    Parameter:=Parameter+'-s -c "'+CommentStr+'"';//shutdown
  if (Delay>0) and (Mode<>1) then
    Parameter:=Parameter+' -t '+IntToStr(Delay)   //add optional delay for restart/shutdown
  else
    begin
      if forced=true then
        Parameter:=Parameter+' -f';               //forced in case of 0 second delay otherwise shutdown.exe uses this option automatically
    end;
  Process.Parameters.Text:=Parameter;
  Process.Options:=Process.Options+[];
  Process.ShowWindow:=swoShow;
  Process.Priority:=ppNormal;

  //run and free process
  Process.Execute;
  Process.Free;

  //close application to prevent Windows not shutting down due to a still running task
  TerminateProcess(GetCurrentProcess, 1);
end;

procedure WindowsLogoff;
//Restart Windows
begin
  Call_Shutdown_Exe(1); //forced, Delay and Comment not used for Logoff
end;

procedure WindowsRestart(const forced: Boolean = true; const Delay: Byte = 1; const Comment: String ='');
//Restart Windows
begin
  Call_Shutdown_Exe(2, forced, Delay, Comment);
end;

procedure WindowsShutdown(const forced: Boolean = true; const Delay: Byte = 1; const Comment: String ='');
//Shutdown Windows
begin
  Call_Shutdown_Exe(0, forced, Delay, Comment);
end;

end.


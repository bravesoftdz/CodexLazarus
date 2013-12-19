unit CdxUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxUtils
Version:    1.1
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
                    function SubnetFromIPv4()
1.0b  17.12.2013    added Github URL
1.1   18.12.2013    function HexToBinStr()
                    function IntToBinStr()
                    procedure WindowsLogoff()
                    procedure WindowsRestart()
                    procedure WindowsShutdown()
                    procedure ApplicationRestart()
                    function ApplicationVersion()
1.2   19.12.2013    function ComPortExists()
                    function SecondsToTimeString()
                    function GetDriveFormat()
                    function IsDriveFat()
                    function IsDriveNTFS()
                    function GetFileSize()

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Windows, Process;

procedure ApplicationRestart;
function ApplicationVersion(const ShortForm: Boolean = false): String;
function ComPortExists(COM: Integer): Boolean;
function GetDriveFormat(const DriveLetter: Char = 'C'): String;
function GetFileSize(FileName: String): Int64;
function GetFileVersion(Filename: String; const ShortForm: Boolean = false): String;
function HexToBinStr(HexString: String): String;
function IntToBinStr(Value: Integer): String;
function IsDriveFAT(const DriveLetter: Char = 'C'): Boolean;
function IsDriveNTFS(const DriveLetter: Char = 'C'): Boolean;
function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString;  Flags: TReplaceFlags): UnicodeString;
function UTF8Chr(Unicode: Cardinal): UTF8String;
function SecondsToTimeString(Seconds: Integer; SecondsAsMilliseconds: Boolean = false): String;
function Split(Delimiter: Char; Text: String): TStrings;
function SubnetFromIPv4(IP: String): String;
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
end;

function IntToBinStr(Value: Integer): String;
//Converts a hexadecimal String to a binary String
begin
  result:=HexToBinStr(IntToHex(Value,SizeOf(Integer)));
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


unit CdxWindowsVersion;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxWindowsVersion
Version:    1.0
Purpose:    Identify Windows version
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   13.12.2013    Initial version
1.0b  17.12.2013    added Github URL


------------------------------------------------------------------------
Additional Technical Information:
------------------------------------------------------------------------

List of Windows 9x version identifiers (Win32Platform = 1):
Source: http://support.microsoft.com/kb/158238/en-us

Release                    Version                      File dates
Windows 95 retail, OEM     4.00.950                     7/11/95
Windows 95 retail SP1      4.00.950A                    7/11/95-12/31/95
OEM Service Release 2      4.00.1111* (4.00.950B)       8/24/96
OEM Service Release 2.1    4.03.1212-1214* (4.00.950B)  8/24/96-8/27/97
OEM Service Release 2.5    4.03.1214* (4.00.950C)       8/24/96-11/18/97
Windows 98 retail, OEM     4.10.1998                    5/11/98
Windows 98, Security CD    4.10.1998A
Windows 98 Second Edition  4.10.2222A                   4/23/99
Windows 98 SE Security CD  4.10.2222B
Windows Me                 4.90.3000                    6/08/00
Windows Me Security CD     4.90.3000A


List of Windows NT version identifiers (Win32Platform = 2):
Source: http://msdn.microsoft.com/en-us/library/windows/desktop/ms724833%28v=vs.85%29.aspx

OS                      Version Major Minor Other
Windows 8.1             6.3*  	6   	3   	OSVERSIONINFOEX.wProductType == VER_NT_WORKSTATION
Windows Server 2012 R2	6.3*    6   	3   	OSVERSIONINFOEX.wProductType != VER_NT_WORKSTATION
Windows 8             	6.2   	6   	2   	OSVERSIONINFOEX.wProductType == VER_NT_WORKSTATION
Windows Server 2012   	6.2   	6   	2   	OSVERSIONINFOEX.wProductType != VER_NT_WORKSTATION
Windows 7             	6.1   	6   	1   	OSVERSIONINFOEX.wProductType == VER_NT_WORKSTATION
Windows Server 2008 R2	6.1   	6   	1	    OSVERSIONINFOEX.wProductType != VER_NT_WORKSTATION
Windows Server 2008   	6.0     6   	0   	OSVERSIONINFOEX.wProductType != VER_NT_WORKSTATION
Windows Vista         	6.0   	6   	0   	OSVERSIONINFOEX.wProductType == VER_NT_WORKSTATION
Windows Server 2003 R2  5.2   	5   	2   	GetSystemMetrics(SM_SERVERR2) != 0
Windows Home Server   	5.2   	5   	2   	OSVERSIONINFOEX.wSuiteMask & VER_SUITE_WH_SERVER
Windows Server 2003   	5.2   	5   	2   	GetSystemMetrics(SM_SERVERR2) == 0
Windows XP Prof. x64    5.2   	5   	2   	(OSVERSIONINFOEX.wProductType == VER_NT_WORKSTATION) && (SYSTEM_INFO.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64)
Windows XP Media Center 5.1   	5     1     SM_MEDIACENTER != 0
Windows XP Starter      5.1     5     1   	SM_STARTER != 0
Windows XP Tablet PC    5.1     5     1   	SM_TABLETPC != 0
Windows XP              5.1   	5   	1   	Not applicable
Windows 2000            5.0   	5   	0   	Not applicable
}

interface

uses
  SysUtils, Windows;

//OSVERSIONINFOEX structure for Windows API (source: http://msdn.microsoft.com/en-us/library/windows/desktop/ms724833%28v=vs.85%29.aspx)
type
  _OSVERSIONINFOEX = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of AnsiChar;
    wServicePackMajor:    WORD;
    wServicePackMinor:    WORD;
    wSuiteMask:           WORD;
    wProductType:         BYTE;
    wReserved:            BYTE;
  end;
  TOSVERSIONINFOEX=_OSVERSIONINFOEX;

  function GetWindowsBuildNumber: DWord;
  function GetWindowsFullVersion: String;
  function GetWindowsVersion(const ShowServicePackVersion: Boolean = true): String;
  function GetWindowsVersionNumber(const ShowPlatform: Boolean = false): String;
  //Windows API call
  function GetVersionEx(var lpVersionInformationEx: TOSVERSIONINFOEX): Bool; stdcall; Overload; external kernel32 name 'GetVersionExA';

const
  //wProductType constant (The operating system is Windows 8, Windows 7, Windows Vista, Windows XP Professional, Windows XP Home Edition, or Windows 2000 Professional.)
  VER_NT_WORKSTATION:       Byte = 1;
  //64Bit processor architecture (source: http://msdn.microsoft.com/en-us/library/windows/desktop/ms724958%28v=vs.85%29.aspx)
  PROCESSOR_ARCHITECTURE_AMD64: Byte = 9; //x64 (AMD or Intel)

implementation

function GetWindowsVersionNumber(const ShowPlatform: Boolean = false): String;
//Identify Windows version number
begin
  if ShowPlatform=true then
    result:=IntToStr(Win32Platform)+'.'+IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion)
  else
    result:=IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion)
end;

function GetWindowsVersion(const ShowServicePackVersion: Boolean = true): String;
//Identify Windows version
var
  OSVersion: String;
  OS: String;
  wProductType: Byte;
  OSVERSIONINFOEX: TOSVERSIONINFOEX;
  SYSTEMINFO: SYSTEM_INFO;

begin
  //create Windows version number
  OSVersion:=GetWindowsVersionNumber(true);

  //if not Windows 9x get additional system infos
  if Win32Platform>1 then
    begin
      OSVERSIONINFOEX.dwOSVersionInfoSize:=SizeOf(TOSVERSIONINFOEX);
      if GetVersionEx(OSVERSIONINFOEX)=true then
        wProductType:=OSVERSIONINFOEX.wProductType
      else
        wProductType:=VER_NT_WORKSTATION;
      GetSystemInfo(SYSTEMINFO);
    end;

  //check version number and additional system infos to create according Windows version String
  if OSVersion='1.4.0' then
    OS:='Windows 95'
  else if OSVersion='1.4.3' then
    OS:='Windows 95 OEM Service Release 2.x'
  else if OSVersion='1.4.10' then
    OS:='Windows 98'
  else if OSVersion='1.4.90' then
    OS:='Windows ME'
  else if OSVersion='2.3.51' then
    OS:='Windows NT 3.51'
  else if OSVersion='2.4.0' then
    OS:='Windows NT 4.0'
  else if OSVersion='2.5.0' then
    OS:='Windows 2000'
  else if OSVersion='2.5.1' then
    begin
      if GetSystemMetrics(SM_MEDIACENTER)<>0 then
        OS:='Windows XP Media Center Edition'
      else if GetSystemMetrics(SM_STARTER)<>0 then
          OS:='Windows XP Starter Edition'
      else if GetSystemMetrics(SM_TABLETPC)<>0 then
          OS:='Windows XP Tablet PC Edition'
      else
        OS:='Windows XP';
    end
  else if OSVersion='2.5.2' then
    begin
      if (wProductType=VER_NT_WORKSTATION) and (SYSTEMINFO.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64) then
        OS:='Windows XP Professional x64 Edition'
      else if GetSystemMetrics(SM_SERVERR2)=0 then
        OS:='Windows Server 2003'
      else if GetSystemMetrics(SM_SERVERR2)<>0 then
        OS:='Windows Server 2003 R2'
      else
        OS:='Windows Home Server';
    end
  else if OSVersion='2.6.0' then
    begin
      if wProductType=VER_NT_WORKSTATION then
        OS:='Windows Vista'
      else
        OS:='Windows Server 2008';
    end
  else if OSVersion='2.6.1' then
    begin
      if wProductType=VER_NT_WORKSTATION then
        OS:='Windows 7'
      else
        OS:='Windows Server 2008 R2';
    end
  else if OSVersion='2.6.2' then
    begin
      if wProductType=VER_NT_WORKSTATION then
        OS:='Windows 8'
      else
        OS:='Windows Server 2012';
    end
  else if OSVersion='2.6.3' then
    begin
      if wProductType=VER_NT_WORKSTATION then
        OS:='Windows 8.1'
      else
        OS:='Windows Server 2012 R2';
    end
  else
    begin
      //Fallback result for unknown Windows versions
      if Win32Platform=1 then
        OS:=IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion)+' (9x)'
      else if Win32Platform=2 then
        OS:=IntToStr(Win32MajorVersion)+'.'+IntToStr(Win32MinorVersion)+' (NT)'
      else
        OS:=OSVersion;
    end;

  //add optional Servicepack version
  if (ShowServicePackVersion=true) and (Win32CSDVersion<>'') then
    result:=OS+' '+Win32CSDVersion
  else
    result:=OS;
end;

function GetWindowsBuildNumber: DWord;
//Identify Windows build
begin
  Result:=Win32BuildNumber;
end;

function GetWindowsFullVersion: String;
//Show full Windows version
begin
  result:=GetWindowsVersion(true)+' (Version '+GetWindowsVersionNumber+' Build '+IntToStr(GetWindowsBuildNumber)+')';
end;

end.

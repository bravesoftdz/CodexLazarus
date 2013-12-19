unit CdxUtils;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxUtils
Version:    1.2b
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
                    function UnicodeStringReplace() moved to CdxStrUtils unit
                    function UTF8Chr() moved to CdxStrUtils unit
                    function Split() moved to CdxStrUtils unit
                    function SubnetFromIPv4() moved to CdxStrUtils unit
                    function HexToBinStr() moved to CdxStrUtils unit
                    function IntToBinStr() moved to CdxStrUtils unit
                    function SecondsToTimeString() moved to CdxStrUtils unit
1.2b   19.12.2013   procedure WindowsLogoff() moved to CdxSysUtils unit
                    procedure WindowsRestart() moved to CdxSysUtils unit
                    procedure WindowsShutdown() moved to CdxSysUtils unit
                    procedure ApplicationRestart() moved to CdxSysUtils unit
                    function ApplicationVersion() moved to CdxSysUtils unit
                    function ComPortExists() moved to CdxSysUtils unit
                    function IsDriveFat() moved to CdxFileUtils unit
                    function IsDriveNTFS() moved to CdxFileUtils unit
                    function GetDriveFormat() moved to CdxFileUtils unit
                    function GetFileSize() moved to CdxFileUtils unit
                    function GetFileSize() moved to CdxFileUtils unit
                    WARNING: CdxUtils unit is actually without any functionality!

}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


implementation

end.


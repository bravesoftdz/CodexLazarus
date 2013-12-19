unit CdxMultimedia;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxMultimedia
Version:    1.0
Purpose:    Set of additional multimedia helper functions
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.1   19.12.2013    Initial version
                    function ValueToLogVolume()

}

interface
  function ValueToLogVolume(Value: Integer; MaxValue: Integer): Integer;

implementation

Uses
  Math;

function ValueToLogVolume(Value: Integer; MaxValue: Integer): Integer;
//Recalculates a linear volume value to a logarithmic volume value which does
//sound more linear to the human ear. Can be used for e.g. for DirectShow
//filters with sound volume range from -10000 (muted) to 0 (loud)
begin
  if Value<=MaxValue then
    result:=round(2500*log10(Value/MaxValue))
  else
    result:=0;
end;


end.

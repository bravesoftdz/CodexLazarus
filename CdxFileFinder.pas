unit CdxFileFinder;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxFileFinder
Version:    1.1
Purpose:    Find file(s)/folder(s) with optional graphical progress
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   13.03.2008    Initial version written in D7
1.1   15.12.2013    Rework as FPC/Lazarus version
                    Renamed functions with appendix "Ex" to prevent collisions with other similiar FPC/Lazarus etc. functions
                    Supports now also UTF8 and Symlinks
1.1b  17.12.2013    added Github URL

}

//enables in FPC/Lazarus Delphi compatibility which makes porting easier since original code was written in Delphi
{$MODE DELPHI}

interface

//(de)activate needed search functions
{$DEFINE FindFile}
{$DEFINE FindFolder}
{$DEFINE FindAllFiles}
{$DEFINE FindAllFolders}

//use ProcessMessages as Anti-Freeze
{$DEFINE UseProcessMessages}

uses
  SysUtils, Classes, Forms, StdCtrls, ExtCtrls, FileCtrl, Controls, LazUTF8;

  //Globaler Abbruch Event definieren
  type TGlobalBreakEvent=class(TObject)
  private
    { Private-Deklarationen }
    procedure GlobalBreakClick(Sender: TObject);
  public
    { Public-Deklarationen }
  end;


  procedure OpenProgressForm();
  procedure CloseProgressForm();
  procedure Progress(Actual: String);
  {$IFDEF FindFile}
  function FindFileEx(StartDir: UTF8String; FileName: UTF8String; ShowProgress: Boolean = false; Title: String = ''): UTF8String;
  {$ENDIF}
  {$IFDEF FindFolder}
  function FindFolderEx(StartDir: UTF8String; FolderName: UTF8String; ShowProgress: Boolean = false; Title: String = ''): UTF8String;
  {$ENDIF}
  {$IFDEF FindAllFiles}
  function FindAllFilesEx(StartDir: UTF8String; ShowProgress: Boolean = false; Title: String = ''): TStringList;
  {$ENDIF}
  {$IFDEF FindAllFolders}
  function FindAllFoldersEx(StartDir: UTF8String; ShowProgress: Boolean = false; Title: String = ''): TStringList;
  {$ENDIF}

var
  GlobalBreak: Boolean;
  ProgressForm: TForm;
  ProgressFormTitle: String;
  ProgressPanel: TPanel;
  ProgressLabel: TLabel;
  ProgressCancelButton: TButton;
  GlobalBreakEvent: TGlobalBreakEvent;

implementation

procedure TGlobalBreakEvent.GlobalBreakClick;
//Global search break condition
begin
  GlobalBreak:=true;
end;

procedure Progress(Actual: String);
//Actualize progress
begin
  if ProgressLabel<>NIL then
    ProgressLabel.Caption:=SysToUTF8(MinimizeName(Actual,ProgressLabel.Canvas,ProgressLabel.Width));
  {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
end;

procedure OpenProgressForm();
//Create and open graphical progress
begin
  GlobalBreak:=false;
  GlobalBreakEvent:=TGlobalBreakEvent.Create;
  ProgressForm:=TForm.Create(Application);
  ProgressForm.Parent:=nil;
  ProgressForm.BorderIcons:=[];
  ProgressForm.FormStyle:=fsStayOnTop;
  ProgressForm.Position:=poScreenCenter;
  if ProgressFormTitle='' then
    ProgressForm.Caption:=Application.Title
  else
    ProgressForm.Caption:=ProgressFormTitle;
  ProgressForm.BorderStyle:=bsDialog;
  ProgressForm.Width:=600;
  ProgressForm.Height:=90;
  ProgressPanel:=TPanel.Create(ProgressForm);
  ProgressPanel.Parent:=ProgressForm;
  ProgressPanel.Top:=0;
  ProgressPanel.Left:=0;
  ProgressPanel.Height:=44;
  ProgressPanel.Width:=600;
  ProgressPanel.DoubleBuffered:=true;
  ProgressLabel:=TLabel.Create(ProgressPanel);
  ProgressLabel.Parent:=ProgressPanel;
  ProgressLabel.AutoSize:=false;
  ProgressLabel.Width:=584;
  ProgressLabel.Height:=24;
  ProgressLabel.Left:=6;
  ProgressLabel.Top:=13;
  ProgressLabel.Visible:=true;
  ProgressLabel.Transparent:=true;
  ProgressLabel.Font.Name:='Tahoma';
  ProgressLabel.Font.Size:=10;
  ProgressCancelButton:=TButton.Create(ProgressForm);
  ProgressCancelButton.Parent:=ProgressForm;
  ProgressCancelButton.Caption:='Stop';
  ProgressCancelButton.OnClick:=GlobalBreakEvent.GlobalBreakClick;
  ProgressCancelButton.Width:=150;
  ProgressCancelButton.Height:=26;
  ProgressCancelButton.Top:=53;
  ProgressCancelButton.Left:=(ProgressForm.ClientWidth div 2)-(ProgressCancelButton.Width div 2);
  ProgressCancelButton.Font.Name:='Tahoma';
  ProgressForm.Show;
  ProgressCancelButton.SetFocus;
end;

procedure CloseProgressForm();
//Close and destroy graphical progress
begin
  ProgressFormTitle:='';
  try
  GlobalBreakEvent.Free;
  ProgressForm.Close;
  ProgressLabel.Free;
  ProgressCancelButton.Free;
  ProgressPanel.Free;
  ProgressForm.Free;
  except
  ProgressForm.Destroy;
  end;
end;

{$IFDEF FindFile}
function FindFileEx(StartDir: UTF8String; FileName: UTF8String; ShowProgress: Boolean = false; Title: String = ''): UTF8String;
//Find a single file
var
  SearchFileResult : TSearchRec;
  SearchFolderResult : TSearchRec;
  ActualDir: UTF8String;
  Folder: TStringList;
  FolderExists: Boolean;
  Counter: Integer;
  FolderCounter: Integer;

label
  JumpLabel, JumpLabel2;

begin
  //Default result
  FindFileEx:='';

  //Check start directory
  if StartDir='' then
    exit;
  StartDir:=StringReplace(StartDir,'/','\',[rfReplaceAll, rfIgnoreCase]);
  if (RightStr(StartDir,1)<>'\') then
    StartDir:=StartDir+'\';
  if DirectoryExists(StartDir)=false then
    exit;

  //Show progress
  ProgressFormTitle:=Title;
  if ShowProgress=true then
    OpenProgressForm;

  //Set defaults
  ActualDir:=StartDir;
  Folder:=TStringList.Create;
  Folder.Add(ActualDir);
  FolderCounter:=0;
  GlobalBreak:=false;
  SearchFolderResult.Size:=0;

  //Search files in start directory
  if FindFirst(Folder[Folder.Count-1]+UTF8toSys(FileName), faAnyFile, SearchFileResult) = 0 then
    begin
      repeat
        if (SearchFileResult.Name<>'') and (SearchFileResult.Name<>'.') and (SearchFileResult.Name<>'..') and (SearchFileResult.Attr<>faDirectory) then
          begin
            if (UpperCase(SearchFileResult.Name)=UpperCase(UTF8toSys(FileName))) then
              begin
                if FileExists(Folder[Folder.Count-1]+SearchFileResult.Name) then
                  begin
                    FindFileEx:=SysToUTF8(Folder[Folder.Count-1]+SearchFileResult.Name);
                    goto JumpLabel2;
                  end;
              end;
          end
        else
          Progress(ActualDir+SearchFolderResult.Name+'\'+SearchFileResult.Name);
        {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
      until FindNext(SearchFileResult) <> 0;
    end;

JumpLabel:
  //Search for folders
  if FindFirst(ActualDir+'*', faAnyFile and faDirectory, SearchFolderResult)=0 then
    begin
      repeat
        if GlobalBreak=true then
          goto JumpLabel2;
        if (((faDirectory or faSymLink) and SearchFolderResult.Attr)=faDirectory) and (SearchFolderResult.Name<>'.') and (SearchFolderResult.Name<>'..') and (SearchFolderResult.Name<>ActualDir+SearchFolderResult.Name+'\') then
          begin
            if DirectoryExists(ActualDir+SearchFolderResult.Name) then
              begin
                FolderExists:=false;
                for Counter:=0 to Folder.Count-1 do
                  begin
                    if ((ActualDir+SearchFolderResult.Name+'\')=Folder.Strings[Counter]) then
                      begin
                        FolderExists:=true;
                        break;
                      end;
                  end;
                if FolderExists=false then
                  begin
                    Folder.Add(ActualDir+SearchFolderResult.Name+'\');
                    Progress(ActualDir+SearchFolderResult.Name+'\');
                    //Search for files in actual found folder
                    if FindFirst(Folder[Folder.Count-1]+UTF8toSys(FileName), faAnyFile, SearchFileResult) = 0 then
                      begin
                        repeat
                          if (SearchFileResult.Name<>'') and (SearchFileResult.Name<>'.') and (SearchFileResult.Name<>'..') and (SearchFileResult.Attr<>faDirectory) then
                            begin
                              if (UpperCase(SearchFileResult.Name)=UpperCase(UTF8toSys(FileName))) then
                                begin
                                  if FileExists(Folder[Folder.Count-1]+SearchFileResult.Name) then
                                    begin
                                      FindFileEx:=SysToUTF8(Folder[Folder.Count-1]+SearchFileResult.Name);
                                      goto JumpLabel2;
                                    end;
                                end;
                            end
                          else
                            Progress(ActualDir+SearchFolderResult.Name+'\'+SearchFileResult.Name);
                          {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
                        until FindNext(SearchFileResult) <> 0;
                      end;
                    FindClose(SearchFileResult);
                  end;
              end;
          end;
        until FindNext(SearchFolderResult) <> 0;
      end;
    FindClose(SearchFolderResult);
    {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}

    FolderCounter:=FolderCounter+1;
    if (Folder.Count>FolderCounter)then
      begin
        ActualDir:=Folder[FolderCounter];
        goto JumpLabel;
      end;

//Exit
JumpLabel2:
  FindClose(SearchFileResult);
  FindClose(SearchFolderResult);
  Folder.Free;
  if ShowProgress=true then
    CloseProgressForm;
end;
{$ENDIF}

{$IFDEF FindFolder}
function FindFolderEx(StartDir: UTF8String; FolderName: UTF8String; ShowProgress: Boolean = false; Title: String = ''): UTF8String;
//Find single folder
var
  SearchFolderResult : TSearchRec;
  ActualDir: UTF8String;
  Folder: TStringList;
  FolderExists: Boolean;
  Counter: Integer;
  FolderCounter: Integer;

label
  JumpLabel, JumpLabel2;

begin
  //Default result
  FindFolderEx:='';

  //Check start directory
  if StartDir='' then
    exit;
  StartDir:=StringReplace(StartDir,'/','\',[rfReplaceAll, rfIgnoreCase]);
  if (RightStr(StartDir,1)<>'\') then
    StartDir:=StartDir+'\';
  if DirectoryExists(UTF8toSys(StartDir))=false then
    exit;

  //Show progress
  ProgressFormTitle:=Title;
  if ShowProgress=true then
    OpenProgressForm;

  //Set defaults
  ActualDir:=StartDir;
  Folder:=TStringList.Create;
  Folder.Add(ActualDir);
  FolderCounter:=0;
  GlobalBreak:=false;

JumpLabel:
  //Search folders
  if FindFirst(UTF8ToSys(ActualDir)+'*', faAnyFile and faDirectory, SearchFolderResult)=0 then
    begin
      repeat
        if GlobalBreak=true then
          goto JumpLabel2;
        if (((faDirectory or faSymLink) and SearchFolderResult.Attr)=faDirectory) and (SearchFolderResult.Name<>'.') and (SearchFolderResult.Name<>'..') and (SearchFolderResult.Name<>ActualDir+SearchFolderResult.Name+'\') then
          begin
            if DirectoryExists(ActualDir+SearchFolderResult.Name) then
              begin
                if (UpperCase(SearchFolderResult.Name)=UpperCase(UTF8toSys(FolderName))) then
                  begin
                    FindFolderEx:=SysToUTF8(ActualDir+SearchFolderResult.Name+'\');
                    goto JumpLabel2;
                  end;
                FolderExists:=false;
                for Counter:=0 to Folder.Count-1 do
                  begin
                    if (ActualDir+SearchFolderResult.Name+'\'=Folder.Strings[Counter]) then
                      begin
                        FolderExists:=true;
                        break;
                      end;
                  end;
                if FolderExists=false then
                  begin
                    Folder.Add(ActualDir+SearchFolderResult.Name+'\');
                    Progress(ActualDir+SearchFolderResult.Name+'\');
                  end;
              end;
          end;
        until FindNext(SearchFolderResult) <> 0;
      end;
    FindClose(SearchFolderResult);
    {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
    FolderCounter:=FolderCounter+1;
    if (Folder.Count>FolderCounter) and (GlobalBreak=false) then
      begin
        ActualDir:=Folder[FolderCounter];
        goto JumpLabel;
      end;

//Exit
JumpLabel2:
  FindClose(SearchFolderResult);
  Folder.Free;
  if ShowProgress=true then
    CloseProgressForm;
end;
{$ENDIF}

{$IFDEF FindAllFiles}
function FindAllFilesEx(StartDir: UTF8String; ShowProgress: Boolean = false; Title: String = ''): TStringList;
//Find all files from a folder
var
  SearchFileResult : TSearchRec;
  SearchFolderResult : TSearchRec;
  ActualDir: String;
  Files: TStringList;
  Folder: TStringList;
  FolderExists: Boolean;
  Counter: Integer;
  FolderCounter: Integer;

label
  JumpLabel, JumpLabel2;

begin
  //Default result
  FindAllFilesEx:=TStringList.Create;
  FindAllFilesEx.Clear;

  //Check start directory
  StartDir:=StringReplace(StartDir,'/','\',[rfReplaceAll, rfIgnoreCase]);
  if (RightStr(StartDir,1)<>'\') then
    StartDir:=StartDir+'\';
  if DirectoryExists(UTF8toSys(StartDir))=false then
    exit;

  //Show progress
  ProgressFormTitle:=Title;
  if ShowProgress=true then
    OpenProgressForm;

  //Set defaults
  ActualDir:=StartDir;
  Files:=TStringList.Create;
  Folder:=TStringList.Create;
  Folder.Add(ActualDir);
  FolderCounter:=0;
  GlobalBreak:=false;

  //Search all files in start directory
  if FindFirst(Folder[Folder.Count-1]+'*', faAnyFile, SearchFileResult) = 0 then
    begin
      repeat
        if GlobalBreak=true then
          goto JumpLabel2;
        if (SearchFileResult.Name<>'.') and (SearchFileResult.Name<>'..') and (SearchFileResult.Attr<>faDirectory) then
          begin
            Files.Add(Folder[Folder.Count-1]+SearchFileResult.Name);
            Progress(Folder[Folder.Count-1]+SearchFileResult.Name);
          end;
        {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
      until FindNext(SearchFileResult) <> 0;
    end;
  FindClose(SearchFileResult);

JumpLabel:
  //Search folder
  if FindFirst(ActualDir+'*', faAnyFile and faDirectory, SearchFolderResult)=0 then
    begin
      repeat
        if GlobalBreak=true then
          goto JumpLabel2;
        if (((faDirectory or faSymLink) and SearchFolderResult.Attr)=faDirectory) and (SearchFolderResult.Name<>'.') and (SearchFolderResult.Name<>'..') and (SearchFolderResult.Name<>ActualDir+SearchFolderResult.Name+'\') then
          begin
            if DirectoryExists(ActualDir+SearchFolderResult.Name) then
              begin
                FolderExists:=false;
                for Counter:=0 to Folder.Count-1 do
                  begin
                    if (ActualDir+SearchFolderResult.Name+'\'=Folder.Strings[Counter]) then
                      begin
                        FolderExists:=true;
                        break;
                      end;
                  end;
                if FolderExists=false then
                  begin
                    Folder.Add(ActualDir+SearchFolderResult.Name+'\');
                    Progress(Folder[Folder.Count-1]);
                    //Search for files in actual found folder
                    if FindFirst(Folder[Folder.Count-1]+'*', faAnyFile, SearchFileResult) = 0 then
                      begin
                        repeat
                          if GlobalBreak=true then
                            goto JumpLabel2;
                          if (SearchFileResult.Name<>'.') and (SearchFileResult.Name<>'..') and (SearchFileResult.Attr<>faDirectory) then
                            begin
                              Files.Add(Folder[Folder.Count-1]+SearchFileResult.Name);
                              Progress(Folder[Folder.Count-1]+SearchFileResult.Name);
                            end;
                          {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
                        until FindNext(SearchFileResult) <> 0;
                      end;
                    FindClose(SearchFileResult);
                  end;
              end;
          end;
        until FindNext(SearchFolderResult) <> 0;
      end;
    FindClose(SearchFolderResult);
    {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
    FolderCounter:=FolderCounter+1;
    if (Folder.Count>FolderCounter)then
      begin
        ActualDir:=Folder[FolderCounter];
        goto JumpLabel;
      end;

//Exit
JumpLabel2:
  FindClose(SearchFileResult);
  FindClose(SearchFolderResult);
  Files.Sort;
  Files.Text:=SysToUTF8(Files.Text);
  if GlobalBreak=true then
    Files.Clear;
  FindAllFilesEx:=Files;
  Folder.Free;
  if ShowProgress=true then
    CloseProgressForm;
end;
{$ENDIF}

{$IFDEF FindAllFolders}
function FindAllFoldersEx(StartDir: UTF8String; ShowProgress: Boolean = false; Title: String = ''): TStringList;
//Find all (sub)folders
var
  SearchFolderResult : TSearchRec;
  ActualDir: UTF8String;
  Folder: TStringList;
  FolderExists: Boolean;
  Counter: Integer;
  FolderCounter: Integer;

label
  JumpLabel, JumpLabel2;

begin
  //Default result
  FindAllFoldersEx:=TStringList.Create;
  FindAllFoldersEx.Clear;

  //Check start directory
  StartDir:=StringReplace(StartDir,'/','\',[rfReplaceAll, rfIgnoreCase]);
  if (RightStr(StartDir,1)<>'\') then
    StartDir:=StartDir+'\';

  if DirectoryExists(UTF8toSys(StartDir))=false then
    exit;

  //ShowProgress
  ProgressFormTitle:=Title;
  if ShowProgress=true then
    OpenProgressForm;

  //Set defaults
  ActualDir:=StartDir;
  Folder:=TStringList.Create;
  Folder.Add(ActualDir);
  FolderCounter:=0;
  GlobalBreak:=false;

JumpLabel:
  //Search folder
  if FindFirst(UTF8toSys(ActualDir)+'*', faAnyFile and faDirectory, SearchFolderResult)=0 then
    begin
      repeat
        if GlobalBreak=true then
          goto JumpLabel2;
        if (((faDirectory or faSymLink) and SearchFolderResult.Attr)=faDirectory) and (SearchFolderResult.Name<>'.') and (SearchFolderResult.Name<>'..') and (SearchFolderResult.Name<>ActualDir+SearchFolderResult.Name+'\') then
          begin
            if DirectoryExists(UTF8toSys(ActualDir)+SearchFolderResult.Name) then
              begin
                FolderExists:=false;
                for Counter:=0 to Folder.Count-1 do
                  begin
                    if (ActualDir+SearchFolderResult.Name+'\'=Folder.Strings[Counter]) then
                      begin
                        FolderExists:=true;
                        break;
                      end;
                  end;
                if FolderExists=false then
                  begin
                    Folder.Add(ActualDir+SysToUTF8(SearchFolderResult.Name)+'\');
                    Progress(ActualDir+SearchFolderResult.Name+'\');
                  end;
              end;
          end;
        until FindNext(SearchFolderResult) <> 0;
      end;
    FindClose(SearchFolderResult);
    {$IFDEF UseProcessMessages}Application.ProcessMessages;{$ENDIF}
    FolderCounter:=FolderCounter+1;
    if (Folder.Count>FolderCounter)then
      begin
        ActualDir:=Folder[FolderCounter];
        goto JumpLabel;
      end;
    Folder.Sort;
    FindAllFoldersEx:=Folder;

//Exit
JumpLabel2:
  FindClose(SearchFolderResult);
  if ShowProgress=true then
    CloseProgressForm;
end;
{$ENDIF}

end.

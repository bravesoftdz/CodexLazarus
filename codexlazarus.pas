{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodexLazarus;

interface

uses
  CdxWindowsVersion, CdxFileFinder, CdxHTMLmask, CdxUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('CodexLazarus', @Register);
end.
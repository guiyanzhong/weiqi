{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit weiqi; 

interface

uses
  WeiqiBoard, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('WeiqiBoard', @WeiqiBoard.Register); 
end; 

initialization
  RegisterPackage('weiqi', @Register); 
end.

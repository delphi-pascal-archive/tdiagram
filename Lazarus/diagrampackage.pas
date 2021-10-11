{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit diagrampackage; 

interface

uses
  Diagram, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Diagram', @Diagram.Register); 
end; 

initialization
  RegisterPackage('diagrampackage', @Register); 
end.

program c64demo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  c64demoMainUnit in 'c64demoMainUnit.pas' {Form1},
  C64 in '..\src\c64.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

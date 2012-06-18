program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, game_boards
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Weiqi for Fun';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


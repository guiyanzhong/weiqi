unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, WeiqiBoard;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MINewGame: TMenuItem;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    WeiqiBoard1: TWeiqiBoard;
    procedure FormCreate(Sender: TObject);
    procedure MINewGameClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WeiqiBoard1ItemClick(Sender: TObject; X, Y: Integer);
  private
    { private declarations }
    ps: TPlayerSide;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.WeiqiBoard1ItemClick(Sender: TObject; X, Y: Integer);
begin
  if WeiqiBoard1.Move(X, Y, ps) then
  begin
    if ps = psBlack then
      ps := psWhite
    else
      ps := psBlack;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ps := psBlack;
end;

procedure TForm1.MINewGameClick(Sender: TObject);
begin
  WeiqiBoard1.Clear;
  ps := psBlack;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin

end;

end.


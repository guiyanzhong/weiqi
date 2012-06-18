unit WeiqiBoard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Math, Controls, Graphics;

type

  TPlayerSide = (psNone, psBlack, psWhite);

  { TWeiqiBoard }

  TWeiqiBoardClickEvent = procedure(Sender: TObject; X, Y: Integer) of Object;

  TWeiqiBoard = class(TCustomControl)
  private
    boardSize: Integer;
    BoardItems: Array of Array of TPlayerSide;
    BoardALiveStatus: Array of Array of Boolean;
    BoardVisitedStatus: Array of Array of Boolean;
    FOnBoardClick: TWeiqiBoardClickEvent;
    procedure OnMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnPaintHandler(Sender: TObject);
    function RemoveCapturedItems(Last_X, Last_Y: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Move(X, Y: Integer; PlaySide: TPlayerSide): Boolean;
  published
    property Align;
    property OnItemClick: TWeiqiBoardClickEvent read FOnBoardClick write FOnBoardClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Weiqi',[TWeiqiBoard]);
end;

{ TWeiqiBoard }

procedure TWeiqiBoard.OnMouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  boardWidth, boardLeft, boardTop, xx, yy: Integer;
begin
  boardWidth := Min(Width, Height) - 10;
  boardLeft := (Width - boardWidth) div 2;
  boardTop := (Height - boardWidth) div 2;
  xx := (X - boardLeft) * boardSize div boardWidth;
  yy := (Y - boardTop) * boardSize div boardWidth;
  if (xx >= 0) and (yy >= 0) and (xx < boardSize) and (yy < boardSize) then
    if (X > boardLeft) and (Y > boardTop) then
      if Assigned(FOnBoardClick) then FOnBoardClick(Self, xx, yy);
end;

procedure TWeiqiBoard.OnPaintHandler(Sender: TObject);
var
  boardWidth, boardLeft, boardTop, i, j, temp_x, temp_y: Integer;
  gridWidth: Double;

  procedure DrawDot(X, Y: Integer);
  var
    x_px, y_px, r: Integer;
  begin
    x_px := Round(boardLeft + gridWidth / 2 + gridWidth * X);
    y_px := Round(boardTop + gridWidth / 2 + gridWidth * Y);
    r := 3;
    Canvas.Brush.Color := clBlack;
    Canvas.Pen.Color := clBlack;
    Canvas.EllipseC(x_px, y_px, r, r);
  end;
  procedure DrawItem(X, Y: Integer; C: TColor);
  var
    x_px, y_px, r: Integer;
  begin
    x_px := Round(boardLeft + gridWidth / 2 + gridWidth * X);
    y_px := Round(boardTop + gridWidth / 2 + gridWidth * Y);
    r := Round(gridWidth * 2 / 5);
    Canvas.Brush.Color := C;
    Canvas.Pen.Color := clBlack;
    Canvas.EllipseC(x_px, y_px, r, r);
    //if BoardVisitedStatus[X][Y] then
    //begin
    //  Canvas.Pen.Color := clGreen;
    //  Canvas.Pen.Width := 3;
    //  Canvas.Line(x_px - r, y_px, x_px, y_px + r);
    //  Canvas.Line(x_px, y_px + r, x_px + r, y_px - r);
    //  Canvas.Pen.Width := 1;
    //  Canvas.Pen.Color := clBlack;
    //end;
    if not BoardALiveStatus[X][Y] then
    begin
      Canvas.Pen.Color := clRed;
      Canvas.Pen.Width := 3;
      Canvas.Line(x_px - r, y_px -r, x_px + r, y_px + r);
      Canvas.Line(x_px + r, y_px -r, x_px - r, y_px + r);
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack;
    end;
  end;

begin
  // Draw background.
  Canvas.Brush.Color := $00cccccc;
  Canvas.FillRect(0, 0, Width, Height);
  // Draw board rectangle.
  boardWidth := Min(Width, Height) - 10;
  boardLeft := (Width - boardWidth) div 2;
  boardTop := (Height - boardWidth) div 2;
  Canvas.Brush.Color := $0077ddff;
  Canvas.Rectangle(boardLeft, boardTop, boardLeft + boardWidth, boardTop + boardWidth);
  // Draw board lines.
  gridWidth := boardWidth * 1.0 / boardSize;
  for i := 0 to boardSize - 1 do
  begin
    temp_x := Round(boardLeft + gridWidth / 2 + gridWidth * i);
    temp_y := Round(boardTop + gridWidth / 2 + gridWidth * i);
    Canvas.Line(Round(boardLeft + gridWidth / 2), temp_y, Round(boardLeft + boardWidth - gridWidth / 2), temp_y);
    Canvas.Line(temp_x, Round(boardTop + gridWidth / 2), temp_x, Round(boardTop + boardWidth - gridWidth / 2));
  end;
  // Draw dots.
  if boardSize = 19 then
  begin
    DrawDot(3, 3);
    DrawDot(boardSize - 4, boardSize - 4);
    DrawDot(3, boardSize - 4);
    DrawDot(boardSize - 4, 3);
    DrawDot((boardSize - 1) div 2, (boardSize - 1) div 2);
  end;
  // Draw items.
  for i := 0 to boardSize - 1 do
    for j := 0 to boardSize - 1 do
    begin
      if BoardItems[i][j] = psBlack then
        DrawItem(i, j, clBlack)
      else
        if BoardItems[i][j] = psWhite then
          DrawItem(i, j, clWhite);
    end;
end;

function TWeiqiBoard.RemoveCapturedItems(Last_X, Last_Y: Integer): Boolean;
var
  i, j: Integer;
  possible_has_more: Boolean;
  PlayerSide: TPlayerSide;
begin
  // Initialize data.
  for i := 0 to boardSize - 1 do
    for j := 0 to boardSize - 1 do
      BoardALiveStatus[i][j] := False;
  for i := 0 to boardSize - 1 do
    for j := 0 to boardSize - 1 do
      BoardVisitedStatus[i][j] := False;

  // Check for blank items.
  for i := 0 to boardSize - 1 do
    for j := 0 to boardSize - 1 do
      if BoardItems[i][j] = psNone then
      begin
        BoardVisitedStatus[i][j] := True;
        BoardALiveStatus[i][j] := True;
        if i > 0 then BoardALiveStatus[i - 1][j] := True;
        if j > 0 then BoardALiveStatus[i][j - 1] := True;
        if i < boardSize - 1 then BoardALiveStatus[i + 1][j] := True;
        if j < boardSize - 1 then BoardALiveStatus[i][j + 1] := True;
      end;
  // Check for same color items.
  repeat
    possible_has_more := False;
    for i := 0 to boardSize - 1 do
      for j := 0 to boardSize - 1 do
        if (BoardItems[i][j] <> psNone) and BoardALiveStatus[i][j] and not BoardVisitedStatus[i][j] then
        begin
          BoardVisitedStatus[i][j] := True;
          possible_has_more := True;
          PlayerSide := BoardItems[i][j];
          if i > 0 then if BoardItems[i - 1][j] = PlayerSide then BoardALiveStatus[i - 1][j] := True;
          if j > 0 then if BoardItems[i][j - 1] = PlayerSide then BoardALiveStatus[i][j - 1] := True;
          if i < boardSize - 1 then if BoardItems[i + 1][j] = PlayerSide then BoardALiveStatus[i + 1][j] := True;
          if j < boardSize - 1 then if BoardItems[i][j + 1] = PlayerSide then BoardALiveStatus[i][j + 1] := True;
        end;
  until not possible_has_more;

  // Remove captured ones.
  Result := BoardALiveStatus[Last_X][Last_Y];
  //if Result then
  //  for i := 0 to boardSize - 1 do
  //    for j := 0 to boardSize - 1 do
  //      if (BoardItems[i][j] <> psNone) and (not BoardALiveStatus[i][j]) then
  //        BoardItems[i][j] := psNone;
end;

constructor TWeiqiBoard.Create(AOwner: TComponent);
var
  i, j: Integer;
begin
  inherited Create(AOwner);
  Left := 0;
  Top := 0;
  Width := 600;
  Height := 600;
  // Initialize data members.
  boardSize := 19;
  SetLength(BoardItems, boardSize);
  for i := 0 to boardSize - 1 do
  begin
    SetLength(BoardItems[i], boardSize);
    for j := 0 to boardSize - 1 do
      BoardItems[i][j] := psNone;
  end;
  SetLength(BoardALiveStatus, boardSize);
  for i := 0 to boardSize - 1 do
  begin
    SetLength(BoardALiveStatus[i], boardSize);
    for j := 0 to boardSize - 1 do
      BoardALiveStatus[i][j] := True;
  end;
  SetLength(BoardVisitedStatus, boardSize);
  for i := 0 to boardSize - 1 do
  begin
    SetLength(BoardVisitedStatus[i], boardSize);
    for j := 0 to boardSize - 1 do
      BoardVisitedStatus[i][j] := False;
  end;
  OnMouseUp := @OnMouseUpHandler;
  OnPaint := @OnPaintHandler;
end;

destructor TWeiqiBoard.Destroy;
var
  i: Integer;
begin
  // Clear data members.
  for i := 0 to boardSize - 1 do
    SetLength(BoardVisitedStatus[i], 0);
  SetLength(BoardVisitedStatus, 0);
  for i := 0 to boardSize - 1 do
    SetLength(BoardALiveStatus[i], 0);
  SetLength(BoardALiveStatus, 0);
  for i := 0 to boardSize - 1 do
    SetLength(BoardItems[i], 0);
  SetLength(BoardItems, 0);
  inherited Destroy;
end;

procedure TWeiqiBoard.Clear;
var
  i, j: Integer;
begin
  for i := 0 to boardSize - 1 do
    for j := 0 to boardSize - 1 do
      BoardItems[i][j] := psNone;
  Invalidate;
end;

function TWeiqiBoard.Move(X, Y: Integer; PlaySide: TPlayerSide): Boolean;
begin
  Assert((X >= 0) and (Y >= 0) and (X < boardSize) and (Y < boardSize));
  if (X >= 0) and (Y >= 0) and (X < boardSize) and (Y < boardSize) then
  begin
    Result := BoardItems[X][Y] = psNone;
    if Result then
    begin
      BoardItems[X][Y] := PlaySide;
      RemoveCapturedItems(X, Y);
      Invalidate;
      //  if RemoveCapturedItems(X, Y) then
      //    Invalidate
      //else
      //begin
      //  BoardItems[X][Y] := psNone;
      //  Result := False;
      //end;
    end;
  end
  else
    Result := False;
end;

end.


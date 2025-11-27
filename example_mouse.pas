program mouse_example;

{$mode objfpc}{$H+}

uses
  Crt,
  SysUtils,
  {$if FPC_fullVersion >= 20701}
  GraphAllegro5Port, MouseAllegro5Port
  {$else}
  Graph, Mouse
  {$endif}
;

const
  NumCols = 5;
  NumRows = 5;
  CellSize = 80;

var
  mouseEvent: TMouseEvent;
  cellColors: array[0..NumRows-1,0..NumCols-1] of Integer;
  r, c: Integer;

procedure Init;
var
  GraphicsDriver, GraphicsMode: Integer;
begin
  GraphicsDriver := Detect;
  GraphicsMode := 0;

  InitGraph(GraphicsDriver, GraphicsMode, '');
  if GraphResult <> grOk then
  begin
    WriteLn('[ERROR] Graphics init failed.');
    Halt(1);
  end;
end;

procedure DrawTable(me: TMouseEvent);
var
  rr, cc: Integer;
begin
  ClearDevice;
  for rr := 0 to NumRows-1 do
    for cc := 0 to NumCols-1 do
    begin
      SetFillStyle(1, cellColors[rr][cc]);
      Bar(cc*CellSize, rr*CellSize, (cc+1)*CellSize, (rr+1)*CellSize);
      SetColor(White);
      Rectangle(cc*CellSize, rr*CellSize, (cc+1)*CellSize, (rr+1)*CellSize);
    end;

  SetColor(Yellow);
  OutTextXY(10, NumRows*CellSize + 10,
    'Mouse: (' + IntToStr(me.x) + ',' + IntToStr(me.y) +
    ') Buttons: ' + IntToStr(me.buttons));
end;

begin
  Init;
  InitMouse;

  for r := 0 to NumRows-1 do
    for c := 0 to NumCols-1 do
      cellColors[r][c] := DarkGray;

  repeat
    if PollMouseEvent(mouseEvent) then
    begin
      r := mouseEvent.y div CellSize;
      c := mouseEvent.x div CellSize;

      if (r >= 0) and (r < NumRows) and (c >= 0) and (c < NumCols) then
      begin
        if mouseEvent.eventType = ME_MOUSE_MOVE then
          cellColors[r][c] := LightBlue;

        if mouseEvent.eventType = ME_BUTTON_DOWN then
        begin
          case mouseEvent.buttons of
            MouseLeftButton: cellColors[r][c] := Red;
            MouseRightButton: cellColors[r][c] := Green;
            MouseMiddleButton: cellColors[r][c] := Blue;
          end;
        end;
      end;
    end;

    DrawTable(mouseEvent);
    FlipDisplay;
  until KeyPressed;

  ReadKey;

  CloseMouse;
  CloseGraph;
end.


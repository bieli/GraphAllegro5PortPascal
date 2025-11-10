program GraphExample;

{$mode objfpc}{$H+}

uses
  Crt,
  {$if FPC_fullVersion >= 20701}
  GraphAllegro5Port
  {$else}
  Graph
  {$endif}
;

const
  FontPath = '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf';
  FrameDelay = 30;
  MaxLines = 50;
  RobotSpeed = 2;
  LaserLength = 180;

var
  GraphicsDriver, GraphicsMode, ErrCode: Integer;
  FontHandle: Pointer;
  MaxX, MaxY, MidX, MidY: Integer;
  Angle: Integer;
  BallX, BallY, BallDX, BallDY: Integer;
  RobotX, RobotY: Integer;
  LaserActive: Boolean;
  LaserTimer: Integer;
  PolyPoints: array[0..7] of Integer;
  LineData: array[0..MaxLines - 1] of record
    x1, y1, x2, y2: Integer;
    color: Integer;
  end;

procedure Init;
var
  i: Integer;
begin
  GraphicsDriver := Detect();
  {$if FPC_fullVersion >= 20701}
    GraphicsMode := m800x600;
  {$else}
    GraphicsMode := 0;
  {$endif}
  InitGraph(GraphicsDriver, GraphicsMode, '');
  ErrCode := GraphResult();
  if ErrCode <> grOK then
  begin
    ClrScr;
    Writeln('Graphics error: ', GraphErrorMsg(ErrCode));
    ReadLn;
    Halt(1);
  end;

  MaxX := GetMaxX;
  MaxY := GetMaxY;
  MidX := MaxX div 2;
  MidY := MaxY div 2;

  SetBkColor(DarkGray);
  ClearDevice;

  {$if FPC_fullVersion >= 20701}
    FontHandle := RegisterBGIFont(FontPath, 20);
  {$else}
    FontHandle := RegisterBGIFont('GOTH.CHR');
  {$endif}
  SetTextStyle(FontHandle, 0, 20);

  BallX := MidX;
  BallY := MidY;
  BallDX := 4;
  BallDY := 3;

  RobotX := 50;
  RobotY := MaxY - 350;
  LaserActive := False;
  LaserTimer := 0;

  Randomize;
  for i := 0 to MaxLines - 1 do
  begin
    LineData[i].x1 := Random(MaxX);
    LineData[i].y1 := Random(MaxY);
    LineData[i].x2 := Random(MaxX);
    LineData[i].y2 := Random(MaxY);
    LineData[i].color := Random(15) + 1;
  end;
end;

procedure DrawStaticPrimitives;
begin
  SetColor(Yellow);
  Line(20, 20, MaxX - 20, 20);
  Rectangle(50, 50, 150, 100);
  Bar(160, 50, 260, 100);
  SetFillStyle(1, LightBlue);
  FillEllipse(320, 75, 40, 25);
  Circle(400, 75, 30);
  Ellipse(480, 75, 60, 30);
  PieSlice(560, 75, 30, 300, 40);
  Bar3D(620, 50, 700, 100, 15, True);
  Sector(760, 75, 0, 180, 40, 25);

  SetColor(LightGreen);
  PolyPoints[0] := 100; PolyPoints[1] := 200;
  PolyPoints[2] := 150; PolyPoints[3] := 250;
  PolyPoints[4] := 100; PolyPoints[5] := 300;
  PolyPoints[6] :=  50; PolyPoints[7] := 250;
  DrawPoly(4, PolyPoints);
  SetFillStyle(2, Green);
  FillPoly(4, PolyPoints);

  MoveTo(200, 200);
  LineTo(300, 200);
  MoveRel(0, 20);
  LineRel(100, 50);

  SetColor(White);
  OutTextXY(50, MaxY - 60, 'GraphAllegro5Port Demo');
end;

procedure DrawRobot(X, Y: Integer);
begin
  SetColor(Yellow);
  SetFillStyle(1, Yellow);
  Bar(X, Y, X + 30, Y + 40);         // Body
  Circle(X + 15, Y - 10, 10);        // Head
  Line(X, Y + 10, X - 10, Y + 20);   // Left arm
  Line(X + 30, Y + 10, X + 40, Y + 20); // Right arm
  Line(X + 10, Y + 40, X + 10, Y + 60); // Left leg
  Line(X + 20, Y + 40, X + 20, Y + 60); // Right leg

  if LaserActive then
  begin
    SetColor(Red);
    Line(X + 40, Y + 15, X + 40 + LaserLength, Y + 15);
  end;
end;

procedure DrawAnimatedOverlay;
var
  i: Integer;
begin
  for i := 0 to MaxLines - 1 do
  begin
    SetColor(LineData[i].color);
    Line(LineData[i].x1, LineData[i].y1, LineData[i].x2, LineData[i].y2);
    Inc(LineData[i].x1, Random(5) - 1);
    Inc(LineData[i].y1, Random(5) - 1);
    Inc(LineData[i].x2, Random(5) - 1);
    Inc(LineData[i].y2, Random(5) - 1);
  end;

  SetFillStyle(5, Cyan);
  SetColor(Cyan);
  Line(20 + Angle, 20, MaxX - 20 - Angle, 20);

  SetColor(Red);
  Circle(BallX, BallY, 20);
  SetFillStyle(1, Red);
  FillEllipse(BallX, BallY, 20, 20);

  SetColor(LightRed);
  PieSlice(MidX, MaxY - 200, Angle, Angle + 90, 100);

  DrawRobot(RobotX, RobotY);
end;

procedure UpdateState;
begin
  Inc(Angle, 5);
  if Angle >= 360 then Angle := 0;

  Inc(BallX, BallDX);
  Inc(BallY, BallDY);
  if (BallX < 20) or (BallX > MaxX - 20) then BallDX := -BallDX;
  if (BallY < 20) or (BallY > MaxY - 20) then BallDY := -BallDY;

  Inc(RobotX, RobotSpeed);
  if RobotX > MaxX then RobotX := -50;

  Inc(LaserTimer);
  if LaserTimer mod 60 < 20 then
    LaserActive := True
  else
    LaserActive := False;
end;

begin
  Init;
  Angle := 0;
  //Delay(15000);

  repeat
    ClearDevice;
    DrawStaticPrimitives;
    DrawAnimatedOverlay;
    UpdateState;
    {$if FPC_fullVersion >= 20701}
    FlipDisplay;
    {$endif}
    Delay(FrameDelay);
  until KeyPressed;

  CloseGraph;
end.

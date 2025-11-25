unit GraphAllegro5Port;

{$mode objfpc}{$H+}

{*
Multiplatform (DOS, M$, UNIX/*BSD, Linux, Mac OS X, iOS, and Android) port 
for "Graph" Turbo Pascal graphic library based on "Allegro5" graphic library.

It is prepared for FPC (Free Pascal Compiler).

Additional materials:
 - Allegro5 sources: https://github.com/liballeg/allegro5
 - Allegro5 Platform-specific functions: https://liballeg.org/a5docs/trunk/platform.html
 - Allegro5 addons: https://liballeg.org/a5docs/trunk/index.html#addons
*}

interface

uses
  allegro5, al5primitives, al5font, al5ttf, SysUtils;

const
  Black = 0;
  Blue = 1;
  Green = 2;
  Cyan = 3;
  Red = 4;
  Magenta = 5;
  Brown = 6;
  LightGray = 7;
  DarkGray = 8;
  LightBlue = 9;
  LightGreen = 10;
  LightCyan = 11;
  LightRed = 12;
  LightMagenta = 13;
  Yellow = 14;
  White = 15;

{*
  grError

    

  Error: Unknown error.

  grFileNotFound

    

  Error: File for driver not found.

  grFontNotFound

    

  Error: font description file not found.

  grInvalidDriver

    

  Error: Invalid driver specified

  grInvalidFont

    

  Error: Invalid font description

  grInvalidFontNum

    

  Error: Invalid font number

  grInvalidMode

    

  Error: Invalid mode specified.

  grInvalidVersion

    

  Error: Invalid version.

  grIOerror

    

  Error: Unspecified Input/Output error.

  grNoFloodMem

    

  Error: Could not allocate memory for flood operation.

  grNoFontMem

    

  Error: Not enough memory to load font.

  grNoInitGraph

    

  Error: Graphical system not initialized

  grNoLoadMem

    

  Error: Memory error.

  grNoScanMem

    

  Error: Could not allocate memory for scan

  grNotDetected

    

  Error: Graphics device not detected.

  grOk
*}
  grOK = 0;

  detectMode = 0;
  m640x480 = 30009;
  m800x600 = 30010;
  m1600x1200 = 30014;

{*
  Solidln = 0; //draws a solid line.
  Dottedln = 1; //Draws a dotted line.
  Centerln = 2; //draws a non-broken centered line.
  Dashedln = 3; //draws a dashed line.
  UserBitln = 4; //Draws a User-defined bit pattern.
*}

  NormWidth = 1;
  ThickWidth = 3;
  mr_yes = 1;
  mr_cancel = 2;
  sc_infoUpper = 0;
  sc_warningUpper = 1;

  Default = 0;
  DefaultFont = 0;
  HorizDir = 0;
  VertDir = 1;


type
  TMessageResult = (mrNone, mrYes, mrCancel);
  TMessageButtons = set of TMessageResult;
  TColorType = Word;
  TColor = ALLEGRO_COLOR;
  TFillStyle = (EmptyFill, SolidFill, LineFill);


function Detect(): Integer;
procedure DetectGraph(var GraphDriver: Integer; var GraphMode: Integer);
procedure InitGraph(var GraphDriver: Integer; var GraphMode: Integer; const PathToDriver: String);
procedure CloseGraph;
function GraphResult: Integer;

function GraphErrorMsg(ErrCode: Integer): String;

procedure SetColor(Color: Integer);
//procedure SetFillStyle(Style: TFillStyle);
procedure SetFillStyle(Pattern: Word; Color: TColorType);
procedure SetLineStyle(LineStyle, Pattern, LineWidth: Word);
procedure SetBkColor(Color: TColorType);

procedure Arc(x, y: Integer; stangle, endangle, radius: Word);
procedure Line(X1, Y1, X2, Y2: Integer);
procedure Rectangle(X1, Y1, X2, Y2: Integer);
procedure Bar(X1, Y1, X2, Y2: Integer);
procedure Circle(X, Y, Radius: Integer);
procedure Ellipse(X, Y, XRadius, YRadius: Integer);
procedure FillEllipse(X, Y, XRadius, YRadius: Integer);
procedure DrawPoly(NumPoints: Integer; Points: array of Integer);
procedure FillPoly(NumPoints: Integer; Points: array of Integer);
procedure OutTextXY(X, Y: Integer; Text: String);

function RegisterBGIFont(const FontPath: string; Size: Integer): Pointer;
procedure SetTextStyle(Font: Pointer; Direction, Size: Integer);

procedure ClearDevice;

procedure PieSlice(X, Y, StAngle, EndAngle, Radius: Integer);
procedure Bar3D(X1, Y1, X2, Y2, Depth: Integer; TopFlag: Boolean);
procedure Sector(X, Y, StAngle, EndAngle, XRadius, YRadius: Integer);

procedure MoveTo(X, Y: Integer);
procedure LineTo(X, Y: Integer);
procedure MoveRel(DX, DY: Integer);
procedure LineRel(DX, DY: Integer);

function GetMaxX: Integer;
function GetMaxY: Integer;


procedure FlipDisplay;

function GetScreenDimensions: Boolean;
procedure CenterWindowOnScreen(DisplayWidth, DisplayHeight: Integer);
procedure ResizeFont(FontPath: AnsiString; NewSize: Integer);


implementation


var
  Display: ALLEGRO_DISPLAYptr;
  DisplayWidth, DisplayHeight: Integer;
  ScreenWidth, ScreenHeight: Integer;
  CurrentColor: TColor;
  CurrentFillStylePattern: Word;
  Font: ALLEGRO_FONTptr;
  CurrentX, CurrentY: Integer;
  CurrentLineWidth: Word;
  LastErrorCode: Integer;
  LastGraphErrorMsg: String;
  ErrorMessages: array[0..8] of string = (
    'No error.',
    'Failed to initialize Allegro!',
    'Failed to initialize Allegro primitives addon!',
    'Failed to initialize Allegro font addon!',
    'Failed to initialize Allegro ttf addon!',
    'Failed to create display!',
    'Failed to create font!',
    'No monitors detected',
    'Failed to resize font'
  );

{*
----- INSIDE GRAPH LIBRARY
*}

function Detect(): Integer;
begin
  Result := 0;
end;

procedure DetectGraph(var GraphDriver: Integer; var GraphMode: Integer);
begin
  GraphDriver := 0;
  GraphMode := 0;
end;

procedure InitGraph(var GraphDriver: Integer; var GraphMode: Integer; const PathToDriver: String);
var
  X, Y: Integer;
begin
  LastErrorCode := 0; // Reset the error code
  //GraphicsDriver := 0;
  //GraphicsMode := 0;
  if not al_init then
  begin
    LastErrorCode := 1; // Error initializing Allegro
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;
  if not al_init_primitives_addon then
  begin
    LastErrorCode := 2; // Error initializing Allegro primitives addon
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;
  if not al_init_font_addon then
  begin
    LastErrorCode := 3; // Error initializing Allegro font addon
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;
  if not al_init_ttf_addon then
  begin
    LastErrorCode := 4; // Error initializing Allegro ttf addon
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;

  X := 640;
  Y := 480;

  case GraphMode of
    m640x480:
    begin
      X := 640;
      Y := 480;
    end;
    m800x600:
    begin
      X := 800;
      Y := 600;
    end;
    m1600x1200:
    begin
      X := 1600;
      Y := 1200;
    end;
  end;


  CenterWindowOnScreen(X, Y);

  CurrentColor := al_map_rgb(255, 255, 255); // Default to white
  Font := al_create_builtin_font();
  if Font = nil then
  begin
    LastErrorCode := 6; // Error creating font
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;
  CurrentX := 0;
  CurrentY := 0;
  //CurrentFillStyle := SolidFill;
  CurrentFillStylePattern := 0;
  CurrentLineWidth := NormWidth;
  CurrentColor := al_map_rgb(255, 255, 255); // Default to white
end;

procedure CloseGraph;
begin
  if Display <> nil then
    al_destroy_display(Display);
  if Font <> nil then
    al_destroy_font(Font);
end;

function GraphResult: Integer;
begin
  Result := LastErrorCode;
end;

function GraphErrorMsg(ErrCode: Integer): String;
begin
  if (ErrCode >= Low(ErrorMessages)) and (ErrCode <= High(ErrorMessages)) then
    Result := ErrorMessages[ErrCode]
  else
    Result := 'Unknown error code.';
end;

procedure SetColor(Color: Integer);
begin
  case Color of
    Black:       CurrentColor := al_map_rgb(0, 0, 0);
    Blue:        CurrentColor := al_map_rgb(0, 0, 255);
    Green:       CurrentColor := al_map_rgb(0, 255, 0);
    Cyan:        CurrentColor := al_map_rgb(0, 255, 255);
    Red:         CurrentColor := al_map_rgb(255, 0, 0);
    Magenta:     CurrentColor := al_map_rgb(255, 0, 255);
    Brown:       CurrentColor := al_map_rgb(165, 42, 42);
    LightGray:   CurrentColor := al_map_rgb(211, 211, 211);
    DarkGray:    CurrentColor := al_map_rgb(169, 169, 169);
    LightBlue:   CurrentColor := al_map_rgb(173, 216, 230);
    LightGreen:  CurrentColor := al_map_rgb(144, 238, 144);
    LightCyan:   CurrentColor := al_map_rgb(224, 255, 255);
    LightRed:    CurrentColor := al_map_rgb(255, 182, 193);
    LightMagenta:CurrentColor := al_map_rgb(255, 160, 122);
    Yellow:      CurrentColor := al_map_rgb(255, 255, 0);
    White:       CurrentColor := al_map_rgb(255, 255, 255);
  else
    CurrentColor := al_map_rgb(255, 255, 255);
  end;
end;

//procedure SetFillStyle(Style: TFillStyle);
procedure SetFillStyle(Pattern: Word; Color: TColorType);
begin
  //CurrentFillStyle := Style;
  CurrentFillStylePattern := Pattern;
  SetColor(Color);
end;

procedure SetLineStyle(LineStyle, Pattern, LineWidth: Word);
begin
  CurrentLineWidth := LineWidth;
end;

procedure SetBkColor(Color: TColorType);
begin
  SetColor(Color);
  al_clear_to_color(CurrentColor);
  //al_flip_display;
end;

procedure Arc(x, y: Integer; stangle, endangle, radius: Word);
const
  DEG_TO_RAD = Pi / 180;
  SEGMENT_STEP = 4;  { degrees between points — smaller = smoother }
var
  angle, radStart, radEnd: Double;
  px1, py1, px2, py2: Single;
begin
  radStart := stangle * DEG_TO_RAD;
  radEnd := endangle * DEG_TO_RAD;

  if radEnd < radStart then
    radEnd := radEnd + 2 * Pi;

  angle := radStart;
  while angle < radEnd do
  begin
    px1 := x + radius * cos(angle);
    py1 := y - radius * sin(angle);  { TP uses upward Y }
    px2 := x + radius * cos(angle + SEGMENT_STEP * DEG_TO_RAD);
    py2 := y - radius * sin(angle + SEGMENT_STEP * DEG_TO_RAD);

    al_draw_line(px1, py1, px2, py2, CurrentColor, CurrentLineWidth);
    angle := angle + SEGMENT_STEP * DEG_TO_RAD;
  end;
end;

procedure Line(X1, Y1, X2, Y2: Integer);
begin
  al_draw_line(X1, Y1, X2, Y2, CurrentColor, CurrentLineWidth);
  //al_flip_display;
end;

procedure Rectangle(X1, Y1, X2, Y2: Integer);
begin
  al_draw_rectangle(X1, Y1, X2, Y2, CurrentColor, CurrentLineWidth);
  //al_flip_display;
end;

procedure Bar(X1, Y1, X2, Y2: Integer);
begin
  al_draw_filled_rectangle(X1, Y1, X2, Y2, CurrentColor);
  //al_flip_display;
end;

procedure Circle(X, Y, Radius: Integer);
begin
  al_draw_circle(X, Y, Radius, CurrentColor, CurrentLineWidth);
  //al_flip_display;
end;

procedure Ellipse(X, Y, XRadius, YRadius: Integer);
begin
  al_draw_ellipse(X, Y, XRadius, YRadius, CurrentColor, CurrentLineWidth);
  //al_flip_display;
end;

procedure FillEllipse(X, Y, XRadius, YRadius: Integer);
begin
  al_draw_filled_ellipse(X, Y, XRadius, YRadius, CurrentColor);
  //al_flip_display;
end;

procedure DrawPoly(NumPoints: Integer; Points: array of Integer);
var
  i: Integer;
  Vertices: array of ALLEGRO_VERTEX;
begin
  SetLength(Vertices, NumPoints);
  for i := 0 to NumPoints - 1 do
  begin
    Vertices[i].x := Points[2 * i];
    Vertices[i].y := Points[2 * i + 1];
    Vertices[i].z := 0;
    Vertices[i].color := CurrentColor;
  end;
  al_draw_prim(Vertices, nil, 0, NumPoints, ALLEGRO_PRIM_LINE_LOOP);
  //al_flip_display;
end;

procedure FillPoly(NumPoints: Integer; Points: array of Integer);
var
  i: Integer;
  Vertices: array of ALLEGRO_VERTEX;
begin
  SetLength(Vertices, NumPoints);
  for i := 0 to NumPoints - 1 do
  begin
    Vertices[i].x := Points[2 * i];
    Vertices[i].y := Points[2 * i + 1];
    Vertices[i].z := 0;
    Vertices[i].color := CurrentColor;
  end;
  al_draw_prim(Vertices, nil, 0, NumPoints, ALLEGRO_PRIM_TRIANGLE_FAN);
  //al_flip_display;
end;

procedure OutTextXY(X, Y: Integer; Text: String);
begin
  al_draw_text(Font, CurrentColor, X, Y, ALLEGRO_ALIGN_LEFT, PChar(Text));
  ////al_flip_display;
end;

function RegisterBGIFont(const FontPath: string; Size: Integer): Pointer;
begin
  Result := al_load_ttf_font(PChar(FontPath), Size, 0);
  if Result = nil then
    raise Exception.Create('Failed to load font: ' + FontPath);
  Font := Result;
end;

procedure SetTextStyle(Font: Pointer; Direction, Size: Integer);
begin
  // Direction: 0 = HorizDir, 1 = VertDir (like BGI)
  // Size is ignored here because it's set during RegisterBGIFont
  if Direction = 1 then
    al_set_target_bitmap(al_get_backbuffer(al_get_current_display));  // Needed for rotation
  // Allegro doesn't use SetTextStyle directly — you rotate text manually if needed
end;

procedure ClearDevice;
begin
  al_clear_to_color(al_map_rgb(0, 0, 0));
  //al_flip_display;
end;

procedure PieSlice(X, Y, StAngle, EndAngle, Radius: Integer);
begin
  al_draw_pieslice(X, Y, Radius, StAngle * ALLEGRO_PI / 180, (EndAngle - StAngle) * ALLEGRO_PI / 180, CurrentColor, CurrentLineWidth);
  //al_flip_display;
end;

procedure Bar3D(X1, Y1, X2, Y2, Depth: Integer; TopFlag: Boolean);
begin
  al_draw_filled_rectangle(X1, Y1, X2, Y2, CurrentColor);
  al_draw_filled_rectangle(X1, Y1 - Depth, X2, Y2 - Depth, CurrentColor);
  al_draw_line(X1, Y1, X1, Y1 - Depth, CurrentColor, CurrentLineWidth);
  al_draw_line(X2, Y1, X2, Y1 - Depth, CurrentColor, CurrentLineWidth);
  al_draw_line(X1, Y2, X1, Y2 - Depth, CurrentColor, CurrentLineWidth);
  al_draw_line(X2, Y2, X2, Y2 - Depth, CurrentColor, CurrentLineWidth);
  if TopFlag then
    al_draw_filled_rectangle(X1, Y1 - Depth, X2, Y2 - Depth, CurrentColor);
  //al_flip_display;
end;

procedure Sector(X, Y, StAngle, EndAngle, XRadius, YRadius: Integer);
begin
  al_draw_filled_pieslice(X, Y, XRadius, StAngle * ALLEGRO_PI / 180, (EndAngle - StAngle) * ALLEGRO_PI / 180, CurrentColor);
  //al_flip_display;
end;

procedure MoveTo(X, Y: Integer);
begin
  CurrentX := X;
  CurrentY := Y;
end;

procedure LineTo(X, Y: Integer);
begin
  al_draw_line(CurrentX, CurrentY, X, Y, CurrentColor, CurrentLineWidth);
  //al_flip_display;
  CurrentX := X;
  CurrentY := Y;
end;

procedure MoveRel(DX, DY: Integer);
begin
  CurrentX := CurrentX + DX;
  CurrentY := CurrentY + DY;
end;

procedure LineRel(DX, DY: Integer);
var
  NewX, NewY: Integer;
begin
  NewX := CurrentX + DX;
  NewY := CurrentY + DY;
  al_draw_line(CurrentX, CurrentY, NewX, NewY, CurrentColor, CurrentLineWidth);
  //al_flip_display;
  CurrentX := NewX;
  CurrentY := NewY;
end;

function GetMaxX: Integer;
begin
  if Display <> nil then
    Result := al_get_display_width(Display)
  else
    Result := -1;  // Return -1 if the display is not initialized
end;

function GetMaxY: Integer;
begin
  if Display <> nil then
    Result := al_get_display_height(Display)
  else
    Result := -1;  // Return -1 if the display is not initialized
end;

{*
----- OUTSIDE GRAPH LIBRARY - ALLEGRO5 SPECIFIC
*}

procedure FlipDisplay();
begin
  al_flip_display();
end;

function GetScreenDimensions(): Boolean;
var
  MonitorsCount: Integer;
  MonitorIndex: Integer;
  MonitorIndexSelected: Integer;
  MonitorInfo: ALLEGRO_MONITOR_INFO;
begin
  MonitorsCount := al_get_num_video_adapters();

  if MonitorsCount = 0 then
  begin
    LastErrorCode := 7; // No monitors detected
    raise Exception.Create(GraphErrorMsg(LastErrorCode));
  end;

  {Writeln('Number of monitors detected: ', MonitorsCount);}
  
  for MonitorIndex := 0 to MonitorsCount - 1 do
  begin
    if al_get_monitor_info(MonitorIndex, MonitorInfo) then
    begin
      MonitorIndexSelected := MonitorIndex;
      {Writeln('[INFO] Monitor ', MonitorIndex, ':');}
      {Writeln('[INFO]   Position: (', MonitorInfo.x1, ', ', MonitorInfo.y1, ') - (', MonitorInfo.x2, ', ', MonitorInfo.y2, ')');}
    end
    else
    begin
      Writeln('[ERROR] Failed to get info for monitor ', MonitorIndex);
      Halt(1);
    end;
  end;

  if MonitorIndexSelected >= MonitorsCount then
  begin
    Writeln('[ERROR] Invalid monitor index!');
    Halt(1);
  end;

  al_set_new_display_adapter(MonitorIndexSelected);

  if al_get_monitor_info(MonitorIndexSelected, MonitorInfo) then
  begin
    ScreenWidth := MonitorInfo.x2 - MonitorInfo.x1;
    ScreenHeight := MonitorInfo.y2 - MonitorInfo.y1;
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

procedure CenterWindowOnScreen(DisplayWidth, DisplayHeight: Integer);
begin  
  if GetScreenDimensions then
  begin
    al_set_new_display_flags(ALLEGRO_WINDOWED);
    Display := al_create_display(DisplayWidth, DisplayHeight);
    if Display = nil then
    begin
      LastErrorCode := 5; // Error creating display
      raise Exception.Create(GraphErrorMsg(LastErrorCode));
    end;

    al_set_window_position(Display, (ScreenWidth - DisplayWidth) div 2, (ScreenHeight - DisplayHeight) div 2);
  end
  else
  begin
    raise Exception.Create('Failed to get screen dimensions');
  end;
end;

procedure ResizeFont(FontPath: AnsiString; NewSize: Integer);
begin
  {Writeln('[DEBUG] FontPath:', FontPath);}
  Font := al_load_font(FontPath, NewSize, 0);
  if Font = nil then
    begin
      LastErrorCode := 8; // Failed to resize font
      raise Exception.Create(GraphErrorMsg(LastErrorCode));
    end;
end;

end.

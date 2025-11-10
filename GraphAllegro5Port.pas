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

@Author: Marcin Bielak
@Source: https://github.com/bieli/GraphAllegro5PortPascal
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
procedure ClearDevice;

procedure SetBkColor(Color: TColorType);

function GetMaxX: Integer;
function GetMaxY: Integer;

procedure OutTextXY(X, Y: Integer; Text: String);
function RegisterBGIFont(const FontPath: string; Size: Integer): Pointer;
procedure SetTextStyle(Font: Pointer; Direction, Size: Integer);

procedure FlipDisplay;

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

function GraphErrorMsg(EGraphErrorMsgrrCode: Integer): String;
begin
  if (ErrCode >= Low(ErrorMessages)) and (ErrCode <= High(ErrorMessages)) then
    Result := ErrorMessages[ErrCode]
  else
    Result := 'Unknown error code.';
end;

procedure ClearDevice;
begin
  al_clear_to_color(al_map_rgb(0, 0, 0));
  //al_flip_display;
end;

procedure SetBkColor(Color: TColorType);
begin
  SetColor(Color);
  al_clear_to_color(CurrentColor);
  //al_flip_display;
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

{*
----- OUTSIDE GRAPH LIBRARY - ALLEGRO5 SPECIFIC
*}

procedure FlipDisplay();
begin
  al_flip_display();
end;

end.

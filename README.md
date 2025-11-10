# GraphAllegro5PortPascal
Multiplatform (DOS, M$, UNIX/*BSD, Linux, Mac OS X, iOS, and Android) port  for "Graph" Turbo Pascal graphic library based on "Allegro5" graphic library.

![CI status](https://github.com/bieli/GraphAllegro5PortPascal/actions/workflows/build.yml/badge.svg)

## Motivation
While browsing through my archive disks, I discovered some old Pascal source files from the time I was learning Turbo Pascal on IBM PC machines back in the 1990s. :-)

Since all my Pascal programs were created during my school years — and I had a lot of fun learning the language — I decided to write a port of the Graph library to run my classic Turbo Pascal programs on Linux. 

I chose the [**Allegro5**](https://liballeg.org/) library because portability is essential for reproducing older Pascal programs.

```c
   █████████                                █████        █████████   ████  ████                                       ██████████ ███████████                      █████    ███████████                                       ████ 
  ███▒▒▒▒▒███                              ▒▒███        ███▒▒▒▒▒███ ▒▒███ ▒▒███                                      ▒███▒▒▒▒▒▒█▒▒███▒▒▒▒▒███                    ▒▒███    ▒▒███▒▒▒▒▒███                                     ▒▒███ 
 ███     ▒▒▒  ████████   ██████   ████████  ▒███████   ▒███    ▒███  ▒███  ▒███   ██████   ███████ ████████   ██████ ▒███     ▒  ▒███    ▒███  ██████  ████████  ███████   ▒███    ▒███  ██████    █████   ██████   ██████   ▒███ 
▒███         ▒▒███▒▒███ ▒▒▒▒▒███ ▒▒███▒▒███ ▒███▒▒███  ▒███████████  ▒███  ▒███  ███▒▒███ ███▒▒███▒▒███▒▒███ ███▒▒███▒█████████  ▒██████████  ███▒▒███▒▒███▒▒███▒▒▒███▒    ▒██████████  ▒▒▒▒▒███  ███▒▒   ███▒▒███ ▒▒▒▒▒███  ▒███ 
▒███    █████ ▒███ ▒▒▒   ███████  ▒███ ▒███ ▒███ ▒███  ▒███▒▒▒▒▒███  ▒███  ▒███ ▒███████ ▒███ ▒███ ▒███ ▒▒▒ ▒███ ▒███▒▒▒▒▒▒▒▒███ ▒███▒▒▒▒▒▒  ▒███ ▒███ ▒███ ▒▒▒   ▒███     ▒███▒▒▒▒▒▒    ███████ ▒▒█████ ▒███ ▒▒▒   ███████  ▒███ 
▒▒███  ▒▒███  ▒███      ███▒▒███  ▒███ ▒███ ▒███ ▒███  ▒███    ▒███  ▒███  ▒███ ▒███▒▒▒  ▒███ ▒███ ▒███     ▒███ ▒███ ███   ▒███ ▒███        ▒███ ▒███ ▒███       ▒███ ███ ▒███         ███▒▒███  ▒▒▒▒███▒███  ███ ███▒▒███  ▒███ 
 ▒▒█████████  █████    ▒▒████████ ▒███████  ████ █████ █████   █████ █████ █████▒▒██████ ▒▒███████ █████    ▒▒██████ ▒▒████████  █████       ▒▒██████  █████      ▒▒█████  █████       ▒▒████████ ██████ ▒▒██████ ▒▒████████ █████
  ▒▒▒▒▒▒▒▒▒  ▒▒▒▒▒      ▒▒▒▒▒▒▒▒  ▒███▒▒▒  ▒▒▒▒ ▒▒▒▒▒ ▒▒▒▒▒   ▒▒▒▒▒ ▒▒▒▒▒ ▒▒▒▒▒  ▒▒▒▒▒▒   ▒▒▒▒▒███▒▒▒▒▒      ▒▒▒▒▒▒   ▒▒▒▒▒▒▒▒  ▒▒▒▒▒         ▒▒▒▒▒▒  ▒▒▒▒▒        ▒▒▒▒▒  ▒▒▒▒▒         ▒▒▒▒▒▒▒▒ ▒▒▒▒▒▒   ▒▒▒▒▒▒   ▒▒▒▒▒▒▒▒ ▒▒▒▒▒ 
                                  ▒███                                                    ███ ▒███                                                                                                                                
                                  █████                                                  ▒▒██████                                                                                                                                 
                                 ▒▒▒▒▒                                                    ▒▒▒▒▒▒                                                                                           
```

## Implemented procedures / functions from Graph library

```pascal
function Detect(): Integer;
procedure DetectGraph(var GraphDriver: Integer; var GraphMode: Integer);
procedure InitGraph(var GraphDriver: Integer; var GraphMode: Integer; const PathToDriver: String);
procedure CloseGraph;
function GraphResult: Integer;

function GraphErrorMsg(ErrCode: Integer): String;

procedure SetColor(Color: Integer);
procedure SetFillStyle(Pattern: Word; Color: TColorType);
procedure SetLineStyle(LineStyle, Pattern, LineWidth: Word);
procedure SetBkColor(Color: TColorType);

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
```

Additional Allegro5 specific:
```pascal

procedure FlipDisplay;

function GetScreenDimensions: Boolean;
procedure CenterWindowOnScreen(DisplayWidth, DisplayHeight: Integer);
procedure ResizeFont(FontPath: AnsiString; NewSize: Integer);
```

### How to start?
- FPC (Free Pascal Compiler) is installed on Linux OS
- Allegro5 developers bindings are already installed in Linux OS
- this repository is cloned to local Linux machine
- call `make` and run `example` program

#### Example commands from Linux Ubuntu/Debian
```bash
$ sudo apt install fpc
$ sudo apt install liballegro5-dev
$ make
$ ./example
```

#### Typical program flow
```pascal
Program TypicalProgramFlowExample

// ...

begin
  Init;

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
```

#### What you can see?
It will be new OS window with animations - it means, that example.pas and Graph port library compiled on your Linux machine.

![example program screen](example.gif)


## Additional sources
- [Allegro5 library reference manual](https://liballeg.org/a5docs/trunk/index.html)
- [Allegro5 C library sources](https://github.com/liballeg/allegro5)
- [Allegro5 Pascal wrapper](https://github.com/niuniomartinez/allegro-pas)

## TODO List
- [X] add first impl. and example program for DetectGraph and InitGraph procedures/functions
- [ ] - [ ] add impl. for typical graphics primitives like Line, Bar, Ract and others
- [ ] create and test CI for FPC example compiling
- [ ] add all graphics primitives demos in example program
- [ ] create instruction how to port any older program written in Turbo/Borland Pascal and Graph library

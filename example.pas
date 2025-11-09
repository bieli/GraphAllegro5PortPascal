program GraphExample;

{$mode objfpc}{$H+}

{*
One-line compile command:
$ fpc -Fu"${HOME}/fpcupdeluxe/ccr/allegro-pas/src/lib/" example.pas
*}

Uses Crt,
{$if FPC_fullVersion >= 20701}
GraphAllegro5Port
{$else}
Graph
{$endif}
;

var
  GraphicsDriver, GraphicsMode, ErrCode: Integer;

begin
  {$if FPC_fullVersion >= 20701}
    GraphicsMode := m640x480;
  {$else}
    GraphicsMode := 0;
  {$endif}

  GraphicsDriver := Detect();
  InitGraph(GraphicsDriver, GraphicsMode, 'c:\tp70\bgi');
  ErrCode := GraphResult();
  If ErrCode <> grOK then
  Begin
    ClrScr();
    Writeln('Graphics error occured: ', GraphErrorMsg(ErrCode));
    Writeln('If a file not found error is displayed above');
    Writeln('then, change the dir from the current');
    Writeln('location to C:\ -> TP -> BGI, from the file menu!');
    Readln;
    Halt(1);
  End;

  ClearDevice();

  SetBkColor(Blue);

  // NOT SPECIFIC FOR 'Graph' LIBRARY, BUT REQUIRED TO CALLING IN THIS PORT!
  {$if FPC_fullVersion >= 20701}
  FlipDisplay();
  {$endif}

  ReadLn; // Wait for user input before closing
  CloseGraph();
end.

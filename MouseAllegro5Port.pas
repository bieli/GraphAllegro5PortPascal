unit MouseAllegro5Port;

{$mode objfpc}{$H+}

interface

uses
  allegro5;

type
  TMouseEvent = record
    x, y: Integer;
    buttons: Integer;
    eventType: Integer;
  end;

const
  ME_NONE          = 0;
  ME_MOUSE_MOVE    = 1;
  ME_BUTTON_DOWN   = 2;
  ME_BUTTON_UP     = 3;
  ME_DISPLAY_CLOSE = 4;

  MouseLeftButton   = 1;
  MouseRightButton  = 2;
  MouseMiddleButton = 3;

var
  EventQueue: ALLEGRO_EVENT_QUEUEptr;

procedure InitMouse;
function PollMouseEvent(out me: TMouseEvent): Boolean;
procedure CloseMouse;

implementation


procedure InitMouse;
begin
  if not al_install_mouse then
  begin
    WriteLn('[ERROR] MouseAllegro5Port - Problem with call al_install_mouse() from Allegro5 library or mouse is not installed!');
    Halt(1);
  end;

  EventQueue := al_create_event_queue;
  al_register_event_source(EventQueue, al_get_mouse_event_source);
end;

function PollMouseEvent(out me: TMouseEvent): Boolean;
var
  ev: ALLEGRO_EVENT;
begin
  Result := False;
  if EventQueue = nil then Exit;

  if al_get_next_event(EventQueue, ev) then
  begin
    me.x := ev.mouse.x;
    me.y := ev.mouse.y;
    me.buttons := ev.mouse.button;

    case ev.ftype of
      ALLEGRO_EVENT_MOUSE_AXES:
        me.eventType := ME_MOUSE_MOVE;
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        me.eventType := ME_BUTTON_DOWN;
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        me.eventType := ME_BUTTON_UP;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        me.eventType := ME_DISPLAY_CLOSE;
    else
      me.eventType := ME_NONE;
    end;

    Result := True;
  end;
end;

procedure CloseMouse;
begin
  if EventQueue <> nil then
  begin
    al_destroy_event_queue(EventQueue);
    EventQueue := nil;
  end;
end;

end.


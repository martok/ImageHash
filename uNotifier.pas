unit uNotifier;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type    
  TUpdateUIEvent = procedure of object;

  { TThreadStatusNotifier }

  TThreadStatusNotifier = class
  private
    fOnUpdateUIEvent: TUpdateUIEvent;
    fMessagePending: integer;
    fTotalCount: integer;
    fLoaded: integer;
    fClassified: integer;
  protected
    procedure MainNotify;
  public
    procedure StartProcess(aTotalFiles: Integer);
    procedure NotifyHashProgress;
    procedure NotifyClassfierProgress;

    property TotalCount: integer read fTotalCount write fTotalCount;
    property Loaded: integer read fLoaded write fLoaded;
    property Classified: integer read fClassified write fClassified;

    property OnUpdateUIEvent: TUpdateUIEvent read fOnUpdateUIEvent write fOnUpdateUIEvent;
  end;


implementation

{ TThreadStatusNotifier }

procedure TThreadStatusNotifier.StartProcess(aTotalFiles: Integer);
begin
  fTotalCount:= aTotalFiles;
  fLoaded:= 0;
  fClassified:= 0;
  fMessagePending:= 0;
  TThread.ForceQueue(Nil, @MainNotify);
end;

procedure TThreadStatusNotifier.NotifyHashProgress;
begin
  Inc(fLoaded);
  if InterlockedCompareExchange(fMessagePending, 1, 0) = 0 then
    TThread.ForceQueue(Nil, @MainNotify);
end;

procedure TThreadStatusNotifier.NotifyClassfierProgress;
begin
  Inc(fClassified);
  if InterlockedCompareExchange(fMessagePending, 1, 0) = 0 then
    TThread.ForceQueue(Nil, @MainNotify);
end;  

procedure TThreadStatusNotifier.MainNotify;
begin
  InterlockedExchange(fMessagePending, 0);
  if Assigned(fOnUpdateUIEvent) then
    fOnUpdateUIEvent();
end;

end.


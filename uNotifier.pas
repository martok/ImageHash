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
    fDispatchBlocked: integer;
    fTotalCount: integer;
    fLoaded: integer;
    fClassified: integer;
  protected
    procedure MainNotify;
    procedure MaybeDispatchNotify;
  public
    procedure StartProcess(aTotalFiles: Integer);
    procedure NotifyHashProgress;
    procedure NotifyClassfierProgress;
    procedure BlockDispatch;
    procedure EnableDispatch;

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
  fDispatchBlocked:= 0;
  TThread.ForceQueue(Nil, @MainNotify);
end;

procedure TThreadStatusNotifier.NotifyHashProgress;
begin
  Inc(fLoaded);
  MaybeDispatchNotify;
end;

procedure TThreadStatusNotifier.NotifyClassfierProgress;
begin
  Inc(fClassified);  
  MaybeDispatchNotify;
end;      

procedure TThreadStatusNotifier.MainNotify;
begin
  if Assigned(fOnUpdateUIEvent) then
    fOnUpdateUIEvent();
  // if dispatch is blocked, don't clear the counter so EnableDispatch re-queues an event later
  if fDispatchBlocked = 0 then
    InterlockedExchange(fMessagePending, 0);
end;

procedure TThreadStatusNotifier.BlockDispatch;
begin
  InterlockedExchange(fDispatchBlocked, 1);
end;

procedure TThreadStatusNotifier.EnableDispatch;
begin
  InterlockedExchange(fDispatchBlocked, 0);
  if InterlockedExchange(fMessagePending, 0) > 1 then
    TThread.ForceQueue(Nil, @MainNotify);
end;

procedure TThreadStatusNotifier.MaybeDispatchNotify;
begin
  if (InterlockedExchangeAdd(fMessagePending, 1) = 0) and
     (fDispatchBlocked = 0) then
    TThread.ForceQueue(Nil, @MainNotify);
end;

end.


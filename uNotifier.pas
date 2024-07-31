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
    fLoaderStarted: QWord;
    fClassifierStarted: QWord;
    fTotalCount: integer;
    fLoaded: integer;
    fClassified: integer;
  protected
    procedure MainNotify;
    procedure MaybeDispatchNotify;
  public
    procedure Reset;
    procedure StartHash(aTotalFiles: Integer);
    procedure NotifyHashProgress;     
    procedure NotifyHashDone;
    procedure StartClassifier(aTotalFiles: Integer);
    procedure NotifyClassfierProgress;
    procedure NotifyClassfierDone;
    procedure BlockDispatch;
    procedure EnableDispatch;

    property LoaderStarted: QWord read fLoaderStarted;
    property ClassifierStarted: QWord read fClassifierStarted;
    property TotalCount: integer read fTotalCount write fTotalCount;
    property Loaded: integer read fLoaded write fLoaded;
    property Classified: integer read fClassified write fClassified;

    property OnUpdateUIEvent: TUpdateUIEvent read fOnUpdateUIEvent write fOnUpdateUIEvent;
  end;

const
  NOT_RUNNING = QWord(-1);


implementation

{ TThreadStatusNotifier }


procedure TThreadStatusNotifier.Reset;
begin
  fTotalCount:= 0;
  fLoaded:= 0;
  fClassified:= 0;
  fLoaderStarted:= 0;
  fClassifierStarted:= 0;
  fMessagePending:= 0;
  fDispatchBlocked:= 0;
end;

procedure TThreadStatusNotifier.StartHash(aTotalFiles: Integer);
begin
  fTotalCount:= aTotalFiles;
  fLoaded:= 0;
  fLoaderStarted:= GetTickCount64;
end;

procedure TThreadStatusNotifier.StartClassifier(aTotalFiles: Integer);
begin
  fTotalCount:= aTotalFiles;
  fClassified:= 0;
  fClassifierStarted:= GetTickCount64;
end;

procedure TThreadStatusNotifier.NotifyHashProgress;
begin
  Inc(fLoaded);
  MaybeDispatchNotify;
end;

procedure TThreadStatusNotifier.NotifyHashDone;
begin
  fLoaderStarted:= NOT_RUNNING;
  MaybeDispatchNotify;
end;

procedure TThreadStatusNotifier.NotifyClassfierProgress;
begin
  Inc(fClassified);  
  MaybeDispatchNotify;
end;

procedure TThreadStatusNotifier.NotifyClassfierDone;
begin
  fClassifierStarted:= NOT_RUNNING;
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


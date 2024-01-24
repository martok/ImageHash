unit uThreadClassifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, fgl, IntegerList, uThreadHashing;

type
  TCluster = TIntegerList;
  TClusterList = specialize TFPGList<TCluster>;

  { TClassifierThread }

  TClassifierThread = class(TThread)
  private
    fClustersLock: TCriticalSection;
    fCount: integer;
    fList: PImageInfoList;
    fWakeEvent: TEvent;
    fClusters: TClusterList;
    fLimit: integer;
    fImageFinishEvent: TImageFinishEvent;
    fMinDimension: integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property WakeEvent: TEvent read fWakeEvent;
    property Count: integer read fCount write fCount;
    property List: PImageInfoList read fList write fList;
    property Limit: integer read fLimit write fLimit;
    property MinDimension: integer read fMinDimension write fMinDimension;
    property Clusters: TClusterList read fClusters;

    procedure GetClusters(const aClusters: TClusterList);

    property OnImageFinish: TImageFinishEvent read fImageFinishEvent write fImageFinishEvent;
  end;

function hashWithinRange(i1, i2: PImageInfoItem; Limit: integer): boolean; //inline;

implementation

function hashWithinRange(i1, i2: PImageInfoItem; Limit: integer): boolean; //inline;
var
  h1000, h1090, h1180,
  h2000, h2090, h2180: QWord;
begin
  h1000:= i1^.Hash00;
  h1090:= i1^.Hash90;
  h1180:= i1^.Hash180;
  h2000:= i2^.Hash00;
  h2090:= i2^.Hash90;
  h2180:= i2^.Hash180;
  {
    4 kinds of relative orientations exist: 0, 90, 180, 270 °, assuming the orientation of i1 is upright.
    We use arithmetic checks to compensate for gradients, which dHash does badly (because < at 0° implies
    <= at 180° rotation, but is not). Arithmetic checks mean that excellent matches on one side compensate
    for that effect, with minimal false positives
  }
  // 0 deg relative error
  Result:= (
    PopCnt(h1000 xor h2000) +
    PopCnt(h1090 xor h2090) +
    PopCnt(h1180 xor h2180) <= 3*Limit
  );
  // 90 deg relative error
  Result:= Result or (
    PopCnt(h1000 xor h2090) +
    PopCnt(h1090 xor h2180) <= 2*Limit
  );
  // 180 deg relative error
  Result:= Result or (
    PopCnt(h1000 xor h2180) +
    PopCnt(h1180 xor h2000) <= 2*Limit
    // 90 point away from each other
  );
  // 270 deg relative error
  Result:= Result or (
    PopCnt(h1090 xor h2000) +
    PopCnt(h1180 xor h2090) <= 2*Limit
  );
end;



{ TClassifierThread }

constructor TClassifierThread.Create;
begin
  inherited Create(false);
  fList:= nil;
  fCount:= 0;
  fWakeEvent:= TSimpleEvent.Create;
  fClusters:= TClusterList.Create;
  fClustersLock:= TCriticalSection.Create;
  fLimit:= 3;
  fMinDimension:= 0;
end;

destructor TClassifierThread.Destroy;
var
  c: TCluster;
begin
  fClustersLock.Acquire;
  for c in fClusters do
    c.Free;
  FreeAndNil(fClusters);
  FreeAndNil(fClustersLock);
  FreeAndNil(fWakeEvent);
  inherited Destroy;
end;

procedure TClassifierThread.GetClusters(const aClusters: TClusterList);
var
  c: TCluster;
begin
  fClustersLock.Acquire;
  try
    for c in fClusters do begin
      if c.Count > 1 then
        aClusters.Add(c);
    end;
  finally
    fClustersLock.Release;
  end;
end;

function SortFunc_ClusterByLength(const Item1, Item2: TCluster): Integer;
begin
  Result:= Item2.Count - Item1.Count;
end;

procedure TClassifierThread.Execute; 

  function CheckInsertIntoCluster(c: TCluster; im: PImageInfoItem): boolean;
  var
    r: PImageInfoItem;
    i, miss: integer;
  begin
    miss:= 0;
    // match against all images in that cluster, one mismatch is enough to discard
    Result:= true;
    for i in c do begin
      r:= @fList[i];
      if not hashWithinRange(r, im, fLimit) then
        inc(miss);
    end;
    Result:= miss <= c.Count div 2;
  end;

var
  cursor: integer;
  im, r: PImageInfoItem;
  c: TCluster;
  i, ci: integer;
  matchclusters: TClusterList;
  chosenCluster: TCluster;
begin
  cursor:= 0;
  while not Terminated and (cursor < fCount) do begin
    if InterlockedCompareExchange(fList[cursor].Status, STATUS_DONE, STATUS_DONE) <> STATUS_DONE then begin
      fWakeEvent.ResetEvent;
      Sleep(10);
      fWakeEvent.WaitFor(100);
      Continue;
    end;
    im:= @fList[Cursor];
    if (im^.Error = '') and (im^.ImgW>=fMinDimension) and (im^.ImgH>=fMinDimension) then begin
      chosenCluster:= nil;
      fClustersLock.Acquire;
      matchclusters:= TClusterList.Create;
      try        
        // compare against all known clusters
        for c in fClusters do begin
          if CheckInsertIntoCluster(c, im) then 
            // potential candidate cluster
            matchclusters.Add(c);
        end;
        // did we find a candidate?
        if matchclusters.Count > 0 then begin
          // add to the largest of them
          matchclusters.Sort(@SortFunc_ClusterByLength);
          chosenCluster:= matchclusters[0];
          chosenCluster.Add(cursor);

          // see if we can now merge the other candidate groups to this (found missing link)
          for ci:= 1 to matchclusters.Count-1 do begin
            c:= matchclusters[ci];
            for i:= c.Count-1 downto 0 do begin
              r:= @fList[c[i]];
              if CheckInsertIntoCluster(chosenCluster, r) then begin
                // this image could have gone into chosenCluster
                chosenCluster.Add(c[i]);
                c.Delete(i);
              end;
            end;
            // did we find a new home for all items in that cluster?
            if c.Count = 0 then
              fClusters.Remove(c);
          end;
        end;
      finally
        FreeAndNil(matchclusters);
        fClustersLock.Release;
      end;

      if not Assigned(chosenCluster) then begin
        c:= TCluster.Create;
        c.Add(cursor);
        fClustersLock.Acquire;
        try
          fClusters.Add(c);
        finally
          fClustersLock.Release;
        end;
      end;  
    end;

    if Assigned(fImageFinishEvent) then
      Queue(fImageFinishEvent);
    inc(cursor);
  end;
end;

end.


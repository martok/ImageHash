unit uThreadClassifier;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, syncobjs, fgl, IntegerList, uThreadHashing, uNotifier;

type
  {
    Lockfree cluster classes based on 2 assumptions:
     - machineword writes are always atomic and serialized based on cache-coherency
     - dynarrays are refcounted, if changing the length changes the pointer, that change is also atomic
    TCluster is refcounted (same implementation as TInterfacedObject)
    TClusterList takes/releases ownership automatically
    TClusterList has one unavoidable short lock for safe iteration
  }

  TClusterList = class;

  TIndexArray = array of integer;
  TCluster = class
  private
    fRefCount,
    fDestroyCount: integer;
    function GetItem(Index: Integer): integer; inline;
  public
    Items: TIndexArray;
    class function NewInstance: TObject; override;
    constructor Create(aItems: TIndexArray);
    destructor Destroy; override;
    function Count: integer; inline;
    procedure AfterConstruction; override;
    function AddRefed: TCluster;
    function Release: integer;
    procedure Append(aItem: integer);
    procedure Delete(aIndex: integer);
    property Item[Index: Integer]: integer read GetItem; default;
    function CheckInsertIntoCluster(im: PImageInfoItem; aList: PImageInfoList; MaxDistance: integer): boolean;
  end;

  TClusterArray = array of TCluster;

  TClusterListEnumerator = record
  private
    fClusters: TClusterArray;
    fCurrentPosition: integer;
    Function GetCurrent: TCluster; inline;
  public
    Function MoveNext: Boolean; inline;
    property Current: TCluster read GetCurrent;
  end;

  TClusterList = class
  private
    fEnumeratorLock: TRTLCriticalSection;
    function GetItem(Index: Integer): TCluster;
  public
    Items: TClusterArray;
    constructor Create;
    destructor Destroy; override;
    function Count: integer; inline;
    procedure Append(aCluster: TCluster);
    procedure Delete(aIndex: integer);
    procedure Remove(aCluster: TCluster);   
    property Item[Index: Integer]: TCluster read GetItem; default;
    procedure SortByLength;
    // Enumerate AddRefed Clusters on a static copy of Items, must call .Release in loop body
    function GetEnumerator: TClusterListEnumerator;
  end;

  TClassifierThread = class(TThread)
  private
    fCount: integer;
    fList: PImageInfoList;
    fClusters: TClusterList;
    fLimit: integer;
    fMinDimension: integer;
    fNotifier: TThreadStatusNotifier;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read fCount write fCount;
    property List: PImageInfoList read fList write fList;
    property Limit: integer read fLimit write fLimit;
    property MinDimension: integer read fMinDimension write fMinDimension;
    property Clusters: TClusterList read fClusters;

    procedure GetClusters(const aClusters: TClusterList);

    property Notifier: TThreadStatusNotifier read fNotifier write fNotifier;
  end;

function hashWithinRange(i1, i2: PImageInfoItem; Limit: integer): boolean; //inline;

implementation

uses
  sortbase, uUtils;

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
    GetBitCount64(h1000 xor h2000) +
    GetBitCount64(h1090 xor h2090) +
    GetBitCount64(h1180 xor h2180) <= 3*Limit
  );
  // 90 deg relative error
  Result:= Result or (
    GetBitCount64(h1000 xor h2090) +
    GetBitCount64(h1090 xor h2180) <= 2*Limit
  );
  // 180 deg relative error
  Result:= Result or (
    GetBitCount64(h1000 xor h2180) +
    GetBitCount64(h1180 xor h2000) <= 2*Limit
    // 90 point away from each other
  );
  // 270 deg relative error
  Result:= Result or (
    GetBitCount64(h1090 xor h2000) +
    GetBitCount64(h1180 xor h2090) <= 2*Limit
  );
end;

{ TCluster }

class function TCluster.NewInstance: TObject;
begin
  Result:=inherited newinstance;  
  if NewInstance<>nil then
    TCluster(Result).fRefCount:=1;
end;

constructor TCluster.Create(aItems: TIndexArray);
begin
  inherited Create;
  Items:= aItems;
end;

destructor TCluster.Destroy;
begin             
  fRefCount:=0;
  fDestroyCount:=0;
  inherited Destroy;
end;

procedure TCluster.AfterConstruction;
begin
  InterlockedDecrement(fRefCount);
end;

function TCluster.AddRefed: TCluster;
begin
  if interlockedincrement(fRefCount) > 0 then
    Result:= Self
  else
    Result:= nil;
end;

function TCluster.Release: integer;
begin
  Result:=InterlockedDecrement(fRefCount);
  if Result=0 then begin
    if interlockedincrement(fDestroyCount)=1 then
      self.Destroy;
  end;
end; 

function TCluster.Count: integer;
begin
  Result:= Length(Items);
end;

procedure TCluster.Append(aItem: integer);
begin
  Insert(aItem, Items, Maxint);
end;

procedure TCluster.Delete(aIndex: integer);
begin
  System.Delete(Items, aIndex, 1);
end;

function TCluster.GetItem(Index: Integer): integer;
begin
  Result:= Items[Index];
end;

function TCluster.CheckInsertIntoCluster(im: PImageInfoItem; aList: PImageInfoList; MaxDistance: integer): boolean;
var
  r: PImageInfoItem;
  i, miss: integer;
begin
  miss:= 0;
  // match against all images in that cluster, one mismatch is enough to discard
  Result:= true;
  for i in Items do begin
    r:= @aList[i];
    if not hashWithinRange(r, im, MaxDistance) then
      inc(miss);
  end;
  Result:= miss <= Count div 2;
end;

{ TClusterListEnumerator }

function TClusterListEnumerator.GetCurrent: TCluster;
begin
  Result:= fClusters[fCurrentPosition];
end;

function TClusterListEnumerator.MoveNext: Boolean;
begin          
  Inc(fCurrentPosition);
  Result:= fCurrentPosition < Length(fClusters);
end;

{ TClusterList }

constructor TClusterList.Create;
begin
  Items:= nil;
  InitCriticalSection(fEnumeratorLock);
end;

destructor TClusterList.Destroy;
var
  i: Integer;
begin
  for i:= 0 to high(Items) do
    Items[i].Release;
  DoneCriticalSection(fEnumeratorLock);
  inherited Destroy;
end;

function TClusterList.Count: integer;
begin
  Result:= Length(Items);
end;

procedure TClusterList.Append(aCluster: TCluster);
begin
  Insert(aCluster.AddRefed, Items, Maxint);
end;

procedure TClusterList.Delete(aIndex: integer);
var
  it: TCluster;
begin
  EnterCriticalSection(fEnumeratorLock);
  try
    it:= Items[aIndex];
    System.Delete(Items, aIndex, 1);
    it.Release;
  finally
    LeaveCriticalSection(fEnumeratorLock);
  end;
end;

procedure TClusterList.Remove(aCluster: TCluster);
var
  i: Integer;
begin
  for i:= 0 to high(Items) do begin
    if Items[i] = aCluster then begin
      Delete(i);
      exit;
    end;
  end;
end;

function TClusterList.GetItem(Index: Integer): TCluster;
begin
  Result:= Items[Index];
end;

function SortFunc_ClusterByLength(Item1, Item2: TCluster): Integer;
begin
  Result:= Item2.Count - Item1.Count;
end;

procedure TClusterList.SortByLength;
var
  SortList: TClusterArray;
begin
  SortList:= Copy(Items);
  DefaultSortingAlgorithm^.PtrListSorter_NoContextComparer(@SortList[0],
                                                           Length(SortList),
                                                           TListSortComparer_NoContext(@SortFunc_ClusterByLength));
  Items:= SortList;
end;

function TClusterList.GetEnumerator: TClusterListEnumerator;
var
  lst: TClusterArray;
  i: integer;
begin
  EnterCriticalSection(fEnumeratorLock);
  try
    lst:= Items;
    for i:= 0 to high(lst) do
      lst[i].AddRefed;
  finally
    LeaveCriticalSection(fEnumeratorLock);
  end;
  Result.fClusters:= lst;
  Result.fCurrentPosition:= -1;
end;


{ TClassifierThread }

constructor TClassifierThread.Create;
begin
  inherited Create(false);
  fList:= nil;
  fCount:= 0;
  fClusters:= TClusterList.Create;
  fLimit:= 3;
  fMinDimension:= 0;
end;

destructor TClassifierThread.Destroy;
begin
  FreeAndNil(fClusters);
  inherited Destroy;
end;

procedure TClassifierThread.GetClusters(const aClusters: TClusterList);
var
  c: TCluster;
begin
  for c in fClusters do begin
    if c.Count > 1 then
      aClusters.Append(c);
    c.Release;
  end;
end;

procedure TClassifierThread.Execute; 
var
  cursor: integer;
  im, r: PImageInfoItem;
  chosenCluster: TCluster;
  c: TCluster;
  i, ci: integer;
  matchclusters: TClusterList;
begin   
  Priority:= tpLowest;

  cursor:= 0;
  while not Terminated and (cursor < fCount) do begin
    if InterlockedCompareExchange(fList[cursor].Status, STATUS_DONE, STATUS_DONE) <> STATUS_DONE then begin
      Sleep(50);
      Continue;
    end;
    im:= @fList[Cursor];
    if (im^.Error = '') and (im^.ImgW>=fMinDimension) and (im^.ImgH>=fMinDimension) then begin
      chosenCluster:= nil;
      matchclusters:= TClusterList.Create;
      try        
        // compare against all known clusters
        for c in fClusters do begin
          if c.CheckInsertIntoCluster(im, fList, fLimit) then
            // potential candidate cluster
            matchclusters.Append(c);
          c.Release;
        end;
        // did we find a candidate?
        if matchclusters.Count > 0 then begin
          // add to the largest of them
          matchclusters.SortByLength;
          chosenCluster:= matchclusters[0].AddRefed;
          chosenCluster.Append(cursor);

          // see if we can now merge the other candidate groups to this (found missing link)
          matchclusters.Delete(0);
          for c in matchclusters do begin
            for i:= c.Count-1 downto 0 do begin
              r:= @fList[c[i]];
              if chosenCluster.CheckInsertIntoCluster(r, fList, fLimit) then begin
                // this image could have gone into chosenCluster         
                chosenCluster.Append(c[i]);
                c.Delete(i);
              end;
            end;
            // did we find a new home for all items in that cluster?
            if c.Count = 0 then begin 
              fClusters.Remove(c);
            end;
            c.Release;
          end;
        end;
      finally
        FreeAndNil(matchclusters);
      end;

      if Assigned(chosenCluster) then
        chosenCluster.Release
      else begin
        c:= TCluster.Create([cursor]);
        fClusters.Append(c);
      end;
    end;

    fNotifier.NotifyClassfierProgress;
    inc(cursor);
  end;
  fNotifier.NotifyClassfierDone;
end;

end.


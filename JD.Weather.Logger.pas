unit JD.Weather.Logger;

(*
  JD Logger - A thread which processes application log entries from any thread.

  Usage: This unit works entirely by itself, and relies only on the Delphi RTL.
    It automatically creates an instance of the thread in initialization of
    this unit. It is not intended to create more than one instance.

  By default, no actual log is available. When you add log entries, but have
    not configured the logger to actually do something with those entries,
    then they will be discarded. Therefore, you must choose a method of
    actually using that log - depending on your purpose.

  Adding Log Entries:
  - Call the global procedure `PostLog` to input a new log entry.
    - Param `Level`: Any integer value which represents the severity
      level of the given log. What each number represents is up to you.
    - Param `Msg`: The actual message associated with the log.
    - Param `Timestamp`: The date/time log was created.
      - NOTE: This is actually automatic, and is not available when creating
        a log entry. Instead, it takes the current date/time.

  Using Log Entries:
  - Events: Assign an event handler to `Logger.OnLog`. Whenever a new log
    entry is found, it will trigger this event so that you may further
    use that log in any way you need, such as display.
  - Files: Assign a valid path\filename to `Logger.Filename` and enable
    the property `LogToFile`. Whenever a new log entry is found, it will
    save the log entry to the specified file.
  - Both: You can log the same entries to both a file and the event handler.

  NOTES:
  - Log entries are not immediately handled. Technically, every log entry is
    temporarily stored in a list, which is considered a buffer. This assumes
    that you may be adding log entries faster than those logs can actually
    be handled. This is actually the reason this is in a thread. The thread
    constantly checks this list, and processes each log in the order which
    it was originally added.
*)

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  System.Generics.Collections;

type
  TLogger = class;
  TLoggerItem = class;

  TLoggerEvent = procedure(Sender: TObject; const Log: TLoggerItem) of object;

  TLogger = class(TComponent)
  private
    FOnLog: TLoggerEvent;
    procedure SetFilename(const Value: TFilename);
    procedure SetLogToFile(const Value: Boolean);
    function GetFilename: TFilename;
    function GetLogToFile: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLog(const Level: Integer; const Msg: String);
  published
    property Filename: TFilename read GetFilename write SetFilename;
    property LogToFile: Boolean read GetLogToFile write SetLogToFile;
    property OnLog: TLoggerEvent read FOnLog write FOnLog;
  end;

  TLoggerItem = class(TObject)
  private
    FOrigin: TLogger;
    FLevel: Integer;
    FTimestamp: TDateTime;
    FMsg: String;
  public
    constructor Create(AOrigin: TLogger); overload;
    constructor Create(AOrigin: TLogger; const Level: Integer; const Timestamp: TDateTime;
      const Msg: String); overload;
    destructor Destroy; override;
  end;

implementation

type
  TLoggerThread = class;

  TLoggerThread = class(TThread)
  private
    FItems: TObjectList<TLoggerItem>;
    FComps: TObjectList<TLogger>;
    FFilename: TFilename;
    FLogToFile: Boolean;
    FOnLog: TLoggerEvent;
    FLogItem: TLoggerItem;
    FLogComp: TLogger;
    FLock: TCriticalSection;
    procedure SetFilename(const Value: TFilename);
    procedure SetLogToFile(const Value: Boolean);
    procedure RegisterComp(const AComp: TLogger);
    procedure UnregisterComp(const AComp: TLogger);
    procedure DoLog(const ALog: TLoggerItem);
  protected
    procedure Execute; override;
    procedure SYNC_OnLog;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddLog(AOrigin: TLogger; const Level: Integer; const Msg: String);
    property Filename: TFilename read FFilename write SetFilename;
    property LogToFile: Boolean read FLogToFile write SetLogToFile;
    property OnLog: TLoggerEvent read FOnLog write FOnLog;
  end;

var
  _Logger: TLoggerThread;

{ TLoggerItem }

constructor TLoggerItem.Create(AOrigin: TLogger);
begin
  FOrigin:= AOrigin;
end;

constructor TLoggerItem.Create(AOrigin: TLogger; const Level: Integer; const Timestamp: TDateTime;
  const Msg: String);
begin
  Create(AOrigin);
  FLevel:= Level;
  FTimestamp:= Timestamp;
  FMsg:= Msg;
end;

destructor TLoggerItem.Destroy;
begin

  inherited;
end;

{ TLoggerThread }

procedure TLoggerThread.AddLog(AOrigin: TLogger; const Level: Integer; const Msg: String);
var
  I: TLoggerItem;
begin
  I:= TLoggerItem.Create(AOrigin, Level, Now, Msg);
  FLock.Enter;
  try
    FItems.Add(I);
  finally
    FLock.Leave;
  end;
end;

constructor TLoggerThread.Create;
begin
  inherited Create(True);
  FLock:= TCriticalSection.Create;
  FItems:= TObjectList<TLoggerItem>.Create(True);
  FComps:= TObjectList<TLogger>.Create(False);
end;

destructor TLoggerThread.Destroy;
begin
  FLock.Enter;
  try
    FComps.Clear;
    FreeAndNil(FComps);
    FItems.Clear;
    FreeAndNil(FItems);
  finally
    FLock.Leave;
  end;
  FreeAndNil(FLock);
  inherited;
end;

procedure TLoggerThread.DoLog(const ALog: TLoggerItem);
var
  X: Integer;
begin
  FLock.Enter;
  try
    FLogItem:= ALog;
    for X := 0 to FComps.Count-1 do begin
      FLogComp:= FComps[X];
      Synchronize(SYNC_OnLog);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerThread.Execute;
var
  I: TLoggerItem;
begin
  while not Terminated do begin
    try
      FLock.Enter;
      try
        if FItems.Count > 0 then begin
          I:= FItems[0];
          try
            DoLog(I);
          finally
            FItems.Delete(0);
          end;
        end;
      finally
        FLock.Leave;
      end;
    except
      on E: Exception do begin

      end;
    end;
    Sleep(1);
  end;
end;

procedure TLoggerThread.SetFilename(const Value: TFilename);
begin
  FLock.Enter;
  try
    FFilename := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerThread.SetLogToFile(const Value: Boolean);
begin
  FLock.Enter;
  try
    FLogToFile := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerThread.SYNC_OnLog;
begin
  if Assigned(FLogComp) then
    if Assigned(FLogComp.FOnLog) then
      FLogComp.FOnLog(Self, FLogItem);
end;

procedure TLoggerThread.RegisterComp(const AComp: TLogger);
begin
  FLock.Enter;
  try
    FComps.Add(AComp);
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerThread.UnregisterComp(const AComp: TLogger);
begin
  FLock.Enter;
  try
    FComps.Delete(FComps.IndexOf(AComp));
  finally
    FLock.Leave;
  end;
end;

{ TLogger }

constructor TLogger.Create(AOwner: TComponent);
begin
  inherited;
  _Logger.RegisterComp(Self);

end;

destructor TLogger.Destroy;
begin

  _Logger.UnregisterComp(Self);
  inherited;
end;

procedure TLogger.AddLog(const Level: Integer; const Msg: String);
begin
  _Logger.AddLog(Self, Level, Msg);
end;

function TLogger.GetFilename: TFilename;
begin
  Result:= _Logger.Filename;
end;

function TLogger.GetLogToFile: Boolean;
begin
  Result:= _Logger.LogToFile;
end;

procedure TLogger.SetFilename(const Value: TFilename);
begin
  _Logger.Filename := Value;
end;

procedure TLogger.SetLogToFile(const Value: Boolean);
begin
  _Logger.LogToFile := Value;
end;

initialization
  _Logger:= TLoggerThread.Create;
  _Logger.Start;
finalization
  _Logger.Terminate;
  //If this is configured to show in a UI, it should terminate immediately.
  //Otherwise, if logging to a file, it should wait until finished.
  //TODO: Add option whether to wait or not
  if _Logger.FLogToFile then
    _Logger.WaitFor;
  FreeAndNil(_Logger);
end.

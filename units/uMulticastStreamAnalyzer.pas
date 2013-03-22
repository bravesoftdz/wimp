unit uMulticastStreamAnalyzer;

interface

uses
    System.Classes, IdBaseComponent, IdComponent, IdIPMCastBase, IdIPMCastClient,
    System.SysUtils, System.IniFiles, System.Contnrs, Windows, System.SyncObjs,
    IdGlobal, IdSocketHandle, USLogger;

type
    TPacketInfo = record
        PID: uint16;
        Counter: uint8;
        Microtime: int64;
        isOnlyAdaptation: boolean;
        hasError: boolean;
    end;

    TStreamInfo = class(TObject)
        m_uPID: uint16;
        m_rLastPacket: TPacketInfo;
        m_uiPacketsCount: uint64;
        m_uiErrorsCount: uint64;
    public
        constructor Create(PID: uint16);
    end;

    TMulticastStreamAnalyzer = class(TThread)
    protected
        m_sMulticastGroup: string;
        m_uiMulticastPort: uint16;

        m_iPerformanceFreq: int64;

        m_oMulticastClient: TIdIPMCastClient;

        procedure Execute; override;

        procedure MulticastClientRead(Sender: TObject; const AData: TArray<System.Byte>; ABinding: TIdSocketHandle);
        procedure MulticastClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);

        procedure Log(LogLevel: TLogLevel; Mess: string);

        function ParseMPEGTSPacket(var Packet: TArray<System.Byte>): TPacketInfo;
        procedure ProcessMPEGTSPacket(PacketInfo: TPacketInfo);
        procedure UpdateMPEGTSStreamInfo(PacketInfo: TPacketInfo);
    public
        m_slStreamsInfo: TStringList;
        m_oCriticalSection: TCriticalSection;

        constructor Create(MulticastGroup: string; MulticastPort: uint16);
        destructor Destroy; override;
    end;

implementation

uses
    uwMain;

{ StreamInfo }

constructor TStreamInfo.Create(PID: uint16);
begin
    m_uPID := PID;
end;

{ MulticastStreamAnalyzer }

constructor TMulticastStreamAnalyzer.Create(MulticastGroup: string; MulticastPort: uint16);
begin
    inherited Create;

    m_sMulticastGroup := MulticastGroup;
    m_uiMulticastPort := MulticastPort;

    if (not QueryPerformanceFrequency(m_iPerformanceFreq)) then
        Log(llError, 'Error while getting frequency information, timing may be wrong');

    m_slStreamsInfo := TStringList.Create;
    m_slStreamsInfo.Duplicates := dupError;
    m_slStreamsInfo.Sorted := true;
    m_slStreamsInfo.OwnsObjects := true;

    m_oCriticalSection := TCriticalSection.Create;
    m_oMulticastClient := TIdIPMCastClient.Create(nil);
end;

destructor TMulticastStreamAnalyzer.Destroy;
begin
    FreeAndNil(m_oMulticastClient);
    FreeAndNil(m_slStreamsInfo);
    FreeAndNil(m_oCriticalSection);

    Log(llDebug, 'Multicast Watcher stopped');
    inherited Destroy;
end;

procedure TMulticastStreamAnalyzer.Execute;
begin
    NameThreadForDebugging('MulticastStreamAnalyzer');

    m_oMulticastClient.OnIPMCastRead := MulticastClientRead;

    m_oMulticastClient.MulticastGroup := m_sMulticastGroup;
    m_oMulticastClient.DefaultPort := m_uiMulticastPort;
    m_oMulticastClient.ReuseSocket := rsTrue;
    m_oMulticastClient.ThreadedEvent := true;
    m_oMulticastClient.Active := true;

    Log(llDebug, 'Multicast Watcher started');
end;

procedure TMulticastStreamAnalyzer.MulticastClientRead(Sender: TObject; const AData: TArray<System.Byte>; ABinding: TIdSocketHandle);
var
    Packet: TArray<System.Byte>;
    PacketInfo: TPacketInfo;
    i, j: int32;
begin
    i := 0;
    j := i + 188;
    while (j <= Length(AData)) do begin
        Packet := Copy(AData, i, j + 1);
        PacketInfo := ParseMPEGTSPacket(Packet);
        ProcessMPEGTSPacket(PacketInfo);
        UpdateMPEGTSStreamInfo(PacketInfo);
        i := j;
        j := i + 188;
    end;
end;

procedure TMulticastStreamAnalyzer.MulticastClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
    //
end;

procedure TMulticastStreamAnalyzer.Log(LogLevel: TLogLevel; Mess: string);
begin
    Queue(
        procedure
        begin
            WMain.Log(LogLevel, Mess);
        end
    );
end;

function TMulticastStreamAnalyzer.ParseMPEGTSPacket(var Packet: TArray<System.Byte>): TPacketInfo;
var
    PacketInfo: TPacketInfo;
begin
    // Default
    PacketInfo.PID := 0;
    PacketInfo.hasError := false;
    PacketInfo.isOnlyAdaptation := false;

    // Check for packet length
    if (Length(Packet) < 188) then begin
        PacketInfo.hasError := true;
        Log(llDebug, 'Packet has wrong length');
    end;

    // Check for "MPEGTS" Mark
    if (Packet[0] <> $47) then begin
        PacketInfo.hasError := true;
        Log(llDebug, 'Packet has no MPEGTS mark');
    end;

    // Check for "Transport Error Flag"
    if ((Packet[1] and $80) <> 0) then begin
        PacketInfo.hasError := true;
        Log(llDebug, 'Transport error flag is set');
    end;

    // Check for "Payload Unit Start Indicator"
    if ((Packet[1] and $40) <> 0) then begin
        //Log(llDebug, 'payload_unit_start_indicator flag is set');
    end;

    // Check for "Transport Priority flag"
    if ((Packet[1] and $20) <> 0) then begin
        //Log(llDebug, 'transport priority flag is set');
    end;

    // Check for adaptation field
    if (((Packet[3] and $30) shr 4) = $02) then begin
        PacketInfo.isOnlyAdaptation := true;
        //Log(llDebug, 'Packet with adaptation field: ' + IntToStr((Packet[3] and $30) shr 4));
    end;

    // PID
    PacketInfo.PID := ((Packet[1] and $1F) shl 8) + Packet[2];

    // Counter
    PacketInfo.Counter := Packet[3] and $0F;

    // Time
    QueryPerformanceCounter(PacketInfo.Microtime);

    Result := PacketInfo;
end;

procedure TMulticastStreamAnalyzer.ProcessMPEGTSPacket(PacketInfo: TPacketInfo);
begin
    //
end;

procedure TMulticastStreamAnalyzer.UpdateMPEGTSStreamInfo(PacketInfo: TPacketInfo);
var
    StreamInfo: TStreamInfo;
    Index: Integer;
begin
    m_oCriticalSection.Enter;

    Index := m_slStreamsInfo.IndexOf(IntToHex(PacketInfo.PID, 4));

    if (Index = -1) then
        Index := m_slStreamsInfo.AddObject(IntToHex(PacketInfo.PID, 4), TStreamInfo.Create(PacketInfo.PID));

    StreamInfo := m_slStreamsInfo.Objects[Index] as TStreamInfo;

    Inc(StreamInfo.m_uiPacketsCount);

    if (PacketInfo.hasError) then
        Inc(StreamInfo.m_uiErrorsCount);

    if ((StreamInfo.m_rLastPacket.PID <> 0) and (not PacketInfo.isOnlyAdaptation)) then
        if (((StreamInfo.m_rLastPacket.Counter < $0F) and ((StreamInfo.m_rLastPacket.Counter + 1) <> PacketInfo.Counter))
        or ((StreamInfo.m_rLastPacket.Counter = $0F) and (PacketInfo.Counter <> 0))) then begin
            Inc(StreamInfo.m_uiErrorsCount);
            Log(llDebug, 'Error! PID = ' + IntToStr(PacketInfo.PID) + '; Last Counter = ' + IntToStr(StreamInfo.m_rLastPacket.Counter) + '; Counter = ' + IntToStr(PacketInfo.Counter));
        end;

    StreamInfo.m_rLastPacket := PacketInfo;

    m_oCriticalSection.Leave;
end;

end.

unit uMulticastStreamAnalyzer;

interface

uses
    System.Classes, IdBaseComponent, IdComponent, IdIPMCastBase, IdIPMCastClient,
    System.SysUtils, System.IniFiles, System.Contnrs, Windows, System.SyncObjs,
    IdGlobal, IdSocketHandle, USLogger;

const
    MPEGTS_NULLPID = $1FFF;

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
        m_bHaveError: boolean;
        m_bCanHaveError: boolean;
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

        m_uiPossibleErrors: uint8;

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

function PrintPacketError(PID: uint16; Counter: uint8): string;
begin
    Result := 'PID: #' + IntToHex(PID, 4) + '; Counter: ' + UIntToStr(Counter) + '.';
end;

constructor TStreamInfo.Create(PID: uint16);
begin
    m_uPID := PID;
    m_bHaveError := false;
    m_bCanHaveError := false;
    m_uiPacketsCount := 0;
    m_uiErrorsCount := 0;

    m_rLastPacket.PID := MPEGTS_NULLPID;
    m_rLastPacket.Counter := 0;
    m_rLastPacket.Microtime := 0;
end;

{ MulticastStreamAnalyzer }

constructor TMulticastStreamAnalyzer.Create(MulticastGroup: string; MulticastPort: uint16);
begin
    inherited Create;

    m_uiPossibleErrors := 0;

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
    if (Assigned(m_oMulticastClient)) then begin
        m_oMulticastClient.Active := false;
        FreeAndNil(m_oMulticastClient);
    end;

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
    m_oMulticastClient.BufferSize := 8196 * 4;
    m_oMulticastClient.Active := true;

    Log(llDebug, 'Multicast Watcher for group ' + m_sMulticastGroup + ' started.');
end;

procedure TMulticastStreamAnalyzer.MulticastClientRead(Sender: TObject; const AData: TArray<System.Byte>; ABinding: TIdSocketHandle);
var
    Packet: TArray<System.Byte>;
    PacketInfo: TPacketInfo;
    i, j: int32;
begin
    if (Length(AData) <> 1316) then
        Log(llDebug, 'Data size: ' + UIntToStr(Length(AData)));

    i := 0;
    j := i + 188;
    while (j <= Length(AData)) do begin
        Packet := Copy(AData, i, j + 1);
        PacketInfo := ParseMPEGTSPacket(Packet);
        if (PacketInfo.PID <> MPEGTS_NULLPID) then begin
            ProcessMPEGTSPacket(PacketInfo);
            UpdateMPEGTSStreamInfo(PacketInfo);
        end;
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
    PacketInfo.PID := MPEGTS_NULLPID;
    PacketInfo.hasError := false;
    PacketInfo.isOnlyAdaptation := false;

    // Check for packet length
    if (Length(Packet) < 188) then begin
        //Log(llDebug, 'Packet has wrong length');
        Exit;
    end;

    // Check for "MPEGTS" Mark
    if (Packet[0] <> $47) then begin
        //Log(llDebug, 'Packet has no MPEGTS mark');
        Exit;
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
    CanBeError: Boolean;
    Index, I: Integer;
    PresumablyErrorsCount: uint8;
begin
    m_oCriticalSection.Enter;

    Index := m_slStreamsInfo.IndexOf(IntToHex(PacketInfo.PID, 4));

    if (Index = -1) then
        Index := m_slStreamsInfo.AddObject(IntToHex(PacketInfo.PID, 4), TStreamInfo.Create(PacketInfo.PID));

    StreamInfo := m_slStreamsInfo.Objects[Index] as TStreamInfo;

    Inc(StreamInfo.m_uiPacketsCount);

    if (PacketInfo.hasError) then begin
        Inc(StreamInfo.m_uiErrorsCount);
        Log(llWarning, 'Network Error. Packet with PID = #' + IntToHex(PacketInfo.PID, 4) + ' has error flag.');
    end;

    // Если пакет уже помечен, как имеющий ошибку - сбрасываем флаг и пропускаем его
    if (StreamInfo.m_bHaveError) then begin
        StreamInfo.m_bHaveError := false;
        StreamInfo.m_bCanHaveError := false;
        //Log(llDebug, '1: ' + PrintPacketError(PacketInfo.PID, PacketInfo.Counter));
    end
    else
    // Обработка всех остальных пакетов
    if ((StreamInfo.m_rLastPacket.PID <> MPEGTS_NULLPID) and (not PacketInfo.isOnlyAdaptation)) then begin
        if not (((StreamInfo.m_rLastPacket.Counter + 1) = PacketInfo.Counter)
                or (StreamInfo.m_rLastPacket.Counter = PacketInfo.Counter)  // HACK! TODO: fix this for adopt-only packets
                or (StreamInfo.m_rLastPacket.Counter = (PacketInfo.Counter + $0F))) then begin

            // Находим предположительное количество ошибок от предыдущего пакета до текущего
            if (StreamInfo.m_rLastPacket.Counter < PacketInfo.Counter) then
                PresumablyErrorsCount := PacketInfo.Counter - (StreamInfo.m_rLastPacket.Counter + 1)
            else
                PresumablyErrorsCount := (PacketInfo.Counter + $0F) - StreamInfo.m_rLastPacket.Counter;

            // Если ошибок более 7 - был потерян не один пакет. Точнее сказать невозможно.
            if (PresumablyErrorsCount > 7) then
            begin
                // Считаем как 2 ошибки
                Inc(StreamInfo.m_uiErrorsCount, 2);
                // Маркируем все потоки для пропуска следующего пакета без ошибок
                for I := 0 to m_slStreamsInfo.Count - 1 do
                    (m_slStreamsInfo.Objects[I] as TStreamInfo).m_bHaveError := true;
                // Пишем в лог
                Log(llWarning, 'Packet sequence error. More than one packet with PID = #' + IntToHex(PacketInfo.PID, 4) + ' has wrong counter: ' + UIntToStr(PacketInfo.Counter) + ', must be: ' + UIntToStr(StreamInfo.m_rLastPacket.Counter + 1));
            end
            else
            // Если ошибок менее чем возможных ошибок потока - относим к уже обработанной ошибке
            if (m_uiPossibleErrors >= PresumablyErrorsCount) then begin
                // Уменьшаем количество возможных ошибок на количество предположительных ошибок
                m_uiPossibleErrors := m_uiPossibleErrors - PresumablyErrorsCount;
                // Помечаем данный поток как не имеющий ошибок
                StreamInfo.m_bCanHaveError := false;
            end
            else
            // Иначе - считаем необработанной ошибкой
            begin
                // Считаем ошибку
                Inc(StreamInfo.m_uiErrorsCount);
                // Выставляем количество возможных ошибок
                m_uiPossibleErrors := 7 - PresumablyErrorsCount;
                // Помечаем все потоки, кроме текущего, как возможно имеющие ошибки
                for I := 0 to m_slStreamsInfo.Count - 1 do
                    if ((m_slStreamsInfo.Objects[I] as TStreamInfo).m_uPID <> PacketInfo.PID) then
                        (m_slStreamsInfo.Objects[I] as TStreamInfo).m_bCanHaveError := true;
                // Пишем в лог
                //Log(llDebug, 'Possible errors: ' + UIntToStr(m_uiPossibleErrors) + '; Pres. Errors: ' + UIntToStr(PresumablyErrorsCount));
                Log(llWarning, 'Packet Error. Packet with PID = #' + IntToHex(PacketInfo.PID, 4) + ' has wrong counter: ' + UIntToStr(PacketInfo.Counter) + ', must be: ' + UIntToStr(StreamInfo.m_rLastPacket.Counter + 1));
            end;

            //Log(llDebug, 'Possible Errors Counter: ' + UIntToStr(m_uiPossibleErrors));
        end
        // Иначе - очищаем флаг возможных ошибок, пакет правильный
        else
            StreamInfo.m_bCanHaveError := false;
    end;

    // Если ни один поток не помечен, как возможно имеющий ошибку - очищаем счетчик возможных ошибок
    if (m_uiPossibleErrors > 0) then begin
        CanBeError := false;
        for I := 0 to m_slStreamsInfo.Count - 1 do
            if ((m_slStreamsInfo.Objects[I] as TStreamInfo).m_bCanHaveError) then begin
                CanBeError := true;
                //Log(llDebug, 'Stream with PID ' + IntToHex((m_slStreamsInfo.Objects[I] as TStreamInfo).m_uPID, 4) + ' can be with err');
            end;
        if (not CanBeError) then begin
            m_uiPossibleErrors := 0;
            //Log(llDebug, 'All streams clear');
        end;
    end
    else
    // Если мы уже нашли все возможные ошибки - очищаем флаги возможностей ошибок в потоках
    if (m_uiPossibleErrors = 0) then begin
        for I := 0 to m_slStreamsInfo.Count - 1 do
            (m_slStreamsInfo.Objects[I] as TStreamInfo).m_bCanHaveError := false;
    end;


    StreamInfo.m_rLastPacket := PacketInfo;

    m_oCriticalSection.Leave;
end;



end.

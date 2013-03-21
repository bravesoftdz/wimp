unit uMulticastStreamAnalyzer;

interface

uses
    System.Classes, IdBaseComponent, IdComponent, IdIPMCastBase, IdIPMCastClient,
    System.SysUtils, System.IniFiles, IdGlobal, IdSocketHandle, USLogger;

type
    TMulticastStreamAnalyzer = class(TThread)
    protected
        m_sMulticastGroup: string;
        m_uiMulticastPort: uint16;

        m_oMulticastClient: TIdIPMCastClient;
        m_slStreamsInfo: THashedStringList;

        procedure Execute; override;

        procedure MulticastClientRead(Sender: TObject; const AData: TArray<System.Byte>; ABinding: TIdSocketHandle);
        procedure MulticastClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);

        procedure Log(LogLevel: TLogLevel; Mess: string);

        procedure ParseMPEGTSPacket(var Packet: TArray<System.Byte>);
    public
        constructor Create(MulticastGroup: string; MulticastPort: uint16);
        destructor Destroy; override;
    end;

    TStreamInfo = class(TObject)
    end;

implementation

uses
    uwMain;

{ MulticastStreamAnalyzer }

constructor TMulticastStreamAnalyzer.Create(MulticastGroup: string; MulticastPort: uint16);
begin
    inherited Create;

    m_sMulticastGroup := MulticastGroup;
    m_uiMulticastPort := MulticastPort;

    m_oMulticastClient := TIdIPMCastClient.Create(nil);
end;

destructor TMulticastStreamAnalyzer.Destroy;
begin
    FreeAndNil(m_oMulticastClient);
    inherited Destroy;
end;

procedure TMulticastStreamAnalyzer.Execute;
begin
    NameThreadForDebugging('MulticastStreamAnalyzer');

    m_oMulticastClient.OnIPMCastRead := MulticastClientRead;

    m_oMulticastClient.MulticastGroup := m_sMulticastGroup;
    m_oMulticastClient.DefaultPort := m_uiMulticastPort;
    m_oMulticastClient.ReuseSocket := rsTrue;

    m_oMulticastClient.Active := true;

    Log(llDebug, 'Multicast Watcher started');
end;

procedure TMulticastStreamAnalyzer.MulticastClientRead(Sender: TObject; const AData: TArray<System.Byte>; ABinding: TIdSocketHandle);
var
    Packet: TArray<System.Byte>;
    i, j: uint32;
begin
    i := 0;
    j := i + 188;
    while (j <= Length(AData)) do begin
        Packet := Copy(AData, i, j + 1);
        ParseMPEGTSPacket(Packet);
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

procedure TMulticastStreamAnalyzer.ParseMPEGTSPacket(var Packet: TArray<System.Byte>);
var
    tmpByte: uint8;
    tmpWord: uint16;
begin
    if (Packet[0] <> $47) then begin
        Log(llDebug, 'Packet has no MPEGTS mark');
    end;

    if ((Packet[1] and $80) <> 0) then begin
        Log(llDebug, 'Transport error flag is set');
    end;

    if ((Packet[1] and $40) <> 0) then begin
        //Log(llDebug, 'payload_unit_start_indicator flag is set');
    end;

    if ((Packet[1] and $20) <> 0) then begin
        //Log(llDebug, 'transport priority flag is set');
    end;

    tmpWord := ((Packet[1] and $1F) shl 8) + Packet[2];
end;

end.

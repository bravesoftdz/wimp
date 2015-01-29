unit uwMain;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.DateUtils,
    System.Classes, uMulticastStreamAnalyzer, System.SyncObjs, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.Grids,
    IdIPMCastBase, IdIPMCastClient,
    USLogger, Vcl.ComCtrls, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
    VCLTee.Chart, VCLTee.TeeSpline;

const
    ERRORS_GRAPH_TIME = 5; // minutes

type
    TWMain = class(TForm)
        pnlControls: TPanel;
        ledMulticastGroup: TLabeledEdit;
        ledMulticastPort: TLabeledEdit;
        bbtnStartStop: TBitBtn;
        sgStats: TStringGrid;
        lbLog: TListBox;
        timerUpdateView: TTimer;
        pcMainPageControl: TPageControl;
        tsStats: TTabSheet;
        tsLog: TTabSheet;
        tsGraphBandwidth: TTabSheet;
        tcGraphBandwidth: TChart;
        tcsBandwidth: TLineSeries;
        tcsPackets: TLineSeries;
        tsGraphErrors: TTabSheet;
        tcErrorsGraph: TChart;
        tcsErrorsCount: TBarSeries;
        timerCheckStream: TTimer;
        ledBindingIP: TLabeledEdit;
        procedure bbtnStartStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure timerUpdateViewTimer(Sender: TObject);
        procedure timerCheckStreamTimer(Sender: TObject);
        procedure updateGrapsWidth(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        m_uSumPacketsCount: uint64;
        m_uSumErrorsGraphed: uint64;
        m_uStreamCheckPacketsCount: uint64;                                     // Количество пакетов с последнего тика таймера проверки потока
        m_bIsStreamContinuous: Boolean;                                         // ???
        m_bIsMulticastThreadRunning: Boolean;
        m_tMulticastThread: TMulticastStreamAnalyzer;
        m_sLogWriter: TStreamWriter;
        function StartMulticastWatcher(MulticastGroup: string; MulticastPort: uint16; BindingIP: string = ''): boolean;
        procedure StopMulticastWatcher;
    public
        constructor Create(AOwner: TComponent); override;
        procedure Log(LogLevel: TLogLevel; Mess: string);
    end;

var
    WMain: TWMain;

implementation

{$R *.dfm}

constructor TWMain.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    m_uSumPacketsCount := 0;
    m_bIsMulticastThreadRunning := False;
end;

procedure TWMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if (m_bIsMulticastThreadRunning) then
        StopMulticastWatcher;
end;

procedure TWMain.FormCreate(Sender: TObject);
begin
    pcMainPageControl.ActivePageIndex := 0;

    sgStats.Cells[0, 0] := 'PID';
    sgStats.Cells[1, 0] := 'Packets';
    sgStats.Cells[2, 0] := 'Errors';
    sgStats.Cells[3, 0] := 'Packets/sec';
    sgStats.Cells[4, 0] := 'KBytes/sec';

    tcsBandwidth.Clear;
    tcsPackets.Clear;
    tcGraphBandwidth.BottomAxis.DateTimeFormat := 'hh:nn:ss';

    tcsErrorsCount.Clear;
    tcErrorsGraph.BottomAxis.DateTimeFormat := 'hh:nn';
end;

procedure TWMain.bbtnStartStopClick(Sender: TObject);
var
    idIPMCastClientTemp: TIdIPMCastClient;
begin
    if (not m_bIsMulticastThreadRunning) then begin
        idIPMCastClientTemp := TIdIPMCastClient.Create(nil);
        if (idIPMCastClientTemp.IsValidMulticastGroup(ledMulticastGroup.Text)) then begin
            if ((StrToInt(ledMulticastPort.Text) > 0) and (StrToInt(ledMulticastPort.Text) < 65535)) then begin
                if (StartMulticastWatcher(ledMulticastGroup.Text, StrToInt(ledMulticastPort.Text), ledBindingIP.Text)) then begin
                    ledMulticastGroup.Enabled := false;
                    ledMulticastPort.Enabled := false;
                    ledBindingIP.Enabled := false;
                    m_bIsMulticastThreadRunning := true;
                    bbtnStartStop.Caption := 'Stop';
                end;
            end else
                ShowMessage('"' + ledMulticastPort.Text + '" is not valid port!');
        end else
            ShowMessage('"' + ledMulticastGroup.Text + '" is not valid multicast group!');
        FreeAndNil(idIPMCastClientTemp);
    end else begin
        StopMulticastWatcher;

        m_bIsMulticastThreadRunning := false;
        bbtnStartStop.Caption := 'Start';
        ledMulticastGroup.Enabled := true;
        ledMulticastPort.Enabled := true;
        ledBindingIP.Enabled := true;
    end;
end;

function TWMain.StartMulticastWatcher(MulticastGroup: string; MulticastPort: uint16; BindingIP: string = ''): boolean;
begin
    Result := false;

    m_uStreamCheckPacketsCount := 0;
    m_bIsStreamContinuous := True;

    try
        m_sLogWriter := TStreamWriter.Create(ExtractFilePath(ParamStr(0)) + MulticastGroup + '.' + UIntToStr(MulticastPort) + '.log', true, TEncoding.UTF8);
        m_tMulticastThread := TMulticastStreamAnalyzer.Create(MulticastGroup, MulticastPort, BindingIP);
        // TODO: Log(llDebug, 'WiMP v???? started.');
        Result := true;
    except
        on E: Exception do begin
            Log(llDebug, 'Start error: ' + E.Message);
            StopMulticastWatcher;
        end;
    end;
end;

procedure TWMain.StopMulticastWatcher;
begin
    try
        if (Assigned(m_tMulticastThread)) then
            FreeAndNil(m_tMulticastThread);
    except
        on E: Exception do
            Log(llDebug, 'Stop error: ' + E.Message);
    end;

    if (Assigned(m_sLogWriter)) then
        FreeAndNil(m_sLogWriter);
end;

procedure TWMain.Log(LogLevel: TLogLevel; Mess: string);
var
    DateInfo: string;
begin
    DateInfo := '[' + DateToStr(Now()) + ' ' + TimeToStr(Now()) + ']';

    lbLog.Items.Add(DateInfo + ' ' + Mess);
    lbLog.TopIndex := lbLog.Items.Count - 1;

    if (Assigned(m_sLogWriter)) then begin
        m_sLogWriter.WriteLine(DateInfo + ' ' + Mess);
        m_sLogWriter.Flush;
    end;
end;

procedure TWMain.updateGrapsWidth(Sender: TObject);
begin
    // Нужно ли?
end;

procedure TWMain.timerUpdateViewTimer(Sender: TObject);
var
    i: integer;
    SumPacketsCount: uint64;
    SumErrorsCount: uint64;
    CurrentDateTime: TDateTime;
    CurrentDateTimeForGraph: TDateTime;
begin
    SumPacketsCount := 0;
    SumErrorsCount := 0;

    CurrentDateTime := Now();
    CurrentDateTimeForGraph := RecodeTime(CurrentDateTime, HourOfTheDay(CurrentDateTime), MinuteOfTheHour(CurrentDateTime) - (MinuteOfTheHour(CurrentDateTime) mod ERRORS_GRAPH_TIME), 0, 0);

    if (Assigned(m_tMulticastThread)) then begin
        if (Assigned(m_tMulticastThread.m_oCriticalSection)) then
            m_tMulticastThread.m_oCriticalSection.Enter;

        sgStats.RowCount := m_tMulticastThread.m_slStreamsInfo.Count + 2;

        for i := 0 to m_tMulticastThread.m_slStreamsInfo.Count - 1 do begin
            SumPacketsCount := SumPacketsCount + (m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiPacketsCount;
            SumErrorsCount := SumErrorsCount + (m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiErrorsCount;

            sgStats.Cells[0, i + 1] := '#' + IntToHex((m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uPID, 4);
            sgStats.Cells[1, i + 1] := UIntToStr((m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiPacketsCount);
            sgStats.Cells[2, i + 1] := UIntToStr((m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiErrorsCount);
            sgStats.Cells[3, i + 1] := '-';
            sgStats.Cells[4, i + 1] := '-';
        end;

        if (Assigned(m_tMulticastThread.m_oCriticalSection)) then
            m_tMulticastThread.m_oCriticalSection.Leave;

        // Пишем в статистику суммарную информацию
        sgStats.Cells[0, sgStats.RowCount - 1] := 'Summary';
        sgStats.Cells[1, sgStats.RowCount - 1] := UIntToStr(SumPacketsCount);
        sgStats.Cells[2, sgStats.RowCount - 1] := UIntToStr(SumErrorsCount);

        if (SumPacketsCount > m_uSumPacketsCount) then begin
            sgStats.Cells[3, sgStats.RowCount - 1] := UIntToStr(SumPacketsCount - m_uSumPacketsCount);
            sgStats.Cells[4, sgStats.RowCount - 1] := UIntToStr(Round((SumPacketsCount - m_uSumPacketsCount) / 7 * 1316 / 1024));
        end else begin
            sgStats.Cells[3, sgStats.RowCount - 1] := '0';
            sgStats.Cells[4, sgStats.RowCount - 1] := '0';
        end;
    end;

    // Добавляем на график
    if (SumPacketsCount > m_uSumPacketsCount) then begin
        tcsBandwidth.AddXY(Now(), (SumPacketsCount - m_uSumPacketsCount) / 7 * 1316 / 1024);
        tcsPackets.AddXY(Now(), SumPacketsCount - m_uSumPacketsCount);
    end else begin
        tcsBandwidth.AddXY(Now(), 0);
        tcsPackets.AddXY(Now(), 0);
    end;

    // Добавляем на график ошибок
    if ((tcsErrorsCount.Count < 1) or (tcsErrorsCount.XValue[tcsErrorsCount.Count - 1] <> CurrentDateTimeForGraph)) then
        tcsErrorsCount.AddXY(CurrentDateTimeForGraph, 0);

    if (SumErrorsCount > m_uSumErrorsGraphed) then begin
        if (tcsErrorsCount.XValue[tcsErrorsCount.Count - 1] = CurrentDateTimeForGraph) then begin
            tcsErrorsCount.YValue[tcsErrorsCount.Count - 1] := tcsErrorsCount.YValue[tcsErrorsCount.Count - 1] + (SumErrorsCount - m_uSumErrorsGraphed);
        end else
            tcsErrorsCount.AddXY(CurrentDateTimeForGraph, SumErrorsCount - m_uSumErrorsGraphed);
    end;

    // Если количество пакетов было отличным от нуля - обновляем исторический счетчик
    if (SumPacketsCount > 0) then
        m_uSumPacketsCount := SumPacketsCount;

    if (SumErrorsCount > 0) then
        m_uSumErrorsGraphed := SumErrorsCount;

    // Чистим графики
    if (tcsBandwidth.Count > 900) then
        tcsBandwidth.Delete(0);
    if (tcsPackets.Count > 900) then
        tcsPackets.Delete(0);
    if (tcsErrorsCount.Count > 100) then
        tcsErrorsCount.Delete(0);
end;

procedure TWMain.timerCheckStreamTimer(Sender: TObject);
var
    SumPacketsCount: uint64;
    i: integer;
begin
    SumPacketsCount := 0;

    if (m_bIsMulticastThreadRunning and Assigned(m_tMulticastThread)) then begin
        if (Assigned(m_tMulticastThread.m_oCriticalSection)) then
            m_tMulticastThread.m_oCriticalSection.Enter;

        for i := 0 to m_tMulticastThread.m_slStreamsInfo.Count - 1 do begin
            SumPacketsCount := SumPacketsCount + (m_tMulticastThread.m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiPacketsCount;
        end;

        if (Assigned(m_tMulticastThread.m_oCriticalSection)) then
            m_tMulticastThread.m_oCriticalSection.Leave;
    end;

    if (m_bIsMulticastThreadRunning and (SumPacketsCount <> 0)) then begin
        if (SumPacketsCount <= m_uStreamCheckPacketsCount) then begin
            if (m_bIsStreamContinuous) then begin
                m_bIsStreamContinuous := False;
                log(llWarning, 'Stream is not continuous.')
            end;
        end else
            if (not m_bIsStreamContinuous) then begin
                m_bIsStreamContinuous := True;
                log(llWarning, 'Stream resumed.');
            end;
            m_uStreamCheckPacketsCount := SumPacketsCount;
    end;
end;

end.

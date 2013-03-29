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
        procedure bbtnStartStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure timerUpdateViewTimer(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        m_uSumPacketsCount: uint64;
        m_uSumErrorsGraphed: uint64;
        m_bIsMulticastThreadRunning: Boolean;
        m_tMulticastThread: TMulticastStreamAnalyzer;
        m_sLogWriter: TStreamWriter;
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
    m_bIsMulticastThreadRunning := false;
end;

procedure TWMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if (Assigned(m_tMulticastThread)) then
        FreeAndNil(m_tMulticastThread);

    if (Assigned(m_sLogWriter)) then
        FreeAndNil(m_sLogWriter);
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
                try
                    m_sLogWriter := TStreamWriter.Create(ExtractFilePath(ParamStr(0)) + ledMulticastGroup.Text + '.' + ledMulticastPort.Text + '.log', true, TEncoding.UTF8);
                    ledMulticastGroup.Enabled := false;
                    ledMulticastPort.Enabled := false;
                    m_tMulticastThread := TMulticastStreamAnalyzer.Create(ledMulticastGroup.Text, StrToInt(ledMulticastPort.Text));
                    m_bIsMulticastThreadRunning := true;
                    bbtnStartStop.Caption := 'Stop';
                except
                    on E: Exception do begin
                        Log(llDebug, 'General error: ' + E.Message);
                        FreeAndNil(m_sLogWriter);
                    end;
                end;
            end else
                ShowMessage('"' + ledMulticastPort.Text + '" is not valid port!');
        end else
            ShowMessage('"' + ledMulticastGroup.Text + '" is not valid multicast group!');
        FreeAndNil(idIPMCastClientTemp);
    end else begin
        FreeAndNil(m_tMulticastThread);
        FreeAndNil(m_sLogWriter);
        m_bIsMulticastThreadRunning := false;
        bbtnStartStop.Caption := 'Start';
        ledMulticastGroup.Enabled := true;
        ledMulticastPort.Enabled := true;
    end;
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

    // Чистим график до 100 значений
    if (tcsBandwidth.Count > 200) then
        tcsBandwidth.Delete(0);
    if (tcsPackets.Count > 200) then
        tcsPackets.Delete(0);
end;

end.

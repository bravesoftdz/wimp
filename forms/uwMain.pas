unit uwMain;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, uMulticastStreamAnalyzer, System.SyncObjs, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.Grids,
    IdIPMCastBase, IdIPMCastClient,
    USLogger, Vcl.ComCtrls, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
    VCLTee.Chart, VCLTee.TeeSpline;

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
        procedure bbtnStartStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure timerUpdateViewTimer(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        m_uSumPacketsCount: uint64;
        m_bIsMulticastThreadRunning: Boolean;
        m_tMulticastThread: TMulticastStreamAnalyzer;
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
end;

procedure TWMain.bbtnStartStopClick(Sender: TObject);
var
    idIPMCastClientTemp: TIdIPMCastClient;
begin
    if (not m_bIsMulticastThreadRunning) then begin
        idIPMCastClientTemp := TIdIPMCastClient.Create(nil);
        if (idIPMCastClientTemp.IsValidMulticastGroup(ledMulticastGroup.Text)) then begin
            if ((StrToInt(ledMulticastPort.Text) > 0) and (StrToInt(ledMulticastPort.Text) < 65535)) then begin
                ledMulticastGroup.Enabled := false;
                ledMulticastPort.Enabled := false;
                m_tMulticastThread := TMulticastStreamAnalyzer.Create(ledMulticastGroup.Text, StrToInt(ledMulticastPort.Text));
                m_bIsMulticastThreadRunning := true;
                bbtnStartStop.Caption := 'Stop';
            end else
                ShowMessage('"' + ledMulticastPort.Text + '" is not valid port!');
        end else
            ShowMessage('"' + ledMulticastGroup.Text + '" is not valid multicast group!');
        FreeAndNil(idIPMCastClientTemp);
    end else begin
        FreeAndNil(m_tMulticastThread);
        m_bIsMulticastThreadRunning := false;
        bbtnStartStop.Caption := 'Start';
        ledMulticastGroup.Enabled := true;
        ledMulticastPort.Enabled := true;
    end;
end;

procedure TWMain.Log(LogLevel: TLogLevel; Mess: string);
begin
    lbLog.Items.Add('[' + DateToStr(Now()) + ' ' + TimeToStr(Now()) + ']' + ' ' + Mess);
end;

procedure TWMain.timerUpdateViewTimer(Sender: TObject);
var
    i: integer;
    SumPacketsCount: uint64;
    SumErrorsCount: uint64;
begin
    SumPacketsCount := 0;
    SumErrorsCount := 0;

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

    // Если количество пакетов было отличным от нуля - обновляем исторический счетчик
    if (SumPacketsCount > 0) then
        m_uSumPacketsCount := SumPacketsCount;

    // Чистим график до 100 значений
    if (tcsBandwidth.Count > 400) then
        tcsBandwidth.Delete(0);
    if (tcsPackets.Count > 400) then
        tcsPackets.Delete(0);
end;

end.

unit uwMain;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, uMulticastStreamAnalyzer, System.SyncObjs, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.Buttons,
    Vcl.Grids,
    USLogger;

type
    TWMain = class(TForm)
        pnlControls: TPanel;
        ledMulticastGroup: TLabeledEdit;
        ledMulticastPort: TLabeledEdit;
        bbtnStartStop: TBitBtn;
        sgStats: TStringGrid;
        lbLog: TListBox;
        timerUpdateView: TTimer;
        procedure bbtnStartStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure timerUpdateViewTimer(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        m_bIsMulticastThreadRunning: Boolean;
        m_tMulticastThread: TMulticastStreamAnalyzer;
        m_slStreamsInfo: TStringList;
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
    m_bIsMulticastThreadRunning := false;
end;

procedure TWMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if (Assigned(m_tMulticastThread)) then
        FreeAndNil(m_tMulticastThread);
end;

procedure TWMain.FormCreate(Sender: TObject);
begin
    sgStats.Cells[0, 0] := 'PID';
    sgStats.Cells[1, 0] := 'Packets';
    sgStats.Cells[2, 0] := 'Errors';
end;

procedure TWMain.bbtnStartStopClick(Sender: TObject);
begin
    if (not m_bIsMulticastThreadRunning) then begin
        m_tMulticastThread := TMulticastStreamAnalyzer.Create(ledMulticastGroup.Text, StrToInt(ledMulticastPort.Text));
        m_bIsMulticastThreadRunning := true;
        bbtnStartStop.Caption := 'Stop';
    end else begin
        FreeAndNil(m_tMulticastThread);
        m_bIsMulticastThreadRunning := false;
        bbtnStartStop.Caption := 'Start';
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
        m_slStreamsInfo := m_tMulticastThread.m_slStreamsInfo;
        m_tMulticastThread.m_oCriticalSection.Leave;

        sgStats.RowCount := m_slStreamsInfo.Count + 2;
        for i := 0 to m_slStreamsInfo.Count - 1 do begin
            SumPacketsCount := SumPacketsCount + (m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiPacketsCount;
            SumErrorsCount := SumErrorsCount + (m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiErrorsCount;

            sgStats.Cells[0, i + 1] := '#' + IntToHex((m_slStreamsInfo.Objects[i] as TStreamInfo).m_uPID, 4);
            sgStats.Cells[1, i + 1] := UIntToStr((m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiPacketsCount);
            sgStats.Cells[2, i + 1] := UIntToStr((m_slStreamsInfo.Objects[i] as TStreamInfo).m_uiErrorsCount);
        end;

        sgStats.Cells[0, m_slStreamsInfo.Count + 1] := 'Summary';
        sgStats.Cells[1, m_slStreamsInfo.Count + 1] := UIntToStr(SumPacketsCount);
        sgStats.Cells[2, m_slStreamsInfo.Count + 1] := UIntToStr(SumErrorsCount);
    end;
end;

end.

unit uwMain;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, uMulticastStreamAnalyzer, Vcl.Graphics,
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
        procedure bbtnStartStopClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
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
    m_bIsMulticastThreadRunning := false;
end;

procedure TWMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if (Assigned(m_tMulticastThread)) then
        FreeAndNil(m_tMulticastThread);
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

end.

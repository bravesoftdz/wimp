program WiMP;

uses
{$IFDEF DEBUG}
    FastMM4,
{$ENDIF}
    Vcl.Forms,
    uwMain in 'forms\uwMain.pas' {WMain},
    Vcl.Themes,
    Vcl.Styles,
    uMulticastStreamAnalyzer in 'units\uMulticastStreamAnalyzer.pas';

{$R *.res}

begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    TStyleManager.TrySetStyle('Metropolis UI Blue');
    Application.Title := 'Where is My Packet?';
    Application.CreateForm(TWMain, WMain);
    Application.Run;
end.

program WiMP;

uses
    FastMM4,
    Vcl.Forms,
    uwMain in 'forms\uwMain.pas' {WMain},
    Vcl.Themes,
    Vcl.Styles,
    uMulticastStreamAnalyzer in 'units\uMulticastStreamAnalyzer.pas';

{$R *.res}

begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Where is My Packet?';
    TStyleManager.TrySetStyle('Metropolis UI Blue');
    Application.CreateForm(TWMain, WMain);
    Application.Run;
end.

program KicadOctopartInterface;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainWindow, Kicad.SchematicFile, wndEditProperties,
	LibreOfficeCalcInterface, wndConfig
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
	Application.Title := 'Kicad BOM Tool';
  Application.Scaled := True;
  Application.Initialize;
	Application.CreateForm(TfrmMain, frmMain);
	Application.CreateForm(TfrmEditProperties, frmEditProperties);
	Application.CreateForm(TfrmConfig, frmConfig);
  Application.Run;
end.


unit wndConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
	JSONPropStorage, ScalingBitBtn, AppConfig, Windows;

type

	{ TfrmConfig }

  TfrmConfig = class(TForm)
		edtDoNotPlaceFieldName: TEdit;
		edtTemplate: TEdit;
		gbBOMExport: TGroupBox;
		gbSchematic: TGroupBox;
		lblDoNotPlace: TLabel;
		lblTemplate: TLabel;
		btnSelectFile: TScalingBitBtn;
		OpenDialog1: TOpenDialog;
		ScalingBitBtn2: TScalingBitBtn;
		procedure btnSelectFileClick(Sender: TObject);
  procedure FormHide(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure btnSelectFileResize(Sender: TObject);
	procedure ScalingBitBtn2Click(Sender: TObject);
  private

  public

  end;

var
  frmConfig: TfrmConfig;

implementation

{$R *.lfm}

{ TfrmConfig }

procedure TfrmConfig.btnSelectFileResize(Sender: TObject);
begin
  btnSelectFile.Width := btnSelectFile.Height;
end;

procedure TfrmConfig.ScalingBitBtn2Click(Sender: TObject);
begin
  ModalResult := 1;
  if FileExists(edtTemplate.Text) then
  begin
    AppConfig.PropStorage.WriteString('BOMExport.TemplateFile', edtTemplate.Text);
    AppConfig.PropStorage.WriteString('Components.DoNotPlaceFieldName', edtDoNotPlaceFieldName.Text);
    ModalResult := 0;
    Close;
	end else
  begin
    MessageBoxW(Handle, PWideChar(UTF8Decode('Die ausgewählte Template-Datei existiert nicht.')), PWideChar(UTF8Decode('Fehler')), MB_ICONERROR);
	end;
end;

procedure TfrmConfig.FormShow(Sender: TObject);
begin
  edtTemplate.Text := PropStorage.ReadString('BOMExport.TemplateFile', edtTemplate.Text);
  edtDoNotPlaceFieldName.Text := PropStorage.ReadString('Components.DoNotPlaceFieldName', edtDoNotPlaceFieldName.Text);
  AppConfig.RestoreFormState(Self);
end;

procedure TfrmConfig.FormHide(Sender: TObject);
begin
  AppConfig.StoreFormState(Self);
end;

procedure TfrmConfig.btnSelectFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtTemplate.Text := OpenDialog1.FileName;
end;

end.


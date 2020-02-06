unit wndEditProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ScalingBitBtn, Kicad.SchematicFile, AppConfig;

type

	{ TfrmEditProperties }

  TfrmEditProperties = class(TForm)
		edtShort_Footprint: TEdit;
		edtSupplier: TEdit;
		edtSupplierSKU: TEdit;
		edtManufacturer: TEdit;
		edtPartNumber: TEdit;
		edtFootprint: TEdit;
		edtValue: TEdit;
		edtDesignator: TEdit;
		gbProperties: TGroupBox;
		lblShortFootprint: TLabel;
		lblSupplierSKU: TLabel;
		lblSupplier: TLabel;
		lblDesignator: TLabel;
		lblValue: TLabel;
		lblFootprint: TLabel;
		lblManufacturer: TLabel;
		lblPartNumber: TLabel;
		btnSave: TScalingBitBtn;
		procedure editChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure btnSaveClick(Sender: TObject);
		procedure gbPropertiesClick(Sender: TObject);
  private
		fComponents: TComponentList;
  public
    property SchematicComponents: TComponentList read fComponents write fComponents;
    function ComponentNameToFieldName(ComponentName: String): String;
  end;

var
  frmEditProperties: TfrmEditProperties;

implementation

uses MainWindow;

{$R *.lfm}

{ TfrmEditProperties }

procedure TfrmEditProperties.FormCreate(Sender: TObject);
begin
  fComponents := TComponentList.Create;
end;

procedure TfrmEditProperties.editChange(Sender: TObject);
begin
  if (Sender is TEdit) then
  begin
    TEdit(Sender).Font.Color := clBlack;
    btnSave.Enabled := true;
	end;
end;

procedure TfrmEditProperties.FormDestroy(Sender: TObject);
begin
  fComponents.Free;
end;

procedure TfrmEditProperties.FormHide(Sender: TObject);
begin
  AppConfig.StoreFormState(Self);
end;

procedure TfrmEditProperties.FormShow(Sender: TObject);

  function GetFieldValue(FieldName: String): String;
	var
		i: Integer;
  begin
    Result := fComponents[0].FieldValue[FieldName];
    for i := 1 to fComponents.Count-1 do
    begin
      if fComponents[i].FieldValue[FieldName] <> fComponents[0].FieldValue[FieldName] then
      begin
        Result := '<mehrere Werte>';
        exit;
  		end;
		end;
	end;

var
  i: Integer;
	e: TEdit;
begin
  AppConfig.RestoreFormState(Self);
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i].InheritsFrom(TEdit) then
    begin
			e := TEdit(Components[i]);
      if e.Name = 'edtDesignator' then
      begin
        e.Text := fComponents.Designator;
			end else begin
        e.Font.Color := clGray;
        e.Text := GetFieldValue(ComponentNameToFieldName(e.Name));
			end;
		end;
	end;
end;

procedure TfrmEditProperties.btnSaveClick(Sender: TObject);
var
  c: TSchematicComponent;
  e: TEdit;
  i: Integer;
begin
  for c in fComponents do
  begin
    for i := 0 to ComponentCount-1 do
    begin
      if Components[i].InheritsFrom(TEdit) then
      begin
        e := TEdit(Components[i]);
        if e.Color <> clBlack then
          c.Fields[ComponentNameToFieldName(e.Name)] := e.Text;
      end;
		end;
  end;

  frmMain.UpdateListview(Sender);
  Hide;
end;

procedure TfrmEditProperties.gbPropertiesClick(Sender: TObject);
begin

end;

function TfrmEditProperties.ComponentNameToFieldName(ComponentName: String): String;
begin
  Result := ComponentName.Substring(3).Replace('_', ' ');
end;


end.


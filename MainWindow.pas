unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
	StdCtrls, BCToolBar, BGRAImageList, Kicad.SchematicFile, BOMExport, AppConfig,
	Windows;

type

	{ TfrmMain }

  TfrmMain = class(TForm)
		BCToolBar1: TBCToolBar;
		cbxShort_Footprint: TCheckBox;
		FormIcons: TBGRAImageList;
		btnOpenSchematic: TToolButton;
		cbxGroupValue: TCheckBox;
		cbxGroupFootprint: TCheckBox;
		cbxGroupManufacturer: TCheckBox;
		cbxGroupPartNumber: TCheckBox;
		cbxColumnDesignator: TCheckBox;
		cbxColumnValue: TCheckBox;
		cbxColumnFootprint: TCheckBox;
		cbxColumnQuantity: TCheckBox;
		cbxColumnManufacturer: TCheckBox;
		cbxColumnPartNumber: TCheckBox;
		cbxColumnSupplier: TCheckBox;
		cbxColumnSupplierPartNumber: TCheckBox;
		edtFilter: TEdit;
		GroupBox1: TGroupBox;
		gbFilter: TGroupBox;
		gbColumns: TGroupBox;
		ListView1: TListView;
		OpenDialog1: TOpenDialog;
		btnExport: TToolButton;
		SaveDialog1: TSaveDialog;
		btnSaveSchematic: TToolButton;
		btnConfig: TToolButton;
		procedure btnOpenSchematicClick(Sender: TObject);
		procedure cbxColumnQuantityChange(Sender: TObject);
		procedure cbxColumnClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure gbColumnsClick(Sender: TObject);
		procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
		procedure ListView1DblClick(Sender: TObject);
		procedure ListView1KeyDown(Sender: TObject; var Key: Word;
			{%H-}Shift: TShiftState);
		procedure btnConfigClick(Sender: TObject);
		procedure UpdateListview(Sender: TObject);
		procedure btnExportClick(Sender: TObject);
  private
		function GetGroupedComponentList: TGroupedComponentList;
		procedure OpenPropertiesDialog;

  public

  end;

var
  frmMain: TfrmMain;
  Schematic: TKicadSchematic = nil;
  ComponentGroups: TGroupedComponentList = nil;

implementation

uses wndEditProperties, wndConfig;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnOpenSchematicClick(Sender: TObject);
begin
  //if OpenDialog1.Execute then
  //begin
    schematic := TKicadSchematic.Create;
    //schematic.Open(OpenDialog1.Filename);
    schematic.Open('C:\Users\user258\Lazarus\projects\KicadOctopartInterface\test-project\ESP32-Mesh.sch');

    UpdateListview(Sender);
  //end;
end;

procedure TfrmMain.cbxColumnQuantityChange(Sender: TObject);
begin

end;

procedure TfrmMain.cbxColumnClick(Sender: TObject);
begin
  ListView1.Columns[(Sender as TCheckBox).TabOrder].Visible := (Sender as TCheckBox).Checked;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  AppConfig.StoreFormState(Self);
  if ComponentGroups <> nil then FreeAndNil(ComponentGroups);
  Schematic.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  AppConfig.RestoreFormState(Self);
end;

procedure TfrmMain.gbColumnsClick(Sender: TObject);
begin

end;

procedure TfrmMain.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateListview(Sender);
end;

procedure TfrmMain.ListView1DblClick(Sender: TObject);
begin
  OpenPropertiesDialog;
end;

procedure TfrmMain.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then OpenPropertiesDialog;
end;

procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  frmConfig.ShowModal;
end;

procedure TfrmMain.UpdateListview(Sender: TObject);
var
  g: TComponentGroup;
	li: TListItem;
	i, colWidth: Integer;
begin
  if ComponentGroups <> nil then FreeAndNil(ComponentGroups);
  ComponentGroups := GetGroupedComponentList;
  if ComponentGroups = nil then exit;

  ListView1.BeginUpdate;
  ListView1.Items.Clear;
  for i := 0 to ComponentGroups.Count-1 do
  begin
    g := ComponentGroups.Data[i];

    li := ListView1.Items.Add;
    li.Data := g;
    li.Caption := IntToStr(g.Quantity);
    li.SubItems.Add(g.Designator);
    li.SubItems.Add(g.FieldValue['Value']);
    li.SubItems.Add(g.FieldValue['Footprint']);
    li.SubItems.Add(g.FieldValue['Short Footprint']);
    li.SubItems.Add(g.FieldValue['Manufacturer']);
    li.SubItems.Add(g.FieldValue['Part Number']);
    li.SubItems.Add(g.FieldValue['Supplier']);
    li.SubItems.Add(g.FieldValue['Order Code']);
  end;
  colWidth := ListView1.ClientWidth div (ListView1.Columns.Count);
  for i := 0 to ListView1.Columns.Count-2 do ListView1.Columns[i].Width := colWidth;
  ListView1.Column[0].Width := ListView1.Column[0].Width div 3;
  ListView1.EndUpdate;
  ListView1.AutoWidthLastColumn := true;
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
var
  Columns: TStringList;
begin
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
    begin
      if MessageBoxW(Handle, PWideChar(UTF8Decode('Die Datei ' + SaveDialog1.FileName + ' existiert bereits. Überschreiben?')), PWideChar(UTF8Decode('Datei existiert bereits')), MB_ICONWARNING or MB_YESNO) = idYes then
      begin
        DeleteFile(PChar(SaveDialog1.FileName));
			end else exit;
		end;

    Columns := TStringList.Create;
    if cbxColumnDesignator.Checked then Columns.Add('Designator');
    if cbxColumnValue.Checked then Columns.Add('Value');
    if cbxColumnFootprint.Checked then Columns.Add('Footprint');
    if cbxColumnQuantity.Checked then Columns.Add('Quantity');
    if cbxColumnManufacturer.Checked then Columns.Add('Manufacturer');
    if cbxColumnPartNumber.Checked then Columns.Add('Part Number');
    if cbxColumnSupplier.Checked then Columns.Add('Supplier');
    if cbxColumnSupplierPartNumber.Checked then Columns.Add('Supplier Part Number');
    ExportBOMToCalc(SaveDialog1.Filename, GetGroupedComponentList, schematic);
    //ExportBOM(SaveDialog1.Filename, Columns, GetGroupedComponentList);
    Columns.Free;
	end;
end;

function TfrmMain.GetGroupedComponentList: TGroupedComponentList;
var
	groupFields: TStringList;
  cl: TComponentList;
begin
  Result := nil;
  if schematic = nil then exit;

  groupFields := TStringList.Create;
  if cbxGroupValue.Checked then
    groupFields.Add('Value');
  if cbxGroupFootprint.Checked then
    groupFields.Add('Footprint');
  if cbxGroupManufacturer.Checked then
    groupFields.Add('Manufacturer');
  if cbxGroupPartNumber.Checked then
    groupFields.Add('Part Number');

  if groupFields.Count = 0 then groupFields.Add('Designator');
  cl := TComponentList.CreateFromObjects(schematic.Objects);
  Result := TGroupedComponentList.CreateFromList(cl, groupFields);
  Result.Sort;
  cl.Free;
  groupFields.Free;
end;

procedure TfrmMain.OpenPropertiesDialog;
var
	c: TSchematicComponent;
	i: Integer;
begin
  frmEditProperties.SchematicComponents.Clear;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items[i].Selected then
    begin
      for c in TComponentGroup(ListView1.Items[i].Data) do
      begin
        frmEditProperties.SchematicComponents.Add(c);
			end;
    end;
	end;
	frmEditProperties.Show;
end;

end.


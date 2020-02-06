unit BOMExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Kicad.SchematicFile, Dialogs, fpspreadsheet, {%H-}fpsallformats, fpstypes, Windows, ShellApi, LibreOfficeCalcInterface, fgl, AppConfig;

function ExportBOM(Filename: String; Columns: TStringList; Components: TGroupedComponentList): Boolean;
function ExportBOMToCalc(Filename: String; Components: TGroupedComponentList; schematic: TKicadSchematic): Boolean;

implementation

uses MainWindow, wndConfig;

function ExportBOM(Filename: String; Columns: TStringList; Components: TGroupedComponentList): Boolean;
var
	wb: TsWorkbook;
	ws: TsWorksheet;
	i, col_index: Integer;
  g: TComponentGroup;

  procedure WriteCell(Row: Integer; Column: String; var column_index: Integer);
  begin
      if Columns.IndexOf(Column) >= 0 then
      begin
        ws.WriteText(Row, column_index, g.FieldValue[Column]);
        Inc(column_index);
  		end;
  end;

begin
  Result := false;
	wb := TsWorkbook.Create;
  try
	  ws := wb.AddWorksheet('Stückliste');

	  for i := 0 to Columns.Count-1 do
	  begin
	    ws.WriteText(0, i, Columns[i]);
		end;

	  for i := 0 to Components.Count-1 do
	  begin
	    g := Components.Data[i];
	    col_index := 0;

			WriteCell(i+1, 'Designator', col_index);
			WriteCell(i+1, 'Value', col_index);
			WriteCell(i+1, 'Footprint', col_index);
	    if Columns.IndexOf('Quantity') >= 0 then
	    begin
	      ws.WriteNumber(i+1, col_index, g.Quantity);
	      Inc(col_index);
			end;
			WriteCell(i+1, 'Manufacturer', col_index);
			WriteCell(i+1, 'Part Number', col_index);
			WriteCell(i+1, 'Supplier', col_index);
			WriteCell(i+1, 'Supplier Part Number', col_index);
	  end;
	  wb.WriteToFile(Filename);
	  ShellExecute(0, 'open', PWideChar(UTF8Decode(Filename)), nil, nil, SW_SHOW);
    Result := true;
  finally
	  wb.Free;
  end;
end;

function ExportBOMToCalc(Filename: String; Components: TGroupedComponentList; schematic: TKicadSchematic): Boolean;
type
  TFPMapIntStr = specialize TFPGMap<Integer, String>;
var
  cd: TCalcDocument;
  cellStr: String;
  i, j, y_offset: Integer;
  Columns: TFPMapIntStr;
  g: TComponentGroup;
  templateFilename, cellParamName: String;
  SchematicParameters: TKicadSchematic.TParameters;
begin
  Result := False;
  if Components = nil then exit;
  if Components.Count = 0 then exit;
  try
	  cd := TCalcDocument.Create;
    templateFilename := PropStorage.ReadString('BOMExport.TemplateFile', '');
	  if templateFilename = '' then
	  begin
	    if not frmConfig.ShowModal = 0 then exit;
		end;

	  if not FileExists(templateFilename) then
	  begin
	    if not frmConfig.ShowModal = 0 then exit;
		end;

	  cd.ReadFromFile(AppConfig.PropStorage.ReadString('BOMExport.TemplateFile', ''));

    SchematicParameters := schematic.Paramters;

    // scan for parameters and replace them
    for i := 0 to 50 do
    begin
      for j := 0 to 50 do
      begin
        cellParamName := cd.GetCell(i, j);
        if cellParamName.StartsWith('PARAM:') then
        begin
          // showmessage('Found: ' + cellParamName + ' = ' + cellParamName.Substring(6));
          cellParamName := cellParamName.Substring(6);
          if SchematicParameters.IndexOf(cellParamName) >= 0 then begin
            cellStr := SchematicParameters.KeyData[cellParamName];
            cd.SetCell(i, j, cellStr);
          end;
        end;
			end;
		end;

	  // find beginning of actual table
	  for i := 0 to 100 do
	  begin
	    if cd.GetCell(0, i).StartsWith('FIELD:') then
	    begin
	      y_offset := i;
	      break;
			end;
		end;

	  // get the column index for every column
	  Columns := TFPMapIntStr.Create;
	  for i := 0 to 100 do
	  begin
	    if (cd.GetCell(i,y_offset).StartsWith('FIELD:')) then
	    begin
	      Columns.Add(i, cd.GetCell(i,y_offset).Substring(6));
	    end;
		end;

	  // fill in the data
	  for i := 0 to Components.Count-1 do
	  begin
	    g := Components.Data[i];
	    for j := 0 to Columns.Count-1 do
	    begin
	      if Columns.Data[j] = 'Index' then
	        cellStr := IntToStr(i+1)
        else if Columns.Data[j] = 'Quantity' then
          cellStr := IntToStr(g.Quantity)
        else if Columns.Data[j] = AppConfig.PropStorage.ReadString('Components.DoNotPlaceFieldName', 'DNP') then
        begin
          if g.FieldValue[Columns.Data[j]] = '' then
            cellStr := 'ja'
          else
            cellStr := 'nein';
				end	else cellStr := g.FieldValue[Columns.Data[j]];
	      cd.SetCell(Columns.Keys[j], i + y_offset, cellStr);
			end;
		end;

	  cd.SaveToFile(Filename);
    Result := true;
  finally
	  cd.Free;
	  Columns.Free;
  end;
end;

end.


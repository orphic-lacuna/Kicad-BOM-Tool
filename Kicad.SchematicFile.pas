unit Kicad.SchematicFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, regexpr;

type

  TSchematicObject = class;
  TSchematicComponent = class;
  TSchematicSheet = class;

  	{ TKicadSchematic }

  TKicadSchematic = class
  private
    //type TComponents = specialize TFPGMap<String, TSchematicComponent>;
    //type TSheets = specialize TFPGMap<String, TSchematicSheet>;
    type TComponents = specialize TFPGList<TSchematicComponent>;
    type TSheets = specialize TFPGList<TSchematicSheet>;
  public
    type TObjects = specialize TFPGMap<String, TSchematicObject>;
    type TParameters = specialize TFPGMap<String, String>;
  private
    fParent: TSchematicSheet;
    fFilename: String;
    fLines: TStringList;
    fParameters: TParameters;

    { Contains only components of current schematic, no components of sub-sheets. }
    fComponents: TComponents;
    { Contains only sheets of current schematic, no components of sub-sheets. }
    fSheets: TSheets;
    { Contains all objects of current schematic, also all objects of all sub-sheets. }
    fObjects: TObjects;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TSchematicComponent;
    function GetLine(Index: Integer): String;
		function GetTopLevelSchematic: TKicadSchematic;
    procedure Parse;
  public
    property Paramters: TParameters read fParameters;
    property Filename: String read fFilename;

    property ParentSheet: TSchematicSheet read fParent;
    property TopLevelSchematic: TKicadSchematic read GetTopLevelSchematic;
    { Contains the source code of this schematic file. }
    property Lines[Index: Integer]: String read GetLine;
    property Sheets: TSheets read fSheets;
    { Contains only components of current schematic, no components of sub-sheets. }
    property Components: TComponents read fComponents;
    { Contains all objects of current schematic, also all objects of all sub-sheets. }
    property Objects: TObjects read fObjects;
    constructor Create(aParent: TSchematicSheet = nil);
    destructor Destroy; override;
    procedure Open(aFilename: String);
	end;

	{ TComponentList }

  TComponentList = class(specialize TFPGList<TSchematicComponent>)
  private
		function GetDesignator: String;

  public
    property Designator: String read GetDesignator;
    class function CreateFromObjects(objects: TKicadSchematic.TObjects): TComponentList;
  end;

	{ TComponentGroup }

  TComponentGroup = class(TComponentList)
  private
		function GetFieldValue(Index: String): String;
		function GetQuantity: Integer;

  public
    property Quantity: Integer read GetQuantity;
    property FieldValue[Index: String]: String read GetFieldValue;
	end;

	{ TGroupedComponentList }

  TGroupedComponentList = class(specialize TFPGMap<String, TComponentGroup>)
  private
    fSortingColumn: String;
    // fCompare
    class function GetGroupDescriptor(c: TSchematicComponent; fFields: TStringList): String;
  public
    class function CreateFromList(aList: TComponentList; fFields: TStringList): TGroupedComponentList;
    destructor Destroy; override;
	end;

	{ TSchematicObject }

  TSchematicObject = class
  protected
    fParent: TKicadSchematic;
    { Reference to the line in the schematic file, where the description for this component beings. }
    fStartLineIndex: Integer;
    fUID: String;
    { Parses the schematic file content of fParent beginning with fStartLineIndex. The path is used for determining the correct designator. }
    function Parse: Integer; virtual; abstract;
  public
    property UID: String read fUID;
    property ParentSchematic: TKicadSchematic read fParent;
    constructor Create(aParent: TKicadSchematic; aStartLineIndex: Integer); virtual;
	end;

  { TSchematicSheet }

  TSchematicSheet = class(TSchematicObject)
  protected
    fName: String;
    fFilename: String;
    fSchematic: TKicadSchematic;
    function Parse: Integer; override;
  public
    property Schematic: TKicadSchematic read fSchematic;
    destructor Destroy; override;
  end;

	{ TSchematicComponent }

  TSchematicComponent = class(TSchematicObject)
  private
    type TFields = specialize TFPGMap<String, String>;
  private
    fDesignator: String;
    fQuantity: Integer;
    fPartIndex: Integer;
    fFields: TFields;
		function GetFieldValue(Index: String): String;
    function GetIsVirtualComponent: Boolean;
  protected
    function Parse: Integer; override;
  public
    property Designator: String read fDesignator;
    property UID: String read fUID;
    property Fields: TFields read fFields;
    property FieldValue[Index: String]: String read GetFieldValue;
    property Quantity: Integer read fQuantity;
    property IsVirtualComponent: Boolean read GetIsVirtualComponent;
    constructor Create(aParent: TKicadSchematic; aStartLineIndex: Integer); override;
    destructor Destroy; override;
	end;

implementation



{ Short Footprints }

function GetShortFootprint(Footprint: String): String;
var
  i: Integer;
  regexes: TStringList;
  r: TRegExpr;
  cancel: Boolean;
begin
  Result := '';
  regexes := TStringList.Create;
  regexes.Text := '_([0-9]{4})_[0-9]{4}Metric'#13#10'(SOT-416)'#13#10'(SOT-23-5)'#13#10'(SC-70)'#13#10'(SOD-123)';
  i := 0;
  cancel := False;
	while not cancel and (i < regexes.Count) do
  begin
    if regexes.Strings[i].IsEmpty then
    begin
      Inc(i);
      continue;
		end;

    r := TRegExpr.Create(regexes.Strings[i]);
    if r.Exec(Footprint) then
    begin
      Result := r.Match[1];
      cancel := true;
		end;
		r.Free;
    Inc(i);
	end;
	regexes.Free;
end;


{ TComponentList }

function TComponentList.GetDesignator: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Self.Count-1 do
  begin
    Result += Self.Items[i].Designator + ',';
	end;
  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

class function TComponentList.CreateFromObjects(objects: TKicadSchematic.TObjects): TComponentList;
var
	i: Integer;
begin
  Result := TComponentList.Create;
  for i := 0 to objects.Count-1 do
  begin
    if objects.Data[i] is TSchematicComponent then
      Result.Add(TSchematicComponent(objects.Data[i]));
	end;
end;

{ TComponentGroup }

function TComponentGroup.GetFieldValue(Index: String): String;
begin
  if Index = 'Designator' then
    Result := Designator
  else begin
    Result := '';
    if Count > 0 then
      Result := Self.Items[0].FieldValue[Index];
	end;
end;

function TComponentGroup.GetQuantity: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Self.Count-1 do
  begin
    Result += Self.Items[i].Quantity;
	end;
end;

{ TGroupedComponentList }

class function TGroupedComponentList.GetGroupDescriptor(c: TSchematicComponent;
	fFields: TStringList): String;
var
	f: String;
begin
  Result := '';
  for f in fFields do
  begin
    Result += c.FieldValue[f];
  end;
end;

class function TGroupedComponentList.CreateFromList(aList: TComponentList; fFields: TStringList): TGroupedComponentList;
var
  c: TSchematicComponent;
  index_of: Integer;
	new_list: TComponentGroup;
begin
  Result := TGroupedComponentList.Create;
  for c in aList do
  begin
    index_of := Result.IndexOf(GetGroupDescriptor(c, fFields));
    if index_of >= 0 then
    begin
      // group already exists
      Result.Data[index_of].Add(c);
		end else begin
      new_list := TComponentGroup.Create;
      new_list.Add(c);
      Result.Add(GetGroupDescriptor(c, fFields), new_list);
		end;
	end;
end;

destructor TGroupedComponentList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Data[i].Free;
  inherited Destroy;
end;

{ TSchematicObject }

constructor TSchematicObject.Create(aParent: TKicadSchematic; aStartLineIndex: Integer);
begin
  fParent := aParent;
  fStartLineIndex := aStartLineIndex;
end;

{ TSchematicSheet }

function TSchematicSheet.Parse: Integer;
var
  line_index: Integer;
  splitted_line: TStringList;
begin
  line_index := fStartLineIndex + 1;
  splitted_line := TStringList.Create;
  splitted_line.StrictDelimiter := true;
  splitted_line.Delimiter := ' ';
  while fParent.Lines[line_index] <> '$EndSheet' do
  begin
    splitted_line.DelimitedText := fParent.Lines[line_index];
    if splitted_line[0] = 'U' then
      fUID := splitted_line[1]
    else if splitted_line[0] = 'F0' then
      fName := splitted_line[1].DeQuotedString
      //fName := Copy(splitted_line[1], 2, Length(splitted_line[1])-2)
    else if splitted_line[0] = 'F1' then
      fFilename := ExtractFilePath(fParent.fFilename) + splitted_line[1].DeQuotedString;
      //fFilename := Copy(splitted_line[1], 2, Length(splitted_line[1])-2);

    Inc(line_index);
	end;

  splitted_line.Free;

  // parse sub schematic
  fSchematic := TKicadSchematic.Create;
  fSchematic.Open(fFilename);

  Result := line_index + 1;
end;

destructor TSchematicSheet.Destroy;
begin
  if fSchematic <> nil then FreeAndNil(fSchematic);
  inherited Destroy;
end;

{ TSchematicComponent }

function TSchematicComponent.Parse: Integer;
var
  line_index, i, hierarchicalQuantity: Integer;
  splitted_line: TStringList;
  field_number: Integer;
  stdDesignator, hierarchicalDesignators, tmpDesignator: String;
begin
  stdDesignator := '';
  hierarchicalDesignators := '';
  hierarchicalQuantity := 0;
  line_index := fStartLineIndex + 1;
  splitted_line := TStringList.Create;
  splitted_line.StrictDelimiter := true;
  splitted_line.Delimiter := ' ';
  while fParent.Lines[line_index] <> '$EndComp' do
  begin
    splitted_line.DelimitedText := fParent.Lines[line_index];
    if splitted_line[0] = 'L' then
      stdDesignator := splitted_line[2]
    else if splitted_line[0] = 'U' then
    begin
      fUID := splitted_line[3];
      fPartIndex := StrToInt(splitted_line[1]);
		end else if splitted_line[0] = 'AR' then
    begin
      for i := 1 to splitted_line.Count-1 do
      begin
        if splitted_line[i].StartsWith('Ref=') then
        begin
          tmpDesignator := splitted_line[i].Substring(4).DeQuotedString('"');
          if not tmpDesignator.EndsWith('?') then
          begin
            hierarchicalDesignators := hierarchicalDesignators + tmpDesignator + ',';
            Inc(hierarchicalQuantity);
					end;
				end;
			end;
		end else if splitted_line[0] = 'F' then
    begin
      field_number := StrToInt(splitted_line[1]);
      case field_number of
        0:;
        1: fFields.Add('Value', splitted_line[2]);
        2: fFields.Add('Footprint', splitted_line[2]);
        3: fFields.Add('Datasheet', splitted_line[2]);
        else begin
          fFields.Add(splitted_line[11].DeQuotedString('"'), splitted_line[2].DeQuotedString('"'));
				end;
			end;
		end;

    Inc(line_index);
	end;
  splitted_line.Free;

  // try to guess the short footprint
  if fFields.IndexOf('Short Footprint') < 0 then
  begin
    fFields.Add('Short Footprint', GetShortFootprint(fFields['Footprint']));
	end;

  if hierarchicalQuantity > 0 then
  begin
    fDesignator := hierarchicalDesignators.Substring(0, Length(hierarchicalDesignators)-1);
    fQuantity := hierarchicalQuantity;
  end else begin
    fDesignator := stdDesignator;
    fQuantity := 1;
	end;

	Result := line_index + 1;
end;

function TSchematicComponent.GetIsVirtualComponent: Boolean;
begin
  Result := fDesignator.StartsWith('#') or (fPartIndex > 1);
  if fFields.IndexOf('Value') >= 0 then
    Result := Result or fFields['Value'].ToLower.Contains('testpoint');
end;

function TSchematicComponent.GetFieldValue(Index: String): String;
var
  i: Integer;
begin
  i := fFields.IndexOf(Index);
  if i >= 0 then Result := fFields.Data[i]
  else begin
    if Index = 'Designator' then
      Result := fDesignator
    else
      Result := '';
	end;
end;

constructor TSchematicComponent.Create(aParent: TKicadSchematic; aStartLineIndex: Integer);
begin
	inherited Create(aParent, aStartLineIndex);
  fFields := TFields.Create;
end;

destructor TSchematicComponent.Destroy;
begin
  fFields.Free;
  inherited Destroy;
end;

{ TKicadSchematic }

function TKicadSchematic.GetLine(Index: Integer): String;
begin
  Result := fLines.Strings[Index];
end;

function TKicadSchematic.GetTopLevelSchematic: TKicadSchematic;
begin
  Result := Self;
  while Result.ParentSheet <> nil do
    Result := Result.ParentSheet.ParentSchematic;
end;

function TKicadSchematic.GetComponents(Index: Integer): TSchematicComponent;
begin
  Result := fComponents[Index];
end;

function TKicadSchematic.GetComponentCount: Integer;
begin
  Result := fComponents.Count;
end;

procedure TKicadSchematic.Parse;
var
  line_index, i: Integer;
  schematic_object, existing_object: TSchematicObject;
begin
  line_index := 0;
  while line_index < fLines.Count do
  begin
    schematic_object := nil;
    if fLines[line_index] = '$Comp' then
      schematic_object := TSchematicComponent.Create(Self, line_index)
		else if fLines[line_index] = '$Sheet' then
      schematic_object := TSchematicSheet.Create(Self, line_index)
    else begin
      for i := 0 to fParameters.Count-1 do
      begin
        if fLines[line_index].StartsWith(fParameters.Keys[i]) then begin
          fParameters.Data[i] := fLines[line_index].Substring(Length(fParameters.Keys[i])+1).DeQuotedString('"');
        end;
			end;
		end;

    // parse the found object
    if schematic_object <> nil then
      line_index := schematic_object.Parse
    else begin
      Inc(line_index);
      continue;
    end;

    // check if this is only a virtual component
    if schematic_object is TSchematicComponent then
    begin
      if TSchematicComponent(schematic_object).IsVirtualComponent then
      begin
        FreeAndNil(schematic_object);
        continue;
      end;
    end;

    // check if this object has already been parsed
    if TopLevelSchematic.Objects.IndexOf(schematic_object.UID) >= 0 then
    begin
      existing_object := TopLevelSchematic.Objects[schematic_object.UID];
      schematic_object.Free;
      schematic_object := existing_object;
    end;

    fObjects.Add(schematic_object.UID, schematic_object);

    // add object to the corresponding list
    if schematic_object is TSchematicComponent then
    begin
			fComponents.Add(TSchematicComponent(schematic_object));
			//fComponents.Add(schematic_object.fUID, TSchematicComponent(schematic_object));
		end else if schematic_object is TSchematicSheet then
    begin
      //fSheets.Add(schematic_object.fUID, TSchematicSheet(schematic_object));
      fSheets.Add(TSchematicSheet(schematic_object));
      // add all components of the parsed sheet to the component list of current schematic (Self)
      for i := 0 to TSchematicSheet(schematic_object).Schematic.Objects.Count-1 do
        fObjects.Add(TSchematicSheet(schematic_object).Schematic.Objects.Keys[i], TSchematicSheet(schematic_object).Schematic.Objects.Data[i]);
    end;
	end;
end;

constructor TKicadSchematic.Create(aParent: TSchematicSheet);
begin
  fComponents := TComponents.Create;
  fSheets := TSheets.Create;
  fParent := aParent;
  fObjects := TObjects.Create;
  fParameters := TParameters.Create;
  fParameters.Add('Rev', '');
  fParameters.Add('Title', '');
  fParameters.Add('Date', '');
  fParameters.Add('Comment1', '');
  fParameters.Add('Comment2', '');
  fParameters.Add('Comment3', '');
  fParameters.Add('Comment4', '');
end;

destructor TKicadSchematic.Destroy;
var
  c: TSchematicComponent;
  s: TSchematicSheet;
begin
  for c in fComponents do
  begin
    TopLevelSchematic.Objects[c.UID] := nil;
    c.Free;
  end;

  for s in fSheets do
  begin
    TopLevelSchematic.Objects[s.UID] := nil;
    s.Free;
  end;

	fObjects.Free;
  fLines.Free;

  if fComponents <> nil then FreeAndNil(fComponents);
  if fSheets <> nil then FreeAndNil(fSheets);
  inherited Destroy;
end;

procedure TKicadSchematic.Open(aFilename: String);
begin
  fFilename := aFilename;
  fLines := TStringList.Create;
  fLines.LoadFromFile(fFileName);
  Parse();
end;

end.


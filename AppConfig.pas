unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONPropStorage, Forms, Controls, ExtCtrls;


var
  AppDataDir: UnicodeString;
  PropStorage: TJSONPropStorage;

procedure StoreFormState(aForm: TForm);
procedure RestoreFormState(aForm: TForm; DontRestoreSize: Boolean = False);

implementation

uses shlobj;

function GetAppDataDir: UnicodeString;
//var
  //Pfad: UnicodeString;
begin
{  SetLength(Pfad, MAX_PATH+1);
  SHGetFolderPathW(0, CSIDL_APPDATA, 0, 0, @Pfad[1]);
  Result := StrPas(PWideChar(@Pfad[1])) + UTF8Decode('\CT-CamControl');}
  Result := ExtractFilePath(ParamStr(0));
  //if not DirectoryExists(Result) then raise Exception.Create('Konfigurations-Verzeichnis ' + UTF8Encode(Result) + ' existiert nicht. Ist die Installation fehlerhaft?');
end;

procedure StoreFormState(aForm: TForm);
{var
  i: Integer;
  s: TSplitter;}
begin
  with PropStorage do begin
    WriteRect('Forms.' + aForm.Name + '.NormalPos', Bounds(aForm.Left, aForm.Top, aForm.Width, aForm.Height));
    WriteRect('Forms.' + aForm.Name + '.RestoredPos', Bounds(aForm.RestoredLeft, aForm.RestoredTop, aForm.RestoredWidth, aForm.RestoredHeight));
    WriteInteger('Forms.' + aForm.Name + '.WindowState', Integer(aForm.WindowState));
  end;
  { outcommented, because we must go through the form design recursively to capture all splitters
  // save splitter positions
  for i := 0 to aForm.ControlCount-1 do
  begin
    if aForm.Controls[i] is TSplitter then
    begin
      s := TSplitter(aForm.Controls[i]);
      with PropStorage do begin
        if s.ResizeAnchor = akLeft then
          WriteInteger('Forms.' + aForm.Name + '.Splitters.' + s.Name, s.Left)
        else if s.ResizeAnchor = akTop then
          WriteInteger('Forms.' + aForm.Name + '.Splitters.' + s.Name, s.Top);
			end;
		end;
	end;
  }
end;

function CheckFormIsPartialVisible(Rect: TRect; Part: Integer): TRect;
var
  Width: Integer;
  Height: Integer;
begin
  Result := Rect;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
  if Result.Right < (Screen.DesktopLeft + Part) then begin
    Result.Left := Screen.DesktopLeft + Part - Width;
    Result.Right := Screen.DesktopLeft + Part;
  end;
  if Result.Left > (Screen.DesktopLeft + Screen.DesktopWidth - Part) then begin
    Result.Left := Screen.DesktopLeft + Screen.DesktopWidth - Part;
    Result.Right := Screen.DesktopLeft + Screen.DesktopWidth - Part + Width;
  end;
  if Result.Bottom < (Screen.DesktopTop + Part) then begin
    Result.Top := Screen.DesktopTop + Part - Height;
    Result.Bottom := Screen.DesktopTop + Part;
  end;
  if Result.Top > (Screen.DesktopTop + Screen.DesktopHeight - Part) then begin
    Result.Top := Screen.DesktopTop + Screen.DesktopHeight - Part;
    Result.Bottom := Screen.DesktopTop + Screen.DesktopHeight - Part + Height;
  end;
end;

procedure RestoreFormState(aForm: TForm; DontRestoreSize: Boolean = False);
var
  LastWindowState: TWindowState;
  myRect: TRect;
{	i: Integer;
	s: TSplitter;}
begin
  with PropStorage do
  begin
    LastWindowState := TWindowState(ReadInteger('Forms.' + aForm.Name + '.WindowState', Integer(aForm.WindowState)));

    if LastWindowState = wsMaximized then begin
      aForm.WindowState := wsNormal;
      ReadRect('Forms.' + aForm.Name + '.RestoredPos', myRect, Bounds(aForm.RestoredLeft, aForm.RestoredTop, aForm.RestoredWidth, aForm.RestoredHeight));
      if DontRestoreSize then
      begin
        myRect.Right := myRect.Left + aForm.RestoredWidth;
        myRect.Bottom := myRect.Top + aForm.RestoredHeight;
			end;
			aForm.BoundsRect := CheckFormIsPartialVisible(myRect, 100);
      aForm.WindowState := wsMaximized;
    end else begin
      aForm.WindowState := wsNormal;
      ReadRect('Forms.' + aForm.Name + '.NormalPos', myRect, Bounds(aForm.Left, aForm.Top, aForm.Width, aForm.Height));
      if DontRestoreSize then
      begin
        myRect.Right := myRect.Left + aForm.RestoredWidth;
        myRect.Bottom := myRect.Top + aForm.RestoredHeight;
			end;
      aForm.BoundsRect := CheckFormIsPartialVisible(myRect, 100);
    end;
  end;
  { outcommented because we must ensure, that we do not load any invalid splitter positions, and we must iterate recursively over the form controls to handle all splitters
  // restore splitter positions
  for i := 0 to aForm.ControlCount-1 do
  begin
    if aForm.Controls[i] is TSplitter then
    begin
      s := TSplitter(aForm.Controls[i]);
      with PropStorage do begin
        if s.ResizeAnchor = akLeft then
          s.Left := ReadInteger('Forms.' + aForm.Name + '.Splitters.' + s.Name, s.Left)
        else if s.ResizeAnchor = akTop then
          s.Top := ReadInteger('Forms.' + aForm.Name + '.Splitters.' + s.Name, s.Top);
			end;
		end;
	end;}
end;

initialization
  AppDataDir := GetAppDataDir;
  PropStorage := TJSONPropStorage.Create(nil);
  PropStorage.JSONFileName := UTF8Encode(AppDataDir) + '\Config.json';
  PropStorage.Formatted := True;

finalization
  PropStorage.Free;

end.

end.


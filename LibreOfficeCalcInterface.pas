unit LibreOfficeCalcInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, comobj, lclproc, variants;

type

	{ TCalcDocument }

  TCalcDocument = class
  private
    varCLApp: OleVariant;
    varDesktop: OleVariant;
    varSheets: OleVariant;
    varSheet: OleVariant;
    varDocument: OleVariant;
    varLoadParams: OleVariant;
  public
    function GetCell(X, Y: Integer): String;
    procedure SetCell(X, Y: Integer; Value: String);
    procedure ReadFromFile(Filename: String);
    procedure SaveToFile(Filename: String);
    constructor Create;
  end;

implementation

{function x: Boolean;
var
begin
  // Erstellt das OleObjekt und erstellt den Zeiger darauf
  varCLApp := CreateOleObject('com.sun.star.ServiceManager');
  // Erstellt den Zeiger auf den Officedesktop
  varDesktop := varCLApp.createInstance('com.sun.star.frame.Desktop');
  // Erstellt ein leeres Array das für die Dokumentenerstellung benötigt wird
  varLoadParams := VarArrayCreate([0, -1], varVariant);
  // Öffnet ein vorhandenes Dokument (Workbook) und setzt den Zeiger darauf
  varDokument := varDesktop.LoadComponentFromURL('file:///C:/Users/user258/Testdatei.ods','_blank',0, varLoadParams);
  // Erstellt den Zeiger auf die Sheets
  varSheets := varDokument.getSheets();
  // Erstellt den Zeiger auf ein einzelnes Sheet
  varSheet1 := varSheets.getByIndex(0);

  x := UTF8Decode('Txt 1 ÄäÜ');
  varSheet1.getCellByPosition(0,1).setString(x);
  //ShowMessage(varSheet1.getCellByPosition(0,0).getString);


  // Speichert die Datei unter ihrem aktuellen Namen
  varDokument.store;
  //varDokument.close(True);

  Application.ProcessMessages;
  varDokument.close(True);

  varDesktop.terminate; //Beendet die Anwendung

  varCLApp := UnAssigned;
  varDesktop := UnAssigned;
  varSheets := UnAssigned;
  varSheet1 := UnAssigned;
  varDokument := UnAssigned;
  varLoadParams := UnAssigned;
end;
}

{ TCalcDocument }

function TCalcDocument.GetCell(X, Y: Integer): String;
begin
  Result := varSheet.getCellByPosition(X,Y).getString;
end;

procedure TCalcDocument.SetCell(X, Y: Integer; Value: String);
var
  tmp: WideString;
begin
  tmp := UTF8Decode(Value);
  varSheet.getCellByPosition(X,Y).setString(tmp);
end;

procedure TCalcDocument.ReadFromFile(Filename: String);
var
  x: WideString;
begin
  // Erstellt ein leeres Array das für die Dokumentenerstellung benötigt wird
  varLoadParams := VarArrayCreate([0, -1], varVariant);
  // Öffnet ein vorhandenes Dokument (Workbook) und setzt den Zeiger darauf
  // 'file:///C:/Users/user258/Testdatei.ods'
  x := 'file:///' + UTF8Decode(StringReplace(Filename, '\', '/', [rfReplaceAll]));
  //x := UTF8Decode(Filename);
  varDocument := varDesktop.LoadComponentFromURL(x,'_blank',0, varLoadParams);
  // Erstellt den Zeiger auf die Sheets
  varSheets := varDocument.getSheets();
  // Erstellt den Zeiger auf ein einzelnes Sheet
  varSheet := varSheets.getByIndex(0);
end;

procedure TCalcDocument.SaveToFile(Filename: String);
var
  strDateiname: Widestring;
begin
  strDateiname := 'file:///' + UTF8Decode(StringReplace(Filename, '\', '/', [rfReplaceAll]));
  varDocument.StoreAsURL(strDateiname, varLoadParams);
end;

constructor TCalcDocument.Create;
begin
  varCLApp := CreateOleObject('com.sun.star.ServiceManager');
  varDesktop := varCLApp.createInstance('com.sun.star.frame.Desktop');
end;

end.


unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  EditBtn,
  StdCtrls,
  Process,
  IniFiles;

type

  { TfmMain }

  TfmMain = class(TForm)
    btnRun: TButton;
    cbScript: TComboBox;
    ebCUEFileName: TFileNameEdit;
    lblConversionLog: TLabel;
    meConversionLog: TMemo;
    procedure btnRunClick(Sender: TObject);
    procedure ebCUEFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    settings: TINIFile;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  settings := TINIFile.Create('cueshell.ini');

  ebCUEFileName.InitialDir := settings.ReadString('cueshell', 'CUEInitialDir', '');
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  settings.WriteString('cuegui', 'CUEInitialDir', ExtractFilePath(ebCUEFileName.InitialDir));

  CloseAction := caFree;
end;

procedure TfmMain.ebCUEFileNameAcceptFileName(Sender: TObject; var Value: String);
begin
  ebCUEFileName.InitialDir := ExtractFilePath(Value);
end;

procedure TfmMain.btnRunClick(Sender: TObject);
var
  e, c, o: ansistring;
begin
  case cbScript.ItemIndex of
       0: begin
          e := 'python';
          c := 'cuesplit.py';
       end;
       1: begin
          e := 'python';
          c := 'cuezeros.py';
       end;
  end;

  RunCommand(e, [c, ebCUEFileName.FileName], o, [poStderrToOutPut]);

  meConversionLog.Lines.Text := o
end;

end.


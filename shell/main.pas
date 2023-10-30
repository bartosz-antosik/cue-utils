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
    btnConvertWAVtoTAGs: TButton;
    cbScript: TComboBox;
    ebCUEFileName: TFileNameEdit;
    lblConversionLog: TLabel;
    meConversionLog: TMemo;
    procedure btnConvertTAGstoCSVClick(Sender: TObject);
    procedure btnConvertWAVtoTAGsClick(Sender: TObject);
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
  settings := TINIFile.Create('cuegui.ini');

  ebCUEFileName.InitialDir := settings.ReadString('cuegui', 'CUEInitialDir', '');
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

procedure TfmMain.btnConvertWAVtoTAGsClick(Sender: TObject);
var
  o: string;
begin
  if RunCommand('python.exe', ['wav2time.py ', ebCUEFileName.FileName], o) then
    meConversionLog.Lines.Text := o
  else
    meConversionLog.Lines.Text := 'Error runing script!';
end;

procedure TfmMain.btnConvertTAGstoCSVClick(Sender: TObject);
var
  o: string;
begin
end;

end.


unit Display;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, dsoControl;

{ TForm2 }

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    SGridData: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
     { private declarations }
  public
    procedure ProcessDataBuffers;
  end;

var
  Form2: TForm2;
  HexStrings:  array[0..255] of String[2];

implementation

{$R *.lfm}

Uses main;

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
var i: integer;
begin
 {Create a table of two-digit hex strings}
 for i := 0 to 255 do HexStrings[i] := IntToHex(i,2);
 Form2.Left := Form1.Left+Form1.Width+2;
 Form2.Top := Form1.Top;
 Form2.Height := Form1.Height;
end;

procedure TForm2.ProcessDataBuffers;
var i: integer;
    temp: word;
begin
 Form2.SGridData.clean;
 {First Pass Over Raw Data.}
 {=====From trigger point to end of buffer}
 for i := 0 to (buffersize-1) do
  begin
   {initialize the data string list index column }
   SGridData.cells[0,i] := intToStr(i);
   {Handle Dbuffer[i,0] data}
   temp := Data.DB[i,CH2];     //DBuffer[i,0] contains channel 2 data
   SGridData.cells[2,i] := HexStrings[temp];
   temp := Data.DB[i,CH1];     //DBuffer[i,1] contains channel 1 data
   SGridData.cells[1,i] := HexStrings[temp];
  end;
end;
end.


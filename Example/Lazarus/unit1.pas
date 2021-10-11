unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Diagram;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Diagram1: TDiagram;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var x,y:double;
begin
  x:=-10;
  Diagram1.Curve['Curve1'].ID:='косинусоида'; // переменовываем созданую по умолчанию кривую Curve1
  Diagram1.Curve['косинусоида'].Pen.Color:=clBlue;
  Diagram1.Curve['косинусоида'].Pen.Width:=2;
  Diagram1.Curve['косинусоида'].MinY:=-3;
  Diagram1.Curve['косинусоида'].MaxY:=3;
  Diagram1.Curve['косинусоида'].AxisY.ScaleStep:=1;
  Diagram1.Curve['косинусоида'].Grid.GridStepY:=0.25;
  Diagram1.AddCurve('синусоида');
  Diagram1.Curve['синусоида'].Pen.Color:=clRed;
  Diagram1.Curve['синусоида'].Pen.Width:=2;
  Diagram1.Curve['синусоида'].MinY:=-3;
  Diagram1.Curve['синусоида'].MaxY:=3;
  Diagram1.AddCurve('3*cos(x)/2');
  Diagram1.Curve['3*cos(x)/2'].Pen.Color:=clGreen;
  Diagram1.Curve['3*cos(x)/2'].Pen.Width:=2;
  Diagram1.Curve['3*cos(x)/2'].MinY:=-3;
  Diagram1.Curve['3*cos(x)/2'].MaxY:=3;
  Diagram1.AddCurve('2*sin(x)*cos(x)/x');
  Diagram1.Curve['2*sin(x)*cos(x)/x'].Pen.Color:=$FF8000;
  Diagram1.Curve['2*sin(x)*cos(x)/x'].Pen.Width:=2;
  Diagram1.Curve['2*sin(x)*cos(x)/x'].MinY:=-3;
  Diagram1.Curve['2*sin(x)*cos(x)/x'].MaxY:=3;
  //  Diagram1.Curve['2*sin(x)*cos(x)/x'].Visible:=false; // не отображаем кривую и легенду
  Diagram1.Legend.Top:=4;
  Diagram1.Legend.Right:=2;
  Diagram1.Legend.Visible:=true;
  Diagram1.Legend.Style:=lsWindow;
  repeat
  y:=cos(x);
  Diagram1.Curve['косинусоида'].AddPoint(x,y);
  y:=sin(x);
  Diagram1.Curve['синусоида'].AddPoint(x,y);
  y:=3*cos(x)/2;
  Diagram1.Curve['3*cos(x)/2'].AddPoint(x,y);
  if x<>0 then
  y:=2*sin(x)*cos(x)/x;
  Diagram1.Curve['2*sin(x)*cos(x)/x'].AddPoint(x,y);
  x:=x+0.1;
  until x>10;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SaveDialog1.FileName:='';
  SaveDialog1.Execute;
  if SaveDialog1.FileName<>'' then
  Diagram1.SaveToSVG(SaveDialog1.FileName);
end;

end.


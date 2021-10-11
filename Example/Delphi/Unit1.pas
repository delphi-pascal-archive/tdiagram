unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Diagram, StdCtrls;

type
  TForm1 = class(TForm)
    Diagram1: TDiagram;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

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

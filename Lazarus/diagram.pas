//***********************************************************
// Компонент для отображения графиков для Lazarus 0.9.28
// Версия - 1.0.8
// Лицензия - GPL v.1
// Автор - TEvg
// **********************************************************

unit Diagram;

{$mode Delphi} {$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, Dialogs, ExtCtrls, Variants;

type

  TDateTimeMode = set of (mYear, mMonth, mDay, mHour, mMin, mSec, mMSec);

  TDrawEvent = procedure(Sender: TObject) of object;

  TVariantPoint = record
    X: variant;
    Y: variant;
  end;

  TVariantMarker = record
    X: variant;
    Y: variant;
  end;

  TPPoint = ^TVariantPoint;
  TPMarker = ^TVariantMarker;

  TAxisType = (atAbsciss, atOrdinate);

  TLegendStyle = (lsWindow, lsFlat);
  TTColor = array[1..4] of byte;

  TCustomDiagram = class(TCustomImage)
  private
    { Private declarations }
    FWidth: integer;
    FHeight: integer;
    procedure SetColorBackground(Value: TColor); virtual; abstract;
  protected
    { Protected declarations }
    FColorBackground: TColor;
  public
    { Public declarations }
  published
    { Published declarations }
    property ColorBackground: TColor read FColorBackground write SetColorBackground;
    property Align;
    property Visible;
  end;

  TCustomCurve = class(TObject)
  private
    { Private declarations }
    function GetDiagram: TCustomDiagram; virtual; abstract;
    procedure SetDiagram(Value: TCustomDiagram); virtual; abstract;
    property Diagram: TCustomDiagram read GetDiagram write SetDiagram;
  protected
    { Protected declarations }
    aFMinX: variant;
    aFMaxX: variant;
    aFMinY: variant;
    aFMaxY: variant;
    aFVisible: boolean;
  public
    { Public declarations }
    property MinX: variant read aFMinX write aFMinX;
    property MaxX: variant read aFMaxX write aFMaxX;
    property MinY: variant read aFMinY write aFMinY;
    property MaxY: variant read aFMaxY write aFMaxY;
    property Visible: boolean read aFVisible write aFVisible;
  end;

  TCustomAxis = class(TObject)
  private
    { Private declarations }
    function GetCurve: TCustomCurve; virtual; abstract;
    procedure SetCurve(Curve: TCustomCurve); virtual; abstract;
    property Curve: TCustomCurve read GetCurve write SetCurve;
  public
    { Public declarations }
  end;

  TCustomGrid = class(TObject)
  private
    { Private declarations }
    function GetCurve: TCustomCurve; virtual; abstract;
    procedure SetCurve(Curve: TCustomCurve); virtual; abstract;
    property Curve: TCustomCurve read GetCurve write SetCurve;
  public
    { Public declarations }
  end;

  TLegend = class(TPersistent)
  private
    AOwner: TComponent;
    FFont: TFont;
    FRight: integer;
    FStyle: TLegendStyle;
    FTop: integer;
    FVisible: boolean;
    function GetSize: integer;
    procedure SetRight(const AValue: integer);
    procedure SetSize(const AValue: integer);
    procedure SetStyle(const AValue: TLegendStyle);
    procedure SetTop(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Font: TFont read FFont write FFont; // свойства шрифта легенды
  published
    { Published declarations }
    property Right: integer read FRight write SetRight; // отступ легенды от правого края
    property Top: integer read FTop write SetTop;       // отступ легенды от верхнего края
    property Visible: boolean read FVisible write SetVisible; // опрередяет отображать ли легенду
    property Style: TLegendStyle read FStyle write SetStyle;  // режим отрисовки - в окне..
                                                              // или непосредственно поверх графиков
    property Size: integer read GetSize write SetSize; // высота одного элемента легенды в пунктах
  end;

  TGrid = class(TCustomGrid) // класс, определяющий свойство координатной сетки
  private
    { Private declarations }
    FCurve: TCustomCurve;
    FGridStepX: variant;
    FGridStepY: variant;
    FVisible: boolean;
    FPen: TPen;
    function GetCurve: TCustomCurve; override;
    procedure SetCurve(lCurve: TCustomCurve); override;
  public
    { Public declarations }
    constructor Create(lCurve: TCustomCurve);
    destructor Destroy; override;
    procedure Draw; // отрисовка координатной сетки
    property Pen: TPen read FPen write FPen; // обращение к перу координатной сетки
    property GridStepX: variant read FGridStepX write FGridStepX; // шаг сетки по оси абцисс
    property GridStepY: variant read FGridStepY write FGridStepY; // шаг сетки по оси ординат
    property Visible: boolean read FVisible write FVisible; // определяет отображать ли координатную сетку
  end;

  TAxis = class(TCustomAxis) // класс, определяющий свойство координатной оси
  private
    { Private declarations }
    FCoordinate: variant;
    FArrowWidth: integer;
    FArrowLeng: integer;
    FVisible: boolean;
    FVisibleArrow: boolean;
    FVisibleScale: boolean;
    FIndentMin: integer;
    FIndentMax: integer;
    FScaleStep: variant;
    FScalePointReference: variant;
    FAxisType: TAxisType;
    FDistance: integer;
    FScaleSize: integer;
    FScaleEmptyIntervalMin: variant;
    FScaleEmptyIntervalMax: variant;
    FDateTimeMode: TDateTimeMode;
    FFloatPrecision: byte;
    FFloatDigits: byte;
    FPen: TPen;
    FScalePen: TPen;
    FFont: TFont;
    FCurve: TCustomCurve;
    function ToStr(Value: variant): TStringList;
    function GetCurve: TCustomCurve; override;
    procedure SetCurve(lCurve: TCustomCurve); override;
    procedure DrawX;
    procedure DrawY;
  public
    { Public declarations }
    constructor Create(lCurve: TCustomCurve);
    destructor Destroy; override;
    procedure Draw; // отрисовка координатной оси
    property Pen: TPen read FPen write FPen;                 // обращение к перу координатной оси
    property ScalePen: TPen read FScalePen write FScalePen;  // обращение к перу шкалы
    property Coordinate: variant read FCoordinate write FCoordinate; // координата расположения оси
    property ArrowWidth: integer read FArrowWidth write FArrowWidth; // ширина стрелки координатной оси, пикселов
    property ArrowLeng: integer read FArrowLeng write FArrowLeng; // длина стрелки координатной оси, пикселов
    property Visible: boolean read FVisible write FVisible;       // определяет отображать ли ось
    property VisibleArrow: boolean read FVisibleArrow write FVisibleArrow; // определяет отображать ли стрелку
    property VisibleScale: boolean read FVisibleScale write FVisibleScale; // определяет отображать ли шкалу
    property IndentMin: integer read FIndentMin write FIndentMin; // опреляет отступ оси от края компонента, пикселов
    property IndentMax: integer read FIndentMax write FIndentMax; // опреляет отступ оси от противоположного края компонента, пикселов
    property ScaleStep: variant read FScaleStep write FScaleStep; // шаг шкалы
    property ScalePointReference: variant read FScalePointReference
      write FScalePointReference; // нулевая точка шкалы (точка привязки)
    property AxisType: TAxisType read FAxisType write FAxisType;   // тип координатной оси (абцисс или ординат)
    property DateTimeMode: TDateTimeMode read FDateTimeMode write FDateTimeMode;  // определяет режим отображения времени на координатной оси
    property Distance: integer read FDistance write FDistance;     // отступ надписей шкалы от оси, пикселов
    property FloatPrecision: byte read FFloatPrecision write FFloatPrecision; // определение формата чисел с плавающей точкой..
    property FloatDigits: byte read FFloatDigits write FFloatDigits;          // ..для отображения надписей шкалы
    property ScaleSize: integer read FScaleSize write FScaleSize;  // длина черточек шкалы, пикселов
    property ScaleEmptyIntervalMin: variant read FScaleEmptyIntervalMin
      write FScaleEmptyIntervalMin;   // задание границ интервала на координатной оси..
    property ScaleEmptyIntervalMax: variant read FScaleEmptyIntervalMax
    write FScaleEmptyIntervalMax;     //..на котором шкала не отображается
    property Font: TFont read FFont write FFont; // шрифт, используемый для отрисовки шкалы
  end;

  TCurve = class(TCustomCurve) // класс, определяющий свойство кривой
  private
    { Private declarations }
    FDiagram: TCustomDiagram;
    FGrid: TGrid;
    FAxisX: TAxis;
    FAxisY: TAxis;
    FPen: TPen;
    FPointList: TList;
    FMarkerList: TList;
    FID: string;
    FMarkerColor: TColor;
    function GetDiagram: TCustomDiagram; override;
    procedure SetDiagram(Value: TCustomDiagram); override;
    function GetPoint(Index: integer): TVariantPoint;
    procedure SetPoint(Index: integer; Value: TVariantPoint);
    function GetMarker(Index: integer): TVariantMarker;
    procedure SetMarker(Index: integer; Value: TVariantMarker);
  public
    { Public declarations }
    property Pen: TPen read FPen write FPen;        // обращение к перу используемого кривой, для задания его свойств
    property AxisX: TAxis read FAxisX write FAxisX; // обращение к оси абцисс
    property AxisY: TAxis read FAxisY write FAxisY; // обращение к оси ординат
    property Grid: TGrid read FGrid write FGrid;    // обращение к координатной сетке
    procedure AddPoint(X, Y: variant);      // добавляет к кривой новую точку с координатами X,Y
    procedure PutMarker(X, Y: variant);     // добавляет к кривой новый маркер с координатами X,Y
    procedure Draw;                         // отрисовывает кривую
    procedure Clear;                        // удаляет все точки кривой
    procedure InsertPoint(Index: integer; X, Y: variant); // вставка новой точки с координатами X,Y, в позицию Index
    function PointCount: integer;           // возвращает количество точек кривой
    function MarkerCount: integer;          // возвращает количество маркеров кривой
    procedure DeletePoint(Index: integer);  // удаление точки с номером Index
    procedure DeleteMarker(Index: integer); // удаление маркера с номером Index
    constructor Create(lDiagram: TCustomDiagram);
    destructor Destroy; override;
    property ID: string read FID write FID; // идентификатор кривой ID
    property Point[Index: integer]: TVariantPoint read GetPoint write SetPoint; // обращение к точке с позицией Index
    property Marker[Index: integer]: TVariantMarker read GetMarker write SetMarker; // обращение к маркеру с позицией Index
    property MarkerColor: TColor read FMarkerColor write FMarkerColor; // определяет цвет отрисовки маркеров
  end;

type

  { TDiagram }

  TDiagram = class(TCustomDiagram)
  private
    { Private declarations }
    FAOwner: TComponent;
    FCurveList: TList;
    FOnReDraw: TDrawEvent;
    FLegend: TLegend;
    procedure SetColorBackground(lColor: TColor); override;
    function GetCurveIx(Index: integer): TCurve;
    procedure SetCurveIx(Index: integer; Value: TCurve);
    function GetCurveCount: integer;
    function GetCurve(ID: string): TCurve;
    procedure SetCurve(ID: string; Value: TCurve);
    procedure DrawLegend; // процедура отрисовки легенды
    procedure SetLegend(const AValue: TLegend);
  public
    { Public declarations }
    property Curve[ID: string]: TCurve read GetCurve write SetCurve; // обращение к кривой по идентификатору ID
    property CurveIx[Index: integer]: TCurve read GetCurveIx write SetCurveIx; // обращение к кривой по номеру в списке
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetIndex(ID: string): integer; // возвращает номер в списке, по ID
    procedure AddCurve(ID: string); // создает новую кривую
    function DeleteCurve(ID: string): boolean; // удаляет кривую
    procedure DeleteCurveIndex(Index: integer); // удаляет кривую с номером Index
    procedure SaveToSVG(FileName: string);
    procedure ReDraw; // полностью перерисовывает компонент
    procedure Paint; override;
    property CurveCount: integer read GetCurveCount; // возвращает количество кривых в данный момент
  published
    { Published declarations }
    property Legend: TLegend read FLegend write SetLegend; // легенда и её свойства
    property OnReDraw: TDrawEvent read FOnReDraw write FOnReDraw;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard', [TDiagram]);
end;

constructor TAxis.Create(lCurve: TCustomCurve);
begin
  inherited Create;
  FCurve := lCurve;
  FPen := TPen.Create;
  FScalePen := TPen.Create;
  FFont := TFont.Create;
  Pen.Color := clBlack;
  Pen.Style := psSolid;
  Pen.Width := 2;
  Font.Color := clBlack;
  Font.Size := 10;
  ScalePen.Color := clBlack;
  ScalePen.Style := psSolid;
  ScalePen.Width := 1;
  FFloatPrecision := 4;
  FFloatDigits := 1;
  DateTimeMode := [mHour, mMin];
end;

destructor TAxis.Destroy;
begin
  FPen.Destroy;
  FScalePen.Destroy;
  FFont.Destroy;
  inherited Destroy;
end;

constructor TGrid.Create(lCurve: TCustomCurve);
begin
  inherited Create;
  FCurve := lCurve;
  FGridStepX := 1;
  FGridStepY := 1;
  FGridStepX := (Curve.MaxX - Curve.MinX) / 10;
  FGridStepY := (Curve.MaxY - Curve.MinY) / 10;
  FPen := TPen.Create;
  Pen.Color := clGray;
  Pen.Style := psSolid;
  Pen.Width := 1;
end;

destructor TGrid.Destroy;
begin
  FPen.Destroy;
  inherited Destroy;
end;

constructor TCurve.Create(lDiagram: TCustomDiagram);
begin
  inherited Create;
  MinX := -12;
  MaxX := 12;
  MinY := -12;
  MaxY := 12;
  Visible := True;
  FDiagram := lDiagram;
  FPen := TPen.Create;
  FPointList := TList.Create;
  FMarkerList := TList.Create;
  FAxisX := TAxis.Create(Self);
  FAxisY := TAxis.Create(Self);
  FGrid := TGrid.Create(Self);
  Pen.Color := clBlack;
  Pen.Style := psSolid;
  Pen.Width := 1;
  Grid.Visible := False;
  Grid.GridStepX := 1;
  Grid.GridStepY := 1;
  AxisX.Visible := False;
  AxisY.Visible := False;
  AxisX.Coordinate := 0;
  AxisY.Coordinate := 0;
  AxisX.IndentMin := 5;
  AxisX.IndentMax := 5;
  AxisY.IndentMin := 5;
  AxisY.IndentMax := 5;
  AxisX.VisibleArrow := True;
  AxisY.VisibleArrow := True;
  AxisX.ArrowLeng := 20;
  AxisX.ArrowWidth := 6;
  AxisY.ArrowLeng := 20;
  AxisY.ArrowWidth := 6;
  AxisX.ScaleStep := 2;
  AxisY.ScaleStep := 2;
  AxisX.ScalePointReference := 0;
  AxisY.ScalePointReference := 0;
  AxisX.AxisType := atAbsciss;
  AxisY.AxisType := atOrdinate;
  AxisX.Distance := 6;
  AxisY.Distance := 5;
  AxisX.ScaleSize := 8;
  AxisY.ScaleSize := 8;
  AxisX.VisibleScale := True;
  AxisY.VisibleScale := True;
  AxisX.ScaleEmptyIntervalMin := -0.99;
  AxisY.ScaleEmptyIntervalMin := -0.99;
  AxisX.ScaleEmptyIntervalMax := 0.99;
  AxisY.ScaleEmptyIntervalMax := 0.99;
end;

destructor TCurve.Destroy;
begin
  while PointCount > 0 do
    DeletePoint(0);
  FPointList.Destroy;
  while MarkerCount > 0 do
    DeleteMarker(0);
  FMarkerList.Destroy;
  FPen.Destroy;
  FAxisY.Destroy;
  FAxisX.Destroy;
  FGrid.Destroy;
  inherited Destroy;
end;

constructor TDiagram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAOwner := AOwner;
  FWidth := 0;
  FHeight := 0;
  FColorBackground := clWhite;
  FCurveList := TList.Create;
  AddCurve('Curve1');
  Curve['Curve1'].Grid.Visible := True;
  Curve['Curve1'].AxisX.Visible := True;
  Curve['Curve1'].AxisY.Visible := True;
  FLegend := TLegend.Create;
  FLegend.AOwner := Self;
  FLegend.Font := TFont.Create;
  FLegend.Top := 4;
  FLegend.Right := 4;
  FLegend.Style := lsWindow;
  FLegend.Visible := True;
  FLegend.Size := 10;
end;

destructor TDiagram.Destroy;
var
  i: integer;
  FCurve: TCurve;
begin
  FLegend.Font.Free;
  FLegend.Free;
  for i := 0 to FCurveList.Count - 1 do
  begin
    FCurve := FCurveList.Items[i];
    FCurve.Free;
  end;
  FCurveList.Destroy;
  inherited Destroy;
end;

{ TLegend }

procedure TLegend.SetRight(const AValue: integer);
begin
  if FRight = AValue then
    exit;
  FRight := AValue;
  if csDesigning in AOwner.ComponentState then
    (AOwner as TDiagram).ReDraw;
end;

function TLegend.GetSize: integer;
begin
  Result := FFont.Size;
end;

procedure TLegend.SetSize(const AValue: integer);
begin
  if Size = AValue then
    exit;
  FFont.Size := AValue;
  if csDesigning in AOwner.ComponentState then
    (AOwner as TDiagram).ReDraw;
end;

procedure TLegend.SetStyle(const AValue: TLegendStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;
  if csDesigning in AOwner.ComponentState then
    (AOwner as TDiagram).ReDraw;
end;

procedure TLegend.SetTop(const AValue: integer);
begin
  if FTop = AValue then
    exit;
  FTop := AValue;
  if csDesigning in AOwner.ComponentState then
    (AOwner as TDiagram).ReDraw;
end;

procedure TLegend.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    exit;
  FVisible := AValue;
  if csDesigning in AOwner.ComponentState then
    (AOwner as TDiagram).ReDraw;
end;

procedure TCurve.AddPoint(X, Y: variant);
var
  X1, Y1, X2, Y2: variant;
  Width, Height: integer;
  PPoint: TPPoint;
  ix1, ix2, iy1, iy2: integer;
begin
  if assigned(Self) then
  begin
    Width := Diagram.Width;
    Height := Diagram.Height;
    if FPointList.Count > 0 then
    begin
      try
        X1 := (Width * (Point[PointCount - 1].X - MinX)) / (MaxX - MinX);
        Y1 := Height - (Height * (Point[PointCount - 1].Y - MinY)) / (MaxY - MinY);
        X2 := (Width * (X - MinX)) / (MaxX - MinX);
        Y2 := Height - (Height * (Y - MinY)) / (MaxY - MinY);
        ix1 := Round(X1);
        iy1 := Round(Y1);
        ix2 := Round(X2);
        iy2 := Round(Y2);
        if (ix1 <> ix2) or (iy1 <> iy2) then
        begin
          if Visible then
          begin
            Diagram.Canvas.Pen := Pen;
            Diagram.Canvas.MoveTo(ix1, iy1);
            Diagram.Canvas.LineTo(ix2, iy2);
          end;{if Visible - отрисовываем}
          New(PPoint);
          PPoint^.X := X;
          PPoint^.Y := Y;
          FPointList.Add(PPoint);
        end;{(ix1<>ix2) or (iy1<>iy2) - вычисленные координаты точки
                                        отличаются от предыдущей - отрисовываем}
      except
        on EAbort do
          exit;
      end;{try except}
    end
    else
    begin
      New(PPoint);
      PPoint^.X := X;
      PPoint^.Y := Y;
      FPointList.Add(PPoint);
    end;{else}
  end;
end;

function TCurve.PointCount: integer;
begin
  Result := FPointList.Count;
end;

procedure TCurve.Clear;
var
  i: integer;
  PPoint: TPPoint;
begin
  for i := 0 to FPointList.Count - 1 do
  begin
    PPoint := FPointList.Items[i];
    Dispose(PPoint);
  end;
  FPointList.Clear;
end;

procedure TCurve.InsertPoint(Index: integer; X, Y: variant);
var
  PPoint: TPPoint;
begin
  if (FPointList.Count > 1) and (FPointList.Count > Index) then
  begin
    New(PPoint);
    PPoint^.X := X;
    PPoint^.Y := Y;
    FPointList.Insert(Index, PPoint);
  end;
end;

procedure TCurve.DeletePoint(Index: integer);
var
  PPoint: TPPoint;
begin
  PPoint := FPointList.Items[Index];
  Dispose(PPoint);
  FPointList.Delete(Index);
end;

procedure TCurve.SetPoint(Index: integer; Value: TVariantPoint);
begin
  FPointList.Items[Index] := @Value;
end;

function TCurve.GetPoint(Index: integer): TVariantPoint;
var
  PPoint: TPPoint;
begin
  PPoint := FPointList.Items[Index];
  Result := PPoint^;
end;

procedure TGrid.Draw;
var
  xc, yc: integer;
  x, y: variant;
begin
  if Visible then
  begin
    x := Curve.MinX;
    y := Curve.MinY;
    Curve.Diagram.Canvas.Pen := Pen;
    while x < Curve.MaxX do
    begin
      x := x + GridStepX;
      xc := round(Curve.Diagram.Width * (x - Curve.MinX) / (Curve.MaxX - Curve.MinX));
      Curve.Diagram.Canvas.MoveTo(xc, 0);
      Curve.Diagram.Canvas.LineTo(xc, Curve.Diagram.Height);
    end;{while}
    while y < Curve.MaxY do
    begin
      y := y + GridStepY;
      yc := round(Curve.Diagram.Height - (Curve.Diagram.Height * (y - Curve.MinY)) /
        (Curve.MaxY - Curve.MinY));
      Curve.Diagram.Canvas.MoveTo(0, yc);
      Curve.Diagram.Canvas.LineTo(Curve.Diagram.Width, yc);
    end;{while}
  end;{if}
end;

function TGrid.GetCurve: TCustomCurve;
begin
  Result := FCurve;
end;

procedure TGrid.SetCurve(lCurve: TCustomCurve);
begin
  FCurve := lCurve;
end;

procedure TDiagram.ReDraw;
var
  i: integer;
begin
  Canvas.Brush.Color := FColorBackground;
  Canvas.Pen.Color := FColorBackground;
  Canvas.Rectangle(0, 0, Width, Height);
  for i := CurveCount - 1 downto 0 do
    CurveIx[i].Draw;
  DrawLegend;
end;

procedure TDiagram.SetColorBackground(lColor: TColor);
begin
  FColorBackground := lColor;
  if csDesigning in ComponentState then
    ReDraw;
end;

procedure TDiagram.Paint;
begin
  if (Self.Width <> FWidth) or (Self.Height <> FHeight) then
  begin
    FWidth := Self.Width;
    FHeight := Self.Height;
    Picture.Bitmap.SetSize(FWidth, FHeight);
    ReDraw;
  end;
  inherited Paint;
end;

procedure TCurve.Draw;
var
  i, xc, yc: integer;
  x, y: variant;
begin
  if Visible then
  begin
    Grid.Draw;
    AxisX.Draw;
    AxisY.Draw;
    Diagram.Canvas.Pen := Pen;
    for i := 1 to PointCount - 1 do
    begin
      x := Point[i - 1].X;
      y := Point[i - 1].Y;
      xc := round((Diagram.Width * (x - MinX)) / (MaxX - MinX));
      yc := round(Diagram.Height - (Diagram.Height * (y - MinY)) / (MaxY - MinY));
      Diagram.Canvas.MoveTo(xc, yc);
      x := Point[i].X;
      y := Point[i].Y;
      xc := round((Diagram.Width * (x - MinX)) / (MaxX - MinX));
      yc := round(Diagram.Height - (Diagram.Height * (y - MinY)) / (MaxY - MinY));
      Diagram.Canvas.LineTo(xc, yc);
    end; {for}
    Diagram.Canvas.Pen.Color := FMarkerColor;
    for i := 0 to MarkerCount - 1 do
    begin
      x := Marker[i].X;
      y := Marker[i].Y;
      xc := round((Diagram.Width * (x - MinX)) / (MaxX - MinX));
      yc := round(Diagram.Height - (Diagram.Height * (y - MinY)) / (MaxY - MinY));
      Diagram.Canvas.Arc(xc - 3, yc - 3, xc + 3, yc + 3, 0, 1, 0, 0); // рисуем сам круг
    end; {for}
  end; {if}
end;

function TCurve.GetDiagram: TCustomDiagram;
begin
  Result := FDiagram;
end;

procedure TCurve.SetDiagram(Value: TCustomDiagram);
begin
  if assigned(Self) then
    FDiagram := Value;
end;

function TAxis.GetCurve: TCustomCurve;
begin
  Result := FCurve;
end;

procedure TAxis.SetCurve(lCurve: TCustomCurve);
begin
  FCurve := lCurve;
end;

procedure TAxis.Draw;
begin
  if AxisType = atAbsciss then
    DrawX;
  if AxisType = atOrdinate then
    DrawY;
end;

procedure TAxis.DrawX;
var
  xc, yc, correction: integer;
  i: byte;
  FStringList: TStringList;
  x: variant;
  st: string;
begin
  if Visible then
  begin
    Curve.Diagram.Canvas.Pen := Pen;
    yc := round(Curve.Diagram.Height - (Curve.Diagram.Height * (Coordinate - Curve.MinY)) /
      (Curve.MaxY - Curve.MinY));
    Curve.Diagram.Canvas.MoveTo(IndentMin, yc);
    Curve.Diagram.Canvas.LineTo(Curve.Diagram.Width - IndentMax, yc);
    if VisibleArrow then
    begin
      if ((Pen.Width shr 1) shl 1) = Pen.Width then
        correction := 1
      else
        correction := 0;
      Curve.Diagram.Canvas.Pen.Width := 1;
      Curve.Diagram.Canvas.Brush.Color := Pen.Color;
      Curve.Diagram.Canvas.Polygon([Point(Curve.Diagram.Width - IndentMax, yc),
        Point(Curve.Diagram.Width - IndentMax - ArrowLeng, yc - (ArrowWidth shr 1) -
        correction), Point(Curve.Diagram.Width - IndentMax - ArrowLeng, yc +
        (ArrowWidth shr 1)), Point(Curve.Diagram.Width - IndentMax, yc - correction)]);
    end;{if}
    if VisibleScale then
    begin
      Curve.Diagram.Canvas.Font := Font;
      yc := Curve.Diagram.Height - (Curve.Diagram.Height * (Coordinate - Curve.MinY)) /
        (Curve.MaxY - Curve.MinY);
      x := ScalePointReference;
      Curve.Diagram.Canvas.Pen := ScalePen;
      Curve.Diagram.Canvas.Brush.Color := Curve.Diagram.ColorBackground;
      while x < Curve.MaxX do
      begin
        xc := (Curve.Diagram.Width * (x - Curve.MinX)) / (Curve.MaxX - Curve.MinX);
        if (xc > IndentMin) and (xc < Curve.Diagram.Width - IndentMax) then
        begin
          Curve.Diagram.Canvas.MoveTo(xc, yc - (ScaleSize shr 1));
          Curve.Diagram.Canvas.LineTo(xc, yc + (ScaleSize shr 1));
          FStringList := ToStr(x);
          if (x < ScaleEmptyIntervalMin) or (x > ScaleEmptyIntervalMax) then
            for i := 0 to FStringList.Count - 1 do
            begin
              st := FStringList.Strings[i];
              Curve.Diagram.Canvas.TextOut(xc - (Curve.Diagram.Canvas.TextWidth(st) shr 1),
                yc + Distance + i * (Curve.Diagram.Canvas.TextHeight(st)), st);
            end;{for}
          FStringList.Free;
        end;{if}
        x := x + ScaleStep;
      end;{while}
      x := ScalePointReference;
      while x > Curve.MinX do
      begin
        x := x - ScaleStep;
        xc := (Curve.Diagram.Width * (x - Curve.MinX)) / (Curve.MaxX - Curve.MinX);
        if (xc > IndentMin) and (xc < Curve.Diagram.Width - IndentMax) then
        begin
          Curve.Diagram.Canvas.MoveTo(xc, yc - (ScaleSize shr 1));
          Curve.Diagram.Canvas.LineTo(xc, yc + (ScaleSize shr 1));
          FStringList := ToStr(x);
          if (x < ScaleEmptyIntervalMin) or (x > ScaleEmptyIntervalMax) then
            for i := 0 to FStringList.Count - 1 do
            begin
              st := FStringList.Strings[i];
              Curve.Diagram.Canvas.TextOut(xc - (Curve.Diagram.Canvas.TextWidth(st) shr 1),
                yc + Distance + i * (Curve.Diagram.Canvas.TextHeight(st)), st);
            end;{for}
          FStringList.Free;
        end;{if}
      end;{while}
    end;{if}
  end;{if}
end;

procedure TAxis.DrawY;
var
  xc, yc, correction: integer;
  y: variant;
  i: byte;
  st: string;
  FStringList: TStringList;
begin
  if Visible then
  begin
    Curve.Diagram.Canvas.Pen := Pen;
    xc := round((Curve.Diagram.Width * (Coordinate - Curve.MinX)) /
      (Curve.MaxX - Curve.MinX));
    Curve.Diagram.Canvas.MoveTo(xc, IndentMax);
    Curve.Diagram.Canvas.LineTo(xc, Curve.Diagram.Height - IndentMin);
    if VisibleArrow then
    begin
      if ((Pen.Width shr 1) shl 1) = Pen.Width then
        correction := 1
      else
        correction := 0;
      Curve.Diagram.Canvas.Pen.Width := 1;
      Curve.Diagram.Canvas.Brush.Color := Pen.Color;
      Curve.Diagram.Canvas.Polygon([Point(xc, IndentMax),
        Point(xc - correction - ArrowWidth shr 1, IndentMax + ArrowLeng),
        Point(xc + ArrowWidth shr 1, IndentMax + ArrowLeng),
        Point(xc - correction, IndentMax)]);
      if VisibleScale then
      begin
        xc := (Curve.Diagram.Width * (Coordinate - Curve.MinX)) / (Curve.MaxX - Curve.MinX);
        y := ScalePointReference;
        Curve.Diagram.Canvas.Pen := ScalePen;
        Curve.Diagram.Canvas.Brush.Color := Curve.Diagram.ColorBackground;
        while y < Curve.MaxY do
        begin
          yc := Curve.Diagram.Height - (Curve.Diagram.Height * (y - Curve.MinY)) /
            (Curve.MaxY - Curve.MinY);
          if (yc > IndentMin) and (yc < Curve.Diagram.Height - IndentMax) then
          begin
            Curve.Diagram.Canvas.MoveTo(xc - (ScaleSize shr 1), yc);
            Curve.Diagram.Canvas.LineTo(xc + (ScaleSize shr 1), yc);
            if (y < ScaleEmptyIntervalMin) or (y > ScaleEmptyIntervalMax) then
            begin
              FStringList := ToStr(y);
              st := '';
              for i := 0 to FStringList.Count - 1 do
                st := st + ' ' + FStringList.Strings[i];
              Delete(st, 1, 1);
              FStringList.Free;
              Curve.Diagram.Canvas.TextOut(xc - Distance - Curve.Diagram.Canvas.TextWidth(st),
                yc - (Curve.Diagram.Canvas.TextHeight(st) shr 1), st);
            end;
          end;{if}
          y := y + ScaleStep;
        end;{while}
        y := ScalePointReference;
        while y > Curve.MinY do
        begin
          y := y - ScaleStep;
          yc := Curve.Diagram.Height - (Curve.Diagram.Height * (y - Curve.MinY)) /
            (Curve.MaxY - Curve.MinY);
          if (yc > IndentMin) and (yc < Curve.Diagram.Height - IndentMax) then
          begin
            Curve.Diagram.Canvas.MoveTo(xc - (ScaleSize shr 1), yc);
            Curve.Diagram.Canvas.LineTo(xc + (ScaleSize shr 1), yc);
            if (y < ScaleEmptyIntervalMin) or (y > ScaleEmptyIntervalMax) then
            begin
              FStringList := ToStr(y);
              st := '';
              for i := 0 to FStringList.Count - 1 do
                st := st + ' ' + FStringList.Strings[i];
              Delete(st, 1, 1);
              FStringList.Free;
              Curve.Diagram.Canvas.TextOut(xc - Distance - Curve.Diagram.Canvas.TextWidth(st),
                yc - (Curve.Diagram.Canvas.TextHeight(st) shr 1), st);
            end;
          end;{if}
        end;{while}
      end;{if}
    end;{if}
  end;{if}
end;

function TDiagram.GetCurveIx(Index: integer): TCurve;
var
  FCurve: TCurve;
begin
  FCurve := FCurveList.Items[Index];
  Result := FCurve;
end;

procedure TDiagram.SetCurveIx(Index: integer; Value: TCurve);
begin
  FCurveList.Items[Index] := Value;
end;

procedure TDiagram.AddCurve(ID: string);
var
  FCurve: TCurve;
begin
  FCurve := TCurve.Create(Self);
  FCurve.ID := ID;
  FCurveList.Add(FCurve);
end;

function TDiagram.DeleteCurve(ID: string): boolean;
var
  FCurve: TCurve;
  ix: integer;
begin
  Result := False;
  ix := GetIndex(ID);
  if ix = -1 then
    exit
  else
  begin
    FCurve := CurveIx[ix];
    FCurve.Free;
    FCurveList.Delete(ix);
  end;{else}
end;

procedure TDiagram.DeleteCurveIndex(Index: integer);
var
  FCurve: TCurve;
begin
  FCurve := FCurveList.Items[index];
  FCurve.Free;
  FCurveList.Delete(index);
end;

function TDiagram.GetIndex(ID: string): integer;
var
  FCurve: TCurve;
  i, Count: integer;
begin
  i := 0;
  Result := -1;
  Count := FCurveList.Count;
  repeat
    FCurve := FCurveList.Items[i];
    if FCurve.ID = ID then
    begin
      Result := i;
      break;
    end;
    Inc(i);
  until i >= Count;
end;

function TDiagram.GetCurveCount: integer;
begin
  Result := FCurveList.Count;
end;

function TDiagram.GetCurve(ID: string): TCurve;
var
  ix: integer;
begin
  ix := GetIndex(ID);
  if ix = -1 then
    Result := nil
  else
    Result := FCurveList.Items[ix];
end;

procedure TDiagram.SetCurve(ID: string; Value: TCurve);
var
  ix: integer;
begin
  ix := GetIndex(ID);
  if ix <> -1 then
    FCurveList.Items[ix] := Value;
end;

procedure TDiagram.DrawLegend;
var
  st: string;
  i, ix: integer;
  FLeft, FWidth, FHeight: integer;
  FTextWidth, FTextHeight: integer;
begin
  if FLegend.Visible and (CurveCount > 0) then
  begin
    Canvas.Font := FLegend.FFont;
    Canvas.Brush.Color := FColorBackground;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    st := '';
    for i := 0 to CurveCount - 1 do // находим кривую с самым длинным названием
      if CurveIx[i].Visible then
        if Canvas.TextWidth(st) < Canvas.TextWidth(CurveIx[i].ID) then
          st := CurveIx[i].ID;
    st := ' - ' + st;
    FTextWidth := Canvas.TextWidth(st); // и устанавливаем ширину строки
    FTextHeight := Canvas.TextHeight(st); // и высоту строки
    FWidth := FTextWidth + FTextHeight + 4;
    ix := 0;
    for i := 0 to CurveCount - 1 do
      if CurveIx[i].Visible then Inc(ix); // посчитаем сколько у нас видимых кривых
    FHeight := ix * (FTextHeight) + 2; // и вычислим размеры рамки
    FLeft := Width - FWidth - FLegend.Right;
    if FLegend.Style = lsWindow then // отрисовываем рамку
      Canvas.Rectangle(FLeft, FLegend.Top, FLeft + FWidth, FLegend.Top + FHeight); // и нарисуем рамку
    ix := 0;
    for i := 0 to CurveCount - 1 do // вывод строк легенды
    begin
      if CurveIx[i].Visible then
      begin
        Canvas.Font.Color := CurveIx[i].Pen.Color;
        Canvas.Brush.Color := CurveIx[i].Pen.Color;
        Canvas.Rectangle(FLeft + 2, FLegend.Top + ix * FTextHeight + 2, FLeft + FTextHeight,
          FLegend.Top + ix * FTextHeight + FTextHeight);
        Canvas.Brush.Color := FColorBackground;
        Canvas.TextOut(FLeft + FTextHeight + 2, FLegend.Top + ix * FTextHeight +
          1, ' - ' + CurveIx[i].ID);
        Inc(ix);
      end;
    end;
  end;
end;

procedure TDiagram.SetLegend(const AValue: TLegend);
begin
  if FLegend = AValue then
    exit;
  FLegend := AValue;
end;

function TAxis.ToStr(Value: variant): TStringList;
var
  FStringList: TStringList;
  st: string;
  fNum: double;
  wYear, wMonth, wDay, wHour, wMinute, wSecond, wMilliSecond: word;

  function CheckLeng(s: string; Leng: byte): string;
  begin
    if Leng = 2 then
      if length(s) = 1 then
        Result := '0' + s
      else
        Result := s;
    if Leng = 3 then
      if length(s) = 1 then
        Result := '00' + s
      else
      if length(s) = 2 then
        Result := '0' + s
      else
        Result := s;
  end;

begin
  FStringList := TStringList.Create;
  st := '';
  case VarType(Value) of
    varInteger: FStringList.Add(IntToStr(Value));
    varSmallint: FStringList.Add(IntToStr(Value));
    varByte: FStringList.Add(IntToStr(Value));
    varSingle:
    begin
      fNum := Value;
      FStringList.Add(FloatToStrF(fNum, ffFixed, FFloatPrecision, FFloatDigits));
    end;
    varDouble:
    begin
      fNum := Value;
      FStringList.Add(FloatToStrF(fNum, ffFixed, FFloatPrecision, FFloatDigits));
    end;
    varCurrency:
    begin
      fNum := Value;
      FStringList.Add(FloatToStrF(fNum, ffFixed, FFloatPrecision, FFloatDigits));
    end;
    varBoolean: if Value then
        FStringList.Add('true')
      else
        FStringList.Add('false');
    varString: FStringList.Add(Value);
    varDate:
    begin
      DecodeDate(Value, wYear, wMonth, wDay);
      if mDay in DateTimeMode then
        st := st + CheckLeng(IntToStr(wDay), 2) + '.';
      if mMonth in DateTimeMode then
        st := st + CheckLeng(IntToStr(wMonth), 2) + '.';
      if mYear in DateTimeMode then
        st := st + IntToStr(wYear);
      if st <> '' then
        if st[length(st)] = '.' then
          Delete(st, length(st), 1);
      if st <> '' then
        FStringList.Add(st);
      st := '';
      DecodeTime(Value, wHour, wMinute, wSecond, wMilliSecond);
      if mHour in DateTimeMode then
        st := st + CheckLeng(IntToStr(wHour), 2) + ':';
      if mMin in DateTimeMode then
        st := st + CheckLeng(IntToStr(wMinute), 2) + ':';
      if mSec in DateTimeMode then
        st := st + CheckLeng(IntToStr(wSecond), 2);
      if st <> '' then
        if st[length(st)] = ':' then
          Delete(st, length(st), 1);
      if mMSec in DateTimeMode then
      begin
        if st <> '' then
          st := st + '.';
        st := st + CheckLeng(IntToStr(wMilliSecond), 3);
      end;
      if st <> '' then
        FStringList.Add(st);
    end {varDate}
    else {case VarType}
  end; {case VarType}
  Result := FStringList;
end;

procedure TCurve.PutMarker(X, Y: variant);
var
  MX, MY, Width, Height: integer;
  PMarker: TPMarker;
begin
  Width := Diagram.Width;
  Height := Diagram.Height;
  MX := round((Width * (X - MinX)) / (MaxX - MinX));
  MY := round((Height - (Height * (Y - MinY))) / (MaxY - MinY));
  Diagram.Canvas.Pen := Pen;
  Diagram.Canvas.Arc(MX - 3, MY - 3, MX + 3, MY + 3, 0, 1, 0, 0); // рисуем сам круг
  New(PMarker);
  PMarker^.X := X;
  PMarker^.Y := Y;
  FMarkerList.Add(PMarker);
end;

procedure TCurve.DeleteMarker(Index: integer);
var
  PMarker: TPMarker;
begin
  PMarker := FMarkerList.Items[Index];
  Dispose(PMarker);
  FMarkerList.Delete(Index);
end;

function TCurve.MarkerCount: integer;
begin
  Result := FMarkerList.Count;
end;

function TCurve.GetMarker(Index: integer): TVariantMarker;
var
  PMarker: TPMarker;
begin
  PMarker := FMarkerList.Items[Index];
  Result := PMarker^;
end;

procedure TCurve.SetMarker(Index: integer; Value: TVariantMarker);
begin
  FMarkerList.Items[Index] := @Value;
end;

procedure TDiagram.SaveToSVG(FileName: string);
var
  SVGFile: textfile;
  s, st: string;
  xc, yc: single;
  x, y: variant;
  i, j, ix: integer;
  FStringList: TStringList;
  FLeft, FWidth, FHeight: integer;
  FTextWidth, FTextHeight: integer;

  function FloatToStrC(Value: variant): string;
  var
    s: string;
  begin
    s := '0.0';
    if (VarType(Value) = varInteger) or (VarType(Value) = varSmallint) or
      (VarType(Value) = varByte) or (VarType(Value) = varSingle) or
      (VarType(Value) = varDouble) or (VarType(Value) = varDate) then
      s := FloatToStr(Value);
    if pos(',', s) <> 0 then
      s[pos(',', s)] := '.';
    Result := s;
  end;

  function ColorToStrSVG(Color: TColor): string;
  var
    p: ^TTColor;
  begin
    p := @color;
    Result := inttohex(p^[1], 2) + inttohex(p^[2], 2) + inttohex(p^[3], 2);
  end;

  procedure SaveLine(x1, y1, x2, y2: single; id: string; Color: TColor; Width: byte);
  begin
    writeln(SVGFile, '<path');
    writeln(SVGFile, 'd="M ' + FloatToStrC(x1) + ',' + FloatToStrC(y1) +
      ' ' + FloatToStrC(x2) + ',' + FloatToStrC(y2) + '"');
    writeln(SVGFile, 'id="path' + id + '"');
    writeln(SVGFile, 'style="fill:none;stroke:#' + ColorToStrSVG(Color) +
      ';stroke-width:' + IntToStr(Width) + 'px" />');
  end;

  procedure SaveText(x, y: single; id, Text: string; Font: TFont);
  begin
    writeln(SVGFile, '<text');
    writeln(SVGFile, 'x="' + FloatToStrC(x) + '"');
    writeln(SVGFile, 'y="' + FloatToStrC(y) + '"');
    writeln(SVGFile, 'id="' + id + '"');
    writeln(SVGFile, 'style="font-size:' + IntToStr(abs(Font.Height)) +
      'px;font-style:normal;font-weight:normal;fill:#' + ColorToStrSVG(Font.Color) +
      ';fill-opacity:1;stroke:none;font-family:' + Font.Name + '">');
    writeln(SVGFile, Text);
    writeln(SVGFile, '</text>');
  end;

begin
  assignfile(SVGFile, filename);
  rewrite(SVGFile);
  writeln(SVGFile, '<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
  writeln(SVGFile, '<svg');
  writeln(SVGFile, ' version="1.1"');
  writeln(SVGFile, ' width="' + IntToStr(Width) + '"');
  writeln(SVGFile, ' height="' + IntToStr(Height) + '"');
  writeln(SVGFile, ' id="svg1">');
  writeln(SVGFile, '<g id="layer1">');
  writeln(SVGFile, '<path');
  writeln(SVGFile, 'd="M 0,0 '+FloatToStrC(Width) + ',0 '+FloatToStrC(Width) +
  ',' + FloatToStrC(Height) + ' 0,' + FloatToStrC(Height) + ' z"');
  writeln(SVGFile, 'id="path10"');
  writeln(SVGFile, 'style="fill:#' + ColorToStrSVG(FColorBackground) +
    ';stroke:#' + ColorToStrSVG(clBlack) + ';stroke-width:1px" />');
  writeln(SVGFile, '</g>');
  i := CurveCount - 1;
  repeat
  writeln(SVGFile, '<g id="layer' + IntToStr(CurveCount - i + 1) + '">');
  with CurveIx[i] do
    begin
      if Visible then
        if Grid.Visible then
        begin
          x := MinX;
          y := MinY;
          j:=0;
          while x < MaxX do
          begin
            x := x + Grid.GridStepX;
            inc(j);
            xc := Width * (x - MinX) / (MaxX - MinX);
            SaveLine(xc, 0, xc, Height, '51' + IntToStr(i) + IntToStr(j), Grid.Pen.Color,
              Grid.Pen.Width);
          end;{while}
          while y < MaxY do
          begin
            y := y + Grid.GridStepY;
            inc(j);
            yc := Height - (Height * (y - MinY)) / (MaxY - MinY);
            SaveLine(0, yc, Width, yc, '52' + IntToStr(i) + IntToStr(j), Grid.Pen.Color,
              Grid.Pen.Width);
          end;{while}
        end;{if Grid.Visible}
      if AxisY.Visible then
      begin
        xc := (Width * (AxisY.Coordinate - MinX)) / (MaxX - MinX);
        SaveLine(xc, AxisY.IndentMax + AxisY.ArrowLeng, xc, Height - AxisY.IndentMin,
          '40' + IntToStr(i), AxisY.Pen.Color, AxisY.Pen.Width);
        if AxisY.VisibleArrow then
        begin
          writeln(SVGFile, '<path');
          writeln(SVGFile, 'd="M ' + FloatToStrC(xc - AxisY.ArrowWidth shr 1) +
            ',' + FloatToStrC(AxisY.IndentMax + AxisY.ArrowLeng) + ' ' +
            FloatToStrC(xc + AxisY.ArrowWidth shr 1) + ',' +
            FloatToStrC(AxisY.IndentMax + AxisY.ArrowLeng) + ' ' + FloatToStrC(xc) +
            ',' + FloatToStrC(AxisY.IndentMax) + ' z"');
          writeln(SVGFile, 'id="path' + '43' + IntToStr(i) + '"');
          writeln(SVGFile, 'style="fill:#' + ColorToStrSVG(AxisY.Pen.Color) +
            ';stroke:#' + ColorToStrSVG(AxisY.Pen.Color) + ';stroke-width:1px" />');
        end;
        if AxisY.VisibleScale then
        begin
          xc := Width * (AxisY.Coordinate - MinX) / (MaxX - MinX);
          y := AxisY.ScalePointReference;
          while y < MaxY do
          begin
            yc := Height - (Height * (y - MinY)) / (MaxY - MinY);
            if (yc > AxisY.IndentMin) and (yc < Height - AxisY.IndentMax) then
            begin
              SaveLine(xc - (AxisY.ScaleSize shr 1), yc, xc + (AxisY.ScaleSize shr 1), yc,
                '41' + IntToStr(i), AxisY.Pen.Color, AxisY.Pen.Width);
              if (y < AxisY.ScaleEmptyIntervalMin) or (y > AxisY.ScaleEmptyIntervalMax) then
              begin
                FStringList := AxisY.ToStr(y);
                st := '';
                for ix := 0 to FStringList.Count - 1 do
                  st := st + ' ' + FStringList.Strings[ix];
                Delete(st, 1, 1);
                FStringList.Free;
                SaveText(xc - AxisY.Distance - Canvas.TextWidth(st),
                  yc + (AxisY.Font.Size shr 1), 'text41' + IntToStr(i), st, AxisY.Font);
              end;
            end;{if}
            y := y + AxisY.ScaleStep;
          end;{while}
          y := AxisY.ScalePointReference;
          while y > MinY do
          begin
            y := y - AxisY.ScaleStep;
            yc := Height - (Height * (y - MinY)) / (MaxY - MinY);
            if (yc > AxisY.IndentMin) and (yc < Height - AxisY.IndentMax) then
            begin
              SaveLine(xc - (AxisY.ScaleSize shr 1), yc, xc + (AxisY.ScaleSize shr 1), yc,
                '42' + IntToStr(i), AxisY.Pen.Color, AxisY.Pen.Width);
              if (y < AxisY.ScaleEmptyIntervalMin) or (y > AxisY.ScaleEmptyIntervalMax) then
              begin
                FStringList := AxisY.ToStr(y);
                st := '';
                for ix := 0 to FStringList.Count - 1 do
                  st := st + ' ' + FStringList.Strings[ix];
                Delete(st, 1, 1);
                FStringList.Free;
                SaveText(xc - AxisY.Distance - Canvas.TextWidth(st),
                  yc + (AxisY.Font.Size shr 1), 'text42' + IntToStr(i), st, AxisY.Font);
              end;
            end;{if}
          end;{while}
        end;{if VisibleScale}
      end;{AxisY.Visible}
      if AxisX.Visible then
      begin
        yc := round(Height - (Height * (AxisX.Coordinate - MinY)) / (MaxY - MinY));
        SaveLine(AxisX.IndentMin, yc, Width - AxisX.IndentMax - AxisX.ArrowLeng, yc,
          '30' + IntToStr(i), AxisX.Pen.Color, AxisX.Pen.Width);
        if AxisX.VisibleArrow then
        begin
          writeln(SVGFile, '<path');
          writeln(SVGFile, 'd="M ' + FloatToStrC(Width - AxisX.IndentMax) +
            ',' + FloatToStrC(yc) + ' ' + FloatToStrC(Width - AxisX.IndentMax -
            AxisX.ArrowLeng) + ',' + FloatToStrC(yc - (AxisX.ArrowWidth shr 1)) +
            ' ' + FloatToStrC(Width - AxisX.IndentMax - AxisX.ArrowLeng) + ',' +
            FloatToStrC(yc + (AxisX.ArrowWidth shr 1)) + ' ' +
            FloatToStrC(Width - AxisX.IndentMax) + ',' + FloatToStrC(yc) + ' z"');
          writeln(SVGFile, 'id="path33' + IntToStr(i) + '"');
          writeln(SVGFile, 'style="fill:#' + ColorToStrSVG(AxisX.Pen.Color) +
            ';stroke:#' + ColorToStrSVG(AxisX.Pen.Color) + ';stroke-width:1px" />');
        end;{if}
        if AxisX.VisibleScale then
        begin
          yc := Height - (Height * (AxisX.Coordinate - MinY)) / (MaxY - MinY);
          x := AxisX.ScalePointReference;
          while x < MaxX do
          begin
            xc := (Width * (x - MinX)) / (MaxX - MinX);
            if (xc > AxisX.IndentMin) and (xc < Width - AxisX.IndentMax) then
            begin
              SaveLine(xc, yc - (AxisX.ScaleSize shr 1), xc, yc + (AxisX.ScaleSize shr 1),
                '31' + IntToStr(i), AxisX.Pen.Color, AxisX.Pen.Width);
              FStringList := AxisX.ToStr(x);
              if (x < AxisX.ScaleEmptyIntervalMin) or (x > AxisX.ScaleEmptyIntervalMax) then
                for ix := 0 to FStringList.Count - 1 do
                begin
                  st := FStringList.Strings[ix];
                  SaveText(xc - Canvas.TextWidth(st) shr 1, yc + AxisX.Distance +
                    (ix + 1) * abs(AxisX.Font.Height),
                    'text31' + IntToStr(i)+IntToStr(ix), st, AxisX.Font);
                end;{for}
              FStringList.Free;
            end;{if}
            x := x + AxisX.ScaleStep;
          end;{while}
          x := AxisX.ScalePointReference;
          while x > MinX do
          begin
            x := x - AxisX.ScaleStep;
            xc := (Width * (x - MinX)) / (MaxX - MinX);
            if (xc > AxisX.IndentMin) and (xc < Width - AxisX.IndentMax) then
            begin
              SaveLine(xc, yc - (AxisX.ScaleSize shr 1), xc, yc + (AxisX.ScaleSize shr 1),
                '32' + IntToStr(i), AxisX.Pen.Color, AxisX.Pen.Width);
              FStringList := AxisX.ToStr(x);
              if (x < AxisX.ScaleEmptyIntervalMin) or (x > AxisX.ScaleEmptyIntervalMax) then
                for ix := 0 to FStringList.Count - 1 do
                begin
                  st := FStringList.Strings[ix];
                  SaveText(xc - Canvas.TextWidth(st) shr 1, yc + AxisX.Distance +
                    (ix + 1) * abs(AxisX.Font.Height),
                    'text32' + IntToStr(i)+IntToStr(ix), st, AxisX.Font);
                end;{for}
              FStringList.Free;
            end;{if}
          end;{while}
        end;{if VisibleScale}
      end;{if AxisX.Visible}
      if PointCount > 1 then
      begin
        writeln(SVGFile, '<path');
        s := 'd="M';
        for j := 1 to PointCount do
        begin
          x := (Width * (Point[j - 1].X - MinX)) / (MaxX - MinX);
          y := Height - (Height * (Point[j - 1].Y - MinY)) / (MaxY - MinY);
          s := s + ' ' + FloatToStrC(x) + ',' + FloatToStrC(y);
        end;
        s := s + '"';
        writeln(SVGFile, s);
        writeln(SVGFile, 'id="path20' + IntToStr(i) + '"');
        writeln(SVGFile, 'style="fill:none;stroke:#' + ColorToStrSVG(Pen.Color) +
          ';stroke-width:' + IntToStr(Pen.Width) + 'px" />');
      if MarkerCount>0 then
       for ix := 0 to MarkerCount - 1 do
        begin
          x := Marker[ix].X;
          y := Marker[ix].Y;
          xc := (Width * (x - MinX)) / (MaxX - MinX);
          yc := Height - (Height * (y - MinY)) / (MaxY - MinY);
          writeln(SVGFile, '<path');
          writeln(SVGFile, 'd="M ' + FloatToStrC(xc-1) + ',' + FloatToStrC(yc+3) + ' a 3,3 0 1 1 2,0 3,3 z"');
          writeln(SVGFile, 'id="path21'+inttostr(i)+inttostr(ix)+'"');
          writeln(SVGFile,  'style="fill:#'+ColorToStrSVG(FMarkerColor)+
     ';fill-rule:evenodd;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" />');
        end; {for}
      end;
      Dec(i);
    end;
    writeln(SVGFile, '</g>');
  until i < 0;
  if FLegend.Visible and (CurveCount > 0) then
  begin
    writeln(SVGFile, '<g id="layer' + IntToStr(CurveCount + 1) + '">');
    st := '';
    for i := 0 to CurveCount - 1 do // находим кривую с самым длинным названием
      if CurveIx[i].Visible then
        if Canvas.TextWidth(st) < Canvas.TextWidth(CurveIx[i].ID) then
          st := CurveIx[i].ID;
    st := ' - ' + st;
    FTextWidth := Canvas.TextWidth(st); // и устанавливаем ширину строки
    FTextHeight := abs(FLegend.Font.Height); // и высоту строки
    FWidth := FTextWidth + FTextHeight + 4;
    ix := 0;
    for i := 0 to CurveCount - 1 do
      if CurveIx[i].Visible then
        Inc(ix); // посчитаем сколько у нас видимых кривых
    FHeight := ix * (FTextHeight) + 2; // и вычислим размеры рамки
    FLeft := Width - FWidth - FLegend.Right - 2;
    if FLegend.Style = lsWindow then begin // отрисовываем рамку
    writeln(SVGFile, '<path');
    writeln(SVGFile, 'd="M ' + FloatToStrC(FLeft) + ',' + FloatToStrC(FLegend.Top) +
      ' ' + FloatToStrC(FLeft + FWidth) + ',' + FloatToStrC(FLegend.Top) +
      ' ' + FloatToStrC(FLeft + FWidth) + ',' + FloatToStrC(FLegend.Top + FHeight) +
      ' ' + FloatToStrC(FLeft) + ',' + FloatToStrC(FLegend.Top + FHeight) + ' z"');
    writeln(SVGFile, 'id="path12"');
    writeln(SVGFile, 'style="fill:#' + ColorToStrSVG(FColorBackground) +
      ';stroke:#' + ColorToStrSVG(clBlack) + ';stroke-width:1px" />');
    end;
    ix := 0;
    for i := 0 to CurveCount - 1 do // вывод строк легенды
    begin
      if CurveIx[i].Visible then
      begin
        writeln(SVGFile, '<path');
        writeln(SVGFile, 'd="M ' + FloatToStrC(FLeft + 2) + ',' +
          FloatToStrC(FLegend.Top + ix * FTextHeight + 2) + ' ' +
          FloatToStrC(FLeft + FTextHeight) + ',' + FloatToStrC(FLegend.Top +
          ix * FTextHeight + 2) + ' ' + FloatToStrC(FLeft + FTextHeight) +
          ',' + FloatToStrC(FLegend.Top + ix * FTextHeight + FTextHeight) +
          ' ' + FloatToStrC(FLeft + 2) + ',' + FloatToStrC(FLegend.Top + ix *
          FTextHeight + FTextHeight) + ' z"');
        writeln(SVGFile, 'id="path11' + IntToStr(i) + '"');
        writeln(SVGFile, 'style="fill:#' + ColorToStrSVG(CurveIx[i].Pen.Color) +
          ';stroke:#' + ColorToStrSVG(clBlack) + ';stroke-width:1px" />');
        FLegend.Font.Color := CurveIx[i].Pen.Color;
        SaveText(FLeft + FTextHeight + 4, FLegend.Top + (ix + 1) * FTextHeight -
          2, 'text11' + IntToStr(i), ' - ' + CurveIx[i].ID, FLegend.Font);
        Inc(ix);
      end;
    end;
    writeln(SVGFile, '</g>');
  end;
  writeln(SVGFile, '</svg>');
  CloseFile(SVGFile);
end;

initialization
 {$I TDiagram.lrs}

end.


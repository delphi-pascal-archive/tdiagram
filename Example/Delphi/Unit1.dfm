object Form1: TForm1
  Left = 192
  Top = 116
  Width = 467
  Height = 444
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Diagram1: TDiagram
    Left = 0
    Top = 0
    Width = 459
    Height = 417
    ColorBackground = clWhite
    Align = alClient
    Legend.Right = 4
    Legend.Top = 4
    Legend.Visible = True
    Legend.Style = lsWindow
    Legend.Size = 10
  end
  object Button1: TButton
    Left = 2
    Top = 2
    Width = 105
    Height = 25
    Caption = 'Сохранить в SVG'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SaveDialog1: TSaveDialog
    Left = 8
    Top = 32
  end
end

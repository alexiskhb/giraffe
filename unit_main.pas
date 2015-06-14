unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, StdCtrls;

type

  { TMainForm }

  TDirection = (ru, r, rd, d, ld, l, lu, u);

  TLine = class
  private
    FDirection: TDirection;
    FSize: integer;
    FStartPoint: TPoint;
    FEndPoint: TPoint;
  public
    property Direction: TDirection read FDirection;
    property Size: integer read FSize;
    property StartPoint: TPoint read FStartPoint;
    property EndPoint: TPoint read FEndPoint;
    constructor Create(ABitmap: TBitmap; ADirection:
      TDirection; ASize: integer; AStartPoint: TPoint);
  end;

  TMainForm = class(TForm)
    cbColor: TColorButton;
    memoText: TMemo;
    pbPicture: TPaintBox;
    pnlPicture: TPanel;
    pnlText: TPanel;
    pnlColorSize: TPanel;
    spinSize: TSpinEdit;
    bmPicture: TBitmap;
    procedure FormCreate(Sender: TObject);
    procedure pbPictureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPicturePaint(Sender: TObject);
    procedure DrawFigure(APoint: TPoint; AColor: TColor; ASize: integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

constructor TLine.Create(ABitmap: TBitmap; ADirection:
  TDirection; ASize: integer; AStartPoint: TPoint);
begin

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ActiveControl := memoText;
  bmPicture := TBitmap.Create;
  bmPicture.Width := MainForm.Width;
  bmPicture.Height := MainForm.Height;
  with bmPicture.Canvas do begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Pen.Color := clWhite;
    Pen.Style := psSolid;
  end;
  bmPicture.Canvas.Rectangle(0, 0, Width, Height);
end;

procedure TMainForm.pbPictureMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawFigure(Point(X, Y), cbColor.ButtonColor, spinSize.Value);
end;

procedure TMainForm.pbPicturePaint(Sender: TObject);
begin
  with bmPicture do
    pbPicture.Canvas.CopyRect(Rect(0, 0, Width, Height), bmPicture.Canvas, Rect(0, 0, Width, Height));
end;

procedure TMainForm.DrawFigure(APoint: TPoint; AColor: TColor; ASize: integer);
var
  i: integer;
  CurChar: char;
begin
  bmPicture.Canvas.MoveTo(APoint);

end;

end.


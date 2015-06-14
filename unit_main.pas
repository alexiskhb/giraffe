unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, StdCtrls;

const
  DefaultLineWidth = 1;
  BoldLineWidth = 2;
  {DFFSM = Determ Figure Finite-State Machine}
  DFFSMSize = 6;

type

  { TMainForm }

  TDirection = (dNone, dRightUp, dRight, dRightDown, dDown, dLeftDown, dLeft, dLeftUp, dUp);

  TDetermFigureFSMAdjMatrix  = array [0..DFFSMSize - 1, 0..DFFSMSize - 1] of integer;

  {
  0 = start
  1 = read first char of direction
  2 = read second char of direction
  3 = reading length
  4 = draw
  5 = finish
  }
  TFSMState = (msStart, msFirstChar, msSecondChar, msNum, msDraw, msFinish);

  TDetermFigureFSM = class
  private
    FCurState: TFSMState;
    FCurNum: integer;
    FCurChars: string;
  public
    constructor Create;
  end;

  TLine = class
  private
    FDirection: TDirection;
    FSize: integer;
    FStartPoint: TPoint;
    FEndPoint: TPoint;
    FColor: TColor;
  public
    property Direction: TDirection read FDirection;
    property Size: integer read FSize;
    property StartPoint: TPoint read FStartPoint;
    property EndPoint: TPoint read FEndPoint;
    constructor Create(ABitmap: TBitmap; ADirection: TDirection;
      ASize: integer; AStartPoint: TPoint; AColor: TColor);
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

constructor TDetermFigureFSM.Create;
var
  i, j: integer;
begin
  FCurState := msStart;
  FCurNum := 0;
  FCurChars := '';
end;

constructor TLine.Create(ABitmap: TBitmap; ADirection: TDirection;
  ASize: integer; AStartPoint: TPoint; AColor: TColor);
begin
  with ABitmap do begin
    Canvas.Pen.Color := AColor;
    if ADirection = dNone then
      Canvas.Pen.Width := BoldLineWidth;


    Canvas.Pen.Width := DefaultLineWidth;
  end;
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
    Pen.Width := DefaultLineWidth;
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
  CurDirection: TDirection;
begin


end;

end.


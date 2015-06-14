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
  DFFSMSize = 5;
  ValidDirectionChars: set of char = ['r', 'R', 'd', 'D', 'l', 'L', 'u', 'U'];
  ValidIntegers: set of char = ['0'..'9'];

type

  { TMainForm }

  TDirection = (dNone, dRightUp, dRight, dRightDown, dDown, dLeftDown, dLeft, dLeftUp, dUp);

  {
  0 = start
  1 = reading first char of direction
  2 = reading second char of direction
  3 = reading length
  4 = draw
  5 = finish
  }
  TFSMState = (msStart, msFirstChar, msSecondChar, msNum, msFinish);

  TDetermFigureFSM = class
  private
    FCurState: TFSMState;
    FCurNum: integer;
    FCurChars: string;
  public
    function Analyze(ANewChar: char; var ACurLength: integer;
      var ADirection: TDirection): boolean;
    constructor Create;
  end;

  TLine = class
  private
    FDirection: TDirection;
    FLength: integer;
    FStartPoint: TPoint;
    FEndPoint: TPoint;
    FColor: TColor;
  public
    property Direction: TDirection read FDirection;
    property Size: integer read FLength;
    property StartPoint: TPoint read FStartPoint;
    property EndPoint: TPoint read FEndPoint;
    constructor Create(ABitmap: TBitmap; ADirection: TDirection;
      ALength: integer; AStartPoint: TPoint; AColor: TColor);
  end;

  TMainForm = class(TForm)
  public
    FSM: TDetermFigureFSM;
  published
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
    procedure DrawFigure(APoint: TPoint; AColor: TColor; AScale: integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function TDetermFigureFSM.Analyze(ANewChar: char; var ACurLength: integer;
  var ADirection: TDirection): boolean;
var
  NewState: TFSMState;
begin
  Result := false;
  {CurState = reading num}
  if FCurState = msNum then
    if ANewChar in ValidIntegers then begin
      FCurNum := FCurNum*10 + StrToInt(ANewChar);
      exit(false);
    end
    else begin
      FCurState := msStart;
      ACurLength := FCurNum;
      FCurNum := 0;
      exit(true);
    end;

  {CurState = reading first char}
  if FCurState = msFirstChar then begin
    case FCurChars of
        'r', 'R': ADirection := dRight;
        'd', 'D': ADirection := dDown;
        'l', 'L': ADirection := dLeft;
        'u', 'U': ADirection := dUp;
    end;
    if ANewChar in ValidDirectionChars then begin
      FCurChars := FCurChars + ANewChar;
      FCurState := msSecondChar;
      exit(false);
    end
    else
    if ANewChar in ValidIntegers then begin
      FCurNum := StrToInt(ANewChar);
      FCurState := msNum;
      FCurChars := '';
      exit(false);
    end
    else begin
      FCurState := msStart;
      FCurChars := '';
      exit(false);
    end;
  end;

  {CurState = reading second char}
  if FCurState = msSecondChar then begin
    case FCurChars of
      'rr': ADirection := dRight;
      'dd': ADirection := dDown;
      'll': ADirection := dLeft;
      'uu': ADirection := dUp;
      'ru', 'rU', 'Ru', 'RU', 'ur', 'uR', 'Ur', 'UR': ADirection := dRightUp;
      'rd', 'rD', 'Rd', 'RD', 'dr', 'dR', 'Dr', 'DR': ADirection := dRightDown;
      'lu', 'lU', 'Lu', 'LU', 'ul', 'uL', 'Ul', 'UL': ADirection := dLeftUp;
      'ld', 'lD', 'Ld', 'LD', 'dl', 'dL', 'Dl', 'DL': ADirection := dLeftDown;
      else ADirection := dNone;
    end;
    if ANewChar in ValidIntegers then begin
      FCurState := msNum;
      FCurNum := StrToInt(ANewChar);
      FCurChars := '';
      exit(false);
    end
    else begin
      FCurState := msStart;
      FCurChars := '';
      exit(false);
    end;
  end;

  {CurState = reading start}
  if FCurState = msStart then begin
    if ANewChar in ValidDirectionChars then begin
      FCurChars := ANewChar;
      FCurState := msFirstChar;
      exit(false);
    end
    else
    if ANewChar in ValidIntegers then begin
      FCurNum := StrToInt(ANewChar);
      FCurState := msNum;
      exit(false);
    end;
  end;
end;

constructor TDetermFigureFSM.Create;
var
  i, j: integer;
begin
  FCurState := msStart;
  FCurNum := 0;
  FCurChars := '';
end;

constructor TLine.Create(ABitmap: TBitmap; ADirection: TDirection;
  ALength: integer; AStartPoint: TPoint; AColor: TColor);
begin
  with ABitmap.Canvas do begin
    Pen.Color := AColor;
    if ADirection = dNone then
      Pen.Width := BoldLineWidth;





    Pen.Width := DefaultLineWidth;
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
  FSM := TDetermFigureFSM.Create;
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

procedure TMainForm.DrawFigure(APoint: TPoint; AColor: TColor; AScale: integer);
var
  i, j: integer;
  CurLength: integer;
  CurDirection: TDirection;
  CurMemoString: string;
  NewLine: TLine;
begin
  for i := 0 to memoText.Lines.Count - 1 do begin
    CurMemoString := memoText.Lines[i];
    for j := 0 to Length(CurMemoString) - 1 do begin
      if FSM.Analyze(CurMemoString[j], CurLength, CurDirection) then
        //NewLine := TLine.Create();


    end;
  end;
end;

end.


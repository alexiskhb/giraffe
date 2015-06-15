unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynMemo, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Spin, StdCtrls, Buttons, Menus, unit_history;

const
  DefaultLineWidth = 1;
  BoldLineWidth = 3;
  {DFFSM = Determ Figure Finite-State Machine}
  ValidDirectionChars: set of char = ['r', 'R', 'd', 'D', 'l', 'L', 'u', 'U', 'n', 'N'];
  ValidIntegers: set of char = ['0'..'9'];
  ArrowChars: array [0..8] of string = ('↰', '↑', '↱', '←', '', '→', '↲', '↓', '↳');
  ArrowTrueChars: array [0..8] of string = ('ul', 'u', 'ur', 'l', 'n', 'r', 'dl', 'd', 'dr');

type

  { TMainForm }

  TDirection = (dNone, dRightUp, dRight, dRightDown, dDown, dLeftDown, dLeft, dLeftUp, dUp);

  {
  0 = start
  1 = reading first char of direction
  2 = reading second char of direction
  3 = reading length
  4 = finish
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
    function AnalyzeNumFirst(ANewChar: char; var ACurLength: integer;
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
      ALength, AScale: integer; APoint: TPoint; AColor: TColor);
  end;

  TDot = class
  private
    FSize: integer;
    FPoint: TPoint;
    FColor: TColor;
  public
    property Size: integer read FSize;
    property StartPoint: TPoint read FPoint;
    constructor Create(ABitmap: TBitmap; AScale: integer;
      APoint: TPoint; AColor: TColor);
  end;

  TMainForm = class(TForm)
    btnHistory: TBitBtn;
    procedure btnHistoryClick(Sender: TObject);
  public
    FSM: TDetermFigureFSM;
    ArrowButtons: array [0..8] of TBitBtn;
  published
    memoText: TSynEdit;
    pnlArrows: TPanel;
    cbColor: TColorButton;
    pbPicture: TPaintBox;
    pnlPicture: TPanel;
    pnlText: TPanel;
    pnlColorSize: TPanel;
    spinSize: TSpinEdit;
    bmPicture: TBitmap;
    bbClear: TBitBtn;
    bbSave: TBitBtn;
    SaveDialog: TSaveDialog;
    procedure bbClearClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbPictureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPicturePaint(Sender: TObject);
    procedure DrawFigure(APoint: TPoint; AColor: TColor;
      AScale: integer; IsNumFirst: boolean);
    procedure DrawDot(APoint: TPoint; AColor: TColor; AScale: integer);
    procedure CreateArrowButtons(APanel: TPanel);
    procedure ArrowButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function TDetermFigureFSM.Analyze(ANewChar: char; var ACurLength: integer;
  var ADirection: TDirection): boolean;
begin
  Result := false;
  {CurState = reading num}
  if FCurState = msNum then
    if ANewChar in ValidIntegers then begin
      FCurNum := FCurNum*10 + StrToInt(ANewChar);
      exit(false);
    end
    else
    if ANewChar in ValidDirectionChars then begin
      FCurState := msFirstChar;
      FCurChars := ANewChar;
      ACurLength := FCurNum;
      FCurNum := 0;
      exit(true);
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
        'n', 'N': ADirection := dNone;
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
      'rr', 'RR': ADirection := dRight;
      'dd', 'DD': ADirection := dDown;
      'll', 'LL': ADirection := dLeft;
      'uu', 'UU': ADirection := dUp;
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

function TDetermFigureFSM.AnalyzeNumFirst(ANewChar: char; var ACurLength: integer;
  var ADirection: TDirection): boolean;
begin
  Result := false;
  {CurState = reading num}
  if FCurState = msNum then
    if ANewChar in ValidIntegers then begin
      FCurNum := FCurNum*10 + StrToInt(ANewChar);
      exit(false);
    end
    else
    if ANewChar in ValidDirectionChars then begin
      FCurChars := ANewChar;
      FCurState := msFirstChar;
      ACurLength := FCurNum;
      FCurNum := 0;
      exit(false);
    end
    else begin
      FCurState := msStart;
      ACurLength := FCurNum;
      FCurNum := 0;
      exit(false);
    end;

  {CurState = reading first char}
  if FCurState = msFirstChar then begin
    case FCurChars of
        'r', 'R': ADirection := dRight;
        'd', 'D': ADirection := dDown;
        'l', 'L': ADirection := dLeft;
        'u', 'U': ADirection := dUp;
        'n', 'N': ADirection := dNone;
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
      exit(true);
    end
    else begin
      FCurState := msStart;
      FCurChars := '';
      exit(true);
    end;
  end;

  {CurState = reading second char}
  if FCurState = msSecondChar then begin
    case FCurChars of
      'rr', 'RR': ADirection := dRight;
      'dd', 'DD': ADirection := dDown;
      'll', 'LL': ADirection := dLeft;
      'uu', 'UU': ADirection := dUp;
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
      exit(true);
    end
    else begin
      FCurState := msStart;
      FCurChars := '';
      exit(true);
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
begin
  FCurState := msStart;
  FCurNum := 0;
  FCurChars := '';
end;

constructor TLine.Create(ABitmap: TBitmap; ADirection: TDirection;
  ALength, AScale: integer; APoint: TPoint; AColor: TColor);
var
  DirectLength: integer;
begin
  DirectLength := AScale * ALength * 2;
  FStartPoint := APoint;
  FColor := AColor;
  case ADirection of
    dRightUp: FEndPoint := Point(APoint.X + DirectLength, APoint.Y - DirectLength);
    dRight: FEndPoint := Point(APoint.X + DirectLength, APoint.Y);
    dRightDown: FEndPoint := Point(APoint.X + DirectLength, APoint.Y + DirectLength);
    dDown: FEndPoint := Point(APoint.X, APoint.Y + DirectLength);
    dLeftDown: FEndPoint := Point(APoint.X - DirectLength, APoint.Y + DirectLength);
    dLeft: FEndPoint := Point(APoint.X - DirectLength, APoint.Y);
    dLeftUp: FEndPoint := Point(APoint.X - DirectLength, APoint.Y - DirectLength);
    dUp: FEndPoint := Point(APoint.X, APoint.Y - DirectLength);
    else FEndPoint := APoint;
  end;
  with ABitmap.Canvas do begin
    if ADirection = dNone then
      Pen.Width := BoldLineWidth;
    Pen.Color := AColor;
    MoveTo(FStartPoint);
    LineTo(FEndPoint);
    Pen.Width := DefaultLineWidth;
  end;
end;

constructor TDot.Create(ABitmap: TBitmap; AScale: integer;
  APoint: TPoint; AColor: TColor);
begin
  FPoint := APoint;
  FColor := AColor;
  with ABitmap.Canvas do begin
    Pen.Color := AColor;
    Pen.Width := AScale div 2;
    MoveTo(FPoint);
    LineTo(FPoint);
    Pen.Width := DefaultLineWidth;
  end;
end;

procedure TMainForm.btnHistoryClick(Sender: TObject);
begin
  HistoryForm.Show;
end;

procedure TMainForm.bbClearClick(Sender: TObject);
begin
  bmPicture.Canvas.FillRect(0, 0, Width, Height);
  pbPicture.Invalidate;
end;

procedure TMainForm.bbSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    bmPicture.SaveToFile(SaveDialog.FileName + '.bmp');
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  bmPicture.Width := MainForm.Width;
  bmPicture.Height := MainForm.Height;
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
  bmPicture.Canvas.FillRect(0, 0, Width, Height);
  FSM := TDetermFigureFSM.Create;
  CreateArrowButtons(pnlArrows);
end;

procedure TMainForm.pbPictureMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MemoString: string;
  i, j: integer;
  IsNumFirst: boolean;
begin
  for i := 0 to memoText.Lines.Count - 1 do begin
    MemoString := memoText.Lines[i];
    if (Length(MemoString) > 0) and (MemoString[Length(MemoString) - 1] <> ' ') then
      memoText.Lines[i] := MemoString + '  ';
  end;

  i := 0;
  j := 0;
  MemoString := '#';
  while (j < Length(MemoString)) and not ((MemoString[j] in ValidDirectionChars) or (MemoString[j] in ValidIntegers)) do begin
    MemoString := memoText.Lines[i];
    while (j < Length(MemoString)) and not ((MemoString[j] in ValidDirectionChars) or (MemoString[j] in ValidIntegers)) do begin
      inc(j);
      if j >= Length(MemoString) then break;
    end;
    inc(i);
    if (i = memoText.Lines.Count) and (j = Length(MemoString)) then break;
  end;

  IsNumFirst := (j < Length(MemoString)) and (MemoString[j] in ValidIntegers);
  if Button = mbLeft then begin
    DrawFigure(Point(X, Y), cbColor.ButtonColor, spinSize.Value, IsNumFirst);
    with HistoryForm.memoHistory do begin
      Text := Text + memoText.Text;
      Lines.Append('');
    end;
  end
  else begin
    DrawDot(Point(X, Y), cbColor.ButtonColor, spinSize.Value);
  end;
end;

procedure TMainForm.pbPicturePaint(Sender: TObject);
begin
  with bmPicture do
    pbPicture.Canvas.CopyRect(Rect(0, 0, Width, Height), bmPicture.Canvas, Rect(0, 0, Width, Height));
end;

procedure TMainForm.DrawFigure(APoint: TPoint; AColor: TColor;
  AScale: integer; IsNumFirst: boolean);
var
  i, j: integer;
  CurLength: integer;
  CurDirection: TDirection;
  CurPoint: TPoint;
  CurMemoString: string;
  NewLine: TLine;
begin
  CurPoint := APoint;
  CurDirection := dNone;
  CurLength := 0;
  NewLine := nil;
  for i := 0 to memoText.Lines.Count - 1 do begin
    CurMemoString := memoText.Lines[i];
    for j := 0 to Length(CurMemoString) - 1 do begin
      if IsNumFirst then begin
        if FSM.AnalyzeNumFirst(CurMemoString[j], CurLength, CurDirection) then begin
          NewLine := TLine.Create(bmPicture, CurDirection, CurLength, AScale, CurPoint, AColor);
          CurPoint := NewLine.EndPoint;
          FreeAndNil(NewLine);
        end;
      end
      else
        if FSM.Analyze(CurMemoString[j], CurLength, CurDirection) then begin
          NewLine := TLine.Create(bmPicture, CurDirection, CurLength, AScale, CurPoint, AColor);
          CurPoint := NewLine.EndPoint;
          FreeAndNil(NewLine);
        end;
    end;
  end;
  with bmPicture do
    pbPicture.Canvas.CopyRect(Rect(0, 0, Width, Height), bmPicture.Canvas, Rect(0, 0, Width, Height));
end;

procedure TMainForm.DrawDot(APoint: TPoint; AColor: TColor; AScale: integer);
var
  Dot: TDot;
begin
  Dot := TDot.Create(bmPicture, AScale, APoint, AColor);
  pbPicture.Invalidate;
  FreeAndNil(Dot);
end;

procedure TMainForm.CreateArrowButtons(APanel: TPanel);
var
  i, j, k: integer;
  DefaultButtonHeight: integer;
begin
  DefaultButtonHeight := APanel.Height div 3;
  for i := 0 to 2 do
    for j := 0 to 2 do begin
      k := i*3 + j;
      ArrowButtons[k] := TBitBtn.Create(APanel);
      with ArrowButtons[k] do begin
        Parent := APanel;
        Height := DefaultButtonHeight;
        Width := Height;
        Top := Height * i;
        Left := Width * j;
        Caption := ArrowChars[k];
        Hint := ArrowTrueChars[k];
        ShowHint := true;
        Font.Size := 12;
        OnClick := @ArrowButtonClick;
      end;
    end;
end;

procedure TMainForm.ArrowButtonClick(Sender: TObject);
var
  Button: TBitBtn;
  i, j: integer;
  MemoString: string;
  Direction: string;
begin
  Button := Sender as TBitBtn;
  Direction := Button.Hint;
  i := memoText.CaretY - 1;
  j := memoText.CaretX - 1;
  MemoString := memoText.Lines[i];
  MemoString := Copy(MemoString, 1, j) + Direction + Copy(MemoString, j + 1, Length(MemoString));
  memoText.Lines[i] := MemoString;
  memoText.CaretXY := Point(j + Length(Direction) + 1, i + 1);
  ActiveControl := memoText;
end;

end.


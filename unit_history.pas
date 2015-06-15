unit unit_history;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics,
  Dialogs;

type

  { THistoryForm }

  THistoryForm = class(TForm)
    memoHistory: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  HistoryForm: THistoryForm;

implementation

{$R *.lfm}

{ THistoryForm }

procedure THistoryForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

end.


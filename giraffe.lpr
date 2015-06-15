program giraffe;

{$mode objfpc}{$H+}

uses
          {$IFDEF UNIX}{$IFDEF UseCThreads}
          cthreads,
          {$ENDIF}{$ENDIF}
          Interfaces, // this includes the LCL widgetset
          Forms, unit_main, unit_history
          { you can add units after this };

{$R *.res}

begin
          RequireDerivedFormResource := True;
          Application.Initialize;
	  Application.CreateForm(TMainForm, MainForm);
	  Application.CreateForm(THistoryForm, HistoryForm);
          Application.Run;
end.


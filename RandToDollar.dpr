program RandToDollar;

{$APPTYPE GUI}

uses
  Forms,
  Unit1 in 'Unit1.pas' {TForm1, Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


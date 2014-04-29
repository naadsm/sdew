program SdewDemo;

uses
  Forms,

  Sdew,
  XMLReader,

  Demo in 'Demo.pas' {Form1},
  xmlHerd in 'xmlHerd.pas',
  Loc in 'Loc.pas',
  DiseaseModelStatMethods in 'DiseaseModelStatMethods.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

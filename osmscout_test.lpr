program osmscout_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, OsMapPainter, OsMapTypes, OsMapStyles, OsMapGeometry,
  OsMapObjects, OsMapParameters, OsMapObjTypes, OsMapTags, OsMapFiles,
  OsMapUtils, OsMapObjFeatures, OsMapStyleConfig, OsMapLabels, OsMapTransform,
  OsMapPainterAgg, OsMapProjection, OsMapManager, OsMapFormatMp;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


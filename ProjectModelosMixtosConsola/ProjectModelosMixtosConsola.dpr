{**
@abstract(Modulo principal de la herramienta de ajuste de modelos mixtos con
interfaz de texto.)
@author(Agustin Barto <abarto@gmail.com>)
@created(July 1, 2005)
@lastmod(July 6, 2005)
Este es el modulo principal de la herramienta de ajuste de modelos mixtos con
interfaz de texto. Su tarea es instanciar el objeto de aplicacion y ejecutar
las tareas requeridas por el usuario.
}

program ProjectModelosMixtosConsola;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  UnitAplicacionModelosMixtos in 'UnitAplicacionModelosMixtos.pas',
  UnitArregloEnteros in 'UnitArregloEnteros.pas',
  UnitArregloStrings in 'UnitArregloStrings.pas',
  UnitSubrutinasEstadisticas in 'UnitSubrutinasEstadisticas.pas',
  UnitColumnaDatos in 'UnitColumnaDatos.pas',
  UnitEfecto in 'UnitEfecto.pas',
  UnitEstructura in 'UnitEstructura.pas',
  UnitGrupoEfectos in 'UnitGrupoEfectos.pas',
  UnitInformacionTiempoEjecucionProcedimiento in 'UnitInformacionTiempoEjecucionProcedimiento.pas',
  UnitMatrizValores in 'UnitMatrizValores.pas',
  UnitOpcion in 'UnitOpcion.pas',
  UnitParametros in 'UnitParametros.pas',
  UnitProcedimiento in 'UnitProcedimiento.pas',
  UnitSubrutinasAlgebraicas in 'UnitSubrutinasAlgebraicas.pas',
  UnitConfiguracionProcedimiento in 'UnitConfiguracionProcedimiento.pas',
  UnitConfiguracionDatos in 'UnitConfiguracionDatos.pas',
  UnitConfiguracionModelo in 'UnitConfiguracionModelo.pas',
  UnitConfiguracion in 'UnitConfiguracion.pas',
  UaVector in 'UtilidadesAlgebraicas\UaVector.pas',
  UaComun in 'UtilidadesAlgebraicas\UaComun.pas',
  UaConstantes in 'UtilidadesAlgebraicas\UaConstantes.pas',
  UaGeneradorNumerosAleatorios in 'UtilidadesAlgebraicas\UaGeneradorNumerosAleatorios.pas',
  UaMatriz in 'UtilidadesAlgebraicas\UaMatriz.pas',
  UaMiscelanea in 'UtilidadesAlgebraicas\UaMiscelanea.pas',
  UnitParserConfiguracion in 'ParserConfiguracion\UnitParserConfiguracion.pas';

var
  I: Integer;
  Parametros: TStrings;
  AplicacionModelosMixtos: TAplicacionModelosMixtos;

begin
  Parametros := TStringList.Create;
  for I := 1 to ParamCount do
    Parametros.Add(ParamStr(I));

  try
    AplicacionModelosMixtos := TAplicacionModelosMixtos.Create(Parametros);
    AplicacionModelosMixtos.Ejecutar;
  except
    on E:Exception do
    begin
      WriteLn('ProjectModelosMixtosConsola: Se produjo un error irrecuperable: ' + E.Message);
      WriteLn('ProjectModelosMixtosConsola: Terminado.');
    end;
  end;

  FreeAndNil(AplicacionModelosMixtos);
  FreeAndNil(Parametros);
end.

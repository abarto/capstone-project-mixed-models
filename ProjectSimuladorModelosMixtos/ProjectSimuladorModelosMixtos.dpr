program ProjectSimuladorModelosMixtos;uses  Forms,
  UnitFormPrincipal in 'UnitFormPrincipal.pas' {FormPrincipal},
  UaVector in 'UtilidadesAlgebraicas\UaVector.pas',
  UaMatriz in 'UtilidadesAlgebraicas\UaMatriz.pas',
  UaComun in 'UtilidadesAlgebraicas\UaComun.pas',
  UaMiscelanea in 'UtilidadesAlgebraicas\UaMiscelanea.pas',
  UaConstantes in 'UtilidadesAlgebraicas\UaConstantes.pas',
  UnitColumnaDatos in 'UnitColumnaDatos.pas',
  UnitArregloEnteros in 'UnitArregloEnteros.pas',
  UnitArregloStrings in 'UnitArregloStrings.pas',
  UnitCalculoSimbolico in 'UnitCalculoSimbolico.pas',
  UnitConfiguracionDatos in 'UnitConfiguracionDatos.pas',
  UnitConfiguracionModelo in 'UnitConfiguracionModelo.pas',
  UnitEfecto in 'UnitEfecto.pas',
  UnitEstructura in 'UnitEstructura.pas',
  UnitGrupoEfectos in 'UnitGrupoEfectos.pas',
  UnitOpcion in 'UnitOpcion.pas',
  UnitParametros in 'UnitParametros.pas',
  UaGeneradorNumerosAleatorios in 'UtilidadesAlgebraicas\UaGeneradorNumerosAleatorios.pas',
  UnitParserConfiguracion in 'ParserConfiguracion\UnitParserConfiguracion.pas',
  UnitConfiguracion in 'UnitConfiguracion.pas',
  UnitProcedimiento in 'UnitProcedimiento.pas',
  UnitMatrizValores in 'UnitMatrizValores.pas',
  UnitConstantesProcedimiento in 'UnitConstantesProcedimiento.pas',
  UnitSubrutinasAlgebraicas in 'UnitSubrutinasAlgebraicas.pas',
  UnitInformacionTiempoEjecucionProcedimiento in 'UnitInformacionTiempoEjecucionProcedimiento.pas',
  UnitConfiguracionProcedimiento in 'UnitConfiguracionProcedimiento.pas';

{$R *.res}begin  Application.Initialize;  Application.CreateForm(TFormPrincipal, FormPrincipal);  Application.Run;end.
program ProjectModelosMixtos;uses  Forms,
  UnitFormPrincipal in 'UnitFormPrincipal.pas' {FormPrincipal},
  UnitOpcion in 'UnitOpcion.pas',
  UnitColumnaDatos in 'UnitColumnaDatos.pas',
  UnitConfiguracion in 'UnitConfiguracion.pas',
  UnitEfecto in 'UnitEfecto.pas',
  UnitEstructura in 'UnitEstructura.pas',
  UnitGrupoEfectos in 'UnitGrupoEfectos.pas',
  UnitMatrizValores in 'UnitMatrizValores.pas',
  UnitFormTabla in 'UnitFormTabla.pas' {FormTabla},
  UnitProcedimiento in 'UnitProcedimiento.pas',
  UnitFormBitacoraDepuracion in 'UnitFormBitacoraDepuracion.pas' {FormBitacoraDepuracion},
  UnitInformacionTiempoEjecucionProcedimiento in 'UnitInformacionTiempoEjecucionProcedimiento.pas',
  UnitConfiguracionProcedimiento in 'UnitConfiguracionProcedimiento.pas',
  UnitConfiguracionModelo in 'UnitConfiguracionModelo.pas',
  UnitConfiguracionDatos in 'UnitConfiguracionDatos.pas',
  UnitArregloStrings in 'UnitArregloStrings.pas',
  UnitArregloEnteros in 'UnitArregloEnteros.pas',
  UnitParametros in 'UnitParametros.pas',
  UnitSubrutinasAlgebraicas in 'UnitSubrutinasAlgebraicas.pas',
  UnitThreadProcedimiento in 'UnitThreadProcedimiento.pas',
  UaVector in 'UtilidadesAlgebraicas\UaVector.pas',
  UaMatriz in 'UtilidadesAlgebraicas\UaMatriz.pas',
  UaComun in 'UtilidadesAlgebraicas\UaComun.pas',
  UaMiscelanea in 'UtilidadesAlgebraicas\UaMiscelanea.pas',
  UaConstantes in 'UtilidadesAlgebraicas\UaConstantes.pas',
  UnitConstantesProcedimiento in 'UnitConstantesProcedimiento.pas',
  UaGeneradorNumerosAleatorios in 'UtilidadesAlgebraicas\UaGeneradorNumerosAleatorios.pas',
  UnitSubrutinasEstadisticas in 'UnitSubrutinasEstadisticas.pas';

{$R *.res}begin  Application.Initialize;  Application.CreateForm(TFormPrincipal, FormPrincipal);  Application.CreateForm(TFormBitacoraDepuracion, FormBitacoraDepuracion);
  Application.Run;end.
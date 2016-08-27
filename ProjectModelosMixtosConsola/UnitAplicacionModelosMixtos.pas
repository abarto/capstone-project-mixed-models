{**
@abstract(Objeto aplicacion de herramienta de ajuste en consola de texto.)
@author(Agustin Barto <abarto@gmail.com>)
@created(July 1, 2005)
@lastmod(July 6, 2005)
Este modulo contiene el objeto aplicacion de la herramienta de ajuste de
modelos mixtos que puede ejecutarse desde una consola de texto.
}

unit UnitAplicacionModelosMixtos;

interface

uses
  Classes, SysUtils,
  UnitArregloStrings, UnitConfiguracion, UnitEfecto, UnitOpcion,
  UnitProcedimiento;

type
  {** Aplicacion de la herramienta de ajuste de modelos mixtos que puede
      ejecutarse desde una consola de texto. El objetivo de esta clase es
      facilitar la tarea de ejecutar m�ltiples casos de ajuste en una sola
      sesi�n.
      @abstract(Aplicacion de la herramienta de ajuste de modelos mixtos que
      que se ejecuta desde una consola de texto.) }
  TAplicacionModelosMixtos = class
  private
    {** Par�metros de linea de comandos. }
    FParametros: TStrings;
    {** Nombres de los archivos de configuracion a procesar. }
    FNombresArchivosConfiguracion: TStrings;
  public
    {** Constructor.
        @param(AParametros Parametros de la linea de comandos.) }
    constructor Create(const AParametros: TStrings); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Metodo para iniciar la ejecucion del procedimiento de ajuste. }
    procedure Ejecutar;
  end { AplicacionModelosMixtos };

implementation

constructor TAplicacionModelosMixtos.Create(const AParametros: TStrings);
begin { TAplicacionModelosMixtos.Create }
  Assert(Assigned(AParametros), 'TAplicacionModelosMixtos.Create: Assigned(Parametros)');

  FParametros := AParametros;

  // Verificar parametros
  if FParametros.Count = 0 then
    // No se pasaron parametros a la aplicacion
    raise Exception.Create('Numero incorrecto de parametros (0).')
  else
  begin
    if FParametros.Strings [0] = '-l' then
    begin
      if FParametros.Count < 2 then
      begin
        raise Exception.Create('No se suministro el nombre del archivo los la lista de nombres de archivos de configuracion.')
      end
      else
      begin
        FNombresArchivosConfiguracion := TStringList.Create;
        FNombresArchivosConfiguracion.LoadFromFile(FParametros.Strings [1]);
      end;
    end
    else
    begin
      FNombresArchivosConfiguracion := TStringList.Create;
      FNombresArchivosConfiguracion.Add(FParametros.Strings [0]);
    end;
  end;
end { TAplicacionModelosMixtos.Create };

destructor TAplicacionModelosMixtos.Destroy;
begin { TAplicacionModelosMixtos.Destroy }
  try
    FreeAndNil(FNombresArchivosConfiguracion);
  finally
    inherited Destroy;
  end;
end { TAplicacionModelosMixtos.Destroy };

procedure TAplicacionModelosMixtos.Ejecutar;
var
  I, IndiceOpcionSalida, IndiceVariable, IndiceGrupo, IndiceEfecto, IndiceNivel, IndiceEstimador, IndiceParametro, IndiceArchivoConfiguracion: Integer;
  AnchoCampo1, AnchoCampo2, AnchoCampo3, AnchoCampo4, CantidadVariablesClasificacion, CantidadColumnasZ: Integer;
  NombreArchivoConfiguracion, NombreArchivoSalida: TFileName;
  MostrarSalidaConsola, TieneArchivoSalida, GenerarSalidaCompacta, TieneUnidadExperimental: Boolean;
  Procedimiento: TProcedimiento;
  Configuracion: TConfiguracion;
  OpcionesSalida, OpcionesSalidaPorDefecto: TArregloStrings;
  BitacoraInicializacionModelo, BitacoraInicializacion, BitacoraEjecucion, BitacoraIteracion, BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento, BitacoraPostprocesamiento: TStrings;
  LineasSalida: TStrings;
  InicioInicializacion, DuracionInicializacion, InicioIteracion, DuracionIteracion: TDateTime;
  Hora, Minuto, Segundo, MiliSegundo: Word;
  String1, String2: String;
  NombreArchivoResumenVerosimilitud, NombreArchivoResumenBeta, NombreArchivoResumenTheta, NombreArchivoResumenIteracion: TFileName;
  ArchivoSalida, ArchivoResumenVerosimilitud, ArchivoResumenBeta, ArchivoResumenTheta, ArchivoResumenIteracion: TextFile;
  EscribirResumenVerosimilitud, EscribirResumenBeta, EscribirResumenTheta, EscribirResumenIteracion: Boolean;
begin { TAplicacionModelosMixtos.Ejecutar }
  Procedimiento := nil;
  Configuracion := nil;

  BitacoraInicializacionModelo := TStringList.Create;
  BitacoraInicializacion  := TStringList.Create;
  BitacoraIteracion := TStringList.Create;
  BitacoraEjecucion := TStringList.Create;
  BitacoraValidacionDatos := TStringList.Create;
  BitacoraValidacionModelo := TStringList.Create;
  BitacoraValidacionProcedimiento := TStringList.Create;
  BitacoraPostprocesamiento := TStringList.Create;
  LineasSalida := TStringList.Create;

  OpcionesSalidaPorDefecto := TArregloStrings.Create(7);
  OpcionesSalidaPorDefecto [0] := 'informacion_modelo';
  OpcionesSalidaPorDefecto [1] := 'dimensiones';
  OpcionesSalidaPorDefecto [2] := 'estructura_media_solucion';
  OpcionesSalidaPorDefecto [3] := 'efectos_aleatorios_solucion';
  OpcionesSalidaPorDefecto [4] := 'efectos_aleatorios_estimacion';
  OpcionesSalidaPorDefecto [5] := 'informacion_ajuste';
  OpcionesSalidaPorDefecto [6] := 'estadisticas_iteracion';

  for IndiceArchivoConfiguracion := 0 to FNombresArchivosConfiguracion.Count - 1 do
  begin
    NombreArchivoConfiguracion := FNombresArchivosConfiguracion.Strings [IndiceArchivoConfiguracion];
    Configuracion := TConfiguracion.Create(NombreArchivoConfiguracion);

    BitacoraInicializacionModelo.Clear;
    BitacoraInicializacion.Clear;
    BitacoraIteracion.Clear;
    BitacoraEjecucion.Clear;
    BitacoraValidacionDatos.Clear;
    BitacoraValidacionModelo.Clear;
    BitacoraValidacionProcedimiento.Clear;
    BitacoraPostprocesamiento.Clear;
    LineasSalida.Clear;

    // Parsear configuracion
    if Configuracion.Parsear then
    begin
      // Parseo exitoso

      // Inicializar modelo
      if Configuracion.Modelo.Inicializar(BitacoraInicializacion) then
      begin
        // Inicalizacion exitosa

        // Validar la configuracion
        if Configuracion.Validar(BitacoraValidacionDatos, BitacoraValidacionModelo, BitacoraValidacionProcedimiento) then
        begin
          // Validacion exitosa

          // Instanciar procedimiento
          Procedimiento := TProcedimiento.Create(Configuracion.Procedimiento);

          // Inicializar opciones de salida

          if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_salida') then
          begin
            NombreArchivoSalida := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
            TieneArchivoSalida := True;
            AssignFile(ArchivoSalida, NombreArchivoSalida);
            ReWrite(ArchivoSalida);
          end
          else
          begin
            NombreArchivoSalida := '';
            TieneArchivoSalida := False;
          end;

          if Procedimiento.Opciones.BuscarAsignar(I, 'mostrar_salida_consola') then
            MostrarSalidaConsola := (Procedimiento.Opciones [I] as TOpcionParametroBoolean).Parametro
          else
            MostrarSalidaConsola := False;

          if Procedimiento.Opciones.BuscarAsignar(I, 'generar_salida_compacta') then
            GenerarSalidaCompacta := (Procedimiento.Opciones [I] as TOpcionParametroBoolean).Parametro
          else
            GenerarSalidaCompacta := True;

          // Obtener o construir arreglo de componentes de salida
          if Procedimiento.Opciones.BuscarAsignar(I, 'salida') then
            OpcionesSalida := (Procedimiento.Opciones [I] as TOpcionParametroTObject).Parametro as TArregloStrings
          else
            OpcionesSalida := OpcionesSalidaPorDefecto;

          // Inicializar opciones de salida resumida
          if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_verosimilitud') then
          begin
            NombreArchivoResumenVerosimilitud := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
            EscribirResumenVerosimilitud := True;
            AssignFile(ArchivoResumenVerosimilitud, NombreArchivoResumenVerosimilitud);
            if FileExists(NombreArchivoResumenVerosimilitud) then
              Append(ArchivoResumenVerosimilitud)
            else
              ReWrite(ArchivoResumenVerosimilitud);
          end
          else
          begin
            NombreArchivoResumenVerosimilitud := '';
            EscribirResumenVerosimilitud := False;
          end;

          if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_beta') then
          begin
            NombreArchivoResumenBeta := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
            EscribirResumenBeta := True;
            AssignFile(ArchivoResumenBeta, NombreArchivoResumenBeta);
            if FileExists(NombreArchivoResumenBeta) then
              Append(ArchivoResumenBeta)
            else
              ReWrite(ArchivoResumenBeta);
          end
          else
          begin
            NombreArchivoResumenBeta := '';
            EscribirResumenBeta := False;
          end;

          if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_theta') then
          begin
            NombreArchivoResumenTheta := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
            EscribirResumenTheta := True;
            AssignFile(ArchivoResumenTheta, NombreArchivoResumenTheta);
            if FileExists(NombreArchivoResumenTheta) then
              Append(ArchivoResumenTheta)
            else
              ReWrite(ArchivoResumenTheta);
          end
          else
          begin
            NombreArchivoResumenTheta := '';
            EscribirResumenTheta := False;
          end;

          if Procedimiento.Opciones.BuscarAsignar(I, 'archivo_resumen_iteracion') then
          begin
            NombreArchivoResumenIteracion := (Procedimiento.Opciones [I] as TOpcionParametroString).Parametro;
            EscribirResumenIteracion := True;
            AssignFile(ArchivoResumenIteracion, NombreArchivoResumenIteracion);
            if FileExists(NombreArchivoResumenIteracion) then
              Append(ArchivoResumenIteracion)
            else
              ReWrite(ArchivoResumenIteracion);
          end
          else
          begin
            NombreArchivoResumenIteracion := '';
            EscribirResumenIteracion := False;
          end;

          // Inicializar procedimiento

          InicioInicializacion := Now;
          if Procedimiento.Inicializar(BitacoraInicializacion) then
          begin
            // Inicializacion de procedimiento exitosa

            DuracionInicializacion := Now - InicioInicializacion;

            // Ejecutar

            InicioIteracion := Now;
            if Procedimiento.Ejecutar(BitacoraEjecucion, BitacoraIteracion) then
            begin
              // Ejecucion exitosa

              DuracionIteracion := Now - InicioIteracion;

              // Post-procesar resultados
              if Procedimiento.PostProcesar(BitacoraPostprocesamiento) then
              begin
                // Post-procesamiento exitoso

                WriteLn('"' + NombreArchivoConfiguracion + '" -> Ok.');

                // Generar salida

                if EscribirResumenVerosimilitud then
                begin
                  String1 := FloatToStr(-2.0 * Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica) + ' ' + FloatToStr(-2.0 * Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual);
                  WriteLn(ArchivoResumenVerosimilitud, String1);
                end;

                if EscribirResumenBeta then
                begin
                  String1 := '';
                  for I := 1 to Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos.Dimension do
                    String1 := String1 + FloatToStr(Procedimiento.InformacionPostProcesamientoEstructuraMedia.XBeta [I]) + ' ';
                  WriteLn(ArchivoResumenBeta, String1);
                end;

                if EscribirResumenTheta then
                begin
                  String1 := '';
                  for I := 1 to Procedimiento.ParametrosEfectosAleatorios.Dimension do
                    String1 := String1 + FloatToStr(Procedimiento.ParametrosEfectosAleatorios [I]) + ' ';
                  WriteLn(ArchivoResumenTheta, String1);
                end;

                if EscribirResumenIteracion then
                begin
                  DecodeTime(DuracionInicializacion, Hora, Minuto, Segundo, MiliSegundo);
                  String1 := IntToStr(Procedimiento.CantidadIteraciones) + ' ' + IntToStr(Procedimiento.CantidadIteracionesBucleInterno) + ' ' + IntToStr(Procedimiento.CantidadEvaluacionesFuncionObjetivo) + ' ' + IntToStr(Hora * 60 * 60 + Minuto * 60 + Segundo);
                  DecodeTime(DuracionIteracion, Hora, Minuto, Segundo, MiliSegundo);
                  String1 := String1 + ' ' + IntToStr(Hora * 60 * 60 + Minuto * 60 + Segundo);
                  WriteLn(ArchivoResumenIteracion, String1);
                end;

                if GenerarSalidaCompacta then
                begin
                  // Generar salida compacta

                  String1 := 'beta = [';
                  for I := 1 to Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos.Dimension do
                    String1 := String1 + ' ' + FloatToStr(Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [I]);
                  String1 := String1 + ' ]';                    

                  LineasSalida.Add(String1);

                  String1 := 'theta = [';
                  for I := 1 to Procedimiento.ParametrosEfectosAleatorios.Dimension do
                    String1 := String1 + ' ' + FloatToStr(Procedimiento.ParametrosEfectosAleatorios [I]);
                  String1 := String1 + ' ]';

                  LineasSalida.Add(String1);

                  LineasSalida.Add('l = ' + FloatToStr(-2.0 * Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica));
                  LineasSalida.Add('l_r = ' + FloatToStr(-2.0 * Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual));

                  LineasSalida.Add('cantidad_iteraciones = ' + IntToStr(Procedimiento.CantidadIteraciones));
                  LineasSalida.Add('cantidad_iteraciones_bucle_interno = ' + IntToStr(Procedimiento.CantidadIteracionesBucleInterno));
                  LineasSalida.Add('cantidad_evaluaciones_funcion_objetivo = ' + IntToStr(Procedimiento.CantidadEvaluacionesFuncionObjetivo));

                  DecodeTime(DuracionInicializacion, Hora, Minuto, Segundo, MiliSegundo);
                  LineasSalida.Add('duracion_inicializacion = ' + IntToStr(Hora*60*60 + Minuto*60 + Segundo));

                  DecodeTime(DuracionIteracion, Hora, Minuto, Segundo, MiliSegundo);
                  LineasSalida.Add('duracion_iteracion = ' + IntToStr(Hora*60*60 + Minuto*60 + Segundo));
                end
                else
                begin
                  // Generar salida tradicional

                  for IndiceOpcionSalida := OpcionesSalida.Bajo to OpcionesSalida.Alto do
                  begin
                    if OpcionesSalida [IndiceOpcionSalida] = 'informacion_modelo' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Informacion de modelo:');
                      LineasSalida.Add('');
                      LineasSalida.Add(Format('%-22s %s', ['Modelo:', Procedimiento.Modelo.Etiqueta]));
                      LineasSalida.Add(Format('%-22s %s', ['Conjunto de Datos:', Procedimiento.Modelo.Datos.Etiqueta]));
                      LineasSalida.Add(Format('%-22s %s', ['Variable Dependiente:', Procedimiento.Modelo.VariableDependiente]));

                      if Procedimiento.Modelo.VariablesClasificacion.Count > 0 then
                      begin
                        AnchoCampo1 := 0;
                        for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
                        begin
                          if AnchoCampo1 < Length(Procedimiento.Modelo.VariablesClasificacion [IndiceVariable]) then
                            AnchoCampo1 := Length(Procedimiento.Modelo.VariablesClasificacion [IndiceVariable]);
                        end;

                        LineasSalida.Add('');
                        LineasSalida.Add('Variables de Clasificacion:');
                        LineasSalida.Add('');
                        LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10s %s', ['Clase', 'Niveles', 'Valores']));
                        LineasSalida.Add('------------------------------------------------------------');

                        for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10d %s', [Procedimiento.Modelo.VariablesClasificacion [IndiceVariable], Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [IndiceVariable].Dimension, Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [IndiceVariable].Texto]));
                      end;

                      if Procedimiento.VariablesClasificacionAuxiliares.Count > 0 then
                      begin
                        LineasSalida.Add('');
                        LineasSalida.Add('Variables de Clasificacion Auxiliares:');
                        LineasSalida.Add('');

                        AnchoCampo1 := 0;
                        for IndiceVariable := 0 to Procedimiento.VariablesClasificacionAuxiliares.Count - 1 do
                        begin
                          if AnchoCampo1 < Length(Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable]) then
                            AnchoCampo1 := Length(Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable]);
                        end;

                        CantidadVariablesClasificacion := Procedimiento.Modelo.VariablesClasificacion.Count;

                        LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10s %s', ['Clase', 'Niveles', 'Valores']));
                        LineasSalida.Add('------------------------------------------------------------');

                        for IndiceVariable := 0 to Procedimiento.Modelo.VariablesClasificacion.Count - 1 do
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-10d %s', [Procedimiento.VariablesClasificacionAuxiliares [IndiceVariable], Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [CantidadVariablesClasificacion + IndiceVariable].Dimension, Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [CantidadVariablesClasificacion + IndiceVariable].Texto]));
                      end;
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'dimensiones' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Dimensiones:');
                      LineasSalida.Add('');
                      LineasSalida.Add(Format('%-30s %4d', ['Parametros de Covarianza:', Procedimiento.CantidadParametrosCovarianza]));
                      LineasSalida.Add(Format('%-30s %4d', ['Cantidad Columnas X:', Procedimiento.X.CantidadColumnas]));
                      if Procedimiento.Modelo.TieneEfectosAleatorios or Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
                        CantidadColumnasZ := Procedimiento.Z.CantidadColumnas
                      else
                        CantidadColumnasZ := 0;
                      LineasSalida.Add(Format('%-30s %4d', ['Cantidad Columnas Z:', CantidadColumnasZ]));
                      LineasSalida.Add(Format('%-30s %4d', ['Cantidad Observaciones:', Procedimiento.CantidadObservaciones]));

                      if (Procedimiento.Modelo.TieneEfectosAleatorios or Procedimiento.Modelo.TieneEfectosAleatoriosGrupos) then
                      begin
                        LineasSalida.Add('Dimensiones por grupo de efectos aleatorios:');

                        for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                        begin
                          LineasSalida.Add(Format('%-s %d:', ['  Dimensiones Grupo ', IndiceGrupo]));
                          LineasSalida.Add(Format('%-45s %4d', ['    Cantidad Parametros:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Dimension]));
                          LineasSalida.Add(Format('%-45s %4d', ['    Cant. Columnas de Z del Grupo:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].CantidadColumnasMatrizDiseno]));
                          LineasSalida.Add(Format('%-45s', ['    Cant. Columnas de Z por Efecto:']));

                          for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
                          begin
                            if not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto) then
                              LineasSalida.Add(Format('%-s: %d (%d columnas nulas)', ['      ' + Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes]));
                          end;

                          if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                          begin
                            LineasSalida.Add(Format('%-50s %4d', ['    Unid. Experimentales:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles - Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadNivelesInexistentes]));
                            LineasSalida.Add(Format('%-50s %4d', ['    Cant. Max. Observ. por Unid. Experimental:', Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones]));
                          end;
                        end;
                      end;

                      if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
                      begin
                        LineasSalida.Add('Dimensiones del error:');
                        LineasSalida.Add(Format('%-45s %4d', ['  Unid. Experimentales:', Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles - Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNivelesInexistentes]));
                        LineasSalida.Add(Format('%-45s %4d', ['  Cant. Max. Observ. por Unid. Experimental:', Procedimiento.InformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones]));
                      end;
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'estructura_media_solucion' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Solucion para la estructura de media:');
                      LineasSalida.Add('');

                      // Calcular la longitud maxima del nombre de un efecto
                      AnchoCampo1 := 0;
                      for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
                      begin
                        if  AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto) then
                           AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto);
                      end;

                      // Verificar si el nombre de efecto mas largo es menor que "efecto".
                      if AnchoCampo1 < 6 then
                        AnchoCampo1 := 6;

                      // Calcular la longitud maxima del identificador de un nivel, y buscar
                      // la cantidad maxima de variables de clasificacion en un efecto fijo.
                      AnchoCampo2 := 0;
                      CantidadVariablesClasificacion := 0;
                      for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
                      begin
                        if (not (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                           (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
                        begin
                          if CantidadVariablesClasificacion < Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension then
                            CantidadVariablesClasificacion := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension;

                          for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Alto do
                          begin
                            for IndiceNivel := Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Bajo to Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Alto do
                            begin
                              if AnchoCampo2 < Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]) then
                                AnchoCampo2 := Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]);
                            end;
                          end;
                        end;
                      end;

                      // Verificar si la lista mas larga de niveles de las variables de
                      // clasificacion es menor que "Niveles".
                      if AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1) < 8 then
                        AnchoCampo3 := 8
                      else
                        AnchoCampo3 := AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1);

                      LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) + 's %-15s %-15s %-15s', ['Efecto', 'Niveles', 'Estimador', 'Error Estandar', 'Valor-t']));
                      LineasSalida.Add('----------------------------------------------------------------------------------------------------');
                      LineasSalida.Add('');

                      IndiceEstimador := 1;
                      for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
                      begin
                        String2 := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto;

                        if (not (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                           (Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
                        begin

                          for IndiceNivel := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
                          begin
                            String1 := '';

                            for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Bajo to Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto - 1 do
                              String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's ', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable], Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, IndiceVariable]]]);
                            String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto], Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, Procedimiento.InformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto]]]);

                            if Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador] = 0.0 then
                              LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15s %-15s %-15s', [String2, String1, '0.0', '-', '-']))
                            else
                              LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f %-15.4f', [String2, String1, Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos [IndiceEstimador]]));

                            Inc(IndiceEstimador);
                          end;
                        end
                        else
                        begin
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f %-15.4f', [String2, '', Procedimiento.InformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos [IndiceEstimador]]));
                          Inc(IndiceEstimador);
                        end;
                      end;
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'efectos_aleatorios_estimacion' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Estimadores de parametros de covarianza:');
                      LineasSalida.Add('');

                      // Calcular la longitudes maximas de nombres
                      AnchoCampo1 := 0;
                      AnchoCampo2 := 0;
                      TieneUnidadExperimental := False;
                      if Procedimiento.Modelo.TieneEfectosAleatorios or
                         Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
                      begin
                        for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                        begin
                          if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                          begin
                            TieneUnidadExperimental := True;
                            if AnchoCampo2 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto) then
                              AnchoCampo2 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto);
                          end;

                          for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                          begin
                            if AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro]) then
                              AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro]);
                          end;
                        end;
                      end;

                      AnchoCampo3 := 0;
                      AnchoCampo4 := 0;
                      if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
                      begin
                        if AnchoCampo4 < Length(Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto) then
                          AnchoCampo4 := Length(Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto);
                      end;
                      for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
                      begin
                        if AnchoCampo3 < Length(Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro]) then
                          AnchoCampo3 := Length(Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro]);
                      end;

                      // Verificar si el ancho del segundo campo es menor que la longitud de
                      // "Unidad Experimental"
                      if AnchoCampo2 < Length('Unidad Experimental') then
                        AnchoCampo2 := Length('Unidad Experimental');
                      if AnchoCampo4 < Length('Unidad Experimental') then
                        AnchoCampo4 := Length('Unidad Experimental');

                      // Verificar si el hessiano es positivo semi-definido, de no serlo, no
                      // es posible calcular el error estandar de los estimadores de
                      // parametros de covarianza.
                      if Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.HessianoPositivoDefinido then
                      begin
                        // El hessiano es positivo semidefinido

                        IndiceEstimador := 1;

                        // Parametros de efectos aleatorios

                        if Procedimiento.Modelo.TieneEfectosAleatorios or
                           Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
                        begin
                          LineasSalida.Add('Parametros de efectos aleatorios:');
                          LineasSalida.Add('');

                          // Verificar si algun grupo de efectos aleatorios define una unidad
                          // experimental
                          if TieneUnidadExperimental then
                          begin
                            LineasSalida.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
                            LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                            for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                            begin
                              if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                                String1 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto
                              else
                                String1 := '';

                              for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                              begin
                                LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                                Inc(IndiceEstimador);
                              end;
                            end;
                          end
                          else
                          begin
                            LineasSalida.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
                            LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                            for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                            begin
                              for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                              begin
                                LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-20.4f %-20.4f %-20.4f', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                                Inc(IndiceEstimador);
                              end;
                            end;
                          end;

                          LineasSalida.Add('');
                        end;

                        LineasSalida.Add('Parametros del error:');
                        LineasSalida.Add('');

                        // Verificar si se definio unidad experimental para el error
                        if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
                        begin
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo3) + 's %-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
                          LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                          String1 := Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto;

                          for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
                          begin
                            LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                            Inc(IndiceEstimador);
                          end;
                        end
                        else
                        begin
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
                          LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                          for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
                          begin
                            LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20.4f %-20.4f', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [IndiceEstimador]]));
                            Inc(IndiceEstimador);
                          end;
                        end;
                      end
                      else
                      begin
                        // El hessiano no es positivo semidefinido

                        IndiceEstimador := 1;

                        // Parametros de efectos aleatorios

                        if Procedimiento.Modelo.TieneEfectosAleatorios or
                           Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
                        begin
                          LineasSalida.Add('Parametros de efectos aleatorios:');
                          LineasSalida.Add('');

                          // Verificar si algun grupo de efectos aleatorios define una unidad
                          // experimental
                          if TieneUnidadExperimental then
                          begin
                            LineasSalida.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
                            LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                            for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                            begin
                              if Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].TieneUnidadExperimental then
                                String1 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].UnidadExperimental.Texto
                              else
                                String1 := '';

                              for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                              begin
                                LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                                Inc(IndiceEstimador);
                              end;
                            end;
                          end
                          else
                          begin
                            LineasSalida.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-20s %-20s %-20s', ['Grupo', 'Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
                            LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                            for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                            begin
                              for IndiceParametro := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Alto do
                              begin
                                LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-20.4f %-20s %-20s', [IndiceGrupo, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                                Inc(IndiceEstimador);
                              end;
                            end;
                          end;

                          LineasSalida.Add('');
                        end;

                        LineasSalida.Add('Parametros del error:');
                        LineasSalida.Add('');

                        // Verificar si se definio unidad experimental para el error
                        if Procedimiento.InformacionTiempoEjecucionError.TieneUnidadExperimental then
                        begin
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo3) + 's %-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Unidad Experimental', 'Estimador', 'Error Estandar', 'Valor-Z']));
                          LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                          String1 := Procedimiento.InformacionTiempoEjecucionError.UnidadExperimental.Texto;

                          for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
                          begin
                            LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                            Inc(IndiceEstimador);
                          end;
                        end
                        else
                        begin
                          LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo4) + 's %-20s %-20s %-20s', ['Nombre', 'Estimador', 'Error Estandar', 'Valor-Z']));
                          LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                          for IndiceParametro := Procedimiento.InformacionTiempoEjecucionError.Parametros.Bajo to Procedimiento.InformacionTiempoEjecucionError.Parametros.Alto do
                          begin
                            LineasSalida.Add(Format('%-' + IntToStr(AnchoCampo2) + 's %-20.4f %-20s %-20s', [Procedimiento.InformacionTiempoEjecucionError.Parametros.Nombres [IndiceParametro], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [IndiceEstimador], '-', '-']));
                            Inc(IndiceEstimador);
                          end;
                        end;
                      end
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'efectos_aleatorios_solucion' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Solucion para los efectos aleatorios:');
                      LineasSalida.Add('');

                      // Generar la solucion para la estructura de covarianza si el modelo
                      // posee efectos aleatorios.
                      if Procedimiento.Modelo.TieneEfectosAleatorios or
                         Procedimiento.Modelo.TieneEfectosAleatoriosGrupos then
                      begin
                        // Calcular la longitud maxima del nombre de un efecto
                        AnchoCampo1 := 0;
                        for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                        begin
                          for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
                          begin
                            if  AnchoCampo1 < Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto) then
                               AnchoCampo1 := Length(Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto);
                          end;
                        end;

                        // Verificar si el nombre de efecto mas largo es menor que "efecto".
                        if AnchoCampo1 < 6 then
                          AnchoCampo1 := 6;

                        // Calcular la longitud maxima del identificador de un nivel, y buscar
                        // la cantidad maxima de variables de clasificacion en un efecto
                        // aleatorio.
                        AnchoCampo2 := 0;
                        CantidadVariablesClasificacion := 0;
                        for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                        begin
                          for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
                          begin
                            if (not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                               (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
                            begin
                              if CantidadVariablesClasificacion < Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension then
                                CantidadVariablesClasificacion := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesColumnasVariablesClasificacion.Dimension;

                              for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion.Alto do
                              begin
                                for IndiceNivel := Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Bajo to Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]].Alto do
                                begin
                                  if AnchoCampo2 < Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]) then
                                    AnchoCampo2 := Length(Procedimiento.MatrizNivelesVariablesClasificacion.Arreglos [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable]] [IndiceNivel]);
                                end;
                              end;
                            end;
                          end;
                        end;

                        // Verificar si la lista mas larga de niveles de las variables de
                        // clasificacion es menor que "Niveles".
                        if AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1) < 8 then
                          AnchoCampo3 := 8
                        else
                          AnchoCampo3 := AnchoCampo2 * CantidadVariablesClasificacion * (CantidadVariablesClasificacion - 1);

                        LineasSalida.Add(Format('%-10s %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) + 's %-15s %-15s', ['Grupo', 'Efecto', 'Niveles', 'Estimador', 'Error Estandar']));
                        LineasSalida.Add('----------------------------------------------------------------------------------------------------');

                        IndiceEstimador := 1;
                        for IndiceGrupo := 0 to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
                        begin
                          for IndiceEfecto := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos.Alto do
                          begin
                            String2 := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto.Texto;

                            if (not (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].Efecto is TIntercepto)) and
                               (Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].TieneVariablesClasificacion) then
                            begin
                              for IndiceNivel := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
                              begin
                                String1 := '';

                                for IndiceVariable := Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Bajo to Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto - 1 do
                                  String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's ', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [IndiceVariable], Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, IndiceVariable]]]);
                                String1 := String1 + Format('%-' + IntToStr(AnchoCampo2) + 's', [Procedimiento.MatrizNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNivelesVariablesClasificacion [Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto], Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles [IndiceNivel, Procedimiento.InformacionTiempoEjecucionEfectosAleatorios [IndiceGrupo].InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Arreglos [IndiceNivel].Alto]]]);

                                LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f', [IndiceGrupo, String2, String1, Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.PredictoresEfectosAleatorios [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios [IndiceEstimador]]));
                                Inc(IndiceEstimador);
                              end;
                            end
                            else
                            begin
                              LineasSalida.Add(Format('%-10d %-' + IntToStr(AnchoCampo1) + 's %-' + IntToStr(AnchoCampo3) +  's %-15.4f %-15.4f', [IndiceGrupo, String2, '', Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.PredictoresEfectosAleatorios [IndiceEstimador], Procedimiento.InformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios [IndiceEstimador]]));
                              Inc(IndiceEstimador);
                            end;
                          end;
                        end;
                      end;
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'bitacora_iteracion' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Bitacora de iteracion:');
                      LineasSalida.Add('');
                      LineasSalida.AddStrings(BitacoraIteracion);
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'informacion_ajuste' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Informacion de ajuste:');
                      LineasSalida.Add('');
                      LineasSalida.Add(Format('%-45s %.2f', ['Verosimilitud Logaritmica:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica]));
                      LineasSalida.Add(Format('%-45s %.2f', ['-2.0 * Verosimilitud Logaritmica:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmica]));
                      LineasSalida.Add(Format('%-45s %.2f', ['Verosimilitud Logaritmica Residual:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual]));
                      LineasSalida.Add(Format('%-45s %.2f', ['-2.0 * Verosimilitud Logaritmica Residual:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmicaResidual]));
                      LineasSalida.Add(Format('%-45s %.2f', ['AIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.AIC]));
                      LineasSalida.Add(Format('%-45s %.2f', ['AICC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.AICC]));
                      LineasSalida.Add(Format('%-45s %.2f', ['HQIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.HQIC]));
                      LineasSalida.Add(Format('%-45s %.2f', ['BIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.BIC]));
                      LineasSalida.Add(Format('%-45s %.2f', ['CAIC:', Procedimiento.InformacionPostProcesamientoCriteriosAjuste.CAIC]));
                    end
                    else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_x' then
                    else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_z' then
                    else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_g' then
                    else if OpcionesSalida [IndiceOpcionSalida] = 'mostrar_r' then
                    else if OpcionesSalida [IndiceOpcionSalida] = 'estadisticas_iteracion' then
                    begin
                      LineasSalida.Add('');
                      LineasSalida.Add(IntToStr(IndiceOpcionSalida + 1) + ') ' + 'Estadisticas de iteracion:');
                      LineasSalida.Add('');
                      LineasSalida.Add(Format('%-50s %8d', ['Cantidad de Iteraciones:', Procedimiento.CantidadIteraciones]));
                      LineasSalida.Add(Format('%-50s %8d', ['Cantidad de Iteraciones de Bucle Interno:', Procedimiento.CantidadIteracionesBucleInterno]));
                      LineasSalida.Add(Format('%-50s %8d', ['Cantidad de Evaluaciones de la Funcion Objetivo:', Procedimiento.CantidadEvaluacionesFuncionObjetivo]));
                      LineasSalida.Add(Format('%-50s %8s', ['Duracion de Etapa de Inicializacion:', FormatDateTime('hh:mm:ss', DuracionInicializacion)]));
                      LineasSalida.Add(Format('%-50s %8s', ['Duracion de Etapa de Iteracion:', FormatDateTime('hh:mm:ss', DuracionIteracion)]));
                    end;
                  end;
                end;

                if MostrarSalidaConsola then
                begin
                  for I := 0 to LineasSalida.Count - 1 do
                    WriteLn(LineasSalida.Strings [I]);
                end;

                if TieneArchivoSalida then
                begin
                  for I := 0 to LineasSalida.Count - 1 do
                    WriteLn(ArchivoSalida, LineasSalida.Strings [I]);
                end;
              end
              else
              begin
                // Post-procesamiento fallido

                WriteLn('"' + NombreArchivoConfiguracion + '" -> (600).');

                if EscribirResumenVerosimilitud then
                  WriteLn(ArchivoResumenVerosimilitud, '-');

                if EscribirResumenBeta then
                  WriteLn(ArchivoResumenBeta, '-');

                if EscribirResumenTheta then
                  WriteLn(ArchivoResumenTheta, '-');

                if EscribirResumenIteracion then
                  WriteLn(ArchivoResumenIteracion, '- 600');

                if MostrarSalidaConsola then
                begin
                  for I := 0 to BitacoraPostProcesamiento.Count - 1 do
                    WriteLn(BitacoraPostProcesamiento.Strings [I]);
                  for I := 0 to BitacoraEjecucion.Count - 1 do
                    WriteLn(BitacoraEjecucion.Strings [I]);
                  for I := 0 to BitacoraIteracion.Count - 1 do
                    WriteLn(BitacoraEjecucion.Strings [I]);
                end;

                if TieneArchivoSalida then
                begin
                  for I := 0 to BitacoraPostProcesamiento.Count - 1 do
                    WriteLn(ArchivoSalida, BitacoraPostProcesamiento.Strings [I]);
                  for I := 0 to BitacoraEjecucion.Count - 1 do
                    WriteLn(ArchivoSalida, BitacoraEjecucion.Strings [I]);
                  for I := 0 to BitacoraIteracion.Count - 1 do
                    WriteLn(ArchivoSalida, BitacoraIteracion.Strings [I]);
                end;
              end;
            end
            else
            begin
              // Ejecucion fallida

              WriteLn('"' + NombreArchivoConfiguracion + '" -> (500).');

              if EscribirResumenVerosimilitud then
                WriteLn(ArchivoResumenVerosimilitud, '-');

              if EscribirResumenBeta then
                WriteLn(ArchivoResumenBeta, '-');

              if EscribirResumenTheta then
                WriteLn(ArchivoResumenTheta, '-');

              if EscribirResumenIteracion then
                WriteLn(ArchivoResumenIteracion, '- 500');

              if MostrarSalidaConsola then
              begin
                for I := 0 to BitacoraEjecucion.Count - 1 do
                  WriteLn(BitacoraEjecucion.Strings [I]);
                for I := 0 to BitacoraIteracion.Count - 1 do
                  WriteLn(BitacoraEjecucion.Strings [I]);
              end;

              if TieneArchivoSalida then
              begin
                for I := 0 to BitacoraEjecucion.Count - 1 do
                  WriteLn(ArchivoSalida, BitacoraEjecucion.Strings [I]);
                for I := 0 to BitacoraIteracion.Count - 1 do
                  WriteLn(ArchivoSalida, BitacoraIteracion.Strings [I]);
              end;
            end;
          end
          else
          begin
            // Inicializacion de procedimiento fallida

            WriteLn('"' + NombreArchivoConfiguracion + '" -> (400).');

            if EscribirResumenVerosimilitud then
              WriteLn(ArchivoResumenVerosimilitud, '-');

            if EscribirResumenBeta then
              WriteLn(ArchivoResumenBeta, '-');

            if EscribirResumenTheta then
              WriteLn(ArchivoResumenTheta, '-');

            if EscribirResumenIteracion then
              WriteLn(ArchivoResumenIteracion, '- 400');

            if MostrarSalidaConsola then
            begin
              for I := 0 to BitacoraInicializacion.Count - 1 do
                WriteLn(BitacoraInicializacion.Strings [I]);
            end;

            if TieneArchivoSalida then
            begin
              for I := 0 to BitacoraInicializacion.Count - 1 do
                WriteLn(ArchivoSalida, BitacoraInicializacion.Strings [I]);
            end;
          end;

          if EscribirResumenVerosimilitud then
          begin
            Flush(ArchivoResumenVerosimilitud);
            CloseFile(ArchivoResumenVerosimilitud);
          end;

          if EscribirResumenBeta then
          begin
            Flush(ArchivoResumenBeta);
            CloseFile(ArchivoResumenBeta);
          end;

          if EscribirResumenTheta then
          begin
            Flush(ArchivoResumenTheta);
            CloseFile(ArchivoResumenTheta);
          end;

          if EscribirResumenIteracion then
          begin
            Flush(ArchivoResumenIteracion);
            CloseFile(ArchivoResumenIteracion);
          end;

          if TieneArchivoSalida then
          begin
            Flush(ArchivoSalida);
            CloseFile(ArchivoSalida);
          end;
        end
        else
        begin
          // Validacion fallida

          WriteLn('"' + NombreArchivoConfiguracion + '" -> (300).');
        end;
      end
      else
      begin
        // Inicializacion fallida

        WriteLn('"' + NombreArchivoConfiguracion + '" -> (200).');
      end;
    end
    else
    begin
      // Parseo fallido

      WriteLn('"' + NombreArchivoConfiguracion + '" -> (100).');
    end;

    FreeAndNil(Procedimiento);
    FreeAndNil(Configuracion);
  end;

  FreeAndNil(OpcionesSalidaPorDefecto);
  FreeAndNil(BitacoraInicializacionModelo);
  FreeAndNil(BitacoraInicializacion);
  FreeAndNil(BitacoraIteracion);
  FreeAndNil(BitacoraEjecucion);
  FreeAndNil(BitacoraValidacionDatos);
  FreeAndNil(BitacoraValidacionModelo);
  FreeAndNil(BitacoraValidacionProcedimiento);
  FreeAndNil(BitacoraPostProcesamiento);
  FreeAndNil(LineasSalida);
end { TAplicacionModelosMixtos.Ejecutar };

end { UnitAplicacionModelosMixtos }.

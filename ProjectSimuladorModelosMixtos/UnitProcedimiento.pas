{**
@abstract(Clase principal del proyecto. Encapsula el procedimiento de
simulacion.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(February 06, 2005)
Este modulo contiene la clase principal de todo el proyecto. Agrupa estructuras
de datos y metodos asociados al procedimiento de simulacion modelos mixtos.
}
unit UnitProcedimiento;

interface

uses
  Classes, Contnrs, Math, SysUtils,
  UaConstantes, UaComun, UaGeneradorNumerosAleatorios, UaVector, UaMatriz,
  UnitArregloEnteros, UnitArregloStrings, UnitCalculoSimbolico,
  UnitColumnaDatos, UnitConfiguracion, UnitConfiguracionDatos,
  UnitConfiguracionModelo, UnitConstantesProcedimiento, UnitEfecto,
  UnitEstructura, UnitGrupoEfectos,
  UnitInformacionTiempoEjecucionProcedimiento, UnitMatrizValores, UnitOpcion,
  UnitParametros, UnitSubrutinasAlgebraicas;

type
  {** Clase principal del proyecto. Esta clase agrupa estructuras de datos y
      metodos necesarios para el procesamiento de la configuracion y posterior
      simulacion de los modelos mixtos de acuerdo a la misma.
      @abstract(Clase con estructuras de datos y metodos necesarios para la
      simulacion del modelo.) }
  TProcedimiento = class
  protected
    {** Configuracion de datos del modelo a simular. }
    FDatos: TConfiguracionDatos;
    {** Configracion del modelo a simular. }
    FModelo: TConfiguracionModelo;
    {** Configuracion de procedimiento. }
    FOpciones: TListaOpciones;
    {** Matriz de valores generada. }
    FValores: TMatrizValores;

    {** Cantidad (maxima) de observaciones a generar. Depende tanto de la
        configuracion de datos, como de la cantidad de posibles combinaciones
        de niveles de las columnas categoricas. }
    FCantidadObservaciones: Integer;
    {** Nombre de archivo de salida. }
    FArchivoSalida: TFileName;
    {** Funcion caracteristica del conjunto de observaciones. Indica si una
        observacion particular debe incluirse en los datos generados. }
    FFuncionCaracteristica: TCsExpresionBooleana;
    {** Longituda maxima de valores de columna. Se utiliza para imprimir de
        manera tabulada los valores simulados. }
    FLongitudMaximaValoresColumna: TArregloEnteros;
    {** Matriz con los valores de los niveles de cada variable de
        clasificacion. }
    FMatrizNivelesVariablesClasificacion: TArregloArreglosStrings;
    {** Cantidad de filas de la matriz de covarianza de efectos aleatorios (G).
        Se utiliza para no recalcular el valor a partir de la cantidad de filas
        de las submatrices de cada grupo de efectos aleatorios. }
    FCantidadFilasMatrizCovarianzaEfectosAleatorios: Integer;
    {** Cantidad de parametros relacionados a efectos aleatorios. Se utiliza
        para no recalcular el valor a partir de la cantidad de parametros de
        cada grupo de efectos aleatorios. }
    FCantidadParametrosEfectosAleatorios: Integer;
    {** Indica si se debe comentar el valor de los parametros en la salida. }
    FComentarValoresParametros: Boolean;
    {** Generador numeros aleatorios de observaciones. }
    FGeneradorNumerosAleatorios: TUaGeneradorNumerosAleatorios;
    {** Lista con informacion de criterios (agrupamiento, ordenamiento, etc.)
        Se utiliza para no recalcular la informacion cuando es utilizada a la
        vez por varios grupos de efectos aleatorios o en la configuracion del
        error. Ver
        @link(UnitInformacionTiempoEjecucionProcedimiento.TInformacionTiempoEjecucionCriterio). }
    FInformacionTiempoEjecucionCriterios: TListaInformacionTiempoEjecucionEfectos;
    {** Lista con informacion sobre los grupos de efectos aleatorios. Esta lista
        incluye la informacion sobre los efectos aleatorios no-agrupados (la
        misma encabeza la lista). Ver
        @link(UnitInformacionTiempoEjecucionProcedimiento.TInformacionTiempoEjecucionGrupoEfectosAleatorios). }
    FInformacionTiempoEjecucionEfectosAleatorios: TListaInformacionTiempoEjecucionGrupoEfectosAleatorios;
    {** Informacion sobre los efectos fijos del modelo. Ver
        @link(UnitInformacionTiempoEjecucionProcedimiento.TInformacionTiempoEjecucionEfectosFijos). }
    FInformacionTiempoEjecucionEfectosFijos: TInformacionTiempoEjecucionEfectosFijos;
    {** Informacion sobre el error modelo. Ver
        @link(UnitInformacionTiempoEjecucionProcedimiento.TInformacionTiempoEjecucionError). }
    FInformacionTiempoEjecucionError: TInformacionTiempoEjecucionError;
    {** Vector que contiene los valores actuales de los parametros de los
        efectos aleatorios. Este vector se utiliza para construir todas las
        estructuras dinamicas dependientes de parametros durante el
        procedimiento. Los parametros asociados a los efectos aleatorios se
        almacenan primero y luego los relacionados al error. }
    FValoresParametrosEfectosAleatorios: TUaVector;
    {** Lista con variables de clasificacion auxiliares. Estas son necesarias
        dado que es posible que una covariable sea transformada en variable de
        clasificacion cuando la misma es utilizada dentro de un criterio. }
    FVariablesClasificacionAuxiliares: TStrings;
    {** Salida generada. }
    FSalida: TStrings;
    {** Semilla para generadores de numeros aleatorios de las observaciones. }
    FSemillaGeneradorNumerosAleatorios: LongInt;

    // Matrices, vectores y valores asociados al modelo

    {** Beta. Vector de parametros de los efectos fijos. }
    FBeta: TUaMatriz;
    {** Vector (matriz de Nx1) de observaciones de la variable dependiente. }
    FY:  TUaMatriz;
    {** Transpuesta de FY. }
    FYT: TUaMatriz;
    {** Matriz de covarianza de los efectos aleatorios. }
    FG: TUaMatriz;
    {** Descomposicion de Cholesky de FG. }
    FL: TUaMatriz;
    {** Transpuesta de FL. }
    FLT: TUaMatriz;
    {** Matriz de covarianza del error. }
    FR: TUaMatriz;
    {** Descomposicion de Cholesky de FR. }
    FRaizCuadradaR: TUaMatriz;
    {** Inversa de FR. }
    FRI: TUaMatriz;
    {** Matriz de diseno de efectos fijos. }
    FX: TUaMatriz;
    {** Transpuesta de FX. }
    FXT: TUaMatriz;
    {** X*Beta. }
    FXBeta: TUaMatriz;
    {** Rango de FX. }
    FRangoX: TUaReal;
    {** Matriz de diseno de los efectos aleatorios. }
    FZ: TUaMatriz;
    {** Transpuesta de FZ. }
    FZT: TUaMatriz;
    {** Matriz de varianza/covarianza del modelo. }
    FV: TUaMatriz;
    {** Descomposicion de cholesky de la matriz de varianza/covarianza del
        modelo. }
    FLV: TUaMatriz;
    {** -2.0 * Verosimilitud logaritmica. }
    FVerosimilitudLogaritmica: TUaReal;
    {** -2.0 * Verosimilitud logaritmica residual. }
    FVerosimilitudLogaritmicaResidual: TUaReal;

    // Metodos

    {** Calcula el valor de los terminos ll, l2 y l3 de la funcion de maxima
        verosimilitud.
        @param(L1 Primer termino de la funcion objetivo.)
        @param(L2 Segundo termino de la funcion objetivo.)
        @param(L3 Tercer termino de la funcion objetivo.) }
    procedure CalcularTerminosValorFuncionObjetivo(var L1, L2, L3: TUaReal);
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un criterio. Ver
        @link(TInformacionTiempoEjecucionCriterio).
        @param(Criterio Critrio sobre el cual construir la informacion.)
        @returns(Informacion de tiempo de ejecucion sobre el criterio.) }
    function ConstruirInformacionTiempoEjecucionCriterio(const Criterio: TEfecto): TInformacionTiempoEjecucionCriterio;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un efecto. Ver
        @link(TInformacionTiempoEjecucionEfecto).
        @param(Efecto Efecto sobre el cual construir la informacion.)
        @returns(Informacion de tiempo de ejecucion sobre el efecto.) }
    function ConstruirInformacionTiempoEjecucionEfecto(const Efecto: TEfecto): TInformacionTiempoEjecucionEfecto;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de los efectos fijos. Ver
        @link(TInformacionTiempoEjecucionEfectosFijos).
        @param(Grupo Grupo que contiene la configuracion de efectos fijos.)
        @returns(Informacion de tiempo de ejecucion sobre los efectos fijos.) }
    function ConstruirInformacionTiempoEjecucionEfectosFijos(const Grupo: TGrupoEfectos): TInformacionTiempoEjecucionEfectosFijos;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion del error. Ver @link(TInformacionTiempoEjecucionError).
        @param(Error Lista de opciones del error.)
        @returns(Informacion de tiempo de ejecucion sobre el error.) }
    function ConstruirInformacionTiempoEjecucionError(const Error: TListaOpciones): TInformacionTiempoEjecucionError;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un grupo de efectos aleatorios. Ver
        @link(TInformacionTiempoEjecucionGrupoEfectosAleatorios).
        @param(Grupo Grupo que contiene la configuracion del grupo de efectos
        aleatorios.)
        @returns(Informacion de tiempo de ejecucion sobre el grupo de efectos
        aleatorios.) }
    function ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios(const Grupo: TGrupoEfectos): TInformacionTiempoEjecucionGrupoEfectosAleatorios;
    {** Construye la matriz de covarianza de efectos aleatorios G a partir de
        los parametros actuales.
        @param(Matriz Matriz de covarianza de efectos aleatorios G.) }
    procedure ConstruirMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz);
    {** Construye la matriz de covarianza del error R a partir de los
        parametros actuales.
        @param(Matriz Matriz de covarianza del error R.) }
    procedure ConstruirMatrizCovarianzaError(var Matriz: TUaMatriz);
    {** Construye la matriz de diseno de efectos aleatorios Z a partir de la
        configuracion del modelo y la informacion en tiempo de ejecucion
        procesada.
        @param(Matriz Matriz de diseno de efectos aleatorios.) }
    procedure ConstruirMatrizDisenoEfectosAleatorios(var Matriz: TUaMatriz);
    {** Construye la matriz de diseno de efectos fijos X a partir de la
        configuracion del modelo y la informacion en tiempo de ejecucion
        procesada.
        @param(Matriz Matriz de diseno de efectos fijos.) }
    procedure ConstruirMatrizDisenoEfectosFijos(var Matriz: TUaMatriz);
    {** Construye la submatriz de covarianza asociada a un grupo de efectos
        aleatorios sobre otra matriz, de acuerdo a la informacion de tiempo de
        ejecucion del mismo. Utiliza los parametros actualez para construir
        la misma.
        @param(Informacion Informacion sobre el grupo de efectos.)
        @param(Matriz Matriz que contiene a la submatriz generada.)
        @param(FilaInicial Fila donde insertar la submatriz (Se inserta a
        partir de la celda FilaInicial x FilaInicial).) }
    procedure ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; const FilaInicial: Integer = 1);
    {** Construye la submatriz asociada a un efecto de la matriz de diseno de
        efectos fijos X.
        @param(Informacion Informacion sobre el efecto fijo.)
        @param(Matriz Matriz que contendra a la submatriz.)
        @param(ColumnaInicial Columna donde se insertara la submatriz (Se
        inserta a partir de la celda 1 x ColumnaInicial).) }
    procedure ConstruirSubmatrizDisenoEfectoFijo(const Informacion: TInformacionTiempoEjecucionEfecto; var Matriz: TUaMatriz; const ColumnaInicial: Integer = 1);
    {** Construye la submatriz asociada a un grupo de efectos aleatorios de la
        matriz de diseno de efectos aleatorios Z.
        @param(Informacion Informacion sobre el grupo de efectos aleatorios.)
        @param(Matriz Matriz que contendra a la submatriz.)
        @param(ColumnaInicial Columna donde se insertara la submatriz (Se
        inserta a partir de la celda 1 x ColumnaInicial).) }
    procedure ConstruirSubmatrizDisenoGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; const ColumnaInicial: Integer = 1);
  public
    {** Constructor.
        @param(AConfiguracion Configuracio del modelo a simular.) }
    constructor Create(const AConfiguracion: TConfiguracion);
    {** Destructor. }
    destructor Destroy; override;
    {** Ejecuta el proceso de ajuste.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de ajuste.)
        @returns(@true si el procedimiento finalizo correctamente, o @false en
        caso contrario.) }
    function Ejecutar(var Bitacora: TStrings): Boolean;
    {** Inicializa las estructuras de datos internas del procedimiento.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de inicializacion.)
        @returns(@true si el procedimiento finalizo correctamente, o @false en
        caso contrario.) }
    function Inicializar(var Bitacora: TStrings): Boolean;
    {** Matriz de valores generados. }
    property Valores: TMatrizValores read FValores;
    {** Nombre archivo salida. }
    property ArchivoSalida: TFileName read FArchivoSalida write FArchivoSalida;
    {** Salida generada. }
    property Salida: TStrings read FSalida;
    {** Matriz de dise�o de efectos fijos. }
    property X: TUaMatriz read FX;
    {** Matriz de dise�o de efectos aleatorios. }
    property Z: TUaMatriz read FZ;
    {** Matriz de varianza/covarianza del error. }
    property R: TUaMatriz read FR;
    {** Inversa de R. }
    property RI: TUaMatriz read FR;
    {** Raiz cuadrada de R. }
    property RaizCruadradaR: TUaMatriz read FRaizCuadradaR;
    {** Matriz de varianza/covarianza de efectos aleatorios. }
    property G: TUaMatriz read FG;
    {** Descomposicion de Cholesky de G. }
    property L: TUaMatriz read FL;
    {** Matriz de varianza del modelo. }
    property V: TUaMatriz read FV;
    {** Descomposicion de Choleksy de V. }
    property LV: TUaMatriz read FLV;
    {** Vector de parametros de efectos fijos. }
    property Beta: TUaMatriz read FBeta;
    {** X * Beta. }
    property XBeta: TUaMatriz read FXBeta;
    {** Vector de parametros de efectos aleatorios (Theta). }
    property ValoresParametrosEfectosAleatorios: TUaVector read FValoresParametrosEfectosAleatorios;
    {** -2.0 * Verosimilitud logaritmica del caso. }
    property VerosimilitudLogaritmica: TUaReal read FVerosimilitudLogaritmica;
    {** -2.0 * Verosimilitud logaritmica residual del caso. }
    property VerosimilitudLogaritmicaResidual: TUaReal read FVerosimilitudLogaritmicaResidual;
  end { TProcedimiento };

implementation

// -----------------------------------------------------------------------------
// TProcedimiento
// -----------------------------------------------------------------------------

constructor TProcedimiento.Create(const AConfiguracion: TConfiguracion);
begin { TProcedimiento.Create }
  Assert(Assigned(AConfiguracion), 'TProcedimiento.Create: Assigned(AConfiguracion)');
  Assert(AConfiguracion.Valida, 'TProcedimiento.Create: AConfiguracion.Valida');

  FModelo := AConfiguracion.Modelo;
  FDatos := AConfiguracion.Datos;
  FOpciones := AConfiguracion.Procedimiento.Opciones;
  FValores := nil;
  FY := nil;
  FG := nil;
  FR := nil;
  FBeta := nil;
  FX := nil;
  FZ := nil;
  FZT := nil;

  FVariablesClasificacionAuxiliares := TStringList.Create;
  FInformacionTiempoEjecucionEfectosAleatorios := TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Create;
  FInformacionTiempoEjecucionCriterios := TListaInformacionTiempoEjecucionEfectos.Create;
  FSalida := TStringList.Create;
end { TProcedimiento.Create };

destructor TProcedimiento.Destroy;
begin { TProcedimiento.Destroy }
  FreeAndNil(FVariablesClasificacionAuxiliares);
  FreeAndNil(FMatrizNivelesVariablesClasificacion);
  FreeAndNil(FSalida);
  FreeAndNil(FInformacionTiempoEjecucionEfectosFijos);
  FreeAndNil(FInformacionTiempoEjecucionEfectosAleatorios);
  FreeAndNil(FInformacionTiempoEjecucionError);
  FreeAndNil(FInformacionTiempoEjecucionCriterios);
  FreeAndNil(FValoresParametrosEfectosAleatorios);
  FreeAndNil(FGeneradorNumerosAleatorios);

  FreeAndNil(FY);
  FreeAndNil(FYT);
  FreeAndNil(FG);
  FreeAndNil(FR);
  FreeAndNil(FRaizCuadradaR);
  FreeAndNil(FRI);
  FreeAndNil(FBeta);
  FreeAndNil(FX);
  FreeAndNil(FXT);
  FreeAndNil(FZ);
  FreeAndNil(FZT);
  FreeAndNil(FV);
  FreeAndNil(FLV);
  FreeAndNil(FXBeta);
end { TProcedimiento.Destroy };

function TProcedimiento.Inicializar(var Bitacora: TStrings): Boolean;
var
  I, J, K, IndiceFila, CantidadCombinacionesNiveles: Integer;
  Listo, IncluirObservacion, CantidadObservacionesDefinida: Boolean;
  Contexto: TCsContexto;
  Niveles: TArregloStrings;
  Expresion: TCsExpresion;
  ColumnaDatosCategoricos: TColumnaDatosCategoricos;
  IndicesColumnasNumericas, IndicesColumnasCategoricas, IndicesColumnasVector, IndicesNivelColumnasCategoricas, IndicesNivelColumnasVector: TArregloEnteros;
begin { TProcedimiento.Inicializar }
  Assert(Assigned(Bitacora), 'TProcedimiento.Inicializar: Assigned(Bitacora)');

  // Inicializar referencias
  Niveles := nil;
  Contexto := nil;
  IndicesColumnasNumericas := nil;
  IndicesColumnasCategoricas := nil;
  IndicesNivelColumnasCategoricas := nil;

  try
    // Inicializar datos

    // Inicializar opciones de datos

    // Opcion cantidad de observaciones
    CantidadObservacionesDefinida := FDatos.Opciones.BuscarAsignar(I, 'cantidad_observaciones');
    if CantidadObservacionesDefinida then
      FCantidadObservaciones := (FDatos.Opciones [I] as TOpcionParametroInteger).Parametro
    else
      FCantidadObservaciones := CCantidadObservaciones;

    // Calcular la cantidad de combinaciones de niveles de columnas categoricas
    CantidadCombinacionesNiveles := 1;
    for I := 0 to FDatos.Columnas.Count - 1 do
    begin
      if (FDatos.Columnas [I] is TColumnaDatosCategoricos) then
      begin
        ColumnaDatosCategoricos := FDatos.Columnas [I] as TColumnaDatosCategoricos;

        J := 0;
        for K := ColumnaDatosCategoricos.CantidadObservacionesNivel.Bajo to ColumnaDatosCategoricos.CantidadObservacionesNivel.Alto do
          J := J + ColumnaDatosCategoricos.CantidadObservacionesNivel [K];

        CantidadCombinacionesNiveles := CantidadCombinacionesNiveles * J;
      end;
    end;

    // No generar mas que la cantidad de posibles combinaciones
    if (not CantidadObservacionesDefinida) or
       (CantidadCombinacionesNiveles < FCantidadObservaciones) then
      FCantidadObservaciones := CantidadCombinacionesNiveles;

    // Opcion funcion caracteristica
    if FDatos.Opciones.BuscarAsignar(I, 'funcion_caracteristica') then
      FFuncionCaracteristica := (FDatos.Opciones [I] as TOpcionParametroTObject).Parametro as TCsExpresionBooleana
    else
      FFuncionCaracteristica := TCsConstanteBooleana.Create(True);

    // Inicializar opciones de procedimiento

    // Comentar valores de parametros en el archivo de salida
    if FOpciones.BuscarAsignar(I, 'comentar_valores_parametros') then
      FComentarValoresParametros := (FOpciones [I] as TOpcionParametroBoolean).Parametro
    else
      FComentarValoresParametros := True;

    // Opcion archivo de salida
    if FArchivoSalida = '' then
      FArchivoSalida := 'ValoresSimulados' + FloatToStr(Now) + '.txt';

    // Semillas de generadores de numeros aleatorios
    if FOpciones.BuscarAsignar(I, 'semilla_generador_numeros_aleatorios') then
      FSemillaGeneradorNumerosAleatorios := (FDatos.Opciones [I] as TOpcionParametroInteger).Parametro
    else
      // FSemillaGeneradorNumerosAleatorios := 19650218;
      FSemillaGeneradorNumerosAleatorios := -1;

    if not Assigned(FGeneradorNumerosAleatorios) then
      // FGeneradorNumerosAleatorios := TUaMersenneTwister.Create(FSemillaGeneradorNumerosAleatorios);
      FGeneradorNumerosAleatorios := TUaMersenneTwister.Create(FSemillaGeneradorNumerosAleatorios);

    // Inicializar indices de columnas categoricas, numericas y de vector (de
    // cualquier tipo).
    IndicesColumnasCategoricas := TArregloEnteros.Create(0);
    IndicesColumnasNumericas := TArregloEnteros.Create(0);
    IndicesColumnasVector := TArregloEnteros.Create(0);
    for I := 0 to FDatos.Columnas.Count - 1 do
    begin
      if (FDatos.Columnas [I] is TColumnaDatosCategoricos) then
      begin
        // Columna de valores categoricos

        IndicesColumnasCategoricas.Dimension := IndicesColumnasCategoricas.Dimension + 1;
        IndicesColumnasCategoricas [IndicesColumnasCategoricas.Alto] := I;
      end
      else if (FDatos.Columnas [I] is TColumnaDatosReales) or
              (FDatos.Columnas [I] is TColumnaDatosEnteros) then
      begin
        // Columna de datos numericos (generados por una expresion)

        IndicesColumnasNumericas.Dimension := IndicesColumnasNumericas.Dimension + 1;
        IndicesColumnasNumericas [IndicesColumnasNumericas.Alto] := I;
      end
      else
      begin
        // Columna con vector de datos

        IndicesColumnasVector.Dimension := IndicesColumnasVector.Dimension + 1;
        IndicesColumnasVector [IndicesColumnasVector.Alto] := I;
      end;
    end;

    // Construir contexto y asignarlo a las expresiones del modelo
    Contexto := TCsContexto.Create(FDatos.Columnas.Count);

    // Asignar contexto a las expresiones de las columnas y funcion caracteristica
    for I := IndicesColumnasNumericas.Bajo to IndicesColumnasNumericas.Alto do
    begin
      if (FDatos.Columnas [IndicesColumnasNumericas [I]] is TColumnaDatosReales) then
        (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosReales).Expresion.Contexto := Contexto
      else if (FDatos.Columnas [IndicesColumnasNumericas [I]] is TColumnaDatosEnteros) then
        (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosEnteros).Expresion.ExpresionReal.Contexto := Contexto;
    end;

    FFuncionCaracteristica.AsignarContexto(Contexto);

    // Construir matriz de valores, tomando en cuenta la columna (extra) de la
    // variable dependiente.
    FreeAndNil(FValores);
    FValores := TMatrizValores.Create(0, FDatos.Columnas.Count + 1);

    // Construir arreglo de indices actuales de niveles de columnas de datos categoricos.
    IndicesNivelColumnasCategoricas := TArregloEnteros.Create(IndicesColumnasCategoricas.Dimension);
    IndicesNivelColumnasCategoricas.PonerA(0);

    // Construir arreglo de indices de nivel de columnas con vector de datos
    IndicesNivelColumnasVector := TArregloEnteros.Create(IndicesColumnasVector.Dimension);
    IndicesNivelColumnasVector.PonerA(0);

    // Calcular el valor inicial del contexto

    // Columnas numericas
    for I := IndicesColumnasNumericas.Bajo to IndicesColumnasNumericas.Alto do
    begin
      if FDatos.Columnas [IndicesColumnasNumericas [I]] is TColumnaDatosReales then
        Contexto [IndicesColumnasNumericas [I]] := (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosReales).Expresion.Valor
      else
        Contexto [IndicesColumnasNumericas [I]] := (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosEnteros).Expresion.Valor;
    end;

    // Columnas categoricas
    for I := IndicesColumnasCategoricas.Bajo to IndicesColumnasCategoricas.Alto do
      Contexto [IndicesColumnasCategoricas [I]] := IndicesNivelColumnasCategoricas [I];

    // Columnas con vector de datos
    for I := IndicesColumnasVector.Bajo to IndicesColumnasVector.Alto do
      Contexto [IndicesColumnasVector [I]] := IndicesNivelColumnasVector [I];

    // Inicializar arreglo de longitudes maximas de valores de columnas
    FLongitudMaximaValoresColumna := TArregloEnteros.Create(FDatos.Columnas.Count + 1);
    FLongitudMaximaValoresColumna.PonerA(0);

    // Generar la matriz de valores
    I := 1;
    IndiceFila := 0;
    while (I <= FCantidadObservaciones) do
    begin
      // Verificar si se debe incluir la observacion
      IncluirObservacion := FFuncionCaracteristica.Valor;

      if IncluirObservacion then
      begin
        // Incluir la observacione en la matriz de valores y recalcular la
        // longitud maxima de valores de columnas.

        // Columnas numericas
        for J := IndicesColumnasNumericas.Bajo to IndicesColumnasNumericas.Alto do
        begin
          FValores [IndiceFila, IndicesColumnasNumericas [J]] := FloatToStr(Contexto [IndicesColumnasNumericas [J]]);
          if (FLongitudMaximaValoresColumna [IndicesColumnasNumericas [J]] < Length(FValores [IndiceFila, IndicesColumnasNumericas [J]])) then
            FLongitudMaximaValoresColumna [IndicesColumnasNumericas [J]] := Length(FValores [IndiceFila, IndicesColumnasNumericas [J]]);
        end;

        // Columnas categoricas
        for J := IndicesColumnasCategoricas.Bajo to IndicesColumnasCategoricas.Alto do
        begin
          FValores [IndiceFila, IndicesColumnasCategoricas [J]] := (FDatos.Columnas [IndicesColumnasCategoricas [J]] as TColumnaDatosCategoricos).Niveles [IndicesNivelColumnasCategoricas [J]];
          if (FLongitudMaximaValoresColumna [IndicesColumnasCategoricas [J]] < Length(FValores [IndiceFila, IndicesColumnasCategoricas [J]])) then
            FLongitudMaximaValoresColumna [IndicesColumnasCategoricas [J]] := Length(FValores [IndiceFila, IndicesColumnasCategoricas [J]]);
        end;

        // Columnas con vector de datos
        for J := IndicesColumnasVector.Bajo to IndicesColumnasVector.Alto do
        begin
          FValores [IndiceFila, IndicesColumnasVector [J]] := (FDatos.Columnas [IndicesColumnasVector [J]] as TColumnaVectorDatos).Valores [IndicesNivelColumnasVector [J]];
          if (FLongitudMaximaValoresColumna [IndicesColumnasVector [J]] < Length(FValores [IndiceFila, IndicesColumnasVector [J]])) then
            FLongitudMaximaValoresColumna [IndicesColumnasVector [J]] := Length(FValores [IndiceFila, IndicesColumnasVector [J]]);
        end;

        Inc(IndiceFila);
      end;

      // Actualizar contexto

      // Actualizar indices de columnas categoricas
      Listo := False;
      J := IndicesNivelColumnasCategoricas.Bajo;
      while (J <= IndicesNivelColumnasCategoricas.Alto) and (not Listo) do
      begin
        if (IndicesNivelColumnasCategoricas [J] < (FDatos.Columnas [IndicesColumnasCategoricas [J]] as TColumnaDatosCategoricos).Niveles.Alto) then
        begin
          IndicesNivelColumnasCategoricas [J] := IndicesNivelColumnasCategoricas [J] + 1;
          Listo := True;
        end
        else
          IndicesNivelColumnasCategoricas [J] := (FDatos.Columnas [IndicesColumnasCategoricas [J]] as TColumnaDatosCategoricos).Niveles.Bajo;
        
        Inc(J);
      end;

      // Actualizar el indice de columnas de vectores de datos
      IndicesNivelColumnasVector.PonerA(I + 1);

      // Actualizar valor de columnas numericas
      for J := IndicesColumnasNumericas.Bajo to IndicesColumnasNumericas.Alto do
      begin
        if FDatos.Columnas [IndicesColumnasNumericas [J]] is TColumnaDatosReales then
          Contexto [IndicesColumnasNumericas [J]] := (FDatos.Columnas [IndicesColumnasNumericas [J]] as TColumnaDatosReales).Expresion.Valor
        else
          Contexto [IndicesColumnasNumericas [J]] := (FDatos.Columnas [IndicesColumnasNumericas [J]] as TColumnaDatosEnteros).Expresion.Valor;
      end;

      // Actualizar valor de columnas categoricas
      for J := IndicesColumnasCategoricas.Bajo to IndicesColumnasCategoricas.Alto do
        Contexto [IndicesColumnasCategoricas [J]] := IndicesNivelColumnasCategoricas [J];

      // Actualizar el valor de las columnas con vectores de datos
      for J := IndicesColumnasVector.Bajo to IndicesColumnasVector.Alto do
        Contexto [IndicesColumnasVector [J]] := IndicesNivelColumnasVector [J];

      Inc(I);
    end;

    // Reasignar contexto en expresiones
    for I := IndicesColumnasNumericas.Bajo to IndicesColumnasNumericas.Alto do
    begin
      if (FDatos.Columnas [IndicesColumnasNumericas [I]] is TColumnaDatosReales) then
        (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosReales).Expresion.Contexto := nil
      else if (FDatos.Columnas [IndicesColumnasNumericas [I]] is TColumnaDatosEnteros) then
        (FDatos.Columnas [IndicesColumnasNumericas [I]] as TColumnaDatosEnteros).Expresion.ExpresionReal.Contexto := nil;
    end;
    FFuncionCaracteristica.AsignarContexto(nil);

    // Inicializar el resto del procedimiento (la mayor parte del codigo se
    // extrajo del metodo de inicializacion del procedimiento de ajuste.)

    // Construir matriz de indices de niveles a partir de la lista de niveles por variable
    if not Assigned(FMatrizNivelesVariablesClasificacion) then
      FMatrizNivelesVariablesClasificacion := TArregloArreglosStrings.Create(FModelo.VariablesClasificacion.Count);

    for I := FMatrizNivelesVariablesClasificacion.Bajo to FMatrizNivelesVariablesClasificacion.Alto do
    begin
      J := FDatos.Columnas.IndexOf(FModelo.VariablesClasificacion [I]);

      if (FDatos.Columnas [J] is TColumnaDatosCategoricos) then
        FMatrizNivelesVariablesClasificacion.Arreglos [I] := TArregloStrings.Create((FDatos.Columnas [J] as TColumnaDatosCategoricos).Niveles)
      else
      begin
        Niveles := FMatrizNivelesVariablesClasificacion.Arreglos [I];
        FreeAndNil(Niveles);

        Niveles := TArregloStrings.Create(0);

        for K := FValores.Bajo to FValores.Alto do
        begin
          if Niveles.IndiceDe(FValores [K, J]) = -1 then
          begin
            Niveles.Dimension := Niveles.Dimension + 1;
            Niveles [Niveles.Alto] := FValores [K, J];
          end;
        end;

        FMatrizNivelesVariablesClasificacion.Arreglos [I] := Niveles;
      end;
    end;

    // Verificar si el modelo posee efectos fijos
    if FModelo.TieneEfectosFijos then
    begin
      // El modelo posee efectos fijos, construir informacion de tiempo de ejecucion
      FreeAndNil(FInformacionTiempoEjecucionEfectosFijos);
      FInformacionTiempoEjecucionEfectosFijos := ConstruirInformacionTiempoEjecucionEfectosFijos(FModelo.EfectosFijos);

      // Construir matriz de diseno efectos fijos y su traspuesta
      ConstruirMatrizDisenoEfectosFijos(FX);
      FreeAndNil(FXT);
      FXT := FX.Transpuesta;
      FRangoX := FX.Rango;

      // Validar y construir beta
      FreeAndNil(FBeta);
      if FModelo.Opciones.BuscarAsignar(I, 'beta') then
      begin
        if ((FModelo.Opciones [I] as TOpcionParametroTObject).Parametro is TCsExpresionBooleana) then
        begin
          // Se suministro Beta como expresion

          Expresion := (FModelo.Opciones [I] as TOpcionParametroTObject).Parametro as TCsExpresion;

          // Validar expresion
          if FDatos.ValidarExpresion(Expresion, [0 .. FDatos.Columnas.Count - 1], Bitacora) then
          begin
            FBeta := TUaMatriz.Create(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno, 1);
            for I := 1 to FBeta.CantidadFilas do
              FBeta [I, 1] := Expresion.Valor;
          end
          else
          begin
            Result := False;
            Exit;
          end;
        end
        else
        begin
          // Se suministro Beta como vector

          if ((FModelo.Opciones [I] as TOpcionParametroTObject).Parametro as TUaVector).Dimension < FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno then
          begin
            Bitacora.Add('La dimension de Beta suministrado (' + IntToStr(((FModelo.Opciones [I] as TOpcionParametroTObject).Parametro as TUaVector).Dimension) +  ') no coincide con la cantidad de columnas de la matriz de diseno (' + IntToStr(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno) + ').');
            Result := False;
            Exit;
          end
          else
          begin
            FBeta := TUaMatriz.Create(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno, 1);
            FBeta.Columna [1] := (FModelo.Opciones [I] as TOpcionParametroTObject).Parametro as TUaVector;
          end;
        end;
      end
      else
      begin
        FBeta := TUaMatriz.Create(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno, 1);
        FBeta.Asignar(1.0);
      end;

      // Calcular X*Beta
      FXBeta := FX.Producto(FBeta);
    end;

    // Inicializar variables de clasificacion auxiliares
    FVariablesClasificacionAuxiliares.Clear;

    // Inicializar informacion sobre criterios
    FInformacionTiempoEjecucionCriterios.Clear;

    // Verificar si el modelo posee efectos aleatorios
    FInformacionTiempoEjecucionEfectosAleatorios.Clear;
    if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
    begin
      // Se definieron efectos aleatorios

      if FModelo.TieneEfectosAleatorios then
      begin
        // Construir informacion tiempo ejecucion de efectos aleatorios
        FInformacionTiempoEjecucionEfectosAleatorios.Add(ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios(FModelo.EfectosAleatorios));
      end;

      if FModelo.TieneEfectosAleatoriosGrupos then
      begin
        // Construir lista informacion tiempo ejecucion grupos efectos aleatorios
        for I := 0 to FModelo.GruposEfectosAleatorios.Count - 1 do
          FInformacionTiempoEjecucionEfectosAleatorios.Add(ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios(FModelo.GruposEfectosAleatorios [I]));
      end;

      // Calcular la cantidad de parametros y la dimension de la matriz de covarianza
      FCantidadParametrosEfectosAleatorios := 0;
      FCantidadFilasMatrizCovarianzaEfectosAleatorios := 0;
      for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
      begin
        FCantidadParametrosEfectosAleatorios := FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Dimension;
        FCantidadFilasMatrizCovarianzaEfectosAleatorios := FCantidadFilasMatrizCovarianzaEfectosAleatorios + FInformacionTiempoEjecucionEfectosAleatorios [I].CantidadFilasMatrizCovarianza;
      end;

      // Construir matriz de diseno de efectos aleatorios y su traspuesta
      ConstruirMatrizDisenoEfectosAleatorios(FZ);
      FreeAndNil(FZT);
      FZT := FZ.Transpuesta;
    end
    else
    begin
      // No se definieron efectos aleatorios

      // Inicializar la cantidad de parametros y la dimension de la matriz de covarianza
      FCantidadParametrosEfectosAleatorios := 0;
      FCantidadFilasMatrizCovarianzaEfectosAleatorios := 0;
    end;

    // Construir informacion de tiempo de ejecucion sobre el error
    FreeAndNil(FInformacionTiempoEjecucionError);
    FInformacionTiempoEjecucionError := ConstruirInformacionTiempoEjecucionError(FModelo.Error);

    // Inicializar parametros
    FreeAndNil(FValoresParametrosEfectosAleatorios);
    FValoresParametrosEfectosAleatorios := TUaVector.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    I := 1;
    for J := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
    begin
      FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Valores := FValoresParametrosEfectosAleatorios;
      for K := FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Alto do
      begin
        FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Indices [K] := I;
        Inc(I);
      end;
    end;
    FInformacionTiempoEjecucionError.Parametros.Valores := FValoresParametrosEfectosAleatorios;
    for J := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
    begin
      FInformacionTiempoEjecucionError.Parametros.Indices [J] := I;
      Inc(I);
    end;

    // Inicializar valores iniciales con los valores de configuracion
    I := 1;
    for J := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
    begin
      for K := FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Alto do
      begin
        // Si no fue provisto un valor inicial, utilizar el valor por defecto
        if FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].TieneValorInicial then
          FValoresParametrosEfectosAleatorios [I] := FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.ValoresIniciales [K]
        else
          FValoresParametrosEfectosAleatorios [I] := CValorInicialParametros;

        Inc(I);
      end;
    end;
    for J := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
    begin
      // Si no fue provisto un valor inicial, utilizar el valor por defecto
      if FInformacionTiempoEjecucionError.Parametros.InformacionParametros [J].TieneValorInicial then
        FValoresParametrosEfectosAleatorios [I] := FInformacionTiempoEjecucionError.Parametros.ValoresIniciales [J]
      else
        FValoresParametrosEfectosAleatorios [I] := CValorInicialParametros;

      Inc(I);
    end;

    // Limpiar salida
    FSalida.Clear;

    Result := True;
  except on E: Exception do
    begin
      Result := False;
      Bitacora.Add('TProcedimiento.Inicializar: ' + E.Message);
    end;
  end;

  FreeAndNil(Contexto);
  FreeAndNil(IndicesColumnasNumericas);
  FreeAndNil(IndicesColumnasCategoricas);
  FreeAndNil(IndicesNivelColumnasCategoricas);
end { TProcedimiento.Inicializar };

function TProcedimiento.ConstruirInformacionTiempoEjecucionCriterio(const Criterio: TEfecto): TInformacionTiempoEjecucionCriterio;
var
  I, J, K, L: Integer;
  Variables: TStrings;
  Niveles: TArregloStrings;
  Indices: TArregloEnteros;
  Informacion: TInformacionTiempoEjecucionCriterio;
  ArreglosIndicesNiveles: TListaArreglosEnteros;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionCriterio }
  // Inicializar referencias
  Niveles := nil;
  Variables := nil;
  Indices := nil;
  Informacion := nil;
  ArreglosIndicesNiveles := nil;

  Informacion := TInformacionTiempoEjecucionCriterio.Create(Criterio);

  // Construir lista de variables
  Variables := Informacion.Efecto.ListaVariables;

  // Construir arreglos de indices a columnas y a lista de niveles del procedimiento
  Informacion.IndicesColumnasVariablesClasificacion := TArregloEnteros.Create(Variables.Count);
  Informacion.IndicesNivelesVariablesClasificacion := TArregloEnteros.Create(Variables.Count);
  for I := 0 to Variables.Count - 1 do
  begin
    J := FModelo.Datos.Columnas.IndexOf(Variables [I]);
    Informacion.IndicesColumnasVariablesClasificacion [I] := J;

    K := FModelo.VariablesClasificacion.IndexOf(Variables [I]);

    // Verificar la existencia de covariables y construir la variable de clasificacion auxiliar correspondiente
    if (FModelo.VariablesClasificacion.IndexOf(Variables [I]) = -1) and
       (FVariablesClasificacionAuxiliares.IndexOf(Variables [I]) = -1) then
    begin
      // Construir una variable de clasificacion auxiliar
      FVariablesClasificacionAuxiliares.Add(Variables [I]);

      // Agregar niveles al procedimiento
      Niveles := TArregloStrings.Create;

      for L := FValores.Bajo to FValores.Alto do
      begin
        if Niveles.IndiceDe(FValores [L, J]) = -1 then
        begin
          Niveles.Dimension := Niveles.Dimension + 1;
          Niveles [Niveles.Alto] := FValores [L, J];
        end;
      end;

      // Ingresar los niveles a la matriz de niveles del procedimiento
      FMatrizNivelesVariablesClasificacion.Dimension := FMatrizNivelesVariablesClasificacion.Dimension + 1;
      FMatrizNivelesVariablesClasificacion.Arreglos [FMatrizNivelesVariablesClasificacion.Alto] := Niveles;
    end;

    Informacion.IndicesNivelesVariablesClasificacion [I] := K;
  end;

  // Liberar lista de variables
  FreeAndNil(Variables);

  // Construir arreglo de indices de niveles y arreglo de indices de nivel por observacion y observacion por nivel
  ArreglosIndicesNiveles := TListaArreglosEnteros.Create;
  Informacion.IndicesNivelesObservaciones := TArregloEnteros.Create(FValores.CantidadFilas);
  for I := FValores.Bajo to FValores.Alto do
  begin
    Indices := TArregloEnteros.Create(Informacion.IndicesColumnasVariablesClasificacion.Dimension);

    // Leer los indices de las variables del criterio para la observacion
    for J := Informacion.IndicesColumnasVariablesClasificacion.Bajo to Informacion.IndicesColumnasVariablesClasificacion.Alto do
      Indices [J] := FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [J]].IndiceDe(FValores [I, Informacion.IndicesColumnasVariablesClasificacion [J]]);

    // Verificar si el arreglo de indices se encuentra en la lista de indices de niveles
    K := ArreglosIndicesNiveles.IndexOf(Indices);

    if K = -1 then
    begin
      // El nivel no se encuentra en la lista, agregarlo a la lista
      K := ArreglosIndicesNiveles.Add(Indices);
    end;

    // Almacenar el indice del nivel para la observacion
    Informacion.IndicesNivelesObservaciones [I] := K;
  end;

  // Construir el arreglo de indices de niveles a partir de la lista y liberarla
  Informacion.IndicesNiveles := TArregloArreglosEnteros.Create(ArreglosIndicesNiveles);
  FreeAndNil(ArreglosIndicesNiveles);

  // Construir arreglo de observaciones por nivel
  Informacion.IndicesObservacionesNiveles := TArregloArreglosEnteros.Create(Informacion.IndicesNiveles.Dimension);
  for I := Informacion.IndicesNivelesObservaciones.Bajo to Informacion.IndicesNivelesObservaciones.Alto do
  begin
    if not Assigned(Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]]) then
      // El arreglo no existe, crearlo
      Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]] := TArregloEnteros.Create(1)
    else
      // El arreglo existe, redimensionarlo
      Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]].Dimension := Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]].Dimension + 1;

    // Agregar el nivel
    Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]] [Informacion.IndicesObservacionesNiveles.Arreglos [Informacion.IndicesNivelesObservaciones [I]].Alto] := I;
  end;

  // Calcular cantidad de observaciones por nivel y maximo de las mismas
  Informacion.CantidadObservacionesNivel := TArregloEnteros.Create(Informacion.IndicesNiveles.Dimension);
  Informacion.CantidadMaximaObservaciones := 0;
  for I := Informacion.IndicesObservacionesNiveles.Bajo to Informacion.IndicesObservacionesNiveles.Alto do
  begin
    Informacion.CantidadObservacionesNivel [I] := Informacion.IndicesObservacionesNiveles.Arreglos [I].Dimension;
    if Informacion.CantidadObservacionesNivel [I] >= Informacion.CantidadMaximaObservaciones then
      Informacion.CantidadMaximaObservaciones := Informacion.CantidadObservacionesNivel [I];
  end;

  // Inicializar el resto de la informacion del criterio. No todos los campos heredados son utiles en este contexto
  Informacion.CantidadNiveles := Informacion.IndicesNiveles.Dimension;
  Informacion.CantidadNivelesInexistentes := 0;
  Informacion.NivelesInexistentes := nil;
  Informacion.IndicesColumnasCovariables := nil;
  Informacion.TieneVariablesClasificacion := true;
  Informacion.TieneCovariables := false;

  Result := Informacion;
end { TProcedimiento.ConstruirInformacionTiempoEjecucionCriterio };

function TProcedimiento.ConstruirInformacionTiempoEjecucionEfecto(const Efecto: TEfecto): TInformacionTiempoEjecucionEfecto;
var
  I, J, K, CantidadCombinacionesNiveles, IndiceUltimaCovariable: Integer;
  B, C, Done: Boolean;
  Informacion: TInformacionTiempoEjecucionEfecto;
  Indices: TArregloEnteros;
  Variables: TStrings;
  VariablesEfectosAnidados: TStrings;
  S: String;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionEfecto }
  // Inicializar referencias
  Informacion := nil;
  Indices := nil;
  Variables := nil;
  VariablesEfectosAnidados := nil;

  Informacion := TInformacionTiempoEjecucionEfecto.Create(Efecto);

  if not (Efecto is TIntercepto) then
  begin
    // El efecto no es intercepto

    // Construir informacion sobre la composicion del efecto
    if Efecto is TAnidamiento then
    begin
      Variables := (Efecto as TAnidamiento).Cabecera.ListaVariables;
      VariablesEfectosAnidados := (Efecto as TAnidamiento).ListaVariablesEfectosAnidados;

      // Invertir el orden de la lista de efectos anidados para indexar adecuadamente cada nivel
      for I := 0 to ((VariablesEfectosAnidados.Count - 1) div 2) do
      begin
        S := VariablesEfectosAnidados [I];
        VariablesEfectosAnidados [I] := VariablesEfectosAnidados [VariablesEfectosAnidados.Count - 1 - I];
        VariablesEfectosAnidados [VariablesEfectosAnidados.Count - 1 - I] := S;
      end;
    end
    else
    begin
      Variables := Efecto.ListaVariables;
    end;

    // Buscar covariables
    I := 0;
    IndiceUltimaCovariable := -1;
    Done := false;
    while (I <= Variables.Count - 1) and (not Done) do
    begin
      J := FModelo.VariablesClasificacion.IndexOf(Variables [I]);

      if (J = -1) then
      begin
        // Covariable
        Informacion.TieneCovariables := true;
        IndiceUltimaCovariable := I;
      end
      else
      begin
        // Variable de clasificacion (No pueden ocurrir mas covariables por construccion)
        Informacion.TieneVariablesClasificacion := true;
        Done := true;
      end;

      Inc(I);
    end;

    // Construir arreglo indices covariables
    if Informacion.TieneCovariables then
    begin
      Informacion.IndicesColumnasCovariables := TArregloEnteros.Create(IndiceUltimaCovariable + 1);

      for I := Informacion.IndicesColumnasCovariables.Bajo to Informacion.IndicesColumnasCovariables.Alto do
        Informacion.IndicesColumnasCovariables [I] := FModelo.Datos.Columnas.IndexOf(Variables [I]);

      // Eliminar covariables de la lista de variables
      I := 0;
      while (I <= IndiceUltimaCovariable) do
      begin
        // Eliminar la primera variable (debe ser covariable)
        Variables.Delete(0);
        Inc(I);
      end;
    end;

    // De ser necesario, invertir el orden de la lista de variables para indexar adecuadamente cada nivel
    if Variables.Count > 0 then
    begin
      for I := 0 to ((Variables.Count - 1) div 2) do
      begin
        S := Variables [I];
        Variables [I] := Variables [Variables.Count - 1 - I];
        Variables [Variables.Count - 1 - I] := S;
      end;
    end;

    // Verificar que el efecto sea un anidamiento (poseen variables clasificacion por construccion)
    if (Efecto is TAnidamiento) then
    begin
      // Concatenar variables de efectos anidados para construir arreglos de indices
      Variables.AddStrings(VariablesEfectosAnidados);
      Informacion.TieneVariablesClasificacion := true;

      // Liberar lista de variables de efectos anidados
      FreeAndNil(VariablesEfectosAnidados);
    end;

    // Construir arreglos de indices variables clasificacion y arreglo indices de niveles
    if Informacion.TieneVariablesClasificacion then
    begin
      Informacion.IndicesColumnasVariablesClasificacion := TArregloEnteros.Create(Variables.Count);
      Informacion.IndicesNivelesVariablesClasificacion := TArregloEnteros.Create(Variables.Count);

      for I := 0 to Variables.Count - 1 do
      begin
        Informacion.IndicesColumnasVariablesClasificacion [I] := FModelo.Datos.Columnas.IndexOf(Variables [I]);
        Informacion.IndicesNivelesVariablesClasificacion [I] := FModelo.VariablesClasificacion.IndexOf(Variables [I]);
      end;

      // Calcular cantidad de combinaciones de niveles
      CantidadCombinacionesNiveles := 1;
      for I := 0 to Informacion.IndicesNivelesVariablesClasificacion.Dimension - 1 do
        CantidadCombinacionesNiveles := CantidadCombinacionesNiveles * FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [I]].Dimension;

      // Construir e inicializar arreglo de columnas nulas
      SetLength(Informacion.NivelesInexistentes, CantidadCombinacionesNiveles);
      for I := Low(Informacion.NivelesInexistentes) to High(Informacion.NivelesInexistentes) do
        Informacion.NivelesInexistentes [I] := false;

      // Construir matriz de indices de niveles
      Informacion.IndicesNiveles := TArregloArreglosEnteros.Create(CantidadCombinacionesNiveles, Informacion.IndicesColumnasVariablesClasificacion.Dimension);

      // La cantidad total de columnas (nulas incluidas) es igual a la cantidad de combinaciones de niveles
      Informacion.CantidadNiveles := CantidadCombinacionesNiveles;

      // No hace falta inicializar el primer nivel a cero (por construccion)

      // Inicializar el resto de los niveles
      for I := Informacion.IndicesNiveles.Bajo + 1 to Informacion.IndicesNiveles.Alto do
      begin
        // Copiar fila anterior
        Informacion.IndicesNiveles.Arreglos [I] := Informacion.IndicesNiveles.Arreglos [I - 1].Clonar;
        Indices := Informacion.IndicesNiveles.Arreglos [I];

        // Actualizar los indices correspondientes
        J := Informacion.IndicesColumnasVariablesClasificacion.Bajo;
        Done := false;
        while (J <= Informacion.IndicesColumnasVariablesClasificacion.Alto) and (not Done) do
        begin
          if (Indices [J] < FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [J]].Alto) then
          begin
            // Incrementar el indice de la columna actual y continuar en la fila siguiente
            Indices [J] := Indices [J] + 1;
            Done := true;
          end
          else
          begin
            // No es posible incrementar el indice actual, verificar posible ultimo indice
            if (J <> Informacion.IndicesColumnasVariablesClasificacion.Alto) then
            begin
              // No es el ultimo indice, reinicializar el indice actual
              Indices [J] := 0;
            end
            else
            begin
              // Es el ultimo indice, continuar con la fila siguiente
              Done := true;
            end;
          end;

          Inc(J);
        end;
      end;

      // Construir arreglo de indicadores de columnas nulas
      for I := Informacion.IndicesNiveles.Bajo to Informacion.IndicesNiveles.Alto do
      begin
        B := true;
        J := FValores.Bajo;
        while (J <= FValores.Alto) and B do
        begin
          K := Informacion.IndicesColumnasVariablesClasificacion.Bajo;
          C := true;
          while (K <= Informacion.IndicesColumnasVariablesClasificacion.Alto) and C do
          begin
            C := C and (FValores [J, Informacion.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [K]] [Informacion.IndicesNiveles [I, K]]);
            Inc(K);
          end;

          // Si C es verdadero, todos los indices coincidieron y por lo tanto no es una columna nula
          B := not C;
          Inc(J);
        end;

        Informacion.NivelesInexistentes [I] := B;
        if B then
          Inc(Informacion.CantidadNivelesInexistentes);
      end;
    end
    else
    begin
      // El efecto solo posee covariables (por construccion), inicializar solo los valores necesarios

      Informacion.CantidadNiveles := 1;
      Informacion.CantidadNivelesInexistentes := 0;
      Informacion.IndicesNiveles := TArregloArreglosEnteros.Create(1);
      SetLength(Informacion.NivelesInexistentes, 1);
      Informacion.NivelesInexistentes [Low(Informacion.NivelesInexistentes)] := false;
    end;
  end
  else
  begin
    // El efecto es un intercepto, inicializar solo la informacion necesaria

    Informacion.CantidadNiveles := 1;
    Informacion.CantidadNivelesInexistentes := 0;
    Informacion.IndicesNiveles := TArregloArreglosEnteros.Create(1);
    SetLength(Informacion.NivelesInexistentes, 1);
    Informacion.NivelesInexistentes [Low(Informacion.NivelesInexistentes)] := false;
  end;

  Result := Informacion;
end { TProcedimiento.ConstruirInformacionTiempoEjecucionEfecto };

function TProcedimiento.ConstruirInformacionTiempoEjecucionEfectosFijos(const Grupo: TGrupoEfectos): TInformacionTiempoEjecucionEfectosFijos;
var
  I, IndiceEfecto: Integer;
  Informacion: TInformacionTiempoEjecucionEfectosFijos;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionEfectosFijos }
  Informacion := TInformacionTiempoEjecucionEfectosFijos.Create(FModelo.EfectosFijos);

  // Construir arreglo de informacion tiempo ejecucion efectos
  IndiceEfecto := 0;

  // Verificar inclusion de intercepto
  if Informacion.IncluirIntercepto then
  begin
    // Los efectos fijos incluyen intercepto

    Informacion.InformacionTiempoEjecucionEfectos := TArregloInformacionTiempoEjecucionEfectos.Create(Informacion.Efectos.Count + 1);
    Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto] := ConstruirInformacionTiempoEjecucionEfecto(TIntercepto.Create);
    Inc(IndiceEfecto);
  end
  else
  begin
    // Los efectos fijos no incluyen intercepto

    Informacion.InformacionTiempoEjecucionEfectos := TArregloInformacionTiempoEjecucionEfectos.Create(Informacion.Efectos.Count);
  end;

  for I := 0 to Informacion.Efectos.Count - 1 do
  begin
    Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto] := ConstruirInformacionTiempoEjecucionEfecto(Informacion.Efectos [I]);
    Inc(IndiceEfecto);
  end;

  // Calcular dimensiones de matrices
  SetLength(Informacion.CantidadColumnasSubmatrizDisenoEfecto, Informacion.InformacionTiempoEjecucionEfectos.Dimension);
  Informacion.CantidadColumnasMatrizDiseno := 0;
  if Informacion.GenerarColumnasNulas then
  begin
    for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
    begin
      Informacion.CantidadColumnasSubmatrizDisenoEfecto [I] := Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles;
      Informacion.CantidadColumnasMatrizDiseno := Informacion.CantidadColumnasMatrizDiseno + Informacion.CantidadColumnasSubmatrizDisenoEfecto [I];
    end;
  end
  else
  begin
    for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
    begin
      Informacion.CantidadColumnasSubmatrizDisenoEfecto [I] := Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;
      Informacion.CantidadColumnasMatrizDiseno := Informacion.CantidadColumnasMatrizDiseno + Informacion.CantidadColumnasSubmatrizDisenoEfecto [I];
    end;
  end;

  Result := Informacion;
end { TProcedimiento.ConstruirInformacionTiempoEjecucionEfectosFijos };

function TProcedimiento.ConstruirInformacionTiempoEjecucionError(const Error: TListaOpciones): TInformacionTiempoEjecucionError;
var
  I, J, K, IndiceParametro, CantidadParametros, Orden, Dimension: Integer;
  Informacion: TInformacionTiempoEjecucionError;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  EstructuraOrden: TEstructuraOrden;
  InformacionParametroIndice: TInformacionParametroIndice;
  ListaParametros: TListaInformacionParametroIndice;
  CantidadObservacionesNivel: TArregloEnteros;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionError }
  // Inicializar referencias
  Informacion := nil;
  InformacionCriterio := nil;
  EstructuraOrden := nil;
  InformacionParametroIndice := nil;
  ListaParametros := nil;
  CantidadObservacionesNivel := nil;

  // Inicializar informacion
  Informacion := TInformacionTiempoEjecucionError.Create(Error);

  // Verificar la existencia de unidad experimental
  if Informacion.TieneUnidadExperimental then
  begin
    // Buscar, o construir informacion de tiempo de ejecucion sobre el criterio
    I := FInformacionTiempoEjecucionCriterios.IndexOf(Informacion.UnidadExperimental);

    if I = -1 then
    begin
      // La informacion sobre el criterio no existe, construirla
      InformacionCriterio := ConstruirInformacionTiempoEjecucionCriterio(Informacion.UnidadExperimental);
      FInformacionTiempoEjecucionCriterios.Add(InformacionCriterio);
    end
    else
      // La informacion sobre el criterio existe, acceder a ella
      InformacionCriterio := FInformacionTiempoEjecucionCriterios [I] as TInformacionTiempoEjecucionCriterio;

    Informacion.InformacionTiempoEjecucionUnidadExperimental := InformacionCriterio;
  end;

  // Verificar la existencia de criterio agrupamiento unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    // Buscar, o construir informacion de tiempo de ejecucion sobre el criterio
    I := FInformacionTiempoEjecucionCriterios.IndexOf(Informacion.CriterioAgrupamientoUnidadExperimental);

    if I = -1 then
    begin
      // La informacion sobre el criterio no existe, construirla
      InformacionCriterio := ConstruirInformacionTiempoEjecucionCriterio(Informacion.CriterioAgrupamientoUnidadExperimental);
      FInformacionTiempoEjecucionCriterios.Add(InformacionCriterio);
    end
    else
      // La informacion sobre el criterio existe, acceder a ella
      InformacionCriterio := FInformacionTiempoEjecucionCriterios [I] as TInformacionTiempoEjecucionCriterio;

    Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental := InformacionCriterio;
  end;

  // Verificar la existencia de criterio ordenamiento
  if Informacion.TieneCriterioOrdenamiento then
  begin
    // Buscar, o construir informacion de tiempo de ejecucion sobre el criterio
    I := FInformacionTiempoEjecucionCriterios.IndexOf(Informacion.CriterioOrdenamiento);

    if I = -1 then
    begin
      // La informacion sobre el criterio no existe, construirla
      InformacionCriterio := ConstruirInformacionTiempoEjecucionCriterio(Informacion.CriterioOrdenamiento);
      FInformacionTiempoEjecucionCriterios.Add(InformacionCriterio);
    end
    else
      // La informacion sobre el criterio existe, acceder a ella
      InformacionCriterio := FInformacionTiempoEjecucionCriterios [I] as TInformacionTiempoEjecucionCriterio;

    Informacion.InformacionTiempoEjecucionCriterioOrdenamiento := InformacionCriterio;
  end;

  // Calcular las dimensiones de las matrices (para el calculo de la cantidad de parametros)
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
    Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental.PonerA(0);

    Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
    Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental.PonerA(0);

    // Orden := 0;

    if Informacion.TieneUnidadExperimental then
    begin
      // Calcular la cantidad maxima de observaciones de un nivel de criterio agrupamiento unidad experimental por nivel unidad experimental

      // Construir arreglo de conteo de observaciones de niveles de criterio agrupamiento unidad experimental
      CantidadObservacionesNivel := TArregloEnteros.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

      for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
      begin
        CantidadObservacionesNivel.PonerA(0);

        // Contar la cantidad de observaciones de cada nivel de criterio agrupamiento
        for J := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [I].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [I].Alto do
          CantidadObservacionesNivel.Incrementar(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [I, J]]);

        // Almacenar los maximos
        for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
        begin
          if CantidadObservacionesNivel [J] > Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [J] then
          begin
            Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [J] := CantidadObservacionesNivel [J];
            Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [J] := CantidadObservacionesNivel [J];
          end;
        end;
      end;

      // Liberar arreglo de conteo
      FreeAndNil(CantidadObservacionesNivel);
    end
    else
    begin
      Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadObservacionesNivel;
      Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental.PonerA(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadObservacionesNivel);
    end;

    Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
    Dimension := 0;
  end
  else if Informacion.TieneUnidadExperimental then
    Dimension := Informacion.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones
  else
    Dimension := FValores.CantidadFilas;

  // Calcular dimension de las matriz de covarianza del grupo
  Informacion.CantidadFilasMatrizCovarianza := FValores.CantidadFilas;

  // Construir arreglo de parametros, la construccion es ad-hoc al tipo de estructura, pero en general se ordenan primero las varianzas y luego las covarianzas
  if (Informacion.Estructura.Nombre = 'componentes_varianza') then
  begin
    // Estructura Componentes de Varianza

    // Calcular la cantidad de parametros
    CantidadParametros := 1;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Nombrar los parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'cv_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Nombrar los parametros
      IndiceParametro := 0;
      Informacion.Parametros.Nombres [IndiceParametro] := 'cv';
    end;
  end
  else if (Informacion.Estructura.Nombre = 'simetria_compuesta') then
  begin
    // Estructura Simetria Compuesta

    // Calcular la cantidad de parametros
    CantidadParametros := 2;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar los nombres de los parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar los nombres de los parametros
      IndiceParametro := 0;
      Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]';
      Inc(IndiceParametro);
      Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]';
    end;
  end
  else if (Informacion.Estructura.Nombre = 'general') then
  begin
    // Estructura General

    // Calcular la cantidad de parametros

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular dimension
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Trunc(Dimension * (Dimension + 1) / 2);

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IntToStr(I) + ')';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := Trunc(Dimension * (Dimension + 1) / 2);

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(I) + ', ' + IntToStr(I) + ']';
        Inc(IndiceParametro);

        for J := I + 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
          Inc(IndiceParametro);
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'diagonal_heterogenea') then
  begin
    // Estructura Diagonal heterogenea

    // Calcular la cantidad de parametros

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular dimension
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Dimension;

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := Dimension;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(I) + ']';
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic') then
  begin
    // Estructura Factor Analytic

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular el orden adecuado de la submatriz de covarianza
        J := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];
        if EstructuraOrden.Orden < J then
          Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] := EstructuraOrden.Orden;

        // Calcular dimension y Orden
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];
        Orden := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + Dimension);

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          for K := 1 to Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular el orden adecuado de la matriz de covarianza
      if EstructuraOrden.Orden > Dimension then
        Informacion.OrdenEstructuraCovarianza := Dimension
      else
        Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

      // Calcular el orden
      Orden := Informacion.OrdenEstructuraCovarianza;

      // Calcular la cantidad de parametros
      CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + Dimension);

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if J <= I then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Ingresar los parametros correspondientes a la matriz diagonal
      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(I) + ']';
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_sin_diagonal') then
  begin
    // Estructura Factor Analytic sin Diagonal

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular el orden adecuado de la submatriz de covarianza
        J := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];
        if EstructuraOrden.Orden < J then
          Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] := EstructuraOrden.Orden;

        // Calcular dimension y orden
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];
        Orden := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          for K := 1 to Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular el orden adecuado de la matriz de covarianza
      if EstructuraOrden.Orden > Dimension then
        Informacion.OrdenEstructuraCovarianza := Dimension
      else
        Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

      // Calcular el orden
      Orden := Informacion.OrdenEstructuraCovarianza;

      // Calcular la cantidad de parametros
      CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;

      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if J <= I then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_diagonal_escalar') then
  begin
    // Estructura Factor Analytic con Diagonal Escalar

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular el orden adecuado de la submatriz de covarianza
        J := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];
        if EstructuraOrden.Orden < J then
          Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] := EstructuraOrden.Orden;

        // Calcular dimension y orden
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];
        Orden := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + 1);

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          for K := 1 to Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular el orden adecuado de la matriz de covarianza
      if EstructuraOrden.Orden > Dimension then
        Informacion.OrdenEstructuraCovarianza := Dimension
      else
        Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

      // Calcular el orden
      Orden := Informacion.OrdenEstructuraCovarianza;

      // Calcular la cantidad de parametros
      CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + 1);

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;

      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if J <= I then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Ingresar los parametros correspondientes a la matriz diagonal
      Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D';
    end;
  end
  else if (Informacion.Estructura.Nombre = 'bandeada') then
  begin
    // Estructura Bandeada

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Calcular la cantidad de parametros
      CantidadParametros := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Calcular el orden adecuado de la submatriz de covarianza
        J := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];
        if EstructuraOrden.Orden < J then
          Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] := EstructuraOrden.Orden;

        // Calcular dimension y orden
        Dimension := Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I];
        Orden := Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I];

        // Almacenar la cantidad de parametros por grupo
        Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

        // Sumar al total
        CantidadParametros := CantidadParametros + Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end;

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            if Abs(J - K) < Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Calcular el orden adecuado de la matriz de covarianza
      if EstructuraOrden.Orden > Dimension then
        Informacion.OrdenEstructuraCovarianza := Dimension
      else
        Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

      // Calcular el orden
      Orden := Informacion.OrdenEstructuraCovarianza;

      // Calcular la cantidad de parametros
      CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

      // Construir arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar nombres de parametros
      IndiceParametro := 0;
      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(I) + ', ' + IntToStr(I) + ']';
        Inc(IndiceParametro);

        for J := I + 1 to Dimension do
        begin
          if Abs(I - J) < Orden then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'autoregresiva') then
  begin
    // Estructura Autoregresiva de primer orden

    // Calcular la cantidad de parametros
    CantidadParametros := 2;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar los nombres de los parametros
      IndiceParametro := 0;
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      // Construir el arreglo de parametros
      Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

      // Ingresar los nombres de los parametros
      IndiceParametro := 0;
      Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]';
      Inc(IndiceParametro);
      Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]';
    end;
  end;

  // Ingresar valores de parametros desde la configuracion del modelo
  if Informacion.Opciones.BuscarAsignar(I, 'parametros') then
  begin
    ListaParametros := (Informacion.Opciones [I] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

    J := 0;
    while (J <= ListaParametros.Count - 1) and (J <= Informacion.Parametros.Alto) do
    begin
      InformacionParametroIndice := ListaParametros [J];

      Informacion.Parametros.InformacionParametros [J].TieneValorInicial := InformacionParametroIndice.TieneValorInicial;
      Informacion.Parametros.InformacionParametros [J].ValorInicial := InformacionParametroIndice.ValorInicial;

      Inc(J);
    end;
  end;

  Result := Informacion;
end { TProcedimiento.ConstruirInformacionTiempoEjecucionError };

function TProcedimiento.ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios(const Grupo: TGrupoEfectos): TInformacionTiempoEjecucionGrupoEfectosAleatorios;
var
  I, J, K, IndiceEfecto, IndiceParametro, CantidadParametros, Orden, Dimension: Integer;
  Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  InformacionParametroIndice: TInformacionParametroIndice;
  ListaParametros: TListaInformacionParametroIndice;
  EstructuraOrden: TEstructuraOrden;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios }
  // Inicializar referencias
  Informacion := nil;
  InformacionCriterio := nil;
  InformacionParametroIndice := nil;
  ListaParametros := nil;
  EstructuraOrden := nil;

  // Inicializar informacion
  Informacion := TInformacionTiempoEjecucionGrupoEfectosAleatorios.Create(Grupo);

  // Verificar la existencia de unidad experimental
  if Informacion.TieneUnidadExperimental then
  begin
    // Buscar, o construir informacion de tiempo de ejecucion sobre el criterio
    I := FInformacionTiempoEjecucionCriterios.IndexOf(Informacion.UnidadExperimental);

    if I = -1 then
    begin
      // La informacion sobre el criterio no existe, construirla
      InformacionCriterio := ConstruirInformacionTiempoEjecucionCriterio(Informacion.UnidadExperimental);
      FInformacionTiempoEjecucionCriterios.Add(InformacionCriterio);
    end
    else
      // La informacion sobre el criterio existe, acceder a ella
      InformacionCriterio := FInformacionTiempoEjecucionCriterios [I] as TInformacionTiempoEjecucionCriterio;

    Informacion.InformacionTiempoEjecucionUnidadExperimental := InformacionCriterio;
  end;

  // Verificar la existencia de criterio agrupamiento unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    // Buscar, o construir informacion de tiempo de ejecucion sobre el criterio
    I := FInformacionTiempoEjecucionCriterios.IndexOf(Informacion.CriterioAgrupamientoUnidadExperimental);

    if I = -1 then
    begin
      // La informacion sobre el criterio no existe, construirla
      InformacionCriterio := ConstruirInformacionTiempoEjecucionCriterio(Informacion.CriterioAgrupamientoUnidadExperimental);
      FInformacionTiempoEjecucionCriterios.Add(InformacionCriterio);
    end
    else
      // La informacion sobre el criterio existe, acceder a ella
      InformacionCriterio := FInformacionTiempoEjecucionCriterios [I] as TInformacionTiempoEjecucionCriterio;

    Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental := InformacionCriterio;
  end;

  // Construir arreglo de informacion tiempo ejecucion efectos

  IndiceEfecto := 0;

  // Verificar inclusion de intercepto
  if Informacion.IncluirIntercepto then
  begin
    // El grupo de efectos aleatorios incluye intercepto

    Informacion.InformacionTiempoEjecucionEfectos := TArregloInformacionTiempoEjecucionEfectos.Create(Informacion.Efectos.Count + 1);
    Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto] := ConstruirInformacionTiempoEjecucionEfecto(TIntercepto.Create);
    Inc(IndiceEfecto);
  end
  else
  begin
    // El grupo de efectos aleatorios no incluye intercepto

    Informacion.InformacionTiempoEjecucionEfectos := TArregloInformacionTiempoEjecucionEfectos.Create(Informacion.Efectos.Count);
  end;

  for I := 0 to Informacion.Efectos.Count - 1 do
  begin
    Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto] := ConstruirInformacionTiempoEjecucionEfecto(Informacion.Efectos [I]);
    Inc(IndiceEfecto);
  end;

  // Calcular las dimensiones de las matrices de diseno del grupo
  Informacion.CantidadColumnasEfectos := 0;
  for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
  begin
    if Informacion.GenerarColumnasNulas then
      Informacion.CantidadColumnasEfectos := Informacion.CantidadColumnasEfectos + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles
    else
      Informacion.CantidadColumnasEfectos := Informacion.CantidadColumnasEfectos + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;
  end;

  Dimension := Informacion.CantidadColumnasEfectos;

  // Calcular dimension de las matriz de covarianza del grupo
  Informacion.CantidadFilasMatrizCovarianza := Informacion.CantidadColumnasEfectos;

  // Verificar definicion unidad experimental
  if Informacion.TieneUnidadExperimental then
    Informacion.CantidadFilasMatrizCovarianza := Informacion.CantidadFilasMatrizCovarianza * Informacion.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles;

  // Verificar definicion unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    Informacion.CantidadFilasMatrizCovarianza := Informacion.CantidadFilasMatrizCovarianza * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;

    // Construir arreglo de cantidades de parametros por nivel de criterio agrupamiento unidad experimental
    Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
  end;

  // Construir arreglo de parametros, la construccion es ad-hoc al tipo de estructura, pero en general se ordenan primero las varianzas y luego las covarianzas
  if (Informacion.Estructura.Nombre = 'componentes_varianza') then
  begin
    // Estructura Componentes de Varianza

    // Calcular la cantidad de parametros
    CantidadParametros := Informacion.InformacionTiempoEjecucionEfectos.Dimension;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      // Multiplicar la cantidad de parametros por la cantidad de niveles
      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir el arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar los nombres de los parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      IndiceParametro := 0;

      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'cv_[' + Informacion.InformacionTiempoEjecucionEfectos [J].Efecto.Texto + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros
      IndiceParametro := 0;

      for I := Informacion.Parametros.Bajo to Informacion.Parametros.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'cv_[' + Informacion.InformacionTiempoEjecucionEfectos [I].Efecto.Texto + ']';
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'simetria_compuesta') then
  begin
    // Estructura Simetria Compuesta

    // Calcular la cantidad de parametros
    CantidadParametros := 2;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      // Multiplicar la cantidad de parametros por la cantidad de niveles
      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir el arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar los nombres de los parametros

    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]_grupo(' + IntToStr(I) + ')';
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros
      Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]';
      Inc(IndiceParametro);
      Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]';
    end;
  end
  else if (Informacion.Estructura.Nombre = 'general') then
  begin
    // Estructura General

    // Calcular la cantidad de parametros
    CantidadParametros := Trunc(Dimension * (Dimension + 1) / 2);

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Dimension do
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IntToStr(I) + ')';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(I) + ', ' + IntToStr(I) + ']';
        Inc(IndiceParametro);

        for J := I + 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
          Inc(IndiceParametro);
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'diagonal_heterogenea') then
  begin
    // Estructura Diagonal heterogenea

    // Calcular la cantidad de parametros
    CantidadParametros := Dimension;

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(I) + ']';
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic') then
  begin
    // Estructura Factor Analytic

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Calcular el orden adecuado de la matriz de covarianza
    if EstructuraOrden.Orden > Dimension then
      Informacion.OrdenEstructuraCovarianza := Dimension
    else
      Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

    Orden := Informacion.OrdenEstructuraCovarianza;

    // Calcular la cantidad de parametros
    CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + Dimension);

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Dimension do
        begin
          for K := 1 to Orden do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        for J := 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if J <= I then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Ingresar los parametros correspondientes a la matriz diagonal
      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(I) + ']';
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_sin_diagonal') then
  begin
    // Estructura Factor Analytic sin Diagonal

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Calcular el orden adecuado de la matriz de covarianza
    if EstructuraOrden.Orden > Dimension then
      Informacion.OrdenEstructuraCovarianza := Dimension
    else
      Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

    Orden := Informacion.OrdenEstructuraCovarianza;

    // Calcular la cantidad de parametros
    CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Dimension do
        begin
          for K := 1 to Orden do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if I <= J then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_diagonal_escalar') then
  begin
    // Estructura Factor Analytic con Diagonal Escalar

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Calcular el orden adecuado de la matriz de covarianza
    if EstructuraOrden.Orden > Dimension then
      Informacion.OrdenEstructuraCovarianza := Dimension
    else
      Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

    Orden := Informacion.OrdenEstructuraCovarianza;

    // Calcular la cantidad de parametros
    CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1) + 1);

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar los parametros correspondientes a la matriz de factores
        for J := 1 to Dimension do
        begin
          for K := 1 to Orden do
          begin
            if K <= J then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      // Ingresar los parametros correspondientes a la matriz de factores
      for I := 1 to Dimension do
      begin
        for J := 1 to Orden do
        begin
          if J <= I then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Ingresar los parametros correspondientes a la matriz diagonal
      Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D';
    end;
  end
  else if (Informacion.Estructura.Nombre = 'bandeada') then
  begin
    // Estructura Bandeada

    EstructuraOrden := Informacion.Estructura as TEstructuraOrden;

    // Calcular el orden adecuado de la matriz de covarianza
    if EstructuraOrden.Orden > Dimension then
      Informacion.OrdenEstructuraCovarianza := Dimension
    else
      Informacion.OrdenEstructuraCovarianza := EstructuraOrden.Orden;

    Orden := Informacion.OrdenEstructuraCovarianza;

    // Calcular la cantidad de parametros
    CantidadParametros := Trunc((Orden / 2) * (2 * Dimension - Orden + 1));

    // Verificar definicion de criterio agrupamiento unidad experimental para calcular la cantidad de parametros
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar nombres de parametros, por filas y por grupos de parametros
    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for J := 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IntToStr(I) + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Dimension do
          begin
            if Abs(J - K) < Orden then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IntToStr(I) + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros

      for I := 1 to Dimension do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(I) + ', ' + IntToStr(I) + ']';
        Inc(IndiceParametro);

        for J := I + 1 to Dimension do
        begin
          if Abs(I - J) < Orden then
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(I) + ', ' + IntToStr(J) + ']';
            Inc(IndiceParametro);
          end;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'autoregresiva') then
  begin
    // Estructura Autoregresiva de primer orden

    // Calcular la cantidad de parametros
    CantidadParametros := 2;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Almacenar la cantidad de parametros por grupo
      Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental.PonerA(CantidadParametros);

      // Multiplicar la cantidad de parametros por la cantidad de niveles
      CantidadParametros := CantidadParametros * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles;
    end;

    // Construir el arreglo de parametros
    Informacion.Parametros := TArregloParametros.Create(CantidadParametros);

    // Ingresar los nombres de los parametros

    IndiceParametro := 0;

    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental, nombrar los parametros en funcion de cada nivel
      for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]_grupo(' + IntToStr(I) + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]_grupo(' + IntToStr(I) + ')';
      end;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental, nombrar un solo conjunto de parametros
      Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]';
      Inc(IndiceParametro);
      Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]';
    end;
  end;

  // Ingresar valores de parametros desde la configuracion del modelo
  if Informacion.Opciones.BuscarAsignar(I, 'parametros') then
  begin
    ListaParametros := (Informacion.Opciones [I] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

    J := 0;
    while (J <= ListaParametros.Count - 1) and (J <= Informacion.Parametros.Alto) do
    begin
      InformacionParametroIndice := ListaParametros [J];

      // Asignar la tupla
      Informacion.Parametros.InformacionParametros [J].TieneValorInicial := InformacionParametroIndice.TieneValorInicial;
      Informacion.Parametros.InformacionParametros [J].ValorInicial := InformacionParametroIndice.ValorInicial;

      Inc(J);
    end;
  end;

  Result := Informacion;
end { TProcedimiento.ConstruirInformacionTiempoEjecucionGrupoEfectosAleatorios };

procedure TProcedimiento.ConstruirMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz);
var
  I, CantidadFilas, IndiceFila: Integer;
  InformacionGrupo: TInformacionTiempoEjecucionGrupoEfectosAleatorios;
begin { TProcedimiento.ConstruirMatrizCovarianzaEfectosAleatorios }
  if not Assigned(Matriz) then
  begin
    // Inicializar la matriz

    // Calcular la dimension
    CantidadFilas := 0;
    for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
      CantidadFilas := CantidadFilas + FInformacionTiempoEjecucionEfectosAleatorios [I].CantidadFilasMatrizCovarianza;

    // Construir la matriz
    Matriz := TUaMatriz.Create(CantidadFilas, CantidadFilas);
  end;

  // Limpiar la matriz
  Matriz.AsignarCero;

  // Ingresar los valores
  IndiceFila := 1;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
  begin
    InformacionGrupo := FInformacionTiempoEjecucionEfectosAleatorios [I];
    ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios(InformacionGrupo, Matriz, IndiceFila);
    IndiceFila := IndiceFila + InformacionGrupo.CantidadFilasMatrizCovarianza;
  end;
end { TProcedimiento.ConstruirMatrizCovarianzaEfectosAleatorios };

procedure TProcedimiento.ConstruirMatrizCovarianzaError(var Matriz: TUaMatriz);
var
  I, J, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna, IndiceNivelCriterioOrdenamiento, IndiceNivelCriterioOrdenamientoColumna, IndiceParametro, IndiceFila, IndiceColumna, IndiceObservacionCriterioAgrupamientoUnidadExperimentalDentroObservacionUnidadExperimental: Integer;
  M, M1: TUaMatriz;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  InformacionError: TInformacionTiempoEjecucionError;
  IndicesUltimaObservacion: TArregloEnteros;
  ArregloIndicesUltimaObservacion: TArregloArreglosEnteros;
  SubmatrizUnidadExperimental: TUaMatriz;
  SubmatricesNivelesCriterioAgrupamientoUnidadExperimental: array of TUaMatriz;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirMatrizCovarianzaError }
  // Inicializar referencias
  IndicesUltimaObservacion := nil;
  ArregloIndicesUltimaObservacion := nil;
  SubmatrizUnidadExperimental := nil;
  SubmatricesNivelesCriterioAgrupamientoUnidadExperimental := nil;

  InformacionError := FInformacionTiempoEjecucionError;

  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FValores.CantidadFilas, FValores.CantidadFilas)
  else
    // Llamada subsiguiente, limpiar
    Matriz.AsignarCero;

  // Construir matriz de covarianza
  if InformacionError.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    // Se definio criterio agrupamiento unidad experimental, construir una submatriz por nivel

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental;

    // Construir arreglo de submatrices
    SetLength(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental, InformacionCriterio.CantidadNiveles);
    for I := Low(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental) to High(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental) do
      SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [I] := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I]);

    // Construir cada submatriz, de acuerdo a la estructura
    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro];

        Inc(IndiceParametro);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro] + InformacionError.Parametros [IndiceParametro + 1];

          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
            SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, J] := InformacionError.Parametros [IndiceParametro + 1];
        end;

        IndiceParametro := IndiceParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental

      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);

          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Construir matriz auxiliar
        M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

        // Construir A
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if J <= I then
            begin
              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);

        // Sumar la matriz escalar D
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] + InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic sin Matriz Diagonal

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Construir matriz auxiliar
        M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

        // Construir A
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if J <= I then
            begin
              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic con Matriz Diagonal Escalar

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Construir matriz auxiliar
        M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

        // Construir A
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if J <= I then
            begin
              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);

        // Sumar la matriz escalar D
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] + InformacionError.Parametros [IndiceParametro];

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);

          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if Abs(I - J) < InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] then
            begin
              SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva primer orden

      // Construir la submatriz correspondiente a cada nivel del criterio de agrupamiento unidad experimental
      IndiceParametro := 0;
      for IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionCriterio.IndicesNiveles.Bajo to InformacionCriterio.IndicesNiveles.Alto do
      begin
        // Ingresar los valores de parametros
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := InformacionError.Parametros [IndiceParametro];

          R1 := InformacionError.Parametros [IndiceParametro];
          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, J] := R1;
          end;
        end;

        IndiceParametro := IndiceParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      end;
    end;

    // Construir matriz de covarianza del error

    // Verificar definicion de unidad experimental
    if InformacionError.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental y criterio agrupamiento unidad experimental

      // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental y criterio agrupamiento unidad experimental
      IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
      IndicesUltimaObservacion.PonerA(0);
      ArregloIndicesUltimaObservacion := TArregloArreglosEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles, InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
      ArregloIndicesUltimaObservacion.PonerA(0);

      // Verificar definicion de criterio de ordenamiento
      if InformacionError.TieneCriterioOrdenamiento then
      begin
        // Se definio criterio ordenamiento

        // Construir submatriz de unidad experimental
        SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones, InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones);

        // Construir la matriz de covarianza por cada nivel de la unidad experimental
        for IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Limpiar submatriz unidad experimental
          SubmatrizUnidadExperimental.AsignarCero;

          for IndiceFila := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
          begin
            IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceFila]];

            // Ingresar valor en la diagonal principal de la submatriz
            SubmatrizUnidadExperimental [IndiceFila + 1, IndiceFila + 1] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1];

            // Ingresar los valores del resto de las columnas
            J := ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1;
            for IndiceColumna := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] - 1 do
            begin
              IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceColumna]];

              // Verificar coincidencia de niveles de criterio agrupamiento unidad experimental
              if IndiceNivelCriterioAgrupamientoUnidadExperimental = IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna then
              begin
                // El nivel coincide, ingresar los valores
                SubmatrizUnidadExperimental [IndiceFila + 1, IndiceColumna + 1] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, J + 1];
                Inc(J);
              end;
            end;

            // Incrementar el indices de la ultima observacion visitada
            IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
            ArregloIndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental);
          end;

          // Ingresar los valores (reordenados) a la matriz de covarianza

          // Inicializar indices de ultima observacion
          IndicesUltimaObservacion.PonerA(0);

          for I := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
          begin
            IndiceFila := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];
            IndiceNivelCriterioOrdenamiento := InformacionError.InformacionTiempoEjecucionCriterioOrdenamiento.IndicesNivelesObservaciones [IndiceFila];

            // Se reordena por el modulo por la cantidad de observaciones del nivel de unidad experimental
            if IndiceNivelCriterioOrdenamiento > InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] then
              IndiceNivelCriterioOrdenamiento := IndiceNivelCriterioOrdenamiento mod InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental];

            // Ingresar valor de la diagonal principal
            Matriz [IndiceFila + 1, IndiceFila + 1] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamiento + 1];

            // Ingresar el valor al resto de las columnas
            for J := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
            begin
              IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, J];
              IndiceNivelCriterioOrdenamientoColumna := InformacionError.InformacionTiempoEjecucionCriterioOrdenamiento.IndicesNivelesObservaciones [IndiceColumna];

              // Se reordena por el modulo por la cantidad de observaciones del nivel de unidad experimental
              if IndiceNivelCriterioOrdenamientoColumna > InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] then
                IndiceNivelCriterioOrdenamientoColumna := IndiceNivelCriterioOrdenamiento mod InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental];

              // Ingresar los valores
              Matriz [IndiceFila + 1, IndiceColumna + 1] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamientoColumna + 1];
              Matriz [IndiceColumna + 1, IndiceFila + 1] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamientoColumna + 1];
            end;

            // Incrementar indice de ultima observacion
            IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
          end;
        end;

        // Liberar submatriz unidad experimental
        FreeAndNil(SubmatrizUnidadExperimental);
      end
      else
      begin
        // No se definio criterio de ordenamiento, no es necesario construir submarices por cada nivel

        // Ingresar los valores a la matriz
        for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];
          IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

          // Ingresar valor en la diagonal principal
          Matriz [IndiceFila, IndiceFila] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1];

          // Ingresar los valores del resto de las columnas
          J := ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1;
          for I := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
          begin
            IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];

            // Verificar coincidencia de nivel de criterio agrupamiento unidad experimental
            if InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceColumna] = IndiceNivelCriterioAgrupamientoUnidadExperimental then
            begin
              // Los niveles coinciden, ingresar los valores
              Matriz [IndiceFila, IndiceColumna + 1] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, J + 1];
              Matriz [IndiceColumna + 1, IndiceFila] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [ArregloIndicesUltimaObservacion [IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, J + 1];
              Inc(J);
            end;
          end;

          // Incrementar el indices de la ultima observacion visitada
          IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
          ArregloIndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental);
        end;
      end;

      // Liberar arreglos de ultima observacion
      FreeAndNil(IndicesUltimaObservacion);
      FreeAndNil(ArregloIndicesUltimaObservacion);
    end
    else
    begin
      // No se definio unidad experimental y se definio criterio agrupamiento unidad experimental

      // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de criterio agrupamiento unidad experimental
      IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
      IndicesUltimaObservacion.PonerA(0);

      // Se ignora el criterio de ordenamiento

      // Ingresar los valores a la matriz
      for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimental := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

        // Ingresar valor en la diagonal principal
        Matriz [IndiceFila, IndiceFila] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1];

        // Ingresar los valores del resto de las columnas
        for I := IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
        begin
          IndiceColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, I];

          Matriz [IndiceFila, IndiceColumna + 1] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
          Matriz [IndiceColumna + 1, IndiceFila] := SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
        end;

        // Incrementar el indice de la ultima observacion visitada
        IndicesUltimaObservacion.Incrementar(IndiceNivelCriterioAgrupamientoUnidadExperimental);
      end;
    end;

    // Liberar submatrices criterio agrupamiento unidad experimental
    for I := Low(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental) to High(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental) do
      FreeAndNil(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental [I]);
    SubmatricesNivelesCriterioAgrupamientoUnidadExperimental := nil;

    // Liberar arreglos de ultima observacion
    FreeAndNil(IndicesUltimaObservacion);
  end
  else if InformacionError.TieneUnidadExperimental then
  begin
    // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental, construir una una submatriz

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionUnidadExperimental;

    // Construir la submatriz
    SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

    // Ingresar valores en la submatriz
    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro];
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro] + InformacionError.Parametros [IndiceParametro + 1];

        for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
          SubmatrizUnidadExperimental [I, J] := InformacionError.Parametros [IndiceParametro + 1];
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          SubmatrizUnidadExperimental [I, J] := InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      SubmatrizUnidadExperimental := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := SubmatrizUnidadExperimental [I, I] + InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic sin Matriz Diagonal

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      SubmatrizUnidadExperimental := M.Producto(M1);
      FreeAndNil(M1);

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic con Matriz Diagonal Escalar

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      SubmatrizUnidadExperimental := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        SubmatrizUnidadExperimental [I, I] := SubmatrizUnidadExperimental [I, I] + InformacionError.Parametros [IndiceParametro];

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          if Abs(I - J) < InformacionError.OrdenEstructuraCovarianza then
          begin
            SubmatrizUnidadExperimental [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        SubmatrizUnidadExperimental [I, I] := InformacionError.Parametros [IndiceParametro];

        R1 := InformacionError.Parametros [IndiceParametro];
        for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
          SubmatrizUnidadExperimental [I, J] := R1;
        end;
      end;
    end;

    // Construir matriz de covarianza del error

    // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental
    IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
    IndicesUltimaObservacion.PonerA(0);

    // Verificar definicion de criterio de reordenamiento
    if InformacionError.TieneCriterioOrdenamiento then
    begin
      // Se definio criterio de ordenamiento

      // Ingresar los valores a la matriz
      for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];
        IndiceNivelCriterioOrdenamiento := InformacionError.InformacionTiempoEjecucionCriterioOrdenamiento.IndicesNivelesObservaciones [IndiceFila - 1];

        // Reordenar de acuerdo a la cantidad de observaciones de la unidad experimental
        if IndiceNivelCriterioOrdenamiento > InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] then
          IndiceNivelCriterioOrdenamiento := IndiceNivelCriterioOrdenamiento mod InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental];

        // Ingresar valor en la diagonal principal
        Matriz [IndiceFila, IndiceFila] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamiento + 1];

        // Ingresar los valores del resto de las columnas
        for I := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
        begin
          IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];
          IndiceNivelCriterioOrdenamientoColumna := InformacionError.InformacionTiempoEjecucionCriterioOrdenamiento.IndicesNivelesObservaciones [IndiceColumna];

          // Reordenar de acuerdo a la cantidad de observaciones de la unidad experimental
          if IndiceNivelCriterioOrdenamientoColumna > InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] then
            IndiceNivelCriterioOrdenamientoColumna := IndiceNivelCriterioOrdenamientoColumna mod InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental];

          // Ingresar los valores
          Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamientoColumna + 1];
          Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizUnidadExperimental [IndiceNivelCriterioOrdenamiento + 1, IndiceNivelCriterioOrdenamientoColumna + 1];
        end;

        // Incrementar el indice de la ultima observacion visitada
        IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
      end;
    end
    else
    begin
      // No se definio criterio de ordenamiento

      // Ingresar los valores a la matriz
      for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

        // Ingresar valor en la diagonal principal
        Matriz [IndiceFila, IndiceFila] := SubmatrizUnidadExperimental [IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1, IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1];

        // Ingresar los valores del resto de las columnas
        for I := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
        begin
          IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];

          Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizUnidadExperimental [IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1, I + 1];
          Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizUnidadExperimental [IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1, I + 1];
        end;

        // Incrementar el indice de la ultima observacion visitada
        IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
      end;
    end;

    // Liberar submatriz unidad experimental
    FreeAndNil(SubmatrizUnidadExperimental);

    // Liberar arreglos de indices ultima observacion
    FreeAndNil(IndicesUltimaObservacion);
  end
  else
  begin
    // No se definio ni unidad experimental ni criterio agrupamiento unidad experimental

    // Construir matriz de covarianza del error

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro];
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro] + InformacionError.Parametros [IndiceParametro + 1];

        for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          Matriz [I, J] := InformacionError.Parametros [IndiceParametro + 1];
          Matriz [J, I] := InformacionError.Parametros [IndiceParametro + 1];
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          Matriz [I, J] := InformacionError.Parametros [IndiceParametro];
          Matriz [J, I] := InformacionError.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Liberar matriz
      FreeAndNil(Matriz);

      // Construir AA'
      M1 := M.Transpuesta;
      Matriz := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := Matriz [I, I] + InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic sin Matriz Diagonal

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Liberar matriz
      FreeAndNil(Matriz);

      // Construir AA'
      M1 := M.Transpuesta;
      Matriz := M.Producto(M1);
      FreeAndNil(M1);

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic con Matriz Diagonal Escalar

      // Construir matriz auxiliar
      M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Liberar matriz
      FreeAndNil(Matriz);

      // Construir AA'
      M1 := M.Transpuesta;
      Matriz := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        Matriz [I, I] := Matriz [I, I] + InformacionError.Parametros [IndiceParametro];

      // Liberar matriz auxiliar
      FreeAndNil(M);
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          if Abs(I - J) < InformacionError.OrdenEstructuraCovarianza then
          begin
            Matriz [I, J] := InformacionError.Parametros [IndiceParametro];
            Matriz [J, I] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      // Ingresar los valores de parametros
      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        Matriz [I, I] := InformacionError.Parametros [IndiceParametro];

        R1 := InformacionError.Parametros [IndiceParametro];
        for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
          Matriz [I, J] := R1;
          Matriz [J, I] := R1;
        end;
      end;
    end;
  end;

  Assert(not Assigned(IndicesUltimaObservacion), 'TProcedimiento.ConstruirMatrizCovarianzaError: not Assigned(IndicesUltimaObservacion)');
  Assert(not Assigned(ArregloIndicesUltimaObservacion), 'TProcedimiento.ConstruirMatrizCovarianzaError: not Assigned(ArregloIndicesUltimaObservacion)');
  Assert(not Assigned(SubmatrizUnidadExperimental), 'TProcedimiento.ConstruirMatrizCovarianzaError: not Assigned(SubmatrizUnidadExperimental)');
  Assert(not Assigned(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental), 'TProcedimiento.ConstruirMatrizCovarianzaError: not Assigned(SubmatricesNivelesCriterioAgrupamientoUnidadExperimental)');
end { TProcedimiento.ConstruirMatrizCovarianzaError };

procedure TProcedimiento.ConstruirMatrizDisenoEfectosAleatorios(var Matriz: TUaMatriz);
var
  I, IndiceColumna, CantidadColumnas: Integer;
  InformacionGrupo: TInformacionTiempoEjecucionGrupoEfectosAleatorios;
begin { TProcedimiento.ConstruirMatrizDisenoEfectosAleatorios }
  if Assigned(Matriz) then
    FreeAndNil(Matriz);

  // Calcular la dimension de la matriz
  CantidadColumnas := 0;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
    CantidadColumnas := CantidadColumnas + FInformacionTiempoEjecucionEfectosAleatorios [I].CantidadColumnasMatrizDiseno;

  // Construir la matriz
  I := FValores.CantidadFilas;
  Matriz := TUaMatriz.Create(I, CantidadColumnas);

  // Ingresar los valores de la matriz
  IndiceColumna := 1;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
  begin
    InformacionGrupo := FInformacionTiempoEjecucionEfectosAleatorios [I];

    ConstruirSubmatrizDisenoGrupoEfectosAleatorios(InformacionGrupo, Matriz, IndiceColumna);
    IndiceColumna := IndiceColumna + InformacionGrupo.CantidadColumnasMatrizDiseno;
  end;
end { TProcedimiento.ConstruirMatrizDisenoEfectosAleatorios };

procedure TProcedimiento.ConstruirMatrizDisenoEfectosFijos(var Matriz: TUaMatriz);
var
  I, IndiceColumna, CantidadColumnas: Integer;
begin { TProcedimiento.ConstruirMatrizDisenoEfectosFijos }
  if Assigned(Matriz) then
    FreeAndNil(Matriz);

  // Calcular las dimensiones de la matriz
  CantidadColumnas := 0;
  for I := FInformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to FInformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
    CantidadColumnas := CantidadColumnas + FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasSubmatrizDisenoEfecto [I];

  // Construir la matriz
  Matriz := TUaMatriz.Create(FValores.CantidadFilas, CantidadColumnas);

  // Ingresar los valores de la matriz

  IndiceColumna := 1;

  // Ingresar los valores para cada efecto
  for I := FInformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Bajo to FInformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos.Alto do
  begin
    ConstruirSubmatrizDisenoEfectoFijo(FInformacionTiempoEjecucionEfectosFijos.InformacionTiempoEjecucionEfectos [I], Matriz, IndiceColumna);
    IndiceColumna := IndiceColumna + FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasSubmatrizDisenoEfecto [I];
  end;
end { TProcedimiento.ConstruirMatrizDisenoEfectosFijos };

procedure TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; const FilaInicial: Integer);
var
  I, J, K, L, Dimension: Integer;
  IndiceEfecto, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental, IndiceNivelEfecto: Integer;
  IndiceFila, IndiceParametro: Integer;
  M, N, M1: TUaMatriz;
  Submatrices: array of TUaMatriz;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios }
  // Inicializar referencias
  M := nil;
  N := nil;
  M1 := nil;
  Submatrices := nil;

  // Ingresar los parametros a la matriz
  IndiceFila := FilaInicial;

  if (Informacion.Estructura.Nombre = 'componentes_varianza') then
  begin
    // Estructura Componentes de Varianza

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
            begin
              for IndiceNivelEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
              begin
                if Informacion.GenerarColumnasNulas or (not Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].NivelesInexistentes [IndiceNivelEfecto]) then
                begin
                  Matriz [IndiceFila, IndiceFila] := Informacion.Parametros [IndiceParametro];
                  Inc(IndiceFila);
                end;
              end;

              Inc(IndiceParametro);
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
          begin
            for IndiceNivelEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
            begin
              if Informacion.GenerarColumnasNulas or (not Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].NivelesInexistentes [IndiceNivelEfecto]) then
              begin
                Matriz [IndiceFila, IndiceFila] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceFila);
              end;
            end;

            Inc(IndiceParametro);
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
        begin
          for IndiceNivelEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
          begin
            if Informacion.GenerarColumnasNulas or (not Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].NivelesInexistentes [IndiceNivelEfecto]) then
            begin
              Matriz [IndiceFila, IndiceFila] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceFila);
            end;
          end;

          Inc(IndiceParametro);
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
      begin
        for IndiceNivelEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].IndicesNiveles.Alto do
        begin
          if Informacion.GenerarColumnasNulas or (not Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].NivelesInexistentes [IndiceNivelEfecto]) then
          begin
            Matriz [IndiceFila, IndiceFila] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceFila);
          end;
        end;

        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'simetria_compuesta') then
  begin
    // Estructura Simetria Compuesta

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro] + Informacion.Parametros [IndiceParametro + 1];

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro + 1];
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro] + Informacion.Parametros [IndiceParametro + 1];

            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro] + Informacion.Parametros [IndiceParametro + 1];

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro + 1];
          end;
        end;

        IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro] + Informacion.Parametros [IndiceParametro + 1];

        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro + 1];
          Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro + 1];
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'general') then
  begin
    // Estructura General

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
                Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);

            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
              Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
          Inc(IndiceParametro);

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
            Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;

        IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
          Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'diagonal_heterogenea') then
  begin
    // Estructura Diagonal Heterogenea

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;

        IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic') then
  begin
    // Estructura Factor Analytic

    // Construir mariz auxiliar
    M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

    // Verificar si se definio criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Construir submatrices para cada nivel del criterio agrupamiento unidad experimental
      SetLength(Submatrices, Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Construir A
        M.AsignarCero;
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);

        // Sumar la matriz escalar D
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] + Informacion.Parametros [IndiceParametro];
          Inc(IndiceParametro);
        end;
      end;

      // Continuar construyendo la submatriz

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio criterio agrupamiento unidad experimental y unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              for J := I to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio criterio agrupamiento unidad experimental y no se definio unidad experimental

        for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;

      // Liberar submatrices
      for I := Low(Submatrices) to High(Submatrices) do
        FreeAndNil(Submatrices [I]);
      Submatrices := nil;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        for J := 1 to Informacion.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      N := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        N [I, I] := N [I, I] + Informacion.Parametros [IndiceParametro];
        Inc(IndiceParametro);
      end;

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end
      else
      begin
        // No se definio ni criterio agrupamiento unidad experimental ni unidad experimental
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          for J := I to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;
      end;

      // Liberar matrices auxiliares
      FreeAndNil(N);
    end;

    // Liberar matriz auxiliar
    FreeAndNil(M);
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_sin_diagonal') then
  begin
    // Estructura Factor Analytic sin Diagonal

    // Construir mariz auxiliar
    M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

    // Verificar si se definio criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Construir submatrices para cada nivel del criterio agrupamiento unidad experimental
      SetLength(Submatrices, Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Construir A
        M.AsignarCero;
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);
      end;

      // Continuar construyendo la submatriz

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio criterio agrupamiento unidad experimental y unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              for J := I to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio criterio agrupamiento unidad experimental y no se definio unidad experimental

        for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;

      // Liberar submatrices
      for I := Low(Submatrices) to High(Submatrices) do
        FreeAndNil(Submatrices [I]);
      Submatrices := nil;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        for J := 1 to Informacion.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      N := M.Producto(M1);
      FreeAndNil(M1);

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end
      else
      begin
        // No se definio ni criterio agrupamiento unidad experimental ni unidad experimental
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          for J := I to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;
      end;

      // Liberar matrices auxiliares
      FreeAndNil(N);
    end;

    // Liberar matriz auxiliar
    FreeAndNil(M);
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_diagonal_escalar') then
  begin
    // Estructura Factor Analytic con Diagonal Escalar

    // Construir mariz auxiliar
    M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

    // Verificar si se definio criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio criterio agrupamiento unidad experimental

      // Construir submatrices para cada nivel del criterio agrupamiento unidad experimental
      SetLength(Submatrices, Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Construir A
        M.AsignarCero;
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Construir AA'
        M1 := M.Transpuesta;
        Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] := M.Producto(M1);
        FreeAndNil(M1);

        // Sumar la matriz escalar D
        for I := 1 to Informacion.CantidadColumnasEfectos do
          Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I, I] + Informacion.Parametros [IndiceParametro];
      end;

      // Continuar construyendo la submatriz

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio criterio agrupamiento unidad experimental y unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              for J := I to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio criterio agrupamiento unidad experimental y no se definio unidad experimental

        for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := Submatrices [IndiceNivelCriterioAgrupamientoUnidadExperimental] [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;

      // Liberar submatrices
      for I := Low(Submatrices) to High(Submatrices) do
        FreeAndNil(Submatrices [I]);
      Submatrices := nil;
    end
    else
    begin
      // No se definio criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Construir A
      M.AsignarCero;
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        for J := 1 to Informacion.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            M [I, J] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Construir AA'
      M1 := M.Transpuesta;
      N := M.Producto(M1);
      FreeAndNil(M1);

      // Sumar la matriz escalar D
      for I := 1 to Informacion.CantidadColumnasEfectos do
        N [I, I] := N [I, I] + Informacion.Parametros [IndiceParametro];

      // Verificar si se definio unidad experimental
      if Informacion.TieneUnidadExperimental then
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            for J := I to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end
      else
      begin
        // No se definio ni criterio agrupamiento unidad experimental ni unidad experimental
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          for J := I to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;
      end;

      // Liberar matrices auxiliares
      FreeAndNil(N);
    end;

    // Liberar matriz auxiliar
    FreeAndNil(M);
  end
  else if (Informacion.Estructura.Nombre = 'bandeada') then
  begin
    // Estructura Bandeada

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                if Abs(I - J) < Informacion.OrdenEstructuraCovarianza then
                begin
                  Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
                  Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
                  Inc(IndiceParametro);
                end;
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);

            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              if Abs(I - J) < Informacion.OrdenEstructuraCovarianza then
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
                Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
          Inc(IndiceParametro);

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            if Abs(I - J) < Informacion.OrdenEstructuraCovarianza then
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
              Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
        Inc(IndiceParametro);

        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          if Abs(I - J) < Informacion.OrdenEstructuraCovarianza then
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := Informacion.Parametros [IndiceParametro];
            Matriz [IndiceFila + J, IndiceFila + I] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'autoregresiva') then
  begin
    // Estructura Autoregresiva de primer orden

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
          begin
            IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro] ;

              R1 := Informacion.Parametros [IndiceParametro];
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Matriz [IndiceFila + I, IndiceFila + J] := R1;
                Matriz [IndiceFila + J, IndiceFila + I] := R1;
              end;
            end;

            IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          IndiceParametro := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];

            R1 := Informacion.Parametros [IndiceParametro];
            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
              Matriz [IndiceFila + I, IndiceFila + J] := R1;
              Matriz [IndiceFila + J, IndiceFila + I] := R1;
            end;
          end;

          IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];

          R1 := Informacion.Parametros [IndiceParametro];
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
            Matriz [IndiceFila + I, IndiceFila + J] := R1;
            Matriz [IndiceFila + J, IndiceFila + I] := R1;
          end;
        end;

        IndiceFila := IndiceFila + Informacion.CantidadColumnasEfectos;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := Informacion.Parametros [IndiceParametro];

        R1 := Informacion.Parametros [IndiceParametro];
        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
          Matriz [IndiceFila + I, IndiceFila + J] := R1;
          Matriz [IndiceFila + J, IndiceFila + I] := R1;
        end;
      end;
    end;
  end;

  Assert(not Assigned(M), 'TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(M)');
  Assert(not Assigned(N), 'TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(N)');
  Assert(not Assigned(M1), 'TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(M1)');
  Assert(not Assigned(Submatrices), 'TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(Submatrices)');
end { TProcedimiento.ConstruirSubmatrizCovarianzaGrupoEfectosAleatorios };

procedure TProcedimiento.ConstruirSubmatrizDisenoEfectoFijo(const Informacion: TInformacionTiempoEjecucionEfecto; var Matriz: TUaMatriz; const ColumnaInicial: Integer);
var
  I, J, K, IndiceColumna: Integer;
  B: Boolean;
  R: Real;
  V: TUaVector;
begin { TProcedimiento.ConstruirSubmatrizDisenoEfectoFijo }
  // Inicializar referencias
  V := nil;

  IndiceColumna := ColumnaInicial;

  if Informacion.Efecto is TIntercepto then
  begin
    // El efecto es un intercepto
    Matriz.AsignarColumna(IndiceColumna, 1.0);
  end
  else
  begin
    // El efecto no es un intercepto
    if Informacion.TieneCovariables then
    begin
      // Tiene covariables, contruir vector de regresores
      V := TUaVector.Create(FValores.CantidadFilas);
      for I := FValores.Bajo to FValores.Alto do
      begin
        R := 1;
        for J := Informacion.IndicesColumnasCovariables.Bajo to Informacion.IndicesColumnasCovariables.Alto do
          R := R * StrToFloat(FValores [I, Informacion.IndicesColumnasCovariables [J]]);
        V [I + 1] := R;
      end;
    end;

    if Informacion.TieneVariablesClasificacion then
    begin
      // Tiene variables de clasificacion

      // Cargar el contenido de la matriz por columna
      for I := Informacion.IndicesNiveles.Bajo to Informacion.IndicesNiveles.Alto do
      begin
        // Verificar si es necesario generar la columna
        if FInformacionTiempoEjecucionEfectosFijos.GenerarColumnasNulas or not (Informacion.NivelesInexistentes [I]) then
        begin
          for J := FValores.Bajo to FValores.Alto do
          begin
            // Verificar la coincidencia del nivel en la columna y fila
            K := Informacion.IndicesColumnasVariablesClasificacion.Bajo;
            B := true;
            while (K <= Informacion.IndicesColumnasVariablesClasificacion.Alto) and B do
            begin
              B := FValores [J, Informacion.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [Informacion.IndicesNivelesVariablesClasificacion [K], Informacion.IndicesNiveles [I, K]];
              Inc(K);
            end;

            // Verificar si debe usar el valor del regresor
            if Informacion.TieneCovariables then
              R := V [J + 1]
            else
              R := 1.0;

            // Ingresar el valor correspondiente
            if B then
              Matriz [J + 1, IndiceColumna] := R;
          end;

          Inc(IndiceColumna);
        end;
      end;
    end
    else
    begin
      // Solo posee covariables (por construccion), copiar el vector de regresores
      Matriz.Columna [IndiceColumna] := V;
      FreeAndNil(V);
    end;
  end;

  Assert(not Assigned(V), 'TProcedimiento.ConstruirSubmatrizDisenoEfectoFijo: not Assigned(V)');
end { TProcedimiento.ConstruirSubmatrizDisenoEfectoFijo };

procedure TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; const ColumnaInicial: Integer);
var
  I, J, K: Integer;
  IndiceColumna, IndiceEfecto, IndiceNivelEfecto, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental: Integer;
  B: Boolean;
  R: Real;
  V: TUaVector;
  InformacionEfecto: TInformacionTiempoEjecucionEfecto;
  VectoresVariablesRegresion: array of TUaVector;
begin { TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios }
  // Inicializar referencias
  V := nil;
  VectoresVariablesRegresion := nil;

  // Construir arreglo de vectores de las variables de regression (para cada efecto que las incluya)
  SetLength(VectoresVariablesRegresion, Informacion.InformacionTiempoEjecucionEfectos.Dimension);
  for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
  begin
    InformacionEfecto := Informacion.InformacionTiempoEjecucionEfectos [I];

    if InformacionEfecto.TieneCovariables then
    begin
      // El efecto tiene covariables, construir vector
      V := TUaVector.Create(FValores.CantidadFilas);

      for J := FValores.Bajo to FValores.Alto do
      begin
        R := 1;
        for K := InformacionEfecto.IndicesColumnasCovariables.Bajo to InformacionEfecto.IndicesColumnasCovariables.Alto do
          R := R * StrToFloat(FValores [J, InformacionEfecto.IndicesColumnasCovariables [K]]);
        V [J + 1] := R;
      end;

      VectoresVariablesRegresion [I] := V;
    end
    else
    begin
      // El efecto no tiene covariables, inicializar el vector por precaucion

      VectoresVariablesRegresion [I] := nil;
    end;
  end;

  // Ingresar los valores a la matriz

  IndiceColumna := ColumnaInicial;

  // Verificar definicion de unidad experimental
  if Informacion.TieneUnidadExperimental then
  begin
    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // Se definio unidad experimental y criterio agrupamiento unidad experimental

      for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
      begin
        for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Ingresar las columnas para cada efecto del grupo
          for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
          begin
            InformacionEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto];

            if InformacionEfecto.Efecto is TIntercepto then
            begin
              // El efecto es un intercepto
              for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
              begin
                for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
                begin
                  if Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] = Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] then
                    // Los indices de niveles de los criterios coinciden
                    Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := 1.0;
                end;
              end;

              Inc(IndiceColumna);
            end
            else
            begin
              // El efecto no es un intercepto

              if InformacionEfecto.TieneVariablesClasificacion then
              begin
                // Tiene variables de clasificacion

                // Ingresar los valores de las columnas del efecto
                for IndiceNivelEfecto := InformacionEfecto.IndicesNiveles.Bajo to InformacionEfecto.IndicesNiveles.Alto do
                begin
                  // Verificar si es necesario generar la columna
                  if Informacion.GenerarColumnasNulas or not (InformacionEfecto.NivelesInexistentes [IndiceNivelEfecto]) then
                  begin
                    // Generar la columna
                    for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
                    begin
                      for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
                      begin
                        // Verificar la coincidencia de los indices de las observaciones de los criterios
                        if Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] = Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] then
                        begin
                          // Los indices de niveles de los criterios coinciden

                          // Verificar la coincidencia del nivel del efecto en columna y fila
                          B := true;
                          K := InformacionEfecto.IndicesColumnasVariablesClasificacion.Bajo;
                          while (K <= InformacionEfecto.IndicesColumnasVariablesClasificacion.Alto) and B do
                          begin
                            B := FValores [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
                            Inc(K);
                          end;

                          // Ingresar el valor correspondiente
                          if B then
                          begin
                            // Verificar si debe usar el valor del regresor
                            if InformacionEfecto.TieneCovariables then
                              R := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1]
                            else
                              R := 1.0;

                            Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := R
                          end;
                        end;
                      end;
                    end;

                    Inc(IndiceColumna);
                  end;
                end;
              end
              else
              begin
                // Solo posee covariables (por construccion), copiar el vector de regresores segun los niveles correspondientes

                for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
                begin
                  for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
                  begin
                    // Verificar la coincidencia de los indices de las observaciones de los criterios
                    if Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] = Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] then
                      // Los indices de niveles de los criterios coinciden
                      Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1];
                  end;
                end;

                Inc(IndiceColumna);
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

      for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar las columnas para cada efecto del grupo
        for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
        begin
          InformacionEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto];

          if InformacionEfecto.Efecto is TIntercepto then
          begin
            // El efecto es un intercepto
            for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
              Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := 1.0;

            Inc(IndiceColumna);
          end
          else
          begin
            // El efecto no es un intercepto

            if InformacionEfecto.TieneVariablesClasificacion then
            begin
              // Tiene variables de clasificacion

              // Ingresar los valores de las columnas del efecto
              for IndiceNivelEfecto := InformacionEfecto.IndicesNiveles.Bajo to InformacionEfecto.IndicesNiveles.Alto do
              begin
                // Verificar si es necesario generar la columna
                if Informacion.GenerarColumnasNulas or not (InformacionEfecto.NivelesInexistentes [IndiceNivelEfecto]) then
                begin
                  // Generar la columna
                  for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
                  begin
                    // Verificar la coincidencia del nivel del efecto en columna y fila
                    B := true;
                    K := InformacionEfecto.IndicesColumnasVariablesClasificacion.Bajo;
                    while (K <= InformacionEfecto.IndicesColumnasVariablesClasificacion.Alto) and B do
                    begin
                      B := FValores [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
                      Inc(K);
                    end;

                    // Ingresar el valor correspondiente
                    if B then
                    begin
                      // Verificar si debe usar el valor del regresor
                      if InformacionEfecto.TieneCovariables then
                        R := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1]
                      else
                        R := 1.0;

                      Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := R
                    end;
                  end;

                  Inc(IndiceColumna);
                end;
              end;
            end
            else
            begin
              // Solo posee covariables (por construccion), copiar el vector de regresores segun los niveles correspondientes

              for I := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
                Matriz [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1, IndiceColumna] := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I] + 1];

              Inc(IndiceColumna);
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    // Verificar definicion de criterio agrupamiento unidad experimental
    if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental y se definio criterio agrupamiento unidad experimental

      for IndiceNivelCriterioAgrupamientoUnidadExperimental := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
      begin
        // Ingresar las columnas para cada efecto del grupo
        for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
        begin
          InformacionEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto];

          if InformacionEfecto.Efecto is TIntercepto then
          begin
            // El efecto es un intercepto
            for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
              Matriz [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] + 1, IndiceColumna] := 1.0;

            Inc(IndiceColumna);
          end
          else
          begin
            // El efecto no es un intercepto

            if InformacionEfecto.TieneVariablesClasificacion then
            begin
              // Tiene variables de clasificacion

              // Ingresar los valores de las columnas del efecto
              for IndiceNivelEfecto := InformacionEfecto.IndicesNiveles.Bajo to InformacionEfecto.IndicesNiveles.Alto do
              begin
                // Verificar si es necesario generar la columna
                if Informacion.GenerarColumnasNulas or not (InformacionEfecto.NivelesInexistentes [IndiceNivelEfecto]) then
                begin
                  // Generar la columna
                  for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
                  begin
                    // Verificar la coincidencia del nivel del efecto en columna y fila
                    B := true;
                    K := InformacionEfecto.IndicesColumnasVariablesClasificacion.Bajo;
                    while (K <= InformacionEfecto.IndicesColumnasVariablesClasificacion.Alto) and B do
                    begin
                      B := FValores [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
                      Inc(K);
                    end;

                    // Ingresar el valor correspondiente
                    if B then
                    begin
                      // Verificar si debe usar el valor del regresor
                      if InformacionEfecto.TieneCovariables then
                        R := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] + 1]
                      else
                        R := 1.0;

                      Matriz [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] + 1, IndiceColumna] := R
                    end;
                  end;

                  Inc(IndiceColumna);
                end;
              end;
            end
            else
            begin
              // Solo posee covariables (por construccion), copiar el vector de regresores segun los niveles correspondientes

              for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
                Matriz [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] + 1, IndiceColumna] := VectoresVariablesRegresion [IndiceEfecto] [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J] + 1];

              Inc(IndiceColumna);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio unidad experimental ni criterio agrupamiento unidad experimental

      // Ingresar las columnas para cada efecto del grupo
      for IndiceEfecto := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
      begin
        InformacionEfecto := Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto];

        if InformacionEfecto.Efecto is TIntercepto then
        begin
          // El efecto es un intercepto
          for I := FValores.Bajo to FValores.Alto do
            Matriz [I + 1, IndiceColumna] := 1.0;

          Inc(IndiceColumna);
        end
        else
        begin
          // El efecto no es un intercepto

          if InformacionEfecto.TieneVariablesClasificacion then
          begin
            // Tiene variables de clasificacion

            // Ingresar los valores de las columnas del efecto
            for IndiceNivelEfecto := InformacionEfecto.IndicesNiveles.Bajo to InformacionEfecto.IndicesNiveles.Alto do
            begin
              // Verificar si es necesario generar la columna
              if Informacion.GenerarColumnasNulas or not (InformacionEfecto.NivelesInexistentes [IndiceNivelEfecto]) then
              begin
                // Generar la columna
                for I := FValores.Bajo to FValores.Alto do
                begin
                  // Verificar la coincidencia del nivel del efecto en columna y fila
                  B := true;
                  K := InformacionEfecto.IndicesColumnasVariablesClasificacion.Bajo;
                  while (K <= InformacionEfecto.IndicesColumnasVariablesClasificacion.Alto) and B do
                  begin
                    B := FValores [I, InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
                    Inc(K);
                  end;

                  // Ingresar el valor correspondiente
                  if B then
                  begin
                    // Verificar si debe usar el valor del regresor
                    if InformacionEfecto.TieneCovariables then
                      R := VectoresVariablesRegresion [IndiceEfecto] [I + 1]
                    else
                      R := 1.0;

                    Matriz [I + 1, IndiceColumna] := R
                  end;
                end;

                Inc(IndiceColumna);
              end;
            end;
          end
          else
          begin
            // Solo posee covariables (por construccion), copiar el vector de regresores segun los niveles correspondientes

            for I := FValores.Bajo to FValores.Alto do
              Matriz [I + 1, IndiceColumna] := VectoresVariablesRegresion [IndiceEfecto] [I + 1];

            Inc(IndiceColumna);
          end;
        end;
      end;
    end;
  end;

  // Liberar arreglo de vectores de variables de regresion
  for I := Low(VectoresVariablesRegresion) to High(VectoresVariablesRegresion) do
    FreeAndNil(VectoresVariablesRegresion [I]);
  VectoresVariablesRegresion := nil;

  Assert(not Assigned(V), 'TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios: not Assigned(V)');
  Assert(not Assigned(VectoresVariablesRegresion), 'TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios: not Assigned(VectoresVariablesRegresion)');
end { TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios };

procedure TProcedimiento.CalcularTerminosValorFuncionObjetivo(var L1, L2, L3: TUaReal);
var
  I, P, Q: Integer;
  W, W0, W1, W2, M1, M2: TUaMatriz;
  D: TUaVector;
  R1, R2, LnDeterminanteR: TUaReal;
begin { TProcedimiento.CalcularTerminosValorFuncionObjetivo }
  // Inicializar referencias
  W := nil;
  W0 := nil;
  W1 := nil;
  W2 := nil;
  M1 := nil;
  D := nil;

  // Si el modelo no posee efectos aleatorios, es posible simplificar el
  // algoritmo. Notar que esto llevara a que no se construyan algunos
  // subproductos necesarios para el calculo del gradiente y del hessiano, por
  // lo que habra que contemplar la falta de definicion de efectos aleatorios en
  // dicho metodo.

  if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
  begin
    // El modelo posee efectos aleatorios

    // Calcular dimensiones (de utilidad a la hora de extraer submatrices)
    P := FX.CantidadColumnas;
    Q := FZ.CantidadColumnas;

    // Construir W0
    W0 := TUaMatriz.Create(P + Q + 1, P + Q + 1);

    // Construir W0(., X)

    // Construir X' * R^(-1) * X
    M1 := FXT.Producto(FRI).Multiplicar(FX);

    // Ingresar X' * R^(-1) * X a W0
    W0.AsignarSubMatriz(1, 1, M1);
    FreeAndNil(M1);

    // Construir Z' * R^(-1) * X
    M1 := FZT.Producto(FRI).Multiplicar(FX);

    // Ingresar Z' * R^(-1) * X a W0
    W0.AsignarSubMatriz(P + 1, 1, M1);
    FreeAndNil(M1);

    // Construir y' * R^(-1) * X
    M1 := FYT.Producto(FRI).Multiplicar(FX);

    // Ingresar y' * R^(-1) * X a W0
    W0.AsignarSubMatriz(P + Q + 1, 1, M1);
    FreeAndNil(M1);

    // Construir W0(., Z)

    // Construir X' * R^(-1) * Z
    M1 := FXT.Producto(FRI).Multiplicar(FZ);

    // Ingresar X' * R^(-1) * Z a W0
    W0.AsignarSubMatriz(1, P + 1, M1);
    FreeAndNil(M1);

    // Construir Z' * R^(-1) * Z
    M1 := FZT.Producto(FRI).Multiplicar(FZ);

    // Ingresar Z' * R^(-1) * Z a W0
    W0.AsignarSubMatriz(P + 1, P + 1, M1);
    FreeAndNil(M1);

    // Construir y' * R^(-1) * Z
    M1 := FYT.Producto(FRI).Multiplicar(FZ);

    // Ingresar y' * R^(-1) * Z a W0
    W0.AsignarSubMatriz(P + Q + 1, P + 1, M1);
    FreeAndNil(M1);

    // Construir W0(., y)

    // Construir X' * R^(-1) * y
    M1 := FXT.Producto(FRI).Multiplicar(FY);

    // Ingresar X' * R^(-1) * y a W0
    W0.AsignarSubMatriz(1, P + Q + 1, M1);
    FreeAndNil(M1);

    // Construir Z' * R^(-1) * y
    M1 := FZT.Producto(FRI).Multiplicar(FY);

    // Ingresar Z' * R^(-1) * y a W0
    W0.AsignarSubMatriz(P + 1, P + Q + 1, M1);
    FreeAndNil(M1);

    // Construir y' * R^(-1) * y
    M1 := FYT.Producto(FRI).Multiplicar(FY);

    // Ingresar y' * R^(-1) * y a W0
    W0.AsignarSubMatriz(P + Q + 1, P + Q + 1, M1);
    FreeAndNil(M1);

    // Construir W
    W := TUaMatriz.Create(P + 2*Q + 1, P + 2*Q + 1);

    // Calcular I + L' * Z' * R^(-1) * Z * L
    M2 := UaMatrizIdentidad(Q);
    M1 := FLT.Producto(FZT).Multiplicar(FRI).Multiplicar(FZ).Multiplicar(FL);
    M2.Sumar(M1);
    FreeAndNil(M1);

    // Ingresar I + L' * Z' * R^(-1) * Z * L a W
    W.AsignarSubMatriz(1, 1, M2);
    FreeAndNil(M2);

    // Calcular L' * W0(Z,.)
    M1 := W0.SubMatriz(P + 1, 1, P + Q, P + Q + 1).PreMultiplicar(FLT);

    // Ingresar L' * W0(Z,.) a W
    W.AsignarSubMatriz(1, Q + 1, M1);
    FreeAndNil(M1);

    // Calcular W0(.,Z) * L
    M1 := W0.SubMatriz(1, P + 1, P + Q + 1, P + Q).Multiplicar(FL);

    // Asignar W0(.,Z) * L a W
    W.AsignarSubMatriz(Q + 1, 1, M1);
    FreeAndNil(M1);

    // Asignar W0 a W
    W.AsignarSubMatriz(Q + 1, Q + 1, W0);
    FreeAndNil(W0);

    // Aplicar SWEEP a las columnas de la particion correspondiente a
    // I + L' * Z' * R^(-1) * Z * L en W. Sumar los logaritmos naturales de los
    // pivots positivos previo a la aplicacion de SWEEP para obtener
    // Log(Det(V)) -  Log(Det(R)).

    // Almacenar los elementos de la diagonal para utilizarlos como factores de
    // el algoritmo G2Sweep.
    D := W.Diagonal;

    L1 := 0;
    for I := 1 to Q do
    begin
      R1 := W [I, I];
      R2 := D [I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        L1 := L1 + Ln(R1);
      G2SweepColumna(W, I, R2);
    end;

    // Liberar D
    FreeAndNil(D);

    // Calcular Ln(Det(R)). Recordar que el doble de la suma de los logaritmos
    // de los elementos positivos de R^(-1/2) es -Ln(Det(R)). Notar que no estoy
    // calculando especificamente lo que indica el paper de Wolfinger, sino que
    // me aprovecho de lo siguiente:

    // 2 * Ln(Det(R^(-1/2))) = Ln(Det(R^(-1/2))^2) = Ln(R)

    LnDeterminanteR := 0.0;
    for I := 1 to FRaizCuadradaR.CantidadColumnas do
    begin
      R1 := FRaizCuadradaR [I, I];
      R2 := FR [I, I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        LnDeterminanteR := LnDeterminanteR + Ln(R1);
    end;
    LnDeterminanteR := 2.0 * LnDeterminanteR;

    {

    LnDeterminanteR := 0.0;
    M1 := InversaGeneralizadaG2Sweep(FRaizCuadradaR.Inversa, FToleranciaSingularidad);
    for I := 1 to M1.CantidadColumnas do
    begin
      R1 := M1 [I, I];
      R2 := M1 [I, I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        LnDeterminanteR := LnDeterminanteR + Ln(R1);
    end;
    LnDeterminanteR := -2.0 * LnDeterminanteR;
    FreeAndNil(M1);

    }

    // Calcular L1 = Log(Det(V))
    L1 := L1 + LnDeterminanteR;

    // Construir W1, Almacenar W1 y liberar W
    W1 := W.SubMatriz(Q + 1, Q + 1, W.CantidadFilas, W.CantidadColumnas);
    FreeAndNil(W);

    // Construir W2
    W2 := TUaMatriz.Create(P + 1, P + 1);

    W1.SubMatrizEn(W2, 1, 1, P, P, 1, 1);
    W1.SubMatrizEn(W2, 1, P + Q + 1, P, P + Q + 1, 1, P + 1);
    W1.SubMatrizEn(W2, P + Q + 1, 1, P + Q + 1, P, P + 1, 1);
    W1.SubMatrizEn(W2, P + Q + 1, P + Q + 1, P + Q + 1, P + Q + 1, P + 1, P + 1);

    // Liberar W1
    FreeAndNil(W1);

    // Aplicar SWEEP a las columnas de la particion correspondiente a
    // X' * V^(-1) * X en W2. Sumar los logaritmos naturales de los pivots
    // positivos previo a la aplicacion de SWEEP para obtener L3.

    // Almacenar los elementos de la diagonal para utilizarlos como factores de
    // el algoritmo G2Sweep.
    D := W2.Diagonal;

    L3 := 0;
    for I := 1 to P do
    begin
      R1 := W2 [I, I];
      R2 := D [I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        L3 := L3 + Ln(R1);

      G2SweepColumna(W2, I, R2);
    end;

    // Liberar D
    FreeAndNil(D);

    L2 := W2 [P + 1, P + 1];

    // Liberar W2
    FreeAndNil(W2);
  end
  else
  begin
    // El modelo no posee efectos aleatorios

    // Calcular dimensiones (de utilidad a la hora de extraer submatrices)
    P := FX.CantidadColumnas;
    Q := 0;

    // Calcular L1 = Log(Det(V)) = Log(Det(R))

    // Calcular Ln(Det(R)). Recordar que el doble de la suma de los logaritmos
    // de los elementos positivos de R^(-1/2) es -Ln(Det(R)). Notar que no estoy
    // calculando especificamente lo que indica el paper de Wolfinger, sino que
    // me aprovecho de lo siguiente:

    // 2 * Ln(Det(R^(-1/2))) = Ln(Det(R^(-1/2))^2) = Ln(R)

    LnDeterminanteR := 0.0;
    for I := 1 to FRaizCuadradaR.CantidadColumnas do
    begin
      R1 := FRaizCuadradaR [I, I];
      R2 := FR [I, I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        LnDeterminanteR := LnDeterminanteR + Ln(R1);
    end;
    LnDeterminanteR := 2.0 * LnDeterminanteR;

    {

    LnDeterminanteR := 0.0;
    M1 := InversaGeneralizadaG2Sweep(FRaizCuadradaR.Inversa, FToleranciaSingularidad);
    for I := 1 to M1.CantidadColumnas do
    begin
      R1 := M1 [I, I];
      R2 := M1 [I, I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        LnDeterminanteR := LnDeterminanteR + Ln(R1);
    end;
    LnDeterminanteR := 2.0 * LnDeterminanteR;
    FreeAndNil(M1);

    }

    L1 := LnDeterminanteR;

    // Construir W2
    W2 := TUaMatriz.Create(P + 1, P + 1);

    // Construir X'V^(-1)X = X'R^(-1)X
    M1 := FXT.Producto(FRI).Multiplicar(FX);

    // Ingresar X'R^(-1)X a W2
    W2.AsignarSubMatriz(1, 1, M1);
    FreeAndNil(M1);

    // Construir X'V^(-1)y = X'R^(-1)y
    M1 := FXT.Producto(FRI).Multiplicar(FY);

    // Ingresar X'R^(-1)y a W2
    W2.AsignarSubMatriz(1, P + 1, M1);
    FreeAndNil(M1);

    // Construir y'V^(-1)X = y'R^(-1)X
    M1 := FYT.Producto(FRI).Multiplicar(FX);

    // Ingresar y'R^(-1)X a W2
    W2.AsignarSubMatriz(P + 1, 1, M1);
    FreeAndNil(M1);

    // Construir y'V^(-1)y = y'R^(-1)y
    M1 := FYT.Producto(FRI).Multiplicar(FY);

    // Ingresar y'R^(-1)y a W2
    W2.AsignarSubMatriz(P + 1, P + 1, M1);
    FreeAndNil(M1);

    // Aplicar SWEEP a las columnas de la particion correspondiente a
    // X' * V^(-1) * X en W2. Sumar los logaritmos naturales de los pivots
    // positivos previo a la aplicacion de SWEEP para obtener L3.

    // Almacenar los elementos de la diagonal para utilizarlos como factores de
    // el algoritmo G2Sweep.
    D := W2.Diagonal;

    L3 := 0;
    for I := 1 to P do
    begin
      R1 := W2 [I, I];
      R2 := D [I] * 1E4 * UaEpsilon;
      if (R1 > 0) and (R1 > R2) then
        L3 := L3 + Ln(R1);

      G2SweepColumna(W2, I, R2);
    end;

    // Liberar D
    FreeAndNil(D);

    L2 := W2 [P + 1, P + 1];

    // Liberar W2
    FreeAndNil(W2);
  end;

  Assert(not Assigned(W), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(W)');
  Assert(not Assigned(W0), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(W0)');
  Assert(not Assigned(W1), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(W1)');
  Assert(not Assigned(W2), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(W2)');
  Assert(not Assigned(M1), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(M1)');
  Assert(not Assigned(D), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(D)');
end { TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos };

function TProcedimiento.Ejecutar(var Bitacora: TStrings): Boolean;
var
  I, J: Integer;
  VariableNormal: TUaVariableAleatoria;
  A, M1: TUaMatriz;
  B, XB: TUaVector;
  String1: String;
  OpcionesProcedimientoAjuste: TStrings;
  Opcion: TOpcion;
  L1, L2, L3: TUaReal;
begin { TProcedimiento.Ejecutar }
  Assert(Assigned(Bitacora), 'TProcedimiento.Ejecutar: Assigned(Bitacora)');

  // Inicializar referencias

  FreeAndNil(FV);
  FreeAndNil(FLV);

  VariableNormal := nil;
  M1 := nil;
  A := nil;
  B := nil;

  try
    // Construir matriz de varianza/covarianza

    ConstruirMatrizCovarianzaError(FR);
    FreeAndNil(FRaizCuadradaR);
    if not DescomposicionCholesky(FR, FRaizCuadradaR, 1E4 * UaEpsilon) then
    begin
      Bitacora.Add('Los parametros generan una matriz de covarianza del error no positiva definida.');
      Result := False;
      Exit;
    end;

    FreeAndNil(FRI);
    FRI := InversaGeneralizadaG2Sweep(FR, 1E4 * UaEpsilon);

    if FModelo.TieneEfectosAleatorios or
       FModelo.TieneEfectosAleatoriosGrupos then
    begin
      // El modelo posee efectos aleatorios

      ConstruirMatrizCovarianzaEfectosAleatorios(FG);
      FreeAndNil(FL);
      FreeAndNil(FLT);

      if not DescomposicionCholesky(FG, FL, 1E4 * UaEpsilon) then
      begin
        Bitacora.Add('Los parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
        Result := False;
        Exit;
      end;

      FLT := FL.Transpuesta;

      FV := FZ.Producto(FG).Multiplicar(FZT).Sumar(FR);
    end
    else
    begin
      // El modelo no posee efectos aleatorios

      FV := FR.Copia;
    end;

    // Verificar que la matriz de varianza/covarianza sea positiva definida
    if DescomposicionCholesky(FV, FLV, 1E4 * UaEpsilon) then
    begin
      VariableNormal := TUaVariableDistribucionNormal.Create(FGeneradorNumerosAleatorios, 0.0, 1.0);

      // Generar vector de variables de distribucion N(0,1)
      A := TUaMatriz.Create(FValores.CantidadFilas, 1);
      for I := 1 to A.CantidadFilas do
        A [I, 1] := VariableNormal.Siguiente;

      // Generar vector de observaciones de la variable dependiente
      FreeAndNil(FY);
      FreeAndNil(FYT);
      M1 := FLV.Producto(A);
      FY := FXBeta.Suma(M1);
      FYT := FY.Transpuesta;
      FreeAndNil(M1);

      // Ingresar Y a la matriz de valores
      for I := 1 to FY.CantidadFilas do
      begin
        String1 := FloatToStr(FY [I, 1]);
        FValores [I - 1, FDatos.Columnas.Count] := String1;
        if FLongitudMaximaValoresColumna [FDatos.Columnas.Count] < Length(String1) then
          FLongitudMaximaValoresColumna [FDatos.Columnas.Count] := Length(String1);
      end;

      // Generar salida de datos

      FSalida.Add('datos "' + FDatos.Etiqueta + '" {');

      String1 := '  columnas';
      for I := 0 to FDatos.Columnas.Count - 1 do
      begin
        if (FDatos.Columnas [I] is TColumnaDatosCategoricos) or
           (FDatos.Columnas [I] is TColumnaVectorDatosCategoricos) then
          String1 := String1 + ' ' + FDatos.Columnas [I].Nombre + ':Categoricos'
        else if (FDatos.Columnas [I] is TColumnaDatosEnteros) or
                (FDatos.Columnas [I] is TColumnaVectorDatosEnteros) then
          String1 := String1 + ' ' + FDatos.Columnas [I].Nombre + ':Enteros'
        else if (FDatos.Columnas [I] is TColumnaDatosReales) or
                (FDatos.Columnas [I] is TColumnaVectorDatosReales) then
          String1 := String1 + ' ' + FDatos.Columnas [I].Nombre + ':Reales';
      end;
      String1 := String1 + ' ' + FModelo.VariableDependiente + ':Reales;';
      FSalida.Add(String1);

      FSalida.Add('  valores {');

      for I := 0 to FValores.CantidadFilas - 1 do
      begin
        String1 := '    ';
        for J := 0 to FValores.CantidadColumnas - 1 do
          String1 := String1 + Format('%' + IntToStr(FLongitudMaximaValoresColumna [J] + 1) + 's', [FValores [I, J]]);
        String1 := String1 + ';';
        FSalida.Add(String1);
      end;
      FSalida.Add('  };');
      FSalida.Add('};');

      // Calcular valores de verosimilitud

      CalcularTerminosValorFuncionObjetivo(L1, L2, L3);

      FVerosimilitudLogaritmica := L1 + L2 + FY.CantidadFilas * Ln(2 * CUaPi);
      FVerosimilitudLogaritmicaResidual := L1 + L2 + L3 + (FY.CantidadFilas - FRangoX) * Ln(2 * CUaPi);

      // Generar salida del modelo

      FSalida.Add('modelo "' + FModelo.Etiqueta + '" {');

      B := FBeta.Columna [1];
      XB := FXBeta.Columna [1];
      FSalida.Add('  // Beta      = ' + B.Texto);
      FSalida.Add('  // X*Beta    = ' + XB.Texto);
      FSalida.Add('  // -2 * L    = ' + FloatToStr(FVerosimilitudLogaritmica));
      FSalida.Add('  // -2 * L_R  = ' + FloatToStr(FVerosimilitudLogaritmicaResidual));
      FreeAndNil(B);
      FreeAndNil(XB);

      if FModelo.VariablesClasificacion.Count > 0 then
      begin
        String1 := '  variables_clasificacion ';
        for I := 0 to FModelo.VariablesClasificacion.Count - 2 do
          String1 := String1 + FModelo.VariablesClasificacion [I] + ' ';
        String1 := String1 + FModelo.VariablesClasificacion [FModelo.VariablesClasificacion.Count - 1] + ';';
        FSalida.Add(String1);
      end;

      FSalida.Add('  variable_dependiente ' + FModelo.VariableDependiente + ';');

      if FModelo.TieneEfectosFijos then
      begin
        FSalida.Add('  efectos_fijos {');

        String1 := '    efectos ';
        for I := 0 to FModelo.EfectosFijos.Efectos.Count - 2 do
          String1 := String1 + FModelo.EfectosFijos.Efectos [I].Texto + ' ';
        String1 := String1 + FModelo.EfectosFijos.Efectos [FModelo.EfectosFijos.Efectos.Count - 1].Texto + ';';
        FSalida.Add(String1);

        for I := 0 to FModelo.EfectosFijos.Opciones.Count - 1 do
        begin
          Opcion := FModelo.EfectosFijos.Opciones [I];

          String1 := '    opcion ' + Opcion.Nombre + ' ';

          if (Opcion is TOpcionParametroBoolean) then
            String1 := String1 + BoolToStr((Opcion as TOpcionParametroBoolean).Parametro, True) + ';'
          else if (Opcion is TOpcionParametroInteger) then
            String1 := String1 + IntToStr((Opcion as TOpcionParametroInteger).Parametro) + ';'
          else if (Opcion is TOpcionParametroReal) then
            String1 := String1 + FloatToStr((Opcion as TOpcionParametroReal).Parametro) + ';'
          else if (Opcion is TOpcionParametroString) then
            String1 := String1 + (Opcion as TOpcionParametroString).Parametro + ';'
          else if (Opcion is TOpcionParametroTEfecto) then
            String1 := String1 + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ';';

          // No existen otra clase de opciones para los efectos fijos.

          FSalida.Add(String1);
        end;

        FSalida.Add('  };');
      end;

      FSalida.Add('  efectos_aleatorios {');

      for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
      begin
        FSalida.Add('    grupo {');

        if FInformacionTiempoEjecucionEfectosAleatorios [I].Efectos.Count > 0 then
        begin
          String1 := '      efectos ';

          for J := 0 to FInformacionTiempoEjecucionEfectosAleatorios [I].Efectos.Count - 2 do
            String1 := String1 + FInformacionTiempoEjecucionEfectosAleatorios [I].Efectos [J].Texto + ' ';
          String1 := String1 + FInformacionTiempoEjecucionEfectosAleatorios [I].Efectos [FInformacionTiempoEjecucionEfectosAleatorios [I].Efectos.Count - 1].Texto + ';';
          FSalida.Add(String1);
        end;

        for J := 0 to FInformacionTiempoEjecucionEfectosAleatorios [I].Opciones.Count - 1 do
        begin
          Opcion := FInformacionTiempoEjecucionEfectosAleatorios [I].Opciones [J];

          if Opcion.Nombre <> 'parametros' then
          begin
            String1 := '      opcion ' + Opcion.Nombre + ' ';

            if (Opcion is TOpcionParametroBoolean) then
              String1 := String1 + BoolToStr((Opcion as TOpcionParametroBoolean).Parametro, True) + ';'
            else if (Opcion is TOpcionParametroInteger) then
              String1 := String1 + IntToStr((Opcion as TOpcionParametroInteger).Parametro) + ';'
            else if (Opcion is TOpcionParametroReal) then
              String1 := String1 + FloatToStr((Opcion as TOpcionParametroReal).Parametro) + ';'
            else if (Opcion is TOpcionParametroString) then
              String1 := String1 + (Opcion as TOpcionParametroString).Parametro + ';'
            else if (Opcion is TOpcionParametroTEfecto) then
              String1 := String1 + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ';'
            else if (Opcion is TOpcionParametroTEstructura) then
              String1 := String1 + (Opcion as TOpcionParametroTEstructura).Parametro.Texto + ';';

            FSalida.Add(String1);
          end;
        end;

        if FComentarValoresParametros then
          String1 := '      // opcion parametros '
        else
          String1 := '      opcion parametros ';

        for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto - 1 do
          String1 := String1 + FloatToStr(FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros [J]) + ' ';
        String1 := String1 + FloatToStr(FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros [FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto]) + ';';
        FSalida.Add(String1);

        FSalida.Add('    };');
      end;

      FSalida.Add('    error {');

      for I := 0 to FInformacionTiempoEjecucionError.Opciones.Count - 1 do
      begin
        Opcion := FInformacionTiempoEjecucionError.Opciones [I];

        if Opcion.Nombre <> 'parametros' then
        begin
          String1 := '      opcion ' + Opcion.Nombre + ' ';

          if (Opcion is TOpcionParametroBoolean) then
            String1 := String1 + BoolToStr((Opcion as TOpcionParametroBoolean).Parametro, True) + ';'
          else if (Opcion is TOpcionParametroInteger) then
            String1 := String1 + IntToStr((Opcion as TOpcionParametroInteger).Parametro) + ';'
          else if (Opcion is TOpcionParametroReal) then
            String1 := String1 + FloatToStr((Opcion as TOpcionParametroReal).Parametro) + ';'
          else if (Opcion is TOpcionParametroString) then
            String1 := String1 + (Opcion as TOpcionParametroString).Parametro + ';'
          else if (Opcion is TOpcionParametroTEfecto) then
            String1 := String1 + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ';'
          else if (Opcion is TOpcionParametroTEstructura) then
            String1 := String1 + (Opcion as TOpcionParametroTEstructura).Parametro.Texto + ';';

          FSalida.Add(String1);
        end;
      end;

      if FComentarValoresParametros then
        String1 := '      // opcion parametros '
      else
        String1 := '      opcion parametros ';

      for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto - 1 do
        String1 := String1 + FloatToStr(FInformacionTiempoEjecucionError.Parametros [I]) + ' ';
      String1 := String1 + FloatToStr(FInformacionTiempoEjecucionError.Parametros [FInformacionTiempoEjecucionError.Parametros.Alto]) + ';';
      FSalida.Add(String1);

      FSalida.Add('    };');

      FSalida.Add('  };');

      FSalida.Add('};');

      FSalida.Add('procedimiento {');

      if FOpciones.BuscarAsignar(I, 'opciones_procedimiento_ajuste') then
      begin
        OpcionesProcedimientoAjuste := (FOpciones [I] as TOpcionParametroTObject).Parametro as TStrings;
        for J := 0 to OpcionesProcedimientoAjuste.Count - 1 do
          FSalida.Add('  ' + OpcionesProcedimientoAjuste.Strings [J] + ' ;');
      end
      else
      begin
        FSalida.Add('  opcion metodo reml ;');
        FSalida.Add('  opcion fuente_valores_iniciales_parametros mivque0 ;');
      end;

      FSalida.Add('  opcion archivo_salida "Ajuste' + FArchivoSalida + '" ;');
      FSalida.Add('};');

      Result := True;
    end
    else
    begin
      Bitacora.Add('Los parametros generan una matriz de varianza/covarianza no positiva definida.');
      Result := False;
    end;
  except on E: Exception do
    begin
      Result := False;
      Bitacora.Add('TProcedimiento.Ejecutar: ' + E.Message);
    end;
  end;

  FreeAndNil(M1);
  FreeAndNil(A);
  FreeAndNil(B);
  FreeAndNil(VariableNormal);
end { TProcedimiento.Ejecutar };

end { UnitProcedimiento }.

{**
@abstract(Contenedores de informacion necesaria para la inicializacion y
ejecucion de los procedimientos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(January 3, 2005)
Este modulo contiene definiciones de clases que almacenan informacion sobre
los modelos y el procedimiento. La informacion se pre-procesa y se almacena
en estos objetos para permitir un acceso mas abstracto a la configuracion. Los
objetos por si solos, son practicamente inutiles dado que los mismos solo sirven
de almacenamiento a informacion construida a partir de los datos de
configuracion del modelo y/o procedimiento.
}
unit UnitInformacionTiempoEjecucionProcedimiento;

interface

uses
  Classes, Contnrs,
  UaComun, UaMatriz, UaVector, UnitArregloEnteros, UnitEfecto, UnitOpcion,
  UnitParametros, UnitEstructura, UnitGrupoEfectos;

type
  {** Informacion de tiempo de ejecucion sobre un efecto.
      @abstract(Informacion sobre un efecto.) }
  TInformacionTiempoEjecucionEfecto = class
  public
    {** Cantidad de niveles del efecto. En el caso de no poseer variables de
        clasificacion, este valor es 1. }
    CantidadNiveles: Integer;
    {** Cantidad de niveles inexistentes (combinaciones no encontradas en los
        datos). En el caso de no poseer variables de clasificacion, este valor
        es 0. }
    CantidadNivelesInexistentes: Integer;
    {** Efecto referido. }
    Efecto: TEfecto;
    {** Indices de las columnas (a estructuras contenidas en la informacion
        sobre el procedimiento) de las covariables del efecto. }
    IndicesColumnasCovariables: TArregloEnteros;
    {** Indices de las columnas (a estructuras contenidas en la informacion
        sobre el procedimiento) de las variables de clasificacion del efecto. }
    IndicesColumnasVariablesClasificacion: TArregloEnteros;
    {** Indices de los niveles (a estructuras contenidas en la informacion
        sobre el procedimiento) de cada nivel del efecto. }
    IndicesNiveles: TArregloArreglosEnteros;
    {** Indices de las variables de clasificacion (a estructuras contenidas en
        la informacion sobre el procedimiento) del efecto. }
    IndicesNivelesVariablesClasificacion: TArregloEnteros;
    {** Arreglo que indica que niveles se encontraron en los datos y cuales
        no. }
    NivelesInexistentes: array of Boolean;
    {** Indica si el efecto tiene covariables. }
    TieneCovariables: Boolean;
    {** Indica si el efecto tiene variables de clasificacion. }
    TieneVariablesClasificacion: Boolean;

    {** Constructor.
        @param(Efecto Efecto del cual se almacenara informacion.) }
    constructor Create(AEfecto: TEfecto);
  end { TInformacionTiempoEjecucionEfecto };

  {** Informacion de tiempo de ejecucion sobre un criterio. Esta informacion es
      la misma que la de los efectos mas ciertas relaciones con las
      observaciones sobre cada nivel.
      @abstract(Informacion sobre un criterio.) }
  TInformacionTiempoEjecucionCriterio = class(TInformacionTiempoEjecucionEfecto)
    {** Indice del nivel en cada observacion. }
    IndicesNivelesObservaciones: TArregloEnteros;
    {** Indices de las observaciones donde ocurrieron cada nivel. }
    IndicesObservacionesNiveles: TArregloArreglosEnteros;
    {** Cantidad observaciones por nivel. }
    CantidadObservacionesNivel: TArregloEnteros;
    {** Cantidad maxima de observaciones (de un nivel). }
    CantidadMaximaObservaciones: Integer;
  end { TInformacionTiempoEjecucionCriterio };

  {** Lista de @link(TInformacionTiempoEjecucionEfecto). Es una clase envoltura
      de TObjectList. Se utiliza esta arquitectura para proveer cheque de tipos
      de las instancias almacenadas y aprovechar la funcionalidad de
      TObjectList.
      @abstract(Lista de @link(TInformacionTiempoEjecucionEfecto).) }
  TListaInformacionTiempoEjecucionEfectos = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad
        @link(InformacionTiempoEjecucionEfectos). Devuelve el elemento
        almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TInformacionTiempoEjecucionEfecto;
    {** Metodo de escritura de la propiedad
        @link(InformacionTiempoEjecucionEfectos). Actualiza el elemento
        almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TInformacionTiempoEjecucionEfecto);
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(Item: TInformacionTiempoEjecucionEfecto): Integer;
    {** Busca un elemento dentro de la lista (Ver @link(TEfecto.IgualA)).
        @param(Efecto Efecto a buscar.)
        @returns(Devuelve el indice del efecto en caso de encontrarlo, o -1 en
        caso contrario.) }
    function IndexOf(Efecto: TEfecto): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(Index: Integer; Item: TInformacionTiempoEjecucionEfecto);
    {** Informacion de tiempo de ejecucion de efectos almacenados. }
    property InformacionTiempoEjecucionEfectos [Index: Integer]: TInformacionTiempoEjecucionEfecto read GetItem write SetItem; default;
  end { TListaInformacionTiempoEjecucionEfectos };

  {** Arreglo dinamico de @link(TInformacionTiempoEjecucionEfecto). Es una
      clase envoltura de un array of TInformacionTiempoEjecucionEfecto. Se
      utiliza una clase envolvente para facilitar la manipulacion de la
      informacion y proveer de una interfaz consistente.
      @abstract(Arreglo dinamico de @link(TInformacionTiempoEjecucionEfecto).) }
  TArregloInformacionTiempoEjecucionEfectos = class
  private
    {** Arreglo dinamico de informacion de tiempo de ejecucion de efectos. }
    FInformacionTiempoEjecucionEfectos: array of TInformacionTiempoEjecucionEfecto;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(InformacionEfectos). Devuelve
        la informacion sobre el efecto asociada a un indice.
        @param(I Indice de la informacion que deseamos acceder.)
        @returns(Arreglo bajo el indice.) }
    function GetInformacion(I: Integer): TInformacionTiempoEjecucionEfecto;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FInformacionTiempoEjecucionEfectos).
        @param(ADimension Nueva dimension del arreglo.) }
    procedure SetDimension(ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(InformacionEfectos).
        Actualiza el valor de una informacion almacenada.
        @param(I Indice de la informacion a actualizar.)
        @param(Informacion Nuevo valor para la informacion.) }
    procedure SetInformacion(I: Integer; Informacion: TInformacionTiempoEjecucionEfecto);
  public
    {** Constructor. Esta version del constructor dimensiona el arreglo
        dinamico e inicializa cada elemento a @nil.
        @param(ADimension Dimension inicial del arreglo dinamico.) }
    constructor Create(ADimension: Integer);
    {** Destructor. }
    destructor Destroy; override;
    {** Busca la informacion de un efecto dentro del arreglo de acuerdo al mismo
        efecto. Para la comparacion se utiliza @link(TEfecto.IgualA).
        @param(Efecto Efecto del cual deseamos conocer su informacion.)
        @returns(Indice de la informacion sobre el efecto en caso de
        encontrarla, o -1 en caso contrario.)}
    function IndiceDe(Efecto: TEfecto): Integer;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension (cantidad de elementos) del arreglo. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Informacion de tiempo de ejecucion sobre efectos almacenados. }
    property InformacionEfectos [I: Integer]: TInformacionTiempoEjecucionEfecto read GetInformacion write SetInformacion; default;
  end { TArregloInformacionTiempoEjecucionEfectos };

  {** Informacion de tiempo de ejecucion sobre los efectos fijos del modelo del
      procedimiento.
      @abstract(Informacion sobre los efectos fijos.) }
  TInformacionTiempoEjecucionEfectosFijos = class
  public
    {** Cantidad de columnas total de la matriz de diseno de efectos fijos. }
    CantidadColumnasMatrizDiseno: Integer;
    {** Cantidad de columnas de cada submatriz de la matriz de diseno
        correspondiente a cada efecto fijo. }
    CantidadColumnasSubmatrizDisenoEfecto: array of Integer;
    {** Efectos fijos propiamente dichos. }
    Efectos: TListaEfectos;
    {** Indica si se deben generar las columnas nulas de la matriz de diseno de
        los efectos fijos. }
    GenerarColumnasNulas: Boolean;
    {** Indica si se debe incluir un intercepto ne la matriz de diseno de los
        efectos fijos. }
    IncluirIntercepto: Boolean;
    {** Informacion de tiempo de ejecucion de los efectos fijos. Ver
        @link(TInformacionTiempoEjecucionEfecto). }
    InformacionTiempoEjecucionEfectos: TArregloInformacionTiempoEjecucionEfectos;
    {** Opciones relacionadas a los efectos fijos. }
    Opciones: TListaOpciones;

    {** Constructor.
        @param(Grupo Grupo de efectos que contiene a los efectos fijos.) }
    constructor Create(Grupo: TGrupoEfectos);
    {** Cantidad de parametros. (Es la cantidad de columnas de la matriz de
        diseno. }
    property CantidadParametros: Integer read CantidadColumnasMatrizDiseno write CantidadColumnasMatrizDiseno;
  end { TInformacionTiempoEjecucionEfectosFijos };

  {** Informacion de tiempo de ejecucion sobre un grupo de efectos aleatorios
      del modelo del procedimiento.
      @abstract(Informacion sobre un grupo de efectos aleatorios.) }
  TInformacionTiempoEjecucionGrupoEfectosAleatorios = class
  public
    {** Cantidad de columnas de la submatriz de diseno. }
    CantidadColumnasEfectos: Integer;
    {** Cantidad de filas de la submatriz de covarianza. }
    CantidadFilasMatrizCovarianza: Integer;
    {** Cantidad de parametros por cada nivel del criterio de agrumiento de
        unidad experimental. }
    CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental: TArregloEnteros;
    {** Criterio de agrupamiento de unidad experimental. }
    CriterioAgrupamientoUnidadExperimental: TEfecto;
    {** Efectos del grupo. }
    Efectos: TListaEfectos;
    {** Estructura de covarianza. }
    Estructura: TEstructura;
    {** Indica si se deben generar las columnas nulas de la submatriz de
        diseno. }
    GenerarColumnasNulas: Boolean;
    {** Grupo de efectos aleatorios del cual se almacena la informacion. }
    Grupo: TGrupoEfectos;
    {** Indica si se debe incluir un intercepto. }
    IncluirIntercepto: Boolean;
    {** Informacion de tiempo de ejecucion sobre los efectos. }
    InformacionTiempoEjecucionEfectos: TArregloInformacionTiempoEjecucionEfectos;
    {** Informacion de tiempo de ejecucion sobre la unidad experimental. }
    InformacionTiempoEjecucionUnidadExperimental: TInformacionTiempoEjecucionCriterio;
    {** Informacion de tiempo de ejecucion sobre el criterio de agrupamiento de
        unidad experimental. }
    InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental: TInformacionTiempoEjecucionCriterio;
    {** Opciones sobre el grupo. }
    Opciones: TListaOpciones;
    {** Orden de la estructura de covarianza. }
    OrdenEstructuraCovarianza: Integer;
    {** Parametros (nombres, valores, cotas, etc.) de la matriz de covarianza. }
    Parametros: TArregloParametros;
    {** Indica si se definiio criterio de agrupamiento de unidad experimental. }
    TieneCriterioAgrupamientoUnidadExperimental: Boolean;
    {** Indica si se definio unidad experimental. }
    TieneUnidadExperimental: Boolean;
    {** Unidad experimental. }
    UnidadExperimental: TEfecto;
    {** Constructor
        @param(AGrupo Grupo sobre el cual se almacena informacion.) }
    constructor Create(AGrupo: TGrupoEfectos);
    {** Cantidad de columnas de la submatriz de diseno. }
    property CantidadColumnasMatrizDiseno: Integer read CantidadFilasMatrizCovarianza write CantidadFilasMatrizCovarianza;
    {** Cantidad de parametros. }
    function CantidadParametros: Integer;
  end { TInformacionTiempoEjecucionGrupoEfectosAleatorios };

  {** Informacion de tiempo de ejecucion sobre el error del modelo del
      procedimiento. La informacion almacenada en
      @link(TInformacionTiempoEjecucionGrupoEfectosAleatorios) es extendida y
      especializada para representar informacion concerniente al error.
      @abstract(Informacion sobre el error.) }
  TInformacionTiempoEjecucionError = class(TInformacionTiempoEjecucionGrupoEfectosAleatorios)
  public
    {** Cantidad maxima de observaciones por nivel de criterio de agrupamiento
        de unidad experimental. }
    CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental: TArregloEnteros;
    {** Criterio de ordenamiento. }
    CriterioOrdenamiento: TEfecto;
    {** Informacion de tiempo de ejecucion sobre el criterio de ordenamiento. }
    InformacionTiempoEjecucionCriterioOrdenamiento: TInformacionTiempoEjecucionCriterio;
    {** Orden de la submatriz de covarianza de cada nivel del criterio de
        agrupamiento de unidad experimental. }
    OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental: TArregloEnteros;
    {** Indica si se definio criterio de ordenamiento. }
    TieneCriterioOrdenamiento: Boolean;

    {** Constructor.
        @param(AOpciones Lista de opciones del error.) }
    constructor Create(AOpciones: TListaOpciones);
  end { TInformacionTiempoEjecucionError };

  {** Lista de @link(TInformacionTiempoEjecucionGrupoEfectosAleatorios). Es una
      clase envoltura de TObjectList. Se utiliza esta arquitectura para proveer
      chequeo de tipos de las instancias almacenadas y aprovechar la
      funcionalidad de TObjectList.
      @abstract(Lista de
      @link(TInformacionTiempoEjecucionGrupoEfectosAleatorios).) }
  TListaInformacionTiempoEjecucionGrupoEfectosAleatorios = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad
        @link(InformacionTiempoEjecucionGruposEfectos). Devuelve el elemento
        almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TInformacionTiempoEjecucionGrupoEfectosAleatorios;
    {** Metodo de escritura de la propiedad
        @link(InformacionTiempoEjecucionGruposEfectos). Actualiza el elemento
        almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios);
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(Index: Integer; Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios);
    {** Informacion sobre grupos de efectos almacenados. }
    property InformacionTiempoEjecucionGruposEfectos [Index: Integer]: TInformacionTiempoEjecucionGrupoEfectosAleatorios read GetItem write SetItem; default;
  end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios };

  {** Esta clase contiene la informacion sobre criterios de ajuste generada
      durante el post-procesamiento de los resultados del ajuste.
      @abstract(Informacion sobre criterios de ajuste.) }
  type TInformacionPostProcesamientoCriteriosAjuste = class
  public
    {** Valor del logaritmo de la funcion de verosimilitud. }
    VerosimilitudLogaritmica: TUaReal;
    {** -2.0 * VerosimilitudLogaritmica. }
    MenosDosVerosimilitudLogaritmica: TUaReal;
    {** Valor del logaritmo de la funcion de verosimilitud residual. }
    VerosimilitudLogaritmicaResidual: TUaReal;
    {** -2.0 * VerosimilitudLogaritmicaResidual. }
    MenosDosVerosimilitudLogaritmicaResidual: TUaReal;
    {** Akaike's Information Criteria. }
    AIC: TUaReal;
    {** Burnham and Anderson's Information Criteria. }
    AICC: TUaReal;
    {** Hannan and Quinn's Information Criteria. }
    HQIC: TUaReal;
    {** Schwarz's Information Criteria. }
    BIC: TUaReal;
    {** Bozdogan's Information Criteria. }
    CAIC: TUaReal;
  end { TInformacionPostProcesamientoCriteriosAjuste };

  {** Esta clase contiene la informacion sobre estructura de media generada
      durante el post-procesamiento de los resultados del ajuste.
      @abstract(Informacion de post-procesamiento sobre estructura de media.) }
  type TInformacionPostProcesamientoEstructuraMedia = class
  public
    {** Estimador de Beta. }
    EstimadoresParametrosEfectosFijos: TUaVector;
    {** Error estandar de los estimadores de parametros de efectos fijos. }
    ErrorEstandarEstimadoresParametrosEfectosFijos: TUaVector;
    {** Grados de libertad del denominador de los estimadores de parametros de
        efectos fijos. }
    GradosLibertadEstimadoresParametrosEfectosFijos: TUaVector;
    {** Valor de t observado de estimadores de parametros dde efectos fijos. }
    ValorTObservadoEstimadoresParametrosEfectosFijos: TUaVector;
    {** Probabilidad (dado los grados de libertad correspondientes) que el
        valor de t supere al observado. }
    ProbabilidadTMayorValorTObservado: TUaVector;
    {** X*Beta }
    XBeta: TUaVector;
    {** Destructor. }
    destructor Destroy; override;
  end { TInformacionPostProcesamientoEstructuraMedia };

  {** Esta clase contiene la informacion sobre estructura de covarianza
      generada durante el post-procesamiento de los resultados del ajuste.
      @abstract(Informacion de post-procesamiento sobre estructura de
      covarianza.) }
  type TInformacionPostProcesamientoEstructuraCovarianza = class
  public
    // Estimadores de parametros de covarianza

    {** Error estandar de los estimadores de parametros de covarianza. }
    ErrorEstandarEstimadoresParametrosCovarianza: TUaVector;
    {** Estimadores de los parametros de covarianza. }
    EstimadoresParametrosCovarianza: TUaVector;
    {** Indica si el hessiano es positivo definido. }
    HessianoPositivoDefinido: Boolean;
    {** Inversa de la matriz de informacion de Fisher observada. }
    MatrizInformacionFisherObservada: TUaMatriz;
    {** Valor de Z de los estimadores de parametros de covarianza. }
    ValorZObservadoEstimadoresParametrosCovarianza: TUaVector;
    {** Probabilidades que los valores de Z excedan a los observados. }
    ProbabilidadValorZEstimadoresParametrosCovarianza: TUaVector;

    // Solucion para la estructura de covarianza

    {** Predictores de los efectos aleatorios (Gamma). }
    PredictoresEfectosAleatorios: TUaVector;
    {** Error estandar de los predictores de efectos aleatorios. }
    ErrorEstandarPredictoresEfectosAleatorios: TUaVector;

    {** Destructor. }
    destructor Destroy; override;
  end { TInformacionPostProcesamientoEstructuraCovarianza };

  {** Esta clase contiene la informacion sobre el modelo generada durante el
      post-procesamiento de los resultados del ajuste.
      @abstract(Informacion de post-procesamiento sobre el modelo.) }
  type TInformacionPostProcesamientoModelo = class
  public
    {** Varianza de y. Si el modelo no posee efectos aleatorios, esta matriz es
        @link(UnitProcedimiento.TProcedimiento.FR). }
    V: TUaMatriz;
    {** Inversa (generalizada) de V. Si el modelo no posee efectos aleatorios,
        esta matriz es @link(UnitProcedimiento.TProcedimiento.FRI). }
    VI: TUaMatriz;
    {** Matriz de covarianza de (Beta_hat - Beta, Gamma_hat - Gamma). Si el
        modelo no posee efectos aleatorios, esta matriz es
        @link(UnitProcedimiento.TProcedimiento.FW2SA). }
    C: TUaMatriz;
  end { TInformacionPostProcesamientoModelo };


implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// InformacionTiempoEjecucionEfecto
// -----------------------------------------------------------------------------

constructor TInformacionTiempoEjecucionEfecto.Create(AEfecto: TEfecto);
begin { TInformacionTiempoEjecucionEfecto.Create }
  Assert(Assigned(AEfecto), 'TInformacionTiempoEjecucionEfecto.Create: Assigned(AEfecto)');

  Efecto := AEfecto;
  CantidadNivelesInexistentes := 0;
end { TInformacionTiempoEjecucionEfecto.Create };

// -----------------------------------------------------------------------------
// TListaInformacionTiempoEjecucionEfectos
// -----------------------------------------------------------------------------

destructor TListaInformacionTiempoEjecucionEfectos.Destroy;
begin { TListaInformacionTiempoEjecucionEfectos.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaInformacionTiempoEjecucionEfectos.Destroy };

procedure TListaInformacionTiempoEjecucionEfectos.SetItem(Index: Integer; Item: TInformacionTiempoEjecucionEfecto);
begin { TListaInformacionTiempoEjecucionEfectos.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionEfectos.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionEfectos.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaInformacionTiempoEjecucionEfectos.SetItem };

function TListaInformacionTiempoEjecucionEfectos.GetItem(Index: Integer): TInformacionTiempoEjecucionEfecto;
begin { TListaInformacionTiempoEjecucionEfectos.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionEfectos.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TInformacionTiempoEjecucionEfecto;
end { TListaInformacionTiempoEjecucionEfectos.GetItem };

procedure TListaInformacionTiempoEjecucionEfectos.Insert(Index: Integer; Item: TInformacionTiempoEjecucionEfecto);
begin { TListaInformacionTiempoEjecucionEfectos.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionEfectos.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionEfectos.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaInformacionTiempoEjecucionEfectos.Insert };

function TListaInformacionTiempoEjecucionEfectos.Add(Item: TInformacionTiempoEjecucionEfecto): Integer;
begin { TListaInformacionTiempoEjecucionEfectos.Add }
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionEfectos.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaInformacionTiempoEjecucionEfectos.Add };

function TListaInformacionTiempoEjecucionEfectos.IndexOf(Efecto: TEfecto): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TListaInformacionTiempoEjecucionEfectos.IndexOf }
  Assert(Assigned(Efecto), 'TListaInformacionTiempoEjecucionEfectos.IndexOf');

  Result := -1;
  I := 0;
  Listo := false;

  while (I <= Count -1) and (not Listo) do
  begin
    if (GetItem(I) as TInformacionTiempoEjecucionEfecto).Efecto.IgualA(Efecto) then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TListaInformacionTiempoEjecucionEfectos.IndexOf };

// -----------------------------------------------------------------------------
// TArregloInformacionTiempoEjecucionEfectos
// -----------------------------------------------------------------------------

function TArregloInformacionTiempoEjecucionEfectos.GetAlto: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.GetAlto }
  Result := High(FInformacionTiempoEjecucionEfectos);
end { TArregloInformacionTiempoEjecucionEfectos.GetAlto };

function TArregloInformacionTiempoEjecucionEfectos.GetBajo: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.GetBajo }
  Result := Low(FInformacionTiempoEjecucionEfectos);
end { TArregloInformacionTiempoEjecucionEfectos.GetBajo };

function TArregloInformacionTiempoEjecucionEfectos.GetDimension: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.GetDimension }
  Result := Length(FInformacionTiempoEjecucionEfectos);
end { TArregloInformacionTiempoEjecucionEfectos.GetDimension };

function TArregloInformacionTiempoEjecucionEfectos.GetInformacion(I: Integer): TInformacionTiempoEjecucionEfecto;
begin { TArregloInformacionTiempoEjecucionEfectos.GetInformacion }
  Assert((I >= Low(FInformacionTiempoEjecucionEfectos)) and (I <= High(FInformacionTiempoEjecucionEfectos)), 'TArregloInformacionTiempoEjecucionEfectos.GetInformacion: (I >= Low(FInformacionTiempoEjecucionEfectos)) and (I <= High(FInformacionTiempoEjecucionEfectos))');

  Result := FInformacionTiempoEjecucionEfectos [I];
end { TArregloInformacionTiempoEjecucionEfectos.GetInformacion };

procedure TArregloInformacionTiempoEjecucionEfectos.SetDimension(ADimension: Integer);
var
  I, DimensionAnterior: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.SetDimension }
  Assert(ADimension > 0, 'TArregloInformacionTiempoEjecucionEfectos.SetDimension: ADimension > 0');

  DimensionAnterior := Length(FInformacionTiempoEjecucionEfectos);

  if DimensionAnterior > ADimension then
  begin
    for I := DimensionAnterior to High(FInformacionTiempoEjecucionEfectos) do
      FreeAndNil(FInformacionTiempoEjecucionEfectos [I]);
    SetLength(FInformacionTiempoEjecucionEfectos, ADimension);
  end
  else if DimensionAnterior < Length(FInformacionTiempoEjecucionEfectos) then
  begin
    SetLength(FInformacionTiempoEjecucionEfectos, ADimension);

    for I := DimensionAnterior to High(FInformacionTiempoEjecucionEfectos) do
      FInformacionTiempoEjecucionEfectos [I] := nil;
  end;
end { TArregloInformacionTiempoEjecucionEfectos.SetDimension };

procedure TArregloInformacionTiempoEjecucionEfectos.SetInformacion(I: Integer; Informacion: TInformacionTiempoEjecucionEfecto);
begin { TArregloInformacionTiempoEjecucionEfectos.SetInformacion }
  Assert((I >= Low(FInformacionTiempoEjecucionEfectos)) and (I <= High(FInformacionTiempoEjecucionEfectos)), 'TArregloInformacionTiempoEjecucionEfectos.SetInformacion: (I >= Low(FInformacionTiempoEjecucionEfectos)) and (I <= High(FInformacionTiempoEjecucionEfectos))');
  Assert(Assigned(Informacion), 'TArregloInformacionTiempoEjecucionEfectos.SetInformacion: Assigned(Informacion)');

  FInformacionTiempoEjecucionEfectos [I] := Informacion;
end { TArregloInformacionTiempoEjecucionEfectos.SetInformacion };

constructor TArregloInformacionTiempoEjecucionEfectos.Create(ADimension: Integer);
var
  I: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.Create }
  Assert(ADimension > 0, 'TArregloInformacionTiempoEjecucionEfectos.Create: ADimension > 0');

  SetLength(FInformacionTiempoEjecucionEfectos, ADimension);

  for I := Low(FInformacionTiempoEjecucionEfectos) to High(FInformacionTiempoEjecucionEfectos) do
    FInformacionTiempoEjecucionEfectos [I] := nil;
end { TArregloInformacionTiempoEjecucionEfectos.Create };

destructor TArregloInformacionTiempoEjecucionEfectos.Destroy;
var
  I: Integer;
begin { TArregloInformacionTiempoEjecucionEfectos.Destroy }
  try
    for I := Low(FInformacionTiempoEjecucionEfectos) to High(FInformacionTiempoEjecucionEfectos) do
      FreeAndNil(FInformacionTiempoEjecucionEfectos [I]);
    FInformacionTiempoEjecucionEfectos := nil;
  finally
    inherited Destroy;
  end;
end { TArregloInformacionTiempoEjecucionEfectos.Destroy };

function TArregloInformacionTiempoEjecucionEfectos.IndiceDe(Efecto: TEfecto): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TArregloInformacionTiempoEjecucionEfectos.IndiceDe }
  Assert(Assigned(Efecto), 'TArregloInformacionTiempoEjecucionEfectos.IndiceDe: Assigned(Efecto)');

  Result := -1;
  I := Low(FInformacionTiempoEjecucionEfectos);
  Listo := false;
  while (I <= High(FInformacionTiempoEjecucionEfectos)) and (not Listo) do
  begin
    if FInformacionTiempoEjecucionEfectos [I].Efecto.IgualA(Efecto) then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TArregloInformacionTiempoEjecucionEfectos.IndiceDe };

// -----------------------------------------------------------------------------
// TInformacionTiempoEjecucionGrupoEfectosFijos// -----------------------------------------------------------------------------

constructor TInformacionTiempoEjecucionEfectosFijos.Create(Grupo: TGrupoEfectos);
var  I: Integer;begin { TInformacionTiempoEjecucionEfectosFijos.Create }  Assert(Assigned(Grupo), 'TInformacionTiempoEjecucionEfectosFijos.Create: Assigned(Grupo)');  Efectos := Grupo.Efectos;  Opciones := Grupo.Opciones;
  // Verificar inclusion de intercepto
  I := Opciones.IndexOf('incluir_intercepto');
  if I <> -1 then
    IncluirIntercepto := (Opciones [I] as TOpcionParametroBoolean).Parametro
  else
  begin
    if Efectos.IncluyeIntercepto then
      IncluirIntercepto := false
    else
      IncluirIntercepto := true;
  end;

  // Verificar generacion de columnas nulas
  I := Opciones.IndexOf('generar_columnas_nulas');
  if I <> -1 then
    GenerarColumnasNulas := (Opciones [I] as TOpcionParametroBoolean).Parametro
  else
    GenerarColumnasNulas := false;
end { TInformacionTiempoEjecucionEfectosFijos.Create };

// -----------------------------------------------------------------------------
// TInformacionTiempoEjecucionGrupoEfectosAleatorios
// -----------------------------------------------------------------------------

constructor TInformacionTiempoEjecucionGrupoEfectosAleatorios.Create(AGrupo: TGrupoEfectos);
var
  I: Integer;
begin { TInformacionTiempoEjecucionGrupoEfectosAleatorios.Create }
  Assert(Assigned(AGrupo), 'TInformacionTiempoEjecucionGrupoEfectosAleatorios.Create: Assigned(AGrupo)');

  Grupo := AGrupo;
  Efectos := Grupo.Efectos;
  Opciones := Grupo.Opciones;

  // Verificar definicion de unidad experimental
  I := Opciones.IndexOf('unidad_experimental');
  if I <> -1 then
  begin
    UnidadExperimental := (Opciones [I] as TOpcionParametroTEfecto).Parametro;
    TieneUnidadExperimental := true;
  end
  else
  begin
    UnidadExperimental := nil;
    TieneUnidadExperimental := false;
  end;

  // Verificar definicion de criterio de agrupamiento unidad experimental
  I := Opciones.IndexOf('criterio_agrupamiento_unidad_experimental');
  if I <> -1 then
  begin
    CriterioAgrupamientoUnidadExperimental := (Opciones [I] as TOpcionParametroTEfecto).Parametro;
    TieneCriterioAgrupamientoUnidadExperimental := true;
  end
  else
  begin
    CriterioAgrupamientoUnidadExperimental := nil;
    TieneCriterioAgrupamientoUnidadExperimental := false;
  end;

  // Verificar definicion de estructura
  I := Opciones.IndexOf('estructura');
  if I <> -1 then
    Estructura := (Opciones [I] as TOpcionParametroTEstructura).Parametro
  else
    Estructura := TEstructura.Create('componentes_varianza');

  // Verificar inclusion de intercepto
  I := Opciones.IndexOf('incluir_intercepto');
  if I <> -1 then
    IncluirIntercepto := (Opciones [I] as TOpcionParametroBoolean).Parametro
  else
    IncluirIntercepto := false;

  // Verificar generacion de columnas nulas
  I := Opciones.IndexOf('generar_columnas_nulas');
  if I <> -1 then
    GenerarColumnasNulas := (Opciones [I] as TOpcionParametroBoolean).Parametro
  else
    GenerarColumnasNulas := true;
end { TInformacionTiempoEjecucionGrupoEfectosAleatorios.Create };

function TInformacionTiempoEjecucionGrupoEfectosAleatorios.CantidadParametros: Integer;
begin { TInformacionTiempoEjecucionGrupoEfectosAleatorios.CantidadParametros }
  Assert(Assigned(Parametros), 'TInformacionTiempoEjecucionGrupoEfectosAleatorios.CantidadParametros: Assigned(Parametros)');

  Result := Parametros.Dimension;
end { TInformacionTiempoEjecucionGrupoEfectosAleatorios.CantidadParametros };

// -----------------------------------------------------------------------------
// TInformacionTiempoEjecucionError
// -----------------------------------------------------------------------------

constructor TInformacionTiempoEjecucionError.Create(AOpciones: TListaOpciones);
var
  I: Integer;
begin { TInformacionTiempoEjecucionError.Create }
  Assert(Assigned(AOpciones), 'TInformacionTiempoEjecucionError.Create: Assigned(AOpciones)');

  Grupo := nil;
  Efectos := nil;
  Opciones := AOpciones;

  // Verificar definicion de unidad experimental
  I := Opciones.IndexOf('unidad_experimental');
  if I <> -1 then
  begin
    UnidadExperimental := (Opciones [I] as TOpcionParametroTEfecto).Parametro;
    TieneUnidadExperimental := true;
  end
  else
  begin
    UnidadExperimental := nil;
    TieneUnidadExperimental := false;
  end;

  // Verificar definicion de criterio de agrupamiento unidad experimental
  I := Opciones.IndexOf('criterio_agrupamiento_unidad_experimental');
  if I <> -1 then
  begin
    CriterioAgrupamientoUnidadExperimental := (Opciones [I] as TOpcionParametroTEfecto).Parametro;
    TieneCriterioAgrupamientoUnidadExperimental := true;
  end
  else
  begin
    CriterioAgrupamientoUnidadExperimental := nil;
    TieneCriterioAgrupamientoUnidadExperimental := false;
  end;

  // Verificar definicion de estructura
  I := Opciones.IndexOf('estructura');
  if I <> -1 then
    Estructura := (Opciones [I] as TOpcionParametroTEstructura).Parametro
  else
    Estructura := TEstructura.Create('componentes_varianza');

  // Verificar definicion de criterio de ordenamiento
  I := Opciones.IndexOf('criterio_ordenamiento');
  if I <> -1 then
  begin
    CriterioOrdenamiento := (Opciones [I] as TOpcionParametroTEfecto).Parametro;
    TieneCriterioOrdenamiento := true;
  end
  else
  begin
    CriterioOrdenamiento := nil;
    TieneCriterioOrdenamiento := false;
  end;
end { TInformacionTiempoEjecucionError.Create };

// -----------------------------------------------------------------------------
// TListaInformacionTiempoEjecucionGrupoEfectosAleatorios
// -----------------------------------------------------------------------------

destructor TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Destroy;
begin { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Destroy };

procedure TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.SetItem(Index: Integer; Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios);
begin { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.SetItem };

function TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.GetItem(Index: Integer): TInformacionTiempoEjecucionGrupoEfectosAleatorios;
begin { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TInformacionTiempoEjecucionGrupoEfectosAleatorios;
end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.GetItem };

procedure TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Insert(Index: Integer; Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios);
begin { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Insert };

function TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Add(Item: TInformacionTiempoEjecucionGrupoEfectosAleatorios): Integer;
begin { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Add }
  Assert(Assigned(Item), 'TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Add };

// -----------------------------------------------------------------------------
// TInformacionPostProcesamientoCriteriosAjuste
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// TInformacionPostProcesamientoEstructuraMedia
// -----------------------------------------------------------------------------

destructor TInformacionPostProcesamientoEstructuraMedia.Destroy;
begin { TInformacionPostProcesamientoEstructuraMedia.Destroy }
  try
    FreeAndNil(EstimadoresParametrosEfectosFijos);
    FreeAndNil(ErrorEstandarEstimadoresParametrosEfectosFijos);
    FreeAndNil(GradosLibertadEstimadoresParametrosEfectosFijos);
    FreeAndNil(ValorTObservadoEstimadoresParametrosEfectosFijos);
    FreeAndNil(ProbabilidadTMayorValorTObservado);
    FreeAndNil(XBeta);
  finally
    inherited Destroy;
  end;
end { TInformacionPostProcesamientoEstructuraMedia.Destroy };

// -----------------------------------------------------------------------------
// TInformacionPostProcesamientoEstructuraCovarianza
// -----------------------------------------------------------------------------

destructor TInformacionPostProcesamientoEstructuraCovarianza.Destroy;
begin { TInformacionPostProcesamientoEstructuraCovarianza.Destroy }
  try
    FreeAndNil(ErrorEstandarEstimadoresParametrosCovarianza);
    FreeAndNil(EstimadoresParametrosCovarianza);
    FreeAndNil(MatrizInformacionFisherObservada);
    FreeAndNil(ValorZObservadoEstimadoresParametrosCovarianza);
    FreeAndNil(ProbabilidadValorZEstimadoresParametrosCovarianza);
  finally
    inherited Destroy;
  end;
end { TInformacionPostProcesamientoEstructuraCovarianza.Destroy };

end { UnitInformacionTiempoEjecucionProcedimiento }.

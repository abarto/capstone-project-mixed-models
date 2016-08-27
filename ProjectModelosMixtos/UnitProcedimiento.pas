{**
@abstract(Clase principal del proyecto. Encapsula los procedimientos de ajuste y
metodos asociados.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(February 06, 2005)
Este modulo contiene la clase principal de todo el proyecto. Agrupa estructuras
de datos y metodos asociados al procedimiento de ajuste de modelos mixtos.
}
unit UnitProcedimiento;

interface

uses
  Classes, Contnrs, Math, SysUtils,
  UaComun, UaConstantes, UaMatriz, UaVector, UnitEfecto, UnitEstructura,
  UnitGrupoEfectos, UnitArregloEnteros, UnitArregloStrings,
  UnitConfiguracionModelo, UnitConstantesProcedimiento, UnitOpcion,
  UnitParametros, UnitConfiguracionProcedimiento,
  UnitInformacionTiempoEjecucionProcedimiento, UnitSubrutinasAlgebraicas,
  UnitSubrutinasEstadisticas;

type
  {** Metodo de ajuste. }
  TMetodoAjuste = function (var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean of object;

  {** Clase principal del proyecto. Esta clase agrupa estructuras de datos y
      metodos necesarios para el procesamiento de la configuracion y posterior
      ajuste de los modelos mixtos de acuerdo a la misma.
      @abstract(Clase con estructuras de datos y metodos necesarios para el
      ajuste.) }
  TProcedimiento = class
  protected
    // Configuracion

    {** Configuracion del procedimiento. }
    FConfiguracion: TConfiguracionProcedimiento;
    {** Configracion del modelo a ajustar. }
    FModelo: TConfiguracionModelo;
    {** Opciones de configuracion del procedimiento. }
    FOpciones: TListaOpciones;

    // Informacion extraida de la configuracion del procedimiento y del
    // modelo. Esta informacion adicional es necesaria para perimitir un acceso
    // mas abstracto a la configuracion del procedimiento, modelo y datos.

    {** Cantidad de filas de la matriz de covarianza de efectos aleatorios (G).
        Se utiliza para no recalcular el valor a partir de la cantidad de filas
        de las submatrices de cada grupo de efectos aleatorios. }
    FCantidadFilasMatrizCovarianzaEfectosAleatorios: Integer;
    {** Cantidad de observaciones. }
    FCantidadObservaciones: Integer;
    {** Cantidad de parametros de covarianza. Es la cantidad de parametros de
        grupos de efectos aleatorios mas la cantidad de parametros del error. }
    FCantidadParametrosCovarianza: Integer;
    {** Cantidad de parametros relacionados a efectos aleatorios. Se utiliza
        para no recalcular el valor a partir de la cantidad de parametros de
        cada grupo de efectos aleatorios. }
    FCantidadParametrosEfectosAleatorios: Integer;
    {** Indica si, a partir de la configuracion de unidad experimental de
        efectos aleatorios y error, es posible realizar operaciones por bloques
        con las matrices de covarianza. Dado que no se han implementado este
        tipo de operaciones, el valor es solo un indicativo. }
    FConstruirBloques: Boolean;
    {** Lista con informacion de criterios (agrupamiento, ordenamiento, etc.)
        Se utiliza para no recalcular la informacion cuando es utilizada a la
        vez por varios grupos de efectos aleatorios o en la configuracion del
        error. }
    FInformacionTiempoEjecucionCriterios: TListaInformacionTiempoEjecucionEfectos;
    {** Lista con informacion sobre los grupos de efectos aleatorios. Esta lista
        incluye la informacion sobre los efectos aleatorios no-agrupados (la
        misma encabeza la lista). }
    FInformacionTiempoEjecucionEfectosAleatorios: TListaInformacionTiempoEjecucionGrupoEfectosAleatorios;
    {** Informacion sobre los efectos fijos del modelo. }
    FInformacionTiempoEjecucionEfectosFijos: TInformacionTiempoEjecucionEfectosFijos;
    {** Informacion sobre el error modelo. }
    FInformacionTiempoEjecucionError: TInformacionTiempoEjecucionError;
    {** Primer termino (L1) del valor de la funcion objetivo de los metodos
        de maxima verosimilitud. }
    FL1: TUaReal;
    {** Segundo termino (L2) del valor de la funcion objetivo de los metodos
        de maxima verosimilitud. }
    FL2: TUaReal;
    {** Tercer termino (L3) del valor de la funcion objetivo de los metodos
        de maxima verosimilitud. }
    FL3: TUaReal;
    {** Matriz con los valores de los niveles de cada variable de
        clasificacion. }
    FMatrizNivelesVariablesClasificacion: TArregloArreglosStrings;
    {** Vector que contiene los valores actuales de los parametros de efectos
        aleatorios. Este vector se utiliza para construir todas las estructuras
        dinamicas dependientes de parametros durante el procedimiento. Los
        parametros asociados a los efectos aleatorios se almacenan primero y
        luego los relacionados al error. }
    FValoresParametrosEfectosAleatorios: TUaVector;
    {** Lista con variables de clasificacion auxiliares. Estas son necesarias
        dado que es posible que una covariable sea transformada en variable de
        clasificacion cuando la misma es utilizada dentro de un criterio. }
    FVariablesClasificacionAuxiliares: TStrings;

    // Matrices, vectores y valores asociados al modelo

    {** Vector (matriz de Nx1) de observaciones de la variable dependiente. }
    FY:  TUaMatriz;
    {** Transpuesta de FY. }
    FYT: TUaMatriz;
    {** Vector de pesos. }
    FW: TUaVector;
    {** Matriz de covarianza de los efectos aleatorios. }
    FG: TUaMatriz;
    {** Descomposicion triangular inferior de Cholesky de FG. }
    FL: TUaMatriz;
    {** Transpuesta de FL. }
    FLT: TUaMatriz;
    {** Matriz de covarianza del error. }
    FR: TUaMatriz;
    {** Inversa de FR. }
    FRI: TUaMatriz;
    {** Raiz cuadrada de FR. }
    FRaizCuadradaR: TUaMatriz;
    {** Matriz de diseno de efectos fijos. }
    FX: TUaMatriz;
    {** Transpuesta de FX. }
    FXT: TUaMatriz;
    {** Rango de la matriz de diseno de efectos fijos. }
    FRangoX: Integer;
    {** Matriz de diseno de los efectos aleatorios. }
    FZ: TUaMatriz;
    {** Transpuesta de FZ. }
    FZT: TUaMatriz;
    {** Estimador de Beta. Se calcula como la segunda particion del resultado
        del segundo paso de SWEEP en el calculo de la verosimilitud. }
    FB: TUaMatriz;
    {** Transpuesta de FB. Se calcula como la cuarta particion del resultado
        del segundo paso de SWEEP en el calculo de la verosimilitud. }
    FBT: TUaMatriz;
    {** Primera particion del resultado del primer paso del operador SWEEP en
        el calculo de la verosimilitud. El valor corresponde a la matriz
        (I + L'Z'R^(-1)ZL)^(-) la cual luego es utilizada en el calculo de las
        derivadas de la funcion objetivo y para la construccion de la matriz de
        covarianza de (Beta, Gamma). }
    FWSA: TUaMatriz;
    {** Cuarta particion del resultado del primer paso del operador SWEEP en
        el calculo de la verosimilitud. De esta matriz se extraen la
        submatrices X'V(-1)X, X'V(-1)y, y'V(-1)X, y'V(-1)y las cuales se
        utilizan para construir la matriz a la cual se le aplica el segundo
        paso de SWEEP. }
    FWSD: TUaMatriz;
    {** Primera particion del resultado del segundo paso del operador SWEEP en
        el calculo de la verosimilitud. El valor corresponde a la matriz
        (X'V^(-1)X)^(-) la cual se utiliza para la construccion de la matriz de
        covarianza de (Beta, Gamma). }
    FW2SA: TUaMatriz;

    // Informacion de postprocesamiento

    {** Informacion sobre criterios de ajuste. }
    FInformacionPostProcesamientoCriteriosAjuste: TInformacionPostProcesamientoCriteriosAjuste;
    {** Informacion sobre estructura de media. }
    FInformacionPostProcesamientoEstructuraMedia: TInformacionPostProcesamientoEstructuraMedia;
    {** Informacion sobre el modelo. }
    FInformacionPostProcesamientoModelo: TInformacionPostProcesamientoModelo;
    {** Informacion sobre estructura de covarianza. }
    FInformacionPostProcesamientoEstructuraCovarianza: TInformacionPostProcesamientoEstructuraCovarianza;

    // Opciones del procedimiento

    {** Cantidad de iteraciones del algoritmo. }
    FCantidadMaximaIteraciones: Integer;
    {** Cantidad de evaluaciones de la funcion objetivo. }
    FCantidadMaximaEvaluacionesFuncionObjetivo: Integer;
    {** Cantidad de pasos iniciales de aplicacion de Scoring. }
    FCantidadPasosScoring: Integer;
    {** Criterio de convergencia del metodo de ajuste. }
    FCriterioConvergencia: TCriterioConvergencia;
    {** Indica si es necesario verificar cotas inferiores. }
    FVerificarCotasInferiores: Boolean;
    {** Indica si es necesario verificar cotas superiores. }
    FVerificarCotasSuperiores: Boolean;
    {** Tolerancia de convergencia. }
    FToleranciaConvergencia: TUaReal;
    {** Tolerancia de singularidades. }
    FToleranciaSingularidad: TUaReal;
    {** Tolerancia de descomposicion de Choleksy. }
    FToleranciaCholesky: TUaReal;
    {** Valor inicial de factor de ridging. }
    FValorInicialFactorRidging: TUaReal;
    {** Valor minimo factor de ridging. }
    FValorMinimoFactorRidging: TUaReal;
    {** Valor maximo factor de ridging. }
    FValorMaximoFactorRidging: TUaReal;
    {** Factor de incremento de factor de ridging. }
    FFactorIncrementoFactorRidging: TUaReal;
    {** Factor de decremento de factor de ridging. }
    FFactorDecrementoFactorRidging: TUaReal;
    {** Metodo de ajuste. }
    FMetodoAjuste: TMetodoAjuste;
    {** Indicia si se debe utilizar la verosimilitud residual para los metodos
        de maxima verosimilitud. }
    FCalcularVerosimilitudResidual: Boolean;

    // Estadisticas de iteracion

    {** Cantidad de iteraciones. }
    FCantidadIteraciones: Integer;
    {** Cantidad de iteraciones en el bucle interno de los metodos
        correspondientes. }
    FCantidadIteracionesBucleInterno: Integer;
    {** Cantidad de evaluaciones de la funcion objetivo. }
    FCantidadEvaluacionesFuncionObjetivo: Integer;

    // Metodos

    {** Calcula los terminos del gradiente y el hessiano de la funcion de maxima
        verosimilitud RESTRINGIDA. Algunos parametros corresponden a parametros
        de salida de @link(CalcularTerminosValorFuncionObjetivoYSubproductos).
        Se recomienda revisar la seccion pertinente en la documentacion del
        proyecto.
        @param(WSA Primera particion del resultado de aplicar SWEEP a
        la matriz W. No debe ser @nil a la entrada.)
        @param(WSD Cuarta particion del resultado de aplicar SWEEP a la
        matriz W. No debe ser @nil a la entrada.)
        @param(W2SA Primera particion del resultado de aplicar SWEEP a
        la matriz W2. No debe ser @nil a la entrada.)
        @param(B Segunda particion del resultado de aplicar SWEEP a la
        matriz W2. No debe ser @nil a la entrada.)
        @param(BT Tercera particion del resultado de aplicar SWEEP a la
        matriz W2. No debe ser @nil a la entrada.)
        @param(CalcularG1 Indica si se debe calcular el termino G1.)
        @param(CalcularG2 Indica si se debe calcular el termino G2.)
        @param(CalcularG3 Indica si se debe calcular el termino G3.)
        @param(CalcularH1 Indica si se debe calcular el termino H1.)
        @param(CalcularH2 Indica si se debe calcular el termino H2.)
        @param(CalcularH3 Indica si se debe calcular el termino H3.)
        @param(G1 Vector (matriz de Nx1) con el termino g1 resultante. No debe
        ser @nil a la entrada.)
        @param(G2 Vector (matriz de Nx1) con el termino g2 resultante. No debe
        ser @nil a la entrada.)
        @param(G3 Vector (matriz de Nx1) con el termino g3 resultante. No debe
        ser @nil a la entrada.)
        @param(H1 Matriz con el termino H1 resultante. No debe ser @nil a la
        entrada.)
        @param(H2 Matriz con el termino H2 resultante. No debe ser @nil a la
        entrada.)
        @param(H3 Matriz con el termino H3 resultante. No debe ser @nil a la
        entrada.) }
    procedure CalcularTerminosGradienteYHessiano(const WSA, WSD, W2SA, B, BT: TUaMatriz; const CalcularG1, CalcularG2, CalcularG3, CalcularH1, CalcularH2, CalcularH3: Boolean; var G1, G2, G3, H1, H2, H3: TUaMatriz);
    {** Calcula el valor de los terminos ll, l2 y l3 de la funcion de maxima
        verosimilitud. Durante el proceso de calculo se generan subproductos
        utiles para el resto del proceso de ajuste. Se recomienda revisar la
        seccion pertinente en la documentacion del proyecto.
        @param(L1 Primer termino de la funcion objetivo.)
        @param(L2 Segundo termino de la funcion objetivo.)
        @param(L3 Tercer termino de la funcion objetivo.)
        @param(WSA Primera particion del resultado de aplicar SWEEP a la matriz
        W. Debe ser @nil a la entrada.)
        @param(WSD Cuarta particion del resultado de aplicar SWEEP a la matriz
        W. Debe ser @nil a la entrada.)
        @param(W2SA Primera particion del resultado de aplicar SWEEP a la
        matriz W2. Debe ser @nil a la entrada.)
        @param(B Segunda particion del resultado de aplicar SWEEP a la matriz
        W2. Debe ser @nil a la entrada.)
        @param(BT Tercera particion del resultado de aplicar SWEEP a la matriz
        W2. Debe ser @nil a la entrada.)
        @param(CalcularWSA Indica si se debe calcular la matriz WSA.)
        @param(CalcularWSD Indica si se debe calcular la matriz WSD.)
        @param(CalcularW2SA Indica si se debe calcular la matriz W2SA.)
        @param(CalcularB Indica si se deben calcular los vectores B y B'.) }
    procedure CalcularTerminosValorFuncionObjetivoYSubproductos(var L1, L2, L3: TUaReal; var WSA, WSD, W2SA, B, BT: TUaMatriz; CalcularWSA: Boolean = True; CalcularWSD: Boolean = True; CalcularW2SA: Boolean = True; CalcularB: Boolean = True);
    {** Construye la derivada primera sobre un parametro de la matriz de
        covarianza de efectos aleatorios. Utiliza los parametros actuales
        para calcular la misma.
        @param(Matriz Derivada primera de la matriz de covarianza de efectos
        aleatorios.)
        @param(Parametro Parametro sobre el cual derivar. Se indexa sobre la
        totalidad de los parametros de efectos aleatorios (en todos los
        grupos).) }
    procedure ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz; Parametro: Integer);
    {** Construye la derivada primera sobre un parametro de la matriz de
        covarianza del error. Utiliza los parametros actuales para calcular la
        misma.
        @param(Matriz Derivada primera de la matriz de covarianza del error.)
        @param(Parametro Parametro sobre el cual derivar. Se indexa sobre los
        parametros del error).) }
    procedure ConstruirDerivadaPrimeraMatrizCovarianzaError(var Matriz: TUaMatriz; Parametro: Integer);
    {** Construye la submatriz de la derivada primera sobre un parametro de la
        matriz de covarianza de efectos aleatorios de un grupo de efectos
        aleatorios. Utiliza los parametros actuales para calcular la misma.
        @param(Matriz Matriz que contendra la submatriz de la derivada.)
        @param(Parametro Parametro sobre el cual derivar. Se indexa sobre los
        parametros del grupo de efectos aleatorios.)
        @param(FilaInicial Fila donde insertar la submatriz (Se inserta a
        partir de la celda FilaInicial x FilaInicial).) }
    procedure ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; Parametro: Integer; const FilaInicial: Integer = 1);
    {** Construye la derivada segunda sobre dos parametros de la matriz de
        covarianza de efectos aleatorios. Utiliza los parametros actuales para
        calcular la misma. Indica si la derivada es nula (la mayoria de los
        casos) o no.
        @param(Matriz Derivada segunda de la matriz de covarianza de efectos
        aleatorios.)
        @param(PrimerParametro Primer parametro sobre el cual derivar. Se
        indexa sobre la totalidad de los parametros de efectos aleatorios (en
        todos los grupos).)
        @param(SegundoParametro Segundo parametro sobre el cual derivar. Se
        indexa sobre la totalidad de los parametros de efectos aleatorios (en
        todos los grupos).)
        @returns(@true si la derivada segunda es nula o @false en caso
        contrario.) }
    function ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer): Boolean;
    {** Construye la derivada segunda sobre dos parametros de la matriz de
        covarianza del error. Utiliza los parametros actuales para calcular la
        misma. Indica si la derivada es nula (la mayoria de los
        casos) o no.
        @param(Matriz Derivada segunda de la matriz de covarianza del error.)
        @param(PrimerParametro Primer parametro sobre el cual derivar. Se
        indexa sobre los parametros del error.)
        @param(SegundoParametro Segundo parametro sobre el cual derivar. Se
        indexa sobre los parametros del error. )
        @returns(@true si la derivada segunda es nula o @false en caso
        contrario.) }
    function ConstruirDerivadaSegundaMatrizCovarianzaError(var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer): Boolean;
    {** Construye la submatriz de la derivada segunda sobre dos parametros de la
        matriz de covarianza de efectos aleatorios de un grupo de efectos
        aleatorios. Utiliza los parametros actuales para calcular el mismo.
        Indica si la derivada es nula (la mayoria de los casos) o no.
        @param(Matriz Matriz que contendra la submatriz de la derivada.)
        @param(PrimerParametro Primer parametro sobre el cual derivar. Se
        indexa sobre los parametros del grupo de efectos aleatorios.)
        @param(SegundoParametro Segundo parametro sobre el cual derivar. Se
        indexa sobre los parametros del grupo de efectos aleatorios.)
        @param(FilaInicial Fila donde insertar la submatriz (Se inserta a
        partir de la celda FilaInicial x FilaInicial).)
        @returns(@true si la derivada segunda es nula o @false en caso
        contrario.) }
    function ConstruirDerivadaSegundaSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer; const FilaInicial: Integer = 1): Boolean;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un criterio.
        @param(Criterio Critrio sobre el cual construir la informacion.)
        @returns(Informacion de tiempo de ejecucion sobre el criterio.) }
    function ConstruirInformacionTiempoEjecucionCriterio(const Criterio: TEfecto): TInformacionTiempoEjecucionCriterio;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un efecto.
        @param(Efecto Efecto sobre el cual construir la informacion.)
        @returns(Informacion de tiempo de ejecucion sobre el efecto.) }
    function ConstruirInformacionTiempoEjecucionEfecto(const Efecto: TEfecto): TInformacionTiempoEjecucionEfecto;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de los efectos fijos.
        @param(Grupo Grupo que contiene la configuracion de efectos fijos.)
        @returns(Informacion de tiempo de ejecucion sobre los efectos fijos.) }
    function ConstruirInformacionTiempoEjecucionEfectosFijos(const Grupo: TGrupoEfectos): TInformacionTiempoEjecucionEfectosFijos;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion del error.
        @param(Error Lista de opciones del error.)
        @returns(Informacion de tiempo de ejecucion sobre el error.) }
    function ConstruirInformacionTiempoEjecucionError(const Error: TListaOpciones): TInformacionTiempoEjecucionError;
    {** Construye la informacion de tiempo de ejecucion a partir de la
        configuracion de un grupo de efectos aleatorios.
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
    {** Construye el vector (matriz de Nx1) de observaciones de la variable
        dependiente a partir de la configuracion del modelo y la informacion en
        tiempo de ejecucion procesada.
        @param(Matriz Vector (matriz de Nx1) de observaciones de la variable
        dependiente.) }
    procedure ConstruirVectorObservacionesVariableDependiente(var Matriz: TUaMatriz);
    {** Construye el vector de pesos de acuerdo a la variable indicada.
        @param(Vector Vector de pesos.) }
    procedure ConstruirVectorPesos(var Vector: TUaVector);
    {** Ejecuta el metodo de grilla de busqueda para el ajuste del modelo de la
        configuracion. Tambien puede utilizarse como fuente de valores
        iniciales para los parametros, siempre y cuando el modelo y la
        configuracion de parametros lo permita.
        @param(ParametrosEfectosAleatorios Vector de parametros de efectos
        aleatorios estimados por el metodo de ajuste.)
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de ajuste.)
        @param(BitacoraIteracion Registro del proceso de ajuste. Solo valido
        para metodos iterativos.)
        @returns(@true si el ajuste fue exitoso, o @false en caso contrario.) }
    function MetodoGrillaBusqueda(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
    {** Ejecuta el metodo MIVQUE0 para el ajuste del modelo de la
        configuracion. Tambien puede utilizarse como fuente de valores
        iniciales para los parametros, siempre y cuando el modelo lo permita.
        @param(ParametrosEfectosAleatorios Vector de parametros de efectos
        aleatorios estimados por el metodo de ajuste.)
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de ajuste.)
        @param(BitacoraIteracion Registro del proceso de ajuste. Solo valido
        para metodos iterativos.)
        @returns(@true si el ajuste fue exitoso, o @false en caso contrario.) }
    function MetodoMIVQUE0(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
    {** Ejecuta el metodo (RE)ML para el ajuste del modelo de la configuracion
        optimizando la verosimilitud logaritmica (residual) mediante el metodo
        de Newton-Raphson/Marquardt.
        @param(ParametrosEfectosAleatorios Vector de parametros de efectos
        aleatorios estimados por el metodo de ajuste.)
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de ajuste.)
        @param(BitacoraIteracion Registro del proceso de ajuste. Solo valido
        para metodos iterativos.)
        @returns(@true si el ajuste fue exitoso, o @false en caso contrario.) }
    function MetodoMaximaVerosimilitudMarquardt(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
    {** Metodo de depuracion. }
    function MetodoDepuracion(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
    {** Verifica que los parametros actuales se encuentren dentro de sus
        cotas.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @param(VerificarCotasInferiores Indica si se debe verificar que todos
        los parametros respeten las cotas inferiores (si las poseen).)
        @param(VerificarCotasSuperiores Indica si se debe verificar que todos
        los parametros respeten las cotas inferiores (si las poseen).)
        @param(Llamador Indica el metodo de donde se inicia la validacion de
        parametros.)
        @returns(@true si los los valores actuales son validos, o @false en
        caso contrario.) }
    function ValidarParametros(var Bitacora: TStrings; const VerificarCotasInferiores, VerificarCotasSuperiores: Boolean; const Llamador: String): Boolean;
  public
    {** Constructor
        @param(AConfiguracion Configuracion del procedimiento.) }
    constructor Create(const AConfiguracion: TConfiguracionProcedimiento);
    {** Destructor. }
    destructor Destroy; override;
    {** Ejecuta el proceso de ajuste.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de ajuste.)
        @param(BitacoraIteracion Registro de la iteracion. Solo valido para
        metodos iterativos.)
        @returns(@true si el procedimiento finalizo correctamente, o @false en
        caso contrario.) }
    function Ejecutar(var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
    {** Inicializar el procedimiento parar construir informacion de tiempo de
        ejecucion y preparar el proceso de ajuste.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de inicializacion.)
        @returns(@true si el procedimiento fue inicializado correctamente, o
        @false en caso contrario.) }
    function Inicializar(var Bitacora: TStrings): Boolean;
    {** Genera el postprocesamiento de los resultados del procedimiento de
        ajuste para calcular estimadores, predictores y tests de ajuste.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el postprocesamiento de los resultados.)
        @returns(@true si el post-procesamiento fue exitoso, o @false en caso
        contrario.) }
    function PostProcesar(var Bitacora: TStrings): Boolean;
    {** Cantidad de iteraciones. }
    property CantidadIteraciones: Integer read FCantidadIteraciones;
    {** Cantidad de iteraciones en el bucle interno de los metodos
        correspondientes. }
    property CantidadIteracionesBucleInterno: Integer read FCantidadIteracionesBucleInterno;
    {** Cantidad de evaluaciones de la funcion objetivo. }
    property CantidadEvaluacionesFuncionObjetivo: Integer read FCantidadEvaluacionesFuncionObjetivo;
    {** Cantidad de observaciones. }
    property CantidadObservaciones: Integer read FCantidadObservaciones;
    {** Cantidad de parametros de covarianza. }
    property CantidadParametrosCovarianza: Integer read FCantidadParametrosCovarianza;
    {** Cantidad de parametros relacionados a efectos aleatorios. }
    property CantidadParametrosEfectosAleatorios: Integer read FCantidadParametrosEfectosAleatorios;
    {** Configuracion del procedimiento. }
    property Configuracion: TConfiguracionProcedimiento read FConfiguracion;
    {** Modelo del procedimiento. }
    property Modelo: TConfiguracionModelo read FModelo;
    {** Opciones del procedimiento. }
    property Opciones: TListaOpciones read FOpciones;
    {** Informacion sobre efectos aleatorios. }
    property InformacionTiempoEjecucionEfectosAleatorios: TListaInformacionTiempoEjecucionGrupoEfectosAleatorios read FInformacionTiempoEjecucionEfectosAleatorios;
    {** Informacion sobre efectos fijos. }
    property InformacionTiempoEjecucionEfectosFijos: TInformacionTiempoEjecucionEfectosFijos read FInformacionTiempoEjecucionEfectosFijos;
    {** Informacion sobre el error. }
    property InformacionTiempoEjecucionError: TInformacionTiempoEjecucionError read FInformacionTiempoEjecucionError;
    {** Informacion de posprocesamiento sobre criterios de ajuste. }
    property InformacionPostProcesamientoCriteriosAjuste: TInformacionPostProcesamientoCriteriosAjuste read FInformacionPostProcesamientoCriteriosAjuste;
    {** Informacion de posprocesamiento sobre estructura de media. }
    property InformacionPostProcesamientoEstructuraMedia: TInformacionPostProcesamientoEstructuraMedia read FInformacionPostProcesamientoEstructuraMedia;
    {** Informacion de postporcesamiento sobre el modelo. }
    property InformacionPostProcesamientoModelo: TInformacionPostProcesamientoModelo read FInformacionPostProcesamientoModelo;
    {** Informacion de posprocesamiento sobre estructuras de covarianza. }
    property InformacionPostProcesamientoEstructuraCovarianza: TInformacionPostProcesamientoEstructuraCovarianza read FInformacionPostProcesamientoEstructuraCovarianza;
    {** Matriz con los valores de los niveles de cada variable de
        clasificacion. }
    property MatrizNivelesVariablesClasificacion: TArregloArreglosStrings read FMatrizNivelesVariablesClasificacion;
    {** Valores actuales de parametros de efectos aleatorios. }
    property ParametrosEfectosAleatorios: TUaVector read FValoresParametrosEfectosAleatorios;
    {** Lista con variables de clasificacion auxiliares. Estas son necesarias
        dado que es posible que una covariable sea transformada en variable de
        clasificacion cuando la misma es utilizada dentro de un criterio. }
    property VariablesClasificacionAuxiliares: TStrings read FVariablesClasificacionAuxiliares;
    {** Vector de observaciones de la variable dependiente. }
    property Y:  TUaMatriz read FY;
    {** Matriz de covarianza de efectos aleatorios. }
    property G: TUaMatriz read FG;
    {** Matriz de covarianza del error. }
    property R: TUaMatriz read FR;
    {** Descomposicion de Cholesky de R. }
    property RaizCuadradaR: TUaMatriz read FRaizCuadradaR;
    {** Matriz de diseno de efectos fijos. }
    property X: TUaMatriz read FX;
    {** Rango de la matriz de diseno de efectos fijos. }
    property RangoX: Integer read FRangoX;
    {** Matriz de diseno de los efectos aleatorios. }
    property Z: TUaMatriz read FZ;
  end { TProcedimiento };

implementation

// -----------------------------------------------------------------------------
// TProcedimiento
// -----------------------------------------------------------------------------

procedure TProcedimiento.CalcularTerminosGradienteYHessiano(const WSA, WSD, W2SA, B, BT: TUaMatriz; const CalcularG1, CalcularG2, CalcularG3, CalcularH1, CalcularH2, CalcularH3: Boolean; var G1, G2, G3, H1, H2, H3: TUaMatriz);
var
  I, J, K, I1, J1, K1, P, Q, N: Integer;
  W1, XA, XAT, C, CT, R1, R1T, M, M1, M2, M3, M4, DGR, DGS, DRR, DRS, D2GRS, D2RRS, H2R, H2S, H3R, H3S, H2RS, H3RS, A1R, A2R, B1: TUaMatriz;
  Factores, FactoresG, FactoresR: array of TUaMatriz;
  SeAnula, SeAnula1, ListoGradienteG, ListoGradienteR: Boolean;
begin { TProcedimiento.CalcularTerminosGradienteYHessiano }
  Assert((not (FModelo.TieneEfectosAleatorios and FModelo.TieneEfectosAleatoriosGrupos)) or Assigned(WSA), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (not (FModelo.TieneEfectosAleatorios and FModelo.TieneEfectosAleatoriosGrupos)) or Assigned(WSA)');
  Assert((not (FModelo.TieneEfectosAleatorios and FModelo.TieneEfectosAleatoriosGrupos)) or Assigned(WSD), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (not (FModelo.TieneEfectosAleatorios and FModelo.TieneEfectosAleatoriosGrupos)) or Assigned(WSD)');
  Assert(Assigned(W2SA), 'TProcedimiento.CalcularTerminosGradienteYHessiano: Assigned(W2SA)');
  Assert(Assigned(B), 'TProcedimiento.CalcularTerminosGradienteYHessiano: Assigned(B)');
  Assert(Assigned(BT), 'TProcedimiento.CalcularTerminosGradienteYHessiano: Assigned(BT)');

  Assert((CalcularG1 and Assigned(G1)) or (not CalcularG1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularG1 and Assigned(G1)) or (not CalcularG1)');
  Assert((CalcularG2 and Assigned(G2)) or (not CalcularG2), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularG2 and Assigned(G2)) or (not CalcularG2)');
  Assert((CalcularG3 and Assigned(G3)) or (not CalcularG3), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularG3 and Assigned(G3)) or (not CalcularG3)');
  Assert((CalcularH1 and Assigned(H1)) or (not CalcularH1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularH1 and Assigned(H1)) or (not CalcularH1)');
  Assert((CalcularH2 and Assigned(H2)) or (not CalcularH2), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularH2 and Assigned(H2)) or (not CalcularH2)');
  Assert((CalcularH3 and Assigned(H3)) or (not CalcularH3), 'TProcedimiento.CalcularTerminosGradienteYHessiano: (CalcularH3 and Assigned(H3)) or (not CalcularH3)');

  // Inicializar referencias
  W1 := nil;
  XA := nil;
  XAT := nil;
  C := nil;
  CT := nil;
  R1 := nil;
  R1T := nil;
  M := nil;
  M1 := nil;
  M2 := nil;
  M3 := nil;
  M4 := nil;
  DGR := nil;
  DGS := nil;
  DRR := nil;
  DRS := nil;
  D2GRS := nil;
  D2RRS := nil;
  H2R := nil;
  H2S := nil;
  H3R := nil;
  H3S := nil;
  H2RS := nil;
  H3RS := nil;
  A1R := nil;
  A2R := nil;
  B1 := nil;
  Factores := nil;
  FactoresG := nil;
  FactoresR := nil;

  // Inicializar los valores de terminos de gradientes (de ser necesario)
  if CalcularG1 then
    G1.AsignarCero;
  if CalcularG2 then
    G2.AsignarCero;
  if CalcularG3 then
    G3.AsignarCero;

  // Inicializar los valores de terminos de hessianos (de ser necesario)
  if CalcularH1 then
    H1.AsignarCero;
  if CalcularH2 then
    H2.AsignarCero;
  if CalcularH3 then
    H3.AsignarCero;

  // Calcular C y C'. C es tal que CC' = (X'V^(-1)X)^(-) = W2SA, por ende
  // C = DescomposicionCholeskyPositivaSemiDefinida(W2SA). Se utiliza la version
  // para matrices positivas semi-definidas dado que (X'V^(-1)X)^(-) es el
  // resultado de aplicar el operador SWEEP, por ende podria tener ceros en la
  // diagonal principal (y en la fila y columna correspondiente).
  DescomposicionCholeskyPositivaSemiDefinida(W2SA, C, FToleranciaCholesky);
  CT := C.Transpuesta;

  // Calcular X* = XC y X*'
  XA := FX.Producto(C);
  XAT := XA.Transpuesta;

  // Calcular r = y - Xb y r'. Para ebitar confundir a r con la matriz de
  // covarianza del error, se renombra r a R1
  M1 := FX.Producto(B);
  R1 := FY.Resta(M1);
  R1T := R1.Transpuesta;
  FreeAndNil(M1);

  // Verificar si el modelo posee efectos aleatorios, dado que de no tenerlos
  // el algoritmo se simplifica considerablemente dado que solo debemos
  // considerar los parametros
  if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
  begin
    // El modelo posee efectos aleatorios

    // Calcular dimensiones (de utilidad a la hora de extraer submatrices)
    P := FX.CantidadColumnas;
    Q := FZ.CantidadColumnas;
    N := FY.CantidadFilas;

    // Calcular factores generales (no dependientes de un parametro en particular)
    SetLength(Factores, 16);

    // Factores [0]  = W1(Z,Z)
    // Factores [1]  = W1(Z,r)
    // Factores [2]  = W1(X,Z)
    // Factores [3]  = W1(Z,X)
    // Factores [4]  = W1(r,Z)
    // Factores [5]  = MZ'
    // Factores [6]  = R^(-1)Z
    // Factores [7]  = R^(-1)ZM = Factores [6] M
    // Factores [8]  = R^(-1)ZMZ' = Factores [7] Z'
    // Factores [9]  = R^(-1)r
    // Factores [10] = R^(-1)X*
    // B1            = MZ'R^(-1)Z - I = Factores [5] Factores [6] - I
    // Factores [11] = B'Z'
    // Factores [12] = I - R(-1)ZMZ' = I - Factores [8]

    {

    // Construir W1
    W1 := TUaMatriz.Create(WSD.CantidadFilas, WSD.CantidadColumnas);

    }

    // Notar que W = WSD (La cuarta particion de W luego de aplicar el operador
    // SWEEP al la primera particion).

    {

    // Calcular W1(X,X) = X^(*)'V^(-1)X^(*) = C'W(X,X)C
    M1 := WSD.SubMatriz(1, 1, P, P).PreMultiplicar(CT).Multiplicar(C);

    // Asignar W1(X,X)
    W1.AsignarSubMatriz(1, 1, M1);
    FreeAndNil(M1);

    }

    // Calcular W1(X,Z) = X^(*)'V^(-1)Z = C'W(X,Z)
    M1 := WSD.SubMatriz(1, P + 1, P, P + Q).PreMultiplicar(CT);

    Factores [2] := M1;

    {

    // Asignar W1(X,Z)
    W1.AsignarSubMatriz(1, P + 1, M1);
    FreeAndNil(M1);

    // Calcular W1(X,r) = X^(*)'V^(-1)r = C'W(X,y) - C'W(X,X)b
    M1 := WSD.SubMatriz(1, P + Q + 1, P, P + Q + 1).PreMultiplicar(CT);
    M2 := WSD.SubMatriz(1, 1, P, P).PreMultiplicar(CT).Multiplicar(B);
    M1.Restar(M2);
    FreeAndNil(M2);

    // Asignar W1(X,r)
    W1.AsignarSubMatriz(1, P + Q + 1, M1);
    FreeAndNil(M1);

    }

    // Calcular W1(Z,X) = Z'V^(-1)X^(*) = W(Z,X)C
    M1 := WSD.SubMatriz(P + 1, 1, P + Q, P).Multiplicar(C);

    Factores [3] := M1;
    // Factores [3] := Factores [2].Transpuesta;

    {

    // Asignar W1(Z,X)
    W1.AsignarSubMatriz(P + 1, 1, M1);
    FreeAndNil(M1);

    }

    // Calcular W1(Z,Z) = Z'V^(-1)Z = W(Z,Z)
    M1 := WSD.SubMatriz(P + 1, P + 1, P + Q, P + Q);

    Factores [0] := M1;

    {

    // Asignar W1(Z,Z)
    W1.AsignarSubMatriz(P + 1, P + 1, M1);
    FreeAndNil(M1);

    }

    // Calcular W1(Z,r) = Z'V^(-1)r = W(Z,y) - W(Z,X)b
    M1 := WSD.SubMatriz(P + 1, P + Q + 1, P + Q, P + Q + 1);
    M2 := WSD.SubMatriz(P + 1, 1, P + Q, P).Multiplicar(B);
    M1.Restar(M2);
    FreeAndNil(M2);

    Factores [1] := M1;

    {

    // Asignar W1(Z,r)
    W1.AsignarSubMatriz(P + 1, P + Q + 1, M1);
    FreeAndNil(M1);

    // Calcular W1(r,X) = r'V^(-1)X^(*) = W(y,X)C - b'W(X,X)C
    M1 := WSD.SubMatriz(P + Q + 1, 1, P + Q + 1, P).Multiplicar(C);
    M2 := WSD.SubMatriz(1, 1, P, P).PreMultiplicar(BT).Multiplicar(C);
    M1.Restar(M2);
    FreeAndNil(M2);

    // Asignar W1(r,X)
    W1.AsignarSubMatriz(P + Q + 1, 1, M1);
    FreeAndNil(M1);

    }

    // Calcular W1(r,Z) = r'V^(-1)Z = W(y,Z) - b'W(X,Z)
    M1 := WSD.SubMatriz(P + Q + 1, P + 1, P + Q + 1, P + Q);
    M2 := WSD.SubMatriz(1, P + 1, P, P + Q).PreMultiplicar(BT);
    M1.Restar(M2);
    FreeAndNil(M2);

    Factores [4] := M1;

    {

    // Asignar W1(r,Z)
    W1.AsignarSubMatriz(P + Q + 1, P + 1, M1);
    FreeAndNil(M1);

    // Calcular W1(r,r) = r'V^(-1)r = W(y,y) - W(y,X)b - b'W(X,y) + b'W(X,X)b
    M1 := WSD.SubMatriz(P + Q + 1, P + Q + 1, P + Q + 1, P + Q + 1);
    M2 := WSD.SubMatriz(P + Q + 1, 1, P + Q + 1, P).Multiplicar(B);
    M3 := WSD.SubMatriz(1, P + Q + 1, P, P + Q + 1).PreMultiplicar(BT);
    M4 := WSD.SubMatriz(1, 1, P, P).PreMultiplicar(BT).Multiplicar(B);
    M1.Restar(M2);
    M3.Sumar(M4);
    M1.Restar(M3);
    FreeAndNil(M2);
    FreeAndNil(M3);
    FreeAndNil(M4);

    // Asignar W1(r,r)
    W1.AsignarSubMatriz(P + Q + 1, P + Q + 1, M1);
    FreeAndNil(M1);

    }

    // Calcular M = (G^(-1) + Z'R^(-1)Z)^(-1) = L (I + L'Z'R^(-1)ZL)^(-) L' = L WSA L'
    M := WSA.PreProducto(FL).Multiplicar(FLT);

    Factores [5] := M.Producto(FZT);
    Factores [6] := FRI.Producto(FZ);
    Factores [7] := Factores [6].Producto(M);
    Factores [8] := Factores [7].Producto(FZT);
    Factores [9] := FRI.Producto(R1);
    Factores [10] := FRI.Producto(XA);

    B1 := Factores [5].Producto(Factores [6]);
    M1 := UaMatrizIdentidad(B1.CantidadFilas, B1.CantidadColumnas);
    B1.Restar(M1);
    FreeAndNil(M1);

    Factores [11] := B1.Transpuesta.Multiplicar(FZT);
    Factores [12] := UaMatrizIdentidad(Factores [8].CantidadFilas, Factores [8].CantidadColumnas).Restar(Factores [8]);

    {

    // Liberar W1, no es necesario en adelante
    FreeAndNil(W1);

    }

    // Asignar matrices para derivadas
    DGR := TUaMatriz.Create(FCantidadFilasMatrizCovarianzaEfectosAleatorios, FCantidadFilasMatrizCovarianzaEfectosAleatorios);
    DGS := TUaMatriz.Create(FCantidadFilasMatrizCovarianzaEfectosAleatorios, FCantidadFilasMatrizCovarianzaEfectosAleatorios);
    D2GRS := TUaMatriz.Create(FCantidadFilasMatrizCovarianzaEfectosAleatorios, FCantidadFilasMatrizCovarianzaEfectosAleatorios);
    DRR := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);
    DRS := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);
    D2RRS := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);

    // Asignar arreglos de factores
    SetLength(FactoresG, 8);
    SetLength(FactoresR, 8);

    I1 := 1;
    ListoGradienteG := false;
    ListoGradienteR := false;

    // Parametros con respecto a G
    for I := 0 to FCantidadParametrosEfectosAleatorios - 1 do
    begin
      // Gradiente y hessiano para r = s

      // Calcular derivadas para r
      ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(DGR, I);
      SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios(D2GRS, I, I);

      // Calcular factores dependientes de G

      // FactoresG [0] = BDGrB'Z'
      // H2r           = W1(X,Z)DGrW1(Z,r)
      // H3r           = W1(X,Z)DGrW1(Z,X)
      FactoresG [0] := B1.Producto(DGR).Multiplicar(Factores [11]);

      if CalcularH2 or CalcularH3 then
      begin
        M1 := Factores [2].Producto(DGR);

        // H2r solo es necesario para el calculo de H2 cruzado (con r correspondiente R y s correspondiente a G)
        if CalcularH2 then
          H2S := M1.Producto(Factores [1]);

        // H3r solo es necesario para el calculo de H3 cruzado (con r correspondiente R y s correspondiente a G)
        if CalcularH3 then
          H3S := M1.Producto(Factores [3]);

        FreeAndNil(M1);
      end;

      // Gradiente
      if not ListoGradienteG then
      begin
        // [g1]r = tr(W1(Z,Z)DGr)
        if CalcularG1 then
        begin
          M1 := Factores [0].Producto(DGR);
          G1 [I1, 1] := M1.Traza;
          FreeAndNil(M1);
        end;

        // [g2]r = -W1(Z,r)'DGrW1(Z,r)
        if CalcularG2 then
        begin
          M1 := Factores [1].Transpuesta.Multiplicar(DGR).Multiplicar(Factores [1]);
          G2 [I1, 1] := -M1 [1, 1];
          FreeAndNil(M1);
        end;

        // [g3]r = -tr(W1(X,Z)DGrW1(Z,X))
        if CalcularG3 then
        begin
          M1 := Factores [2].Producto(DGR).Multiplicar(Factores [3]);
          G3 [I1, 1] := -M1.Traza;
          FreeAndNil(M1);
        end;
      end;

      // Hessiano para r = s

      // [H1]rs = -tr(W1(Z,Z)DGrW1(Z,Z)DGs)
      //          + tr(W1(Z,Z)D2Grs)
      if CalcularH1 then
      begin
        M1 := Factores [0].Producto(DGR);
        M1.Multiplicar(M1);
        H1 [I1, I1] := -M1.Traza;
        FreeAndNil(M1);

        if not SeAnula then
        begin
          // La derivada segunda no es nula
          M1 := Factores [0].Producto(D2GRS);
          H1 [I1, I1] := H1 [I1, I1] + M1.Traza;
          FreeAndNil(M1);
        end;
      end;

      // [H2]rs = 2W1(r,Z)DGrW1(Z,Z)DGsW(Z,r)
      //          - 2W1(r,Z)DGrW1(Z,X)W1(X,Z)DGsW(Z,r)
      //          - W1(r,Z)D2GrsW(Z,r)
      if CalcularH2 then
      begin
        M1 := Factores [4].Producto(DGR);
        M2 := DGR.Producto(Factores [1]);

        M3 := M1.Producto(Factores [0]).Multiplicar(M2);
        H2 [I1, I1] := 2.0 * M3 [1, 1];
        FreeAndNil(M3);

        M3 := M1.Producto(Factores [3]).Multiplicar(Factores [2]).Multiplicar(M2);
        H2 [I1, I1] := H2 [I1, I1] - 2.0 * M3 [1, 1];
        FreeAndNil(M3);

        FreeAndNil(M2);
        FreeAndNil(M1);

        if not SeAnula then
        begin
          // La derivada segunda no es nula
          M1 := Factores [4].Producto(D2GRS).Multiplicar(Factores [1]);
          H2 [I1, I1] := H2 [I1, I1] - M1 [1, 1];
          FreeAndNil(M1);
        end;
      end;

      // [H3]rs = 2tr(W1(X,Z)DGrW1(Z,Z)DGsW1(Z,X))
      //          - tr(W1(X,Z)DGrW1(Z,X)W1(X,Z)DGsW1(Z,X))
      //          - tr(W1(X,Z)D2GrsW(Z,X))
      if CalcularH3 then
      begin
        M1 := Factores [2].Producto(DGR);
        M2 := DGR.Producto(Factores [3]);

        M3 := M1.Producto(Factores [0]).Multiplicar(M2);
        H3 [I1, I1] := 2.0 * M3.Traza;
        FreeAndNil(M3);

        M3 := M1.Producto(Factores [3]).Multiplicar(Factores [2]).Multiplicar(M2);
        H3 [I1, I1] := H3 [I1, I1] - M3.Traza;
        FreeAndNil(M3);

        FreeAndNil(M2);
        FreeAndNil(M1);

        if not SeAnula then
        begin
          // La derivada segunda no es nula
          M1 := Factores [2].Producto(D2GRS).Multiplicar(Factores [3]);
          H3 [I1, I1] := H3 [I1, I1] - M1.Traza;
          FreeAndNil(M1);
        end;
      end;

      // Gradiente y hessiano para r <> s

      // Parametros con respecto a G
      J1 := I1 + 1;
      for J := I + 1 to FCantidadParametrosEfectosAleatorios - 1 do
      begin
        // Calcular derivadas para s
        ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(DGS, J);
        SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios(D2GRS, I, J);

        // Gradiente
        if not ListoGradienteG then
        begin
          // [g1]r = tr(W1(Z,Z)DGr)
          if CalcularG1 then
          begin
            M1 := Factores [0].Producto(DGS);
            G1 [J1, 1] := M1.Traza;
            FreeAndNil(M1);
          end;

          // [g2]r = -W1(Z,r)'DGrW1(Z,r)
          if CalcularG2 then
          begin
            M1 := Factores [1].Transpuesta.Multiplicar(DGS).Multiplicar(Factores [1]);
            G2 [J1, 1] := -M1 [1, 1];
            FreeAndNil(M1);
          end;

          // [g3]r = -tr(W1(X,Z)DGrW1(Z,X))
          if CalcularG3 then
          begin
            M1 := Factores [2].Producto(DGS).Multiplicar(Factores [3]);
            G3 [J1, 1] := -M1.Traza;
            FreeAndNil(M1);
          end;
        end;

        // Hessiano para r <> s

        // [H1]rs = -tr(W1(Z,Z)DGrW1(Z,Z)DGs)
        //          + tr(W1(Z,Z)D2Grs)
        if CalcularH1 then
        begin
          M1 := Factores [0].Producto(DGR).Multiplicar(Factores [0]).Multiplicar(DGS);
          H1 [I1, J1] := -M1.Traza;
          FreeAndNil(M1);

          if not SeAnula then
          begin
            // La derivada segunda no es nula
            M1 := Factores [0].Producto(D2GRS);
            H1 [I1, J1] := H1 [I1, J1] + M1.Traza;
            FreeAndNil(M1);
          end;

          H1 [J1, I1] := H1 [I1, J1];
        end;

        // [H2]rs = 2W1(r,Z)DGrW1(Z,Z)DGsW(Z,r)
        //          - 2W1(r,Z)DGrW1(Z,X)W1(X,Z)DGsW(Z,r)
        //          - W1(r,Z)D2GrsW(Z,r)
        if CalcularH2 then
        begin
          M1 := Factores [4].Producto(DGR);
          M2 := DGS.Producto(Factores [1]);

          M3 := M1.Producto(Factores [0]).Multiplicar(M2);
          H2 [I1, J1] := 2.0 * M3 [1, 1];
          FreeAndNil(M3);

          M3 := M1.Producto(Factores [3]).Multiplicar(Factores [2]).Multiplicar(M2);
          H2 [I1, J1] := H2 [I1, J1] - 2.0 * M3 [1, 1];
          FreeAndNil(M3);

          FreeAndNil(M2);
          FreeAndNil(M1);

          if not SeAnula then
          begin
            // La derivada segunda no es nula
            M1 := Factores [4].Producto(D2GRS).Multiplicar(Factores [1]);
            H2 [I1, J1] := H2 [I1, J1] - M1 [1, 1];
            FreeAndNil(M1);
          end;

          H2 [J1, I1] := H2 [I1, J1];
        end;

        // [H3]rs = 2tr(W1(X,Z)DGrW1(Z,Z)DGsW1(Z,X))
        //          - tr(W1(X,Z)DGrW1(Z,X)W1(X,Z)DGsW1(Z,X))
        //          - tr(W1(X,Z)D2GrsW(Z,X))
        if CalcularH3 then
        begin
          M1 := Factores [2].Producto(DGR);
          M2 := DGS.Producto(Factores [3]);

          M3 := M1.Producto(Factores [0]).Multiplicar(M2);
          H3 [I1, J1] := 2.0 * M3.Traza;
          FreeAndNil(M3);

          M3 := M1.Producto(Factores [3]).Multiplicar(Factores [2]).Multiplicar(M2);
          H3 [I1, J1] := H3 [I1, J1] - M3.Traza;
          FreeAndNil(M3);

          FreeAndNil(M2);
          FreeAndNil(M1);

          if not SeAnula then
          begin
            // La derivada segunda no es nula
            M1 := Factores [2].Producto(D2GRS).Multiplicar(Factores [3]);
            H3 [I1, J1] := H3 [I1, J1] - M1.Traza;
            FreeAndNil(M1);
          end;

          H3 [J1, I1] := H3 [I1, J1];
        end;

        Inc(J1);
      end;

      // Parametros con respecto a R
      for J := 0 to FInformacionTiempoEjecucionError.Parametros.Dimension - 1 do
      begin
        // Calcular derivadas para s
        ConstruirDerivadaPrimeraMatrizCovarianzaError(DRR, J);

        // Calcular factores dependientes de R

        // FactoresR [0] = R^(-1)DRr
        // FactoresR [1] = Z'R^(-1)DRr
        // FactoresR [2] = R^(-1)DRrR^(-1)Z
        // FactoresR [3] = R^(-1)DRr
        //                 - R^(-1)ZMZ'R^(-1)DRr
        //                 - R^(-1)DRrR^(-1)ZMZ'
        //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'
        FactoresR [0] := FRI.Producto(DRR);
        FactoresR [1] := FZT.Producto(FactoresR [0]);
        FactoresR [2] := FactoresR [0].Producto(Factores [6]);

        M1 := Factores [8].Producto(FactoresR [0]);
        FactoresR [3] := FactoresR [0].Resta(M1);
        FreeAndNil(M1);

        M1 := FactoresR [0].Producto(Factores [8]);
        FactoresR [3].Restar(M1);
        FreeAndNil(M1);

        M1 := Factores [8].Producto(FactoresR [0]).Multiplicar(Factores [8]);
        FactoresR [3].Sumar(M1);
        FreeAndNil(M1);

        // Hessiano (cruzado) para r (en R) y s (en G)

        // [H1]rs = -tr(Z'R^(-1)DRrR^(-1)ZDGs)
        //          + 2tr(Z'R^(-1)DRrR^(-1)ZDGsZ'R^(-1)ZM)
        //          - tr(Z'R^(-1)DRrR^(-1)ZMZ'R^(-1)ZDGsZ'R^(-1)ZM)
        if CalcularH1 then
        begin
          M1 := FactoresR [1].Producto(Factores [6]).Multiplicar(DGR);
          H1 [J1, I1] := -M1.Traza;

          M1.Multiplicar(FZT).Multiplicar(Factores [7]);
          H1 [J1, I1] := H1 [J1, I1] + 2.0 * M1.Traza;
          FreeAndNil(M1);

          M1 := FactoresR [1].Producto(Factores [8]).Multiplicar(Factores [6]).Multiplicar(DGR).Multiplicar(FZT).Multiplicar(Factores [7]);
          H1 [J1, I1] := H1 [J1, I1] - M1.Traza;
          FreeAndNil(M1);

          H1 [I1, J1] := H1 [J1, I1];
        end;

        // [H2]rs = H2rs - 2H2r'H2s
        // [H3]rs = tr(H3rs - H3rH3s)

        // H2rs = 2A1rBDGsB'Z'R^(-1)r
        //      = 2A1r FactoresG [0] Factores [9]
        // H3rs = 2A2rBDGsB'Z'R^(-1)X*
        //      = 2A2r FactoresG [0] Factores [10]

        //  A1r = r'R^(-1)DRrR^(-1)Z
        //        - r'R^(-1)ZMZ'R^(-1)DRrR^(-1)Z
        //      = r' Factores [12] FactoresR [2]
        //  A2r = X*'R^(-1)DRrR^(-1)Z
        //        - X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)Z
        //      = X*' Factores [12] FactoresR [2]
        //    B = MZ'R^(-1)Z - I (Calculado arriba como B1)

        // H2r = X*'R^(-1)DRrR^(-1)r
        //       - X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)r
        //       - X*'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
        //       + X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
        //     = X*' FactoresR [3] R^(-1)r

        // H3r = X*'R^(-1)DRrR^(-1)X*
        //       - X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)X*
        //       - X*'R^(-1)DRrR^(-1)ZMZ'R^(-1)X*
        //       + X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)X*
        //     = X*' FactoresR [3] R^(-1)X*

        // H2s, H3s (Para G, Calculado arriba)

        if CalcularH2 then
        begin
          H2R := XAT.Producto(FactoresR [3]).Multiplicar(Factores [9]);

          A1R := R1T.Producto(Factores [12]).Multiplicar(FactoresR [2]);

          H2RS := A1R.Producto(FactoresG [0]).Multiplicar(Factores [9]).Multiplicar(2.0);

          M1 := H2R.Transpuesta.Multiplicar(H2S);
          H2 [J1, I1] := H2RS [1, 1] - 2.0 * M1 [1, 1];
          FreeAndNil(M1);

          H2 [I1, J1] := H2 [J1, I1];
        end;

        if CalcularH3 then
        begin
          H3R := XAT.Producto(FactoresR [3]).Multiplicar(Factores [10]);

          A2R := XAT.Producto(Factores [12]).Multiplicar(FactoresR [2]);

          H3RS := A2R.Producto(FactoresG [0]).Multiplicar(Factores [10]).Multiplicar(2.0);

          M1 := H3R.Producto(H3S);
          // Se pierde H3RS, pero no es necesario en adelante
          H3 [J1, I1] := H3RS.Restar(M1).Traza;
          FreeAndNil(M1);

          H3 [I1, J1] := H3 [J1, I1];
        end;

        // Liberar factores dependientes de R
        for K := Low(FactoresR) to High(FactoresR) do
          FreeAndNil(FactoresR [K]);
        FreeAndNil(A1R);
        FreeAndNil(A2R);
        FreeAndNil(H2R);
        FreeAndNil(H3R);
        FreeAndNil(H2RS);
        FreeAndNil(H3RS);

        Inc(J1);
      end;

      // Luego del recorrido del bucle interno sobre los parametros de G no es
      // necesario calcular mas componentes del gradiente respecto de G.
      ListoGradienteG := True;

      // Liberar factores dependientes de G
      for J := Low(FactoresG) to High(FactoresG) do
        FreeAndNil(FactoresG [J]);
      FreeAndNil(H2S);
      FreeAndNil(H3S);

      Inc(I1);
    end;

    // Parametros con respecto a R
    for I := 0 to FInformacionTiempoEjecucionError.Parametros.Dimension - 1 do
    begin
      // Calcular derivadas para r
      ConstruirDerivadaPrimeraMatrizCovarianzaError(DRR, I);
      SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaError(D2RRS, I, I);

      // Calcular factores

      // FactoresR [0] = R^(-1)DRr
      // FactoresR [1] = R^(-1)DRr
      //                 - R^(-1)ZMZ'R^(-1)DRr
      //                 - R^(-1)DRrR^(-1)ZMZ'
      //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'
      // FactoresR [2] = R^(-1)DRrR^(-1)DRr
      // FactoresR [3] = R^(-1)DRrR^(-1)DRr
      //                 - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRr
      //                 - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRr
      //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRr
      //                 - R^(-1)DRrR^(-1)DRrR^(-1)ZMZ'
      //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRrR^(-1)ZMZ'
      //                 + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'
      //                 - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'
      // H2r = X*'R^(-1)DRrR^(-1)r
      //       - X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)r
      //       - X*'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
      //       + X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
      //     = X*' FactoresR [1] R^(-1)r
      // H3r = X*'R^(-1)DRrR^(-1)X*
      //       - X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)X*
      //       - X*'R^(-1)DRrR^(-1)ZMZ'R^(-1)X*
      //       + X*'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)X*
      //     = X*' FactoresR [1] R^(-1)X*

      FactoresR [0] := FRI.Producto(DRR);

      M1 := Factores [8].Producto(FactoresR [0]);
      FactoresR [1] := FactoresR [0].Resta(M1);
      FreeAndNil(M1);

      M1 := FactoresR [0].Producto(Factores [8]);
      FactoresR [1].Restar(M1);
      FreeAndNil(M1);

      M1 := Factores [8].Producto(FactoresR [0]).Multiplicar(Factores [8]);
      FactoresR [1].Sumar(M1);
      FreeAndNil(M1);

      FactoresR [2] := FactoresR [0].Producto(FactoresR [0]);

      M1 := Factores [8].Producto(FactoresR [2]);
      FactoresR [3] := FactoresR [2].Resta(M1);
      FreeAndNil(M1);

      M1 := FactoresR [0].Producto(Factores [8]).Multiplicar(FactoresR [0]);
      FactoresR [3].Restar(M1);
      FreeAndNil(M1);

      M1 := Factores [8].Producto(FactoresR [0]);
      M1.Multiplicar(M1);
      FactoresR [3].Sumar(M1);
      M1.Multiplicar(Factores [8]);
      FactoresR [3].Restar(M1);
      FreeAndNil(M1);

      M1 := FactoresR [2].Producto(Factores [8]);
      FactoresR [3].Restar(M1);
      FreeAndNil(M1);

      M1 := Factores [8].Producto(FactoresR [2]).Multiplicar(Factores [8]);
      FactoresR [3].Sumar(M1);
      FreeAndNil(M1);

      M1 := FactoresR [0].Producto(Factores [8]).Multiplicar(FactoresR [0]).Multiplicar(Factores [8]);
      FactoresR [3].Sumar(M1);
      FreeAndNil(M1);

      if CalcularH2 or CalcularG3 or CalcularH3 then
      begin
        M1 := XAT.Producto(FactoresR [1]);

        if CalcularH2 then
          H2R := M1.Producto(Factores [9]);

        if CalcularG3 or CalcularH3 then
          H3R := M1.Producto(Factores [10]);

        FreeAndNil(M1);
      end;

      // Gradiente

      if not ListoGradienteR then
      begin
        // [g1]r = tr(R^(-1)DRr)
        //         - tr(MZ'R^(-1)DRrR^(-1)Z)
        if CalcularG1 then
        begin
          G1 [I1, 1] := FactoresR [0].Traza;

          M1 := Factores [5].Producto(FactoresR [0]).Multiplicar(Factores [6]);
          G1 [I1, 1] := G1 [I1, 1] - M1.Traza;
          FreeAndNil(M1);
        end;

        // [g2]r = -r'R^(-1)DRrR^(-1)r
        //         + 2r'R^(-1)ZMZ'R^(-1)DRrR^(-1)r
        //         - r'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
        if CalcularG2 then
        begin
          M1 := R1T.Producto(FactoresR [0]).Multiplicar(Factores [9]);
          G2 [I1, 1] := -M1 [1, 1];
          FreeAndNil(M1);

          M1 := R1T.Producto(Factores [8]).Multiplicar(FactoresR [0]).Multiplicar(Factores [9]);
          G2 [I1, 1] := G2 [I1, 1] + 2.0 * M1 [1, 1];
          FreeAndNil(M1);

          M1 := R1T.Producto(Factores [8]).Multiplicar(FactoresR [0]).Multiplicar(Factores [8]).Multiplicar(Factores [9]);
          G2 [I1, 1] := G2 [I1, 1] - M1 [1, 1];
          FreeAndNil(M1);
        end;

        // [g3]r = -tr(H3r)
        if CalcularG3 then
          G3 [I1, 1] := -H3R.Traza;
      end;

      // Hessiano para s = r

      // [H1]rs = -tr(R^(-1)DRrR^(-1)DRs)
      //          + tr(MZ'R^(-1)DRrR^(-1)DRsR^(-1)Z)
      //          + tr(MZ'R^(-1)DRsR^(-1)DRrR^(-1)Z)
      //          - tr(MZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)Z)
      //          + tr(R^(-1)D2Rrs)
      //          - tr(MZ'R^(-1)D2RrsR^(-1)Z)
      if CalcularH1 then
      begin
        H1 [I1, I1] := -FactoresR [2].Traza;

        // Factor comun
        M1 := Factores [5].Producto(FactoresR [2]).Multiplicar(Factores [6]);
        H1 [I1, I1] := H1 [I1, I1] + 2.0 * M1.Traza;
        FreeAndNil(M1);

        M1 := Factores [5].Producto(FactoresR [0]).Multiplicar(Factores [8]).Multiplicar(FactoresR [0]).Multiplicar(Factores [6]);
        H1 [I1, I1] := H1 [I1, I1] - M1.Traza;
        FreeAndNil(M1);

        if not SeAnula then
        begin
          // La derivada segunda no es nula
          M1 := FRI.Producto(D2RRS);
          H1 [I1, I1] := H1 [I1, I1] + M1.Traza;

          M1.PreMultiplicar(Factores [5]).Multiplicar(Factores [6]);
          H1 [I1, I1] := H1 [I1, I1] - M1.Traza;
          FreeAndNil(M1);
        end;
      end;

      // [H2]rs = H2rs - 2H2r'H2s

      // H2s = H2s (r = s) calculado arriba

      // H2rs = 2r'( R^(-1)DRrR^(-1)DRs
      //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRs
      //             - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
      //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
      //             - R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
      //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
      //             + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
      //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ' ) R^(-1)r
      //        - r'R^(-1)D2RrsR^(-1)r
      //        + 2r'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
      //        - r'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
      //      = 2r' FactoresR [3] R^(-1)r
      //        - r'R^(-1)D2RrsR^(-1)r
      //        + 2r'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
      //        - r'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
      if CalcularH2 then
      begin
        H2RS := R1T.Producto(FactoresR [3]).Multiplicar(Factores [9]).Multiplicar(2.0);

        if not SeAnula then
        begin
          // La derivada segunda no es nula

          M1 := FRI.Producto(D2RRS);

          M2 := R1T.Producto(M1).Multiplicar(Factores [9]);
          H2RS.Restar(M2);

          M2 := R1T.Producto(M1).Multiplicar(Factores [8]).Multiplicar(Factores [9]).Multiplicar(2.0);
          H2RS.Sumar(M2);
          FreeAndNil(M2);

          M2 := R1T.Producto(Factores [8]).Multiplicar(M1).Multiplicar(Factores [8]).Multiplicar(Factores [9]);
          H2RS.Restar(M2);
          FreeAndNil(M2);

          FreeAndNil(M1);
        end;

        M1 := H2R.Transpuesta.Multiplicar(H2R);
        H2 [I1, I1] := H2RS [1, 1] - 2.0 * M1 [1, 1];
        FreeAndNil(M1);
      end;

      // [H3]rs = tr(H3rs - H3rH3s)

      // H3s = H3r (r = s) calculado arriba

      // H3rs = 2X*'( R^(-1)DRrR^(-1)DRs
      //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRs
      //             - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
      //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
      //             - R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
      //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
      //             + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
      //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ' ) R^(-1)X*
      //        - X*'R^(-1)D2RrsR^(-1)X*
      //        + 2X*'R^(-1)D2RrsR^(-1)ZMZR^(-1)X*
      //        - X*'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
      //      = 2X*' FactoresR [3] R^(-1)X*
      //        - X*'R^(-1)D2RrsR^(-1)X*
      //        + 2X*'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
      //        - X*'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
      if CalcularH3 then
      begin
        H3RS := XAT.Producto(FactoresR [3]).Multiplicar(Factores [10]).Multiplicar(2.0);

        if not SeAnula then
        begin
          // La derivada segunda no es nula

          M1 := FRI.Producto(D2RRS);

          M2 := XAT.Producto(M1).Multiplicar(Factores [10]);
          H2RS.Restar(M2);

          M2 := XAT.Producto(M1).Multiplicar(Factores [8]).Multiplicar(Factores [10]).Multiplicar(2.0);
          H2RS.Sumar(M2);
          FreeAndNil(M2);

          M2 := XAT.Producto(Factores [8]).Multiplicar(M1).Multiplicar(Factores [8]).Multiplicar(Factores [10]);
          H2RS.Restar(M2);
          FreeAndNil(M2);

          FreeAndNil(M1);
        end;

        M1 := H3R.Producto(H3R);
        // H3RS se pierde, pero no es necesario en adelante
        H3 [I1, I1] := H3RS.Restar(M1).Traza;
        FreeAndNil(M1);
      end;

      // Liberar factores innecesarios para continuar trabajando
      FreeAndNil(FactoresR [1]);
      FreeAndNil(FactoresR [2]);
      FreeAndNil(FactoresR [3]);
      FreeAndNil(H2RS);
      FreeAndNil(H3RS);

      // Gradiente y hessiano para r <> s

      // Parametros con respecto a R

      J1 := I1 + 1;
      for J := I + 1 to FInformacionTiempoEjecucionError.Parametros.Dimension - 1 do
      begin
        // Calcular derivadas para r y s
        ConstruirDerivadaPrimeraMatrizCovarianzaError(DRS, J);
        SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaError(D2RRS, I, J);

        // Calcular factores

        // FactoresR [1] = R^(-1)DRs
        // FactoresR [2] = R^(-1)DRs
        //                 - R^(-1)ZMZ'R^(-1)DRs
        //                 - R^(-1)DRsR^(-1)ZMZ'
        //                 + R^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
        // FactoresR [3] = R^(-1)DRrR^(-1)DRs
        //                 - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRs
        //                 - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //                 - R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //                 + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //                 + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
        //                 - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
        // H2s = X*'R^(-1)DRsR^(-1)r
        //       - X*'R^(-1)ZMZ'R^(-1)DRsR^(-1)r
        //       - X*'R^(-1)DRsR^(-1)ZMZ'R^(-1)r
        //       + X*'R^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'R^(-1)r
        //     = X*' FactoresR [2] R^(-1)r
        // H3s = X*'R^(-1)DRsR^(-1)X*
        //       - X*'R^(-1)ZMZ'R^(-1)DRsR^(-1)X*
        //       - X*'R^(-1)DRsR^(-1)ZMZ'R^(-1)X*
        //       + X*'R^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'R^(-1)X*
        //     = X*' FactoresR [2] R^(-1)X*

        FactoresR [1] := FRI.Producto(DRS);

        M1 := Factores [8].Producto(FactoresR [1]);
        FactoresR [2] := FactoresR [1].Resta(M1);
        FreeAndNil(M1);

        M1 := FactoresR [1].Producto(Factores [8]);
        FactoresR [2].Restar(M1);
        FreeAndNil(M1);

        M1 := Factores [8].Producto(FactoresR [1]).Multiplicar(Factores [8]);
        FactoresR [2].Sumar(M1);
        FreeAndNil(M1);

        FactoresR [3] := FactoresR [0].Producto(FactoresR [1]);

        M1 := Factores [8].Producto(FactoresR [0]).Multiplicar(FactoresR [1]);
        FactoresR [3].Restar(M1);
        FreeAndNil(M1);

        M1 := FactoresR [0].Producto(Factores [8]).Multiplicar(FactoresR [1]);
        FactoresR [3].Restar(M1);
        FreeAndNil(M1);

        M1 := Factores [8].Producto(FactoresR [0]).Multiplicar(Factores [8]).Multiplicar(FactoresR [1]);
        FactoresR [3].Sumar(M1);
        M1.Multiplicar(Factores [8]);
        FactoresR [3].Restar(M1);
        FreeAndNil(M1);

        M1 := FactoresR [0].Producto(FactoresR [1]).Multiplicar(Factores [8]);
        FactoresR [3].Restar(M1);
        FreeAndNil(M1);

        M1 := Factores [8].Producto(FactoresR [0]).Multiplicar(FactoresR [1]).Multiplicar(Factores [8]);
        FactoresR [3].Sumar(M1);
        FreeAndNil(M1);

        M1 := FactoresR [0].Producto(Factores [8]).Multiplicar(FactoresR [1]).Multiplicar(Factores [8]);
        FactoresR [3].Sumar(M1);
        FreeAndNil(M1);

        // Calcular los factores H2s, H3s (sobre R)

        if CalcularH1 or CalcularG3 or CalcularH3 then
        begin
          M1 := XAT.Producto(FactoresR [2]);

          if CalcularH2 then
            H2S := M1.Producto(Factores [9]);

          if CalcularG3 or CalcularH3 then
            H3S := M1.Producto(Factores [10]);

          FreeAndNil(M1);
        end;

        // Gradiente

        if not ListoGradienteR then
        begin
          // [g1]r = tr(R^(-1)DRr)
          //         - tr(MZ'R^(-1)DRrR^(-1)Z)
          if CalcularG1 then
          begin
            G1 [J1, 1] := FactoresR [1].Traza;

            M1 := Factores [5].Producto(FactoresR [1]).Multiplicar(Factores [6]);
            G1 [J1, 1] := G1 [J1, 1] - M1.Traza;
            FreeAndNil(M1);
          end;

          // [g2]r = -r'R^(-1)DRrR^(-1)r
          //         + 2r'R^(-1)ZMZ'R^(-1)DRrR^(-1)r
          //         - r'R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)r
          if CalcularG2 then
          begin
            M1 := R1T.Producto(FactoresR [1]).Multiplicar(Factores [9]);
            G2 [J1, 1] := -M1 [1, 1];
            FreeAndNil(M1);

            M1 := R1T.Producto(Factores [8]).Multiplicar(FactoresR [1]).Multiplicar(Factores [9]);
            G2 [J1, 1] := G2 [J1, 1] + 2.0 * M1 [1, 1];
            FreeAndNil(M1);

            M1 := R1T.Producto(Factores [8]).Multiplicar(FactoresR [1]).Multiplicar(Factores [8]).Multiplicar(Factores [9]);
            G2 [J1, 1] := G2 [J1, 1] - M1 [1, 1];
            FreeAndNil(M1);
          end;

          // [g3]r = -tr(H3r)
          if CalcularG3 then
            G3 [J1, 1] := -H3S.Traza;
        end;

        // Hessiano para s <> r

        // [H1]rs = -tr(R^(-1)DRrR^(-1)DRs)
        //          + tr(MZ'R^(-1)DRrR^(-1)DRsR^(-1)Z)
        //          + tr(MZ'R^(-1)DRsR^(-1)DRrR^(-1)Z)
        //          - tr(MZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)Z)
        //          + tr(R^(-1)D2Rrs)
        //          - tr(MZ'R^(-1)D2RrsR^(-1)Z)
        if CalcularH1 then
        begin
          M1 := FactoresR [0].Producto(FactoresR [1]);
          H1 [I1, J1] := -M1.Traza;
          FreeAndNil(M1);

          M1 := Factores [5].Producto(FactoresR [0]).Multiplicar(FactoresR [1]).Multiplicar(Factores [6]);
          H1 [I1, J1] := H1 [I1, J1] + M1.Traza;
          FreeAndNil(M1);

          M1 := Factores [5].Producto(FactoresR [1]).Multiplicar(FactoresR [0]).Multiplicar(Factores [6]);
          H1 [I1, J1] := H1 [I1, J1] + M1.Traza;
          FreeAndNil(M1);

          M1 := Factores [5].Producto(FactoresR [0]).Multiplicar(Factores [8]).Multiplicar(FactoresR [1]).Multiplicar(Factores [6]);
          H1 [I1, J1] := H1 [I1, J1] - M1.Traza;
          FreeAndNil(M1);

          if not SeAnula then
          begin
            // La derivada segunda no es nula
            M1 := FRI.Producto(D2RRS);
            H1 [I1, J1] := H1 [I1, J1] + M1.Traza;

            M1.PreMultiplicar(Factores [5]).Multiplicar(Factores [6]);
            H1 [I1, J1] := H1 [I1, J1] - M1.Traza;
            FreeAndNil(M1);
          end;

          H1 [J1, I1] := H1 [I1, J1];
        end;

        // [H2]rs = H2rs - 2H2r'H2s

        // H2rs = 2r'( R^(-1)DRrR^(-1)DRs
        //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRs
        //             - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //             - R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //             + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
        //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ' ) R^(-1)r
        //        - r'R^(-1)D2RrsR^(-1)r
        //        + 2r'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
        //        - r'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
        //      = 2r' FactoresR [3] R^(-1)r
        //        - r'R^(-1)D2RrsR^(-1)r
        //        + 2r'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r
        //        - r'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)r

        if CalcularH2 then
        begin
          H2RS := R1T.Producto(FactoresR [3]).Multiplicar(Factores [9]).Multiplicar(2.0);

          if not SeAnula then
          begin
            // La derivada segunda no es nula

            M1 := FRI.Producto(D2RRS);

            M2 := R1T.Producto(M1).Multiplicar(Factores [9]);
            H2RS.Restar(M2);

            M2 := R1T.Producto(M1).Multiplicar(Factores [8]).Multiplicar(Factores [9]).Multiplicar(2.0);
            H2RS.Sumar(M2);
            FreeAndNil(M2);

            M2 := R1T.Producto(Factores [8]).Multiplicar(M1).Multiplicar(Factores [8]).Multiplicar(Factores [9]);
            H2RS.Restar(M2);
            FreeAndNil(M2);

            FreeAndNil(M1);
          end;

          M1 := H2R.Transpuesta.Multiplicar(H2S);
          H2 [I1, J1] := H2RS [1, 1] - 2.0 * M1 [1, 1];
          FreeAndNil(M1);

          H2 [J1, I1] := H2 [I1, J1];
        end;

        // [H3]rs = tr(H3rs - H3rH3s)

        // H2rs = 2X*'( R^(-1)DRrR^(-1)DRs
        //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)DRs
        //             - R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRs
        //             - R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //             + R^(-1)ZMZ'R^(-1)DRrR^(-1)DRsR^(-1)ZMZ'
        //             + R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ'
        //             - R^(-1)ZMZ'R^(-1)DRrR^(-1)ZMZ'R^(-1)DRsR^(-1)ZMZ' ) R^(-1)X*
        //        - X*'R^(-1)D2RrsR^(-1)X*
        //        + 2X*'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
        //        - X*'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
        //      = 2X*' FactoresR [3] R^(-1)X*
        //        - X*'R^(-1)D2RrsR^(-1)X*
        //        + 2X*'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*
        //        - X*'R^(-1)ZMZ'R^(-1)D2RrsR^(-1)ZMZ'R^(-1)X*

        if CalcularH3 then
        begin
          H3RS := XAT.Producto(FactoresR [3]).Multiplicar(Factores [10]).Multiplicar(2.0);

          if not SeAnula then
          begin
            // La derivada segunda no es nula

            M1 := FRI.Producto(D2RRS);

            M2 := XAT.Producto(M1).Multiplicar(Factores [10]);
            H2RS.Restar(M2);

            M2 := XAT.Producto(M1).Multiplicar(Factores [8]).Multiplicar(Factores [10]).Multiplicar(2.0);
            H2RS.Sumar(M2);
            FreeAndNil(M2);

            M2 := XAT.Producto(Factores [8]).Multiplicar(M1).Multiplicar(Factores [8]).Multiplicar(Factores [10]);
            H2RS.Restar(M2);
            FreeAndNil(M2);

            FreeAndNil(M1);
          end;

          M1 := H3R.Producto(H3S);
          // H3RS se pierde, pero no es necesario en adelante
          H3 [I1, J1] := H3RS.Restar(M1).Traza;
          FreeAndNil(M1);

          H3 [J1, I1] := H3 [I1, J1];
        end;

        // Liberar factores
        FreeAndNil(FactoresR [1]);
        FreeAndNil(FactoresR [2]);
        FreeAndNil(FactoresR [3]);
        FreeAndNil(FactoresR [4]);
        FreeAndNil(H2S);
        FreeAndNil(H2RS);
        FreeAndNil(H3S);
        FreeAndNil(H3RS);

        Inc(J1);
      end;

      // Luego del recorrido del bucle interno sobre los parametros de R no es
      // necesario calcular mas componentes del gradiente respecto de R.
      ListoGradienteR := True;

      // Liberar factores
      for J := Low(FactoresR) to High(FactoresR) do
        FreeAndNil(FactoresR [J]);
      FreeAndNil(H2R);
      FreeAndNil(H3R);

      Inc(I1);
    end;
  end
  else
  begin
    // El modelo no posee efectos aleatorios. Notar que no es necesario tener
    // en cuenta parametros sobre G (no hay ninguno) y  que se modifican las
    // expresiones analiticas teniendo en cuenta que Z es nula, eliminando la
    // mayoria de los factores.

    // Calcular dimensiones (de utilidad a la hora de extraer submatrices)
    P := FX.CantidadColumnas;
    Q := 0;
    N := FY.CantidadFilas;

    // Calcular factores generales (no dependientes de un parametro en particular)
    SetLength(Factores, 16);

    // Factores [0]  = R^(-1)r
    // Factores [1] = R^(-1)X*
    Factores [0] := FRI.Producto(R1);
    Factores [1] := FRI.Producto(XA);

    // Asignar matrices para derivadas
    DRR := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);
    DRS := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);
    D2RRS := TUaMatriz.Create(FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza, FInformacionTiempoEjecucionError.CantidadFilasMatrizCovarianza);

    // Asignar arreglos de factores
    SetLength(FactoresR, 8);

    // Recorrer conjunto de parametros

    I1 := 1;
    ListoGradienteR := false;

    // Parametros con respecto a R
    for I := 0 to FInformacionTiempoEjecucionError.Parametros.Dimension - 1 do
    begin
      // Calcular derivadas para r
      ConstruirDerivadaPrimeraMatrizCovarianzaError(DRR, I);
      SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaError(D2RRS, I, I);

      // Calcular factores

      // FactoresR [0] = R^(-1)DRr

      // H2R = X*'R^(-1)DRrR^(-1)r
      // H3R = X*'R^(-1)DRrR^(-1)X*

      FactoresR [0] := FRI.Producto(DRR);

      if CalcularH2 then
        H2R := XAT.Producto(FactoresR [0]).Multiplicar(Factores [0]);

      if CalcularH3 or CalcularG3 then
        H3R := XAT.Producto(FactoresR [0]).Multiplicar(Factores [1]);

      // Gradiente

      if not ListoGradienteR then
      begin
        // [g1]r = tr(R^(-1)DRr)
        if CalcularG1 then
          G1 [I1, 1] := FactoresR [0].Traza;

        // [g2]r = -r'R^(-1)DRrR^(-1)r
        if CalcularG2 then
        begin
          M1 := R1T.Producto(FactoresR [0]).Multiplicar(Factores [0]);
          G2 [I1, 1] := -M1 [1, 1];
          FreeAndNil(M1);
        end;

        // [g3]r = -tr(H3r)
        if CalcularG3 then
          G3 [I1, 1] := -H3R.Traza;
      end;

      // Hessiano para s = r

      // [H1]rs = -tr(R^(-1)DRrR^(-1)DRs) + tr(R^(-1)D2Rrs)

      if CalcularH1 then
      begin
        M1 := FactoresR [0].Producto(FactoresR [0]);
        H1 [I1, I1] := -M1.Traza;
        FreeAndNil(M1);

        if not SeAnula then
        begin
          M1 := FRI.Producto(D2RRS);
          H1 [I1, I1] := H1 [I1, I1] + M1.Traza;
          FreeAndNil(M1);
        end;
      end;

      // [H2]rs = H2rs - 2H2r'H2s

      // H2s = H2s (r = s) calculado arriba

      // H2RS = 2r'R^(-1)DRrR^(-1)DRsR^(-1)r - r'R^(-1)D2RrsR^(-1)r

      if CalcularH2 then
      begin
        H2RS := R1T.Producto(FactoresR [0]).Multiplicar(FactoresR [0]).Multiplicar(Factores [0]).Multiplicar(2.0);

        if not SeAnula then
        begin
          M1 := R1T.Producto(FRI).Multiplicar(D2RRS).Multiplicar(Factores [0]);
          H2RS.Restar(M1);
          FreeAndNil(M1);
        end;

        M1 := H2R.Transpuesta.Multiplicar(H2R);
        H2 [I1, I1] := H2RS [1, 1] - 2.0 * M1 [1, 1];
        FreeAndNil(M1);
      end;

      // [H3]rs = tr(H3rs - H3rH3s)

      // H3s = H3r (r = s) calculado arriba

      // H3RS = 2X*'R^(-1)DRrR^(-1)DRsR^(-1)X* - X*'R^(-1)D2RrsR^(-1)X*

      if CalcularH3 then
      begin
        H3RS := XAT.Producto(FactoresR [0]).Multiplicar(FactoresR [0]).Multiplicar(Factores [1]).Multiplicar(2.0);

        if not SeAnula then
        begin
          M1 := XAT.Producto(FRI).Multiplicar(D2RRS).Multiplicar(Factores [1]);
          H3RS.Restar(M1);
          FreeAndNil(M1);
        end;

        M1 := H3R.Producto(H3R);
        // Se pierde H3RS, pero no es necesario en adelante
        H3 [I1, I1] := H3RS.Restar(M1).Traza;
      end;

      // Liberar factores innecesarios para continuar trabajando
      FreeAndNil(H2RS);
      FreeAndNil(H3RS);

      // Gradiente y hessiano para r <> s

      // Parametros con respecto a R

      J1 := I1 + 1;
      for J := I + 1 to FInformacionTiempoEjecucionError.Parametros.Dimension - 1 do
      begin
        // Calcular derivadas para r y s
        ConstruirDerivadaPrimeraMatrizCovarianzaError(DRS, J);
        SeAnula := ConstruirDerivadaSegundaMatrizCovarianzaError(D2RRS, I, J);

        // Calcular factores

        // FactoresR [1] = R^(-1)DRs

        // H2S = X*'R^(-1)DRsR^(-1)r
        // H3S  = X*'R^(-1)DRsR^(-1)X*

        FactoresR [1] := FRI.Producto(DRS);

        if CalcularH2 then
          H2S := XAT.Producto(FactoresR [1]).Multiplicar(Factores [0]);

        if CalcularH3 or CalcularG3 then
          H3S := XAT.Producto(FactoresR [1]).Multiplicar(Factores [1]);

        // Gradiente

        if not ListoGradienteR then
        begin
          // [g1]r = tr(R^(-1)DRr)
          if CalcularG1 then
            G1 [J1, 1] := FactoresR [1].Traza;

          // [g2]r = -r'R^(-1)DRrR^(-1)r
          if CalcularG2 then
          begin
            M1 := R1T.Producto(FactoresR [1]).Multiplicar(Factores [0]);
            G2 [J1, 1] := -M1 [1, 1];
            FreeAndNil(M1);
          end;

          // [g3]r = -tr(H3r)
          if CalcularG3 then
            G3 [J1, 1] := -H3S.Traza;
        end;

        // Hessiano para s <> r

        // [H1]rs = -tr(R^(-1)DRrR^(-1)DRs) + tr(R^(-1)D2Rrs)

        if CalcularH1 then
        begin
          M1 := FactoresR [0].Producto(FactoresR [1]);
          H1 [I1, J1] := -M1.Traza;
          FreeAndNil(M1);

          if not SeAnula then
          begin
            M1 := FRI.Producto(D2RRS);
            H1 [I1, J1] := H1 [I1, J1] + M1.Traza;
            FreeAndNil(M1);
          end;

          H1 [J1, I1] := H1 [I1, J1];
        end;

        // [H2]rs = H2rs - 2H2r'H2s

        // H2RS = 2r'R^(-1)DRrR^(-1)DRsR^(-1)r - r'R^(-1)D2RrsR^(-1)r

        if CalcularH2 then
        begin
          H2RS := R1T.Producto(FactoresR [0]).Multiplicar(FactoresR [1]).Multiplicar(Factores [0]).Multiplicar(2.0);

          if not SeAnula then
          begin
            M1 := R1T.Producto(FRI).Multiplicar(D2RRS).Multiplicar(Factores [0]);
            H2RS.Restar(M1);
            FreeAndNil(M1);
          end;

          M1 := H2R.Transpuesta.Multiplicar(H2S);
          H2 [I1, J1] := H2RS [1, 1] - 2.0 * M1 [1, 1];
          FreeAndNil(M1);

          H2 [J1, I1] := H2 [I1, J1];
        end;

        // [H3]rs = tr(H3rs - H3rH3s)

        // H3RS = 2X*'R^(-1)DRrR^(-1)DRsR^(-1)X* - X*'R^(-1)D2RrsR^(-1)X*

        if CalcularH3 then
        begin
          H3RS := XAT.Producto(FactoresR [0]).Multiplicar(FactoresR [1]).Multiplicar(Factores [1]).Multiplicar(2.0);

          if not SeAnula then
          begin
            M1 := XAT.Producto(FRI).Multiplicar(D2RRS).Multiplicar(Factores [1]);
            H3RS.Restar(M1);
            FreeAndNil(M1);
          end;

          M1 := H3R.Producto(H3S);
          // Se pierde H3RS, pero no es necesario en adelante
          H3 [I1, J1] := H3RS.Restar(M1).Traza;

          H3 [J1, I1] := H3 [I1, J1];
        end;

        // Liberar factores
        FreeAndNil(FactoresR [1]);
        FreeAndNil(H2S);
        FreeAndNil(H2RS);
        FreeAndNil(H3S);
        FreeAndNil(H3RS);

        Inc(J1);
      end;

      // Luego del recorrido del bucle interno sobre los parametros de R no es
      // necesario calcular mas componentes del gradiente respecto de R.
      ListoGradienteR := True;

      // Liberar factores
      for J := Low(FactoresR) to High(FactoresR) do
        FreeAndNil(FactoresR [J]);
      FreeAndNil(H2R);
      FreeAndNil(H3R);

      Inc(I1);
    end;
  end;

  // Liberar factores y matrices auxiliares
  for I := Low(Factores) to High(Factores) do
    FreeAndNil(Factores [I]);
  Factores := nil;
  FactoresG := nil;
  FactoresR := nil;

  FreeAndNil(W1);
  FreeAndNil(XA);
  FreeAndNil(XAT);
  FreeAndNil(C);
  FreeAndNil(CT);
  FreeAndNil(R1);
  FreeAndNil(R1T);
  FreeAndNil(M);
  FreeAndNil(M1);
  FreeAndNil(M2);
  FreeAndNil(M3);
  FreeAndNil(M4);
  FreeAndNil(DGR);
  FreeAndNil(DGS);
  FreeAndNil(DRR);
  FreeAndNil(DRS);
  FreeAndNil(D2GRS);
  FreeAndNil(D2RRS);
  FreeAndNil(H2R);
  FreeAndNil(H2S);
  FreeAndNil(H3R);
  FreeAndNil(H3S);
  FreeAndNil(H2RS);
  FreeAndNil(H3RS);
  FreeAndNil(A1R);
  FreeAndNil(A2R);
  FreeAndNil(B1);

  Assert(not Assigned(W1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(W1)');
  Assert(not Assigned(XA), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(XA)');
  Assert(not Assigned(XAT), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(XAT)');
  Assert(not Assigned(C), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(C)');
  Assert(not Assigned(CT), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(CT)');
  Assert(not Assigned(R1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(R1)');
  Assert(not Assigned(R1T), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(R1T)');
  Assert(not Assigned(M), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(M)');
  Assert(not Assigned(M1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(M1)');
  Assert(not Assigned(M2), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(M2)');
  Assert(not Assigned(M3), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(M3)');
  Assert(not Assigned(M4), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(M4)');
  Assert(not Assigned(DGR), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(DGR)');
  Assert(not Assigned(DGS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(DGS)');
  Assert(not Assigned(DRR), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(DRR)');
  Assert(not Assigned(DRS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(DRS)');
  Assert(not Assigned(D2GRS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(D2GRS)');
  Assert(not Assigned(D2RRS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(D2RRS)');
  Assert(not Assigned(H2R), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H2R)');
  Assert(not Assigned(H2S), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H2S)');
  Assert(not Assigned(H3R), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H3R)');
  Assert(not Assigned(H3S), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H3S)');
  Assert(not Assigned(H2RS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H2RS)');
  Assert(not Assigned(H3RS), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(H3RS)');
  Assert(not Assigned(A1R), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(A1R)');
  Assert(not Assigned(A2R), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(A2R)');
  Assert(not Assigned(B1), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(B1)');
  Assert(not Assigned(Factores), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(Factores)');
  Assert(not Assigned(FactoresG), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(FactoresG)');
  Assert(not Assigned(FactoresR), 'TProcedimiento.CalcularTerminosGradienteYHessiano: not Assigned(FactoresR)');
end { TProcedimiento.CalcularTerminosGradienteYHessiano };

procedure TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos(var L1, L2, L3: TUaReal; var WSA, WSD, W2SA, B, BT: TUaMatriz; CalcularWSA: Boolean = True; CalcularWSD: Boolean = True; CalcularW2SA: Boolean = True; CalcularB: Boolean = True);
var
  I, P, Q: Integer;
  W, W0, W1, W2, M1, M2: TUaMatriz;
  D: TUaVector;
  R1, R2, LnDeterminanteR: TUaReal;
begin { TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos }
  Assert(not Assigned(WSA), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(WSA)');
  Assert(not Assigned(WSD), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(WSD)');
  Assert(not Assigned(W2SA), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(W2SA)');
  Assert(not Assigned(B), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(B)');
  Assert(not Assigned(BT), 'TProcedimiento.CalcularTerminosValorFuncionObjetivoYSubproductos: not Assigned(BT)');

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

    // Almacenar (I + L'Z'R^(-1)ZL)^(-)
    if CalcularWSA then
      WSA := W.SubMatriz(1, 1, Q, Q);

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
    if CalcularWSD then
      WSD := W1.Copia;
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

    // Almacenar (X'V^(-1)X)^(-1), B y B'
    if CalcularW2SA then
      W2SA := W2.SubMatriz(1, 1, P, P);

    if CalcularB then
    begin
      B := W2.SubMatriz(1, P + 1, P, P + 1);
      BT := W2.SubMatriz(P + 1, 1, P + 1, P);
    end;

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

    // Almacenar (X'V^(-1)X)^(-1), B y B'
    if CalcularW2SA then
      W2SA := W2.SubMatriz(1, 1, P, P);

    if CalcularB then
    begin
      B := W2.SubMatriz(1, P + 1, P, P + 1);
      BT := W2.SubMatriz(P + 1, 1, P + 1, P);
    end;

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

procedure TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz; Parametro: Integer);
var
  Listo: Boolean;
  I, J, IndiceFila: Integer;
  InformacionGrupo: TInformacionTiempoEjecucionGrupoEfectosAleatorios;
begin { TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios }
  // Construir la matriz, de ser necesario
  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FCantidadFilasMatrizCovarianzaEfectosAleatorios, FCantidadFilasMatrizCovarianzaEfectosAleatorios)
  else
    // Llamada subsiguiente, limpiar
    Matriz.AsignarCero;

  // Verficar que el indice de parametro sea valido
  if Parametro >= FCantidadParametrosEfectosAleatorios then
    Parametro := Parametro mod FCantidadParametrosEfectosAleatorios;

  // Determinar el grupo de efectos aleatorios al que pertenece el parametro
  IndiceFila := 1;
  I := 0;
  J := 0;
  Listo := false;
  InformacionGrupo := nil;
  while (I <= FInformacionTiempoEjecucionEfectosAleatorios.Count - 1) and (not Listo) do
  begin
    InformacionGrupo := FInformacionTiempoEjecucionEfectosAleatorios [I];

    if Parametro <= (J + InformacionGrupo.Parametros.Dimension - 1) then
    begin
      Listo := true;
    end
    else
    begin
      J := J + InformacionGrupo.Parametros.Dimension;
      IndiceFila := IndiceFila + InformacionGrupo.CantidadFilasMatrizCovarianza;
    end;

    Inc(I);
  end;

  Parametro := Parametro - J;
  ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios(InformacionGrupo, Matriz, Parametro, IndiceFila);
end { TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios };

procedure TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError(var Matriz: TUaMatriz; Parametro: Integer);
var
  I, J, K: Integer;
  Listo: Boolean;
  IndiceFila, IndiceColumna, IndiceFilaSubmatriz, IndiceColumnaSubmatriz: Integer;
  IndiceParametro, IndicePrimerParametro, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimentalFila, IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna: Integer;
  IndiceNivelCriterioOrdenamiento, IndiceNivelCriterioOrdenamientoColumna: Integer;
  M, N, M1, SubmatrizUnidadExperimental, SubmatrizCriterioAgrupamientoUnidadExperimental: TUaMatriz;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  InformacionError: TInformacionTiempoEjecucionError;
  IndicesUltimaObservacion, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental: TArregloEnteros;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError }
  // Construir la matriz, de ser necesario
  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FModelo.Datos.Valores.CantidadFilas, FModelo.Datos.Valores.CantidadFilas)
  else
    // Llamada subsiguiente, limpiar
    Matriz.AsignarCero;

  // Inicializar referencias
  IndicesUltimaObservacion := nil;
  IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental := nil;
  M := nil;
  N := nil;
  M1 := nil;
  SubmatrizUnidadExperimental := nil;
  SubmatrizCriterioAgrupamientoUnidadExperimental := nil;

  InformacionError := FInformacionTiempoEjecucionError;

  // Verficar que el indice de parametro sea valido
  if Parametro >= InformacionError.Parametros.Dimension then
    Parametro := Parametro mod InformacionError.Parametros.Dimension;

  if InformacionError.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    // Se definio criterio agrupamiento unidad experimental

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental;

    // Identificar el indice de nivel de criterio agrupamiento unidad experimental
    IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;
    I := InformacionCriterio.IndicesNiveles.Bajo;
    J := Parametro + 1;
    IndicePrimerParametro := 0;
    Listo := false;
    while (I <= InformacionCriterio.IndicesNiveles.Alto) and (not Listo) do
    begin
      if J <= IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] then
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimental := I;
        Listo := true;
        Parametro := Parametro - IndicePrimerParametro;
      end
      else
      begin
        IndicePrimerParametro := IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
        Inc(I);
      end;
    end;

    // Construir submatriz
    SubmatrizCriterioAgrupamientoUnidadExperimental := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza
      for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Verificar si se deriva sobre la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Derivar sobre la varianza
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 1.0;
      end
      else
      begin
        // Derivar sobre la covarianza
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
            SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := 1.0;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      Listo := false;
      while (I <= InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta fila
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          Inc(IndiceFilaSubmatriz);
          I := I + K;
          Dec(K);
        end;
      end;

      // Ingresar el valor a la submatriz
      SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := Parametro + 1;
      IndiceColumnaSubmatriz := Parametro + 1;

      // Ingresar el valor a la submatriz
      SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] / 2) * (2 * InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] - InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A
        K := J - I;

        // Ingresar valor a la submatriz
        SubmatrizCriterioAgrupamientoUnidadExperimental [K, K] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
        N := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

        // Construir A y su derivada

        IndiceParametro := IndicePrimerParametro;
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if J <= I then
            begin
              if (IndiceParametro mod InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Ingresar los valores a la submatriz
        SubmatrizCriterioAgrupamientoUnidadExperimental.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic Sin Diagonal

      // Construir matrices auxiliares
      M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
      N := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

      // Construir A y su derivada

      IndiceParametro := IndicePrimerParametro;
      for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          if J <= I then
          begin
            if (IndiceParametro mod InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
              N [I, J] := 1.0;

            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Calcular dA/dp*A'
      M.Transponer;
      N.Multiplicar(M);

      // Calcular dA/dp*A' + A*dA'/dp
      M1 := N.Transpuesta;
      N.Sumar(M1);
      FreeAndNil(M1);

      // Ingresar los valores a la submatriz
      SubmatrizCriterioAgrupamientoUnidadExperimental.Copiar(N);

      // Liberar matrices auxiliarers
      FreeAndNil(M);
      FreeAndNil(N);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic Diagonal Escalar

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] / 2) * (2 * InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] - InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A

        // Ingresar valor a la submatriz
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
        N := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

        // Construir A y su derivada

        IndiceParametro := IndicePrimerParametro;
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            if J <= I then
            begin
              if (IndiceParametro mod InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Ingresar los valores a la submatriz
        SubmatrizCriterioAgrupamientoUnidadExperimental.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      Listo := false;
      while (I <= InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          I := I + K;
          K := Min(InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] - IndiceFilaSubmatriz);
          Inc(IndiceFilaSubmatriz);
        end;
      end;

      // Ingresar el valor a la submatriz
      SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      // Determinar el indice del primer parametro
      IndiceParametro := IndicePrimerParametro;

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Sigma cuadrado
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 1.0;

          R1 := 1.0;
          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := R1;
          end;
        end;
      end
      else
      begin
        // Ro
        for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
        begin
          SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 0.0;

          R1 := 1.0;
          K := 1;
          for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
          begin
            R2 := InformacionError.Parametros [IndiceParametro] * K * R1;
            SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := R2 ;
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            Inc(K);
          end;
        end;
      end;
    end;

    // Verificar definicion de unidad experimental
    if InformacionError.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental y criterio agrupamiento unidad experimental

      // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental
      IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
      IndicesUltimaObservacion.PonerA(0);

      IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
      IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.PonerA(0);

      // Verificar definicion de criterio de ordenamiento
      if InformacionError.TieneCriterioOrdenamiento then
      begin
        // Se definio criterio de ordenamiento

        // Construir submatriz de unidad experimental
        SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones, InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones);

        // Construir la matriz de covarianza por cada nivel de la unidad experimental
        for IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Limpiar submatriz de unidad experimental
          SubmatrizUnidadExperimental.AsignarCero;

          for IndiceFila := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
          begin
            IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceFila]];

            // Verificar que el indice del nivel de criterio de agrupamiento coincida con el del parametro
            if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
            begin
              // Ingresar valor en la diagonal principal de la submatriz
              SubmatrizUnidadExperimental [IndiceFila + 1, IndiceFila + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1];

              // Ingresar los valores del resto de las columnas
              J := IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1;
              for IndiceColumna := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] - 1 do
              begin
                IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceColumna]];

                // Verificar coincidencia de niveles de criterio agrupamiento unidad experimental
                if IndiceNivelCriterioAgrupamientoUnidadExperimental = IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna then
                begin
                  // El nivel coincide, ingresar los valores
                  SubmatrizUnidadExperimental [IndiceFila + 1, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                  Inc(J);
                end;
              end;

              // Incrementar el indices de la ultima observacion visitada
              IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.Incrementar(IndiceNivelUnidadExperimental);
            end;

            // Incrementar el indices de la ultima observacion visitada
            IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
          end;

          // Ingresar los valores (reordenados) a la derivada de la matriz de covarianza

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

        // Liberar matrices auxiliares
        FreeAndNil(SubmatrizUnidadExperimental)
      end
      else
      begin
        // No se definio criterio de ordenamiento

        // Ingresar los valores a la matriz
        for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];
          IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

          // Verificar coincidencia del nivel de criterio agrupamiento unidad experimental
          if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
          begin
            // Ingresar valor en la diagonal principal
            Matriz [IndiceFila, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1];

            // Ingresar los valores del resto de las columnas
            J := IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1;
            for I := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
            begin
              IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];
              IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceColumna];

              // Verificar coincidencia de nivel de criterio agrupamiento unidad experimental
              if IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna = IndiceNivelCriterioAgrupamientoUnidadExperimental then
              begin
                // Los niveles coinciden, ingresar los valores
                Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                Inc(J);
              end;
            end;

            // Incrementar el indices de la ultima observacion visitada
            IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.Incrementar(IndiceNivelUnidadExperimental);
          end;

          // Incrementar el indices de la ultima observacion visitada
          IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
        end;
      end;

      // Liberar arreglos de indices ultima observacion
      FreeAndNil(IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental);
    end
    else
    begin
      // No se definio unidad experimental y se definio criterio agrupamiento unidad experimental

      // Se ignora el criterio de ordenamiento

      // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental
      IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
      IndicesUltimaObservacion.PonerA(0);

      // Ingresar los valores a la matriz
      for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

        // Verificar coincidencia del nivel de criterio agrupamiento unidad experimental
        if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
        begin
          // Ingresar valor en la diagonal principal
          Matriz [IndiceFila, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1];

          // Ingresar los valores del resto de las columnas
          for I := IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
          begin
            IndiceColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, I];

            Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
            Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
          end;

          // Incrementar el indice de la ultima observacion visitada
          IndicesUltimaObservacion.Incrementar(IndiceNivelCriterioAgrupamientoUnidadExperimental);
        end;
      end;
    end;

    // Liberar matrices auxiliares
    FreeAndNil(SubmatrizCriterioAgrupamientoUnidadExperimental);

    // Liberar arreglos de indices ultima observacion
    FreeAndNil(IndicesUltimaObservacion);
  end
  else if InformacionError.TieneUnidadExperimental then
  begin
    // Se definio unidad experimental y no se definio criterio de agrupamiento

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionUnidadExperimental;

    // Construir submatriz
    SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        SubmatrizUnidadExperimental [I, I] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Verificar si se deriva sobre la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Derivar sobre la varianza
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
          SubmatrizUnidadExperimental [I, I] := 1.0;
      end
      else
      begin
        // Derivar sobre la covarianza
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
          for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
            SubmatrizUnidadExperimental [I, J] := 1.0;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionCriterio.CantidadMaximaObservaciones;
      Listo := false;
      while (I <= InformacionError.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta fila
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          Inc(IndiceFilaSubmatriz);
          I := I + K;
          Dec(K);
        end;
      end;

      // Ingresar el valor a la submatriz
      SubmatrizUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := Parametro + 1;
      IndiceColumnaSubmatriz := Parametro + 1;

      // Ingresar el valor a la submatriz
      SubmatrizUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionCriterio.CantidadMaximaObservaciones - InformacionError.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A
        K := J - I;

        // Ingresar valor a la submatriz
        SubmatrizUnidadExperimental [K, K] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

        // Construir A y su derivada

        IndiceParametro := 0;
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Ingresar los valores a la submatriz
        SubmatrizUnidadExperimental.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic Sin Diagonal

      // Construir matrices auxiliares
      M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);
      N := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

      // Construir A y su derivada

      IndiceParametro := 0;
      for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            if IndiceParametro = Parametro then
              N [I, J] := 1.0;

            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Calcular dA/dp*A'
      M.Transponer;
      N.Multiplicar(M);

      // Calcular dA/dp*A' + A*dA'/dp
      M1 := N.Transpuesta;
      N.Sumar(M1);
      FreeAndNil(M1);

      // Ingresar los valores a la submatriz
      SubmatrizUnidadExperimental.Copiar(N);

      // Liberar matrices auxiliares
      FreeAndNil(M);
      FreeAndNil(N);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic Diagonal Escalar

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionCriterio.CantidadMaximaObservaciones - InformacionError.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A

        // Ingresar valor a la submatriz
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
          SubmatrizUnidadExperimental [I, I] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionError.OrdenEstructuraCovarianza);

        // Construir A y su derivada

        IndiceParametro := 0;
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        N.Sumar(N.Transponer);

        // Ingresar los valores a la submatriz
        SubmatrizUnidadExperimental.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionError.OrdenEstructuraCovarianza;
      Listo := false;
      while (I <= InformacionError.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          I := I + K;
          K := Min(InformacionError.OrdenEstructuraCovarianza, InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones - IndiceFilaSubmatriz);
          Inc(IndiceFilaSubmatriz);
        end;
      end;

      // Ingresar el valor a la submatriz
      SubmatrizUnidadExperimental [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      // Determinar el indice del primer parametro
      IndiceParametro := 0;

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Sigma cuadrado
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          SubmatrizUnidadExperimental [I, I] := 1.0;

          R1 := 1.0;
          for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
          begin
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            SubmatrizUnidadExperimental [I, J] := R1;
          end;
        end;
      end
      else
      begin
        // Ro
        for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
        begin
          SubmatrizUnidadExperimental [I, I] := 0.0;

          R1 := 1.0;
          K := 1;
          for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
          begin
            R2 := InformacionError.Parametros [IndiceParametro] * K * R1;
            SubmatrizUnidadExperimental [I, J] := R2 ;
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            Inc(K);
          end;
        end;
      end;
    end;

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

    // Liberar arreglos de ultima observacion
    FreeAndNil(IndicesUltimaObservacion);
  end
  else
  begin
    // No se definio ni unidad experimental ni criterio agrupamiento unidad experimental

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        Matriz [I, I] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      // Verificar si se deriva sobre la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Derivar sobre la varianza
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
          Matriz [I, I] := 1.0;
      end
      else
      begin
        // Derivar sobre la covarianza
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            Matriz [I, J] := 1.0;
            Matriz [J, I] := 1.0;
          end;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionError.CantidadFilasMatrizCovarianza;
      Listo := false;
      while (I <= InformacionError.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta fila
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          Inc(IndiceFilaSubmatriz);
          I := I + K;
          Dec(K);
        end;
      end;

      // Ingresar el valor a la submatriz
      Matriz [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
      Matriz [IndiceColumnaSubmatriz, IndiceFilaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := Parametro + 1;
      IndiceColumnaSubmatriz := Parametro + 1;

      // Ingresar el valor a la submatriz
      Matriz [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionError.CantidadFilasMatrizCovarianza - InformacionError.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A
        K := J - I;

        // Ingresar valor a la submatriz
        Matriz [K, K] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        IndiceParametro := 0;
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en N
        Matriz.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic Sin Diagonal

      // Construir matrices auxiliares
      M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);
      N := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

      // Construir A y su derivada

      IndiceParametro := 0;
      for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
      begin
        for J := 1 to InformacionError.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            if IndiceParametro = Parametro then
              N [I, J] := 1.0;

            M [I, J] := InformacionError.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Calcular dA/dp*A'
      M.Transponer;
      N.Multiplicar(M);

      // Calcular dA/dp*A' + A*dA'/dp
      M1 := N.Transpuesta;
      N.Sumar(M1);
      FreeAndNil(M1);

      // Ingresar los valores a la matriz
      Matriz.Copiar(N);

      // Liberar matrices auxiliares
      FreeAndNil(M);
      FreeAndNil(N);
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic Diagonal Escalar

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionError.CantidadFilasMatrizCovarianza - InformacionError.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro no vive en A

        // Ingresar valor a la submatriz
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
          Matriz [I, I] := 1.0;
      end
      else
      begin
        // El parametro vive en A

        // Construir matrices auxiliares
        M := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(InformacionError.CantidadFilasMatrizCovarianza, InformacionError.OrdenEstructuraCovarianza);

        // Construir A y su derivada

        IndiceParametro := 0;
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          for J := 1 to InformacionError.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := InformacionError.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Ingresar los valores a la matriz
        Matriz.Copiar(N);

        // Liberar matrices auxiliares
        FreeAndNil(M);
        FreeAndNil(N);
      end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      // Determinar la fila y columna del parametro dentro de la submatriz
      IndiceFilaSubmatriz := 1;
      IndiceColumnaSubmatriz := 1;
      I := 0;
      J := Parametro + 1;
      K := InformacionError.OrdenEstructuraCovarianza;
      Listo := false;
      while (I <= InformacionError.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          I := I + K;
          K := Min(InformacionError.OrdenEstructuraCovarianza, InformacionError.CantidadFilasMatrizCovarianza - IndiceFilaSubmatriz);
          Inc(IndiceFilaSubmatriz);
        end;
      end;

      // Ingresar el valor a la submatriz
      Matriz [IndiceFilaSubmatriz, IndiceColumnaSubmatriz] := 1.0;
      Matriz [IndiceColumnaSubmatriz, IndiceFilaSubmatriz] := 1.0;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      // Determinar el indice del primer parametro
      IndiceParametro := 0;

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Sigma cuadrado
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          Matriz [I, I] := 1.0;

          R1 := 1.0;
          for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            Matriz [I, J] := R1;
            Matriz [J, I] := R1;
          end;
        end;
      end
      else
      begin
        // Ro
        for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          Matriz [I, I] := 0.0;

          R1 := 1.0;
          K := 1;
          for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            R2 := InformacionError.Parametros [IndiceParametro] * K * R1;
            Matriz [I, J] := R2 ;
            Matriz [J, I] := R2 ;
            R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
            Inc(K);
          end;
        end;
      end;
    end;
  end;

  Assert(not Assigned(IndicesUltimaObservacion), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(IndicesUltimaObservacion)');
  Assert(not Assigned(IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental)');
  Assert(not Assigned(M), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(M)');
  Assert(not Assigned(N), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(N)');
  Assert(not Assigned(M1), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(M1)');
  Assert(not Assigned(SubmatrizUnidadExperimental), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(SubmatrizUnidadExperimental)');
  Assert(not Assigned(SubmatrizCriterioAgrupamientoUnidadExperimental), 'TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError: not Assigned(SubmatrizCriterioAgrupamientoUnidadExperimental)');
end { TProcedimiento.ConstruirDerivadaPrimeraMatrizCovarianzaError };

procedure TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; Parametro: Integer; const FilaInicial: Integer);
var
  Listo, ConstruirA: Boolean;
  I, J, K: Integer;
  M, N, M1: TUaMatriz;
  IndiceEfecto, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental, IndiceNivelEfecto: Integer;
  IndiceFila, IndiceFilaSubmatriz, IndiceColumnaSubmatriz, IndiceParametro: Integer;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios }
  // Inicializar referencias
  M := nil;
  N := nil;
  M1 := nil;

  // Inicializar valores de indices de matriz
  IndiceFila := FilaInicial;

  // Verificar que el indice del parametro sea valido
  if Parametro >= Informacion.Parametros.Dimension then
    Parametro := Parametro mod Informacion.Parametros.Dimension;

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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar el indice de efecto
        IndiceEfecto := Parametro;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar generacion de columnas nulas
          if Informacion.GenerarColumnasNulas then
          begin
            // Generar las columnas nulas

            // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
            for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
              IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles;

            // Ingresar los valores a la matriz
            for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles do
            begin
              Matriz [IndiceFila, IndiceFila] := 1.0;
              Inc(IndiceFila);
            end;
          end
          else
          begin
            // No generar las columnas nulas

            // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
            for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
              IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;

            // Ingresar los valores a la matriz
            for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes do
            begin
              Matriz [IndiceFila, IndiceFila] := 1.0;
              Inc(IndiceFila);
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Determinar el indice de efecto
        IndiceEfecto := Parametro;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar generacion de columnas nulas
          if Informacion.GenerarColumnasNulas then
          begin
            // Generar las columnas nulas

            // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
            for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
              IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles;

            // Ingresar los valores a la matriz
            for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles do
            begin
              Matriz [IndiceFila, IndiceFila] := 1.0;
              Inc(IndiceFila);
            end;
          end
          else
          begin
            // No generar las columnas nulas

            // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
            for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
              IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;

            // Ingresar los valores a la matriz
            for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes do
            begin
              Matriz [IndiceFila, IndiceFila] := 1.0;
              Inc(IndiceFila);
            end;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Determinar el indice de efecto
      IndiceEfecto := Parametro;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Verificar generacion de columnas nulas
      if Informacion.GenerarColumnasNulas then
      begin
        // Generar las columnas nulas

        // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
        for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
          IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles;

        // Ingresar los valores a la matriz
        for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles do
        begin
          Matriz [IndiceFila, IndiceFila] := 1.0;
          Inc(IndiceFila);
        end;
      end
      else
      begin
        // No generar las columnas nulas

        // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
        for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
          IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;

        // Ingresar los valores a la matriz
        for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes do
        begin
          Matriz [IndiceFila, IndiceFila] := 1.0;
          Inc(IndiceFila);
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Determinar el indice de efecto
      IndiceEfecto := Parametro;

      // Verificar generacion de columnas nulas
      if Informacion.GenerarColumnasNulas then
      begin
        // Generar las columnas nulas

        // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
        for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
          IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles;

        // Ingresar los valores a la matriz
        for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles do
        begin
          Matriz [IndiceFila, IndiceFila] := 1.0;
          Inc(IndiceFila);
        end;
      end
      else
      begin
        // No generar las columnas nulas

        // Sumar la cantidad de columnas (o filas) correspondiente a efectos anteriores
        for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to IndiceEfecto - 1 do
          IndiceFila := IndiceFila + Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;

        // Ingresar los valores a la matriz
        for IndiceNivelEfecto := 1 to Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [IndiceEfecto].CantidadNivelesInexistentes do
        begin
          Matriz [IndiceFila, IndiceFila] := 1.0;
          Inc(IndiceFila);
        end;
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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores a la matriz

          // Verificar si se debe ingresar la varianza o la covarianza
          if Parametro = 0 then
          begin
            // Varianza
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
          end
          else
          begin
            // Covarianza
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := 1.0;
                Matriz [IndiceFila + J, IndiceFila + I] := 1.0;
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
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores a la matriz

          // Verificar si se debe ingresar la varianza o la covarianza
          if Parametro = 0 then
          begin
            // Varianza
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
          end
          else
          begin
            // Covarianza
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := 1.0;
                Matriz [IndiceFila + J, IndiceFila + I] := 1.0;
              end;
            end;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Ingresar los valores a la matriz

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Varianza
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
      end
      else
      begin
        // Covarianza
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := 1.0;
            Matriz [IndiceFila + J, IndiceFila + I] := 1.0;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Ingresar los valores a la matriz

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Varianza
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
      end
      else
      begin
        // Covarianza
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := 1.0;
            Matriz [IndiceFila + J, IndiceFila + I] := 1.0;
          end;
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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar la fila y columna dentro de la submatriz
        IndiceFilaSubmatriz := 0;
        IndiceColumnaSubmatriz := 0;
        I := 0;
        J := Parametro + 1;
        K := Informacion.CantidadColumnasEfectos;
        Listo := false;
        while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
        begin
          if J <= I + K then
          begin
            // El parametro se encuentra en esta fila
            Listo := true;

            // Calcular el indice de columna dentro de la submatriz
            IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
          end
          else
          begin
            // El parametro se encuentra en otra columna, continuar buscando
            Inc(IndiceFilaSubmatriz);
            I := I + K;
            Dec(K);
          end;
        end;

        // Ingresar el valor en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar si el valor es en la diagonal principal e ingresar el valor
          if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
          end
          else
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
            Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Determinar la fila y columna dentro de la submatriz
        IndiceFilaSubmatriz := 0;
        IndiceColumnaSubmatriz := 0;
        I := 0;
        J := Parametro + 1;
        K := Informacion.CantidadColumnasEfectos;
        Listo := false;
        while (I <= Informacion.Parametros.Dimension) and (not Listo) do
        begin
          if J <= I + K then
          begin
            // El parametro se encuentra en esta columna
            Listo := true;

            // Calcular el indice de columna dentro de la submatriz
            IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1
          end
          else
          begin
            // El parametro se encuentra en otra columna, continuar buscando
            Inc(IndiceFilaSubmatriz);
            I := I + K;
            Dec(K);
          end;
        end;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar si el valor es en la diagonal principal e ingresar el valor
          if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
          end
          else
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
            Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Determinar la fila y columna dentro de la submatriz
      IndiceFilaSubmatriz := 0;
      IndiceColumnaSubmatriz := 0;
      I := 0;
      J := Parametro + 1;
      K := Informacion.CantidadColumnasEfectos;
      Listo := false;
      while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          Inc(IndiceFilaSubmatriz);
          I := I + K;
          Dec(K);
        end;
      end;

      // Verificar si el valor es en la diagonal principal e ingresar el valor
      if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
      end
      else
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
        Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Determinar la fila y columna dentro de la submatriz
      IndiceFilaSubmatriz := 0;
      IndiceColumnaSubmatriz := 0;
      I := 0;
      J := Parametro + 1;
      K := Informacion.CantidadColumnasEfectos;
      Listo := false;
      while (I <= Informacion.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          Inc(IndiceFilaSubmatriz);
          I := I + K;
          Dec(K);
        end;
      end;

      // Verificar si el valor es en la diagonal principal e ingresar el valor
      if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
      end
      else
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
        Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Ingresar el valor en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          Matriz [IndiceFila + Parametro, IndiceFila + Parametro] := 1.0;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          Matriz [IndiceFila + Parametro, IndiceFila + Parametro] := 1.0;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      Matriz [IndiceFila + Parametro, IndiceFila + Parametro] := 1.0;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      Matriz [IndiceFila + Parametro, IndiceFila + Parametro] := 1.0;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic') then
  begin
    // Estructura Factor Analytic

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar el indice del primer parametro
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := Parametro + 1;
        K := 0;
        if J > I then
        begin
          // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
          ConstruirA := false;
          K := J - I - 1;
        end
        else
        begin
          // Es necesario construir A
          ConstruirA := true;

          // Construir matrices auxiliares
          M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
          N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

          // Construir A y su derivada
          for I := 1 to Informacion.CantidadColumnasEfectos do
          begin
            for J := 1 to Informacion.OrdenEstructuraCovarianza do
            begin
              if J <= I then
              begin
                if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                  N [I, J] := 1.0;

                M [I, J] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;
          end;

          // Calcular dA/dp*A'
          M.Transponer;
          N.Multiplicar(M);

          // Calcular dA/dp*A' + A*dA'/dp
          M1 := N.Transpuesta;
          N.Sumar(M1);
          FreeAndNil(M1);

          // Liberar matriz auxiliar
          FreeAndNil(M);
        end;

        // Ingresar los valores en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de la submatriz
          if ConstruirA then
          begin
            // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en N
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
              end;
            end;
          end
          else
          begin
            // Ingresar los valores de dD/dp
            Matriz [IndiceFila + K, IndiceFila + K] := 1.0;
          end;
        end;

        // Liberar matriz auxiliar
        if ConstruirA then
          FreeAndNil(N);
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        IndiceParametro := 0;

        // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := Parametro + 1;
        K := 0;
        if J > I then
        begin
          // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
          ConstruirA := false;
          K := J - I - 1;
        end
        else
        begin
          // Es necesario construir A
          ConstruirA := true;

          // Construir matrices auxiliares
          M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
          N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

          // Construir A y su derivada
          for I := 1 to Informacion.CantidadColumnasEfectos do
          begin
            for J := 1 to Informacion.OrdenEstructuraCovarianza do
            begin
              if J <= I then
              begin
                if IndiceParametro = Parametro then
                  N [I, J] := 1.0;

                M [I, J] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;
          end;

          // Calcular dA/dp*A'
          M.Transponer;
          N.Multiplicar(M);

          // Calcular dA/dp*A' + A*dA'/dp
          M1 := N.Transpuesta;
          N.Sumar(M1);
          FreeAndNil(M1);

          // Liberar matriz auxiliar
          FreeAndNil(M);
        end;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de la submatriz
          if ConstruirA then
          begin
            // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
              end;
            end;
          end
          else
          begin
            // Ingresar los valores de dD/dp
            Matriz [IndiceFila + K, IndiceFila + K] := 1.0;
          end;
        end;

        // Liberar matriz auxiliar
        if ConstruirA then
          FreeAndNil(N);
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Determinar el indice del primer parametro
      IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      K := 0;
      if J > I then
      begin
        // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
        ConstruirA := false;
        K := J - I - 1;
      end
      else
      begin
        // Es necesario construir A
        ConstruirA := true;

        // Construir matrices auxiliar
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Ingresar los valores de la submatriz
      if ConstruirA then
      begin
        // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end
      else
      begin
        // Ingresar los valores de dD/dp
        Matriz [IndiceFila + K, IndiceFila + K] := 1.0;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      K := 0;
      if J > I then
      begin
        // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
        ConstruirA := false;
        K := J - I - 1;
      end
      else
      begin
        // Es necesario construir A
        ConstruirA := true;

        // Construir matrices auxiliares
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;

      // Ingresar los valores de la submatriz
      if ConstruirA then
      begin
        // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end
      else
      begin
        // Ingresar los valores de dD/dp
        Matriz [IndiceFila + K, IndiceFila + K] := 1.0;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_sin_diagonal') then
  begin
    // Estructura Factor Analytic sin Diagonal

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar el indice del primer parametro
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Construir matrices auxiliares
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);

        // Ingresar los valores en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
            end;
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        IndiceParametro := 0;

        // Construir matrices auxiliares
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
              Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
            end;
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Determinar el indice del primer parametro
      IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

      // Construir matrices auxiliar
      M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
      N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

      // Construir A y su derivada
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        for J := 1 to Informacion.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
              N [I, J] := 1.0;

            M [I, J] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Calcular dA/dp*A'
      M.Transponer;
      N.Multiplicar(M);

      // Calcular dA/dp*A' + A*dA'/dp
      M1 := N.Transpuesta;
      N.Sumar(M1);
      FreeAndNil(M1);

      // Liberar matriz auxiliar
      FreeAndNil(M);

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
          Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
        end;
      end;

      // Liberar matriz auxiliar
      FreeAndNil(N);
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Construir matrices auxiliares
      M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
      N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

      // Construir A y su derivada
      for I := 1 to Informacion.CantidadColumnasEfectos do
      begin
        for J := 1 to Informacion.OrdenEstructuraCovarianza do
        begin
          if J <= I then
          begin
            if IndiceParametro = Parametro then
              N [I, J] := 1.0;

            M [I, J] := Informacion.Parametros [IndiceParametro];
            Inc(IndiceParametro);
          end;
        end;
      end;

      // Calcular dA/dp*A'
      M.Transponer;
      N.Multiplicar(M);

      // Calcular dA/dp*A' + A*dA'/dp
      M1 := N.Transpuesta;
      N.Sumar(M1);
      FreeAndNil(M1);

      // Liberar matriz auxiliar
      FreeAndNil(M);

      // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
      for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
      begin
        Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

        for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
          Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
        end;
      end;

      // Liberar matriz auxiliar
      FreeAndNil(N);
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_diagonal_escalar') then
  begin
    // Estructura Factor Analytic con Diagonal Escalar

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar el indice del primer parametro
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := Parametro + 1;
        if J > I then
        begin
          // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
          ConstruirA := false;
        end
        else
        begin
          // Es necesario construir A
          ConstruirA := true;

          // Construir matrices auxiliares
          M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
          N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

          // Construir A y su derivada
          for I := 1 to Informacion.CantidadColumnasEfectos do
          begin
            for J := 1 to Informacion.OrdenEstructuraCovarianza do
            begin
              if J <= I then
              begin
                if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                  N [I, J] := 1.0;

                M [I, J] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;
          end;

          // Calcular dA/dp*A'
          M.Transponer;
          N.Multiplicar(M);

          // Calcular dA/dp*A' + A*dA'/dp
          M1 := N.Transpuesta;
          N.Sumar(M1);
          FreeAndNil(M1);

          // Liberar matriz auxiliar
          FreeAndNil(M);
        end;

        // Ingresar los valores en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de la submatriz
          if ConstruirA then
          begin
            // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
              end;
            end;
          end
          else
          begin
            // Ingresar los valores de dD/dp
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
          end;
        end;

        // Liberar matriz auxiliar
        if ConstruirA then
          FreeAndNil(N);
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        IndiceParametro := 0;

        // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := Parametro + 1;
        if J > I then
        begin
          // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
          ConstruirA := false;
        end
        else
        begin
          // Es necesario construir A
          ConstruirA := true;

          // Construir matrices auxiliares
          M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
          N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

          // Construir A y su derivada
          for I := 1 to Informacion.CantidadColumnasEfectos do
          begin
            for J := 1 to Informacion.OrdenEstructuraCovarianza do
            begin
              if J <= I then
              begin
                if IndiceParametro = Parametro then
                  N [I, J] := 1.0;

                M [I, J] := Informacion.Parametros [IndiceParametro];
                Inc(IndiceParametro);
              end;
            end;
          end;

          // Calcular dA/dp*A'
          M.Transponer;
          N.Multiplicar(M);

          // Calcular dA/dp*A' + A*dA'/dp
          M1 := N.Transpuesta;
          N.Sumar(M1);
          FreeAndNil(M1);

          // Liberar matriz auxiliar
          FreeAndNil(M);
        end;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores de la submatriz
          if ConstruirA then
          begin
            // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
                Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
              end;
            end;
          end
          else
          begin
            // Ingresar los valores de dD/dp
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
          end;
        end;

        // Liberar matriz auxiliar
        if ConstruirA then
          FreeAndNil(N);
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Determinar el indice del primer parametro
      IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
        ConstruirA := false;
      end
      else
      begin
        // Es necesario construir A
        ConstruirA := true;

        // Construir matrices auxiliar
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if (IndiceParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Ingresar los valores de la submatriz
      if ConstruirA then
      begin
        // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end
      else
      begin
        // Ingresar los valores de dD/dp
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      IndiceParametro := 0;

      // Verificar si es necesario construir A y su derivada (el parametro podria corresponder a D)
      I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
      J := Parametro + 1;
      if J > I then
      begin
        // El parametro corresponde a D, no es necesario construir A, calcular el indice dentro de la diagonal de la submatriz
        ConstruirA := false;
      end
      else
      begin
        // Es necesario construir A
        ConstruirA := true;

        // Construir matrices auxiliares
        M := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);
        N := TUaMatriz.Create(Informacion.CantidadColumnasEfectos, Informacion.OrdenEstructuraCovarianza);

        // Construir A y su derivada
        for I := 1 to Informacion.CantidadColumnasEfectos do
        begin
          for J := 1 to Informacion.OrdenEstructuraCovarianza do
          begin
            if J <= I then
            begin
              if IndiceParametro = Parametro then
                N [I, J] := 1.0;

              M [I, J] := Informacion.Parametros [IndiceParametro];
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Calcular dA/dp*A'
        M.Transponer;
        N.Multiplicar(M);

        // Calcular dA/dp*A' + A*dA'/dp
        M1 := N.Transpuesta;
        N.Sumar(M1);
        FreeAndNil(M1);

        // Liberar matriz auxiliar
        FreeAndNil(M);
      end;

      // Ingresar los valores de la submatriz
      if ConstruirA then
      begin
        // Ingresar los valores de dA/dp*A' + A*dA'/dp almacenados en M
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := N [I + 1, I + 1];

          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + J] := N [I + 1, J + 1];
            Matriz [IndiceFila + J, IndiceFila + I] := N [I + 1, J + 1];
          end;
        end;

        // Liberar matriz auxiliar
        FreeAndNil(N);
      end
      else
      begin
        // Ingresar los valores de dD/dp
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;
      end;
    end;
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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar la fila y columna dentro de la submatriz
        IndiceFilaSubmatriz := 0;
        IndiceColumnaSubmatriz := 0;
        I := 0;
        J := Parametro + 1;
        K := Informacion.OrdenEstructuraCovarianza;
        Listo := false;
        while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
        begin
          if J <= I + K then
          begin
            // El parametro se encuentra en esta columna
            Listo := true;

            // Calcular el indice de columna dentro de la submatriz
            IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
          end
          else
          begin
            // El parametro se encuentra en otra columna, continuar buscando
            I := I + K;
            K := Min(Informacion.OrdenEstructuraCovarianza, Informacion.CantidadColumnasEfectos - IndiceFilaSubmatriz - 1);
            Inc(IndiceFilaSubmatriz);
          end;
        end;

        // Ingresar el valor en la matriz
        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar si el valor es en la diagonal principal e ingresar el valor
          if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
          end
          else
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
            Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Determinar la fila y columna dentro de la submatriz
        IndiceFilaSubmatriz := 0;
        IndiceColumnaSubmatriz := 0;
        I := 0;
        J := Parametro + 1;
        K := Informacion.OrdenEstructuraCovarianza;
        Listo := false;
        while (I <= Informacion.Parametros.Dimension) and (not Listo) do
        begin
          if J <= I + K then
          begin
            // El parametro se encuentra en esta columna
            Listo := true;

            // Calcular el indice de columna dentro de la submatriz
            IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
          end
          else
          begin
            // El parametro se encuentra en otra columna, continuar buscando
            I := I + K;
            K := Min(Informacion.OrdenEstructuraCovarianza, Informacion.CantidadColumnasEfectos - IndiceFilaSubmatriz - 1);
            Inc(IndiceFilaSubmatriz);
          end;
        end;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Verificar si el valor es en la diagonal principal e ingresar el valor
          if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
          end
          else
          begin
            Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
            Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Determinar la fila y columna dentro de la submatriz
      IndiceFilaSubmatriz := 0;
      IndiceColumnaSubmatriz := 0;
      I := 0;
      J := Parametro + 1;
      K := Informacion.OrdenEstructuraCovarianza;
      Listo := false;
      while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          I := I + K;
          K := Min(Informacion.OrdenEstructuraCovarianza, Informacion.CantidadColumnasEfectos - IndiceFilaSubmatriz - 1);
          Inc(IndiceFilaSubmatriz);
        end;
      end;

      // Verificar si el valor es en la diagonal principal e ingresar el valor
      if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
      end
      else
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
        Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Determinar la fila y columna dentro de la submatriz
      IndiceFilaSubmatriz := 0;
      IndiceColumnaSubmatriz := 0;
      I := 0;
      J := Parametro + 1;
      K := Informacion.OrdenEstructuraCovarianza;
      Listo := false;
      while (I <= Informacion.Parametros.Dimension) and (not Listo) do
      begin
        if J <= I + K then
        begin
          // El parametro se encuentra en esta columna
          Listo := true;

          // Calcular el indice de columna dentro de la submatriz
          IndiceColumnaSubmatriz := IndiceFilaSubmatriz + J - I - 1;
        end
        else
        begin
          // El parametro se encuentra en otra columna, continuar buscando
          I := I + K;
          K := Min(Informacion.OrdenEstructuraCovarianza, Informacion.CantidadColumnasEfectos - IndiceFilaSubmatriz - 1);
          Inc(IndiceFilaSubmatriz);
        end;
      end;

      // Verificar si el valor es en la diagonal principal e ingresar el valor
      if IndiceFilaSubmatriz = IndiceColumnaSubmatriz then
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
      end
      else
      begin
        Matriz [IndiceFila + IndiceFilaSubmatriz, IndiceFila + IndiceColumnaSubmatriz] := 1.0;
        Matriz [IndiceFila + IndiceColumnaSubmatriz, IndiceFila + IndiceFilaSubmatriz] := 1.0;
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

        // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
        if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        end
        else
          // El indice corresponde al primer grupo
          IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

        // Determinar el indice del primer parametro
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores a la matriz

          // Verificar si se debe ingresar la varianza o la covarianza
          if Parametro = 0 then
          begin
            // Sigma cuadrado
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;

              R1 := 1.0;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Matriz [IndiceFila + I, IndiceFila + J] := R1;
                Matriz [IndiceFila + J, IndiceFila + I] := R1;
              end;
            end;
          end
          else
          begin
            // Ro
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 0.0;

              R1 := 1.0;
              K := 1;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                R2 := Informacion.Parametros [IndiceParametro] * K * R1;
                Matriz [IndiceFila + I, IndiceFila + J] := R2 ;
                Matriz [IndiceFila + J, IndiceFila + I] := R2 ;
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Inc(K);
              end;
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Determinar el indice del primer parametro
        IndiceParametro := 0;

        for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
        begin
          // Definir la fila inicial del nivel
          IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

          // Ingresar los valores a la matriz

          // Verificar si se debe ingresar la varianza o la covarianza
          if Parametro = 0 then
          begin
            // Sigma cuadrado
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 1.0;

              R1 := 1.0;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Matriz [IndiceFila + I, IndiceFila + J] := R1;
                Matriz [IndiceFila + J, IndiceFila + I] := R1;
              end;
            end;
          end
          else
          begin
            // Ro
            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 0.0;

              R1 := 1.0;
              K := 1;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                R2 := Informacion.Parametros [IndiceParametro] * K * R1;
                Matriz [IndiceFila + I, IndiceFila + J] := R2 ;
                Matriz [IndiceFila + J, IndiceFila + I] := R2 ;
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Inc(K);
              end;
            end;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar el indice de nivel y de parametro del criterio agrupamiento unidad experimental
      if Parametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := Parametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        Parametro := Parametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
      end
      else
        // El indice corresponde al primer grupo
        IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;

      // Determinar el indice del primer parametro
      IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

      // Sumar la cantidad de columnas (o filas) necesarias
      IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

      // Ingresar los valores a la matriz

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Sigma cuadrado
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;

          R1 := 1.0;
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
            Matriz [IndiceFila + I, IndiceFila + J] := R1;
            Matriz [IndiceFila + J, IndiceFila + I] := R1;
          end;
        end;
      end
      else
      begin
        // Ro
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := 0.0;

          R1 := 1.0;
          K := 1;
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            R2 := Informacion.Parametros [IndiceParametro] * K * R1;
            Matriz [IndiceFila + I, IndiceFila + J] := R2 ;
            Matriz [IndiceFila + J, IndiceFila + I] := R2 ;
            R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
            Inc(K);
          end;
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Determinar el indice del primer parametro
      IndiceParametro := 0;

      // Ingresar los valores a la matriz

      // Verificar si se debe ingresar la varianza o la covarianza
      if Parametro = 0 then
      begin
        // Sigma cuadrado
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := 1.0;

          R1 := 1.0;
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
            Matriz [IndiceFila + I, IndiceFila + J] := R1;
            Matriz [IndiceFila + J, IndiceFila + I] := R1;
          end;
        end;
      end
      else
      begin
        // Ro
        for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
        begin
          Matriz [IndiceFila + I, IndiceFila + I] := 0.0;

          R1 := 1.0;
          K := 1;
          for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            R2 := Informacion.Parametros [IndiceParametro] * K * R1;
            Matriz [IndiceFila + I, IndiceFila + J] := R2 ;
            Matriz [IndiceFila + J, IndiceFila + I] := R2 ;
            R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
            Inc(K);
          end;
        end;
      end;
    end;
  end;

  Assert(not Assigned(M), 'TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(M)');
  Assert(not Assigned(N), 'TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(N)');
  Assert(not Assigned(M1), 'TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios: not Assigned(M1)');
end { TProcedimiento.ConstruirDerivadaPrimeraSubmatrizCovarianzaGrupoEfectosAleatorios };

function TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios(var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer): Boolean;
var
  Listo, SeAnula: Boolean;
  I, J, IndiceFila: Integer;
  InformacionGrupo: TInformacionTiempoEjecucionGrupoEfectosAleatorios;
begin { TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios }
  // Construir la matriz, de ser necesario
  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FCantidadFilasMatrizCovarianzaEfectosAleatorios, FCantidadFilasMatrizCovarianzaEfectosAleatorios)
  else
    // Llamada subsiguiente, limpiar
    Matriz.AsignarCero;

  // Verficar que los indices de parametros sean validos
  if PrimerParametro >= FCantidadParametrosEfectosAleatorios then
    PrimerParametro := PrimerParametro mod FCantidadParametrosEfectosAleatorios;
  if SegundoParametro >= FCantidadParametrosEfectosAleatorios then
    SegundoParametro := SegundoParametro mod FCantidadParametrosEfectosAleatorios;

  // Determinar el grupo de efectos aleatorios al que pertenecen los parametros y determinar si es necesario calcular la derivada
  IndiceFila := 1;
  I := 0;
  J := 0;
  Listo := false;
  SeAnula := false;
  InformacionGrupo := nil;
  while (I <= FInformacionTiempoEjecucionEfectosAleatorios.Count - 1) and (not Listo) do
  begin
    InformacionGrupo := FInformacionTiempoEjecucionEfectosAleatorios [I];

    if PrimerParametro <= (J + InformacionGrupo.Parametros.Dimension - 1) then
    begin
      if SegundoParametro > (J + InformacionGrupo.Parametros.Dimension - 1) then
        SeAnula := true;
      Listo := true;
    end
    else if SegundoParametro <= (J + InformacionGrupo.Parametros.Dimension - 1) then
    begin
      SeAnula := true;
      Listo := true;
    end
    else
    begin
      J := J + InformacionGrupo.Parametros.Dimension;
      IndiceFila := IndiceFila + InformacionGrupo.CantidadFilasMatrizCovarianza;
    end;

    Inc(I);
  end;

  // Verificar si es necesario calcular la derivada
  if not SeAnula then
  begin
    PrimerParametro := PrimerParametro - J;
    SegundoParametro := SegundoParametro - J;
    SeAnula := ConstruirDerivadaSegundaSubmatrizCovarianzaGrupoEfectosAleatorios(InformacionGrupo, Matriz, PrimerParametro, SegundoParametro, IndiceFila);
  end;

  Result := SeAnula;
end { TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaEfectosAleatorios };

function TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError(var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer): Boolean;
var
  I, J, K, L: Integer;
  Listo, Listo1, SeAnula: Boolean;
  IndiceFila, IndiceColumna, IndiceFilaSubmatriz, IndiceColumnaSubmatriz: Integer;
  IndiceFilaPrimerParametro, IndiceFilaSegundoParametro, IndiceColumnaPrimerParametro, IndiceColumnaSegundoParametro: Integer;
  IndiceParametro, IndicePrimerParametro, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimentalFila, IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna: Integer;
  IndiceNivelCriterioOrdenamiento, IndiceNivelCriterioOrdenamientoColumna: Integer;
  M, N, SubmatrizUnidadExperimental, SubmatrizCriterioAgrupamientoUnidadExperimental: TUaMatriz;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  InformacionError: TInformacionTiempoEjecucionError;
  IndicesUltimaObservacion, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental: TArregloEnteros;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError }
  // Construir la matriz, de ser necesario
  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FModelo.Datos.Valores.CantidadFilas, FModelo.Datos.Valores.CantidadFilas)
  else
    // Llamada subsiguiente, limpiar
    Matriz.AsignarCero;

  // Inicializar referencias
  M := nil;
  N := nil;
  SubmatrizUnidadExperimental := nil;
  SubmatrizCriterioAgrupamientoUnidadExperimental := nil;
  IndicesUltimaObservacion  := nil;
  IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental := nil;

  InformacionError := FInformacionTiempoEjecucionError;

  SeAnula := false;

  // Verificar que los indices de parametros sean validos
  if PrimerParametro >= InformacionError.Parametros.Dimension then
    PrimerParametro := PrimerParametro mod InformacionError.Parametros.Dimension;

  if SegundoParametro >= InformacionError.Parametros.Dimension then
    SegundoParametro := SegundoParametro mod InformacionError.Parametros.Dimension;

  if InformacionError.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    // Se definio criterio agrupamiento unidad experimental

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental;

    // Identificar los indices de nivel de criterio agrupamiento unidad experimental
    IndiceNivelCriterioAgrupamientoUnidadExperimental := 0;
    I := InformacionCriterio.IndicesNiveles.Bajo;
    J := PrimerParametro + 1;
    K := SegundoParametro + 1;
    IndicePrimerParametro := 0;
    Listo := false;
    while (I <= InformacionCriterio.IndicesNiveles.Alto) and (not Listo) do
    begin
      if J <= IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] then
      begin
        // El primer parametro se encuentra en el I-esimo grupo, verificar que el segundo tambien se encuentre alli

        if K <= IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] then
        begin
          // Ambos parametros estan en el mismo grupo

          IndiceNivelCriterioAgrupamientoUnidadExperimental := I;
          Listo := true;
          PrimerParametro := PrimerParametro - IndicePrimerParametro;
          SegundoParametro := SegundoParametro - IndicePrimerParametro;
        end
        else
        begin
          // Los parametros estan en grupos distintos, la derivada se anula
          SeAnula := true;
          Listo := true;
        end;
      end
      else if K <= IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I] then
      begin
        // El segundo parametro se encuentra en el I-esimo grupo y el primer parametro no, por ende la derivada se anula
        SeAnula := true;
        Listo := true;
      end
      else
      begin
        IndicePrimerParametro := IndicePrimerParametro + InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
        Inc(I);
      end;
    end;

    // Construir la submatriz, de ser necesario
    if not SeAnula then
    begin
      if InformacionError.Estructura.Nombre = 'componentes_varianza' then
      begin
        // Estructura Componentes Varianza

        SeAnula := true;
      end
      else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
      begin
        // Estructura Simetria Compuesta

        SeAnula := true;
      end
      else if InformacionError.Estructura.Nombre = 'general' then
      begin
        // Estructura General

        SeAnula := true;
      end
      else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
      begin
        // Estructura Diagonal Heterogenea

        SeAnula := true;
      end
      else if InformacionError.Estructura.Nombre = 'factor_analytic' then
      begin
        // Estructura Factor Analytic

        // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
        I := Trunc((InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] / 2) * (2 * InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] - InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1));
        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        if (J <= I) and (K <= I) then
        begin
          // Ambos parametros viven en A

          // Determinar indice de fila y columna dentro de A de ambos parametros
          IndiceFilaPrimerParametro := 0;
          IndiceFilaSegundoParametro := 0;
          IndiceColumnaPrimerParametro := 0;
          IndiceColumnaSegundoParametro := 0;
          I := 0;
          IndiceFilaSubmatriz := 0;
          L := 1;
          Listo := false;
          Listo1 := false;
          while (I <= InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
          begin
            // Verificar si el primer parametro se encuentra en la fila actual
            if (not Listo) and (J <= I + L) then
            begin
              IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
              IndiceColumnaPrimerParametro := J - I - 1;
              Listo := true;
            end;

            // Verificar si el segundo parametro se encuentra en la fila actual
            if (not Listo1) and (K <= I + L) then
            begin
              IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
              IndiceColumnaSegundoParametro := K - I - 1;
              Listo1 := true;
            end;

            // Actualizar conteo de parametros, de ser necesario
            if (not Listo) or (not Listo1) then
            begin
              I := I + L;
              Inc(IndiceFilaSubmatriz);
              L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
            end;
          end;

          // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
          if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
          begin
            // Los parametros viven en la misma columna de A, construir la submatriz

            // Construir submatriz
            SubmatrizCriterioAgrupamientoUnidadExperimental := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

            SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
            SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
          end
          else
          begin
            // Los parametros viven en distintas columnas de A, la derivada es nula
            SeAnula := true;
          end;
        end
        else
        begin
          // Alguno de los parametros no vive en A, la derivada es nula
          SeAnula := true;
        end;
      end
      else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
      begin
        // Estructura Factor Analytic Sin Diagonal

        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
          end;
        end;

        // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // Los parametros viven en la misma columna de A, construir la submatriz

          // Construir submatriz
          SubmatrizCriterioAgrupamientoUnidadExperimental := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

          SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
          SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
        end
        else
        begin
          // Los parametros viven en distintas columnas de A, la derivada es nula
          SeAnula := true;
        end;
      end
      else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
      begin
        // Estructura Factor Analytic Diagonal Escalar

        // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
        I := Trunc((InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] / 2) * (2 * InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] - InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1));
        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        if (J <= I) and (K <= I) then
        begin
          // Ambos parametros viven en A

          // Determinar indice de fila y columna dentro de A de ambos parametros
          IndiceFilaPrimerParametro := 0;
          IndiceFilaSegundoParametro := 0;
          IndiceColumnaPrimerParametro := 0;
          IndiceColumnaSegundoParametro := 0;
          I := 0;
          IndiceFilaSubmatriz := 0;
          L := 1;
          Listo := false;
          Listo1 := false;
          while (I <= InformacionError.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
          begin
            // Verificar si el primer parametro se encuentra en la fila actual
            if (not Listo) and (J <= I + L) then
            begin
              IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
              IndiceColumnaPrimerParametro := J - I - 1;
              Listo := true;
            end;

            // Verificar si el segundo parametro se encuentra en la fila actual
            if (not Listo1) and (K <= I + L) then
            begin
              IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
              IndiceColumnaSegundoParametro := K - I - 1;
              Listo1 := true;
            end;

            // Actualizar conteo de parametros, de ser necesario
            if (not Listo) or (not Listo1) then
            begin
              I := I + L;
              Inc(IndiceFilaSubmatriz);
              L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);
            end;
          end;

          // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
          if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
          begin
            // Los parametros viven en la misma columna de A, construir la submatriz

            // Construir submatriz
            SubmatrizCriterioAgrupamientoUnidadExperimental := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

            SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
            SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
          end
          else
          begin
            // Los parametros viven en distintas columnas de A, la derivada es nula
            SeAnula := true;
          end;
        end
        else
        begin
          // Alguno de los parametros no vive en A, la derivada es nula
          SeAnula := true;
        end;
      end
      else if InformacionError.Estructura.Nombre = 'bandeada' then
      begin
        // Estructura Bandeada

        SeAnula := true;
      end
      else if InformacionError.Estructura.Nombre = 'autoregresiva' then
      begin
        // Estructura Autoregresiva de primer orden

        SeAnula := false;

        // Determinar primer parametro del grupo
        IndiceParametro := IndicePrimerParametro;

        // Si se deriva dos veces sobre el parametro de la varianza (sigma
        // cuadrado) la derivada se anula.
        if (PrimerParametro = 0) and (SegundoParametro = 0) then
          SeAnula := True
        else
        begin
          // La derivada no se anula, construir submatriz
          SubmatrizCriterioAgrupamientoUnidadExperimental := TUaMatriz.Create(InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental], InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]);

          if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
             ((PrimerParametro = 1) and (SegundoParametro = 0)) then
          begin
            for I := 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
            begin
              SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 0.0 ;

              R1 := 1.0;
              K := 1;
              for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
              begin
                SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := K * R1;
                R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
                Inc(K);
              end;
            end;
          end
          else if (PrimerParametro = 1) and (SegundoParametro = 1) then
          begin
            for I := 0 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
            begin
              SubmatrizCriterioAgrupamientoUnidadExperimental [I, I] := 0.0 ;

              R1 := InformacionError.Parametros [IndiceParametro];
              K := 1;
              for J := I + 1 to InformacionError.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental] do
              begin
                if K = 1 then
                begin
                  R2 := 2.0;
                  SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := 0.0;
                end
                else
                begin
                  SubmatrizCriterioAgrupamientoUnidadExperimental [I, J] := R1 * R2;
                  R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
                  R2 := R2 * (K + 1);
                end;
                Inc(K);
              end;
            end;
          end;
        end;
      end;

      // Verificar si necesario continuar y si hubo definicion de unidad experimental
      if (not SeAnula) and InformacionError.TieneUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental y la derivada no es nula

        // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental
        IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
        IndicesUltimaObservacion.PonerA(0);

        IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadNiveles);
        IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.PonerA(0);

        // Verificar definicion de criterio de ordenamiento
        if InformacionError.TieneCriterioOrdenamiento then
        begin
          // Se definio criterio de ordenamiento

          // Construir submatriz de unidad experimental
          SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones, InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadMaximaObservaciones);

          // Construir la matriz de covarianza por cada nivel de la unidad experimental
          for IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
          begin
            // Limpiar submatriz de unidad experimental
            SubmatrizUnidadExperimental.AsignarCero;

            for IndiceFila := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Bajo to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
            begin
              IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceFila]];

              // Verificar que el indice del nivel de criterio de agrupamiento coincida con el del parametro
              if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
              begin
                // Ingresar valor en la diagonal principal de la submatriz
                SubmatrizUnidadExperimental [IndiceFila + 1, IndiceFila + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1];

                // Ingresar los valores del resto de las columnas
                J := IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1;
                for IndiceColumna := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.CantidadObservacionesNivel [IndiceNivelUnidadExperimental] - 1 do
                begin
                  IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, IndiceColumna]];

                  // Verificar coincidencia de niveles de criterio agrupamiento unidad experimental
                  if IndiceNivelCriterioAgrupamientoUnidadExperimental = IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna then
                  begin
                    // El nivel coincide, ingresar los valores
                    SubmatrizUnidadExperimental [IndiceFila + 1, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                    Inc(J);
                  end;
                end;

                // Incrementar el indices de la ultima observacion visitada
                IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.Incrementar(IndiceNivelUnidadExperimental);
              end;

              // Incrementar el indices de la ultima observacion visitada
              IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
            end;

            // Ingresar los valores (reordenados) a la derivada de la matriz de covarianza

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

          // Liberar matrices auxiliares
          FreeAndNil(SubmatrizUnidadExperimental);
        end
        else
        begin
          // No se definio criterio de ordenamiento

          // Ingresar los valores a la matriz
          for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            IndiceNivelUnidadExperimental := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];
            IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

            // Verificar coincidencia del nivel de criterio agrupamiento unidad experimental
            if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
            begin
              // Ingresar valor en la diagonal principal
              Matriz [IndiceFila, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1];

              // Ingresar los valores del resto de las columnas
              J := IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1;
              for I := IndicesUltimaObservacion [IndiceNivelUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelUnidadExperimental].Alto do
              begin
                IndiceColumna := InformacionError.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I];
                IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceColumna];

                // Verificar coincidencia de nivel de criterio agrupamiento unidad experimental
                if IndiceNivelCriterioAgrupamientoUnidadExperimentalColumna = IndiceNivelCriterioAgrupamientoUnidadExperimental then
                begin
                  // Los niveles coinciden, ingresar los valores
                  Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                  Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental [IndiceNivelUnidadExperimental] + 1, J + 1];
                  Inc(J);
                end;
              end;

              // Incrementar el indices de la ultima observacion visitada
              IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental.Incrementar(IndiceNivelUnidadExperimental);
            end;

            // Incrementar el indices de la ultima observacion visitada
            IndicesUltimaObservacion.Incrementar(IndiceNivelUnidadExperimental);
          end;
        end;

        // Liberar matrices auxiliares
        FreeAndNil(SubmatrizCriterioAgrupamientoUnidadExperimental);

        // Liberar arreglos de indices ultima observacion
        FreeAndNil(IndicesUltimaObservacion);
      end
      else if (not SeAnula) then
      begin
        // No se definio unidad experimental y se definio criterio agrupamiento unidad experimental y la derivada no es nula

        // Se ignora el criterio de ordenamiento

        // Construir e inicializar arreglo de indices con la ultima observacion visitada de cada nivel de unidad experimental
        IndicesUltimaObservacion := TArregloEnteros.Create(InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);
        IndicesUltimaObservacion.PonerA(0);

        // Ingresar los valores a la matriz
        for IndiceFila := 1 to InformacionError.CantidadFilasMatrizCovarianza do
        begin
          IndiceNivelCriterioAgrupamientoUnidadExperimentalFila := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesObservaciones [IndiceFila - 1];

          // Verificar coincidencia del nivel de criterio agrupamiento unidad experimental
          if IndiceNivelCriterioAgrupamientoUnidadExperimentalFila = IndiceNivelCriterioAgrupamientoUnidadExperimental then
          begin
            // Ingresar valor en la diagonal principal
            Matriz [IndiceFila, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1];

            // Ingresar los valores del resto de las columnas
            for I := IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1 to InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles.Arreglos [IndiceNivelCriterioAgrupamientoUnidadExperimental].Alto do
            begin
              IndiceColumna := InformacionError.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, I];

              Matriz [IndiceFila, IndiceColumna + 1] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
              Matriz [IndiceColumna + 1, IndiceFila] := SubmatrizCriterioAgrupamientoUnidadExperimental [IndicesUltimaObservacion [IndiceNivelCriterioAgrupamientoUnidadExperimental] + 1, I + 1];
            end;

            // Incrementar el indice de la ultima observacion visitada
            IndicesUltimaObservacion.Incrementar(IndiceNivelCriterioAgrupamientoUnidadExperimental);
          end;
        end;

        // Liberar arreglos de indices ultima observacion
        FreeAndNil(IndicesUltimaObservacion);
      end;
    end;
  end
  else if InformacionError.TieneUnidadExperimental then
  begin
    // Se definio unidad experimental y no se definio criterio de agrupamiento

    InformacionCriterio := InformacionError.InformacionTiempoEjecucionUnidadExperimental;

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'diagonal_heterogenea' then
    begin
      // Estructura Diagonal Heterogenea

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionCriterio.CantidadMaximaObservaciones - InformacionError.OrdenEstructuraCovarianza + 1));
      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      if (J <= I) and (K <= I) then
      begin
        // Ambos parametros viven en A

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // Los parametros viven en la misma columna de A, construir la submatriz

          // Construir submatriz
          SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

          SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
          SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
        end
        else
        begin
          // Los parametros viven en distintas columnas de A, la derivada es nula
          SeAnula := true;
        end;
      end
      else
      begin
        // Alguno de los parametros vive fuera de A, la derivada es nula
        SeAnula := true;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic Sin Diagonal

      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      // Determinar indice de fila y columna dentro de A de ambos parametros
      IndiceFilaPrimerParametro := 0;
      IndiceFilaSegundoParametro := 0;
      IndiceColumnaPrimerParametro := 0;
      IndiceColumnaSegundoParametro := 0;
      I := 0;
      IndiceFilaSubmatriz := 0;
      L := 1;
      Listo := false;
      Listo1 := false;
      while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
      begin
        // Verificar si el primer parametro se encuentra en la fila actual
        if (not Listo) and (J <= I + L) then
        begin
          IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
          IndiceColumnaPrimerParametro := J - I - 1;
          Listo := true;
        end;

        // Verificar si el segundo parametro se encuentra en la fila actual
        if (not Listo1) and (K <= I + L) then
        begin
          IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
          IndiceColumnaSegundoParametro := K - I - 1;
          Listo1 := true;
        end;

        // Actualizar conteo de parametros, de ser necesario
        if (not Listo) or (not Listo1) then
        begin
          I := I + L;
          Inc(IndiceFilaSubmatriz);
          L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
        end;
      end;

      // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
      if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
      begin
        // Los parametros viven en la misma columna de A, construir la submatriz

        // Construir submatriz
        SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

        SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
        SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
      end
      else
      begin
        // Los parametros viven en distintas columnas de A, la derivada es nula
        SeAnula := true;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic Diagonal Escalar

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionCriterio.CantidadMaximaObservaciones - InformacionError.OrdenEstructuraCovarianza + 1));
      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      if (J <= I) and (K <= I) then
      begin
        // Ambos parametros viven en A

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // Los parametros viven en la misma columna de A, construir la submatriz

          // Construir submatriz
          SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

          SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
          SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := SubmatrizUnidadExperimental [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
        end
        else
        begin
          // Los parametros viven en distintas columnas de A, la derivada es nula
          SeAnula := true;
        end;
      end
      else
      begin
        // Alguno de los parametros vive fuera de A, la derivada es nula
        SeAnula := true;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      SeAnula := false ;

      // Determinar primer parametro del grupo
      IndiceParametro := 0;

      // Si se deriva dos veces sobre el parametro de la varianza (sigma
      // cuadrado) la derivada se anula.
      if (PrimerParametro = 0) and (SegundoParametro = 0) then
        SeAnula := True
      else
      begin
        // La derivada no se anula, construir submatriz
        SubmatrizUnidadExperimental := TUaMatriz.Create(InformacionCriterio.CantidadMaximaObservaciones, InformacionCriterio.CantidadMaximaObservaciones);

        if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
           ((PrimerParametro = 1) and (SegundoParametro = 0)) then
        begin
          for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
          begin
            SubmatrizUnidadExperimental [I, I] := 0.0 ;

            R1 := 1.0;
            K := 1;
            for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
            begin
              SubmatrizUnidadExperimental [I, J] := K * R1;
              R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
              Inc(K);
            end;
          end;
        end
        else if (PrimerParametro = 1) and (SegundoParametro = 1) then
        begin
          for I := 1 to InformacionCriterio.CantidadMaximaObservaciones do
          begin
            SubmatrizUnidadExperimental [I, I] := 0.0 ;

            R1 := InformacionError.Parametros [IndiceParametro];
            K := 1;
            for J := I + 1 to InformacionCriterio.CantidadMaximaObservaciones do
            begin
              if K = 1 then
              begin
                R2 := 2.0;
                SubmatrizUnidadExperimental [I, J] := 0.0;
              end
              else
              begin
                SubmatrizUnidadExperimental [I, J] := R1 * R2;
                R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
                R2 := R2 * (K + 1);
              end;
              Inc(K);
            end;
          end;
        end;
      end;
    end;

    // Ingresar los valores a la matriz, de ser necesario
    if (not SeAnula) then
    begin
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

      // Liberar arreglos de ultima observacion
      FreeAndNil(IndicesUltimaObservacion);
    end;
  end
  else
  begin
    // No se definio ni unidad experimental ni criterio agrupamiento unidad experimental

    if InformacionError.Estructura.Nombre = 'componentes_varianza' then
    begin
      // Estructura Componentes Varianza

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'simetria_compuesta' then
    begin
      // Estructura Simetria Compuesta

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'general' then
    begin
      // Estructura General

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic' then
    begin
      // Estructura Factor Analytic

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionError.CantidadFilasMatrizCovarianza - InformacionError.OrdenEstructuraCovarianza + 1));
      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      if (J <= I) and (K <= I) then
      begin
        // Ambos parametros viven en A

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // Los parametros viven en la misma columna de A, construir la submatriz

          Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
          Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
        end;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_sin_diagonal' then
    begin
      // Estructura Factor Analytic Sin Diagonal

      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      // Determinar indice de fila y columna dentro de A de ambos parametros
      IndiceFilaPrimerParametro := 0;
      IndiceFilaSegundoParametro := 0;
      IndiceColumnaPrimerParametro := 0;
      IndiceColumnaSegundoParametro := 0;
      I := 0;
      IndiceFilaSubmatriz := 0;
      L := 1;
      Listo := false;
      Listo1 := false;
      while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
      begin
        // Verificar si el primer parametro se encuentra en la fila actual
        if (not Listo) and (J <= I + L) then
        begin
          IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
          IndiceColumnaPrimerParametro := J - I - 1;
          Listo := true;
        end;

        // Verificar si el segundo parametro se encuentra en la fila actual
        if (not Listo1) and (K <= I + L) then
        begin
          IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
          IndiceColumnaSegundoParametro := K - I - 1;
          Listo1 := true;
        end;

        // Actualizar conteo de parametros, de ser necesario
        if (not Listo) or (not Listo1) then
        begin
          I := I + L;
          Inc(IndiceFilaSubmatriz);
          L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
        end;
      end;

      // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
      if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
      begin
        // Los parametros viven en la misma columna de A, construir la submatriz

        Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
        Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
      end;
    end
    else if InformacionError.Estructura.Nombre = 'factor_analytic_diagonal_escalar' then
    begin
      // Estructura Factor Analytic Diagonal Escalar

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
      I := Trunc((InformacionError.OrdenEstructuraCovarianza / 2) * (2 * InformacionError.CantidadFilasMatrizCovarianza - InformacionError.OrdenEstructuraCovarianza + 1));
      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      if (J <= I) and (K <= I) then
      begin
        // Ambos parametros viven en A

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= InformacionError.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, InformacionError.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que ambos parametros se encuentren en la misma columna, de otro modo la derivada es nula
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // Los parametros viven en la misma columna de A, construir la submatriz

          Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] := Matriz [IndiceFilaPrimerParametro + 1, IndiceFilaSegundoParametro + 1] + 1.0;
          Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] := Matriz [IndiceFilaSegundoParametro + 1, IndiceFilaPrimerParametro + 1] + 1.0;
        end;
       end;
    end
    else if InformacionError.Estructura.Nombre = 'bandeada' then
    begin
      // Estructura Bandeada

      SeAnula := true;
    end
    else if InformacionError.Estructura.Nombre = 'autoregresiva' then
    begin
      // Estructura Autoregresiva de primer orden

      SeAnula := false ;

      // Determinar primer parametro del grupo
      IndiceParametro := 0;

      // Si se deriva dos veces sobre el parametro de la varianza (sigma
      // cuadrado) la derivada se anula.
      if (PrimerParametro = 0) and (SegundoParametro = 0) then
        SeAnula := True
      else
      begin
        if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
           ((PrimerParametro = 1) and (SegundoParametro = 0)) then
        begin
          for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            Matriz [I, I] := 0.0 ;

            R1 := 1.0;
            K := 1;
            for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
            begin
              Matriz [I, J] := K * R1;
              Matriz [J, I] := K * R1;
              R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
              Inc(K);
            end;
          end;
        end
        else if (PrimerParametro = 1) and (SegundoParametro = 1) then
        begin
          for I := 1 to InformacionError.CantidadFilasMatrizCovarianza do
          begin
            Matriz [I, I] := 0.0 ;

            R1 := InformacionError.Parametros [IndiceParametro];
            K := 1;
            for J := I + 1 to InformacionError.CantidadFilasMatrizCovarianza do
            begin
              if K = 1 then
              begin
                R2 := 2.0;
                Matriz [I, J] := 0.0;
              end
              else
              begin
                Matriz [I, J] := R1 * R2;
                Matriz [J, I] := R1 * R2;
                R1 := R1 * InformacionError.Parametros [IndiceParametro + 1];
                R2 := R2 * (K + 1);
              end;
              Inc(K);
            end;
          end;
        end;
      end;
    end;
  end;

  Result := SeAnula;

  Assert(not Assigned(M), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(M)');
  Assert(not Assigned(N), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(N)');
  Assert(not Assigned(SubmatrizUnidadExperimental), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(SubmatrizUnidadExperimental)');
  Assert(not Assigned(SubmatrizCriterioAgrupamientoUnidadExperimental), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(SubmatrizCriterioAgrupamientoUnidadExperimental)');
  Assert(not Assigned(IndicesUltimaObservacion), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(IndicesUltimaObservacion)');
  Assert(not Assigned(IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental), 'TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError: not Assigned(IndicesUltimaObservacionCriterioAgrupamientoUnidadExperimental)');
end { TProcedimiento.ConstruirDerivadaSegundaMatrizCovarianzaError };

function TProcedimiento.ConstruirDerivadaSegundaSubmatrizCovarianzaGrupoEfectosAleatorios(const Informacion: TInformacionTiempoEjecucionGrupoEfectosAleatorios; var Matriz: TUaMatriz; PrimerParametro, SegundoParametro: Integer; const FilaInicial: Integer): Boolean;
var
  Listo, Listo1, SeAnula: Boolean;
  I, J, K, L: Integer;
  IndiceParametro, IndiceFila, IndiceFilaSubmatriz, IndiceNivelUnidadExperimental, IndiceNivelCriterioAgrupamientoUnidadExperimental: Integer;
  IndiceFilaPrimerParametro, IndiceColumnaPrimerParametro, IndiceFilaSegundoParametro, IndiceColumnaSegundoParametro: Integer;
  R1, R2: TUaReal;
begin { TProcedimiento.ConstruirDerivadaSegundaSubmatrizCovarianzaGrupoEfectosAleatorios }
  // Inicializar valores de indices de matrices
  IndiceFila := FilaInicial;

  // Verificar que los indices de parametros sean validos
  if PrimerParametro >= Informacion.Parametros.Dimension then
    PrimerParametro := PrimerParametro mod Informacion.Parametros.Dimension;

  if SegundoParametro >= Informacion.Parametros.Dimension then
    SegundoParametro := SegundoParametro mod Informacion.Parametros.Dimension;

  // La derivada segunda se anula, salvo excepciones
  SeAnula := true;

  if (Informacion.Estructura.Nombre = 'componentes_varianza') then
  begin
    // Estructura Componentes de Varianza
  end
  else if (Informacion.Estructura.Nombre = 'simetria_compuesta') then
  begin
    // Estructura Simetria Compuesta
  end
  else if (Informacion.Estructura.Nombre = 'general') then
  begin
    // Estructura General
  end
  else if (Informacion.Estructura.Nombre = 'diagonal_heterogenea') then
  begin
    // Estructura Diagonal Heterogenea
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic') or (Informacion.Estructura.Nombre = 'factor_analytic_diagonal_escalar') then
  begin
    // Estructura Factor Analytic

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar indices de nivel de criterio de agrupamiento
        if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
        end
        else
          // El indice corresponde al primer grupo
          I := 0;

        if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
        end
        else
          // El indice corresponde al primer grupo
          J := 0;

        // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
        if I = J then
        begin
          IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

          // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
          I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
          J := PrimerParametro + 1;
          K := SegundoParametro + 1;

          if (J <= I) and (K <= I) then
          begin
            // Ambos parametros viven en A

            // Determinar indice de fila y columna dentro de A de ambos parametros
            IndiceFilaPrimerParametro := 0;
            IndiceFilaSegundoParametro := 0;
            IndiceColumnaPrimerParametro := 0;
            IndiceColumnaSegundoParametro := 0;
            I := 0;
            IndiceFilaSubmatriz := 0;
            L := 1;
            Listo := false;
            Listo1 := false;
            while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
            begin
              // Verificar si el primer parametro se encuentra en la fila actual
              if (not Listo) and (J <= I + L) then
              begin
                IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
                IndiceColumnaPrimerParametro := J - I - 1;
                Listo := true;
              end;

              // Verificar si el segundo parametro se encuentra en la fila actual
              if (not Listo1) and (K <= I + L) then
              begin
                IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
                IndiceColumnaSegundoParametro := K - I - 1;
                Listo1 := true;
              end;

              // Actualizar conteo de parametros, de ser necesario
              if (not Listo) or (not Listo1) then
              begin
                I := I + L;
                Inc(IndiceFilaSubmatriz);
                L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
              end;
            end;

            // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
            if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
            begin
              // La derivada no se anula
              SeAnula := false;

              for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
              begin
                // Definir la fila inicial del nivel
                IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

                // Sumar la cantidad de columnas (o filas) necesarias
                IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

                Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
                Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
              end;
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        if (J <= I) and (K <= I) then
        begin
          // Ambos parametros viven en A

          // Determinar indice de fila y columna dentro de A de ambos parametros
          IndiceFilaPrimerParametro := 0;
          IndiceFilaSegundoParametro := 0;
          IndiceColumnaPrimerParametro := 0;
          IndiceColumnaSegundoParametro := 0;
          I := 0;
          IndiceFilaSubmatriz := 0;
          L := 1;
          Listo := false;
          Listo1 := false;
          while (I <= Informacion.Parametros.Dimension) and not (Listo and Listo1) do
          begin
            // Verificar si el primer parametro se encuentra en la fila actual
            if (not Listo) and (J <= I + L) then
            begin
              IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
              IndiceColumnaPrimerParametro := J - I - 1;
              Listo := true;
            end;

            // Verificar si el segundo parametro se encuentra en la fila actual
            if (not Listo1) and (K <= I + L) then
            begin
              IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
              IndiceColumnaSegundoParametro := K - I - 1;
              Listo1 := true;
            end;

            // Actualizar conteo de parametros, de ser necesario
            if (not Listo) or (not Listo1) then
            begin
              I := I + L;
              Inc(IndiceFilaSubmatriz);
              L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
            end;
          end;

          // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
          if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
          begin
            // La derivada no se anula
            SeAnula := false;

            for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
            begin
              // Definir la fila inicial del nivel
              IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

              Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
              Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
            end;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar indices de nivel de criterio de agrupamiento
      if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end
      else
        // El indice corresponde al primer grupo
        I := 0;

      if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
      end
      else
        // El indice corresponde al primer grupo
        J := 0;

      // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
      if I = J then
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

        // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
        I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        if (J <= I) and (K <= I) then
        begin
          // Ambos parametros viven en A

          // Determinar indice de fila y columna dentro de A de ambos parametros
          IndiceFilaPrimerParametro := 0;
          IndiceFilaSegundoParametro := 0;
          IndiceColumnaPrimerParametro := 0;
          IndiceColumnaSegundoParametro := 0;
          I := 0;
          IndiceFilaSubmatriz := 0;
          L := 1;
          Listo := false;
          Listo1 := false;
          while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
          begin
            // Verificar si el primer parametro se encuentra en la fila actual
            if (not Listo) and (J <= I + L) then
            begin
              IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
              IndiceColumnaPrimerParametro := J - I - 1;
              Listo := true;
            end;

            // Verificar si el segundo parametro se encuentra en la fila actual
            if (not Listo1) and (K <= I + L) then
            begin
              IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
              IndiceColumnaSegundoParametro := K - I - 1;
              Listo1 := true;
            end;

            // Actualizar conteo de parametros, de ser necesario
            if (not Listo) or (not Listo1) then
            begin
              I := I + L;
              Inc(IndiceFilaSubmatriz);
              L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
            end;
          end;

          // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
          if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
          begin
            // La derivada no se anula
            SeAnula := false;

            // Sumar la cantidad de columnas (o filas) necesarias
            IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

            Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
            Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)
      I := Trunc((Informacion.OrdenEstructuraCovarianza / 2) * (2 * Informacion.CantidadColumnasEfectos - Informacion.OrdenEstructuraCovarianza + 1));
      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      if (J <= I) and (K <= I) then
      begin
        // Ambos parametros viven en A

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= Informacion.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // La derivada no se anula
          SeAnula := false;

          Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
          Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
        end;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'factor_analytic_sin_diagonal') then
  begin
    // Estructura Factor Analytic sin Diagonal

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar indices de nivel de criterio de agrupamiento
        if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
        end
        else
          // El indice corresponde al primer grupo
          I := 0;

        if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
        end
        else
          // El indice corresponde al primer grupo
          J := 0;

        // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
        if I = J then
        begin
          IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

          J := PrimerParametro + 1;
          K := SegundoParametro + 1;

          // Determinar indice de fila y columna dentro de A de ambos parametros
          IndiceFilaPrimerParametro := 0;
          IndiceFilaSegundoParametro := 0;
          IndiceColumnaPrimerParametro := 0;
          IndiceColumnaSegundoParametro := 0;
          I := 0;
          IndiceFilaSubmatriz := 0;
          L := 1;
          Listo := false;
          Listo1 := false;
          while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
          begin
            // Verificar si el primer parametro se encuentra en la fila actual
            if (not Listo) and (J <= I + L) then
            begin
              IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
              IndiceColumnaPrimerParametro := J - I - 1;
              Listo := true;
            end;

            // Verificar si el segundo parametro se encuentra en la fila actual
            if (not Listo1) and (K <= I + L) then
            begin
              IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
              IndiceColumnaSegundoParametro := K - I - 1;
              Listo1 := true;
            end;

            // Actualizar conteo de parametros, de ser necesario
            if (not Listo) or (not Listo1) then
            begin
              I := I + L;
              Inc(IndiceFilaSubmatriz);
              L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
            end;
          end;

          // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
          if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
          begin
            // La derivada no se anula
            SeAnula := false;

            for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
            begin
              // Definir la fila inicial del nivel
              IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

              // Sumar la cantidad de columnas (o filas) necesarias
              IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

              Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
              Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= Informacion.Parametros.Dimension) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // La derivada no se anula
          SeAnula := false;

          for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
          begin
            // Definir la fila inicial del nivel
            IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * Informacion.CantidadColumnasEfectos;

            Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
            Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar indices de nivel de criterio de agrupamiento
      if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end
      else
        // El indice corresponde al primer grupo
        I := 0;

      if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
      end
      else
        // El indice corresponde al primer grupo
        J := 0;

      // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
      if I = J then
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

        J := PrimerParametro + 1;
        K := SegundoParametro + 1;

        // Determinar indice de fila y columna dentro de A de ambos parametros
        IndiceFilaPrimerParametro := 0;
        IndiceFilaSegundoParametro := 0;
        IndiceColumnaPrimerParametro := 0;
        IndiceColumnaSegundoParametro := 0;
        I := 0;
        IndiceFilaSubmatriz := 0;
        L := 1;
        Listo := false;
        Listo1 := false;
        while (I <= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental]) and not (Listo and Listo1) do
        begin
          // Verificar si el primer parametro se encuentra en la fila actual
          if (not Listo) and (J <= I + L) then
          begin
            IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
            IndiceColumnaPrimerParametro := J - I - 1;
            Listo := true;
          end;

          // Verificar si el segundo parametro se encuentra en la fila actual
          if (not Listo1) and (K <= I + L) then
          begin
            IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
            IndiceColumnaSegundoParametro := K - I - 1;
            Listo1 := true;
          end;

          // Actualizar conteo de parametros, de ser necesario
          if (not Listo) or (not Listo1) then
          begin
            I := I + L;
            Inc(IndiceFilaSubmatriz);
            L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
          end;
        end;

        // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
        if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
        begin
          // La derivada no se anula
          SeAnula := false;

          // Sumar la cantidad de columnas (o filas) necesarias
          IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

          Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
          Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Verificar que ambos parametros vivan en A (de otro modo la derivada es nula)

      J := PrimerParametro + 1;
      K := SegundoParametro + 1;

      // Determinar indice de fila y columna dentro de A de ambos parametros
      IndiceFilaPrimerParametro := 0;
      IndiceFilaSegundoParametro := 0;
      IndiceColumnaPrimerParametro := 0;
      IndiceColumnaSegundoParametro := 0;
      I := 0;
      IndiceFilaSubmatriz := 0;
      L := 1;
      Listo := false;
      Listo1 := false;
      while (I <= Informacion.Parametros.Dimension) and not (Listo and Listo1) do
      begin
        // Verificar si el primer parametro se encuentra en la fila actual
        if (not Listo) and (J <= I + L) then
        begin
          IndiceFilaPrimerParametro := IndiceFilaSubmatriz;
          IndiceColumnaPrimerParametro := J - I - 1;
          Listo := true;
        end;

        // Verificar si el segundo parametro se encuentra en la fila actual
        if (not Listo1) and (K <= I + L) then
        begin
          IndiceFilaSegundoParametro := IndiceFilaSubmatriz;
          IndiceColumnaSegundoParametro := K - I - 1;
          Listo1 := true;
        end;

        // Actualizar conteo de parametros, de ser necesario
        if (not Listo) or (not Listo1) then
        begin
          I := I + L;
          Inc(IndiceFilaSubmatriz);
          L := Min(IndiceFilaSubmatriz + 1, Informacion.OrdenEstructuraCovarianza);
        end;
      end;

      // Verificar que los parametros se encuentran en la misma columna (de otro modo la derivada es nula)
      if IndiceColumnaPrimerParametro = IndiceColumnaSegundoParametro then
      begin
        // La derivada no se anula
        SeAnula := false;

        Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] := Matriz [IndiceFila + IndiceFilaPrimerParametro, IndiceFila + IndiceFilaSegundoParametro] + 1.0;
        Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] := Matriz [IndiceFila + IndiceFilaSegundoParametro, IndiceFila + IndiceFilaPrimerParametro] + 1.0;
      end;
    end;
  end
  else if (Informacion.Estructura.Nombre = 'bandeada') then
  begin
    // Estructura Bandeada
  end
  else if (Informacion.Estructura.Nombre = 'autoregresiva') then
  begin
    // Estructura Autoregresiva de primer orden

    SeAnula := False;

    if Informacion.TieneUnidadExperimental then
    begin
      // Se definio unidad experimental

      // Verificar si se definio criterio agrupamiento unidad experimental
      if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
      begin
        // Se definio unidad experimental y criterio agrupamiento unidad experimental

        // Determinar indices de nivel de criterio de agrupamiento
        if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
        end
        else
          // El indice corresponde al primer grupo
          I := 0;

        if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
        begin
          // El indice no corresponde al primer grupo
          J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
          SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
        end
        else
          // El indice corresponde al primer grupo
          J := 0;

        // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
        if I = J then
        begin
          IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

          // Determinar el indice del primer parametro del grupo
          IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

          // Si se deriva dos veces sobre el parametro de la varianza (sigma
          // cuadrado) la derivada se anula.
          if (PrimerParametro = 0) and (SegundoParametro = 0) then
            SeAnula := True
          else
          begin
            if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
               ((PrimerParametro = 1) and (SegundoParametro = 0)) then
            begin
              for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
              begin
                // Definir la fila inicial del nivel
                IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

                // Sumar la cantidad de columnas (o filas) necesarias
                IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

                for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
                begin
                  Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

                  R1 := 1.0;
                  K := 1;
                  for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
                  begin
                    Matriz [IndiceFila + I, IndiceFila + J] := K * R1;
                    Matriz [IndiceFila + J, IndiceFila + I] := K * R1;
                    R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                    Inc(K);
                  end;
                end;
              end;
            end
            else if (PrimerParametro = 1) and (SegundoParametro = 1) then
            begin
              for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
              begin
                // Definir la fila inicial del nivel
                IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

                // Sumar la cantidad de columnas (o filas) necesarias
                IndiceFila := IndiceFila + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

                for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
                begin
                  Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

                  R1 := Informacion.Parametros [IndiceParametro];
                  R2 := 1.0;
                  K := 1;
                  for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
                  begin
                    if K = 1 then
                    begin
                      R2 := 2.0;
                      Matriz [IndiceFila + I, IndiceFila + J] := 0.0;
                      Matriz [IndiceFila + J, IndiceFila + I] := 0.0;
                    end
                    else
                    begin
                      Matriz [IndiceFila + I, IndiceFila + J] := R1 * R2;
                      Matriz [IndiceFila + J, IndiceFila + I] := R1 * R2;
                      R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                      R2 := R2 * (K + 1);
                    end;
                    Inc(K);
                  end;
                end;
              end;
            end;
          end;
        end;
      end
      else
      begin
        // Se definio unidad experimental y no se definio criterio agrupamiento unidad experimental

        // Determinar el indice del primer parametro
        IndiceParametro := 0;

        // Si se deriva dos veces sobre el parametro de la varianza (sigma
        // cuadrado) la derivada se anula.
        if (PrimerParametro = 0) and (SegundoParametro = 0) then
          SeAnula := True
        else
        begin
          if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
             ((PrimerParametro = 1) and (SegundoParametro = 0)) then
          begin
            for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
            begin
              // Definir la fila inicial del nivel
              IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

              for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

                R1 := 1.0;
                K := 1;
                for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
                begin
                  Matriz [IndiceFila + I, IndiceFila + J] := K * R1;
                  Matriz [IndiceFila + J, IndiceFila + I] := K * R1;
                  R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                  Inc(K);
                end;
              end;
            end;
          end
          else if (PrimerParametro = 1) and (SegundoParametro = 1) then
          begin
            for IndiceNivelUnidadExperimental := Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesNiveles.Alto do
            begin
              // Definir la fila inicial del nivel
              IndiceFila := FilaInicial + IndiceNivelUnidadExperimental * (Informacion.CantidadColumnasEfectos * Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.CantidadNiveles);

              for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

                R1 := Informacion.Parametros [IndiceParametro];
                R2 := 1.0;
                K := 1;
                for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
                begin
                  if K = 1 then
                  begin
                    R2 := 2.0;
                    Matriz [IndiceFila + I, IndiceFila + J] := 0.0;
                    Matriz [IndiceFila + J, IndiceFila + I] := 0.0;
                  end
                  else
                  begin
                    Matriz [IndiceFila + I, IndiceFila + J] := R1 * R2;
                    Matriz [IndiceFila + J, IndiceFila + I] := R1 * R2;
                    R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                    R2 := R2 * (K + 1);
                  end;
                  Inc(K);
                end;
              end;
            end;
          end;
        end;
      end;
    end
    else if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    begin
      // No se definio unidad experimental, se definio criterio agrupamiento unidad experimental

      // Determinar indices de nivel de criterio de agrupamiento
      if PrimerParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        I := PrimerParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        PrimerParametro := PrimerParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [I];
      end
      else
        // El indice corresponde al primer grupo
        I := 0;

      if SegundoParametro >= Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0] then
      begin
        // El indice no corresponde al primer grupo
        J := SegundoParametro div Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [0];
        SegundoParametro := SegundoParametro mod Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [J];
      end
      else
        // El indice corresponde al primer grupo
        J := 0;

      // Verificar que pertenezcan a los parametros del mismo nivel del criterio agrupamiento unidad experimental (de otro modo la derivada es nula)
      if I = J then
      begin
        IndiceNivelCriterioAgrupamientoUnidadExperimental := I;

        // Determinar el indice del primer parametro del grupo
        IndiceParametro := IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadParametrosNivelCriterioAgrupamientoUnidadExperimental [IndiceNivelCriterioAgrupamientoUnidadExperimental];

        // Si se deriva dos veces sobre el parametro de la varianza (sigma
        // cuadrado) la derivada se anula.
        if (PrimerParametro = 0) and (SegundoParametro = 0) then
          SeAnula := True
        else
        begin
          if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
             ((PrimerParametro = 1) and (SegundoParametro = 0)) then
          begin
            // Sumar la cantidad de columnas (o filas) necesarias
            IndiceFila := FilaInicial + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

              R1 := 1.0;
              K := 1;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := K * R1;
                Matriz [IndiceFila + J, IndiceFila + I] := K * R1;
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                Inc(K);
              end;
            end;
          end
          else if (PrimerParametro = 1) and (SegundoParametro = 1) then
          begin
            // Sumar la cantidad de columnas (o filas) necesarias
            IndiceFila := FilaInicial + IndiceNivelCriterioAgrupamientoUnidadExperimental * Informacion.CantidadColumnasEfectos;

            for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

              R1 := Informacion.Parametros [IndiceParametro];
              R2 := 1.0;
              K := 1;
              for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
              begin
                if K = 1 then
                begin
                  R2 := 2.0;
                  Matriz [IndiceFila + I, IndiceFila + J] := 0.0;
                  Matriz [IndiceFila + J, IndiceFila + I] := 0.0;
                end
                else
                begin
                  Matriz [IndiceFila + I, IndiceFila + J] := R1 * R2;
                  Matriz [IndiceFila + J, IndiceFila + I] := R1 * R2;
                  R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                  R2 := R2 * (K + 1);
                end;
                Inc(K);
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // No se definio ni unidad experimental, ni criterio agrupamiento unidad experimental

      // Determinar el indice del primer parametro
      IndiceParametro := 0;

      // Si se deriva dos veces sobre el parametro de la varianza (sigma
      // cuadrado) la derivada se anula.
      if (PrimerParametro = 0) and (SegundoParametro = 0) then
        SeAnula := True
      else
      begin
        if ((PrimerParametro = 0) and (SegundoParametro = 1)) or
           ((PrimerParametro = 1) and (SegundoParametro = 0)) then
        begin
          // Definir la fila inicial del nivel
          IndiceFila := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

            R1 := 1.0;
            K := 1;
            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              Matriz [IndiceFila + I, IndiceFila + J] := K * R1;
              Matriz [IndiceFila + J, IndiceFila + I] := K * R1;
              R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
              Inc(K);
            end;
          end;
        end
        else if (PrimerParametro = 1) and (SegundoParametro = 1) then
        begin
          // Definir la fila inicial del nivel
          IndiceFila := 0;

          for I := 0 to Informacion.CantidadColumnasEfectos - 1 do
          begin
            Matriz [IndiceFila + I, IndiceFila + I] := 0.0 ;

            R1 := Informacion.Parametros [IndiceParametro];
            K := 1;
            for J := I + 1 to Informacion.CantidadColumnasEfectos - 1 do
            begin
              if K = 1 then
              begin
                R2 := 2.0;
                Matriz [IndiceFila + I, IndiceFila + J] := 0.0;
                Matriz [IndiceFila + J, IndiceFila + I] := 0.0;
              end
              else
              begin
                Matriz [IndiceFila + I, IndiceFila + J] := R1 * R2;
                Matriz [IndiceFila + J, IndiceFila + I] := R1 * R2;
                R1 := R1 * Informacion.Parametros [IndiceParametro + 1];
                R2 := R2 * (K + 1);
              end;
              Inc(K);
            end;
          end;
        end;
      end;
    end;
  end;

  Result := SeAnula;
end { TProcedimiento.ConstruirDerivadaSegundaSubmatrizCovarianzaGrupoEfectosAleatorios };

function TProcedimiento.ConstruirInformacionTiempoEjecucionCriterio(const Criterio: TEfecto): TInformacionTiempoEjecucionCriterio;
var
  I, J, K, L: Integer;
  Variables: TStrings;
  Niveles: TArregloStrings;
  Indices: TArregloEnteros;
  Informacion: TInformacionTiempoEjecucionCriterio;
begin { TProcedimiento.ConstruirInformacionTiempoEjecucionCriterio }
  // Inicializar referencias
  Niveles := nil;
  Variables := nil;
  Indices := nil;
  Informacion := nil;

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

      for L := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
      begin
        if Niveles.IndiceDe(FModelo.Datos.Valores [L, J]) = -1 then
        begin
          Niveles.Dimension := Niveles.Dimension + 1;
          Niveles [Niveles.Alto] := FModelo.Datos.Valores [L, J];
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
  Informacion.IndicesNiveles := TArregloArreglosEnteros.Create(0);
  Informacion.IndicesNivelesObservaciones := TArregloEnteros.Create(FModelo.Datos.Valores.CantidadFilas);
  for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
  begin
    Indices := TArregloEnteros.Create(Informacion.IndicesColumnasVariablesClasificacion.Dimension);

    // Leer los indices de las variables del criterio para la observacion
    for J := Informacion.IndicesColumnasVariablesClasificacion.Bajo to Informacion.IndicesColumnasVariablesClasificacion.Alto do
      Indices [J] := FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [J]].IndiceDe(FModelo.Datos.Valores [I, Informacion.IndicesColumnasVariablesClasificacion [J]]);

    // Verificar si el arreglo de indices se encuentra en la lista de indices de niveles
    K := Informacion.IndicesNiveles.IndiceDe(Indices);
    if K = -1 then
    begin
      // El nivel no se encuentra en el arreglo
      Informacion.IndicesNiveles.Dimension := Informacion.IndicesNiveles.Dimension + 1;
      Informacion.IndicesNiveles.Arreglos [Informacion.IndicesNiveles.Alto] := Indices;
      K := Informacion.IndicesNiveles.Alto;
    end;

    // Almacenar el indice del nivel para la observacion
    Informacion.IndicesNivelesObservaciones [I] := K;
  end;

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
        J := FModelo.Datos.Valores.Bajo;
        while (J <= FModelo.Datos.Valores.Alto) and B do
        begin
          K := Informacion.IndicesColumnasVariablesClasificacion.Bajo;
          C := true;
          while (K <= Informacion.IndicesColumnasVariablesClasificacion.Alto) and C do
          begin
            C := C and (FModelo.Datos.Valores [J, Informacion.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion.Arreglos [Informacion.IndicesNivelesVariablesClasificacion [K]] [Informacion.IndicesNiveles [I, K]]);
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
  I, IndiceEfecto, IndiceParametro, IndiceNivel: Integer;
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
    end
  end
  else
  begin
    for I := Informacion.InformacionTiempoEjecucionEfectos.Bajo to Informacion.InformacionTiempoEjecucionEfectos.Alto do
    begin
      Informacion.CantidadColumnasSubmatrizDisenoEfecto [I] := Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNiveles - Informacion.InformacionTiempoEjecucionEfectos [I].CantidadNivelesInexistentes;
      Informacion.CantidadColumnasMatrizDiseno := Informacion.CantidadColumnasMatrizDiseno + Informacion.CantidadColumnasSubmatrizDisenoEfecto [I];
    end
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
  IdentificadorNivel: String;
  IdentificadoresNivelesCriterioAgrupamiento: TArregloStrings;
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
    Dimension := FModelo.Datos.Valores.CantidadFilas;

  // Calcular dimension de las matriz de covarianza del grupo
  Informacion.CantidadFilasMatrizCovarianza := FModelo.Datos.Valores.CantidadFilas;

  // Construir arreglo de identificadores de indices de niveles de criterio de agrupamiento en unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    IdentificadoresNivelesCriterioAgrupamiento := TArregloStrings.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Dimension);

    for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
    begin
      IdentificadorNivel := '';

      for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto - 1 do
        IdentificadorNivel := IdentificadorNivel + FMatrizNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion [J], Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles [I, J]] + ' ';
      IdentificadorNivel := IdentificadorNivel + FMatrizNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto], Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles [I, Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto]];

      IdentificadoresNivelesCriterioAgrupamiento [I] := IdentificadorNivel;
    end;
  end;

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
        Informacion.Parametros.Nombres [IndiceParametro] := 'cv_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        for J := 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Informacion.CantidadMaximaObservacionesNivelCriterioAgrupamientoUnidadExperimental [I] do
          begin
            if Abs(J - K) < Informacion.OrdenEstructuraCovarianzaNivelCriterioAgrupamientoUnidadExperimental [I] then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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

  // Liberar arreglo de identificadores
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    FreeAndNil(IdentificadoresNivelesCriterioAgrupamiento);

  // Ingresar valores de parametros desde la configuracion del modelo
  if Informacion.Opciones.BuscarAsignar(I, 'parametros') then
  begin
    ListaParametros := (Informacion.Opciones [I] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

    J := 0;
    while (J <= ListaParametros.Count - 1) and (J <= Informacion.Parametros.Alto) do
    begin
      InformacionParametroIndice := ListaParametros [J];

      // Verificar reorden del parametro
      if InformacionParametroIndice.Indice <> -1 then
        K := InformacionParametroIndice.Indice
      else
        K := J;

      Informacion.Parametros.InformacionParametros [K].CotaInferior := InformacionParametroIndice.CotaInferior;
      Informacion.Parametros.InformacionParametros [K].CotaSuperior := InformacionParametroIndice.CotaSuperior;
      Informacion.Parametros.InformacionParametros [K].Paso := InformacionParametroIndice.Paso;
      Informacion.Parametros.InformacionParametros [K].TieneCotaInferior := InformacionParametroIndice.TieneCotaInferior;
      Informacion.Parametros.InformacionParametros [K].TieneCotaSuperior := InformacionParametroIndice.TieneCotaSuperior;
      Informacion.Parametros.InformacionParametros [K].TienePaso := InformacionParametroIndice.TienePaso;
      Informacion.Parametros.InformacionParametros [K].TieneValorInicial := InformacionParametroIndice.TieneValorInicial;
      Informacion.Parametros.InformacionParametros [K].ValorInicial := InformacionParametroIndice.ValorInicial;

      // El valor de indice luego es reutilizado en la inicializacion del
      // procedimiento y el valor del nombre ya fue inicializado.

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
  IdentificadorNivel: String;
  IdentificadoresNivelesCriterioAgrupamiento: TArregloStrings;
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

  // Construir arreglo de identificadores de indices de niveles de criterio de agrupamiento en unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
  begin
    IdentificadoresNivelesCriterioAgrupamiento := TArregloStrings.Create(Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Dimension);

    for I := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles.Alto do
    begin
      IdentificadorNivel := '';

      for J := Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Bajo to Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto - 1 do
        IdentificadorNivel := IdentificadorNivel + FMatrizNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion [J], Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles [I, J]] + ' ';
      IdentificadorNivel := IdentificadorNivel + FMatrizNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto], Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNiveles [I, Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesNivelesVariablesClasificacion.Alto]];

      IdentificadoresNivelesCriterioAgrupamiento [I] := IdentificadorNivel;
    end;
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'cv_[' + Informacion.InformacionTiempoEjecucionEfectos [J].Efecto.Texto + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[1]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'sc_[2]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Dimension do
          begin
            Informacion.Parametros.Nombres [IndiceParametro] := 'ge_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'dh_[' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        for J := 1 to Dimension do
        begin
          Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_[' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
              Informacion.Parametros.Nombres [IndiceParametro] := 'fa_A_[' + IntToStr(J) + ', ' + IntToStr(K) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
              Inc(IndiceParametro);
            end;
          end;
        end;

        // Ingresar los parametros correspondientes a la matriz diagonal
        Informacion.Parametros.Nombres [IndiceParametro] := 'fa_D_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
          Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(J) + ']_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
          Inc(IndiceParametro);

          for K := J + 1 to Dimension do
          begin
            if Abs(J - K) < Orden then
            begin
              Informacion.Parametros.Nombres [IndiceParametro] := 'ba_[' + IntToStr(J) + ', ' + IntToStr(K) + ']' + '_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[1]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
        Inc(IndiceParametro);
        Informacion.Parametros.Nombres [IndiceParametro] := 'ar_[2]_grupo(' + IdentificadoresNivelesCriterioAgrupamiento [I] + ')';
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

  // Liberar arreglo de identificadores de niveles de criterio de agrupamiento de unidad experimental
  if Informacion.TieneCriterioAgrupamientoUnidadExperimental then
    FreeAndNil(IdentificadoresNivelesCriterioAgrupamiento);

  // Ingresar valores de parametros desde la configuracion del modelo
  if Informacion.Opciones.BuscarAsignar(I, 'parametros') then
  begin
    ListaParametros := (Informacion.Opciones [I] as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

    J := 0;
    while (J <= ListaParametros.Count - 1) and (J <= Informacion.Parametros.Alto) do
    begin
      InformacionParametroIndice := ListaParametros [J];

      // Verificar reorden del parametro
      if InformacionParametroIndice.Indice <> -1 then
        K := InformacionParametroIndice.Indice
      else
        K := J;

      // Asignar la tupla
      Informacion.Parametros.InformacionParametros [K].CotaInferior := InformacionParametroIndice.CotaInferior;
      Informacion.Parametros.InformacionParametros [K].CotaSuperior := InformacionParametroIndice.CotaSuperior;
      Informacion.Parametros.InformacionParametros [K].Paso := InformacionParametroIndice.Paso;
      Informacion.Parametros.InformacionParametros [K].TieneCotaInferior := InformacionParametroIndice.TieneCotaInferior;
      Informacion.Parametros.InformacionParametros [K].TieneCotaSuperior := InformacionParametroIndice.TieneCotaSuperior;
      Informacion.Parametros.InformacionParametros [K].TienePaso := InformacionParametroIndice.TienePaso;
      Informacion.Parametros.InformacionParametros [K].TieneValorInicial := InformacionParametroIndice.TieneValorInicial;
      Informacion.Parametros.InformacionParametros [K].ValorInicial := InformacionParametroIndice.ValorInicial;

      // El valor de indice luego es reutilizado en la inicializacion del
      // procedimiento.

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
  R1: TUaReal;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
  InformacionError: TInformacionTiempoEjecucionError;
  IndicesUltimaObservacion: TArregloEnteros;
  ArregloIndicesUltimaObservacion: TArregloArreglosEnteros;
  SubmatrizUnidadExperimental: TUaMatriz;
  SubmatricesNivelesCriterioAgrupamientoUnidadExperimental: array of TUaMatriz;
begin { TProcedimiento.ConstruirMatrizCovarianzaError }
  // Inicializar referencias
  IndicesUltimaObservacion := nil;
  ArregloIndicesUltimaObservacion := nil;
  SubmatrizUnidadExperimental := nil;
  SubmatricesNivelesCriterioAgrupamientoUnidadExperimental := nil;

  InformacionError := FInformacionTiempoEjecucionError;

  if not Assigned(Matriz) then
    // Llamada inicial, dimensionar
    Matriz := TUaMatriz.Create(FModelo.Datos.Valores.CantidadFilas, FModelo.Datos.Valores.CantidadFilas)
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
  I := FModelo.Datos.Valores.CantidadFilas;
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
  Matriz := TUaMatriz.Create(FModelo.Datos.Valores.CantidadFilas, CantidadColumnas);

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
  R1: TUaReal;
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
      V := TUaVector.Create(FModelo.Datos.Valores.CantidadFilas);
      for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
      begin
        R := 1;
        for J := Informacion.IndicesColumnasCovariables.Bajo to Informacion.IndicesColumnasCovariables.Alto do
          R := R * StrToFloat(FModelo.Datos.Valores [I, Informacion.IndicesColumnasCovariables [J]]);
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
          for J := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
          begin
            // Verificar la coincidencia del nivel en la columna y fila
            K := Informacion.IndicesColumnasVariablesClasificacion.Bajo;
            B := true;
            while (K <= Informacion.IndicesColumnasVariablesClasificacion.Alto) and B do
            begin
              B := FModelo.Datos.Valores [J, Informacion.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [Informacion.IndicesNivelesVariablesClasificacion [K], Informacion.IndicesNiveles [I, K]];
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

      if Informacion.TieneCovariables then
        FreeAndNil(V);
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
      V := TUaVector.Create(FModelo.Datos.Valores.CantidadFilas);

      for J := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
      begin
        R := 1;
        for K := InformacionEfecto.IndicesColumnasCovariables.Bajo to InformacionEfecto.IndicesColumnasCovariables.Alto do
          R := R * StrToFloat(FModelo.Datos.Valores [J, InformacionEfecto.IndicesColumnasCovariables [K]]);
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
                            B := FModelo.Datos.Valores [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
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
                      B := FModelo.Datos.Valores [Informacion.InformacionTiempoEjecucionUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelUnidadExperimental, I], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
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
                      B := FModelo.Datos.Valores [Informacion.InformacionTiempoEjecucionCriterioAgrupamientoUnidadExperimental.IndicesObservacionesNiveles [IndiceNivelCriterioAgrupamientoUnidadExperimental, J], InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
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
          for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
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
                for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
                begin
                  // Verificar la coincidencia del nivel del efecto en columna y fila
                  B := true;
                  K := InformacionEfecto.IndicesColumnasVariablesClasificacion.Bajo;
                  while (K <= InformacionEfecto.IndicesColumnasVariablesClasificacion.Alto) and B do
                  begin
                    B := FModelo.Datos.Valores [I, InformacionEfecto.IndicesColumnasVariablesClasificacion [K]] = FMatrizNivelesVariablesClasificacion [InformacionEfecto.IndicesNivelesVariablesClasificacion [K], InformacionEfecto.IndicesNiveles [IndiceNivelEfecto, K]];
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

            for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
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

  Assert(not Assigned(VectoresVariablesRegresion), 'TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios: not Assigned(VectoresVariablesRegresion)');
end { TProcedimiento.ConstruirSubmatrizDisenoGrupoEfectosAleatorios };

procedure TProcedimiento.ConstruirVectorObservacionesVariableDependiente(var Matriz: TUaMatriz);
var
  I, J: Integer;
begin { TProcedimiento.ConstruirVectorObservacionesVariableDependiente }
  if Assigned(Matriz) then
    FreeAndNil(Matriz);

  Matriz := TUaMatriz.Create(FModelo.Datos.Valores.CantidadFilas, 1);

  J := FModelo.Datos.Columnas.IndexOf(FModelo.VariableDependiente);
  for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
    Matriz [I + 1, 1] := StrToFloat(FModelo.Datos.Valores [I, J]);
end { TProcedimiento.ConstruirVectorObservacionesVariableDependiente };

procedure TProcedimiento.ConstruirVectorPesos(var Vector: TUaVector);
var
  I, J: Integer;
begin { TProcedimiento.ConstruirVectorPesos }
  if Assigned(Vector) then
    FreeAndNil(Vector);

  Vector := TUaVector.Create(FModelo.Datos.Valores.CantidadFilas);

  J := FModelo.Datos.Columnas.IndexOf(FModelo.VariablePeso);
  for I := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
    Vector [I + 1] := StrToFloat(FModelo.Datos.Valores [I, J]);
end { TProcedimiento.ConstruirVectorPesos };

function TProcedimiento.MetodoGrillaBusqueda(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
var
  I, J, IndiceParametro: Integer;
  Indices, IndicesSuperiores, IndicesMinimo: TArregloEnteros;
  IncluirParametroBusqueda: array of Boolean;
  ValoresIniciales, Pasos: TUaVector;
  LActual, L1Minimo, L2Minimo, L3Minimo, LMinimo, TerminoConstanteVerosimilitud: TUaReal;
  Listo, Continuar, MinimoValidoEncontrado, ParametrosValidos: Boolean;
  Bitacora1: TStrings;
begin { TProcedimiento.MetodoGrillaBusqueda }
  Assert(Assigned(ParametrosEfectosAleatorios), 'TProcedimiento.MetodoGrillaBusqueda: Assigned(ParametrosEfectosAleatorios)');
  Assert(Assigned(Bitacora), 'TProcedimiento.MetodoGrillaBusqueda: Assigned(Bitacora)');
  Assert(Assigned(BitacoraIteracion), 'TProcedimiento.MetodoGrillaBusqueda: Assigned(BitacoraIteracion)');

  Bitacora.Add('MetodoGrillaBusqueda: Inicializando arreglos de indices.');

  // Inicializar referencias
  FreeAndNil(FWSA);
  FreeAndNil(FWSD);
  FreeAndNil(FW2SA);
  FreeAndNil(FB);
  FreeAndNil(FBT);

  // Determinar que parametros incluir en la busqueda
  SetLength(IncluirParametroBusqueda, FCantidadParametrosCovarianza);
  IndiceParametro := 0;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
  begin
    for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
    begin
      if FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaInferior and
         FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaSuperior and
         FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TienePaso then
        IncluirParametroBusqueda [IndiceParametro] := True
      else
        IncluirParametroBusqueda [IndiceParametro] := False;

      Inc(IndiceParametro);
    end;
  end;
  for J := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
  begin
    if FInformacionTiempoEjecucionError.Parametros.InformacionParametros [J].TieneCotaInferior and
       FInformacionTiempoEjecucionError.Parametros.InformacionParametros [J].TieneCotaSuperior and
       FInformacionTiempoEjecucionError.Parametros.InformacionParametros [J].TienePaso then
      IncluirParametroBusqueda [IndiceParametro] := True
    else
      IncluirParametroBusqueda [IndiceParametro] := False;

    Inc(IndiceParametro);
  end;

  // Inicializar arreglos de indices
  Indices := TArregloEnteros.Create(ParametrosEfectosAleatorios.Dimension);
  IndicesSuperiores := TArregloEnteros.Create(ParametrosEfectosAleatorios.Dimension);
  IndicesMinimo := TArregloEnteros.Create(ParametrosEfectosAleatorios.Dimension);
  Indices.PonerA(0);

  // Inicializar bitacora local
  Bitacora1 := TStringList.Create;

  // Inicializar arreglo de pasos
  Pasos := TUaVector.Create(ParametrosEfectosAleatorios.Dimension);

  // Calcular los indices superiores, y almacenar pasos
  IndiceParametro := 0;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
  begin
    for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
    begin
      if IncluirParametroBusqueda [IndiceParametro] then
      begin
        IndicesSuperiores [IndiceParametro] := Floor((FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].CotaSuperior - FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].CotaInferior) / FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].Paso);
        Pasos [IndiceParametro + 1] := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].Paso;
      end;
      
      Inc(IndiceParametro);
    end;
  end;

  for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
  begin
    if IncluirParametroBusqueda [IndiceParametro] then
    begin
      IndicesSuperiores [IndiceParametro] := Floor((FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].CotaSuperior - FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].CotaInferior) / FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].Paso);
      Pasos [IndiceParametro + 1] := FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].Paso;
    end;
    
    Inc(IndiceParametro);
  end;

  // Iniciar busqueda
  MinimoValidoEncontrado := False;
  FL1 := 0;
  FL2 := 0;
  FL3 := 0;
  LActual := 0;
  L1Minimo := 0;
  L2Minimo := 0;
  L3Minimo := 0;
  LMinimo := 0;

  FCantidadIteraciones := 0;
  FCantidadIteracionesBucleInterno := 0;
  FCantidadEvaluacionesFuncionObjetivo := 0;

  Bitacora.Add('MetodoGrillaBusqueda: Asignando valores iniciales de parametros.');

  // Asignar los valores iniciales
  ValoresIniciales := TUaVector.Create(ParametrosEfectosAleatorios.Dimension);
  IndiceParametro := 1;
  for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
  begin
    for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
    begin
      if IncluirParametroBusqueda [IndiceParametro - 1] then
      begin
        FValoresParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].CotaInferior;
        ValoresIniciales [IndiceParametro] := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].CotaInferior;
      end;

      Inc(IndiceParametro);
    end;
  end;

  for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
  begin
    if IncluirParametroBusqueda [IndiceParametro - 1] then
    begin
      FValoresParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].CotaInferior;
      ValoresIniciales [IndiceParametro] := FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].CotaInferior;
    end;

    Inc(IndiceParametro);
  end;

  Bitacora.Add('MetodoGrillaBusqueda: Validando valores iniciales de parametros.');

  // Calcular valor del termino constante de la funcion de verosimilitud

  // Validar los parametros iniciales (y construir matrices de covarianza y subproductos)
  FreeAndNil(FL);
  FreeAndNil(FLT);
  FreeAndNil(FRaizCuadradaR);
  FreeAndNil(FRI);

  ParametrosValidos := True;

  if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
  begin
    ConstruirMatrizCovarianzaEfectosAleatorios(FG);

    if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
      FLT := FL.Transpuesta
    else
    begin
      Bitacora.Add('MetodoGrillaBusqueda: Los valores iniciales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
      ParametrosValidos := False;
    end;
  end;

  ConstruirMatrizCovarianzaError(FR);

  if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
    FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
  else
  begin
    Bitacora.Add('MetodoGrillaBusqueda: Los valores iniciales de parametros generan una matriz de covarianza de error no positiva definida.');
    ParametrosValidos := False;
  end;

  Bitacora.Add('MetodoGrillaBusqueda: Calculando valores iniciales de verosimilitud.');

  // Calcular valores de verosimilitud, si los parametros iniciales son validos
  if ParametrosValidos then
  begin
    MinimoValidoEncontrado := True;
    IndicesMinimo.PonerA(0);

    // Calcular valores iniciales de la funcion objetivo (y subproductos)
    FreeAndNil(FWSA);
    FreeAndNil(FWSD);
    FreeAndNil(FW2SA);
    FreeAndNil(FB);
    FreeAndNil(FBT);
    CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);

    Inc(FCantidadEvaluacionesFuncionObjetivo);

    if FCalcularVerosimilitudResidual then
      LActual := FL1 + FL2 + FL3
    else
      LActual := FL1 + FL2;

    LMinimo := LActual;
    L1Minimo := FL1;
    L2Minimo := FL2;
    L3Minimo := FL3;
  end;

  Bitacora.Add('MetodoGrillaBusqueda: Iniciando iteracion.');

  FCantidadIteraciones := 1;
  Continuar := True;
  while (FCantidadEvaluacionesFuncionObjetivo < FCantidadMaximaEvaluacionesFuncionObjetivo) and
        (FCantidadIteraciones <= FCantidadMaximaIteraciones) and
        Continuar do
  begin
    // Actualizar indices
    I := Indices.Bajo;
    Listo := False;
    while (I <= Indices.Alto) and (not Listo) do
    begin
      if IncluirParametroBusqueda [I] then
      begin
        if Indices [I] < IndicesSuperiores [I] then
        begin
          Indices [I] := Indices [I] + 1;
          Listo := True;
        end
        else if I < Indices.Alto then
          Indices [I] := 0
        else
        begin
          Listo := True;
          Continuar := False;
        end;
      end;

      Inc(I);
    end;

    // Verificar si los nuevos indices mejoran la verosimilitud

    // Actualizar valores de parametros
    for IndiceParametro := 1 to FValoresParametrosEfectosAleatorios.Dimension do
    begin
      if IncluirParametroBusqueda [IndiceParametro - 1] then
        FValoresParametrosEfectosAleatorios [IndiceParametro] := ValoresIniciales [IndiceParametro] + Pasos [IndiceParametro] * Indices [IndiceParametro - 1];
    end;

    // Validar los parametros (y construir matrices de covarianza y subproductos)
    FreeAndNil(FL);
    FreeAndNil(FLT);
    FreeAndNil(FRaizCuadradaR);
    FreeAndNil(FRI);

    ParametrosValidos := True;

    if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
    begin
      ConstruirMatrizCovarianzaEfectosAleatorios(FG);

      if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
        FLT := FL.Transpuesta
      else
      begin
        Bitacora.Add('MetodoGrillaBusqueda: Los valores actuales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
        ParametrosValidos := False;
      end;
    end;

    ConstruirMatrizCovarianzaError(FR);

    if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
      FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
    else
    begin
      Bitacora.Add('MetodoGrillaBusqueda: Los valores actuales de parametros generan una matriz de covarianza de error no positiva definida.');
      ParametrosValidos := False;
    end;

    // Si los parametros son validos, calcular verosimilitud, de otro modo, continuar.
    if ParametrosValidos then
    begin
      // Calcular valores iniciales de la funcion objetivo (y subproductos)
      FreeAndNil(FWSA);
      FreeAndNil(FWSD);
      FreeAndNil(FW2SA);
      FreeAndNil(FB);
      FreeAndNil(FBT);
      CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);

      Inc(FCantidadEvaluacionesFuncionObjetivo);

      if FCalcularVerosimilitudResidual then
        LActual := FL1 + FL2 + FL3
      else
        LActual := FL1 + FL2;

      // Compara valor del minimo (de existir)
      if MinimoValidoEncontrado then
      begin
        if LActual < LMinimo then
        begin
          LMinimo := LActual;
          L1Minimo := FL1;
          L2Minimo := FL2;
          L3Minimo := FL3;

          IndicesMinimo.PonerA(Indices);
        end;
      end
      else
      begin
        IndicesMinimo.PonerA(Indices);
        MinimoValidoEncontrado := True;
      end;

      BitacoraIteracion.Add(Format('%-10s %d', ['+ Paso:', FCantidadIteraciones]));
      BitacoraIteracion.Add(Format('  %-8s %g', ['L(actual):', LActual + TerminoConstanteVerosimilitud]));
      BitacoraIteracion.Add(Format('  %-8s %g', ['L(minimo):', LMinimo  + TerminoConstanteVerosimilitud]));
    end;

    Inc(FCantidadIteraciones);
  end;

  Bitacora.Add('MetodoGrillaBusqueda: Fin de iteracion.');

  if MinimoValidoEncontrado then
  begin
    // Se encontro un minimo, asignar valores de parametros

    IndiceParametro := 1;
    for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
    begin
      for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
      begin
        if IncluirParametroBusqueda [IndiceParametro - 1] then
          ParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].CotaInferior + FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].Paso * IndicesMinimo [IndiceParametro - 1]
        else
          ParametrosEfectosAleatorios [IndiceParametro] := FValoresParametrosEfectosAleatorios [IndiceParametro];

        Inc(IndiceParametro);
      end;
    end;

    for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
    begin
      if IncluirParametroBusqueda [IndiceParametro - 1] then
        ParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].CotaInferior + FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].Paso * IndicesMinimo [IndiceParametro - 1]
      else
        ParametrosEfectosAleatorios [IndiceParametro] := FValoresParametrosEfectosAleatorios [IndiceParametro];

      Inc(IndiceParametro);
    end;

    // Recalcular los componentes del modelo para postprocesamiento
    FreeAndNil(FL);
    FreeAndNil(FLT);
    FreeAndNil(FRaizCuadradaR);
    FreeAndNil(FRI);

    if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
    begin
      ConstruirMatrizCovarianzaEfectosAleatorios(FG);

      if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
        FLT := FL.Transpuesta
      else
      begin
        Bitacora.Add('MetodoGrillaBusqueda: Los valores iniciales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
        ParametrosValidos := False;
      end;
    end;

    ConstruirMatrizCovarianzaError(FR);

    if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
      FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
    else
    begin
      Bitacora.Add('MetodoGrillaBusqueda: Los valores iniciales de parametros generan una matriz de covarianza de error no positiva definida.');
      ParametrosValidos := False;
    end;

    FreeAndNil(FWSA);
    FreeAndNil(FWSD);
    FreeAndNil(FW2SA);
    FreeAndNil(FB);
    FreeAndNil(FBT);
    CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);

    Inc(FCantidadEvaluacionesFuncionObjetivo);

    if FCalcularVerosimilitudResidual then
      LMinimo := FL1 + FL2 + FL3
    else
      LMinimo := FL1 + FL2;
  end;

  Result := MinimoValidoEncontrado;

  if FCantidadEvaluacionesFuncionObjetivo > FCantidadMaximaEvaluacionesFuncionObjetivo then
    Bitacora.Add('MetodoGrillaBusqueda: Se supero la cantidad maxima de evaluaciones de la funcion objetivo.');
  if FCantidadIteraciones > FCantidadMaximaIteraciones then
    Bitacora.Add('MetodoGrillaBusqueda: Se supero la cantidad maxima de iteraciones.');

  Bitacora.Add('MetodoGrillaBusqueda: Fin de procedimiento.');

  // Liberar bitacora local
  FreeAndNil(Bitacora1);

  // Liberar arreglos
  SetLength(IncluirParametroBusqueda, 0);
  FreeAndNil(Indices);
  FreeAndNil(IndicesSuperiores);
  FreeAndNil(IndicesMinimo);
  FreeAndNil(Pasos);
  FreeAndNil(ValoresIniciales);
end { TProcedimiento.MetodoGrillaBusqueda };

function TProcedimiento.MetodoMIVQUE0(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
var
  ParametrosValidos: Boolean;
  I, J, IndicePrimerParametro, IndiceSegundoParametro: Integer;
  A1, A2, A3, A4, A, P, XTXI, PY, DG1, DG2, DR1, DR2: TUaMatriz;
  B: TUaVector;
begin { TProcedimiento.MetodoMIVQUE0 }
  Assert(Assigned(ParametrosEfectosAleatorios), 'TProcedimiento.MetodoMIVQUE0: Assigned(ParametrosEfectosAleatorios)');
  Assert(Assigned(Bitacora), 'TProcedimiento.MetodoMIVQUE0: Assigned(Bitacora)');

  ParametrosValidos := True;

  try
    // Inicializar referencias
    A := nil;
    P := nil;
    XTXI := nil;
    DG1 := nil;
    DG2 := nil;
    DR1 := nil;
    DR2 := nil;
    B := nil;
    A1 := nil;
    A2 := nil;
    A3 := nil;
    A4 := nil;
    FWSA := nil;
    FWSD := nil;
    FW2SA := nil;
    FB := nil;
    FBT := nil;

    Bitacora.Add('MetodoMIVQUE0: Calculando (X''X)^(-1).');

    // Calcular (X'X)^(-1)
    A1 := FXT.Producto(FX);
    XTXI := InversaGeneralizadaG2Sweep(A1, FToleranciaSingularidad);
    FreeAndNil(A1);

    // Calcular P = V^(-1) - V^(-1) * X * (X' * V^(-1) * X)^(-1) * X' * V^-1 con V = I

    Bitacora.Add('MetodoMIVQUE0: Calculando P = V^(-1) - V^(-1) * X * (X'' * V^(-1) * X)^(-1) * X'' * V^-1 con V = I.');

    A1 := FX.Producto(XTXI).Multiplicar(FXT);
    P := UaMatrizIdentidad(A1.CantidadFilas, A1.CantidadColumnas).Restar(A1);
    FreeAndNil(A1);

    // Liberar (X'X)^(-1)
    FreeAndNil(XTXI);

    Bitacora.Add('MetodoMIVQUE0: Calculando Py.');
    PY := P.Producto(FY);

    Bitacora.Add('MetodoMIVQUE0: Construyendo el sistema de ecuaciones.');

    // Construir el sistema de ecuaciones
    A := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    B := TUaVector.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);

    IndicePrimerParametro := 0;
    IndiceSegundoParametro := 0;

    // Inicializar matrices;
    DG1 := nil;
    DG2 := nil;
    DR1 := nil;
    DR2 := nil;

    Bitacora.Add('MetodoMIVQUE0: Construyendo el sistema de ecuaciones. Parametros de cada grupo de efectos aleatorios.');

    // Recorrer los parametros de cada grupo de efectos aleatorios
    for I := 0 to FCantidadParametrosEfectosAleatorios - 1 do
    begin
      // Calcular la derivada de la matriz de covarianza sobre el primer parametro
      ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(DG1, I);

      // Calcular P*Z*DG1*Z' y P*Z*DG1*Z' x P*Z*DG1*Z'
      if FCalcularVerosimilitudResidual then
      begin
        A1 := P.Producto(FZ).Multiplicar(DG1).Multiplicar(FZT);
        // A3 := FYT.Producto(A1).Multiplicar(P).Multiplicar(FY);
        A3 := FYT.Producto(A1).Multiplicar(PY);
      end
      else
      begin
        A1 := FZ.Producto(DG1).Multiplicar(FZT);
        // A3 := FYT.Producto(P).Multiplicar(A1).Multiplicar(P).Multiplicar(FY);
        A3 := FYT.Producto(P).Multiplicar(A1).Multiplicar(PY);
      end;
      A2 := A1.Producto(A1);

      // Almacenar los valores en el sistema
      A [IndicePrimerParametro + 1, IndicePrimerParametro + 1] := A2.Traza;
      B [IndicePrimerParametro + 1] := A3 [1, 1];

      // Liberar matrices auxiliares
      FreeAndNil(A2);
      FreeAndNil(A3);

      // Recorrer los parametros de cada grupo de efectos aleatorios
      IndiceSegundoParametro := IndicePrimerParametro + 1;
      for J := I + 1 to FCantidadParametrosEfectosAleatorios - 1 do
      begin
        // Calcular la derivada de la matriz de covarianza sobre el segundo parametro
        ConstruirDerivadaPrimeraMatrizCovarianzaEfectosAleatorios(DG2, J);

        // Calcular P*Z*DG2*Z' y P*Z*DG1*Z' x P*Z*DG2*Z'
        if FCalcularVerosimilitudResidual then
          A2 := P.Producto(FZ).Multiplicar(DG2).Multiplicar(FZT)
        else
          A2 := FZ.Producto(DG2).Multiplicar(FZT);
        A3 := A1.Producto(A2);
        // A4 := A2.Producto(A1);

        // Almacenar los valores en el sistema
        A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1] := A3.Traza;
        A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1];
        // A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A4.Traza;

        // Liberar matrices auxiliares
        FreeAndNil(A2);
        FreeAndNil(A3);
        // FreeAndNil(A4);

        Inc(IndiceSegundoParametro);
      end;

      // Recorrer los parametros del error
      for J := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
      begin
        // Calcular la derviada de la matriz de covarianza sobre el segundo parametro
        ConstruirDerivadaPrimeraMatrizCovarianzaError(DR2, J);

        // Calcular P*DR2 y P*Z*DG1*Z' x P*DR2
        if FCalcularVerosimilitudResidual then
          A2 := P.Producto(DR2)
        else
          A2 := DR2.Copia;
        A3 := A1.Producto(A2);
        // A4 := A2.Producto(A1);

        // Alamcenar los valores en el sistema
        A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1] := A3.Traza;
        A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1];
        // A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A4.Traza;

        // Liberar matrices auxiliares
        FreeAndNil(A2);
        FreeAndNil(A3);
        // FreeAndNil(A4);

        Inc(IndiceSegundoParametro);
      end;

      FreeAndNil(A1);

      Inc(IndicePrimerParametro);
    end;

    Bitacora.Add('MetodoMIVQUE0: Construyendo el sistema de ecuaciones. Parametros del error.');

    // Recorrer los parametros del error
    for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
    begin
      // Calcular la derviada de la matriz de covarianza sobre el segundo parametro
      ConstruirDerivadaPrimeraMatrizCovarianzaError(DR1, I);

      // Calcular P*DR1 y P*DR1 x P*DR1
      if FCalcularVerosimilitudResidual then
      begin
        A1 := P.Producto(DR1);
        // A3 := FYT.Producto(A1).Multiplicar(P).Multiplicar(FY);
        A3 := FYT.Producto(A1).Multiplicar(PY);
      end
      else
      begin
        A1 := DR1.Copia;
        // A3 := FYT.Producto(P).Multiplicar(A1).Multiplicar(P).Multiplicar(FY);
        A3 := FYT.Producto(P).Multiplicar(A1).Multiplicar(PY);
      end;
      A2 := A1.Producto(A1);

      // Almacenar los valores en el sistema
      A [IndicePrimerParametro + 1, IndicePrimerParametro + 1] := A2.Traza;
      B [IndicePrimerParametro + 1] := A3 [1, 1];

      // Liberar matrices auxiliares
      FreeAndNil(A2);
      FreeAndNil(A3);

      // Recorrer los parametros del error
      IndiceSegundoParametro := IndicePrimerParametro + 1;
      for J := I + 1 to FInformacionTiempoEjecucionError.Parametros.Alto do
      begin
        // Calcular la derviada de la matriz de covarianza sobre el segundo parametro
        ConstruirDerivadaPrimeraMatrizCovarianzaError(DR2, J);

        // Calcular P*DR2 y P*DR1 x P*DR2
        if FCalcularVerosimilitudResidual then
          A2 := P.Producto(DR2)
        else
          A2 := DR2.Copia;
        A3 := A1.Producto(A2);
        // A4 := A2.Producto(A1);

        // Alamcenar los valores en el sistema
        A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1] := A3.Traza;
        A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A [IndicePrimerParametro + 1, IndiceSegundoParametro + 1];
        // A [IndiceSegundoParametro + 1, IndicePrimerParametro + 1] := A4.Traza;

        // Liberar matrices auxiliares
        FreeAndNil(A2);
        FreeAndNil(A3);
        // FreeAndNil(A4);

        Inc(IndiceSegundoParametro);
      end;

      FreeAndNil(A1);

      Inc(IndicePrimerParametro);
    end;

    Bitacora.Add('MetodoMIVQUE0: Resolviendo el sistema de ecuaciones.');

    // Resolver el sistema de ecuaciones
    ResolverSistemaEcuacionesLinealesEn(A, B);

    Bitacora.Add('MetodoMIVQUE0: Asignando el resultado del ajuste.');

    // Asignar resultado del ajuste
    FValoresParametrosEfectosAleatorios.Asignar(B);

    Bitacora.Add('MetodoMIVQUE0: Validando resultados.');

    // Validar los resultados

    FreeAndNil(FL);
    FreeAndNil(FLT);
    FreeAndNil(FRaizCuadradaR);
    FreeAndNil(FRI);

    ValidarParametros(Bitacora, FVerificarCotasInferiores, FVerificarCotasSuperiores, 'MetodoMIVQUE0');

    if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
    begin
      ConstruirMatrizCovarianzaEfectosAleatorios(FG);

      if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
        FLT := FL.Transpuesta
      else
      begin
        Bitacora.Add('MetodoMIVQUE0: Los valores actuales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
        ParametrosValidos := False;
      end;
    end;

    ConstruirMatrizCovarianzaError(FR);

    if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
      FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
    else
    begin
      Bitacora.Add('MetodoMIVQUE0: Los valores iniciales de parametros generan una matriz de covarianza de error no positiva definida.');
      ParametrosValidos := False;
    end;

    // Si los parametros resultantes son validos, calcular los terminos de la
    // funcion objetivo.
    if ParametrosValidos then
    begin
      FWSA := nil;
      FWSD := nil;
      FW2SA := nil;
      FB := nil;
      FBT := nil;

      CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);
    end;

    Result := ParametrosValidos;

    Assert(not Assigned(A1), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A1)');
    Assert(not Assigned(A2), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A2)');
    Assert(not Assigned(A3), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A3)');
    Assert(not Assigned(A4), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A4)');

    // Liberar matrices auxliares
    FreeAndNil(A);
    FreeAndNil(B);
    FreeAndNil(P);
    FreeAndNil(PY);
    FreeAndNil(XTXI);
    FreeAndNil(DG1);
    FreeAndNil(DG2);
    FreeAndNil(DR1);
    FreeAndNil(DR2);

    Bitacora.Add('MetodoMIVQUE0: Fin de procedimiento.');
  except on E: Exception do
    begin
      // Se produjo un error durante el procedimiento

      Result := False;
      Bitacora.Add('MetodoMIVQUE0: ' + E.Message);

      // Liberar matrices auxiliares
      FreeAndNil(A);
      FreeAndNil(PY);
      FreeAndNil(P);
      FreeAndNil(XTXI);
      FreeAndNil(DG1);
      FreeAndNil(DG2);
      FreeAndNil(DR1);
      FreeAndNil(DR2);
      FreeAndNil(B);
      FreeAndNil(A1);
      FreeAndNil(A2);
      FreeAndNil(A3);
      FreeAndNil(A4);

      FreeAndNil(FG);
      FreeAndNil(FLT);
      FreeAndNil(FR);
      FreeAndNil(FRI);
      FreeAndNil(FRaizCuadradaR);
      FreeAndNil(FWSA);
      FreeAndNil(FWSD);
      FreeAndNil(FW2SA);
      FreeAndNil(FB);
      FreeAndNil(FBT);
    end;
  end;

  Assert(not Assigned(A), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A)');
  Assert(not Assigned(P), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(P)');
  Assert(not Assigned(XTXI), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(XTXI)');
  Assert(not Assigned(DG1), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(DG1)');
  Assert(not Assigned(DG2), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(DG2)');
  Assert(not Assigned(DR1), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(DR1)');
  Assert(not Assigned(DR2), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(DR2)');
  Assert(not Assigned(B), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(B)');
  Assert(not Assigned(A1), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A1)');
  Assert(not Assigned(A2), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A2)');
  Assert(not Assigned(A3), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A3)');
  Assert(not Assigned(A4), 'TProcedimiento.ProcedimientoMIVQUE0: not Assigned(A4)');
end { TProcedimiento.MetodoMIVQUE0 };

function TProcedimiento.MetodoMaximaVerosimilitudMarquardt(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
var
  LAnterior, LActual, L1Anterior, L2Anterior, L3Anterior, Maximo, ValorConvergencia, FactorRidgingActual, TerminoConstanteVerosimilitud: TUaReal;
  M1, G1, G2, G3, H1, H2, H3, G, H, H0, HI: TUaMatriz;
  D, DiagonalTerminoRidging, ParametrosPasoAnterior: TUaVector;
  Continuar, AceptarCambios, Convergencia, HPositivaDefinida, MejoraVerosimilitud, ParametrosValidos, SumarTerminoRidging, IncrementarFactorRidging, FactorRidgingExcedeLimites: Boolean;
  I, J, K, IndiceMaximo, CantidadPasosScoring: Integer;
begin { TProcedimiento.MetodoMaximaVerosimilitudMarquardt }
  Assert(Assigned(ParametrosEfectosAleatorios), 'TProcedimiento.MetodoMaximaVerosimilitudMarquardt: Assigned(ParametrosEfectosAleatorios)');
  Assert(Assigned(Bitacora), 'TProcedimiento.MetodoMaximaVerosimilitudMarquardt: Assigned(Bitacora)');
  Assert(Assigned(BitacoraIteracion), 'TProcedimiento.MetodoMaximaVerosimilitudMarquardt: Assigned(BitacoraIteracion)');

  // Inicializar referencias
  M1 := nil;
  G1 := nil;
  G2 := nil;
  G3 := nil;
  H1 := nil;
  H2 := nil;
  H3 := nil;
  G := nil;
  H0 := nil;
  H := nil;
  HI := nil;
  DiagonalTerminoRidging := nil;
  D := nil;
  ParametrosPasoAnterior := nil;

  Convergencia := False;
  ParametrosValidos := True;
  FactorRidgingExcedeLimites := False;

  Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Validando valores iniciales de parametros.');

  // Verificar parametros iniciales
  Continuar := True;
  FreeAndNil(FL);
  FreeAndNil(FLT);
  FreeAndNil(FRaizCuadradaR);
  FreeAndNil(FRI);

  ParametrosValidos := True;

  if not ValidarParametros(Bitacora, FVerificarCotasInferiores, FVerificarCotasSuperiores, 'MetodoMaximaVerosimilitudMarquardt') then
  begin
    Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Los valores iniciales de parametros no respetan las cotas o generan estructuras de covarianza invalidas: ');
    ParametrosValidos := False;
  end;

  if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
  begin
    ConstruirMatrizCovarianzaEfectosAleatorios(FG);

    if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
      FLT := FL.Transpuesta
    else
    begin
      Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Los valores iniciales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
      ParametrosValidos := False;
    end;
  end;

  ConstruirMatrizCovarianzaError(FR);

  if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
    FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
  else
  begin
    Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Los valores iniciales de parametros generan una matriz de covarianza de error no positiva definida.');
    ParametrosValidos := False;
  end;

  if ParametrosValidos then
  begin
    // Inicializar matrices y vectores auxiliares
    ParametrosPasoAnterior := TUaVector.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    G1 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, 1);
    G2 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, 1);
    G3 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, 1);
    G := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, 1);
    H1 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    H2 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    H3 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    H := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    H0 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
    DiagonalTerminoRidging := TUaVector.Create(H0.CantidadColumnas);

    if FCalcularVerosimilitudResidual then
      TerminoConstanteVerosimilitud := (FCantidadObservaciones - FRangoX) * Ln(2.0 * CUaPi)
    else
      TerminoConstanteVerosimilitud := FCantidadObservaciones * Ln(2.0 * CUaPi);

    // Iniciar la iteracion

    Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Iniciando iteracion.');

    BitacoraIteracion.Add(Format('%10s %30s %30s', ['Paso', 'Valor Funcion Objetivo', 'Valor Convergencia']));
    BitacoraIteracion.Add('--------------------------------------------------------------------------------');

    // Calcular valores iniciales de la funcion objetivo (y subproductos)
    FreeAndNil(FWSA);
    FreeAndNil(FWSD);
    FreeAndNil(FW2SA);
    FreeAndNil(FB);
    FreeAndNil(FBT);
    CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);

    if FCalcularVerosimilitudResidual then
      LActual := FL1 + FL2 + FL3
    else
      LActual := FL1 + FL2;

    I := 1;
    FCantidadIteraciones := 1;
    FCantidadIteracionesBucleInterno := 0;
    FCantidadEvaluacionesFuncionObjetivo := 1;
    CantidadPasosScoring := 0;
    Convergencia := False;
    Continuar := True;
    FactorRidgingExcedeLimites := False;
    FactorRidgingActual := FValorInicialFactorRidging;
    while (not Convergencia) and Continuar do
    begin
      // Almacenar valores del paso de iteracion anterior
      LAnterior := LActual;
      L1Anterior := FL1;
      L2Anterior := FL2;
      L3Anterior := FL3;

      ParametrosPasoAnterior.Asignar(FValoresParametrosEfectosAleatorios);

      // Construir gradiente y hessiano
      if FCalcularVerosimilitudResidual then
      begin
        CalcularTerminosGradienteYHessiano(FWSA, FWSD, FW2SA, FB, FBT, True, True, True, True, CantidadPasosScoring < FCantidadPasosScoring, True, G1, G2, G3, H1, H2, H3);

        G.Asignar(G1).Sumar(G2).Sumar(G3);

        if CantidadPasosScoring < FCantidadPasosScoring then
          H.Asignar(H3).Restar(H1)
        else
          H.Asignar(H1).Sumar(H2).Sumar(H3);
      end
      else
      begin
        CalcularTerminosGradienteYHessiano(FWSA, FWSD, FW2SA, FB, FBT, True, True, False, True, CantidadPasosScoring < FCantidadPasosScoring, False, G1, G2, G3, H1, H2, H3);

        G.Asignar(G1).Sumar(G2);

        if CantidadPasosScoring < FCantidadPasosScoring then
          H.AsignarCero.Restar(H1)
        else
          H.Asignar(H1).Sumar(H2);
      end;
      H0.Asignar(H);

      // Verificar si la matriz hessiano es positiva definida
      HPositivaDefinida := EsPositivaDefinida(H, FToleranciaCholesky);

      if HPositivaDefinida then
      begin
        FreeAndNil(HI);
        HI := InversaGeneralizadaG2Sweep(H, FToleranciaSingularidad);
      end;

      // Verificar convergencia
      case FCriterioConvergencia of
        ConvergenciaParametrosAbsoluta,
        ConvergenciaParametrosRelativa:
        begin
          ValorConvergencia := Abs(FValoresParametrosEfectosAleatorios [1] - ParametrosPasoAnterior [1]);
          IndiceMaximo := 1;
          for J := 2 to FValoresParametrosEfectosAleatorios.Dimension do
          begin
            if Abs(FValoresParametrosEfectosAleatorios [J] - ParametrosPasoAnterior [J]) > ValorConvergencia then
            begin
              ValorConvergencia := Abs(FValoresParametrosEfectosAleatorios [J] - ParametrosPasoAnterior [J]);
              IndiceMaximo := J;
            end;
          end;

          if FCriterioConvergencia = ConvergenciaParametrosRelativa then
            ValorConvergencia := ValorConvergencia / Abs(ParametrosPasoAnterior [IndiceMaximo]);
        end;
        ConvergenciaVerosimilitudAbsoluta,
        ConvergenciaVerosimilitudRelativa:
        begin
          ValorConvergencia := Abs(LActual - LAnterior);
          if FCriterioConvergencia = ConvergenciaVerosimilitudRelativa then
            ValorConvergencia := ValorConvergencia / Abs(LActual);
        end;
        ConvergenciaGradienteAbsoluta,
        ConvergenciaGradienteRelativa:
        begin
          ValorConvergencia := Abs(G [1, 1]);
          IndiceMaximo := 1;
          for J := 2 to G.CantidadFilas do
          begin
            if Abs(G [J, 1]) > ValorConvergencia then
              ValorConvergencia := G [J, 1];
          end;

          if FCriterioConvergencia = ConvergenciaGradienteRelativa then
            ValorConvergencia := ValorConvergencia / Abs(LActual);
        end;
        ConvergenciaHessianoAbsoluta,
        ConvergenciaHessianoRelativa:
        begin
          if HPositivaDefinida then
            M1 := G.Transpuesta.Multiplicar(HI).Multiplicar(G)
          else
            M1 := G.Transpuesta.Multiplicar(G);
          ValorConvergencia := M1 [1, 1];
          FreeAndNil(M1);

          if FCriterioConvergencia = ConvergenciaHessianoRelativa then
            ValorConvergencia := ValorConvergencia / Abs(LActual);
        end;
      end;

      BitacoraIteracion.Add(Format('%10d %30g %30g', [I, LActual + TerminoConstanteVerosimilitud, ValorConvergencia]));

      // Verificar convergencia. El algoritmo no se detiene durante los pasos
      // de Scoring.
      if ValorConvergencia < FToleranciaConvergencia then
      begin
        if CantidadPasosScoring < FCantidadPasosScoring then
        begin
          CantidadPasosScoring := FCantidadPasosScoring;
          Convergencia := False
        end
        else
        begin
          Convergencia := True;
          Continue;
        end;
      end;

      // Iterar hasta que se obtenga una matriz hessiano positiva definida, un
      // paso de Newton que construya parametros validos y que mejore (no
      // empeore) la verosimilitud.

      J := 0;
      AceptarCambios := False;
      SumarTerminoRidging := False;
      IncrementarFactorRidging := False;
      while (not AceptarCambios) and Continuar do
      begin
        Inc(FCantidadIteracionesBucleInterno);

        Inc(J);

        // Actualizar valor del factor de ridging.
        if IncrementarFactorRidging then
          FactorRidgingActual := FactorRidgingActual * FFactorIncrementoFactorRidging;

        // Verificar que el valor del factor de ridging actual no exceda los
        // limites establecidos.
        if (FactorRidgingActual > FValorMaximoFactorRidging) then
        begin
          FactorRidgingExcedeLimites := True;
          Continuar := False;
          Continue;
        end;

        // Verificar si es necesario sumar el termino de riding.
        if SumarTerminoRidging then
        begin
          H0.DiagonalEn(DiagonalTerminoRidging);
          DiagonalTerminoRidging.Sumar(FactorRidgingActual);
          H.Asignar(H0).Diagonal := DiagonalTerminoRidging;
        end;

        // Verificar si la (posiblemente nueva) matriz hessiano es positiva
        // definida.
        HPositivaDefinida := EsPositivaDefinida(H, FToleranciaCholesky);

        // Si la matriz no es positiva definida, incrementar el factor de
        // ridging, indicar que se desea incluir el termino de ridging en el
        // calculo de la matriz hessiano y continuar el algoritmo.
        if not HPositivaDefinida then
        begin
          SumarTerminoRidging := True;
          IncrementarFactorRidging := True;
          FValoresParametrosEfectosAleatorios.Asignar(ParametrosPasoAnterior);
          Continue;
        end;

        // Calcular la inversa de la matriz hessiano y construir el paso de
        // Newton.
        FreeAndNil(HI);
        HI := InversaGeneralizadaG2Sweep(H, FToleranciaSingularidad);

        FreeAndNil(D);
        M1 := HI.Producto(G).Multiplicar(-1.0);
        D := M1.Columna [1];
        FreeAndNil(M1);

        // Actualizar el vector de valores de parametros
        FValoresParametrosEfectosAleatorios.Sumar(D);

        // Validar el nuevo conjunto de valores de parametros de covarianza
        ParametrosValidos := True;

        FreeAndNil(FL);
        FreeAndNil(FLT);
        FreeAndNil(FRaizCuadradaR);
        FreeAndNil(FRI);

        ValidarParametros(Bitacora, FVerificarCotasInferiores, FVerificarCotasSuperiores, 'MetodoMaximaVerosimilitudMarquardt');

        if FModelo.TieneEfectosAleatorios or FModelo.TieneEfectosAleatoriosGrupos then
        begin
          ConstruirMatrizCovarianzaEfectosAleatorios(FG);

          if DescomposicionCholesky(FG, FL, FToleranciaCholesky) then
            FLT := FL.Transpuesta
          else
          begin
            Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Los valores actuales de parametros generan una matriz de covarianza de efectos aleatorios no positiva definida.');
            ParametrosValidos := False;
          end;
        end;

        ConstruirMatrizCovarianzaError(FR);

        if DescomposicionCholesky(FR, FRaizCuadradaR, FToleranciaCholesky) then
          FRI := InversaGeneralizadaG2Sweep(FR, FToleranciaSingularidad)
        else
        begin
          Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Los valores actuales de parametros generan una matriz de covarianza de error no positiva definida.');
          ParametrosValidos := False;
        end;

        // Si los parametros no son validos, incrementar el factor de ridging,
        // indicar que se desea incluir el termino de ridging en el calculo de
        // la matriz hessiano, revertir cambios al conjunto parametros de
        // covarianza y continuar el algoritmo.
        if not ParametrosValidos then
        begin
          SumarTerminoRidging := True;
          IncrementarFactorRidging := True;
          FValoresParametrosEfectosAleatorios.Asignar(ParametrosPasoAnterior);
          Continue;
        end;

        // Calcular el nuevo valor de la funcion objetivo correspondiente y sus
        // subproductos.
        FreeAndNil(FWSA);
        FreeAndNil(FWSD);
        FreeAndNil(FW2SA);
        FreeAndNil(FB);
        FreeAndNil(FBT);
        CalcularTerminosValorFuncionObjetivoYSubproductos(FL1, FL2, FL3, FWSA, FWSD, FW2SA, FB, FBT);
        Inc(FCantidadEvaluacionesFuncionObjetivo);

        if FCalcularVerosimilitudResidual then
          LActual := FL1 + FL2 + FL3
        else
          LActual := FL1 + FL2;

        // Verificar si mejora la verosimilitud.
        MejoraVerosimilitud := ((LAnterior - LActual) > 0) or (Abs(LAnterior - LActual) < FToleranciaSingularidad);

        // Aceptar los cambios si mejoran la verosimilitud, de otro modo,
        // descartar los cambios, incrementar el factor de ridging y continuar
        // con el bucle interno del algoritmo.
        if not MejoraVerosimilitud then
        begin
          IncrementarFactorRidging := True;
          SumarTerminoRidging := True;
          FValoresParametrosEfectosAleatorios.Asignar(ParametrosPasoAnterior);
          Continue;
        end;

        AceptarCambios := True;
      end;

      FactorRidgingActual := FactorRidgingActual / FFactorDecrementoFactorRidging;

      if FactorRidgingActual < FValorMinimoFactorRidging then
        FactorRidgingActual := FValorMinimoFactorRidging;

      Inc(I);
      Inc(FCantidadIteraciones);
      Inc(CantidadPasosScoring);

      // Verificar si es posible continuar
      if (FCantidadEvaluacionesFuncionObjetivo > FCantidadMaximaEvaluacionesFuncionObjetivo) or
         (FCantidadIteraciones > FCantidadMaximaIteraciones) then
        Continuar := False;
    end;
  end;

  Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Fin de iteracion.');

  if not HPositivaDefinida then
    Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Se genero una matriz hessiano no positiva definida.');

  // Verificar el resultado del algoritmo
  if not Convergencia then
  begin
    if FCantidadEvaluacionesFuncionObjetivo > FCantidadMaximaEvaluacionesFuncionObjetivo then
      Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Se supero la cantidad maxima de evaluaciones de la funcion objetivo.');

    if FCantidadIteraciones > FCantidadMaximaIteraciones then
      Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Se supero la cantidad maxima de iteraciones.');

    if not ParametrosValidos then
      Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Se genero un conjunto de valores de parametros invalidos (violan las cotas o generan una verosimilitud infinita).');

    if FactorRidgingExcedeLimites then
      Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: El factor de ridging excedio los limites establecidos.');
  end
  else
  begin
    if ParametrosEfectosAleatorios <> FValoresParametrosEfectosAleatorios then
      ParametrosEfectosAleatorios.Asignar(FValoresParametrosEfectosAleatorios);
  end;

  Bitacora.Add('MetodoMaximaVerosimilitudMarquardt: Fin de procedimiento.');

  // Liberar matrices y vectores auxiliares
  FreeAndNil(M1);
  FreeAndNil(G1);
  FreeAndNil(G2);
  FreeAndNil(G3);
  FreeAndNil(H1);
  FreeAndNil(H2);
  FreeAndNil(H3);
  FreeAndNil(G);
  FreeAndNil(H);
  FreeAndNil(H0);
  FreeAndNil(HI);
  FreeAndNil(D);
  FreeAndNil(DiagonalTerminoRidging);
  FreeAndNil(ParametrosPasoAnterior);

  Result := Convergencia;
end { TProcedimiento.MetodoMaximaVerosimilitudMarquardt };

function TProcedimiento.PostProcesar(var Bitacora: TStrings): Boolean;
var
  Listo, DefinicionConflictiva, TieneUnidadExperimental: Boolean;
  I, J, K: Integer;
  D, N, NA, P, Q: Integer;
  L, TerminoConstanteVerosimilitud: TUaReal;
  C11, C21, C22, M1, M2, H1, H2, H3: TUaMatriz;
  InformacionCriterio: TInformacionTiempoEjecucionCriterio;
begin { TProcedimiento.PostProcesar }
  Assert(Assigned(Bitacora), 'TProcedimiento.PostProcesar: Assigned(Bitacora)');
  Assert(not Assigned(FInformacionPostProcesamientoCriteriosAjuste), 'TProcedimiento.PostProcesar: not Assigned(FInformacionPostProcesamientoCriteriosAjuste)');
  Assert(not Assigned(FInformacionPostProcesamientoEstructuraMedia), 'TProcedimiento.PostProcesar: not Assigned(FInformacionPostProcesamientoEstructuraMedia)');
  Assert(not Assigned(FInformacionPostProcesamientoEstructuraCovarianza), 'TProcedimiento.PostProcesar: not Assigned(FInformacionPostProcesamientoEstructuraCovarianza)');

  Bitacora.Add('PostProcesar: Inicializando postprocesamiento...');

  // Verificar que exista un unico criterio de unidad experimental para
  // calcular el valor de n.
  I := 0;
  DefinicionConflictiva := False;
  TieneUnidadExperimental := False;
  InformacionCriterio := nil;
  while (I <= FInformacionTiempoEjecucionEfectosAleatorios.Count - 1) and
        (not DefinicionConflictiva) do
  begin
    if FInformacionTiempoEjecucionEfectosAleatorios [I].TieneUnidadExperimental then
    begin
      TieneUnidadExperimental := True;
      if Assigned(InformacionCriterio) then
      begin
        if not InformacionCriterio.Efecto.IgualA(FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionUnidadExperimental.Efecto) then
          DefinicionConflictiva := True;
      end
      else
        InformacionCriterio := FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionUnidadExperimental;
    end;

    Inc(I);
  end;

  if FInformacionTiempoEjecucionError.TieneUnidadExperimental and
     (not DefinicionConflictiva) then
  begin
    if TieneUnidadExperimental then
    begin
      if not InformacionCriterio.Efecto.IgualA(FInformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental.Efecto) then
        DefinicionConflictiva := True;
    end
    else
    begin
      TieneUnidadExperimental := True;
      InformacionCriterio := FInformacionTiempoEjecucionError.InformacionTiempoEjecucionUnidadExperimental;
    end;
  end;

  // Si el modelo posee una definicion correcta de unidad experimental, entonces
  // N es igual a la cantidad de unidades experimentales, de otro modo N es
  // igual a la cantidad de observaciones del modelo. Si tiene una definicion
  // conflictiva N toma el valor de la cantidad de niveles del primer efecto
  // aleatorio definido. En caso que este no existiera N toma el valor de la
  // cantidad de observaciones.
  if DefinicionConflictiva then
  begin
    if FModelo.TieneEfectosAleatorios or
       FModelo.TieneEfectosAleatoriosGrupos then
    begin
      // Buscar el primer efecto aleatorio con variables de clasificacion
      I := 0;
      Listo := False;
      while (I < FInformacionTiempoEjecucionEfectosAleatorios.Count) and (not Listo) do
      begin
        J := 0;
        while (J < FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionEfectos.Dimension) and (not Listo) do
        begin
          if (not (FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionEfectos [J].Efecto is TIntercepto)) and
             (FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionEfectos [J].TieneVariablesClasificacion) then
          begin
            N := FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionEfectos [J].CantidadNiveles - FInformacionTiempoEjecucionEfectosAleatorios [I].InformacionTiempoEjecucionEfectos [J].CantidadNivelesInexistentes;
            Listo := True;
          end;

          Inc(J);
        end;

        Inc(I);
      end;
    end
    else
      N := FCantidadObservaciones;
  end
  else if TieneUnidadExperimental then
    N := InformacionCriterio.CantidadNiveles - InformacionCriterio.CantidadNivelesInexistentes
  else
    N := FCantidadObservaciones;

  P := FRangoX;
  Q := FCantidadParametrosCovarianza;

  // Si se utilizo la verosimilitud residual entonces es necesario restar P
  // (el rango de X) a N y D es igual a Q (la cantidad de parametros de
  // covarianza), en caso contrarl D es igual a Q mas P.
  if FCalcularVerosimilitudResidual then
  begin
    if N - P < 0 then
      N := FCantidadObservaciones;
    N := N - P;
    D := Q;
  end
  else
    D := Q + P;

  // Si la cantidad de observaciones es menor que D + 2 entonces N* es igual a
  // D + 2, en caso contrario NA es igual a la cantidad de observaciones.
  if (FCantidadObservaciones < D + 2) then
    NA := D + 2
  else
    NA := FCantidadObservaciones;

  Bitacora.Add('PostProcesar: Generando informacion sobre criterios de ajuste...');

  // Generar informacion sobre criterios de ajuste

  FInformacionPostProcesamientoCriteriosAjuste := TInformacionPostProcesamientoCriteriosAjuste.Create;

  if FCalcularVerosimilitudResidual then
    TerminoConstanteVerosimilitud := (FCantidadObservaciones - P) * Ln(2.0 * CUaPi)
  else
    TerminoConstanteVerosimilitud := FCantidadObservaciones * Ln(2.0 * CUaPi);

  FInformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmica := FL1 + FL2 + TerminoConstanteVerosimilitud;
  FInformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmicaResidual := FL1 + FL2 + FL3 + TerminoConstanteVerosimilitud;

  FInformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica := -0.5 * FInformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmica;
  FInformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual := -0.5 * FInformacionPostProcesamientoCriteriosAjuste.MenosDosVerosimilitudLogaritmicaResidual;

  if FCalcularVerosimilitudResidual then
    L := FInformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmicaResidual
  else
    L := FInformacionPostProcesamientoCriteriosAjuste.VerosimilitudLogaritmica;

  FInformacionPostProcesamientoCriteriosAjuste.AIC := -2.0 * L + 2.0 * D;
  FInformacionPostProcesamientoCriteriosAjuste.AICC := -2.0 * L + (2.0 * D * NA) / (NA - D - 1);

  if N > 1 then
  begin
    FInformacionPostProcesamientoCriteriosAjuste.HQIC := -2.0 * L + 2.0 * D * Ln(Ln(N));
    FInformacionPostProcesamientoCriteriosAjuste.BIC := -2.0 * L + D * Ln(N);
    FInformacionPostProcesamientoCriteriosAjuste.CAIC := -2.0 * L + D * (Ln(N) + 1);
  end
  else
  begin
    FInformacionPostProcesamientoCriteriosAjuste.HQIC := 0.0;
    FInformacionPostProcesamientoCriteriosAjuste.BIC := 0.0;
    FInformacionPostProcesamientoCriteriosAjuste.CAIC := 0.0;
  end;

  // Generar informacion sobre el modelo

  Bitacora.Add('PostProcesar: Generando informacion sobre el modelo...');

  FInformacionPostProcesamientoModelo := TInformacionPostProcesamientoModelo.Create;

  // Si el modelo tiene efectos aleatorios es necesario construir las matrices,
  // de otro modo es posible reutilizar las generadas durante el proceso de
  // ajuste.
  if FModelo.TieneEfectosAleatorios or
     FModelo.TieneEfectosAleatoriosGrupos then
  begin
    FInformacionPostProcesamientoModelo.V := FZ.Producto(FG).Multiplicar(FZT).Sumar(FR);
    FInformacionPostProcesamientoModelo.VI := InversaGeneralizadaG2Sweep(FInformacionPostProcesamientoModelo.V, FToleranciaSingularidad);

    C11 := FW2SA;
    C21 := FG.Producto(FZT).Multiplicar(FInformacionPostProcesamientoModelo.VI).Multiplicar(FX).Multiplicar(C11).Multiplicar(-1.0);

    C22 := FWSA.PreProducto(FL).Multiplicar(FLT);
    M1 := C21.Producto(FXT).Multiplicar(FInformacionPostProcesamientoModelo.VI).Multiplicar(FZ).Multiplicar(FG);
    C22.Restar(M1);
    FreeAndNil(M1);

    FInformacionPostProcesamientoModelo.C := TUaMatriz.Create(C11.CantidadFilas + C21.CantidadFilas, C11.CantidadColumnas + C21.CantidadFilas);
    FInformacionPostProcesamientoModelo.C.AsignarSubMatriz(1, 1, C11);
    FInformacionPostProcesamientoModelo.C.AsignarSubMatriz(C11.CantidadFilas + 1, 1, C21);
    C21.Transponer;
    FInformacionPostProcesamientoModelo.C.AsignarSubMatriz(1, C11.CantidadColumnas + 1, C21);
    FInformacionPostProcesamientoModelo.C.AsignarSubMatriz(C11.CantidadFilas + 1, C11.CantidadColumnas + 1, C22);

    FreeAndNil(C21);
    FreeAndNil(C22);
  end
  else
  begin
    FInformacionPostProcesamientoModelo.V := FR;
    FInformacionPostProcesamientoModelo.VI := FRI;
    FInformacionPostProcesamientoModelo.C := FW2SA;
  end;

  // Generar informacion sobre estructuras de media

  Bitacora.Add('PostProcesar: Generando informacion sobre estructuras de media...');

  FInformacionPostProcesamientoEstructuraMedia := TInformacionPostProcesamientoEstructuraMedia.Create;

  // Asignar beta.
  FInformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos := FB.Columna [1];

  // Calcular el error estandar de los efectos fijos.
  FInformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos := TUaVector.Create(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno);

  for I := 1 to FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno do
    FInformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [I] := Sqrt(FW2SA [I, I]);

  // Calcular el t-value de los efectos fijos
  FInformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos := TUaVector.Create(FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno);

  for I := 1 to FInformacionTiempoEjecucionEfectosFijos.CantidadColumnasMatrizDiseno do
  begin
    if (FInformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [I] <> 0.0) then
      FInformacionPostProcesamientoEstructuraMedia.ValorTObservadoEstimadoresParametrosEfectosFijos [I] := FInformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos [I] / FInformacionPostProcesamientoEstructuraMedia.ErrorEstandarEstimadoresParametrosEfectosFijos [I];
  end;

  // Generar informacion sobre estructura de covarianza

  Bitacora.Add('PostProcesar: Generando informacion sobre estructura de covarianza...');

  FInformacionPostProcesamientoEstructuraCovarianza := TInformacionPostProcesamientoEstructuraCovarianza.Create;

  // Construir vector de estimadores
  FInformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza := FValoresParametrosEfectosAleatorios.Copia;

  // Calcular el hessiano
  M1 := nil;
  M2 := nil;
  H1 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
  H2 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);
  H3 := TUaMatriz.Create(FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension, FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.Parametros.Dimension);

  if FCalcularVerosimilitudResidual then
  begin
    CalcularTerminosGradienteYHessiano(FWSA, FWSD, FW2SA, FB, FBT, False, False, False, True, True, True, M1, M1, M1, H1, H2, H3);
    M2 := H1.Suma(H2).Sumar(H3);
  end
  else
  begin
    CalcularTerminosGradienteYHessiano(FWSA, FWSD, FW2SA, FB, FBT, False, False, False, True, True, False, M1, M1, M1, H1, H2, H3);
    M2 := H1.Suma(H2);
  end;

  FreeAndNil(H1);
  FreeAndNil(H2);
  FreeAndNil(H3);

  // Verificar que el hessiano sea positivo definido
  if EsPositivaDefinida(M2, FToleranciaCholesky) then
  begin
    // El hessiano es positivo definido, construir la matriz de informacion y
    // los vectores de errores estandar y valores de Z observados.
    FInformacionPostProcesamientoEstructuraCovarianza.HessianoPositivoDefinido := True;

    // Construir matriz de informacion
    FInformacionPostProcesamientoEstructuraCovarianza.MatrizInformacionFisherObservada := InversaGeneralizadaG2Sweep(M2, FToleranciaSingularidad);
    FInformacionPostProcesamientoEstructuraCovarianza.MatrizInformacionFisherObservada.Multiplicar(2.0);

    // Construir vector de errores estandar de estimadores de parametros de
    // covarianza.
    FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza := TUaVector.Create(FInformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza.Dimension);
    for I := 1 to FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza.Dimension do
      FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [I] := Sqrt(FInformacionPostProcesamientoEstructuraCovarianza.MatrizInformacionFisherObservada [I, I]);

    // Construir vector de valores Z observados de estimadores de parametros de
    // covarianza.
    FInformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza := TUaVector.Create(FInformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza.Dimension);
    for I := 1 to FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza.Dimension do
    begin
      if FInformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [I] <> 0.0 then
        FInformacionPostProcesamientoEstructuraCovarianza.ValorZObservadoEstimadoresParametrosCovarianza [I] := FInformacionPostProcesamientoEstructuraCovarianza.EstimadoresParametrosCovarianza [I] / FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarEstimadoresParametrosCovarianza [I];
    end;
  end
  else
  begin
    // El hessiano no es positivo definido
    FInformacionPostProcesamientoEstructuraCovarianza.HessianoPositivoDefinido := False;
    FreeAndNil(M2);
  end;

  // Construir informacion sobre los parametros de efectos aleatorios (de ser
  // necesario).

  if FModelo.TieneEfectosAleatorios or
     FModelo.TieneEfectosAleatoriosGrupos then
  begin
    // Calcular (y - XB)
    M1 := FX.Producto(FB);
    M2 := FY.Resta(M1);
    FreeAndNil(M1);

    // Calcular Gamma = GZ'V^(-1)(y - XB)
    M1 := FG.Producto(FZT).Multiplicar(FInformacionPostProcesamientoModelo.VI).Multiplicar(M2);

    FInformacionPostProcesamientoEstructuraCovarianza.PredictoresEfectosAleatorios := M1.Columna [1];
    FreeAndNil(M1);

    // Calcular el error estandar de los predictores de efectos aleatorios
    FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios := TUaVector.Create(FZ.CantidadColumnas);
    for I := 1 to FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios.Dimension do
      FInformacionPostProcesamientoEstructuraCovarianza.ErrorEstandarPredictoresEfectosAleatorios [I] := Sqrt(FInformacionPostProcesamientoModelo.C [FInformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos.Dimension + I, FInformacionPostProcesamientoEstructuraMedia.EstimadoresParametrosEfectosFijos.Dimension + I]);
  end;

  Bitacora.Add('PostProcesar: Fin de postprocesamiento de resultados.');

  Result := True;
end { TProcedimiento.PostProcesar };

function TProcedimiento.ValidarParametros(var Bitacora: TStrings; const VerificarCotasInferiores, VerificarCotasSuperiores: Boolean; const Llamador: String): Boolean;
var
  Listo: Boolean;
  I: Integer;
begin { TProcedimiento.ValidarParametros }
  Assert(Assigned(Bitacora), 'TProcedimiento.ValidarParametros: Assigned(Bitacora)');

  // Validar parametros efectos aleatorios
  I := 0;
  Listo := False;
  while (I <= FInformacionTiempoEjecucionEfectosAleatorios.Count - 1) and (not Listo) do
  begin
    Listo := Listo or not FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Validar(Bitacora, VerificarCotasInferiores, VerificarCotasSuperiores, Llamador);
    Inc(I);
  end;
  Result := not Listo;

  // Validar parametros error
  Result := Result and FInformacionTiempoEjecucionError.Parametros.Validar(Bitacora, VerificarCotasInferiores, VerificarCotasSuperiores, Llamador);
end { TProcedimiento.ValidarParametros };

constructor TProcedimiento.Create(const AConfiguracion: TConfiguracionProcedimiento);
begin { TProcedimiento.Create }
  FConfiguracion := AConfiguracion;
  FModelo := FConfiguracion.Modelo;
  FOpciones := FConfiguracion.Opciones;

  FVariablesClasificacionAuxiliares := TStringList.Create;
  FInformacionTiempoEjecucionEfectosAleatorios := TListaInformacionTiempoEjecucionGrupoEfectosAleatorios.Create;
  FInformacionTiempoEjecucionCriterios := TListaInformacionTiempoEjecucionEfectos.Create;
end { TProcedimiento.Create };

destructor TProcedimiento.Destroy;
begin { TProcedimiento.Destroy }
  FreeAndNil(FVariablesClasificacionAuxiliares);
  FreeAndNil(FMatrizNivelesVariablesClasificacion);
  FreeAndNil(FInformacionTiempoEjecucionEfectosFijos);
  FreeAndNil(FInformacionTiempoEjecucionEfectosAleatorios);
  FreeAndNil(FInformacionTiempoEjecucionError);
  FreeAndNil(FInformacionTiempoEjecucionCriterios);
  FreeAndNil(FValoresParametrosEfectosAleatorios);

  if FModelo.TieneEfectosAleatorios or
     FModelo.TieneEfectosAleatoriosGrupos then
  begin
    if Assigned(FInformacionPostProcesamientoModelo) then
    begin
      FreeAndNil(FInformacionPostProcesamientoModelo.V);
      FreeAndNil(FInformacionPostProcesamientoModelo.VI);
      FreeAndNil(FInformacionPostProcesamientoModelo.C);
    end;
  end;

  FreeAndNil(FInformacionPostProcesamientoModelo);
  FreeAndNil(FInformacionPostProcesamientoCriteriosAjuste);
  FreeAndNil(FInformacionPostProcesamientoEstructuraMedia);
  FreeAndNil(FInformacionPostProcesamientoEstructuraCovarianza);

  FreeAndNil(FY);
  FreeAndNil(FYT);
  FreeAndNil(FW);
  FreeAndNil(FG);
  FreeAndNil(FL);
  FreeAndNil(FLT);
  FreeAndNil(FRaizCuadradaR);
  FreeAndNil(FR);
  FreeAndNil(FRI);
  FreeAndNil(FX);
  FreeAndNil(FXT);
  FreeAndNil(FZ);
  FreeAndNil(FZT);
  FreeAndNil(FWSA);
  FreeAndNil(FWSD);
  FreeAndNil(FW2SA);
  FreeAndNil(FB);
  FreeAndNil(FBT);
end { TProcedimiento.Destroy };

function TProcedimiento.Ejecutar(var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
begin { TProcedimiento.Ejecutar }
  Assert(Assigned(Bitacora), 'TProcedimiento.Ejecutar: Assigned(Bitacora)');

  Result := FMetodoAjuste(FValoresParametrosEfectosAleatorios, Bitacora, BitacoraIteracion);
end { TProcedimiento.Ejecutar };

function TProcedimiento.Inicializar(var Bitacora: TStrings): Boolean;
var
  B, CalcularVerosimilitudResidualAjuste: Boolean;
  I, IndiceParametro, J, K, L: Integer;
  R: TUaReal;
  Bitacora1, Bitacora2: TStrings;
  Niveles: TArregloStrings;
begin { TProcedimiento.Inicializar }
  Assert(Assigned(Bitacora), 'TProcedimiento.Inicializar: Assigned(Bitacora)');

  Result := True;

  try
    // Inicializar referencias
    Niveles := nil;

    // Construir matriz de indices de niveles a partir de la lista de niveles por variable
    FreeAndNil(FMatrizNivelesVariablesClasificacion);
    FMatrizNivelesVariablesClasificacion := TArregloArreglosStrings.Create(FModelo.VariablesClasificacion.Count);

    for I := 0 to FModelo.VariablesClasificacion.Count - 1 do
    begin
      Niveles := TArregloStrings.Create;

      J := FModelo.Datos.Columnas.IndexOf(FModelo.VariablesClasificacion [I]);
      for K := FModelo.Datos.Valores.Bajo to FModelo.Datos.Valores.Alto do
      begin
        if Niveles.IndiceDe(FModelo.Datos.Valores [K, J]) = -1 then
        begin
          Niveles.Dimension := Niveles.Dimension + 1;
          Niveles [Niveles.Alto] := FModelo.Datos.Valores [K, J];
        end;
      end;

      FMatrizNivelesVariablesClasificacion.Arreglos [I] := Niveles;
    end;

    // Inicializar opciones de modelo

    // Verificar la posibilidad de construccion de bloques
    FConstruirBloques := true;

    // Verificar la posibilidad de construccion de bloques en grupos efectos aleatorios
    I := 0;
    B := false;
    while (I <= FModelo.GruposEfectosAleatorios.Count - 2) and (not B) do
    begin
      // Verificar si el grupo define unidad experimental
      K := FModelo.GruposEfectosAleatorios [I].Opciones.IndexOf('unidad_experimental');

      // Verificar si otro grupo define unidad experimental y comparar
      J := I + 1;
      while (J <= FModelo.GruposEfectosAleatorios.Count - 1) and (not B) do
      begin
        L := FModelo.GruposEfectosAleatorios [J].Opciones.IndexOf('unidad_experimental');

        if K = -1 then
        begin
          // El grupo anterior no define unidad experimental, verificar grupo actual
          if L <> -1 then
          begin
            // El grupo actual define unidad experimental, no es posible construir bloques
            FConstruirBloques := false;
            B := true;
          end;
        end
        else
        begin
          // El grupo anterior define unidad experimental, verificar grupo actual
          if L = -1 then
          begin
            // El grupo actual no define unidad experimental, no es posible construir bloques
            FConstruirBloques := false;
            B := true;
          end
          else
          begin
            // El grupo actual define unidad experimental, comparar criterios
            if not ((FModelo.GruposEfectosAleatorios [I].Opciones [K] as TOpcionParametroTEfecto).Parametro.IgualA((FModelo.GruposEfectosAleatorios [J].Opciones [L] as TOpcionParametroTEfecto).Parametro)) then
            begin
              // Los criterios no coinciden, no es posible construir bloques
              FConstruirBloques := false;
              B := true;
            end;
          end;
        end;

        Inc(J);
      end;

      Inc(I);
    end;

    // Verificar la posibilidad de construccion de bloques en grupos efectos aleatorios contra efectos aleatorios no agrupados y definicion de error

    // Verificar si se define unidad experimental en efectos aleatorios no agrupados
    J := FModelo.EfectosAleatorios.Opciones.IndexOf('unidad_experimental');

    // Verificar si se define unidad experimental en error
    K := FModelo.Error.IndexOf('unidad_experimental');

    I := 0;
    while (I <= FModelo.GruposEfectosAleatorios.Count - 1) and (not B) do
    begin
      // Verificar si el grupo define unidad experimental
      L := FModelo.GruposEfectosAleatorios [I].Opciones.IndexOf('unidad_experimental');

      if L = -1 then
      begin
        // El grupo actual no define unidad experimental

        if (J <> 1) or (K <> 1) then
        begin
          // Se define unidad experimental en efectos aleatorios no agrupados o error
          FConstruirBloques := false;
          B := true;
        end;
      end
      else
      begin
        // El grupo actual define unidad experimental

        // Verificar definicion en efectos aleatorios no agrupados
        if (J = -1) then
        begin
          // No define unidad experimental en efectos aleatorios no agrupados, no es posible construir bloques
          FConstruirBloques := false;
          B := true;
        end
        else
        begin
          // Define unidad experimental en efectos aleatorios no agrupados, comparar criterios
          if not ((FModelo.GruposEfectosAleatorios [I].Opciones [L] as TOpcionParametroTEfecto).Parametro.IgualA((FModelo.EfectosAleatorios.Opciones [J] as TOpcionParametroTEfecto).Parametro)) then
          begin
            // Los criterios no coinciden, no es posible construir bloques
            FConstruirBloques := false;
            B := true;
          end;
        end;

        // Verificar definicion en error. Notar que es posible construir bloques si no se define unidad experimental en el error
        if (K <> -1) then
        begin
          // Define unidad experimental en error, comparar criterios
          if not ((FModelo.GruposEfectosAleatorios [I].Opciones [L] as TOpcionParametroTEfecto).Parametro.IgualA((FModelo.Error [K] as TOpcionParametroTEfecto).Parametro)) then
          begin
            // Los criterios no coinciden, no es posible construir bloques
            FConstruirBloques := false;
            B := true;
          end;
        end;
      end;

      Inc(I);
    end;

    // Construir elementos no volatiles del procedimiento

    // Contruir vector de observaciones de la variable dependiente
    ConstruirVectorObservacionesVariableDependiente(FY);
    FYT := FY.Transpuesta;

    // Construir vector de pesos, de ser necesario
    if FModelo.TienePesos then
      ConstruirVectorPesos(FW);

    // Verificar si el modelo posee efectos fijos
    if FModelo.TieneEfectosFijos then
    begin
      // El modelo posee efectos fijos, construir informacion de tiempo de ejecucion
      FInformacionTiempoEjecucionEfectosFijos := ConstruirInformacionTiempoEjecucionEfectosFijos(FModelo.EfectosFijos);

      // Construir matriz de diseno efectos fijos y su traspuesta
      ConstruirMatrizDisenoEfectosFijos(FX);
      FXT := FX.Transpuesta;

      // Calcular el rango de X
      FRangoX := FX.Rango;
    end;

    // Inicializar variables de clasificacion auxiliares
    FVariablesClasificacionAuxiliares.Clear;

    // Inicializar informacion sobre criterios
    FInformacionTiempoEjecucionCriterios.Clear;

    // Verificar si el modelo posee efectos aleatorios
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
    FInformacionTiempoEjecucionError := ConstruirInformacionTiempoEjecucionError(FModelo.Error);

    // Calcular la cantidad de parametros de covarianza
    FCantidadParametrosCovarianza := FCantidadParametrosEfectosAleatorios + FInformacionTiempoEjecucionError.CantidadParametros;

    // Calcular la cantidad de observaciones
    FCantidadObservaciones := FModelo.Datos.Valores.CantidadFilas;

    // Inicializar opciones del procedimiento

    // Inicializar criterio de convergencia

    if FOpciones.BuscarAsignar(I, 'criterio_convergencia') then
    begin
       with (FOpciones [I] as TOpcionParametroString) do
       begin
         if Parametro = 'convergencia_parametros_absoluta' then
           FCriterioConvergencia := ConvergenciaParametrosAbsoluta
         else if Parametro = 'convergencia_parametros_relativa' then
           FCriterioConvergencia := ConvergenciaParametrosRelativa
         else if Parametro = 'convergencia_verosimilitud_absoluta' then
           FCriterioConvergencia := ConvergenciaVerosimilitudAbsoluta
         else if Parametro = 'convergencia_verosimilitud_relativa' then
           FCriterioConvergencia := ConvergenciaVerosimilitudRelativa
         else if Parametro = 'convergencia_gradiente_absoluta' then
           FCriterioConvergencia := ConvergenciaGradienteAbsoluta
         else if Parametro = 'convergencia_gradiente_relativa' then
           FCriterioConvergencia := ConvergenciaGradienteRelativa
         else if Parametro = 'convergencia_hessiano_absoluta' then
           FCriterioConvergencia := ConvergenciaHessianoAbsoluta
         else if Parametro = 'convergencia_hessiano_relativa' then
           FCriterioConvergencia := ConvergenciaHessianoRelativa;
       end;
    end
    else
      FCriterioConvergencia := CCriterioConvergencia;

    // Inicializar otras opciones

    if FOpciones.BuscarAsignar(I, 'cantidad_pasos_scoring') then
      FCantidadPasosScoring := (FOpciones [I] as TOpcionParametroInteger).Parametro
    else
      FCantidadPasosScoring := CCantidadPasosScoring;

    if FOpciones.BuscarAsignar(I, 'cantidad_maxima_iteraciones') then
      FCantidadMaximaIteraciones := (FOpciones [I] as TOpcionParametroInteger).Parametro
    else
      FCantidadMaximaIteraciones := CCantidadMaximaIteraciones;

    if FOpciones.BuscarAsignar(I, 'cantidad_maxima_evaluaciones_funcion_objetivo') then
      FCantidadMaximaEvaluacionesFuncionObjetivo := (FOpciones [I] as TOpcionParametroInteger).Parametro
    else
      FCantidadMaximaEvaluacionesFuncionObjetivo := CCantidadMaximaEvaluacionesFuncionObjetivo;

    if FOpciones.BuscarAsignar(I, 'tolerancia_convergencia') then
      FToleranciaConvergencia := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FToleranciaConvergencia := CToleranciaConvergencia;

    if FOpciones.BuscarAsignar(I, 'tolerancia_singularidad') then
      FToleranciaSingularidad := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FToleranciaSingularidad := 1E4 * UaEpsilon;

    if FOpciones.BuscarAsignar(I, 'tolerancia_cholesky') then
      FToleranciaCholesky := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FToleranciaCholesky := 1E4 * UaEpsilon;

    if FOpciones.BuscarAsignar(I, 'valor_inicial_factor_ridging') then
      FValorInicialFactorRidging := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FValorInicialFactorRidging := CValorInicialFactorRidging;

    if FOpciones.BuscarAsignar(I, 'valor_minimo_factor_ridging') then
      FValorMinimoFactorRidging := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FValorMinimoFactorRidging := CValorMinimoFactorRidging;

    if FOpciones.BuscarAsignar(I, 'valor_maximo_factor_ridging') then
      FValorMaximoFactorRidging := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FValorMaximoFactorRidging := CValorMaximoFactorRidging;

    if FOpciones.BuscarAsignar(I, 'factor_incremento_factor_ridging') then
      FFactorIncrementoFactorRidging := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FFactorIncrementoFactorRidging := CFactorIncrementoFactorRidging;

    if FOpciones.BuscarAsignar(I, 'factor_decremento_factor_ridging') then
      FFactorDecrementoFactorRidging := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      FFactorDecrementoFactorRidging := CFactorDecrementoFactorRidging;

    // Inicializar cotas inferiores

    if FOpciones.BuscarAsignar(I, 'verificar_cota_inferior_parametros') then
      FVerificarCotasInferiores := (FOpciones [I] as TOpcionParametroBoolean).Parametro
    else
      FVerificarCotasInferiores := CVerificarCotaInferiorParametros;

    if FOpciones.BuscarAsignar(I, 'cota_inferior_parametros') then
      R := (FOpciones [I] as TOpcionParametroReal).Parametro
    else
      R := CCotaInferiorParametros;

    if FVerificarCotasInferiores then
    begin
      for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
      begin
        for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
        begin
          // Si no fue provisto una cota, utilizar el valor por defecto
          if not FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaInferior then
          begin
            FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.CotasInferiores [J] := R;
            FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaInferior := True;
          end;
        end;
      end;
      for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
      begin
        // Si no fue provisto una cota, utilizar el valor por defecto
        if not FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].TieneCotaInferior then
        begin
          FInformacionTiempoEjecucionError.Parametros.CotasInferiores [I] := R;
          FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].TieneCotaInferior := True;
        end;
      end;
    end;

    // Inicializar cotas superiores

    if FOpciones.BuscarAsignar(I, 'verificar_cota_superior_parametros') then
      FVerificarCotasSuperiores := (FOpciones [I] as TOpcionParametroBoolean).Parametro
    else
      FVerificarCotasSuperiores := False;

    if FOpciones.BuscarAsignar(I, 'cota_superior_parametros') then
    begin
      R := (FOpciones [I] as TOpcionParametroReal).Parametro;

      for I := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
      begin
        for J := FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.Alto do
        begin
          if not FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaSuperior then
          begin
            FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.CotasSuperiores [J] := R;
            FInformacionTiempoEjecucionEfectosAleatorios [I].Parametros.InformacionParametros [J].TieneCotaSuperior := True;
          end;
        end;
      end;
      for I := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
      begin
        if not FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].TieneCotaSuperior then
        begin
          FInformacionTiempoEjecucionError.Parametros.CotasSuperiores [I] := R;
          FInformacionTiempoEjecucionError.Parametros.InformacionParametros [I].TieneCotaSuperior := True;
        end;
      end;
    end;

    // Inicializar metodo de ajuste
    FCalcularVerosimilitudResidual := False;
    if FOpciones.BuscarAsignar(I, 'metodo') then
    begin
      with (FOpciones [I] as TOpcionParametroString) do
      begin
        if Parametro = 'mivque0' then
          FMetodoAjuste := MetodoMIVQUE0
        else if Parametro = 'reml' then
        begin
          FMetodoAjuste := MetodoMaximaVerosimilitudMarquardt;
          FCalcularVerosimilitudResidual := True;
        end
        else if Parametro = 'ml' then
        begin
          FMetodoAjuste := MetodoMaximaVerosimilitudMarquardt;
          FCalcularVerosimilitudResidual := False;
        end
        else if Parametro = 'grilla_busqueda_ml' then
        begin
          FMetodoAjuste := MetodoGrillaBusqueda;
          FCalcularVerosimilitudResidual := False;
        end
        else if Parametro = 'grilla_busqueda_reml' then
        begin
          FMetodoAjuste := MetodoGrillaBusqueda;
          FCalcularVerosimilitudResidual := True;
        end
        else if Parametro = 'depuracion' then
          FMetodoAjuste := MetodoDepuracion
        else
        begin
          // REML por defecto
          FMetodoAjuste := MetodoMaximaVerosimilitudMarquardt;
          FCalcularVerosimilitudResidual := False;
        end;
      end;
    end
    else
    begin
      // REML por defecto
      FMetodoAjuste := MetodoMaximaVerosimilitudMarquardt;
      FCalcularVerosimilitudResidual := True;
    end;

    // Inicializar parametros

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

    // Inicializar valores iniciales
    B := True;
    if FOpciones.BuscarAsignar(I, 'fuente_valores_iniciales_parametros') then
    begin
      // Identificar fuente de valores iniciales de parametros

      if (FOpciones [I] as TOpcionParametroString).Parametro = 'valores_configuracion' then
      begin
        // Inicializar con los valores de configuracion

        J := 1;
        for K := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
        begin
          for L := FInformacionTiempoEjecucionEfectosAleatorios [K].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [K].Parametros.Alto do
          begin
            // Si no fue provisto un valor inicial, utilizar el valor por defecto
            if FInformacionTiempoEjecucionEfectosAleatorios [K].Parametros.InformacionParametros [L].TieneValorInicial then
              FValoresParametrosEfectosAleatorios [J] := FInformacionTiempoEjecucionEfectosAleatorios [K].Parametros.ValoresIniciales [L]
            else
              FValoresParametrosEfectosAleatorios [J] := CValorInicialParametros;

            Inc(J);
          end;
        end;
        for K := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
        begin
          // Si no fue provisto un valor inicial, utilizar el valor por defecto
          if FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].TieneValorInicial then
            FValoresParametrosEfectosAleatorios [J] := FInformacionTiempoEjecucionError.Parametros.ValoresIniciales [K]
          else
            FValoresParametrosEfectosAleatorios [J] := CValorInicialParametros;

          Inc(J);
        end;

        B := False;
      end
      else if ((FOpciones [I] as TOpcionParametroString).Parametro = 'grilla_busqueda_ml') or
              ((FOpciones [I] as TOpcionParametroString).Parametro = 'grilla_busqueda_reml') then
      begin
        // Inicializar por grilla de busqueda optimizando funcion de verosimilitud

        CalcularVerosimilitudResidualAjuste := FCalcularVerosimilitudResidual;

        if (FOpciones [I] as TOpcionParametroString).Parametro = 'grilla_busqueda_reml' then
          FCalcularVerosimilitudResidual := True;

        // Inicializar parametros no involucrados en la busqueda
        IndiceParametro := 0;
        for J := 0 to FInformacionTiempoEjecucionEfectosAleatorios.Count - 1 do
        begin
          for K := FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Bajo to FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.Alto do
          begin
            if (not FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].TieneCotaInferior) or
               (not FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].TieneCotaSuperior) or
               (not FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].TienePaso) then
            begin
              if FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].TieneValorInicial then
                FValoresParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionEfectosAleatorios [J].Parametros.InformacionParametros [K].ValorInicial
              else
                FValoresParametrosEfectosAleatorios [IndiceParametro] := CValorInicialParametros;
            end;

            Inc(IndiceParametro);
          end;
        end;
        for K := FInformacionTiempoEjecucionError.Parametros.Bajo to FInformacionTiempoEjecucionError.Parametros.Alto do
        begin
          if (not FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].TieneCotaInferior) or
             (not FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].TieneCotaSuperior) or
             (not FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].TienePaso) then
          begin
            if FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].TieneValorInicial then
              FValoresParametrosEfectosAleatorios [IndiceParametro] := FInformacionTiempoEjecucionError.Parametros.InformacionParametros [K].ValorInicial
            else
              FValoresParametrosEfectosAleatorios [IndiceParametro] := CValorInicialParametros;
          end;

          Inc(IndiceParametro);
        end;

        Bitacora1 := TStringList.Create;
        Bitacora2 := TStringList.Create;
        if not MetodoGrillaBusqueda(FValoresParametrosEfectosAleatorios, Bitacora1, Bitacora2) then
        begin
          Result := False;
          Bitacora.AddStrings(Bitacora1);
        end;
        FreeAndNil(Bitacora1);
        FreeAndNil(Bitacora2);

        FCalcularVerosimilitudResidual := CalcularVerosimilitudResidualAjuste;
        B := False;
      end
      else if (FOpciones [I] as TOpcionParametroString).Parametro = 'mivque0' then
      begin
        // Inicializar mediante mivque0 (por defecto)

        Bitacora1 := TStringList.Create;
        Bitacora2 := TStringList.Create;
        if not MetodoMIVQUE0(FValoresParametrosEfectosAleatorios, Bitacora1, Bitacora2) then
        begin
          Result := False;
          Bitacora.AddStrings(Bitacora1);
        end;
        FreeAndNil(Bitacora1);
        FreeAndNil(Bitacora2);

        B := False;
      end;
    end;

    if B then
    begin
      // Inicializar a valores por defecto
      if FOpciones.BuscarAsignar(I, 'valor_inicial_parametros') then
        R := (FOpciones [I] as TOpcionParametroReal).Parametro
      else
        R := CValorInicialParametros;

       FValoresParametrosEfectosAleatorios.Asignar(R);
    end;
  except on E: Exception do
    begin
      Result := False;
      Bitacora.Add('TProcedimiento.Inicializar: ' + E.Message);
    end;
  end;
end { TProcedimiento.Inicializar };

function TProcedimiento.MetodoDepuracion(var ParametrosEfectosAleatorios: TUaVector; var Bitacora: TStrings; var BitacoraIteracion: TStrings): Boolean;
begin { TProcedimiento.MetodoDepuracion }
  Result := False;
end { TProcedimiento.MetodoDepuracion };

end { UnitProcedimiento }.

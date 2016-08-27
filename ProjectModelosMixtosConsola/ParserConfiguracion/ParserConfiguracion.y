%{

{**
@abstract(Parser del archivo de configuracion de procedimiento de ajuste de
modelos mixtos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 29, 2005)
Este modulo contiene la definicion del parser de archivos de configuracion
del procedimiento de ajuste de modelos mixtos. El mismo es generado por
tplex/tpyacc, los cuales pueden obtenerse de las fuentes del compilador
FreePascal.
}

unit UnitParserConfiguracion;

{$H-}

interface

uses
  Classes, SysUtils,
  UnitConfiguracionDatos, UnitConfiguracionModelo,
  UnitConfiguracionProcedimiento, UnitMatrizValores;

type
  {** Clase para describir un error de sintaxis en los archivos de
      configuracion.
      @abstract(Error de sintaxis en el archivo de configuracion.) }
  TErrorSintaxis = class
  private
    {** Indice de la linea donde se produjo el error. }
    FIndiceLinea: Integer;
    {** Indice de la columna donde se produjo el error. }
    FIndiceColumna: Integer;
    {** Contexto del error. }
    FContexto: String;
    {** Mensaje de error generador por el parser. }
    FMensaje: String;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AIndiceLinea Indice de la linea donde se produjo el error.)
        @param(AIndiceColumna Indice de la columna donde se produjo el
        error.)
        @param(AContexto Contexto del error.)
        @param(AMensaje Mensaje generado por el parser.) }
    constructor Create(const AIndiceLinea, AIndiceColumna: Integer; const AContexto, AMensaje: String); overload;
    {** Indice de la linea donde se produjo el error. }
    property IndiceLinea: Integer read FIndiceLinea write FIndiceLinea;
    {** Indice de la columna donde se produjo el error. }
    property IndiceColumna: Integer read FIndiceColumna write FIndiceColumna;
    {** Contexto del error. Es el contenido de yytext al momento de producirse
        el error. }   
    property Contexto: String read FContexto write FContexto;
    {** Mensaje de error generador por el parser. }
    property Mensaje: String read FMensaje write FMensaje;
  end { TErrorSintaxis };

  {** Parser de los archivos de configuracion del procedimiento de ajuste.
      Esta clase envuelve las subrutinas generadas por tplex y tpyacc para
      realizar su tarea.
      @abstract(Parser de los archivos de configuracion.) }
  TParserConfiguracion = class
  private
    {** Stream de entrada. }
    FStreamEntrada: TStream;
    {** Stream de salida. }
    FStreamSalida: TStream;
    {** Configuracion de datos parseada. }
    FConfiguracionDatos: TConfiguracionDatos;
    {** Configuracion de modelo parseada. }
    FConfiguracionModelo: TConfiguracionModelo;
    {** Configuracion de procedimiento parseada. }
    FConfiguracionProcedimiento: TConfiguracionProcedimiento;
    {** Error de sintaxis encontrado. }
    FErrorSintaxis: TErrorSintaxis;
  public
    {** Constructor.
        @param(AStreamEntrada Stream de entrada que contiene la el texto de
        configuracion.) }
    constructor Create(const AStreamEntrada: TStream);  overload;
    {** Constructor.
        @param(AStreamEntrada Stream de entrada que contiene la el texto de
        configuracion.)
        @param(AStreamSalida Stream al cual enviar los mensajes de salida
        del parser.) }
    constructor Create(const AStreamEntrada, AStreamSalida: TStream); overload;
    {** Constructor.
        @param(NombreArchivo Nombre del archivo de configuracion.) }
    constructor Create(const NombreArchivo: TFileName); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Metodo que inicia el proceso de parseo. Es el encargado de llamar a la
        funcion yyparse de la libreria de tplex/tpyacc.
        @returns(@true si el parseo fue exitoso, o @false en caso contrario.) }
    function Parsear: Boolean;
    {** Stream de entrada con el contenido de la fuente de configuracion. }
    property StreamEntrada: TStream read FStreamEntrada write FStreamEntrada;
    {** Stream de salida donde se envian los mensajes de salida del parser. }
    property StreamSalida: TStream read FStreamSalida write FStreamSalida;
    {** Configuracion de datos parseada. }
    property Datos: TConfiguracionDatos read FConfiguracionDatos;
    {** Configuracion de modelo parseada. }
    property Modelo: TConfiguracionModelo read FConfiguracionModelo;
    {** Configuracion de procedimiento parseada. }
    property Procedimiento: TConfiguracionProcedimiento read FConfiguracionProcedimiento;
    {** Error de sintaxis en la configuracion. Si los indices de linea o
        columna son menores que cero, esto indica que no se produjeron errores
        de sintaxis. }
    property ErrorSintaxis: TErrorSintaxis read FErrorSintaxis;
  end { TParserConfiguracion };

implementation

uses
  YaccLib, LexLib,
  StreamIO, UnitArregloStrings, UnitColumnaDatos, UnitEfecto, UnitEstructura,
  UnitGrupoEfectos, UnitOpcion, UnitParametros;

type
  {** Tupla conteniendo los resultados de parsear la seccion
      efectos_aleatorios de la configuracion. }
  TTuplaEfectosAleatorios = record
    {** Efectos aleatorios no agrupados. }
    EfectosAleatorios: TGrupoEfectos;
    {** Lista de grupos de efectos aleatorios. }
    ListaGruposEfectosAleatorios: TListaGruposEfectos;
    {** Lista de opciones del error. }
    Error: TListaOpciones;
  end { TTuplaEfectosAleatorios };

  {** Record variante para indicar la presencia o ausencia y el valor de un
      componente de la tupla de definicion de parametros. }
  TComponenteInformacionParametro = record
    {** Indica si se parseo un valor. }
    ValorPresente: Boolean;
    case Integer of
      {** Valor entero parseado. }
      1: (ValorEntero: Integer);
      {** Valor real parseado. }
      2: (ValorReal: Real);
      {** Valor de cadenas de caracteres parseado. }
      3: (ValorString: ShortString);
  end;
    
var
  {** Error de sintaxis global. }
  ErrorSintaxis1: TErrorSintaxis;
  {** Configuracion de datos. }
  ConfiguracionDatos: TConfiguracionDatos;
  {** Configuracion de modelo. }
  ConfiguracionModelo: TConfiguracionModelo;
  {** Configuracion de procedimiento. }
  ConfiguracionProcedimiento: TConfiguracionProcedimiento;
  {** Le indica al lexer si debe buscar un identificador en la tabla de
      palabras reservadas. }
  IdentificarTokensPalabrasClave: Boolean;
  {** Indice de fila y columna de la matriz de valores. }
  IndiceColumna, IndiceFila: Integer;
  {** Matriz de valores. }
  MatrizValores: TMatrizValores;

procedure yyerror(const Mensaje: String);
begin
  ErrorSintaxis1.IndiceLinea := yylineno;
  ErrorSintaxis1.IndiceColumna := yycolno;
  ErrorSintaxis1.Contexto := yytext;
  ErrorSintaxis1.Mensaje := Mensaje;
end;

%}

%token T_LLAVE_APERTURA
%token T_LLAVE_CLAUSURA
%token T_PARENTESIS_APERTURA
%token T_PARENTESIS_CLAUSURA
%token T_CORCHETE_APERTURA
%token T_CORCHETE_CLAUSURA
%token T_DOS_PUNTOS
%token T_COMA
%token T_PUNTO_Y_COMA
%token T_ASTERISCO

%token <Integer> T_ENTERO
%token <Real> T_REAL
%token <ShortString> T_IDENTIFICADOR
%token <ShortString> T_LITERAL_CADENAS
%token <Boolean> T_BOOLEAN

%token T_DATOS
%token T_COLUMNAS
%token T_CATEGORICOS
%token T_ENTEROS
%token T_REALES
%token T_VALORES
%token T_MODELO
%token T_VARIABLES_CLASIFICACION
%token T_VARIABLE_DEPENDIENTE
%token T_EFECTOS_FIJOS
%token T_EFECTOS
%token T_INTERCEPTO
%token T_OPCION
%token T_GENERAR_COLUMNAS_NULAS
%token T_INCLUIR_INTERCEPTO
%token T_EFECTOS_ALEATORIOS
%token T_GRUPO
%token T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL
%token T_UNIDAD_EXPERIMENTAL
%token T_ESTRUCTURA
%token T_SIMETRIA_COMPUESTA
%token T_COMPONENTES_VARIANZA
%token T_GENERAL
%token T_DIAGONAL_HETEROGENEA
%token T_FACTOR_ANALYTIC
%token T_FACTOR_ANALYTIC_SIN_DIAGONAL
%token T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR
%token T_BANDEADA
%token T_AUTOREGRESIVA
%token T_PARAMETROS
%token T_ERROR
%token T_CRITERIO_ORDENAMIENTO
%token T_PESO
%token T_PROCEDIMIENTO
%token T_METODO
%token T_MIVQUE0
%token T_ML
%token T_REML
%token T_GRILLA_BUSQUEDA_ML
%token T_GRILLA_BUSQUEDA_REML
%token T_VALOR_INICIAL_PARAMETROS
%token T_FUENTE_VALORES_INICIALES_PARAMETROS
%token T_VALORES_POR_DEFECTO
%token T_VALORES_CONFIGURACION
%token T_CRITERIO_CONVERGENCIA
%token T_CONVERGENCIA_PARAMETROS_ABSOLUTA
%token T_CONVERGENCIA_PARAMETROS_RELATIVA
%token T_CONVERGENCIA_VEROSIMILITUD_ABSOLUTA
%token T_CONVERGENCIA_VEROSIMILITUD_RELATIVA
%token T_CONVERGENCIA_GRADIENTE_ABSOLUTA
%token T_CONVERGENCIA_GRADIENTE_RELATIVA
%token T_CONVERGENCIA_HESSIANO_ABSOLUTA
%token T_CONVERGENCIA_HESSIANO_RELATIVA
%token T_TOLERANCIA_CONVERGENCIA
%token T_CONSTANTE_RIDGE
%token T_CANTIDAD_PASOS_SCORING
%token T_COTA_INFERIOR_PARAMETROS
%token T_COTA_SUPERIOR_PARAMETROS
%token T_PASO_GRILLA_PARAMETROS
%token T_VERIFICAR_COTA_INFERIOR_PARAMETROS
%token T_VERIFICAR_COTA_SUPERIOR_PARAMETROS
%token T_CANTIDAD_MAXIMA_ITERACIONES
%token T_CANTIDAD_MAXIMA_EVALUACIONES_FUNCION_OBJETIVO
%token T_TOLERANCIA_SINGULARIDAD
%token T_TOLERANCIA_CHOLEKSY
%token T_VALOR_INICIAL_FACTOR_RIDGING
%token T_FACTOR_INCREMENTO_FACTOR_RIDGING
%token T_FACTOR_DECREMENTO_FACTOR_RIDGING
%token T_VALOR_MINIMO_FACTOR_RIDGING
%token T_VALOR_MAXIMO_FACTOR_RIDGING
%token T_SALIDA
%token T_DEPURACION
%token T_ARCHIVO_SALIDA
%token T_GENERAR_SALIDA_COMPACTA
%token T_MOSTRAR_SALIDA_CONSOLA
%token T_ARCHIVO_RESUMEN_VEROSIMILITUD
%token T_ARCHIVO_RESUMEN_BETA
%token T_ARCHIVO_RESUMEN_THETA
%token T_ARCHIVO_RESUMEN_ITERACION

%type <TConfiguracionDatos> datos
%type <TConfiguracionModelo> modelo
%type <TConfiguracionProcedimiento> procedimiento
%type <TListaColumnasDatos> columnas
%type <TListaColumnasDatos> lista_columnas
%type <TColumnaDatos> columna
%type <Integer> tipo_columna
%type <TMatrizValores> valores
%type <ShortString> valor
%type <TStrings> variables_clasificacion
%type <ShortString> variable_dependiente
%type <TStrings> lista_identificadores
%type <TArregloStrings> arreglo_identificadores
%type <TArregloStrings> arreglo_identificadores1
%type <TGrupoEfectos> efectos_fijos
%type <TListaEfectos> efectos
%type <TListaEfectos> lista_efectos
%type <TEfecto> efecto
%type <TEfecto> cruzamiento
%type <TEfecto> anidamiento
%type <TListaEfectosAnidados> lista_efectos_anidados
%type <TListaOpciones> lista_opciones_efectos_fijos
%type <TOpcion> opcion_efectos_fijos
%type <TOpcion> opcion_efectos_fijos1
%type <TTuplaEfectosAleatorios> efectos_aleatorios
%type <TListaGruposEfectos> lista_grupos_efectos_aleatorios
%type <TGrupoEfectos> grupo_efectos
%type <TListaOpciones> lista_opciones_efectos_aleatorios
%type <TOpcion> opcion_efectos_aleatorios
%type <TOpcion> opcion_efectos_aleatorios1
%type <TEstructura> estructura
%type <Integer> orden
%type <TListaInformacionParametroIndice> lista_parametros
%type <TInformacionParametroIndice> informacion_parametro_indice
%type <TComponenteInformacionParametro> informacion_parametro_componente_entero
%type <TComponenteInformacionParametro> informacion_parametro_componente_real
%type <TComponenteInformacionParametro> informacion_parametro_componente_string
%type <TListaOpciones> error1
%type <TListaOpciones> lista_opciones_error
%type <TOpcion> opcion_error
%type <TOpcion> opcion_error1
%type <TListaOpciones> lista_opciones_modelo
%type <TOpcion> opcion_modelo
%type <TOpcion> opcion_modelo1
%type <TListaOpciones> lista_opciones_procedimiento
%type <TOpcion> opcion_procedimiento
%type <TOpcion> opcion_procedimiento1
%type <ShortString> opcion_procedimiento1_metodo
%type <ShortString> opcion_procedimiento1_fuente
%type <ShortString> opcion_procedimiento1_criterio_convergencia
%type <Real> numero

%%

configuracion : datos         { ConfiguracionDatos := $1; }
                modelo        { ConfiguracionModelo := $3;
                                ConfiguracionModelo.Datos := $1; }
                procedimiento { ConfiguracionProcedimiento := $5;
                                ConfiguracionProcedimiento.Modelo := $3; }
              ;

datos : T_DATOS
        T_LITERAL_CADENAS
        T_LLAVE_APERTURA
        columnas
        valores
        T_LLAVE_CLAUSURA
        T_PUNTO_Y_COMA    { $$ := TConfiguracionDatos.Create($2, $4, $5); }
      ;

columnas : T_COLUMNAS lista_columnas T_PUNTO_Y_COMA { $$ := $2; }
         ;

lista_columnas : lista_columnas columna { $$ := $1; $$.Add($2); }
               | columna                { $$ := TListaColumnasDatos.Create; $$.Add($1); }
               ;
               
columna : T_IDENTIFICADOR T_DOS_PUNTOS tipo_columna { case $3 of
                                                        T_CATEGORICOS : $$ := TColumnaDatosCategoricos.Create($1);
                                                        T_ENTEROS : $$ := TColumnaDatosEnteros.Create($1);
                                                        T_REALES : $$ := TColumnaDatosReales.Create($1);
                                                      end; } 
        ;

tipo_columna : T_CATEGORICOS { $$ := T_CATEGORICOS; }
             | T_ENTEROS     { $$ := T_ENTEROS; }
             | T_REALES      { $$ := T_REALES; }
             ;

valores : T_VALORES
          T_LLAVE_APERTURA
                              { IdentificarTokensPalabrasClave := False;
                                FreeAndNil(MatrizValores);
                                MatrizValores := TMatrizValores.Create(1, 1); }
          lista_filas_valores
                              { IdentificarTokensPalabrasClave := True; }
          T_LLAVE_CLAUSURA
          T_PUNTO_Y_COMA      { $$ := MatrizValores; }
          ;

lista_filas_valores : lista_filas_valores fila_valores T_PUNTO_Y_COMA { IndiceColumna := 0; Inc(IndiceFila); }
                    | fila_valores T_PUNTO_Y_COMA                     { IndiceColumna := 0; Inc(IndiceFila); }
                    ;

fila_valores : fila_valores valor { MatrizValores [IndiceFila, IndiceColumna] := $2; Inc(IndiceColumna); }
             | valor              { MatrizValores [IndiceFila, IndiceColumna] := $1; Inc(IndiceColumna); }
             ;

valor : T_IDENTIFICADOR { $$ := $1 }
      | T_ENTERO        { $$ := IntToStr($1) }
      | T_REAL          { $$ := FloatToStr($1) }
      ; 

modelo : T_MODELO
         T_LITERAL_CADENAS
         T_LLAVE_APERTURA
         variables_clasificacion
         variable_dependiente
         efectos_fijos
         efectos_aleatorios
         lista_opciones_modelo
         T_LLAVE_CLAUSURA
         T_PUNTO_Y_COMA           { $$ := TConfiguracionModelo.Create($2,
                                                                      $4,
                                                                      $5,
                                                                      $6,
                                                                      $7.EfectosAleatorios,
                                                                      $7.ListaGruposEfectosAleatorios,
                                                                      $7.Error,
                                                                      $8); }
       ;

variables_clasificacion :                           { $$ := TStringList.Create; }
                        | T_VARIABLES_CLASIFICACION
                          lista_identificadores
                          T_PUNTO_Y_COMA            { $$ := $2; }
                        ;

lista_identificadores : lista_identificadores T_IDENTIFICADOR { $$ := $1; $$.Add($2); }
                      | T_IDENTIFICADOR                       { $$ := TStringList.Create; $$.Add($1); }
                      ;

arreglo_identificadores : T_CORCHETE_APERTURA arreglo_identificadores1 T_CORCHETE_CLAUSURA { $$ := $2; }
                        ;

arreglo_identificadores1 : arreglo_identificadores1 T_COMA T_IDENTIFICADOR { $$ := $1; $$.Dimension := $$.Dimension + 1; $$ [$$.Alto] := $3; }
                         | T_IDENTIFICADOR                                 { $$ := TArregloStrings.Create(1); $$ [$$.Bajo] := $1; }
                         ;

variable_dependiente : T_VARIABLE_DEPENDIENTE T_IDENTIFICADOR T_PUNTO_Y_COMA { $$ := $2; } 
                     ;

efectos_fijos :                              { $$ := TGrupoEfectos.Create(TListaEfectos.Create, TListaOpciones.Create); }
              | T_EFECTOS_FIJOS
                T_LLAVE_APERTURA
                efectos
                lista_opciones_efectos_fijos
                T_LLAVE_CLAUSURA
                T_PUNTO_Y_COMA               { $$ := TGrupoEfectos.Create($3, $4); }
              ;

efectos :                                        { $$ := TListaEfectos.Create; }
        | T_EFECTOS lista_efectos T_PUNTO_Y_COMA { $$ := $2; }
        ;

lista_efectos : lista_efectos efecto { $$ := $1; $$.Add($2); }
              | efecto               { $$ := TListaEfectos.Create; $$.Add($1); }
              ;

efecto : T_INTERCEPTO { $$ := TIntercepto.Create; }
       | cruzamiento  { $$ := $1; }
       | anidamiento  { $$ := $1; }
       ;

cruzamiento : T_IDENTIFICADOR T_ASTERISCO cruzamiento { $$ := TCruzamiento.Create(TEfectoPrincipal.Create($1), $3); }
            | T_IDENTIFICADOR                         { $$ := TEfectoPrincipal.Create($1); }
            ;

anidamiento : cruzamiento
              T_PARENTESIS_APERTURA
              lista_efectos_anidados
              T_PARENTESIS_CLAUSURA  { $$ := TAnidamiento.Create($1, $3); }
            ;

lista_efectos_anidados : lista_efectos_anidados T_IDENTIFICADOR { $$ := $1; $$.Add(TEfectoPrincipal.Create($2)); }
                       | T_IDENTIFICADOR                        { $$ := TListaEfectosAnidados.Create; $$.Add(TEfectoPrincipal.Create($1)); }
                       ; 

lista_opciones_efectos_fijos :                                                   { $$ := TListaOpciones.Create; }
                             | lista_opciones_efectos_fijos opcion_efectos_fijos { $$ := $1; $$.Add($2); }
                             ;

opcion_efectos_fijos : T_OPCION opcion_efectos_fijos1 T_PUNTO_Y_COMA { $$ := $2; }
                     ;

opcion_efectos_fijos1 : T_GENERAR_COLUMNAS_NULAS T_BOOLEAN { $$ := TOpcionParametroBoolean.Create('generar_columnas_nulas', $2); }
                      | T_INCLUIR_INTERCEPTO T_BOOLEAN     { $$ := TOpcionParametroBoolean.Create('incluir_intercepto', $2); }
                      ;

efectos_aleatorios :                                   { $$.EfectosAleatorios := nil;
                                                         $$.ListaGruposEfectosAleatorios := nil;
                                                         $$.Error := nil; }
                   | T_EFECTOS_ALEATORIOS
                     T_LLAVE_APERTURA
                     lista_grupos_efectos_aleatorios
                     efectos
                     lista_opciones_efectos_aleatorios
                     error1
                     T_LLAVE_CLAUSURA
                     T_PUNTO_Y_COMA                    { $$.EfectosAleatorios := TGrupoEfectos.Create($4, $5);
                                                         $$.ListaGruposEfectosAleatorios := $3;
                                                         $$.Error := $6; }
                   ;

lista_grupos_efectos_aleatorios :                                               { $$ := TListaGruposEfectos.Create; }
                                | lista_grupos_efectos_aleatorios grupo_efectos { $$ := $1; $$.Add($2); }
                                ;

grupo_efectos : T_GRUPO
                T_LLAVE_APERTURA
                efectos
                lista_opciones_efectos_aleatorios
                T_LLAVE_CLAUSURA
                T_PUNTO_Y_COMA                    { $$ := TGrupoEfectos.Create($3, $4); }
              ;

lista_opciones_efectos_aleatorios :                                                             { $$ := TListaOpciones.Create; }
                                  | lista_opciones_efectos_aleatorios opcion_efectos_aleatorios { $$ := $1; $$.Add($2); }
                                  ;

opcion_efectos_aleatorios : T_OPCION opcion_efectos_aleatorios1 T_PUNTO_Y_COMA { $$ := $2; }
                          ;

opcion_efectos_aleatorios1 : T_GENERAR_COLUMNAS_NULAS T_BOOLEAN                 { $$ := TOpcionParametroBoolean.Create('generar_columnas_nulas', $2); }
                           | T_INCLUIR_INTERCEPTO T_BOOLEAN                     { $$ := TOpcionParametroBoolean.Create('incluir_intercepto', $2); }
                           | T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL efecto { $$ := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', $2); }
                           | T_UNIDAD_EXPERIMENTAL efecto                       { $$ := TOpcionParametroTEfecto.Create('unidad_experimental', $2); }
                           | T_ESTRUCTURA estructura                            { $$ := TOpcionParametroTEstructura.Create('estructura', $2); }
                           | T_PARAMETROS lista_parametros                      { $$ := TOpcionParametroTObject.Create('parametros', $2); }
                           ;

estructura : T_SIMETRIA_COMPUESTA                     { $$ := TEstructura.Create('simetria_compuesta'); }
           | T_COMPONENTES_VARIANZA                   { $$ := TEstructura.Create('componentes_varianza'); }
           | T_GENERAL                                { $$ := TEstructura.Create('general'); }
           | T_DIAGONAL_HETEROGENEA                   { $$ := TEstructura.Create('diagonal_heterogenea'); }
           | T_AUTOREGRESIVA                          { $$ := TEstructura.Create('autoregresiva'); }
           | T_FACTOR_ANALYTIC orden                  { $$ := TEstructuraOrden.Create('factor_analytic', $2); }
           | T_FACTOR_ANALYTIC_SIN_DIAGONAL orden     { $$ := TEstructuraOrden.Create('factor_analytic_sin_diagonal', $2); }
           | T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR orden { $$ := TEstructuraOrden.Create('factor_analytic_diagonal_escalar', $2); }
           | T_BANDEADA orden                         { $$ := TEstructuraOrden.Create('bandeada', $2); }
           ;

orden :                       { $$ := 1; }
      | T_PARENTESIS_APERTURA
        T_ENTERO
        T_PARENTESIS_CLAUSURA { $$ := $2; }
      ;

lista_parametros : lista_parametros informacion_parametro_indice { $$ := $1; $$.Add($2); }
                 | informacion_parametro_indice                  { $$ := TListaInformacionParametroIndice.Create; $$.Add($1); }
                 ;
                 
informacion_parametro_indice : numero                                  { $$ := TInformacionParametroIndice.Create($1); }
                             | T_PARENTESIS_APERTURA
                               informacion_parametro_componente_entero
                               T_COMA
                               informacion_parametro_componente_string
                               T_COMA
                               informacion_parametro_componente_real
                               T_COMA
                               informacion_parametro_componente_real
                               T_COMA
                               informacion_parametro_componente_real
                               T_COMA
                               informacion_parametro_componente_real
                               T_PARENTESIS_CLAUSURA                   { $$ := TInformacionParametroIndice.Create($2.ValorEntero,
                                                                                                                  $4.ValorString,
                                                                                                                  $6.ValorReal,
                                                                                                                  $8.ValorReal,
                                                                                                                  $10.ValorReal,
                                                                                                                  $12.ValorReal,
                                                                                                                  $6.ValorPresente,
                                                                                                                  $8.ValorPresente,
                                                                                                                  $10.ValorPresente,
                                                                                                                  $12.ValorPresente); }
                             ;

informacion_parametro_componente_entero :          { $$.ValorPresente := False; $$.ValorEntero := -1; }
                                        | T_ENTERO { $$.ValorPresente := True; $$.ValorEntero := $1; }
                                        ;                             

informacion_parametro_componente_string :                 { $$.ValorPresente := False; $$.ValorString := ''; }
                                        | T_IDENTIFICADOR { $$.ValorPresente := False; $$.ValorString := $1; }
                                        ;                             

informacion_parametro_componente_real   :        { $$.ValorPresente := False; $$.ValorReal := 0.0; }
                                        | numero { $$.ValorPresente := True; $$.ValorReal := $1; }
                                        ;

error1 :                      { $$ := TListaOpciones.Create; }
       | T_ERROR
         T_LLAVE_APERTURA
         lista_opciones_error
         T_LLAVE_CLAUSURA
         T_PUNTO_Y_COMA       { $$ := $3; }
       ;

lista_opciones_error :                                   { $$ := TListaOpciones.Create; }
                     | lista_opciones_error opcion_error { $$ := $1; $$.Add($2); }
                     ;

opcion_error : T_OPCION opcion_error1 T_PUNTO_Y_COMA { $$ := $2; }
             ;

opcion_error1 : T_CRITERIO_ORDENAMIENTO efecto                     { $$ := TOpcionParametroTEfecto.Create('criterio_ordenamiento', $2); }
              | T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL efecto { $$ := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', $2); }
              | T_UNIDAD_EXPERIMENTAL efecto                       { $$ := TOpcionParametroTEfecto.Create('unidad_experimental', $2); }
              | T_ESTRUCTURA estructura                            { $$ := TOpcionParametroTEstructura.Create('estructura', $2); }
              | T_PARAMETROS lista_parametros                      { $$ := TOpcionParametroTObject.Create('parametros', $2); }
              ;

lista_opciones_modelo :                                     { $$ := TListaOpciones.Create; }
                      | lista_opciones_modelo opcion_modelo { $$ := $1; $$.Add($2); }
                      ;

opcion_modelo : T_OPCION opcion_modelo1 T_PUNTO_Y_COMA { $$ := $2; }
              ;

opcion_modelo1 : T_PESO T_IDENTIFICADOR { $$ := TOpcionParametroString.Create('peso', $2); }
               ;

procedimiento : T_PROCEDIMIENTO
                T_LLAVE_APERTURA
                lista_opciones_procedimiento
                T_LLAVE_CLAUSURA
                T_PUNTO_Y_COMA               { $$ := TConfiguracionProcedimiento.Create($3); }
              ;

lista_opciones_procedimiento :                                                   { $$ := TListaOpciones.Create; }
                             | lista_opciones_procedimiento opcion_procedimiento { $$ := $1; $$.Add($2); }
                             ;

opcion_procedimiento : T_OPCION opcion_procedimiento1 T_PUNTO_Y_COMA { $$ := $2; }
                     ;

opcion_procedimiento1 : T_METODO opcion_procedimiento1_metodo                               { $$ := TOpcionParametroString.Create('metodo', $2); }
                      | T_VALOR_INICIAL_PARAMETROS numero                                   { $$ := TOpcionParametroReal.Create('valor_inicial_parametros', $2); }
                      | T_FUENTE_VALORES_INICIALES_PARAMETROS opcion_procedimiento1_fuente  { $$ := TOpcionParametroString.Create('fuente_valores_iniciales_parametros', $2); }
                      | T_CRITERIO_CONVERGENCIA opcion_procedimiento1_criterio_convergencia { $$ := TOpcionParametroString.Create('criterio_convergencia', $2); }
                      | T_TOLERANCIA_CONVERGENCIA numero                                    { $$ := TOpcionParametroReal.Create('tolerancia_convergencia', $2); }
                      | T_CANTIDAD_PASOS_SCORING T_ENTERO                                   { $$ := TOpcionParametroInteger.Create('cantidad_pasos_scoring', $2); }
                      | T_COTA_INFERIOR_PARAMETROS numero                                   { $$ := TOpcionParametroReal.Create('cota_inferior_parametros', $2); }
                      | T_COTA_SUPERIOR_PARAMETROS numero                                   { $$ := TOpcionParametroReal.Create('cota_superior_parametros', $2); }
                      | T_PASO_GRILLA_PARAMETROS numero                                     { $$ := TOpcionParametroReal.Create('paso_grilla_parametros', $2); }
                      | T_VERIFICAR_COTA_INFERIOR_PARAMETROS T_BOOLEAN                      { $$ := TOpcionParametroBoolean.Create('verificar_cota_inferior_parametros', $2); }
                      | T_VERIFICAR_COTA_SUPERIOR_PARAMETROS T_BOOLEAN                      { $$ := TOpcionParametroBoolean.Create('verificar_cota_superior_parametros', $2); }
                      | T_CANTIDAD_MAXIMA_ITERACIONES T_ENTERO                              { $$ := TOpcionParametroInteger.Create('cantidad_maxima_iteraciones', $2); }
                      | T_CANTIDAD_MAXIMA_EVALUACIONES_FUNCION_OBJETIVO T_ENTERO            { $$ := TOpcionParametroInteger.Create('cantidad_maxima_evaluaciones_funcion_objetivo', $2); }
                      | T_TOLERANCIA_SINGULARIDAD numero                                    { $$ := TOpcionParametroReal.Create('tolerancia_singularidad', $2); }
                      | T_TOLERANCIA_CHOLEKSY numero                                        { $$ := TOpcionParametroReal.Create('tolerancia_cholesky', $2); }
                      | T_VALOR_INICIAL_FACTOR_RIDGING numero                               { $$ := TOpcionParametroReal.Create('valor_inicial_factor_ridging', $2); }
                      | T_FACTOR_INCREMENTO_FACTOR_RIDGING numero                           { $$ := TOpcionParametroReal.Create('factor_incremento_factor_ridging', $2); }
                      | T_FACTOR_DECREMENTO_FACTOR_RIDGING numero                           { $$ := TOpcionParametroReal.Create('factor_decremento_factor_ridging', $2); }
                      | T_VALOR_MINIMO_FACTOR_RIDGING numero                                { $$ := TOpcionParametroReal.Create('valor_minimo_factor_ridging', $2); }
                      | T_VALOR_MAXIMO_FACTOR_RIDGING numero                                { $$ := TOpcionParametroReal.Create('valor_maximo_factor_ridging', $2); }
                      | T_SALIDA arreglo_identificadores                                    { $$ := TOpcionParametroTObject.Create('salida', $2); }
                      | T_ARCHIVO_SALIDA T_LITERAL_CADENAS                                  { $$ := TOpcionParametroString.Create('archivo_salida', $2); }
                      | T_GENERAR_SALIDA_COMPACTA T_BOOLEAN                                 { $$ := TOpcionParametroBoolean.Create('generar_salida_compacta', $2); }
                      | T_MOSTRAR_SALIDA_CONSOLA T_BOOLEAN                                  { $$ := TOpcionParametroBoolean.Create('mostrar_salida_consola', $2); }
                      | T_ARCHIVO_RESUMEN_VEROSIMILITUD T_LITERAL_CADENAS                   { $$ := TOpcionParametroString.Create('archivo_resumen_verosimilitud', $2); }
                      | T_ARCHIVO_RESUMEN_BETA T_LITERAL_CADENAS                            { $$ := TOpcionParametroString.Create('archivo_resumen_beta', $2); }
                      | T_ARCHIVO_RESUMEN_THETA T_LITERAL_CADENAS                           { $$ := TOpcionParametroString.Create('archivo_resumen_theta', $2); }
                      | T_ARCHIVO_RESUMEN_ITERACION T_LITERAL_CADENAS                       { $$ := TOpcionParametroString.Create('archivo_resumen_iteracion', $2); }
                      ;

opcion_procedimiento1_metodo : T_ML
                             | T_REML
                             | T_MIVQUE0
                             | T_GRILLA_BUSQUEDA_ML
                             | T_GRILLA_BUSQUEDA_REML
                             | T_DEPURACION
                             ;

opcion_procedimiento1_fuente : T_VALORES_POR_DEFECTO
                             | T_VALORES_CONFIGURACION
                             | T_MIVQUE0
                             | T_GRILLA_BUSQUEDA_ML
                             | T_GRILLA_BUSQUEDA_REML
                             ;

opcion_procedimiento1_criterio_convergencia : T_CONVERGENCIA_PARAMETROS_ABSOLUTA
                                            | T_CONVERGENCIA_PARAMETROS_RELATIVA
                                            | T_CONVERGENCIA_VEROSIMILITUD_ABSOLUTA
                                            | T_CONVERGENCIA_VEROSIMILITUD_RELATIVA
                                            | T_CONVERGENCIA_GRADIENTE_ABSOLUTA
                                            | T_CONVERGENCIA_GRADIENTE_RELATIVA
                                            | T_CONVERGENCIA_HESSIANO_ABSOLUTA
                                            | T_CONVERGENCIA_HESSIANO_RELATIVA
                                            ;

numero : T_ENTERO { $$ := $1; }
       | T_REAL   { $$ := $1; }
       ;

%%

{$I ParserConfiguracionLex.pas}

constructor TErrorSintaxis.Create;
begin { TErrorSintaxis.Create }
  Create(-1, -1, '', '');
end { TErrorSintaxis.Create };

constructor TErrorSintaxis.Create(const AIndiceLinea, AIndiceColumna: Integer; const AContexto, AMensaje: String);
begin { TErrorSintaxis.Create }
  FIndiceLinea := AIndiceLinea;
  FIndiceColumna := AIndiceColumna;
  FContexto := AContexto;
  FMensaje := AMensaje;
end { TErrorSintaxis.Create };

constructor TParserConfiguracion.Create(const AStreamEntrada: TStream);
begin { TParserConfiguracion.Create }
  Assert(Assigned(AStreamEntrada), 'TParserConfiguracion.Create: Assigned(AStreamEntrada)');

  Create(AStreamEntrada, TMemoryStream.Create);
end { TParserConfiguracion.Create };

constructor TParserConfiguracion.Create(const AStreamEntrada, AStreamSalida: TStream);
begin { TParserConfiguracion.Create }
  Assert(Assigned(AStreamEntrada), 'TParserConfiguracion.Create: Assigned(AStreamEntrada)');
  Assert(Assigned(AStreamSalida), 'TParserConfiguracion.Create: Assigned(AStreamSalida)');

  FStreamEntrada := AStreamEntrada;
  FStreamSalida := AStreamSalida;

  AssignStream(yyinput, FStreamEntrada);
  AssignStream(yyoutput, FStreamSalida);

  FConfiguracionDatos := nil;
  ConfiguracionDatos := nil;

  FConfiguracionModelo := nil;
  ConfiguracionModelo := nil;

  FConfiguracionProcedimiento := nil;
  ConfiguracionProcedimiento := nil;
 
  MatrizValores := nil;
  IdentificarTokensPalabrasClave := True;
  IndiceColumna := 0;
  IndiceFila := 0;

  FErrorSintaxis := TErrorSintaxis.Create;
  ErrorSintaxis1 := FErrorSintaxis;

  DecimalSeparator := '.';
end { TParserConfiguracion.Create };

constructor TParserConfiguracion.Create(const NombreArchivo: TFileName);
begin { TParserConfiguracion.Create }
  Create(TFileStream.Create(NombreArchivo, fmOpenRead));
end { TParserConfiguracion.Create };

destructor TParserConfiguracion.Destroy;
begin { TParserConfiguracion.Destroy }
  try
    FreeAndNil(FStreamEntrada);
    FreeAndNil(FStreamSalida);
    FreeAndNil(FErrorSintaxis);
  finally
    inherited Destroy;
  end;
end { TParserConfiguracion.Destroy };

function TParserConfiguracion.Parsear: Boolean;
begin { TParserConfiguracion.Parsear }
  Reset(yyinput);
  ReWrite(yyoutput);
  yylineno := 0;
  yyclear;
  Result := yyparse = 0;

  if Result then
  begin
    FreeAndNil(FConfiguracionDatos);
    FreeAndNil(FConfiguracionModelo);
    FreeAndNil(FConfiguracionProcedimiento);
    FConfiguracionDatos := ConfiguracionDatos;
    FConfiguracionModelo := ConfiguracionModelo;
    FConfiguracionProcedimiento := ConfiguracionProcedimiento;
  end
  else
  begin
    FreeAndNil(ConfiguracionDatos);
    FreeAndNil(ConfiguracionModelo);
    FreeAndNil(ConfiguracionProcedimiento);
  end;

  CloseFile(yyinput);
  CloseFile(yyoutput);
end { TParserConfiguracion.Parsear };

end { UnitParserConfiguracion }.
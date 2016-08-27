%{

{**
@abstract(Parser del archivo de configuracion de procedimiento de simulacion
de modelos mixtos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 22, 2005)
Este modulo contiene la definicion del parser de archivos de configuracion
del procedimiento de simulacion de modelos mixtos. El mismo es generado por
tplex/tpyacc, los cuales pueden obtenerse de las fuentes del compilador
FreePascal.
}
 
unit UnitParserConfiguracion;

{$H-}

interface

uses
  Dialogs,
  Classes, SysUtils,
  UaGeneradorNumerosAleatorios, UaVector, UnitArregloEnteros,
  UnitArregloStrings, UnitCalculoSimbolico, UnitColumnaDatos,
  UnitConfiguracionDatos, UnitConfiguracionModelo,
  UnitConfiguracionProcedimiento, UnitEfecto,
  UnitEstructura, UnitGrupoEfectos, UnitOpcion, UnitParametros;

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

{** Parser de los archivos de configuracion del procedimiento de simulacion.
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
  StreamIO;
  
type
  {** Tipo de identificador de columna parseado. }
  TTipoColumnaDatos = (
    {** Columna de valores categoricos. }
    ColumnaCategoricos,
    {** Columna de valores reales. }
    ColumnaReales,
    {** Columna de valores enteros. }
    ColumnaEnteros,
    {** Columna de valores categoricos con vector de datos. }
    ColumnaVectorCategoricos,
    {** Columna de valores reales con vector de datos. }
    ColumnaVectorReales,
    {** Columna de valores enteros con vector de datos. }
    ColumnaVectorEnteros
  );

  {** Record variante asociando un tipo de columna a un parametro extra el
      cual puede ser una lista de identificadores o una expresion. }
  TTuplaTipoColumnaDatosParametro = record
    {** Tipo de columna de datos. }
    Tipo: TTipoColumnaDatos;
    case Integer of     
      1 : ( {** Niveles de la variable de clasificacion. }
            ParametroNiveles : TArregloStrings;
            {** Cantidad de observaciones por cada nivel. }
            ParametroCantidadObservacionesNivel : TArregloEnteros );
      {** Expresion real generadora. }
      2 : (ParametroExpresionReal : TCsExpresion);
      {** Expresion entera generadora. }
      3 : (ParametroExpresionEntera : TCsExpresionEntera);      
      {** Vector de datos categoricos. }
      4 : (ParametroVectorDatosCategoricos: TArregloStrings);
      {** Vector de datos reales. }
      5 : (ParametroVectorDatosReales: TUaVector);
      {** Vector de datos enteros. }
      6 : (ParametroVectorDatosEnteros: TArregloEnteros);
  end { TTuplaTipoColumnaDatosParametro };

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

var
  {** Configuracion de datos parseada. }
  ConfiguracionDatos: TConfiguracionDatos;
  {** Configuracion de modelo parseada. }
  ConfiguracionModelo: TConfiguracionModelo;
  {** Configuracion de procedimiento parseada. }
  ConfiguracionProcedimiento: TConfiguracionProcedimiento;
  {** Le indica al lexer si debe buscar un identificador en la tabla de
      palabras reservadas. } 
  IdentificarTokensPalabrasClave: Boolean;
  {** Error de sintaxis encontrado. }
  ErrorSintaxis1: TErrorSintaxis;

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
%token T_MAS
%token T_MENOS
%token T_BARRA_DERECHA
%token T_CIRCUNFLEJO
%token T_PESOS
%token T_OPERADOR_NEGACION
%token T_OPERADOR_CONJUNCION
%token T_OPERADOR_DISYUNCION
%token T_OPERADOR_MENOR_QUE
%token T_OPERADOR_MENOR_O_IGUAL_QUE
%token T_OPERADOR_IGUAL
%token T_OPERADOR_DISTINTO
%token T_OPERADOR_MAYOR_QUE
%token T_OPERADOR_MAYOR_O_IGUAL_QUE
%token T_SIMBOLO_VARIABLE_UNIFORME
%token T_SIMBOLO_VARIABLE_NORMAL
%token T_SIMBOLO_VARIABLE_EXPONENCIAL
%token T_FUNCION_1SI

%token <Integer> T_ENTERO
%token <Integer> T_SIMBOLO_VARIABLE
%token <Real> T_REAL
%token <ShortString> T_IDENTIFICADOR
%token <ShortString> T_LITERAL_CADENAS
%token <Boolean> T_BOOLEAN

%left T_MAS T_MENOS
%left T_ASTERISCO T_BARRA_DERECHA T_CIRCUNFLEJO
%left T_OPERADOR_DISYUNCION
%left T_OPERADOR_CONJUNCION T_OPERADOR_IMPLICACION
%right T_OPERADOR_NEGACION T_NUMERAL

%token T_DATOS
%token T_COLUMNA
%token T_CATEGORICOS
%token T_REALES
%token T_ENTEROS
%token T_FUNCION_EXPONENCIAL
%token T_FUNCION_LOGARITMO_NATURAL
%token T_FUNCION_LOGARITMO_BASE_2
%token T_FUNCION_LOGARITMO_BASE_10
%token T_FUNCION_LOGARITMO
%token T_FUNCION_SENO
%token T_FUNCION_COSENO
%token T_FUNCION_TANGENTE
%token T_MODELO
%token T_VARIABLES_CLASIFICACION
%token T_VARIABLE_DEPENDIENTE
%token T_EFECTOS_FIJOS
%token T_EFECTOS
%token T_OPCION
%token T_GENERAR_COLUMNAS_NULAS
%token T_INCLUIR_INTERCEPTO
%token T_EFECTOS_ALEATORIOS
%token T_GRUPO
%token T_ERROR
%token T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL
%token T_UNIDAD_EXPERIMENTAL
%token T_ESTRUCTURA
%token T_PARAMETROS
%token T_SIMETRIA_COMPUESTA
%token T_COMPONENTES_VARIANZA
%token T_GENERAL
%token T_DIAGONAL_HETEROGENEA
%token T_FACTOR_ANALYTIC
%token T_FACTOR_ANALYTIC_SIN_DIAGONAL
%token T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR
%token T_BANDEADA
%token T_AUTOREGRESIVA
%token T_CRITERIO_ORDENAMIENTO
%token T_CANTIDAD_OBSERVACIONES
%token T_CANTIDAD_EJECUCIONES
%token T_ARCHIVO_SALIDA
%token T_FUNCION_CARACTERISTICA
%token T_SEMILLA_GENERADOR_NUMEROS_ALEATORIOS
%token T_BETA
%token T_PROCEDIMIENTO
%token T_OPCIONES_PROCEDIMIENTO_AJUSTE
%token T_COMENTAR_VALORES_PARAMETROS
%token T_ARCHIVO_RESUMEN_VEROSIMILITUD
%token T_ARCHIVO_RESUMEN_BETA
%token T_ARCHIVO_RESUMEN_THETA

%type <TConfiguracionDatos> datos
%type <TConfiguracionModelo> modelo
%type <TConfiguracionProcedimiento> procedimiento
%type <TListaColumnasDatos> definiciones_columnas
%type <TColumnaDatos> definicion_columna
%type <TTuplaTipoColumnaDatosParametro> definicion_columna1
%type <TTuplaTipoColumnaDatosParametro> definicion_columna1categoricos
%type <TTuplaTipoColumnaDatosParametro> definicion_columna1reales
%type <TTuplaTipoColumnaDatosParametro> definicion_columna1enteros
%type <TArregloEnteros> definicion_columna1categoricos1
%type <TArregloStrings> arreglo_identificadores
%type <TArregloStrings> arreglo_identificadores1
%type <TStrings> lista_identificadores
%type <TArregloEnteros> arreglo_enteros
%type <TArregloEnteros> arreglo_enteros1
%type <TUaVector> arreglo_reales
%type <TUaVector> arreglo_reales1
%type <TCsExpresion> expresion_real
%type <TCsFuncion> funcion_real
%type <TCsArregloExpresiones> parametros_funcion_real
%type <TCsExpresionBooleana> expresion_booleana
%type <TStrings> variables_clasificacion
%type <ShortString> variable_dependiente
%type <TListaEfectos> efectos
%type <TListaEfectos> lista_efectos
%type <TGrupoEfectos> efectos_fijos
%type <TEfecto> efecto
%type <TEfecto> cruzamiento
%type <TEfecto> anidamiento
%type <TListaEfectosAnidados> lista_efectos_anidados
%type <TListaOpciones> lista_opciones_efectos_fijos
%type <TOpcion> opcion_efectos_fijos
%type <TOpcion> opcion_efectos_fijos1
%type <TTuplaEfectosAleatorios> efectos_aleatorios
%type <TListaGruposEfectos> lista_grupos_efectos_aleatorios
%type <TGrupoEfectos> grupo_efectos_aleatorios
%type <TListaOpciones> lista_opciones_efectos_aleatorios
%type <TOpcion> opcion_efectos_aleatorios
%type <TOpcion> opcion_efectos_aleatorios1
%type <TEstructura> estructura
%type <TListaInformacionParametroIndice> lista_parametros
%type <Integer> orden
%type <TListaOpciones> error1
%type <TListaOpciones> lista_opciones_error
%type <TOpcion> opcion_error
%type <TOpcion> opcion_error1
%type <TListaOpciones> lista_opciones_datos
%type <TOpcion> opcion_datos
%type <TOpcion> opcion_datos1
%type <TListaOpciones> lista_opciones_modelo
%type <TOpcion> opcion_modelo
%type <TOpcion> opcion_modelo1
%type <TObject> opcion_modelo1_beta
%type <TListaOpciones> lista_opciones_procedimiento
%type <TOpcion> opcion_procedimiento
%type <TOpcion> opcion_procedimiento1
%type <TStrings> arreglo_literales_cadenas
%type <TStrings> arreglo_literales_cadenas1
%type <Real> numero

%%

configuracion : datos         { ConfiguracionDatos := $1; }
                modelo        { ConfiguracionModelo := $3;
                                ConfiguracionModelo.Datos := $1; }
                procedimiento { ConfiguracionProcedimiento := $5; }
              ;

datos : T_DATOS
        T_LITERAL_CADENAS
        T_LLAVE_APERTURA
        definiciones_columnas
        lista_opciones_datos
        T_LLAVE_CLAUSURA
        T_PUNTO_Y_COMA        { $$ := TConfiguracionDatos.Create($2, $4, $5); }
      ;

definiciones_columnas : definiciones_columnas definicion_columna { $$ := $1; $$.Add($2); }
                      | definicion_columna                       { $$ := TListaColumnasDatos.Create; $$.Add($1); }
                      ;

definicion_columna : T_COLUMNA
                     T_IDENTIFICADOR
                     T_DOS_PUNTOS
                     definicion_columna1
                     T_PUNTO_Y_COMA      { case $4.Tipo of
                                             ColumnaCategoricos :
                                               $$ := TColumnaDatosCategoricos.Create($2, $4.ParametroNiveles, $4.ParametroCantidadObservacionesNivel);
                                             ColumnaReales :
                                               $$ := TColumnaDatosReales.Create($2, $4.ParametroExpresionReal);
                                             ColumnaEnteros :
                                               $$ := TColumnaDatosEnteros.Create($2, $4.ParametroExpresionEntera);
                                             ColumnaVectorCategoricos :
                                               $$ := TColumnaVectorDatosCategoricos.Create($2, $4.ParametroVectorDatosCategoricos);
                                             ColumnaVectorReales :
                                               $$ := TColumnaVectorDatosReales.Create($2, $4.ParametroVectorDatosReales);
                                             ColumnaVectorEnteros :
                                               $$ := TColumnaVectorDatosEnteros.Create($2, $4.ParametroVectorDatosEnteros);
                                           end; }
                     ;

definicion_columna1 : T_CATEGORICOS definicion_columna1categoricos { $$ := $2; }
                    | T_REALES definicion_columna1reales           { $$ := $2; }
                    | T_ENTEROS definicion_columna1enteros        { $$ := $2; }
                    ;

definicion_columna1categoricos : arreglo_identificadores definicion_columna1categoricos1            { $$.Tipo := ColumnaCategoricos;
                                                                                                      $$.ParametroNiveles := $1;
                                                                                                      $$.ParametroCantidadObservacionesNivel := $2; }
                               | T_OPERADOR_MENOR_QUE arreglo_identificadores1 T_OPERADOR_MAYOR_QUE { $$.Tipo := ColumnaVectorCategoricos;
                                                                                                      $$.ParametroVectorDatosCategoricos := $2; }
                               ;

definicion_columna1categoricos1 :                 { $$ := nil; }
                                | arreglo_enteros { $$ := $1; }
                                ;

definicion_columna1reales : expresion_real                                            { $$.Tipo := ColumnaReales;
                                                                                        $$.ParametroExpresionReal := $1; }
                          | T_OPERADOR_MENOR_QUE arreglo_reales1 T_OPERADOR_MAYOR_QUE { $$.Tipo := ColumnaVectorReales;
                                                                                        $$.ParametroVectorDatosReales := $2; }
                          ;
 
definicion_columna1enteros : expresion_real                                             { $$.Tipo := ColumnaEnteros;
                                                                                          $$.ParametroExpresionEntera := TCsExpresionEntera.Create($1); }
                           | T_OPERADOR_MENOR_QUE arreglo_enteros1 T_OPERADOR_MAYOR_QUE { $$.Tipo := ColumnaVectorEnteros;
                                                                                          $$.ParametroVectorDatosEnteros := $2; }
                           ;

arreglo_identificadores : T_CORCHETE_APERTURA arreglo_identificadores1 T_CORCHETE_CLAUSURA { $$ := $2; }
                        ;

arreglo_identificadores1 : arreglo_identificadores1 T_COMA T_IDENTIFICADOR { $$ := $1; $$.Dimension := $$.Dimension + 1; $$ [$$.Alto] := $3; }
                         | T_IDENTIFICADOR                                 { $$ := TArregloStrings.Create(1); $$ [$$.Bajo] := $1; }
                         ;

lista_identificadores : lista_identificadores T_IDENTIFICADOR { $$ := $1; $$.Add($2); }
                      | T_IDENTIFICADOR                       { $$ := TStringList.Create; $$.Add($1); }
                      ;

arreglo_reales : T_CORCHETE_APERTURA arreglo_reales1 T_CORCHETE_CLAUSURA { $$ := $2; }
               ;

arreglo_reales1 : arreglo_reales1 T_COMA numero { $$ := $1; $$.Dimension := $$.Dimension + 1; $$ [$$.Dimension] := $3; }
                | numero                        { $$ := TUaVector.Create(1); $$ [1] := $1; }
                ;

arreglo_enteros : T_CORCHETE_APERTURA arreglo_enteros1 T_CORCHETE_CLAUSURA { $$ := $2; }
                ;

arreglo_enteros1 : arreglo_enteros1 T_COMA T_ENTERO { $$ := $1; $$.Dimension := $$.Dimension + 1; $$ [$$.Alto] := $3; }
                 | T_ENTERO                         { $$ := TArregloEnteros.Create(1); $$ [$$.Bajo] := $1; }
                 ;

expresion_real : expresion_real T_MAS expresion_real                        { $$ := TCsSuma.Create($1, $3); }
               | expresion_real T_MENOS expresion_real                      { $$ := TCsResta.Create($1, $3); }
               | expresion_real T_ASTERISCO expresion_real                  { $$ := TCsProducto.Create($1, $3); }
               | expresion_real T_BARRA_DERECHA expresion_real              { $$ := TCsDivision.Create($1, $3); }
               | expresion_real T_CIRCUNFLEJO expresion_real                { $$ := TCsPotencia.Create($1, $3); }
               | T_PARENTESIS_APERTURA expresion_real T_PARENTESIS_CLAUSURA { $$ := $2; }
               | funcion_real
                 T_PARENTESIS_APERTURA
                 parametros_funcion_real
                 T_PARENTESIS_CLAUSURA                                      { $$ := TCsAplicacionFuncion.Create($1, $3); }
               | T_FUNCION_1SI
                 T_PARENTESIS_APERTURA
                 expresion_booleana
                 T_PARENTESIS_CLAUSURA                                      { $$ := TCsAplicacionFuncionBooleanos.Create(CsFuncion1Si, $3); }
               | T_SIMBOLO_VARIABLE_UNIFORME
                 T_PARENTESIS_APERTURA
                 numero
                 T_COMA
                 numero
                 T_PARENTESIS_CLAUSURA                                      { $$ := TCsVariableAleatoria.Create(TUaVariableDistribucionUniforme.Create($3, $5)); }
               | T_SIMBOLO_VARIABLE_NORMAL
                 T_PARENTESIS_APERTURA
                 numero
                 T_COMA
                 numero
                 T_PARENTESIS_CLAUSURA                                      { $$ := TCsVariableAleatoria.Create(TUaVariableDistribucionNormal.Create($3, $5)); }
               | T_SIMBOLO_VARIABLE_EXPONENCIAL
                 T_PARENTESIS_APERTURA
                 numero
                 T_PARENTESIS_CLAUSURA                                      { $$ := TCsVariableAleatoria.Create(TUaVariableDistribucionExponencial.Create($3)); }
               | T_SIMBOLO_VARIABLE                                         { $$ := TCsVariable.Create($1); }
               | numero                                                     { $$ := TCsConstante.Create($1); }
               ;

funcion_real : T_FUNCION_EXPONENCIAL       { $$ := CsFuncionExponencial; }
             | T_FUNCION_LOGARITMO_NATURAL { $$ := CsFuncionLogaritmoNatural; }
             | T_FUNCION_LOGARITMO_BASE_2  { $$ := CsFuncionLogaritmoBase2; }
             | T_FUNCION_LOGARITMO_BASE_10 { $$ := CsFuncionLogaritmoBase10; }
             | T_FUNCION_LOGARITMO         { $$ := CsFuncionLogaritmo; }
             | T_FUNCION_SENO              { $$ := CsFuncionSeno; }
             | T_FUNCION_COSENO            { $$ := CsFuncionCoseno; }
             | T_FUNCION_TANGENTE          { $$ := CsFuncionTangente; }
             ;

parametros_funcion_real : parametros_funcion_real T_COMA expresion_real { $$ := $1; $$.Dimension := $$.Dimension + 1; $$ [$$.Alto] := $3; }
                        | expresion_real                                { $$ := TCsArregloExpresiones.Create(1); $$ [0] := $1; }
                        ;

expresion_booleana : T_OPERADOR_NEGACION expresion_booleana                       { $$ := TCsNegacion.Create($2); }
                   | expresion_booleana T_OPERADOR_CONJUNCION expresion_booleana  { $$ := TCsConjuncion.Create($1, $3); }
                   | expresion_booleana T_OPERADOR_DISYUNCION expresion_booleana  { $$ := TCsDisyuncion.Create($1, $3); }
                   | expresion_real T_OPERADOR_MENOR_QUE expresion_real           { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorQue, $1, $3); }
                   | expresion_real T_OPERADOR_MENOR_IGUAL_QUE expresion_real     { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorIgualQue, $1, $3); }
                   | expresion_real T_OPERADOR_IGUAL expresion_real               { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoIgual, $1, $3); }
                   | expresion_real T_OPERADOR_DISTINTO expresion_real            { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoDistinto, $1, $3); }
                   | expresion_real T_OPERADOR_MAYOR_QUE expresion_real           { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorQue, $1, $3); }
                   | expresion_real T_OPERADOR_MAYOR_IGUAL_QUE expresion_real     { $$ := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorIgualQue, $1, $3); }
                   | T_BOOLEAN                                                    { $$ := TCsConstanteBooleana.Create($1); }
                   ;

lista_opciones_datos :                                   { $$ := TListaOpciones.Create; }
                     | lista_opciones_datos opcion_datos { $$ := $1; $$.Add($2); }
                     ;

opcion_datos : T_OPCION opcion_datos1 T_PUNTO_Y_COMA { $$ := $2; }
             ;

opcion_datos1 : T_CANTIDAD_OBSERVACIONES T_ENTERO               { $$ := TOpcionParametroInteger.Create('cantidad_observaciones', $2); }
              | T_FUNCION_CARACTERISTICA expresion_booleana     { $$ := TOpcionParametroTObject.Create('funcion_caracteristica', $2); }
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

lista_opciones_modelo :                                     { $$ := TListaOpciones.Create; }
                      | lista_opciones_modelo opcion_modelo { $$ := $1; $$.Add($2); }
                      ;

opcion_modelo : T_OPCION opcion_modelo1 T_PUNTO_Y_COMA { $$ := $2; }
              ;

opcion_modelo1 : T_BETA opcion_modelo1_beta { $$ := TOpcionParametroTObject.Create('beta', $2); }
               ;

opcion_modelo1_beta : expresion_real { $$ := $1; }
                    | arreglo_reales { $$ := $1; }
                    ;

variables_clasificacion : T_VARIABLES_CLASIFICACION
                          lista_identificadores
                          T_PUNTO_Y_COMA             { $$ := $2; }
                        ;

variable_dependiente : T_VARIABLE_DEPENDIENTE T_IDENTIFICADOR T_PUNTO_Y_COMA { $$ := $2; }
                     ;

efectos_fijos : T_EFECTOS_FIJOS
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

opcion_efectos_fijos : T_OPCION opcion_efectos_fijos1 T_PUNTO_Y_COMA { $$ := $2 }
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

lista_grupos_efectos_aleatorios :                                                          { $$ := TListaGruposEfectos.Create; }
                                | lista_grupos_efectos_aleatorios grupo_efectos_aleatorios { $$ := $1; $$.Add($2); }
                                ;

grupo_efectos_aleatorios : T_GRUPO
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

opcion_efectos_aleatorios1 : T_GENERAR_COLUMNAS_NULAS T_BOOLEAN                                        { $$ := TOpcionParametroBoolean.Create('generar_columnas_nulas', $2); }
                           | T_INCLUIR_INTERCEPTO T_BOOLEAN                                            { $$ := TOpcionParametroBoolean.Create('incluir_intercepto', $2); }
                           | T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL efecto { $$ := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', $2); }
                           | T_UNIDAD_EXPERIMENTAL efecto                       { $$ := TOpcionParametroTEfecto.Create('unidad_experimental', $2); }
                           | T_ESTRUCTURA estructura                                                   { $$ := TOpcionParametroTEstructura.Create('estructura', $2); }
                           | T_PARAMETROS lista_parametros                                             { $$ := TOpcionParametroTObject.Create('parametros', $2); }
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

lista_parametros : lista_parametros numero { $$ := $1; $$.Add(TInformacionParametroIndice.Create($2)); }
                 | numero                  { $$ := TListaInformacionParametroIndice.Create; $$.Add(TInformacionParametroIndice.Create($1)); }
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
              | T_ESTRUCTURA estructura                                                   { $$ := TOpcionParametroTEstructura.Create('estructura', $2); }
              | T_PARAMETROS lista_parametros                                             { $$ := TOpcionParametroTObject.Create('parametros', $2); }
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

opcion_procedimiento1 : T_CANTIDAD_EJECUCIONES T_ENTERO                           { $$ := TOpcionParametroInteger.Create('cantidad_ejecuciones', $2); }
                      | T_ARCHIVO_SALIDA T_LITERAL_CADENAS                        { $$ := TOpcionParametroString.Create('archivo_salida', $2); }
                      | T_SEMILLA_GENERADOR_NUMEROS_ALEATORIOS T_ENTERO           { $$ := TOpcionParametroInteger.Create('semilla_generador_numeros_aleatorios', $2); }
                      | T_OPCIONES_PROCEDIMIENTO_AJUSTE arreglo_literales_cadenas { $$ := TOpcionParametroTObject.Create('opciones_procedimiento_ajuste', $2); }
                      | T_COMENTAR_VALORES_PARAMETROS T_BOOLEAN                   { $$ := TOpcionParametroBoolean.Create('comentar_valores_parametros', $2); }
                      | T_ARCHIVO_RESUMEN_VEROSIMILITUD T_LITERAL_CADENAS         { $$ := TOpcionParametroString.Create('archivo_resumen_verosimilitud', $2); }
                      | T_ARCHIVO_RESUMEN_BETA T_LITERAL_CADENAS                  { $$ := TOpcionParametroString.Create('archivo_resumen_beta', $2); }
                      | T_ARCHIVO_RESUMEN_THETA T_LITERAL_CADENAS                 { $$ := TOpcionParametroString.Create('archivo_resumen_theta', $2); }
                      ;

arreglo_literales_cadenas : T_CORCHETE_APERTURA
                            arreglo_literales_cadenas1
                            T_CORCHETE_CLAUSURA        { $$ := $2; }
                          ;

arreglo_literales_cadenas1 : T_LITERAL_CADENAS                                   { $$ := TStringList.Create; $$.Add($1); }
                           | arreglo_literales_cadenas1 T_COMA T_LITERAL_CADENAS { $$ := $1; $$.Add($3); }
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

  IdentificarTokensPalabrasClave := True;

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
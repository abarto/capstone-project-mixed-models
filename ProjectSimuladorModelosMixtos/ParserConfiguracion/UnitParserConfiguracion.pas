
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)


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

const T_LLAVE_APERTURA = 257;
const T_LLAVE_CLAUSURA = 258;
const T_PARENTESIS_APERTURA = 259;
const T_PARENTESIS_CLAUSURA = 260;
const T_CORCHETE_APERTURA = 261;
const T_CORCHETE_CLAUSURA = 262;
const T_DOS_PUNTOS = 263;
const T_COMA = 264;
const T_PUNTO_Y_COMA = 265;
const T_ASTERISCO = 266;
const T_MAS = 267;
const T_MENOS = 268;
const T_BARRA_DERECHA = 269;
const T_CIRCUNFLEJO = 270;
const T_PESOS = 271;
const T_OPERADOR_NEGACION = 272;
const T_OPERADOR_CONJUNCION = 273;
const T_OPERADOR_DISYUNCION = 274;
const T_OPERADOR_MENOR_QUE = 275;
const T_OPERADOR_MENOR_O_IGUAL_QUE = 276;
const T_OPERADOR_IGUAL = 277;
const T_OPERADOR_DISTINTO = 278;
const T_OPERADOR_MAYOR_QUE = 279;
const T_OPERADOR_MAYOR_O_IGUAL_QUE = 280;
const T_SIMBOLO_VARIABLE_UNIFORME = 281;
const T_SIMBOLO_VARIABLE_NORMAL = 282;
const T_SIMBOLO_VARIABLE_EXPONENCIAL = 283;
const T_FUNCION_1SI = 284;
const T_ENTERO = 285;
const T_SIMBOLO_VARIABLE = 286;
const T_REAL = 287;
const T_IDENTIFICADOR = 288;
const T_LITERAL_CADENAS = 289;
const T_BOOLEAN = 290;
const T_OPERADOR_IMPLICACION = 291;
const T_NUMERAL = 292;
const T_DATOS = 293;
const T_COLUMNA = 294;
const T_CATEGORICOS = 295;
const T_REALES = 296;
const T_ENTEROS = 297;
const T_FUNCION_EXPONENCIAL = 298;
const T_FUNCION_LOGARITMO_NATURAL = 299;
const T_FUNCION_LOGARITMO_BASE_2 = 300;
const T_FUNCION_LOGARITMO_BASE_10 = 301;
const T_FUNCION_LOGARITMO = 302;
const T_FUNCION_SENO = 303;
const T_FUNCION_COSENO = 304;
const T_FUNCION_TANGENTE = 305;
const T_MODELO = 306;
const T_VARIABLES_CLASIFICACION = 307;
const T_VARIABLE_DEPENDIENTE = 308;
const T_EFECTOS_FIJOS = 309;
const T_EFECTOS = 310;
const T_OPCION = 311;
const T_GENERAR_COLUMNAS_NULAS = 312;
const T_INCLUIR_INTERCEPTO = 313;
const T_EFECTOS_ALEATORIOS = 314;
const T_GRUPO = 315;
const T_ERROR = 316;
const T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL = 317;
const T_UNIDAD_EXPERIMENTAL = 318;
const T_ESTRUCTURA = 319;
const T_PARAMETROS = 320;
const T_SIMETRIA_COMPUESTA = 321;
const T_COMPONENTES_VARIANZA = 322;
const T_GENERAL = 323;
const T_DIAGONAL_HETEROGENEA = 324;
const T_FACTOR_ANALYTIC = 325;
const T_FACTOR_ANALYTIC_SIN_DIAGONAL = 326;
const T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR = 327;
const T_BANDEADA = 328;
const T_AUTOREGRESIVA = 329;
const T_CRITERIO_ORDENAMIENTO = 330;
const T_CANTIDAD_OBSERVACIONES = 331;
const T_CANTIDAD_EJECUCIONES = 332;
const T_ARCHIVO_SALIDA = 333;
const T_FUNCION_CARACTERISTICA = 334;
const T_SEMILLA_GENERADOR_NUMEROS_ALEATORIOS = 335;
const T_BETA = 336;
const T_PROCEDIMIENTO = 337;
const T_OPCIONES_PROCEDIMIENTO_AJUSTE = 338;
const T_COMENTAR_VALORES_PARAMETROS = 339;
const T_ARCHIVO_RESUMEN_VEROSIMILITUD = 340;
const T_ARCHIVO_RESUMEN_BETA = 341;
const T_ARCHIVO_RESUMEN_THETA = 342;

type YYSType = record case Integer of
                 1 : ( yyBoolean : Boolean );
                 2 : ( yyInteger : Integer );
                 3 : ( yyReal : Real );
                 4 : ( yyShortString : ShortString );
                 5 : ( yyTArregloEnteros : TArregloEnteros );
                 6 : ( yyTArregloStrings : TArregloStrings );
                 7 : ( yyTColumnaDatos : TColumnaDatos );
                 8 : ( yyTConfiguracionDatos : TConfiguracionDatos );
                 9 : ( yyTConfiguracionModelo : TConfiguracionModelo );
                10 : ( yyTConfiguracionProcedimiento : TConfiguracionProcedimiento );
                11 : ( yyTCsArregloExpresiones : TCsArregloExpresiones );
                12 : ( yyTCsExpresion : TCsExpresion );
                13 : ( yyTCsExpresionBooleana : TCsExpresionBooleana );
                14 : ( yyTCsFuncion : TCsFuncion );
                15 : ( yyTEfecto : TEfecto );
                16 : ( yyTEstructura : TEstructura );
                17 : ( yyTGrupoEfectos : TGrupoEfectos );
                18 : ( yyTListaColumnasDatos : TListaColumnasDatos );
                19 : ( yyTListaEfectos : TListaEfectos );
                20 : ( yyTListaEfectosAnidados : TListaEfectosAnidados );
                21 : ( yyTListaGruposEfectos : TListaGruposEfectos );
                22 : ( yyTListaInformacionParametroIndice : TListaInformacionParametroIndice );
                23 : ( yyTListaOpciones : TListaOpciones );
                24 : ( yyTObject : TObject );
                25 : ( yyTOpcion : TOpcion );
                26 : ( yyTStrings : TStrings );
                27 : ( yyTTuplaEfectosAleatorios : TTuplaEfectosAleatorios );
                28 : ( yyTTuplaTipoColumnaDatosParametro : TTuplaTipoColumnaDatosParametro );
                29 : ( yyTUaVector : TUaVector );
               end(*YYSType*);

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         ConfiguracionDatos := yyv[yysp-0].yyTConfiguracionDatos; 
       end;
   2 : begin
         ConfiguracionModelo := yyv[yysp-0].yyTConfiguracionModelo;
         ConfiguracionModelo.Datos := yyv[yysp-2].yyTConfiguracionDatos; 
       end;
   3 : begin
         ConfiguracionProcedimiento := yyv[yysp-0].yyTConfiguracionProcedimiento; 
       end;
   4 : begin
         yyval.yyTConfiguracionDatos := TConfiguracionDatos.Create(yyv[yysp-5].yyShortString, yyv[yysp-3].yyTListaColumnasDatos, yyv[yysp-2].yyTListaOpciones); 
       end;
   5 : begin
         yyval.yyTListaColumnasDatos := yyv[yysp-1].yyTListaColumnasDatos; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   6 : begin
         yyval.yyTListaColumnasDatos := TListaColumnasDatos.Create; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   7 : begin
         case yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.Tipo of
         ColumnaCategoricos :
         yyval.yyTColumnaDatos := TColumnaDatosCategoricos.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroNiveles, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroCantidadObservacionesNivel);
         ColumnaReales :
         yyval.yyTColumnaDatos := TColumnaDatosReales.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroExpresionReal);
         ColumnaEnteros :
         yyval.yyTColumnaDatos := TColumnaDatosEnteros.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroExpresionEntera);
         ColumnaVectorCategoricos :
         yyval.yyTColumnaDatos := TColumnaVectorDatosCategoricos.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosCategoricos);
         ColumnaVectorReales :
         yyval.yyTColumnaDatos := TColumnaVectorDatosReales.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosReales);
         ColumnaVectorEnteros :
         yyval.yyTColumnaDatos := TColumnaVectorDatosEnteros.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosEnteros);
         end; 
       end;
   8 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro := yyv[yysp-0].yyTTuplaTipoColumnaDatosParametro; 
       end;
   9 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro := yyv[yysp-0].yyTTuplaTipoColumnaDatosParametro; 
       end;
  10 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro := yyv[yysp-0].yyTTuplaTipoColumnaDatosParametro; 
       end;
  11 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaCategoricos;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroNiveles := yyv[yysp-1].yyTArregloStrings;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroCantidadObservacionesNivel := yyv[yysp-0].yyTArregloEnteros; 
       end;
  12 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaVectorCategoricos;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosCategoricos := yyv[yysp-1].yyTArregloStrings; 
       end;
  13 : begin
         yyval.yyTArregloEnteros := nil; 
       end;
  14 : begin
         yyval.yyTArregloEnteros := yyv[yysp-0].yyTArregloEnteros; 
       end;
  15 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaReales;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroExpresionReal := yyv[yysp-0].yyTCsExpresion; 
       end;
  16 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaVectorReales;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosReales := yyv[yysp-1].yyTUaVector; 
       end;
  17 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaEnteros;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroExpresionEntera := TCsExpresionEntera.Create(yyv[yysp-0].yyTCsExpresion); 
       end;
  18 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaVectorEnteros;
         yyval.yyTTuplaTipoColumnaDatosParametro.ParametroVectorDatosEnteros := yyv[yysp-1].yyTArregloEnteros; 
       end;
  19 : begin
         yyval.yyTArregloStrings := yyv[yysp-1].yyTArregloStrings; 
       end;
  20 : begin
         yyval.yyTArregloStrings := yyv[yysp-2].yyTArregloStrings; yyval.yyTArregloStrings.Dimension := yyval.yyTArregloStrings.Dimension + 1; yyval.yyTArregloStrings [yyval.yyTArregloStrings.Alto] := yyv[yysp-0].yyShortString; 
       end;
  21 : begin
         yyval.yyTArregloStrings := TArregloStrings.Create(1); yyval.yyTArregloStrings [yyval.yyTArregloStrings.Bajo] := yyv[yysp-0].yyShortString; 
       end;
  22 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  23 : begin
         yyval.yyTStrings := TStringList.Create; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  24 : begin
         yyval.yyTUaVector := yyv[yysp-1].yyTUaVector; 
       end;
  25 : begin
         yyval.yyTUaVector := yyv[yysp-2].yyTUaVector; yyval.yyTUaVector.Dimension := yyval.yyTUaVector.Dimension + 1; yyval.yyTUaVector [yyval.yyTUaVector.Dimension] := yyv[yysp-0].yyReal; 
       end;
  26 : begin
         yyval.yyTUaVector := TUaVector.Create(1); yyval.yyTUaVector [1] := yyv[yysp-0].yyReal; 
       end;
  27 : begin
         yyval.yyTArregloEnteros := yyv[yysp-1].yyTArregloEnteros; 
       end;
  28 : begin
         yyval.yyTArregloEnteros := yyv[yysp-2].yyTArregloEnteros; yyval.yyTArregloEnteros.Dimension := yyval.yyTArregloEnteros.Dimension + 1; yyval.yyTArregloEnteros [yyval.yyTArregloEnteros.Alto] := yyv[yysp-0].yyInteger; 
       end;
  29 : begin
         yyval.yyTArregloEnteros := TArregloEnteros.Create(1); yyval.yyTArregloEnteros [yyval.yyTArregloEnteros.Bajo] := yyv[yysp-0].yyInteger; 
       end;
  30 : begin
         yyval.yyTCsExpresion := TCsSuma.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  31 : begin
         yyval.yyTCsExpresion := TCsResta.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  32 : begin
         yyval.yyTCsExpresion := TCsProducto.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  33 : begin
         yyval.yyTCsExpresion := TCsDivision.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  34 : begin
         yyval.yyTCsExpresion := TCsPotencia.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  35 : begin
         yyval.yyTCsExpresion := yyv[yysp-1].yyTCsExpresion; 
       end;
  36 : begin
         yyval.yyTCsExpresion := TCsAplicacionFuncion.Create(yyv[yysp-3].yyTCsFuncion, yyv[yysp-1].yyTCsArregloExpresiones); 
       end;
  37 : begin
         yyval.yyTCsExpresion := TCsAplicacionFuncionBooleanos.Create(CsFuncion1Si, yyv[yysp-1].yyTCsExpresionBooleana); 
       end;
  38 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionUniforme.Create(yyv[yysp-3].yyReal, yyv[yysp-1].yyReal)); 
       end;
  39 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionNormal.Create(yyv[yysp-3].yyReal, yyv[yysp-1].yyReal)); 
       end;
  40 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionExponencial.Create(yyv[yysp-1].yyReal)); 
       end;
  41 : begin
         yyval.yyTCsExpresion := TCsVariable.Create(yyv[yysp-0].yyInteger); 
       end;
  42 : begin
         yyval.yyTCsExpresion := TCsConstante.Create(yyv[yysp-0].yyReal); 
       end;
  43 : begin
         yyval.yyTCsFuncion := CsFuncionExponencial; 
       end;
  44 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoNatural; 
       end;
  45 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoBase2; 
       end;
  46 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoBase10; 
       end;
  47 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmo; 
       end;
  48 : begin
         yyval.yyTCsFuncion := CsFuncionSeno; 
       end;
  49 : begin
         yyval.yyTCsFuncion := CsFuncionCoseno; 
       end;
  50 : begin
         yyval.yyTCsFuncion := CsFuncionTangente; 
       end;
  51 : begin
         yyval.yyTCsArregloExpresiones := yyv[yysp-2].yyTCsArregloExpresiones; yyval.yyTCsArregloExpresiones.Dimension := yyval.yyTCsArregloExpresiones.Dimension + 1; yyval.yyTCsArregloExpresiones [yyval.yyTCsArregloExpresiones.Alto] := yyv[yysp-0].yyTCsExpresion; 
       end;
  52 : begin
         yyval.yyTCsArregloExpresiones := TCsArregloExpresiones.Create(1); yyval.yyTCsArregloExpresiones [0] := yyv[yysp-0].yyTCsExpresion; 
       end;
  53 : begin
         yyval.yyTCsExpresionBooleana := TCsNegacion.Create(yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  54 : begin
         yyval.yyTCsExpresionBooleana := TCsConjuncion.Create(yyv[yysp-2].yyTCsExpresionBooleana, yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  55 : begin
         yyval.yyTCsExpresionBooleana := TCsDisyuncion.Create(yyv[yysp-2].yyTCsExpresionBooleana, yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  56 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  57 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorIgualQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  58 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoIgual, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  59 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoDistinto, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  60 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  61 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorIgualQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  62 : begin
         yyval.yyTCsExpresionBooleana := TCsConstanteBooleana.Create(yyv[yysp-0].yyBoolean); 
       end;
  63 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  64 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  65 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  66 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_observaciones', yyv[yysp-0].yyInteger); 
       end;
  67 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('funcion_caracteristica', yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  68 : begin
         yyval.yyTConfiguracionModelo := TConfiguracionModelo.Create(yyv[yysp-8].yyShortString,
         yyv[yysp-6].yyTStrings,
         yyv[yysp-5].yyShortString,
         yyv[yysp-4].yyTGrupoEfectos,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.EfectosAleatorios,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.Error,
         yyv[yysp-2].yyTListaOpciones); 
       end;
  69 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  70 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  71 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  72 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('beta', yyv[yysp-0].yyTObject); 
       end;
  73 : begin
         yyval.yyTObject := yyv[yysp-0].yyTCsExpresion; 
       end;
  74 : begin
         yyval.yyTObject := yyv[yysp-0].yyTUaVector; 
       end;
  75 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; 
       end;
  76 : begin
         yyval.yyShortString := yyv[yysp-1].yyShortString; 
       end;
  77 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaEfectos, yyv[yysp-2].yyTListaOpciones); 
       end;
  78 : begin
         yyval.yyTListaEfectos := TListaEfectos.Create; 
       end;
  79 : begin
         yyval.yyTListaEfectos := yyv[yysp-1].yyTListaEfectos; 
       end;
  80 : begin
         yyval.yyTListaEfectos := yyv[yysp-1].yyTListaEfectos; yyval.yyTListaEfectos.Add(yyv[yysp-0].yyTEfecto); 
       end;
  81 : begin
         yyval.yyTListaEfectos := TListaEfectos.Create; yyval.yyTListaEfectos.Add(yyv[yysp-0].yyTEfecto); 
       end;
  82 : begin
         yyval.yyTEfecto := TIntercepto.Create; 
       end;
  83 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  84 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  85 : begin
         yyval.yyTEfecto := TCruzamiento.Create(TEfectoPrincipal.Create(yyv[yysp-2].yyShortString), yyv[yysp-0].yyTEfecto); 
       end;
  86 : begin
         yyval.yyTEfecto := TEfectoPrincipal.Create(yyv[yysp-0].yyShortString); 
       end;
  87 : begin
         yyval.yyTEfecto := TAnidamiento.Create(yyv[yysp-3].yyTEfecto, yyv[yysp-1].yyTListaEfectosAnidados); 
       end;
  88 : begin
         yyval.yyTListaEfectosAnidados := yyv[yysp-1].yyTListaEfectosAnidados; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  89 : begin
         yyval.yyTListaEfectosAnidados := TListaEfectosAnidados.Create; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  90 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  91 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  92 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion 
       end;
  93 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
  94 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
       end;
  95 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.Error := nil; 
       end;
  96 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := TGrupoEfectos.Create(yyv[yysp-4].yyTListaEfectos, yyv[yysp-3].yyTListaOpciones);
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := yyv[yysp-5].yyTListaGruposEfectos;
         yyval.yyTTuplaEfectosAleatorios.Error := yyv[yysp-2].yyTListaOpciones; 
       end;
  97 : begin
         yyval.yyTListaGruposEfectos := TListaGruposEfectos.Create; 
       end;
  98 : begin
         yyval.yyTListaGruposEfectos := yyv[yysp-1].yyTListaGruposEfectos; yyval.yyTListaGruposEfectos.Add(yyv[yysp-0].yyTGrupoEfectos); 
       end;
  99 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaEfectos, yyv[yysp-2].yyTListaOpciones); 
       end;
 100 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 101 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
 102 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
 103 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
 104 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
       end;
 105 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 106 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 107 : begin
         yyval.yyTOpcion := TOpcionParametroTEstructura.Create('estructura', yyv[yysp-0].yyTEstructura); 
       end;
 108 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('parametros', yyv[yysp-0].yyTListaInformacionParametroIndice); 
       end;
 109 : begin
         yyval.yyTEstructura := TEstructura.Create('simetria_compuesta'); 
       end;
 110 : begin
         yyval.yyTEstructura := TEstructura.Create('componentes_varianza'); 
       end;
 111 : begin
         yyval.yyTEstructura := TEstructura.Create('general'); 
       end;
 112 : begin
         yyval.yyTEstructura := TEstructura.Create('diagonal_heterogenea'); 
       end;
 113 : begin
         yyval.yyTEstructura := TEstructura.Create('autoregresiva'); 
       end;
 114 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic', yyv[yysp-0].yyInteger); 
       end;
 115 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_sin_diagonal', yyv[yysp-0].yyInteger); 
       end;
 116 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_diagonal_escalar', yyv[yysp-0].yyInteger); 
       end;
 117 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('bandeada', yyv[yysp-0].yyInteger); 
       end;
 118 : begin
         yyval.yyInteger := 1; 
       end;
 119 : begin
         yyval.yyInteger := yyv[yysp-1].yyInteger; 
       end;
 120 : begin
         yyval.yyTListaInformacionParametroIndice := yyv[yysp-1].yyTListaInformacionParametroIndice; yyval.yyTListaInformacionParametroIndice.Add(TInformacionParametroIndice.Create(yyv[yysp-0].yyReal)); 
       end;
 121 : begin
         yyval.yyTListaInformacionParametroIndice := TListaInformacionParametroIndice.Create; yyval.yyTListaInformacionParametroIndice.Add(TInformacionParametroIndice.Create(yyv[yysp-0].yyReal)); 
       end;
 122 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 123 : begin
         yyval.yyTListaOpciones := yyv[yysp-2].yyTListaOpciones; 
       end;
 124 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 125 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
 126 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
 127 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_ordenamiento', yyv[yysp-0].yyTEfecto); 
       end;
 128 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 129 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 130 : begin
         yyval.yyTOpcion := TOpcionParametroTEstructura.Create('estructura', yyv[yysp-0].yyTEstructura); 
       end;
 131 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('parametros', yyv[yysp-0].yyTListaInformacionParametroIndice); 
       end;
 132 : begin
         yyval.yyTConfiguracionProcedimiento := TConfiguracionProcedimiento.Create(yyv[yysp-2].yyTListaOpciones); 
       end;
 133 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 134 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
 135 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
 136 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_ejecuciones', yyv[yysp-0].yyInteger); 
       end;
 137 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_salida', yyv[yysp-0].yyShortString); 
       end;
 138 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('semilla_generador_numeros_aleatorios', yyv[yysp-0].yyInteger); 
       end;
 139 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('opciones_procedimiento_ajuste', yyv[yysp-0].yyTStrings); 
       end;
 140 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('comentar_valores_parametros', yyv[yysp-0].yyBoolean); 
       end;
 141 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_verosimilitud', yyv[yysp-0].yyShortString); 
       end;
 142 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_beta', yyv[yysp-0].yyShortString); 
       end;
 143 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_theta', yyv[yysp-0].yyShortString); 
       end;
 144 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; 
       end;
 145 : begin
         yyval.yyTStrings := TStringList.Create; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
 146 : begin
         yyval.yyTStrings := yyv[yysp-2].yyTStrings; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
 147 : begin
         yyval.yyReal := yyv[yysp-0].yyInteger; 
       end;
 148 : begin
         yyval.yyReal := yyv[yysp-0].yyReal; 
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 726;
yyngotos  = 182;
yynstates = 294;
yynrules  = 148;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 293; act: 3 ),
{ 1: }
  ( sym: 0; act: 0 ),
{ 2: }
{ 3: }
  ( sym: 289; act: 5 ),
{ 4: }
  ( sym: 306; act: 7 ),
{ 5: }
  ( sym: 257; act: 8 ),
{ 6: }
{ 7: }
  ( sym: 289; act: 10 ),
{ 8: }
  ( sym: 294; act: 13 ),
{ 9: }
  ( sym: 337; act: 15 ),
{ 10: }
  ( sym: 257; act: 16 ),
{ 11: }
{ 12: }
  ( sym: 294; act: 13 ),
  ( sym: 258; act: -63 ),
  ( sym: 311; act: -63 ),
{ 13: }
  ( sym: 288; act: 19 ),
{ 14: }
{ 15: }
  ( sym: 257; act: 20 ),
{ 16: }
  ( sym: 307; act: 22 ),
{ 17: }
  ( sym: 258; act: 24 ),
  ( sym: 311; act: 25 ),
{ 18: }
{ 19: }
  ( sym: 263; act: 26 ),
{ 20: }
{ 21: }
  ( sym: 308; act: 29 ),
{ 22: }
  ( sym: 288; act: 31 ),
{ 23: }
{ 24: }
  ( sym: 265; act: 32 ),
{ 25: }
  ( sym: 331; act: 34 ),
  ( sym: 334; act: 35 ),
{ 26: }
  ( sym: 295; act: 37 ),
  ( sym: 296; act: 38 ),
  ( sym: 297; act: 39 ),
{ 27: }
  ( sym: 258; act: 41 ),
  ( sym: 311; act: 42 ),
{ 28: }
  ( sym: 309; act: 44 ),
{ 29: }
  ( sym: 288; act: 45 ),
{ 30: }
  ( sym: 265; act: 46 ),
  ( sym: 288; act: 47 ),
{ 31: }
{ 32: }
{ 33: }
  ( sym: 265; act: 48 ),
{ 34: }
  ( sym: 285; act: 49 ),
{ 35: }
  ( sym: 259; act: 54 ),
  ( sym: 272; act: 55 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 36: }
  ( sym: 265; act: 72 ),
{ 37: }
  ( sym: 261; act: 75 ),
  ( sym: 275; act: 76 ),
{ 38: }
  ( sym: 259; act: 54 ),
  ( sym: 275; act: 79 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 39: }
  ( sym: 259; act: 54 ),
  ( sym: 275; act: 82 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 40: }
{ 41: }
  ( sym: 265; act: 83 ),
{ 42: }
  ( sym: 332; act: 85 ),
  ( sym: 333; act: 86 ),
  ( sym: 335; act: 87 ),
  ( sym: 338; act: 88 ),
  ( sym: 339; act: 89 ),
  ( sym: 340; act: 90 ),
  ( sym: 341; act: 91 ),
  ( sym: 342; act: 92 ),
{ 43: }
  ( sym: 314; act: 94 ),
  ( sym: 258; act: -95 ),
  ( sym: 311; act: -95 ),
{ 44: }
  ( sym: 257; act: 95 ),
{ 45: }
  ( sym: 265; act: 96 ),
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: 273; act: 97 ),
  ( sym: 274; act: 98 ),
  ( sym: 265; act: -67 ),
{ 52: }
  ( sym: 259; act: 99 ),
{ 53: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 275; act: 107 ),
  ( sym: 277; act: 108 ),
  ( sym: 278; act: 109 ),
  ( sym: 279; act: 110 ),
{ 54: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 55: }
  ( sym: 259; act: 54 ),
  ( sym: 272; act: 55 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 56: }
  ( sym: 259; act: 113 ),
{ 57: }
  ( sym: 259; act: 114 ),
{ 58: }
  ( sym: 259; act: 115 ),
{ 59: }
  ( sym: 259; act: 116 ),
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: 261; act: 119 ),
  ( sym: 265; act: -13 ),
{ 74: }
{ 75: }
  ( sym: 288; act: 121 ),
{ 76: }
  ( sym: 288; act: 121 ),
{ 77: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 265; act: -15 ),
{ 78: }
{ 79: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 80: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 265; act: -17 ),
{ 81: }
{ 82: }
  ( sym: 285; act: 126 ),
{ 83: }
{ 84: }
  ( sym: 265; act: 127 ),
{ 85: }
  ( sym: 285; act: 128 ),
{ 86: }
  ( sym: 289; act: 129 ),
{ 87: }
  ( sym: 285; act: 130 ),
{ 88: }
  ( sym: 261; act: 132 ),
{ 89: }
  ( sym: 290; act: 133 ),
{ 90: }
  ( sym: 289; act: 134 ),
{ 91: }
  ( sym: 289; act: 135 ),
{ 92: }
  ( sym: 289; act: 136 ),
{ 93: }
{ 94: }
  ( sym: 257; act: 138 ),
{ 95: }
  ( sym: 310; act: 140 ),
  ( sym: 258; act: -78 ),
  ( sym: 311; act: -78 ),
{ 96: }
{ 97: }
  ( sym: 259; act: 54 ),
  ( sym: 272; act: 55 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 98: }
  ( sym: 259; act: 54 ),
  ( sym: 272; act: 55 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 99: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 100: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 101: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 102: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 103: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 104: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 105: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 106: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 107: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 108: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 109: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 110: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 111: }
  ( sym: 260; act: 156 ),
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
{ 112: }
{ 113: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 114: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 115: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 116: }
  ( sym: 259; act: 54 ),
  ( sym: 272; act: 55 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 290; act: 63 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 117: }
{ 118: }
{ 119: }
  ( sym: 285; act: 126 ),
{ 120: }
  ( sym: 262; act: 162 ),
  ( sym: 264; act: 163 ),
{ 121: }
{ 122: }
  ( sym: 264; act: 163 ),
  ( sym: 279; act: 164 ),
{ 123: }
{ 124: }
  ( sym: 264; act: 165 ),
  ( sym: 279; act: 166 ),
{ 125: }
  ( sym: 264; act: 167 ),
  ( sym: 279; act: 168 ),
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  ( sym: 289; act: 170 ),
{ 133: }
{ 134: }
{ 135: }
{ 136: }
{ 137: }
  ( sym: 258; act: 172 ),
  ( sym: 311; act: 173 ),
{ 138: }
{ 139: }
{ 140: }
  ( sym: 288; act: 181 ),
{ 141: }
{ 142: }
  ( sym: 273; act: 97 ),
  ( sym: 260; act: -55 ),
  ( sym: 265; act: -55 ),
  ( sym: 274; act: -55 ),
{ 143: }
  ( sym: 260; act: 182 ),
  ( sym: 264; act: 183 ),
{ 144: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -52 ),
  ( sym: 264; act: -52 ),
{ 145: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -61 ),
  ( sym: 265; act: -61 ),
  ( sym: 273; act: -61 ),
  ( sym: 274; act: -61 ),
{ 146: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -57 ),
  ( sym: 265; act: -57 ),
  ( sym: 273; act: -57 ),
  ( sym: 274; act: -57 ),
{ 147: }
{ 148: }
  ( sym: 266; act: 102 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -30 ),
  ( sym: 264; act: -30 ),
  ( sym: 265; act: -30 ),
  ( sym: 267; act: -30 ),
  ( sym: 268; act: -30 ),
  ( sym: 273; act: -30 ),
  ( sym: 274; act: -30 ),
  ( sym: 275; act: -30 ),
  ( sym: 277; act: -30 ),
  ( sym: 278; act: -30 ),
  ( sym: 279; act: -30 ),
{ 149: }
  ( sym: 266; act: 102 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -31 ),
  ( sym: 264; act: -31 ),
  ( sym: 265; act: -31 ),
  ( sym: 267; act: -31 ),
  ( sym: 268; act: -31 ),
  ( sym: 273; act: -31 ),
  ( sym: 274; act: -31 ),
  ( sym: 275; act: -31 ),
  ( sym: 277; act: -31 ),
  ( sym: 278; act: -31 ),
  ( sym: 279; act: -31 ),
{ 150: }
{ 151: }
{ 152: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -56 ),
  ( sym: 265; act: -56 ),
  ( sym: 273; act: -56 ),
  ( sym: 274; act: -56 ),
{ 153: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -58 ),
  ( sym: 265; act: -58 ),
  ( sym: 273; act: -58 ),
  ( sym: 274; act: -58 ),
{ 154: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -59 ),
  ( sym: 265; act: -59 ),
  ( sym: 273; act: -59 ),
  ( sym: 274; act: -59 ),
{ 155: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -60 ),
  ( sym: 265; act: -60 ),
  ( sym: 273; act: -60 ),
  ( sym: 274; act: -60 ),
{ 156: }
{ 157: }
  ( sym: 264; act: 184 ),
{ 158: }
  ( sym: 264; act: 185 ),
{ 159: }
  ( sym: 260; act: 186 ),
{ 160: }
  ( sym: 260; act: 187 ),
  ( sym: 273; act: 97 ),
  ( sym: 274; act: 98 ),
{ 161: }
  ( sym: 262; act: 188 ),
  ( sym: 264; act: 167 ),
{ 162: }
{ 163: }
  ( sym: 288; act: 189 ),
{ 164: }
{ 165: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 166: }
{ 167: }
  ( sym: 285; act: 191 ),
{ 168: }
{ 169: }
  ( sym: 262; act: 192 ),
  ( sym: 264; act: 193 ),
{ 170: }
{ 171: }
{ 172: }
  ( sym: 265; act: 194 ),
{ 173: }
  ( sym: 336; act: 196 ),
{ 174: }
  ( sym: 310; act: 140 ),
  ( sym: 315; act: 199 ),
  ( sym: 258; act: -78 ),
  ( sym: 311; act: -78 ),
  ( sym: 316; act: -78 ),
{ 175: }
  ( sym: 258; act: 201 ),
  ( sym: 311; act: 202 ),
{ 176: }
{ 177: }
{ 178: }
  ( sym: 259; act: 203 ),
  ( sym: 265; act: -83 ),
  ( sym: 288; act: -83 ),
{ 179: }
{ 180: }
  ( sym: 265; act: 205 ),
  ( sym: 288; act: 181 ),
{ 181: }
  ( sym: 266; act: 206 ),
  ( sym: 259; act: -86 ),
  ( sym: 265; act: -86 ),
  ( sym: 288; act: -86 ),
{ 182: }
{ 183: }
  ( sym: 259; act: 54 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 184: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 185: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
  ( sym: 289; act: 210 ),
{ 194: }
{ 195: }
  ( sym: 265; act: 211 ),
{ 196: }
  ( sym: 259; act: 54 ),
  ( sym: 261; act: 215 ),
  ( sym: 281; act: 56 ),
  ( sym: 282; act: 57 ),
  ( sym: 283; act: 58 ),
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
  ( sym: 287; act: 62 ),
  ( sym: 298; act: 64 ),
  ( sym: 299; act: 65 ),
  ( sym: 300; act: 66 ),
  ( sym: 301; act: 67 ),
  ( sym: 302; act: 68 ),
  ( sym: 303; act: 69 ),
  ( sym: 304; act: 70 ),
  ( sym: 305; act: 71 ),
{ 197: }
{ 198: }
{ 199: }
  ( sym: 257; act: 217 ),
{ 200: }
{ 201: }
  ( sym: 265; act: 218 ),
{ 202: }
  ( sym: 312; act: 220 ),
  ( sym: 313; act: 221 ),
{ 203: }
  ( sym: 288; act: 223 ),
{ 204: }
{ 205: }
{ 206: }
  ( sym: 288; act: 181 ),
{ 207: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 260; act: -51 ),
  ( sym: 264; act: -51 ),
{ 208: }
  ( sym: 260; act: 225 ),
{ 209: }
  ( sym: 260; act: 226 ),
{ 210: }
{ 211: }
{ 212: }
{ 213: }
  ( sym: 266; act: 102 ),
  ( sym: 267; act: 103 ),
  ( sym: 268; act: 104 ),
  ( sym: 269; act: 105 ),
  ( sym: 270; act: 106 ),
  ( sym: 265; act: -73 ),
{ 214: }
{ 215: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 216: }
  ( sym: 311; act: 230 ),
  ( sym: 316; act: 231 ),
  ( sym: 258; act: -122 ),
{ 217: }
  ( sym: 310; act: 140 ),
  ( sym: 258; act: -78 ),
  ( sym: 311; act: -78 ),
{ 218: }
{ 219: }
  ( sym: 265; act: 233 ),
{ 220: }
  ( sym: 290; act: 234 ),
{ 221: }
  ( sym: 290; act: 235 ),
{ 222: }
  ( sym: 260; act: 236 ),
  ( sym: 288; act: 237 ),
{ 223: }
{ 224: }
{ 225: }
{ 226: }
{ 227: }
  ( sym: 262; act: 238 ),
  ( sym: 264; act: 165 ),
{ 228: }
  ( sym: 258; act: 239 ),
{ 229: }
{ 230: }
  ( sym: 312; act: 241 ),
  ( sym: 313; act: 242 ),
  ( sym: 317; act: 243 ),
  ( sym: 318; act: 244 ),
  ( sym: 319; act: 245 ),
  ( sym: 320; act: 246 ),
{ 231: }
  ( sym: 257; act: 247 ),
{ 232: }
{ 233: }
{ 234: }
{ 235: }
{ 236: }
{ 237: }
{ 238: }
{ 239: }
  ( sym: 265; act: 249 ),
{ 240: }
  ( sym: 265; act: 250 ),
{ 241: }
  ( sym: 290; act: 251 ),
{ 242: }
  ( sym: 290; act: 252 ),
{ 243: }
  ( sym: 288; act: 181 ),
{ 244: }
  ( sym: 288; act: 181 ),
{ 245: }
  ( sym: 321; act: 256 ),
  ( sym: 322; act: 257 ),
  ( sym: 323; act: 258 ),
  ( sym: 324; act: 259 ),
  ( sym: 325; act: 260 ),
  ( sym: 326; act: 261 ),
  ( sym: 327; act: 262 ),
  ( sym: 328; act: 263 ),
  ( sym: 329; act: 264 ),
{ 246: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 247: }
{ 248: }
  ( sym: 258; act: 268 ),
  ( sym: 311; act: 230 ),
{ 249: }
{ 250: }
{ 251: }
{ 252: }
{ 253: }
{ 254: }
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
  ( sym: 259; act: 270 ),
  ( sym: 265; act: -118 ),
{ 261: }
  ( sym: 259; act: 270 ),
  ( sym: 265; act: -118 ),
{ 262: }
  ( sym: 259; act: 270 ),
  ( sym: 265; act: -118 ),
{ 263: }
  ( sym: 259; act: 270 ),
  ( sym: 265; act: -118 ),
{ 264: }
{ 265: }
{ 266: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
  ( sym: 265; act: -108 ),
{ 267: }
  ( sym: 258; act: 276 ),
  ( sym: 311; act: 277 ),
{ 268: }
  ( sym: 265; act: 278 ),
{ 269: }
{ 270: }
  ( sym: 285; act: 279 ),
{ 271: }
{ 272: }
{ 273: }
{ 274: }
{ 275: }
{ 276: }
  ( sym: 265; act: 280 ),
{ 277: }
  ( sym: 317; act: 282 ),
  ( sym: 318; act: 283 ),
  ( sym: 319; act: 284 ),
  ( sym: 320; act: 285 ),
  ( sym: 330; act: 286 ),
{ 278: }
{ 279: }
  ( sym: 260; act: 287 ),
{ 280: }
{ 281: }
  ( sym: 265; act: 288 ),
{ 282: }
  ( sym: 288; act: 181 ),
{ 283: }
  ( sym: 288; act: 181 ),
{ 284: }
  ( sym: 321; act: 256 ),
  ( sym: 322; act: 257 ),
  ( sym: 323; act: 258 ),
  ( sym: 324; act: 259 ),
  ( sym: 325; act: 260 ),
  ( sym: 326; act: 261 ),
  ( sym: 327; act: 262 ),
  ( sym: 328; act: 263 ),
  ( sym: 329; act: 264 ),
{ 285: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
{ 286: }
  ( sym: 288; act: 181 ),
{ 287: }
{ 288: }
{ 289: }
{ 290: }
{ 291: }
{ 292: }
  ( sym: 285; act: 60 ),
  ( sym: 287; act: 62 ),
  ( sym: 265; act: -131 )
{ 293: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -61; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
{ 2: }
  ( sym: -62; act: 4 ),
{ 3: }
{ 4: }
  ( sym: -3; act: 6 ),
{ 5: }
{ 6: }
  ( sym: -63; act: 9 ),
{ 7: }
{ 8: }
  ( sym: -6; act: 11 ),
  ( sym: -5; act: 12 ),
{ 9: }
  ( sym: -4; act: 14 ),
{ 10: }
{ 11: }
{ 12: }
  ( sym: -48; act: 17 ),
  ( sym: -6; act: 18 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
  ( sym: -23; act: 21 ),
{ 17: }
  ( sym: -49; act: 23 ),
{ 18: }
{ 19: }
{ 20: }
  ( sym: -55; act: 27 ),
{ 21: }
  ( sym: -24; act: 28 ),
{ 22: }
  ( sym: -14; act: 30 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: -50; act: 33 ),
{ 26: }
  ( sym: -7; act: 36 ),
{ 27: }
  ( sym: -56; act: 40 ),
{ 28: }
  ( sym: -27; act: 43 ),
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -60; act: 50 ),
  ( sym: -22; act: 51 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 53 ),
{ 36: }
{ 37: }
  ( sym: -12; act: 73 ),
  ( sym: -8; act: 74 ),
{ 38: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 77 ),
  ( sym: -9; act: 78 ),
{ 39: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 80 ),
  ( sym: -10; act: 81 ),
{ 40: }
{ 41: }
{ 42: }
  ( sym: -57; act: 84 ),
{ 43: }
  ( sym: -35; act: 93 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
{ 53: }
  ( sym: -65; act: 100 ),
  ( sym: -64; act: 101 ),
{ 54: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 111 ),
{ 55: }
  ( sym: -60; act: 50 ),
  ( sym: -22; act: 112 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 53 ),
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: -15; act: 117 ),
  ( sym: -11; act: 118 ),
{ 74: }
{ 75: }
  ( sym: -13; act: 120 ),
{ 76: }
  ( sym: -13; act: 122 ),
{ 77: }
{ 78: }
{ 79: }
  ( sym: -60; act: 123 ),
  ( sym: -18; act: 124 ),
{ 80: }
{ 81: }
{ 82: }
  ( sym: -16; act: 125 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
{ 87: }
{ 88: }
  ( sym: -58; act: 131 ),
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
  ( sym: -51; act: 137 ),
{ 94: }
{ 95: }
  ( sym: -25; act: 139 ),
{ 96: }
{ 97: }
  ( sym: -60; act: 50 ),
  ( sym: -22; act: 141 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 53 ),
{ 98: }
  ( sym: -60; act: 50 ),
  ( sym: -22; act: 142 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 53 ),
{ 99: }
  ( sym: -60; act: 50 ),
  ( sym: -21; act: 143 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 144 ),
{ 100: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 145 ),
{ 101: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 146 ),
{ 102: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 147 ),
{ 103: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 148 ),
{ 104: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 149 ),
{ 105: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 150 ),
{ 106: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 151 ),
{ 107: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 152 ),
{ 108: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 153 ),
{ 109: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 154 ),
{ 110: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 155 ),
{ 111: }
{ 112: }
{ 113: }
  ( sym: -60; act: 157 ),
{ 114: }
  ( sym: -60; act: 158 ),
{ 115: }
  ( sym: -60; act: 159 ),
{ 116: }
  ( sym: -60; act: 50 ),
  ( sym: -22; act: 160 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 53 ),
{ 117: }
{ 118: }
{ 119: }
  ( sym: -16; act: 161 ),
{ 120: }
{ 121: }
{ 122: }
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  ( sym: -59; act: 169 ),
{ 133: }
{ 134: }
{ 135: }
{ 136: }
{ 137: }
  ( sym: -52; act: 171 ),
{ 138: }
  ( sym: -36; act: 174 ),
{ 139: }
  ( sym: -32; act: 175 ),
{ 140: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 179 ),
  ( sym: -26; act: 180 ),
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
{ 156: }
{ 157: }
{ 158: }
{ 159: }
{ 160: }
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
  ( sym: -60; act: 190 ),
{ 166: }
{ 167: }
{ 168: }
{ 169: }
{ 170: }
{ 171: }
{ 172: }
{ 173: }
  ( sym: -53; act: 195 ),
{ 174: }
  ( sym: -37; act: 197 ),
  ( sym: -25; act: 198 ),
{ 175: }
  ( sym: -33; act: 200 ),
{ 176: }
{ 177: }
{ 178: }
{ 179: }
{ 180: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 204 ),
{ 181: }
{ 182: }
{ 183: }
  ( sym: -60; act: 50 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 207 ),
{ 184: }
  ( sym: -60; act: 208 ),
{ 185: }
  ( sym: -60; act: 209 ),
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
  ( sym: -60; act: 50 ),
  ( sym: -54; act: 212 ),
  ( sym: -20; act: 52 ),
  ( sym: -19; act: 213 ),
  ( sym: -17; act: 214 ),
{ 197: }
{ 198: }
  ( sym: -38; act: 216 ),
{ 199: }
{ 200: }
{ 201: }
{ 202: }
  ( sym: -34; act: 219 ),
{ 203: }
  ( sym: -31; act: 222 ),
{ 204: }
{ 205: }
{ 206: }
  ( sym: -29; act: 224 ),
{ 207: }
{ 208: }
{ 209: }
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
  ( sym: -60; act: 123 ),
  ( sym: -18; act: 227 ),
{ 216: }
  ( sym: -44; act: 228 ),
  ( sym: -39; act: 229 ),
{ 217: }
  ( sym: -25; act: 232 ),
{ 218: }
{ 219: }
{ 220: }
{ 221: }
{ 222: }
{ 223: }
{ 224: }
{ 225: }
{ 226: }
{ 227: }
{ 228: }
{ 229: }
{ 230: }
  ( sym: -40; act: 240 ),
{ 231: }
{ 232: }
  ( sym: -38; act: 248 ),
{ 233: }
{ 234: }
{ 235: }
{ 236: }
{ 237: }
{ 238: }
{ 239: }
{ 240: }
{ 241: }
{ 242: }
{ 243: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 253 ),
{ 244: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 254 ),
{ 245: }
  ( sym: -41; act: 255 ),
{ 246: }
  ( sym: -60; act: 265 ),
  ( sym: -42; act: 266 ),
{ 247: }
  ( sym: -45; act: 267 ),
{ 248: }
  ( sym: -39; act: 229 ),
{ 249: }
{ 250: }
{ 251: }
{ 252: }
{ 253: }
{ 254: }
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
  ( sym: -43; act: 269 ),
{ 261: }
  ( sym: -43; act: 271 ),
{ 262: }
  ( sym: -43; act: 272 ),
{ 263: }
  ( sym: -43; act: 273 ),
{ 264: }
{ 265: }
{ 266: }
  ( sym: -60; act: 274 ),
{ 267: }
  ( sym: -46; act: 275 ),
{ 268: }
{ 269: }
{ 270: }
{ 271: }
{ 272: }
{ 273: }
{ 274: }
{ 275: }
{ 276: }
{ 277: }
  ( sym: -47; act: 281 ),
{ 278: }
{ 279: }
{ 280: }
{ 281: }
{ 282: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 289 ),
{ 283: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 290 ),
{ 284: }
  ( sym: -41; act: 291 ),
{ 285: }
  ( sym: -60; act: 265 ),
  ( sym: -42; act: 292 ),
{ 286: }
  ( sym: -66; act: 176 ),
  ( sym: -30; act: 177 ),
  ( sym: -29; act: 178 ),
  ( sym: -28; act: 293 ),
{ 287: }
{ 288: }
{ 289: }
{ 290: }
{ 291: }
{ 292: }
  ( sym: -60; act: 274 )
{ 293: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } -1,
{ 3: } 0,
{ 4: } 0,
{ 5: } 0,
{ 6: } -2,
{ 7: } 0,
{ 8: } 0,
{ 9: } 0,
{ 10: } 0,
{ 11: } -6,
{ 12: } 0,
{ 13: } 0,
{ 14: } -3,
{ 15: } 0,
{ 16: } 0,
{ 17: } 0,
{ 18: } -5,
{ 19: } 0,
{ 20: } -133,
{ 21: } 0,
{ 22: } 0,
{ 23: } -64,
{ 24: } 0,
{ 25: } 0,
{ 26: } 0,
{ 27: } 0,
{ 28: } 0,
{ 29: } 0,
{ 30: } 0,
{ 31: } -23,
{ 32: } -4,
{ 33: } 0,
{ 34: } 0,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
{ 38: } 0,
{ 39: } 0,
{ 40: } -134,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } -75,
{ 47: } -22,
{ 48: } -65,
{ 49: } -66,
{ 50: } -42,
{ 51: } 0,
{ 52: } 0,
{ 53: } 0,
{ 54: } 0,
{ 55: } 0,
{ 56: } 0,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } -147,
{ 61: } -41,
{ 62: } -148,
{ 63: } -62,
{ 64: } -43,
{ 65: } -44,
{ 66: } -45,
{ 67: } -46,
{ 68: } -47,
{ 69: } -48,
{ 70: } -49,
{ 71: } -50,
{ 72: } -7,
{ 73: } 0,
{ 74: } -8,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -9,
{ 79: } 0,
{ 80: } 0,
{ 81: } -10,
{ 82: } 0,
{ 83: } -132,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } -69,
{ 94: } 0,
{ 95: } 0,
{ 96: } -76,
{ 97: } 0,
{ 98: } 0,
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } 0,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } 0,
{ 109: } 0,
{ 110: } 0,
{ 111: } 0,
{ 112: } -53,
{ 113: } 0,
{ 114: } 0,
{ 115: } 0,
{ 116: } 0,
{ 117: } -14,
{ 118: } -11,
{ 119: } 0,
{ 120: } 0,
{ 121: } -21,
{ 122: } 0,
{ 123: } -26,
{ 124: } 0,
{ 125: } 0,
{ 126: } -29,
{ 127: } -135,
{ 128: } -136,
{ 129: } -137,
{ 130: } -138,
{ 131: } -139,
{ 132: } 0,
{ 133: } -140,
{ 134: } -141,
{ 135: } -142,
{ 136: } -143,
{ 137: } 0,
{ 138: } -97,
{ 139: } -90,
{ 140: } 0,
{ 141: } -54,
{ 142: } 0,
{ 143: } 0,
{ 144: } 0,
{ 145: } 0,
{ 146: } 0,
{ 147: } -32,
{ 148: } 0,
{ 149: } 0,
{ 150: } -33,
{ 151: } -34,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } -35,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } 0,
{ 162: } -19,
{ 163: } 0,
{ 164: } -12,
{ 165: } 0,
{ 166: } -16,
{ 167: } 0,
{ 168: } -18,
{ 169: } 0,
{ 170: } -145,
{ 171: } -70,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } 0,
{ 176: } -82,
{ 177: } -84,
{ 178: } 0,
{ 179: } -81,
{ 180: } 0,
{ 181: } 0,
{ 182: } -36,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } -40,
{ 187: } -37,
{ 188: } -27,
{ 189: } -20,
{ 190: } -25,
{ 191: } -28,
{ 192: } -144,
{ 193: } 0,
{ 194: } -68,
{ 195: } 0,
{ 196: } 0,
{ 197: } -98,
{ 198: } -100,
{ 199: } 0,
{ 200: } -91,
{ 201: } 0,
{ 202: } 0,
{ 203: } 0,
{ 204: } -80,
{ 205: } -79,
{ 206: } 0,
{ 207: } 0,
{ 208: } 0,
{ 209: } 0,
{ 210: } -146,
{ 211: } -71,
{ 212: } -72,
{ 213: } 0,
{ 214: } -74,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } -77,
{ 219: } 0,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } -89,
{ 224: } -85,
{ 225: } -38,
{ 226: } -39,
{ 227: } 0,
{ 228: } 0,
{ 229: } -101,
{ 230: } 0,
{ 231: } 0,
{ 232: } -100,
{ 233: } -92,
{ 234: } -93,
{ 235: } -94,
{ 236: } -87,
{ 237: } -88,
{ 238: } -24,
{ 239: } 0,
{ 240: } 0,
{ 241: } 0,
{ 242: } 0,
{ 243: } 0,
{ 244: } 0,
{ 245: } 0,
{ 246: } 0,
{ 247: } -124,
{ 248: } 0,
{ 249: } -96,
{ 250: } -102,
{ 251: } -103,
{ 252: } -104,
{ 253: } -105,
{ 254: } -106,
{ 255: } -107,
{ 256: } -109,
{ 257: } -110,
{ 258: } -111,
{ 259: } -112,
{ 260: } 0,
{ 261: } 0,
{ 262: } 0,
{ 263: } 0,
{ 264: } -113,
{ 265: } -121,
{ 266: } 0,
{ 267: } 0,
{ 268: } 0,
{ 269: } -114,
{ 270: } 0,
{ 271: } -115,
{ 272: } -116,
{ 273: } -117,
{ 274: } -120,
{ 275: } -125,
{ 276: } 0,
{ 277: } 0,
{ 278: } -99,
{ 279: } 0,
{ 280: } -123,
{ 281: } 0,
{ 282: } 0,
{ 283: } 0,
{ 284: } 0,
{ 285: } 0,
{ 286: } 0,
{ 287: } -119,
{ 288: } -126,
{ 289: } -128,
{ 290: } -129,
{ 291: } -130,
{ 292: } 0,
{ 293: } -127
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 4,
{ 5: } 5,
{ 6: } 6,
{ 7: } 6,
{ 8: } 7,
{ 9: } 8,
{ 10: } 9,
{ 11: } 10,
{ 12: } 10,
{ 13: } 13,
{ 14: } 14,
{ 15: } 14,
{ 16: } 15,
{ 17: } 16,
{ 18: } 18,
{ 19: } 18,
{ 20: } 19,
{ 21: } 19,
{ 22: } 20,
{ 23: } 21,
{ 24: } 21,
{ 25: } 22,
{ 26: } 24,
{ 27: } 27,
{ 28: } 29,
{ 29: } 30,
{ 30: } 31,
{ 31: } 33,
{ 32: } 33,
{ 33: } 33,
{ 34: } 34,
{ 35: } 35,
{ 36: } 53,
{ 37: } 54,
{ 38: } 56,
{ 39: } 73,
{ 40: } 90,
{ 41: } 90,
{ 42: } 91,
{ 43: } 99,
{ 44: } 102,
{ 45: } 103,
{ 46: } 104,
{ 47: } 104,
{ 48: } 104,
{ 49: } 104,
{ 50: } 104,
{ 51: } 104,
{ 52: } 107,
{ 53: } 108,
{ 54: } 117,
{ 55: } 133,
{ 56: } 151,
{ 57: } 152,
{ 58: } 153,
{ 59: } 154,
{ 60: } 155,
{ 61: } 155,
{ 62: } 155,
{ 63: } 155,
{ 64: } 155,
{ 65: } 155,
{ 66: } 155,
{ 67: } 155,
{ 68: } 155,
{ 69: } 155,
{ 70: } 155,
{ 71: } 155,
{ 72: } 155,
{ 73: } 155,
{ 74: } 157,
{ 75: } 157,
{ 76: } 158,
{ 77: } 159,
{ 78: } 165,
{ 79: } 165,
{ 80: } 167,
{ 81: } 173,
{ 82: } 173,
{ 83: } 174,
{ 84: } 174,
{ 85: } 175,
{ 86: } 176,
{ 87: } 177,
{ 88: } 178,
{ 89: } 179,
{ 90: } 180,
{ 91: } 181,
{ 92: } 182,
{ 93: } 183,
{ 94: } 183,
{ 95: } 184,
{ 96: } 187,
{ 97: } 187,
{ 98: } 205,
{ 99: } 223,
{ 100: } 239,
{ 101: } 255,
{ 102: } 271,
{ 103: } 287,
{ 104: } 303,
{ 105: } 319,
{ 106: } 335,
{ 107: } 351,
{ 108: } 367,
{ 109: } 383,
{ 110: } 399,
{ 111: } 415,
{ 112: } 421,
{ 113: } 421,
{ 114: } 423,
{ 115: } 425,
{ 116: } 427,
{ 117: } 445,
{ 118: } 445,
{ 119: } 445,
{ 120: } 446,
{ 121: } 448,
{ 122: } 448,
{ 123: } 450,
{ 124: } 450,
{ 125: } 452,
{ 126: } 454,
{ 127: } 454,
{ 128: } 454,
{ 129: } 454,
{ 130: } 454,
{ 131: } 454,
{ 132: } 454,
{ 133: } 455,
{ 134: } 455,
{ 135: } 455,
{ 136: } 455,
{ 137: } 455,
{ 138: } 457,
{ 139: } 457,
{ 140: } 457,
{ 141: } 458,
{ 142: } 458,
{ 143: } 462,
{ 144: } 464,
{ 145: } 471,
{ 146: } 480,
{ 147: } 489,
{ 148: } 489,
{ 149: } 503,
{ 150: } 517,
{ 151: } 517,
{ 152: } 517,
{ 153: } 526,
{ 154: } 535,
{ 155: } 544,
{ 156: } 553,
{ 157: } 553,
{ 158: } 554,
{ 159: } 555,
{ 160: } 556,
{ 161: } 559,
{ 162: } 561,
{ 163: } 561,
{ 164: } 562,
{ 165: } 562,
{ 166: } 564,
{ 167: } 564,
{ 168: } 565,
{ 169: } 565,
{ 170: } 567,
{ 171: } 567,
{ 172: } 567,
{ 173: } 568,
{ 174: } 569,
{ 175: } 574,
{ 176: } 576,
{ 177: } 576,
{ 178: } 576,
{ 179: } 579,
{ 180: } 579,
{ 181: } 581,
{ 182: } 585,
{ 183: } 585,
{ 184: } 601,
{ 185: } 603,
{ 186: } 605,
{ 187: } 605,
{ 188: } 605,
{ 189: } 605,
{ 190: } 605,
{ 191: } 605,
{ 192: } 605,
{ 193: } 605,
{ 194: } 606,
{ 195: } 606,
{ 196: } 607,
{ 197: } 624,
{ 198: } 624,
{ 199: } 624,
{ 200: } 625,
{ 201: } 625,
{ 202: } 626,
{ 203: } 628,
{ 204: } 629,
{ 205: } 629,
{ 206: } 629,
{ 207: } 630,
{ 208: } 637,
{ 209: } 638,
{ 210: } 639,
{ 211: } 639,
{ 212: } 639,
{ 213: } 639,
{ 214: } 645,
{ 215: } 645,
{ 216: } 647,
{ 217: } 650,
{ 218: } 653,
{ 219: } 653,
{ 220: } 654,
{ 221: } 655,
{ 222: } 656,
{ 223: } 658,
{ 224: } 658,
{ 225: } 658,
{ 226: } 658,
{ 227: } 658,
{ 228: } 660,
{ 229: } 661,
{ 230: } 661,
{ 231: } 667,
{ 232: } 668,
{ 233: } 668,
{ 234: } 668,
{ 235: } 668,
{ 236: } 668,
{ 237: } 668,
{ 238: } 668,
{ 239: } 668,
{ 240: } 669,
{ 241: } 670,
{ 242: } 671,
{ 243: } 672,
{ 244: } 673,
{ 245: } 674,
{ 246: } 683,
{ 247: } 685,
{ 248: } 685,
{ 249: } 687,
{ 250: } 687,
{ 251: } 687,
{ 252: } 687,
{ 253: } 687,
{ 254: } 687,
{ 255: } 687,
{ 256: } 687,
{ 257: } 687,
{ 258: } 687,
{ 259: } 687,
{ 260: } 687,
{ 261: } 689,
{ 262: } 691,
{ 263: } 693,
{ 264: } 695,
{ 265: } 695,
{ 266: } 695,
{ 267: } 698,
{ 268: } 700,
{ 269: } 701,
{ 270: } 701,
{ 271: } 702,
{ 272: } 702,
{ 273: } 702,
{ 274: } 702,
{ 275: } 702,
{ 276: } 702,
{ 277: } 703,
{ 278: } 708,
{ 279: } 708,
{ 280: } 709,
{ 281: } 709,
{ 282: } 710,
{ 283: } 711,
{ 284: } 712,
{ 285: } 721,
{ 286: } 723,
{ 287: } 724,
{ 288: } 724,
{ 289: } 724,
{ 290: } 724,
{ 291: } 724,
{ 292: } 724,
{ 293: } 727
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 2,
{ 2: } 2,
{ 3: } 3,
{ 4: } 4,
{ 5: } 5,
{ 6: } 5,
{ 7: } 6,
{ 8: } 7,
{ 9: } 8,
{ 10: } 9,
{ 11: } 9,
{ 12: } 12,
{ 13: } 13,
{ 14: } 13,
{ 15: } 14,
{ 16: } 15,
{ 17: } 17,
{ 18: } 17,
{ 19: } 18,
{ 20: } 18,
{ 21: } 19,
{ 22: } 20,
{ 23: } 20,
{ 24: } 21,
{ 25: } 23,
{ 26: } 26,
{ 27: } 28,
{ 28: } 29,
{ 29: } 30,
{ 30: } 32,
{ 31: } 32,
{ 32: } 32,
{ 33: } 33,
{ 34: } 34,
{ 35: } 52,
{ 36: } 53,
{ 37: } 55,
{ 38: } 72,
{ 39: } 89,
{ 40: } 89,
{ 41: } 90,
{ 42: } 98,
{ 43: } 101,
{ 44: } 102,
{ 45: } 103,
{ 46: } 103,
{ 47: } 103,
{ 48: } 103,
{ 49: } 103,
{ 50: } 103,
{ 51: } 106,
{ 52: } 107,
{ 53: } 116,
{ 54: } 132,
{ 55: } 150,
{ 56: } 151,
{ 57: } 152,
{ 58: } 153,
{ 59: } 154,
{ 60: } 154,
{ 61: } 154,
{ 62: } 154,
{ 63: } 154,
{ 64: } 154,
{ 65: } 154,
{ 66: } 154,
{ 67: } 154,
{ 68: } 154,
{ 69: } 154,
{ 70: } 154,
{ 71: } 154,
{ 72: } 154,
{ 73: } 156,
{ 74: } 156,
{ 75: } 157,
{ 76: } 158,
{ 77: } 164,
{ 78: } 164,
{ 79: } 166,
{ 80: } 172,
{ 81: } 172,
{ 82: } 173,
{ 83: } 173,
{ 84: } 174,
{ 85: } 175,
{ 86: } 176,
{ 87: } 177,
{ 88: } 178,
{ 89: } 179,
{ 90: } 180,
{ 91: } 181,
{ 92: } 182,
{ 93: } 182,
{ 94: } 183,
{ 95: } 186,
{ 96: } 186,
{ 97: } 204,
{ 98: } 222,
{ 99: } 238,
{ 100: } 254,
{ 101: } 270,
{ 102: } 286,
{ 103: } 302,
{ 104: } 318,
{ 105: } 334,
{ 106: } 350,
{ 107: } 366,
{ 108: } 382,
{ 109: } 398,
{ 110: } 414,
{ 111: } 420,
{ 112: } 420,
{ 113: } 422,
{ 114: } 424,
{ 115: } 426,
{ 116: } 444,
{ 117: } 444,
{ 118: } 444,
{ 119: } 445,
{ 120: } 447,
{ 121: } 447,
{ 122: } 449,
{ 123: } 449,
{ 124: } 451,
{ 125: } 453,
{ 126: } 453,
{ 127: } 453,
{ 128: } 453,
{ 129: } 453,
{ 130: } 453,
{ 131: } 453,
{ 132: } 454,
{ 133: } 454,
{ 134: } 454,
{ 135: } 454,
{ 136: } 454,
{ 137: } 456,
{ 138: } 456,
{ 139: } 456,
{ 140: } 457,
{ 141: } 457,
{ 142: } 461,
{ 143: } 463,
{ 144: } 470,
{ 145: } 479,
{ 146: } 488,
{ 147: } 488,
{ 148: } 502,
{ 149: } 516,
{ 150: } 516,
{ 151: } 516,
{ 152: } 525,
{ 153: } 534,
{ 154: } 543,
{ 155: } 552,
{ 156: } 552,
{ 157: } 553,
{ 158: } 554,
{ 159: } 555,
{ 160: } 558,
{ 161: } 560,
{ 162: } 560,
{ 163: } 561,
{ 164: } 561,
{ 165: } 563,
{ 166: } 563,
{ 167: } 564,
{ 168: } 564,
{ 169: } 566,
{ 170: } 566,
{ 171: } 566,
{ 172: } 567,
{ 173: } 568,
{ 174: } 573,
{ 175: } 575,
{ 176: } 575,
{ 177: } 575,
{ 178: } 578,
{ 179: } 578,
{ 180: } 580,
{ 181: } 584,
{ 182: } 584,
{ 183: } 600,
{ 184: } 602,
{ 185: } 604,
{ 186: } 604,
{ 187: } 604,
{ 188: } 604,
{ 189: } 604,
{ 190: } 604,
{ 191: } 604,
{ 192: } 604,
{ 193: } 605,
{ 194: } 605,
{ 195: } 606,
{ 196: } 623,
{ 197: } 623,
{ 198: } 623,
{ 199: } 624,
{ 200: } 624,
{ 201: } 625,
{ 202: } 627,
{ 203: } 628,
{ 204: } 628,
{ 205: } 628,
{ 206: } 629,
{ 207: } 636,
{ 208: } 637,
{ 209: } 638,
{ 210: } 638,
{ 211: } 638,
{ 212: } 638,
{ 213: } 644,
{ 214: } 644,
{ 215: } 646,
{ 216: } 649,
{ 217: } 652,
{ 218: } 652,
{ 219: } 653,
{ 220: } 654,
{ 221: } 655,
{ 222: } 657,
{ 223: } 657,
{ 224: } 657,
{ 225: } 657,
{ 226: } 657,
{ 227: } 659,
{ 228: } 660,
{ 229: } 660,
{ 230: } 666,
{ 231: } 667,
{ 232: } 667,
{ 233: } 667,
{ 234: } 667,
{ 235: } 667,
{ 236: } 667,
{ 237: } 667,
{ 238: } 667,
{ 239: } 668,
{ 240: } 669,
{ 241: } 670,
{ 242: } 671,
{ 243: } 672,
{ 244: } 673,
{ 245: } 682,
{ 246: } 684,
{ 247: } 684,
{ 248: } 686,
{ 249: } 686,
{ 250: } 686,
{ 251: } 686,
{ 252: } 686,
{ 253: } 686,
{ 254: } 686,
{ 255: } 686,
{ 256: } 686,
{ 257: } 686,
{ 258: } 686,
{ 259: } 686,
{ 260: } 688,
{ 261: } 690,
{ 262: } 692,
{ 263: } 694,
{ 264: } 694,
{ 265: } 694,
{ 266: } 697,
{ 267: } 699,
{ 268: } 700,
{ 269: } 700,
{ 270: } 701,
{ 271: } 701,
{ 272: } 701,
{ 273: } 701,
{ 274: } 701,
{ 275: } 701,
{ 276: } 702,
{ 277: } 707,
{ 278: } 707,
{ 279: } 708,
{ 280: } 708,
{ 281: } 709,
{ 282: } 710,
{ 283: } 711,
{ 284: } 720,
{ 285: } 722,
{ 286: } 723,
{ 287: } 723,
{ 288: } 723,
{ 289: } 723,
{ 290: } 723,
{ 291: } 723,
{ 292: } 726,
{ 293: } 726
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 3,
{ 3: } 4,
{ 4: } 4,
{ 5: } 5,
{ 6: } 5,
{ 7: } 6,
{ 8: } 6,
{ 9: } 8,
{ 10: } 9,
{ 11: } 9,
{ 12: } 9,
{ 13: } 11,
{ 14: } 11,
{ 15: } 11,
{ 16: } 11,
{ 17: } 12,
{ 18: } 13,
{ 19: } 13,
{ 20: } 13,
{ 21: } 14,
{ 22: } 15,
{ 23: } 16,
{ 24: } 16,
{ 25: } 16,
{ 26: } 17,
{ 27: } 18,
{ 28: } 19,
{ 29: } 20,
{ 30: } 20,
{ 31: } 20,
{ 32: } 20,
{ 33: } 20,
{ 34: } 20,
{ 35: } 20,
{ 36: } 24,
{ 37: } 24,
{ 38: } 26,
{ 39: } 30,
{ 40: } 34,
{ 41: } 34,
{ 42: } 34,
{ 43: } 35,
{ 44: } 36,
{ 45: } 36,
{ 46: } 36,
{ 47: } 36,
{ 48: } 36,
{ 49: } 36,
{ 50: } 36,
{ 51: } 36,
{ 52: } 36,
{ 53: } 36,
{ 54: } 38,
{ 55: } 41,
{ 56: } 45,
{ 57: } 45,
{ 58: } 45,
{ 59: } 45,
{ 60: } 45,
{ 61: } 45,
{ 62: } 45,
{ 63: } 45,
{ 64: } 45,
{ 65: } 45,
{ 66: } 45,
{ 67: } 45,
{ 68: } 45,
{ 69: } 45,
{ 70: } 45,
{ 71: } 45,
{ 72: } 45,
{ 73: } 45,
{ 74: } 47,
{ 75: } 47,
{ 76: } 48,
{ 77: } 49,
{ 78: } 49,
{ 79: } 49,
{ 80: } 51,
{ 81: } 51,
{ 82: } 51,
{ 83: } 52,
{ 84: } 52,
{ 85: } 52,
{ 86: } 52,
{ 87: } 52,
{ 88: } 52,
{ 89: } 53,
{ 90: } 53,
{ 91: } 53,
{ 92: } 53,
{ 93: } 53,
{ 94: } 54,
{ 95: } 54,
{ 96: } 55,
{ 97: } 55,
{ 98: } 59,
{ 99: } 63,
{ 100: } 67,
{ 101: } 70,
{ 102: } 73,
{ 103: } 76,
{ 104: } 79,
{ 105: } 82,
{ 106: } 85,
{ 107: } 88,
{ 108: } 91,
{ 109: } 94,
{ 110: } 97,
{ 111: } 100,
{ 112: } 100,
{ 113: } 100,
{ 114: } 101,
{ 115: } 102,
{ 116: } 103,
{ 117: } 107,
{ 118: } 107,
{ 119: } 107,
{ 120: } 108,
{ 121: } 108,
{ 122: } 108,
{ 123: } 108,
{ 124: } 108,
{ 125: } 108,
{ 126: } 108,
{ 127: } 108,
{ 128: } 108,
{ 129: } 108,
{ 130: } 108,
{ 131: } 108,
{ 132: } 108,
{ 133: } 109,
{ 134: } 109,
{ 135: } 109,
{ 136: } 109,
{ 137: } 109,
{ 138: } 110,
{ 139: } 111,
{ 140: } 112,
{ 141: } 117,
{ 142: } 117,
{ 143: } 117,
{ 144: } 117,
{ 145: } 117,
{ 146: } 117,
{ 147: } 117,
{ 148: } 117,
{ 149: } 117,
{ 150: } 117,
{ 151: } 117,
{ 152: } 117,
{ 153: } 117,
{ 154: } 117,
{ 155: } 117,
{ 156: } 117,
{ 157: } 117,
{ 158: } 117,
{ 159: } 117,
{ 160: } 117,
{ 161: } 117,
{ 162: } 117,
{ 163: } 117,
{ 164: } 117,
{ 165: } 117,
{ 166: } 118,
{ 167: } 118,
{ 168: } 118,
{ 169: } 118,
{ 170: } 118,
{ 171: } 118,
{ 172: } 118,
{ 173: } 118,
{ 174: } 119,
{ 175: } 121,
{ 176: } 122,
{ 177: } 122,
{ 178: } 122,
{ 179: } 122,
{ 180: } 122,
{ 181: } 126,
{ 182: } 126,
{ 183: } 126,
{ 184: } 129,
{ 185: } 130,
{ 186: } 131,
{ 187: } 131,
{ 188: } 131,
{ 189: } 131,
{ 190: } 131,
{ 191: } 131,
{ 192: } 131,
{ 193: } 131,
{ 194: } 131,
{ 195: } 131,
{ 196: } 131,
{ 197: } 136,
{ 198: } 136,
{ 199: } 137,
{ 200: } 137,
{ 201: } 137,
{ 202: } 137,
{ 203: } 138,
{ 204: } 139,
{ 205: } 139,
{ 206: } 139,
{ 207: } 140,
{ 208: } 140,
{ 209: } 140,
{ 210: } 140,
{ 211: } 140,
{ 212: } 140,
{ 213: } 140,
{ 214: } 140,
{ 215: } 140,
{ 216: } 142,
{ 217: } 144,
{ 218: } 145,
{ 219: } 145,
{ 220: } 145,
{ 221: } 145,
{ 222: } 145,
{ 223: } 145,
{ 224: } 145,
{ 225: } 145,
{ 226: } 145,
{ 227: } 145,
{ 228: } 145,
{ 229: } 145,
{ 230: } 145,
{ 231: } 146,
{ 232: } 146,
{ 233: } 147,
{ 234: } 147,
{ 235: } 147,
{ 236: } 147,
{ 237: } 147,
{ 238: } 147,
{ 239: } 147,
{ 240: } 147,
{ 241: } 147,
{ 242: } 147,
{ 243: } 147,
{ 244: } 151,
{ 245: } 155,
{ 246: } 156,
{ 247: } 158,
{ 248: } 159,
{ 249: } 160,
{ 250: } 160,
{ 251: } 160,
{ 252: } 160,
{ 253: } 160,
{ 254: } 160,
{ 255: } 160,
{ 256: } 160,
{ 257: } 160,
{ 258: } 160,
{ 259: } 160,
{ 260: } 160,
{ 261: } 161,
{ 262: } 162,
{ 263: } 163,
{ 264: } 164,
{ 265: } 164,
{ 266: } 164,
{ 267: } 165,
{ 268: } 166,
{ 269: } 166,
{ 270: } 166,
{ 271: } 166,
{ 272: } 166,
{ 273: } 166,
{ 274: } 166,
{ 275: } 166,
{ 276: } 166,
{ 277: } 166,
{ 278: } 167,
{ 279: } 167,
{ 280: } 167,
{ 281: } 167,
{ 282: } 167,
{ 283: } 171,
{ 284: } 175,
{ 285: } 176,
{ 286: } 178,
{ 287: } 182,
{ 288: } 182,
{ 289: } 182,
{ 290: } 182,
{ 291: } 182,
{ 292: } 182,
{ 293: } 183
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 4,
{ 5: } 4,
{ 6: } 5,
{ 7: } 5,
{ 8: } 7,
{ 9: } 8,
{ 10: } 8,
{ 11: } 8,
{ 12: } 10,
{ 13: } 10,
{ 14: } 10,
{ 15: } 10,
{ 16: } 11,
{ 17: } 12,
{ 18: } 12,
{ 19: } 12,
{ 20: } 13,
{ 21: } 14,
{ 22: } 15,
{ 23: } 15,
{ 24: } 15,
{ 25: } 16,
{ 26: } 17,
{ 27: } 18,
{ 28: } 19,
{ 29: } 19,
{ 30: } 19,
{ 31: } 19,
{ 32: } 19,
{ 33: } 19,
{ 34: } 19,
{ 35: } 23,
{ 36: } 23,
{ 37: } 25,
{ 38: } 29,
{ 39: } 33,
{ 40: } 33,
{ 41: } 33,
{ 42: } 34,
{ 43: } 35,
{ 44: } 35,
{ 45: } 35,
{ 46: } 35,
{ 47: } 35,
{ 48: } 35,
{ 49: } 35,
{ 50: } 35,
{ 51: } 35,
{ 52: } 35,
{ 53: } 37,
{ 54: } 40,
{ 55: } 44,
{ 56: } 44,
{ 57: } 44,
{ 58: } 44,
{ 59: } 44,
{ 60: } 44,
{ 61: } 44,
{ 62: } 44,
{ 63: } 44,
{ 64: } 44,
{ 65: } 44,
{ 66: } 44,
{ 67: } 44,
{ 68: } 44,
{ 69: } 44,
{ 70: } 44,
{ 71: } 44,
{ 72: } 44,
{ 73: } 46,
{ 74: } 46,
{ 75: } 47,
{ 76: } 48,
{ 77: } 48,
{ 78: } 48,
{ 79: } 50,
{ 80: } 50,
{ 81: } 50,
{ 82: } 51,
{ 83: } 51,
{ 84: } 51,
{ 85: } 51,
{ 86: } 51,
{ 87: } 51,
{ 88: } 52,
{ 89: } 52,
{ 90: } 52,
{ 91: } 52,
{ 92: } 52,
{ 93: } 53,
{ 94: } 53,
{ 95: } 54,
{ 96: } 54,
{ 97: } 58,
{ 98: } 62,
{ 99: } 66,
{ 100: } 69,
{ 101: } 72,
{ 102: } 75,
{ 103: } 78,
{ 104: } 81,
{ 105: } 84,
{ 106: } 87,
{ 107: } 90,
{ 108: } 93,
{ 109: } 96,
{ 110: } 99,
{ 111: } 99,
{ 112: } 99,
{ 113: } 100,
{ 114: } 101,
{ 115: } 102,
{ 116: } 106,
{ 117: } 106,
{ 118: } 106,
{ 119: } 107,
{ 120: } 107,
{ 121: } 107,
{ 122: } 107,
{ 123: } 107,
{ 124: } 107,
{ 125: } 107,
{ 126: } 107,
{ 127: } 107,
{ 128: } 107,
{ 129: } 107,
{ 130: } 107,
{ 131: } 107,
{ 132: } 108,
{ 133: } 108,
{ 134: } 108,
{ 135: } 108,
{ 136: } 108,
{ 137: } 109,
{ 138: } 110,
{ 139: } 111,
{ 140: } 116,
{ 141: } 116,
{ 142: } 116,
{ 143: } 116,
{ 144: } 116,
{ 145: } 116,
{ 146: } 116,
{ 147: } 116,
{ 148: } 116,
{ 149: } 116,
{ 150: } 116,
{ 151: } 116,
{ 152: } 116,
{ 153: } 116,
{ 154: } 116,
{ 155: } 116,
{ 156: } 116,
{ 157: } 116,
{ 158: } 116,
{ 159: } 116,
{ 160: } 116,
{ 161: } 116,
{ 162: } 116,
{ 163: } 116,
{ 164: } 116,
{ 165: } 117,
{ 166: } 117,
{ 167: } 117,
{ 168: } 117,
{ 169: } 117,
{ 170: } 117,
{ 171: } 117,
{ 172: } 117,
{ 173: } 118,
{ 174: } 120,
{ 175: } 121,
{ 176: } 121,
{ 177: } 121,
{ 178: } 121,
{ 179: } 121,
{ 180: } 125,
{ 181: } 125,
{ 182: } 125,
{ 183: } 128,
{ 184: } 129,
{ 185: } 130,
{ 186: } 130,
{ 187: } 130,
{ 188: } 130,
{ 189: } 130,
{ 190: } 130,
{ 191: } 130,
{ 192: } 130,
{ 193: } 130,
{ 194: } 130,
{ 195: } 130,
{ 196: } 135,
{ 197: } 135,
{ 198: } 136,
{ 199: } 136,
{ 200: } 136,
{ 201: } 136,
{ 202: } 137,
{ 203: } 138,
{ 204: } 138,
{ 205: } 138,
{ 206: } 139,
{ 207: } 139,
{ 208: } 139,
{ 209: } 139,
{ 210: } 139,
{ 211: } 139,
{ 212: } 139,
{ 213: } 139,
{ 214: } 139,
{ 215: } 141,
{ 216: } 143,
{ 217: } 144,
{ 218: } 144,
{ 219: } 144,
{ 220: } 144,
{ 221: } 144,
{ 222: } 144,
{ 223: } 144,
{ 224: } 144,
{ 225: } 144,
{ 226: } 144,
{ 227: } 144,
{ 228: } 144,
{ 229: } 144,
{ 230: } 145,
{ 231: } 145,
{ 232: } 146,
{ 233: } 146,
{ 234: } 146,
{ 235: } 146,
{ 236: } 146,
{ 237: } 146,
{ 238: } 146,
{ 239: } 146,
{ 240: } 146,
{ 241: } 146,
{ 242: } 146,
{ 243: } 150,
{ 244: } 154,
{ 245: } 155,
{ 246: } 157,
{ 247: } 158,
{ 248: } 159,
{ 249: } 159,
{ 250: } 159,
{ 251: } 159,
{ 252: } 159,
{ 253: } 159,
{ 254: } 159,
{ 255: } 159,
{ 256: } 159,
{ 257: } 159,
{ 258: } 159,
{ 259: } 159,
{ 260: } 160,
{ 261: } 161,
{ 262: } 162,
{ 263: } 163,
{ 264: } 163,
{ 265: } 163,
{ 266: } 164,
{ 267: } 165,
{ 268: } 165,
{ 269: } 165,
{ 270: } 165,
{ 271: } 165,
{ 272: } 165,
{ 273: } 165,
{ 274: } 165,
{ 275: } 165,
{ 276: } 165,
{ 277: } 166,
{ 278: } 166,
{ 279: } 166,
{ 280: } 166,
{ 281: } 166,
{ 282: } 170,
{ 283: } 174,
{ 284: } 175,
{ 285: } 177,
{ 286: } 181,
{ 287: } 181,
{ 288: } 181,
{ 289: } 181,
{ 290: } 181,
{ 291: } 181,
{ 292: } 182,
{ 293: } 182
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -62 ),
{ 2: } ( len: 0; sym: -63 ),
{ 3: } ( len: 5; sym: -61 ),
{ 4: } ( len: 7; sym: -2 ),
{ 5: } ( len: 2; sym: -5 ),
{ 6: } ( len: 1; sym: -5 ),
{ 7: } ( len: 5; sym: -6 ),
{ 8: } ( len: 2; sym: -7 ),
{ 9: } ( len: 2; sym: -7 ),
{ 10: } ( len: 2; sym: -7 ),
{ 11: } ( len: 2; sym: -8 ),
{ 12: } ( len: 3; sym: -8 ),
{ 13: } ( len: 0; sym: -11 ),
{ 14: } ( len: 1; sym: -11 ),
{ 15: } ( len: 1; sym: -9 ),
{ 16: } ( len: 3; sym: -9 ),
{ 17: } ( len: 1; sym: -10 ),
{ 18: } ( len: 3; sym: -10 ),
{ 19: } ( len: 3; sym: -12 ),
{ 20: } ( len: 3; sym: -13 ),
{ 21: } ( len: 1; sym: -13 ),
{ 22: } ( len: 2; sym: -14 ),
{ 23: } ( len: 1; sym: -14 ),
{ 24: } ( len: 3; sym: -17 ),
{ 25: } ( len: 3; sym: -18 ),
{ 26: } ( len: 1; sym: -18 ),
{ 27: } ( len: 3; sym: -15 ),
{ 28: } ( len: 3; sym: -16 ),
{ 29: } ( len: 1; sym: -16 ),
{ 30: } ( len: 3; sym: -19 ),
{ 31: } ( len: 3; sym: -19 ),
{ 32: } ( len: 3; sym: -19 ),
{ 33: } ( len: 3; sym: -19 ),
{ 34: } ( len: 3; sym: -19 ),
{ 35: } ( len: 3; sym: -19 ),
{ 36: } ( len: 4; sym: -19 ),
{ 37: } ( len: 4; sym: -19 ),
{ 38: } ( len: 6; sym: -19 ),
{ 39: } ( len: 6; sym: -19 ),
{ 40: } ( len: 4; sym: -19 ),
{ 41: } ( len: 1; sym: -19 ),
{ 42: } ( len: 1; sym: -19 ),
{ 43: } ( len: 1; sym: -20 ),
{ 44: } ( len: 1; sym: -20 ),
{ 45: } ( len: 1; sym: -20 ),
{ 46: } ( len: 1; sym: -20 ),
{ 47: } ( len: 1; sym: -20 ),
{ 48: } ( len: 1; sym: -20 ),
{ 49: } ( len: 1; sym: -20 ),
{ 50: } ( len: 1; sym: -20 ),
{ 51: } ( len: 3; sym: -21 ),
{ 52: } ( len: 1; sym: -21 ),
{ 53: } ( len: 2; sym: -22 ),
{ 54: } ( len: 3; sym: -22 ),
{ 55: } ( len: 3; sym: -22 ),
{ 56: } ( len: 3; sym: -22 ),
{ 57: } ( len: 3; sym: -22 ),
{ 58: } ( len: 3; sym: -22 ),
{ 59: } ( len: 3; sym: -22 ),
{ 60: } ( len: 3; sym: -22 ),
{ 61: } ( len: 3; sym: -22 ),
{ 62: } ( len: 1; sym: -22 ),
{ 63: } ( len: 0; sym: -48 ),
{ 64: } ( len: 2; sym: -48 ),
{ 65: } ( len: 3; sym: -49 ),
{ 66: } ( len: 2; sym: -50 ),
{ 67: } ( len: 2; sym: -50 ),
{ 68: } ( len: 10; sym: -3 ),
{ 69: } ( len: 0; sym: -51 ),
{ 70: } ( len: 2; sym: -51 ),
{ 71: } ( len: 3; sym: -52 ),
{ 72: } ( len: 2; sym: -53 ),
{ 73: } ( len: 1; sym: -54 ),
{ 74: } ( len: 1; sym: -54 ),
{ 75: } ( len: 3; sym: -23 ),
{ 76: } ( len: 3; sym: -24 ),
{ 77: } ( len: 6; sym: -27 ),
{ 78: } ( len: 0; sym: -25 ),
{ 79: } ( len: 3; sym: -25 ),
{ 80: } ( len: 2; sym: -26 ),
{ 81: } ( len: 1; sym: -26 ),
{ 82: } ( len: 1; sym: -28 ),
{ 83: } ( len: 1; sym: -28 ),
{ 84: } ( len: 1; sym: -28 ),
{ 85: } ( len: 3; sym: -29 ),
{ 86: } ( len: 1; sym: -29 ),
{ 87: } ( len: 4; sym: -30 ),
{ 88: } ( len: 2; sym: -31 ),
{ 89: } ( len: 1; sym: -31 ),
{ 90: } ( len: 0; sym: -32 ),
{ 91: } ( len: 2; sym: -32 ),
{ 92: } ( len: 3; sym: -33 ),
{ 93: } ( len: 2; sym: -34 ),
{ 94: } ( len: 2; sym: -34 ),
{ 95: } ( len: 0; sym: -35 ),
{ 96: } ( len: 8; sym: -35 ),
{ 97: } ( len: 0; sym: -36 ),
{ 98: } ( len: 2; sym: -36 ),
{ 99: } ( len: 6; sym: -37 ),
{ 100: } ( len: 0; sym: -38 ),
{ 101: } ( len: 2; sym: -38 ),
{ 102: } ( len: 3; sym: -39 ),
{ 103: } ( len: 2; sym: -40 ),
{ 104: } ( len: 2; sym: -40 ),
{ 105: } ( len: 2; sym: -40 ),
{ 106: } ( len: 2; sym: -40 ),
{ 107: } ( len: 2; sym: -40 ),
{ 108: } ( len: 2; sym: -40 ),
{ 109: } ( len: 1; sym: -41 ),
{ 110: } ( len: 1; sym: -41 ),
{ 111: } ( len: 1; sym: -41 ),
{ 112: } ( len: 1; sym: -41 ),
{ 113: } ( len: 1; sym: -41 ),
{ 114: } ( len: 2; sym: -41 ),
{ 115: } ( len: 2; sym: -41 ),
{ 116: } ( len: 2; sym: -41 ),
{ 117: } ( len: 2; sym: -41 ),
{ 118: } ( len: 0; sym: -43 ),
{ 119: } ( len: 3; sym: -43 ),
{ 120: } ( len: 2; sym: -42 ),
{ 121: } ( len: 1; sym: -42 ),
{ 122: } ( len: 0; sym: -44 ),
{ 123: } ( len: 5; sym: -44 ),
{ 124: } ( len: 0; sym: -45 ),
{ 125: } ( len: 2; sym: -45 ),
{ 126: } ( len: 3; sym: -46 ),
{ 127: } ( len: 2; sym: -47 ),
{ 128: } ( len: 2; sym: -47 ),
{ 129: } ( len: 2; sym: -47 ),
{ 130: } ( len: 2; sym: -47 ),
{ 131: } ( len: 2; sym: -47 ),
{ 132: } ( len: 5; sym: -4 ),
{ 133: } ( len: 0; sym: -55 ),
{ 134: } ( len: 2; sym: -55 ),
{ 135: } ( len: 3; sym: -56 ),
{ 136: } ( len: 2; sym: -57 ),
{ 137: } ( len: 2; sym: -57 ),
{ 138: } ( len: 2; sym: -57 ),
{ 139: } ( len: 2; sym: -57 ),
{ 140: } ( len: 2; sym: -57 ),
{ 141: } ( len: 2; sym: -57 ),
{ 142: } ( len: 2; sym: -57 ),
{ 143: } ( len: 2; sym: -57 ),
{ 144: } ( len: 3; sym: -58 ),
{ 145: } ( len: 1; sym: -59 ),
{ 146: } ( len: 3; sym: -59 ),
{ 147: } ( len: 1; sym: -60 ),
{ 148: } ( len: 1; sym: -60 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


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
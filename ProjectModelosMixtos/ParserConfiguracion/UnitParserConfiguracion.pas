
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)


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
const T_ENTERO = 267;
const T_REAL = 268;
const T_IDENTIFICADOR = 269;
const T_LITERAL_CADENAS = 270;
const T_BOOLEAN = 271;
const T_DATOS = 272;
const T_COLUMNAS = 273;
const T_CATEGORICOS = 274;
const T_ENTEROS = 275;
const T_REALES = 276;
const T_VALORES = 277;
const T_MODELO = 278;
const T_VARIABLES_CLASIFICACION = 279;
const T_VARIABLE_DEPENDIENTE = 280;
const T_EFECTOS_FIJOS = 281;
const T_EFECTOS = 282;
const T_INTERCEPTO = 283;
const T_OPCION = 284;
const T_GENERAR_COLUMNAS_NULAS = 285;
const T_INCLUIR_INTERCEPTO = 286;
const T_EFECTOS_ALEATORIOS = 287;
const T_GRUPO = 288;
const T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL = 289;
const T_UNIDAD_EXPERIMENTAL = 290;
const T_ESTRUCTURA = 291;
const T_SIMETRIA_COMPUESTA = 292;
const T_COMPONENTES_VARIANZA = 293;
const T_GENERAL = 294;
const T_DIAGONAL_HETEROGENEA = 295;
const T_FACTOR_ANALYTIC = 296;
const T_FACTOR_ANALYTIC_SIN_DIAGONAL = 297;
const T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR = 298;
const T_BANDEADA = 299;
const T_AUTOREGRESIVA = 300;
const T_PARAMETROS = 301;
const T_ERROR = 302;
const T_CRITERIO_ORDENAMIENTO = 303;
const T_PESO = 304;
const T_PROCEDIMIENTO = 305;
const T_METODO = 306;
const T_MIVQUE0 = 307;
const T_ML = 308;
const T_REML = 309;
const T_GRILLA_BUSQUEDA_ML = 310;
const T_GRILLA_BUSQUEDA_REML = 311;
const T_VALOR_INICIAL_PARAMETROS = 312;
const T_FUENTE_VALORES_INICIALES_PARAMETROS = 313;
const T_VALORES_POR_DEFECTO = 314;
const T_VALORES_CONFIGURACION = 315;
const T_CRITERIO_CONVERGENCIA = 316;
const T_CONVERGENCIA_PARAMETROS_ABSOLUTA = 317;
const T_CONVERGENCIA_PARAMETROS_RELATIVA = 318;
const T_CONVERGENCIA_VEROSIMILITUD_ABSOLUTA = 319;
const T_CONVERGENCIA_VEROSIMILITUD_RELATIVA = 320;
const T_CONVERGENCIA_GRADIENTE_ABSOLUTA = 321;
const T_CONVERGENCIA_GRADIENTE_RELATIVA = 322;
const T_CONVERGENCIA_HESSIANO_ABSOLUTA = 323;
const T_CONVERGENCIA_HESSIANO_RELATIVA = 324;
const T_TOLERANCIA_CONVERGENCIA = 325;
const T_CONSTANTE_RIDGE = 326;
const T_CANTIDAD_PASOS_SCORING = 327;
const T_COTA_INFERIOR_PARAMETROS = 328;
const T_COTA_SUPERIOR_PARAMETROS = 329;
const T_PASO_GRILLA_PARAMETROS = 330;
const T_VERIFICAR_COTA_INFERIOR_PARAMETROS = 331;
const T_VERIFICAR_COTA_SUPERIOR_PARAMETROS = 332;
const T_CANTIDAD_MAXIMA_ITERACIONES = 333;
const T_CANTIDAD_MAXIMA_EVALUACIONES_FUNCION_OBJETIVO = 334;
const T_TOLERANCIA_SINGULARIDAD = 335;
const T_TOLERANCIA_CHOLEKSY = 336;
const T_VALOR_INICIAL_FACTOR_RIDGING = 337;
const T_FACTOR_INCREMENTO_FACTOR_RIDGING = 338;
const T_FACTOR_DECREMENTO_FACTOR_RIDGING = 339;
const T_VALOR_MINIMO_FACTOR_RIDGING = 340;
const T_VALOR_MAXIMO_FACTOR_RIDGING = 341;
const T_SALIDA = 342;
const T_DEPURACION = 343;
const T_ARCHIVO_SALIDA = 344;

type YYSType = record case Integer of
                 1 : ( yyBoolean : Boolean );
                 2 : ( yyInteger : Integer );
                 3 : ( yyReal : Real );
                 4 : ( yyShortString : ShortString );
                 5 : ( yyTArregloStrings : TArregloStrings );
                 6 : ( yyTColumnaDatos : TColumnaDatos );
                 7 : ( yyTComponenteInformacionParametro : TComponenteInformacionParametro );
                 8 : ( yyTConfiguracionDatos : TConfiguracionDatos );
                 9 : ( yyTConfiguracionModelo : TConfiguracionModelo );
                10 : ( yyTConfiguracionProcedimiento : TConfiguracionProcedimiento );
                11 : ( yyTEfecto : TEfecto );
                12 : ( yyTEstructura : TEstructura );
                13 : ( yyTGrupoEfectos : TGrupoEfectos );
                14 : ( yyTInformacionParametroIndice : TInformacionParametroIndice );
                15 : ( yyTListaColumnasDatos : TListaColumnasDatos );
                16 : ( yyTListaEfectos : TListaEfectos );
                17 : ( yyTListaEfectosAnidados : TListaEfectosAnidados );
                18 : ( yyTListaGruposEfectos : TListaGruposEfectos );
                19 : ( yyTListaInformacionParametroIndice : TListaInformacionParametroIndice );
                20 : ( yyTListaOpciones : TListaOpciones );
                21 : ( yyTMatrizValores : TMatrizValores );
                22 : ( yyTOpcion : TOpcion );
                23 : ( yyTStrings : TStrings );
                24 : ( yyTTuplaEfectosAleatorios : TTuplaEfectosAleatorios );
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
         ConfiguracionProcedimiento.Modelo := yyv[yysp-2].yyTConfiguracionModelo; 
       end;
   4 : begin
         yyval.yyTConfiguracionDatos := TConfiguracionDatos.Create(yyv[yysp-5].yyShortString, yyv[yysp-3].yyTListaColumnasDatos, yyv[yysp-2].yyTMatrizValores); 
       end;
   5 : begin
         yyval.yyTListaColumnasDatos := yyv[yysp-1].yyTListaColumnasDatos; 
       end;
   6 : begin
         yyval.yyTListaColumnasDatos := yyv[yysp-1].yyTListaColumnasDatos; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   7 : begin
         yyval.yyTListaColumnasDatos := TListaColumnasDatos.Create; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   8 : begin
         case yyv[yysp-0].yyInteger of
         T_CATEGORICOS : yyval.yyTColumnaDatos := TColumnaDatosCategoricos.Create(yyv[yysp-2].yyShortString);
         T_ENTEROS : yyval.yyTColumnaDatos := TColumnaDatosEnteros.Create(yyv[yysp-2].yyShortString);
         T_REALES : yyval.yyTColumnaDatos := TColumnaDatosReales.Create(yyv[yysp-2].yyShortString);
         end; 
       end;
   9 : begin
         yyval.yyInteger := T_CATEGORICOS; 
       end;
  10 : begin
         yyval.yyInteger := T_ENTEROS; 
       end;
  11 : begin
         yyval.yyInteger := T_REALES; 
       end;
  12 : begin
         IdentificarTokensPalabrasClave := False;
         FreeAndNil(MatrizValores);
         MatrizValores := TMatrizValores.Create(1, 1); 
       end;
  13 : begin
         IdentificarTokensPalabrasClave := True; 
       end;
  14 : begin
         yyval.yyTMatrizValores := MatrizValores; 
       end;
  15 : begin
         IndiceColumna := 0; Inc(IndiceFila); 
       end;
  16 : begin
         IndiceColumna := 0; Inc(IndiceFila); 
       end;
  17 : begin
         MatrizValores [IndiceFila, IndiceColumna] := yyv[yysp-0].yyShortString; Inc(IndiceColumna); 
       end;
  18 : begin
         MatrizValores [IndiceFila, IndiceColumna] := yyv[yysp-0].yyShortString; Inc(IndiceColumna); 
       end;
  19 : begin
         yyval.yyShortString := yyv[yysp-0].yyShortString 
       end;
  20 : begin
         yyval.yyShortString := IntToStr(yyv[yysp-0].yyInteger) 
       end;
  21 : begin
         yyval.yyShortString := FloatToStr(yyv[yysp-0].yyReal) 
       end;
  22 : begin
         yyval.yyTConfiguracionModelo := TConfiguracionModelo.Create(yyv[yysp-8].yyShortString,
         yyv[yysp-6].yyTStrings,
         yyv[yysp-5].yyShortString,
         yyv[yysp-4].yyTGrupoEfectos,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.EfectosAleatorios,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios,
         yyv[yysp-3].yyTTuplaEfectosAleatorios.Error,
         yyv[yysp-2].yyTListaOpciones); 
       end;
  23 : begin
         yyval.yyTStrings := TStringList.Create; 
       end;
  24 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; 
       end;
  25 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  26 : begin
         yyval.yyTStrings := TStringList.Create; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  27 : begin
         yyval.yyTArregloStrings := yyv[yysp-1].yyTArregloStrings; 
       end;
  28 : begin
         yyval.yyTArregloStrings := yyv[yysp-2].yyTArregloStrings; yyval.yyTArregloStrings.Dimension := yyval.yyTArregloStrings.Dimension + 1; yyval.yyTArregloStrings [yyval.yyTArregloStrings.Alto] := yyv[yysp-0].yyShortString; 
       end;
  29 : begin
         yyval.yyTArregloStrings := TArregloStrings.Create(1); yyval.yyTArregloStrings [yyval.yyTArregloStrings.Bajo] := yyv[yysp-0].yyShortString; 
       end;
  30 : begin
         yyval.yyShortString := yyv[yysp-1].yyShortString; 
       end;
  31 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(TListaEfectos.Create, TListaOpciones.Create); 
       end;
  32 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaEfectos, yyv[yysp-2].yyTListaOpciones); 
       end;
  33 : begin
         yyval.yyTListaEfectos := TListaEfectos.Create; 
       end;
  34 : begin
         yyval.yyTListaEfectos := yyv[yysp-1].yyTListaEfectos; 
       end;
  35 : begin
         yyval.yyTListaEfectos := yyv[yysp-1].yyTListaEfectos; yyval.yyTListaEfectos.Add(yyv[yysp-0].yyTEfecto); 
       end;
  36 : begin
         yyval.yyTListaEfectos := TListaEfectos.Create; yyval.yyTListaEfectos.Add(yyv[yysp-0].yyTEfecto); 
       end;
  37 : begin
         yyval.yyTEfecto := TIntercepto.Create; 
       end;
  38 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  39 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  40 : begin
         yyval.yyTEfecto := TCruzamiento.Create(TEfectoPrincipal.Create(yyv[yysp-2].yyShortString), yyv[yysp-0].yyTEfecto); 
       end;
  41 : begin
         yyval.yyTEfecto := TEfectoPrincipal.Create(yyv[yysp-0].yyShortString); 
       end;
  42 : begin
         yyval.yyTEfecto := TAnidamiento.Create(yyv[yysp-3].yyTEfecto, yyv[yysp-1].yyTListaEfectosAnidados); 
       end;
  43 : begin
         yyval.yyTListaEfectosAnidados := yyv[yysp-1].yyTListaEfectosAnidados; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  44 : begin
         yyval.yyTListaEfectosAnidados := TListaEfectosAnidados.Create; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  45 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  46 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  47 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  48 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
  49 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
       end;
  50 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.Error := nil; 
       end;
  51 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := TGrupoEfectos.Create(yyv[yysp-4].yyTListaEfectos, yyv[yysp-3].yyTListaOpciones);
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := yyv[yysp-5].yyTListaGruposEfectos;
         yyval.yyTTuplaEfectosAleatorios.Error := yyv[yysp-2].yyTListaOpciones; 
       end;
  52 : begin
         yyval.yyTListaGruposEfectos := TListaGruposEfectos.Create; 
       end;
  53 : begin
         yyval.yyTListaGruposEfectos := yyv[yysp-1].yyTListaGruposEfectos; yyval.yyTListaGruposEfectos.Add(yyv[yysp-0].yyTGrupoEfectos); 
       end;
  54 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaEfectos, yyv[yysp-2].yyTListaOpciones); 
       end;
  55 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  56 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  57 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  58 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
  59 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
       end;
  60 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
  61 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
  62 : begin
         yyval.yyTOpcion := TOpcionParametroTEstructura.Create('estructura', yyv[yysp-0].yyTEstructura); 
       end;
  63 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('parametros', yyv[yysp-0].yyTListaInformacionParametroIndice); 
       end;
  64 : begin
         yyval.yyTEstructura := TEstructura.Create('simetria_compuesta'); 
       end;
  65 : begin
         yyval.yyTEstructura := TEstructura.Create('componentes_varianza'); 
       end;
  66 : begin
         yyval.yyTEstructura := TEstructura.Create('general'); 
       end;
  67 : begin
         yyval.yyTEstructura := TEstructura.Create('diagonal_heterogenea'); 
       end;
  68 : begin
         yyval.yyTEstructura := TEstructura.Create('autoregresiva'); 
       end;
  69 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic', yyv[yysp-0].yyInteger); 
       end;
  70 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_sin_diagonal', yyv[yysp-0].yyInteger); 
       end;
  71 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_diagonal_escalar', yyv[yysp-0].yyInteger); 
       end;
  72 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('bandeada', yyv[yysp-0].yyInteger); 
       end;
  73 : begin
         yyval.yyInteger := 1; 
       end;
  74 : begin
         yyval.yyInteger := yyv[yysp-1].yyInteger; 
       end;
  75 : begin
         yyval.yyTListaInformacionParametroIndice := yyv[yysp-1].yyTListaInformacionParametroIndice; yyval.yyTListaInformacionParametroIndice.Add(yyv[yysp-0].yyTInformacionParametroIndice); 
       end;
  76 : begin
         yyval.yyTListaInformacionParametroIndice := TListaInformacionParametroIndice.Create; yyval.yyTListaInformacionParametroIndice.Add(yyv[yysp-0].yyTInformacionParametroIndice); 
       end;
  77 : begin
         yyval.yyTInformacionParametroIndice := TInformacionParametroIndice.Create(yyv[yysp-0].yyReal); 
       end;
  78 : begin
         yyval.yyTInformacionParametroIndice := TInformacionParametroIndice.Create(yyv[yysp-11].yyTComponenteInformacionParametro.ValorEntero,
         yyv[yysp-9].yyTComponenteInformacionParametro.ValorString,
         yyv[yysp-7].yyTComponenteInformacionParametro.ValorReal,
         yyv[yysp-5].yyTComponenteInformacionParametro.ValorReal,
         yyv[yysp-3].yyTComponenteInformacionParametro.ValorReal,
         yyv[yysp-1].yyTComponenteInformacionParametro.ValorReal,
         yyv[yysp-7].yyTComponenteInformacionParametro.ValorPresente,
         yyv[yysp-5].yyTComponenteInformacionParametro.ValorPresente,
         yyv[yysp-3].yyTComponenteInformacionParametro.ValorPresente,
         yyv[yysp-1].yyTComponenteInformacionParametro.ValorPresente); 
       end;
  79 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := False; yyval.yyTComponenteInformacionParametro.ValorEntero := -1; 
       end;
  80 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := True; yyval.yyTComponenteInformacionParametro.ValorEntero := yyv[yysp-0].yyInteger; 
       end;
  81 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := False; yyval.yyTComponenteInformacionParametro.ValorString := ''; 
       end;
  82 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := False; yyval.yyTComponenteInformacionParametro.ValorString := yyv[yysp-0].yyShortString; 
       end;
  83 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := False; yyval.yyTComponenteInformacionParametro.ValorReal := 0.0; 
       end;
  84 : begin
         yyval.yyTComponenteInformacionParametro.ValorPresente := True; yyval.yyTComponenteInformacionParametro.ValorReal := yyv[yysp-0].yyReal; 
       end;
  85 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  86 : begin
         yyval.yyTListaOpciones := yyv[yysp-2].yyTListaOpciones; 
       end;
  87 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  88 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  89 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  90 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_ordenamiento', yyv[yysp-0].yyTEfecto); 
       end;
  91 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
  92 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
  93 : begin
         yyval.yyTOpcion := TOpcionParametroTEstructura.Create('estructura', yyv[yysp-0].yyTEstructura); 
       end;
  94 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('parametros', yyv[yysp-0].yyTListaInformacionParametroIndice); 
       end;
  95 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  96 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  97 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  98 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('peso', yyv[yysp-0].yyShortString); 
       end;
  99 : begin
         yyval.yyTConfiguracionProcedimiento := TConfiguracionProcedimiento.Create(yyv[yysp-2].yyTListaOpciones); 
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
         yyval.yyTOpcion := TOpcionParametroString.Create('metodo', yyv[yysp-0].yyShortString); 
       end;
 104 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('valor_inicial_parametros', yyv[yysp-0].yyReal); 
       end;
 105 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('fuente_valores_iniciales_parametros', yyv[yysp-0].yyShortString); 
       end;
 106 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('criterio_convergencia', yyv[yysp-0].yyShortString); 
       end;
 107 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('tolerancia_convergencia', yyv[yysp-0].yyReal); 
       end;
 108 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_pasos_scoring', yyv[yysp-0].yyInteger); 
       end;
 109 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('cota_inferior_parametros', yyv[yysp-0].yyReal); 
       end;
 110 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('cota_superior_parametros', yyv[yysp-0].yyReal); 
       end;
 111 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('paso_grilla_parametros', yyv[yysp-0].yyReal); 
       end;
 112 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('verificar_cota_inferior_parametros', yyv[yysp-0].yyBoolean); 
       end;
 113 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('verificar_cota_superior_parametros', yyv[yysp-0].yyBoolean); 
       end;
 114 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_maxima_iteraciones', yyv[yysp-0].yyInteger); 
       end;
 115 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_maxima_evaluaciones_funcion_objetivo', yyv[yysp-0].yyInteger); 
       end;
 116 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('tolerancia_singularidad', yyv[yysp-0].yyReal); 
       end;
 117 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('tolerancia_cholesky', yyv[yysp-0].yyReal); 
       end;
 118 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('valor_inicial_factor_ridging', yyv[yysp-0].yyReal); 
       end;
 119 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('factor_incremento_factor_ridging', yyv[yysp-0].yyReal); 
       end;
 120 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('factor_decremento_factor_ridging', yyv[yysp-0].yyReal); 
       end;
 121 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('valor_minimo_factor_ridging', yyv[yysp-0].yyReal); 
       end;
 122 : begin
         yyval.yyTOpcion := TOpcionParametroReal.Create('valor_maximo_factor_ridging', yyv[yysp-0].yyReal); 
       end;
 123 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('salida', yyv[yysp-0].yyTArregloStrings); 
       end;
 124 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_salida', yyv[yysp-0].yyShortString); 
       end;
 125 : begin
         yyval := yyv[yysp-0];
       end;
 126 : begin
         yyval := yyv[yysp-0];
       end;
 127 : begin
         yyval := yyv[yysp-0];
       end;
 128 : begin
         yyval := yyv[yysp-0];
       end;
 129 : begin
         yyval := yyv[yysp-0];
       end;
 130 : begin
         yyval := yyv[yysp-0];
       end;
 131 : begin
         yyval := yyv[yysp-0];
       end;
 132 : begin
         yyval := yyv[yysp-0];
       end;
 133 : begin
         yyval := yyv[yysp-0];
       end;
 134 : begin
         yyval := yyv[yysp-0];
       end;
 135 : begin
         yyval := yyv[yysp-0];
       end;
 136 : begin
         yyval := yyv[yysp-0];
       end;
 137 : begin
         yyval := yyv[yysp-0];
       end;
 138 : begin
         yyval := yyv[yysp-0];
       end;
 139 : begin
         yyval := yyv[yysp-0];
       end;
 140 : begin
         yyval := yyv[yysp-0];
       end;
 141 : begin
         yyval := yyv[yysp-0];
       end;
 142 : begin
         yyval := yyv[yysp-0];
       end;
 143 : begin
         yyval := yyv[yysp-0];
       end;
 144 : begin
         yyval.yyReal := yyv[yysp-0].yyInteger; 
       end;
 145 : begin
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

yynacts   = 280;
yyngotos  = 116;
yynstates = 260;
yynrules  = 145;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 272; act: 3 ),
{ 1: }
  ( sym: 0; act: 0 ),
{ 2: }
{ 3: }
  ( sym: 270; act: 5 ),
{ 4: }
  ( sym: 278; act: 7 ),
{ 5: }
  ( sym: 257; act: 8 ),
{ 6: }
{ 7: }
  ( sym: 270; act: 10 ),
{ 8: }
  ( sym: 273; act: 12 ),
{ 9: }
  ( sym: 305; act: 14 ),
{ 10: }
  ( sym: 257; act: 15 ),
{ 11: }
  ( sym: 277; act: 17 ),
{ 12: }
  ( sym: 269; act: 20 ),
{ 13: }
{ 14: }
  ( sym: 257; act: 21 ),
{ 15: }
  ( sym: 279; act: 23 ),
  ( sym: 280; act: -23 ),
{ 16: }
  ( sym: 258; act: 24 ),
{ 17: }
  ( sym: 257; act: 25 ),
{ 18: }
{ 19: }
  ( sym: 265; act: 27 ),
  ( sym: 269; act: 20 ),
{ 20: }
  ( sym: 263; act: 28 ),
{ 21: }
{ 22: }
  ( sym: 280; act: 31 ),
{ 23: }
  ( sym: 269; act: 33 ),
{ 24: }
  ( sym: 265; act: 34 ),
{ 25: }
{ 26: }
{ 27: }
{ 28: }
  ( sym: 274; act: 37 ),
  ( sym: 275; act: 38 ),
  ( sym: 276; act: 39 ),
{ 29: }
  ( sym: 258; act: 41 ),
  ( sym: 284; act: 42 ),
{ 30: }
  ( sym: 281; act: 44 ),
  ( sym: 258; act: -31 ),
  ( sym: 284; act: -31 ),
  ( sym: 287; act: -31 ),
{ 31: }
  ( sym: 269; act: 45 ),
{ 32: }
  ( sym: 265; act: 46 ),
  ( sym: 269; act: 47 ),
{ 33: }
{ 34: }
{ 35: }
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
  ( sym: 265; act: 54 ),
{ 42: }
  ( sym: 306; act: 56 ),
  ( sym: 312; act: 57 ),
  ( sym: 313; act: 58 ),
  ( sym: 316; act: 59 ),
  ( sym: 325; act: 60 ),
  ( sym: 327; act: 61 ),
  ( sym: 328; act: 62 ),
  ( sym: 329; act: 63 ),
  ( sym: 330; act: 64 ),
  ( sym: 331; act: 65 ),
  ( sym: 332; act: 66 ),
  ( sym: 333; act: 67 ),
  ( sym: 334; act: 68 ),
  ( sym: 335; act: 69 ),
  ( sym: 336; act: 70 ),
  ( sym: 337; act: 71 ),
  ( sym: 338; act: 72 ),
  ( sym: 339; act: 73 ),
  ( sym: 340; act: 74 ),
  ( sym: 341; act: 75 ),
  ( sym: 342; act: 76 ),
  ( sym: 344; act: 77 ),
{ 43: }
  ( sym: 287; act: 79 ),
  ( sym: 258; act: -50 ),
  ( sym: 284; act: -50 ),
{ 44: }
  ( sym: 257; act: 80 ),
{ 45: }
  ( sym: 265; act: 81 ),
{ 46: }
{ 47: }
{ 48: }
  ( sym: 265; act: 83 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
{ 49: }
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
  ( sym: 258; act: -13 ),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
  ( sym: 265; act: 86 ),
{ 56: }
  ( sym: 307; act: 88 ),
  ( sym: 308; act: 89 ),
  ( sym: 309; act: 90 ),
  ( sym: 310; act: 91 ),
  ( sym: 311; act: 92 ),
  ( sym: 343; act: 93 ),
{ 57: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 58: }
  ( sym: 307; act: 98 ),
  ( sym: 310; act: 99 ),
  ( sym: 311; act: 100 ),
  ( sym: 314; act: 101 ),
  ( sym: 315; act: 102 ),
{ 59: }
  ( sym: 317; act: 104 ),
  ( sym: 318; act: 105 ),
  ( sym: 319; act: 106 ),
  ( sym: 320; act: 107 ),
  ( sym: 321; act: 108 ),
  ( sym: 322; act: 109 ),
  ( sym: 323; act: 110 ),
  ( sym: 324; act: 111 ),
{ 60: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 61: }
  ( sym: 267; act: 113 ),
{ 62: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 63: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 64: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 65: }
  ( sym: 271; act: 117 ),
{ 66: }
  ( sym: 271; act: 118 ),
{ 67: }
  ( sym: 267; act: 119 ),
{ 68: }
  ( sym: 267; act: 120 ),
{ 69: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 70: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 71: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 72: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 73: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 74: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 75: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 76: }
  ( sym: 261; act: 129 ),
{ 77: }
  ( sym: 270; act: 130 ),
{ 78: }
{ 79: }
  ( sym: 257; act: 132 ),
{ 80: }
  ( sym: 282; act: 134 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: 265; act: 135 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
{ 85: }
  ( sym: 258; act: 136 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
{ 111: }
{ 112: }
{ 113: }
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
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
  ( sym: 269; act: 138 ),
{ 130: }
{ 131: }
  ( sym: 258; act: 140 ),
  ( sym: 284; act: 141 ),
{ 132: }
{ 133: }
{ 134: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 135: }
{ 136: }
  ( sym: 265; act: 150 ),
{ 137: }
  ( sym: 262; act: 151 ),
  ( sym: 264; act: 152 ),
{ 138: }
{ 139: }
{ 140: }
  ( sym: 265; act: 153 ),
{ 141: }
  ( sym: 304; act: 155 ),
{ 142: }
  ( sym: 282; act: 134 ),
  ( sym: 288; act: 158 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
  ( sym: 302; act: -33 ),
{ 143: }
  ( sym: 258; act: 160 ),
  ( sym: 284; act: 161 ),
{ 144: }
{ 145: }
  ( sym: 259; act: 162 ),
  ( sym: 265; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 283; act: -38 ),
{ 146: }
{ 147: }
  ( sym: 265; act: 164 ),
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 148: }
  ( sym: 266; act: 165 ),
  ( sym: 259; act: -41 ),
  ( sym: 265; act: -41 ),
  ( sym: 269; act: -41 ),
  ( sym: 283; act: -41 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
  ( sym: 269; act: 166 ),
{ 153: }
{ 154: }
  ( sym: 265; act: 167 ),
{ 155: }
  ( sym: 269; act: 168 ),
{ 156: }
{ 157: }
{ 158: }
  ( sym: 257; act: 170 ),
{ 159: }
{ 160: }
  ( sym: 265; act: 171 ),
{ 161: }
  ( sym: 285; act: 173 ),
  ( sym: 286; act: 174 ),
{ 162: }
  ( sym: 269; act: 176 ),
{ 163: }
{ 164: }
{ 165: }
  ( sym: 269; act: 148 ),
{ 166: }
{ 167: }
{ 168: }
{ 169: }
  ( sym: 284; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 258; act: -85 ),
{ 170: }
  ( sym: 282; act: 134 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
{ 171: }
{ 172: }
  ( sym: 265; act: 183 ),
{ 173: }
  ( sym: 271; act: 184 ),
{ 174: }
  ( sym: 271; act: 185 ),
{ 175: }
  ( sym: 260; act: 186 ),
  ( sym: 269; act: 187 ),
{ 176: }
{ 177: }
{ 178: }
  ( sym: 258; act: 188 ),
{ 179: }
{ 180: }
  ( sym: 285; act: 190 ),
  ( sym: 286; act: 191 ),
  ( sym: 289; act: 192 ),
  ( sym: 290; act: 193 ),
  ( sym: 291; act: 194 ),
  ( sym: 301; act: 195 ),
{ 181: }
  ( sym: 257; act: 196 ),
{ 182: }
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
  ( sym: 265; act: 198 ),
{ 189: }
  ( sym: 265; act: 199 ),
{ 190: }
  ( sym: 271; act: 200 ),
{ 191: }
  ( sym: 271; act: 201 ),
{ 192: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 193: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 194: }
  ( sym: 292; act: 205 ),
  ( sym: 293; act: 206 ),
  ( sym: 294; act: 207 ),
  ( sym: 295; act: 208 ),
  ( sym: 296; act: 209 ),
  ( sym: 297; act: 210 ),
  ( sym: 298; act: 211 ),
  ( sym: 299; act: 212 ),
  ( sym: 300; act: 213 ),
{ 195: }
  ( sym: 259; act: 217 ),
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 196: }
{ 197: }
  ( sym: 258; act: 219 ),
  ( sym: 284; act: 180 ),
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
{ 208: }
{ 209: }
  ( sym: 259; act: 221 ),
  ( sym: 265; act: -73 ),
{ 210: }
  ( sym: 259; act: 221 ),
  ( sym: 265; act: -73 ),
{ 211: }
  ( sym: 259; act: 221 ),
  ( sym: 265; act: -73 ),
{ 212: }
  ( sym: 259; act: 221 ),
  ( sym: 265; act: -73 ),
{ 213: }
{ 214: }
{ 215: }
{ 216: }
  ( sym: 259; act: 217 ),
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 265; act: -63 ),
{ 217: }
  ( sym: 267; act: 227 ),
  ( sym: 264; act: -79 ),
{ 218: }
  ( sym: 258; act: 229 ),
  ( sym: 284; act: 230 ),
{ 219: }
  ( sym: 265; act: 231 ),
{ 220: }
{ 221: }
  ( sym: 267; act: 232 ),
{ 222: }
{ 223: }
{ 224: }
{ 225: }
{ 226: }
  ( sym: 264; act: 233 ),
{ 227: }
{ 228: }
{ 229: }
  ( sym: 265; act: 234 ),
{ 230: }
  ( sym: 289; act: 236 ),
  ( sym: 290; act: 237 ),
  ( sym: 291; act: 238 ),
  ( sym: 301; act: 239 ),
  ( sym: 303; act: 240 ),
{ 231: }
{ 232: }
  ( sym: 260; act: 241 ),
{ 233: }
  ( sym: 269; act: 243 ),
  ( sym: 264; act: -81 ),
{ 234: }
{ 235: }
  ( sym: 265; act: 244 ),
{ 236: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 237: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 238: }
  ( sym: 292; act: 205 ),
  ( sym: 293; act: 206 ),
  ( sym: 294; act: 207 ),
  ( sym: 295; act: 208 ),
  ( sym: 296; act: 209 ),
  ( sym: 297; act: 210 ),
  ( sym: 298; act: 211 ),
  ( sym: 299; act: 212 ),
  ( sym: 300; act: 213 ),
{ 239: }
  ( sym: 259; act: 217 ),
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
{ 240: }
  ( sym: 269; act: 148 ),
  ( sym: 283; act: 149 ),
{ 241: }
{ 242: }
  ( sym: 264; act: 250 ),
{ 243: }
{ 244: }
{ 245: }
{ 246: }
{ 247: }
{ 248: }
  ( sym: 259; act: 217 ),
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 265; act: -94 ),
{ 249: }
{ 250: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 264; act: -83 ),
{ 251: }
{ 252: }
  ( sym: 264; act: 253 ),
{ 253: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 264; act: -83 ),
{ 254: }
  ( sym: 264; act: 255 ),
{ 255: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 264; act: -83 ),
{ 256: }
  ( sym: 264; act: 257 ),
{ 257: }
  ( sym: 267; act: 95 ),
  ( sym: 268; act: 96 ),
  ( sym: 260; act: -83 ),
{ 258: }
  ( sym: 260; act: 259 )
{ 259: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -53; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
{ 2: }
  ( sym: -54; act: 4 ),
{ 3: }
{ 4: }
  ( sym: -3; act: 6 ),
{ 5: }
{ 6: }
  ( sym: -55; act: 9 ),
{ 7: }
{ 8: }
  ( sym: -5; act: 11 ),
{ 9: }
  ( sym: -4; act: 13 ),
{ 10: }
{ 11: }
  ( sym: -9; act: 16 ),
{ 12: }
  ( sym: -7; act: 18 ),
  ( sym: -6; act: 19 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: -11; act: 22 ),
{ 16: }
{ 17: }
{ 18: }
{ 19: }
  ( sym: -7; act: 26 ),
{ 20: }
{ 21: }
  ( sym: -46; act: 29 ),
{ 22: }
  ( sym: -12; act: 30 ),
{ 23: }
  ( sym: -13; act: 32 ),
{ 24: }
{ 25: }
  ( sym: -57; act: 35 ),
{ 26: }
{ 27: }
{ 28: }
  ( sym: -8; act: 36 ),
{ 29: }
  ( sym: -47; act: 40 ),
{ 30: }
  ( sym: -16; act: 43 ),
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -59; act: 48 ),
  ( sym: -56; act: 49 ),
  ( sym: -10; act: 50 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
  ( sym: -48; act: 55 ),
{ 43: }
  ( sym: -26; act: 78 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: -10; act: 82 ),
{ 49: }
  ( sym: -59; act: 84 ),
  ( sym: -58; act: 85 ),
  ( sym: -10; act: 50 ),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
  ( sym: -49; act: 87 ),
{ 57: }
  ( sym: -52; act: 94 ),
{ 58: }
  ( sym: -50; act: 97 ),
{ 59: }
  ( sym: -51; act: 103 ),
{ 60: }
  ( sym: -52; act: 112 ),
{ 61: }
{ 62: }
  ( sym: -52; act: 114 ),
{ 63: }
  ( sym: -52; act: 115 ),
{ 64: }
  ( sym: -52; act: 116 ),
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
  ( sym: -52; act: 121 ),
{ 70: }
  ( sym: -52; act: 122 ),
{ 71: }
  ( sym: -52; act: 123 ),
{ 72: }
  ( sym: -52; act: 124 ),
{ 73: }
  ( sym: -52; act: 125 ),
{ 74: }
  ( sym: -52; act: 126 ),
{ 75: }
  ( sym: -52; act: 127 ),
{ 76: }
  ( sym: -14; act: 128 ),
{ 77: }
{ 78: }
  ( sym: -43; act: 131 ),
{ 79: }
{ 80: }
  ( sym: -17; act: 133 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: -10; act: 82 ),
{ 85: }
{ 86: }
{ 87: }
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
{ 109: }
{ 110: }
{ 111: }
{ 112: }
{ 113: }
{ 114: }
{ 115: }
{ 116: }
{ 117: }
{ 118: }
{ 119: }
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
  ( sym: -15; act: 137 ),
{ 130: }
{ 131: }
  ( sym: -44; act: 139 ),
{ 132: }
  ( sym: -27; act: 142 ),
{ 133: }
  ( sym: -23; act: 143 ),
{ 134: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 146 ),
  ( sym: -18; act: 147 ),
{ 135: }
{ 136: }
{ 137: }
{ 138: }
{ 139: }
{ 140: }
{ 141: }
  ( sym: -45; act: 154 ),
{ 142: }
  ( sym: -28; act: 156 ),
  ( sym: -17; act: 157 ),
{ 143: }
  ( sym: -24; act: 159 ),
{ 144: }
{ 145: }
{ 146: }
{ 147: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 163 ),
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
  ( sym: -29; act: 169 ),
{ 158: }
{ 159: }
{ 160: }
{ 161: }
  ( sym: -25; act: 172 ),
{ 162: }
  ( sym: -22; act: 175 ),
{ 163: }
{ 164: }
{ 165: }
  ( sym: -20; act: 177 ),
{ 166: }
{ 167: }
{ 168: }
{ 169: }
  ( sym: -39; act: 178 ),
  ( sym: -30; act: 179 ),
{ 170: }
  ( sym: -17; act: 182 ),
{ 171: }
{ 172: }
{ 173: }
{ 174: }
{ 175: }
{ 176: }
{ 177: }
{ 178: }
{ 179: }
{ 180: }
  ( sym: -31; act: 189 ),
{ 181: }
{ 182: }
  ( sym: -29; act: 197 ),
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 202 ),
{ 193: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 203 ),
{ 194: }
  ( sym: -32; act: 204 ),
{ 195: }
  ( sym: -52; act: 214 ),
  ( sym: -35; act: 215 ),
  ( sym: -34; act: 216 ),
{ 196: }
  ( sym: -40; act: 218 ),
{ 197: }
  ( sym: -30; act: 179 ),
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
{ 208: }
{ 209: }
  ( sym: -33; act: 220 ),
{ 210: }
  ( sym: -33; act: 222 ),
{ 211: }
  ( sym: -33; act: 223 ),
{ 212: }
  ( sym: -33; act: 224 ),
{ 213: }
{ 214: }
{ 215: }
{ 216: }
  ( sym: -52; act: 214 ),
  ( sym: -35; act: 225 ),
{ 217: }
  ( sym: -36; act: 226 ),
{ 218: }
  ( sym: -41; act: 228 ),
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
  ( sym: -42; act: 235 ),
{ 231: }
{ 232: }
{ 233: }
  ( sym: -38; act: 242 ),
{ 234: }
{ 235: }
{ 236: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 245 ),
{ 237: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 246 ),
{ 238: }
  ( sym: -32; act: 247 ),
{ 239: }
  ( sym: -52; act: 214 ),
  ( sym: -35; act: 215 ),
  ( sym: -34; act: 248 ),
{ 240: }
  ( sym: -21; act: 144 ),
  ( sym: -20; act: 145 ),
  ( sym: -19; act: 249 ),
{ 241: }
{ 242: }
{ 243: }
{ 244: }
{ 245: }
{ 246: }
{ 247: }
{ 248: }
  ( sym: -52; act: 214 ),
  ( sym: -35; act: 225 ),
{ 249: }
{ 250: }
  ( sym: -52; act: 251 ),
  ( sym: -37; act: 252 ),
{ 251: }
{ 252: }
{ 253: }
  ( sym: -52; act: 251 ),
  ( sym: -37; act: 254 ),
{ 254: }
{ 255: }
  ( sym: -52; act: 251 ),
  ( sym: -37; act: 256 ),
{ 256: }
{ 257: }
  ( sym: -52; act: 251 ),
  ( sym: -37; act: 258 )
{ 258: }
{ 259: }
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
{ 11: } 0,
{ 12: } 0,
{ 13: } -3,
{ 14: } 0,
{ 15: } 0,
{ 16: } 0,
{ 17: } 0,
{ 18: } -7,
{ 19: } 0,
{ 20: } 0,
{ 21: } -100,
{ 22: } 0,
{ 23: } 0,
{ 24: } 0,
{ 25: } -12,
{ 26: } -6,
{ 27: } -5,
{ 28: } 0,
{ 29: } 0,
{ 30: } 0,
{ 31: } 0,
{ 32: } 0,
{ 33: } -26,
{ 34: } -4,
{ 35: } 0,
{ 36: } -8,
{ 37: } -9,
{ 38: } -10,
{ 39: } -11,
{ 40: } -101,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } 0,
{ 45: } 0,
{ 46: } -24,
{ 47: } -25,
{ 48: } 0,
{ 49: } 0,
{ 50: } -18,
{ 51: } -20,
{ 52: } -21,
{ 53: } -19,
{ 54: } -99,
{ 55: } 0,
{ 56: } 0,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } 0,
{ 61: } 0,
{ 62: } 0,
{ 63: } 0,
{ 64: } 0,
{ 65: } 0,
{ 66: } 0,
{ 67: } 0,
{ 68: } 0,
{ 69: } 0,
{ 70: } 0,
{ 71: } 0,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -95,
{ 79: } 0,
{ 80: } 0,
{ 81: } -30,
{ 82: } -17,
{ 83: } -16,
{ 84: } 0,
{ 85: } 0,
{ 86: } -102,
{ 87: } -103,
{ 88: } -127,
{ 89: } -125,
{ 90: } -126,
{ 91: } -128,
{ 92: } -129,
{ 93: } -130,
{ 94: } -104,
{ 95: } -144,
{ 96: } -145,
{ 97: } -105,
{ 98: } -133,
{ 99: } -134,
{ 100: } -135,
{ 101: } -131,
{ 102: } -132,
{ 103: } -106,
{ 104: } -136,
{ 105: } -137,
{ 106: } -138,
{ 107: } -139,
{ 108: } -140,
{ 109: } -141,
{ 110: } -142,
{ 111: } -143,
{ 112: } -107,
{ 113: } -108,
{ 114: } -109,
{ 115: } -110,
{ 116: } -111,
{ 117: } -112,
{ 118: } -113,
{ 119: } -114,
{ 120: } -115,
{ 121: } -116,
{ 122: } -117,
{ 123: } -118,
{ 124: } -119,
{ 125: } -120,
{ 126: } -121,
{ 127: } -122,
{ 128: } -123,
{ 129: } 0,
{ 130: } -124,
{ 131: } 0,
{ 132: } -52,
{ 133: } -45,
{ 134: } 0,
{ 135: } -15,
{ 136: } 0,
{ 137: } 0,
{ 138: } -29,
{ 139: } -96,
{ 140: } 0,
{ 141: } 0,
{ 142: } 0,
{ 143: } 0,
{ 144: } -39,
{ 145: } 0,
{ 146: } -36,
{ 147: } 0,
{ 148: } 0,
{ 149: } -37,
{ 150: } -14,
{ 151: } -27,
{ 152: } 0,
{ 153: } -22,
{ 154: } 0,
{ 155: } 0,
{ 156: } -53,
{ 157: } -55,
{ 158: } 0,
{ 159: } -46,
{ 160: } 0,
{ 161: } 0,
{ 162: } 0,
{ 163: } -35,
{ 164: } -34,
{ 165: } 0,
{ 166: } -28,
{ 167: } -97,
{ 168: } -98,
{ 169: } 0,
{ 170: } 0,
{ 171: } -32,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } 0,
{ 176: } -44,
{ 177: } -40,
{ 178: } 0,
{ 179: } -56,
{ 180: } 0,
{ 181: } 0,
{ 182: } -55,
{ 183: } -47,
{ 184: } -48,
{ 185: } -49,
{ 186: } -42,
{ 187: } -43,
{ 188: } 0,
{ 189: } 0,
{ 190: } 0,
{ 191: } 0,
{ 192: } 0,
{ 193: } 0,
{ 194: } 0,
{ 195: } 0,
{ 196: } -87,
{ 197: } 0,
{ 198: } -51,
{ 199: } -57,
{ 200: } -58,
{ 201: } -59,
{ 202: } -60,
{ 203: } -61,
{ 204: } -62,
{ 205: } -64,
{ 206: } -65,
{ 207: } -66,
{ 208: } -67,
{ 209: } 0,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } -68,
{ 214: } -77,
{ 215: } -76,
{ 216: } 0,
{ 217: } 0,
{ 218: } 0,
{ 219: } 0,
{ 220: } -69,
{ 221: } 0,
{ 222: } -70,
{ 223: } -71,
{ 224: } -72,
{ 225: } -75,
{ 226: } 0,
{ 227: } -80,
{ 228: } -88,
{ 229: } 0,
{ 230: } 0,
{ 231: } -54,
{ 232: } 0,
{ 233: } 0,
{ 234: } -86,
{ 235: } 0,
{ 236: } 0,
{ 237: } 0,
{ 238: } 0,
{ 239: } 0,
{ 240: } 0,
{ 241: } -74,
{ 242: } 0,
{ 243: } -82,
{ 244: } -89,
{ 245: } -91,
{ 246: } -92,
{ 247: } -93,
{ 248: } 0,
{ 249: } -90,
{ 250: } 0,
{ 251: } -84,
{ 252: } 0,
{ 253: } 0,
{ 254: } 0,
{ 255: } 0,
{ 256: } 0,
{ 257: } 0,
{ 258: } 0,
{ 259: } -78
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
{ 12: } 11,
{ 13: } 12,
{ 14: } 12,
{ 15: } 13,
{ 16: } 15,
{ 17: } 16,
{ 18: } 17,
{ 19: } 17,
{ 20: } 19,
{ 21: } 20,
{ 22: } 20,
{ 23: } 21,
{ 24: } 22,
{ 25: } 23,
{ 26: } 23,
{ 27: } 23,
{ 28: } 23,
{ 29: } 26,
{ 30: } 28,
{ 31: } 32,
{ 32: } 33,
{ 33: } 35,
{ 34: } 35,
{ 35: } 35,
{ 36: } 38,
{ 37: } 38,
{ 38: } 38,
{ 39: } 38,
{ 40: } 38,
{ 41: } 38,
{ 42: } 39,
{ 43: } 61,
{ 44: } 64,
{ 45: } 65,
{ 46: } 66,
{ 47: } 66,
{ 48: } 66,
{ 49: } 70,
{ 50: } 74,
{ 51: } 74,
{ 52: } 74,
{ 53: } 74,
{ 54: } 74,
{ 55: } 74,
{ 56: } 75,
{ 57: } 81,
{ 58: } 83,
{ 59: } 88,
{ 60: } 96,
{ 61: } 98,
{ 62: } 99,
{ 63: } 101,
{ 64: } 103,
{ 65: } 105,
{ 66: } 106,
{ 67: } 107,
{ 68: } 108,
{ 69: } 109,
{ 70: } 111,
{ 71: } 113,
{ 72: } 115,
{ 73: } 117,
{ 74: } 119,
{ 75: } 121,
{ 76: } 123,
{ 77: } 124,
{ 78: } 125,
{ 79: } 125,
{ 80: } 126,
{ 81: } 129,
{ 82: } 129,
{ 83: } 129,
{ 84: } 129,
{ 85: } 133,
{ 86: } 134,
{ 87: } 134,
{ 88: } 134,
{ 89: } 134,
{ 90: } 134,
{ 91: } 134,
{ 92: } 134,
{ 93: } 134,
{ 94: } 134,
{ 95: } 134,
{ 96: } 134,
{ 97: } 134,
{ 98: } 134,
{ 99: } 134,
{ 100: } 134,
{ 101: } 134,
{ 102: } 134,
{ 103: } 134,
{ 104: } 134,
{ 105: } 134,
{ 106: } 134,
{ 107: } 134,
{ 108: } 134,
{ 109: } 134,
{ 110: } 134,
{ 111: } 134,
{ 112: } 134,
{ 113: } 134,
{ 114: } 134,
{ 115: } 134,
{ 116: } 134,
{ 117: } 134,
{ 118: } 134,
{ 119: } 134,
{ 120: } 134,
{ 121: } 134,
{ 122: } 134,
{ 123: } 134,
{ 124: } 134,
{ 125: } 134,
{ 126: } 134,
{ 127: } 134,
{ 128: } 134,
{ 129: } 134,
{ 130: } 135,
{ 131: } 135,
{ 132: } 137,
{ 133: } 137,
{ 134: } 137,
{ 135: } 139,
{ 136: } 139,
{ 137: } 140,
{ 138: } 142,
{ 139: } 142,
{ 140: } 142,
{ 141: } 143,
{ 142: } 144,
{ 143: } 149,
{ 144: } 151,
{ 145: } 151,
{ 146: } 155,
{ 147: } 155,
{ 148: } 158,
{ 149: } 163,
{ 150: } 163,
{ 151: } 163,
{ 152: } 163,
{ 153: } 164,
{ 154: } 164,
{ 155: } 165,
{ 156: } 166,
{ 157: } 166,
{ 158: } 166,
{ 159: } 167,
{ 160: } 167,
{ 161: } 168,
{ 162: } 170,
{ 163: } 171,
{ 164: } 171,
{ 165: } 171,
{ 166: } 172,
{ 167: } 172,
{ 168: } 172,
{ 169: } 172,
{ 170: } 175,
{ 171: } 178,
{ 172: } 178,
{ 173: } 179,
{ 174: } 180,
{ 175: } 181,
{ 176: } 183,
{ 177: } 183,
{ 178: } 183,
{ 179: } 184,
{ 180: } 184,
{ 181: } 190,
{ 182: } 191,
{ 183: } 191,
{ 184: } 191,
{ 185: } 191,
{ 186: } 191,
{ 187: } 191,
{ 188: } 191,
{ 189: } 192,
{ 190: } 193,
{ 191: } 194,
{ 192: } 195,
{ 193: } 197,
{ 194: } 199,
{ 195: } 208,
{ 196: } 211,
{ 197: } 211,
{ 198: } 213,
{ 199: } 213,
{ 200: } 213,
{ 201: } 213,
{ 202: } 213,
{ 203: } 213,
{ 204: } 213,
{ 205: } 213,
{ 206: } 213,
{ 207: } 213,
{ 208: } 213,
{ 209: } 213,
{ 210: } 215,
{ 211: } 217,
{ 212: } 219,
{ 213: } 221,
{ 214: } 221,
{ 215: } 221,
{ 216: } 221,
{ 217: } 225,
{ 218: } 227,
{ 219: } 229,
{ 220: } 230,
{ 221: } 230,
{ 222: } 231,
{ 223: } 231,
{ 224: } 231,
{ 225: } 231,
{ 226: } 231,
{ 227: } 232,
{ 228: } 232,
{ 229: } 232,
{ 230: } 233,
{ 231: } 238,
{ 232: } 238,
{ 233: } 239,
{ 234: } 241,
{ 235: } 241,
{ 236: } 242,
{ 237: } 244,
{ 238: } 246,
{ 239: } 255,
{ 240: } 258,
{ 241: } 260,
{ 242: } 260,
{ 243: } 261,
{ 244: } 261,
{ 245: } 261,
{ 246: } 261,
{ 247: } 261,
{ 248: } 261,
{ 249: } 265,
{ 250: } 265,
{ 251: } 268,
{ 252: } 268,
{ 253: } 269,
{ 254: } 272,
{ 255: } 273,
{ 256: } 276,
{ 257: } 277,
{ 258: } 280,
{ 259: } 281
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
{ 11: } 10,
{ 12: } 11,
{ 13: } 11,
{ 14: } 12,
{ 15: } 14,
{ 16: } 15,
{ 17: } 16,
{ 18: } 16,
{ 19: } 18,
{ 20: } 19,
{ 21: } 19,
{ 22: } 20,
{ 23: } 21,
{ 24: } 22,
{ 25: } 22,
{ 26: } 22,
{ 27: } 22,
{ 28: } 25,
{ 29: } 27,
{ 30: } 31,
{ 31: } 32,
{ 32: } 34,
{ 33: } 34,
{ 34: } 34,
{ 35: } 37,
{ 36: } 37,
{ 37: } 37,
{ 38: } 37,
{ 39: } 37,
{ 40: } 37,
{ 41: } 38,
{ 42: } 60,
{ 43: } 63,
{ 44: } 64,
{ 45: } 65,
{ 46: } 65,
{ 47: } 65,
{ 48: } 69,
{ 49: } 73,
{ 50: } 73,
{ 51: } 73,
{ 52: } 73,
{ 53: } 73,
{ 54: } 73,
{ 55: } 74,
{ 56: } 80,
{ 57: } 82,
{ 58: } 87,
{ 59: } 95,
{ 60: } 97,
{ 61: } 98,
{ 62: } 100,
{ 63: } 102,
{ 64: } 104,
{ 65: } 105,
{ 66: } 106,
{ 67: } 107,
{ 68: } 108,
{ 69: } 110,
{ 70: } 112,
{ 71: } 114,
{ 72: } 116,
{ 73: } 118,
{ 74: } 120,
{ 75: } 122,
{ 76: } 123,
{ 77: } 124,
{ 78: } 124,
{ 79: } 125,
{ 80: } 128,
{ 81: } 128,
{ 82: } 128,
{ 83: } 128,
{ 84: } 132,
{ 85: } 133,
{ 86: } 133,
{ 87: } 133,
{ 88: } 133,
{ 89: } 133,
{ 90: } 133,
{ 91: } 133,
{ 92: } 133,
{ 93: } 133,
{ 94: } 133,
{ 95: } 133,
{ 96: } 133,
{ 97: } 133,
{ 98: } 133,
{ 99: } 133,
{ 100: } 133,
{ 101: } 133,
{ 102: } 133,
{ 103: } 133,
{ 104: } 133,
{ 105: } 133,
{ 106: } 133,
{ 107: } 133,
{ 108: } 133,
{ 109: } 133,
{ 110: } 133,
{ 111: } 133,
{ 112: } 133,
{ 113: } 133,
{ 114: } 133,
{ 115: } 133,
{ 116: } 133,
{ 117: } 133,
{ 118: } 133,
{ 119: } 133,
{ 120: } 133,
{ 121: } 133,
{ 122: } 133,
{ 123: } 133,
{ 124: } 133,
{ 125: } 133,
{ 126: } 133,
{ 127: } 133,
{ 128: } 133,
{ 129: } 134,
{ 130: } 134,
{ 131: } 136,
{ 132: } 136,
{ 133: } 136,
{ 134: } 138,
{ 135: } 138,
{ 136: } 139,
{ 137: } 141,
{ 138: } 141,
{ 139: } 141,
{ 140: } 142,
{ 141: } 143,
{ 142: } 148,
{ 143: } 150,
{ 144: } 150,
{ 145: } 154,
{ 146: } 154,
{ 147: } 157,
{ 148: } 162,
{ 149: } 162,
{ 150: } 162,
{ 151: } 162,
{ 152: } 163,
{ 153: } 163,
{ 154: } 164,
{ 155: } 165,
{ 156: } 165,
{ 157: } 165,
{ 158: } 166,
{ 159: } 166,
{ 160: } 167,
{ 161: } 169,
{ 162: } 170,
{ 163: } 170,
{ 164: } 170,
{ 165: } 171,
{ 166: } 171,
{ 167: } 171,
{ 168: } 171,
{ 169: } 174,
{ 170: } 177,
{ 171: } 177,
{ 172: } 178,
{ 173: } 179,
{ 174: } 180,
{ 175: } 182,
{ 176: } 182,
{ 177: } 182,
{ 178: } 183,
{ 179: } 183,
{ 180: } 189,
{ 181: } 190,
{ 182: } 190,
{ 183: } 190,
{ 184: } 190,
{ 185: } 190,
{ 186: } 190,
{ 187: } 190,
{ 188: } 191,
{ 189: } 192,
{ 190: } 193,
{ 191: } 194,
{ 192: } 196,
{ 193: } 198,
{ 194: } 207,
{ 195: } 210,
{ 196: } 210,
{ 197: } 212,
{ 198: } 212,
{ 199: } 212,
{ 200: } 212,
{ 201: } 212,
{ 202: } 212,
{ 203: } 212,
{ 204: } 212,
{ 205: } 212,
{ 206: } 212,
{ 207: } 212,
{ 208: } 212,
{ 209: } 214,
{ 210: } 216,
{ 211: } 218,
{ 212: } 220,
{ 213: } 220,
{ 214: } 220,
{ 215: } 220,
{ 216: } 224,
{ 217: } 226,
{ 218: } 228,
{ 219: } 229,
{ 220: } 229,
{ 221: } 230,
{ 222: } 230,
{ 223: } 230,
{ 224: } 230,
{ 225: } 230,
{ 226: } 231,
{ 227: } 231,
{ 228: } 231,
{ 229: } 232,
{ 230: } 237,
{ 231: } 237,
{ 232: } 238,
{ 233: } 240,
{ 234: } 240,
{ 235: } 241,
{ 236: } 243,
{ 237: } 245,
{ 238: } 254,
{ 239: } 257,
{ 240: } 259,
{ 241: } 259,
{ 242: } 260,
{ 243: } 260,
{ 244: } 260,
{ 245: } 260,
{ 246: } 260,
{ 247: } 260,
{ 248: } 264,
{ 249: } 264,
{ 250: } 267,
{ 251: } 267,
{ 252: } 268,
{ 253: } 271,
{ 254: } 272,
{ 255: } 275,
{ 256: } 276,
{ 257: } 279,
{ 258: } 280,
{ 259: } 280
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
{ 9: } 7,
{ 10: } 8,
{ 11: } 8,
{ 12: } 9,
{ 13: } 11,
{ 14: } 11,
{ 15: } 11,
{ 16: } 12,
{ 17: } 12,
{ 18: } 12,
{ 19: } 12,
{ 20: } 13,
{ 21: } 13,
{ 22: } 14,
{ 23: } 15,
{ 24: } 16,
{ 25: } 16,
{ 26: } 17,
{ 27: } 17,
{ 28: } 17,
{ 29: } 18,
{ 30: } 19,
{ 31: } 20,
{ 32: } 20,
{ 33: } 20,
{ 34: } 20,
{ 35: } 20,
{ 36: } 23,
{ 37: } 23,
{ 38: } 23,
{ 39: } 23,
{ 40: } 23,
{ 41: } 23,
{ 42: } 23,
{ 43: } 24,
{ 44: } 25,
{ 45: } 25,
{ 46: } 25,
{ 47: } 25,
{ 48: } 25,
{ 49: } 26,
{ 50: } 29,
{ 51: } 29,
{ 52: } 29,
{ 53: } 29,
{ 54: } 29,
{ 55: } 29,
{ 56: } 29,
{ 57: } 30,
{ 58: } 31,
{ 59: } 32,
{ 60: } 33,
{ 61: } 34,
{ 62: } 34,
{ 63: } 35,
{ 64: } 36,
{ 65: } 37,
{ 66: } 37,
{ 67: } 37,
{ 68: } 37,
{ 69: } 37,
{ 70: } 38,
{ 71: } 39,
{ 72: } 40,
{ 73: } 41,
{ 74: } 42,
{ 75: } 43,
{ 76: } 44,
{ 77: } 45,
{ 78: } 45,
{ 79: } 46,
{ 80: } 46,
{ 81: } 47,
{ 82: } 47,
{ 83: } 47,
{ 84: } 47,
{ 85: } 48,
{ 86: } 48,
{ 87: } 48,
{ 88: } 48,
{ 89: } 48,
{ 90: } 48,
{ 91: } 48,
{ 92: } 48,
{ 93: } 48,
{ 94: } 48,
{ 95: } 48,
{ 96: } 48,
{ 97: } 48,
{ 98: } 48,
{ 99: } 48,
{ 100: } 48,
{ 101: } 48,
{ 102: } 48,
{ 103: } 48,
{ 104: } 48,
{ 105: } 48,
{ 106: } 48,
{ 107: } 48,
{ 108: } 48,
{ 109: } 48,
{ 110: } 48,
{ 111: } 48,
{ 112: } 48,
{ 113: } 48,
{ 114: } 48,
{ 115: } 48,
{ 116: } 48,
{ 117: } 48,
{ 118: } 48,
{ 119: } 48,
{ 120: } 48,
{ 121: } 48,
{ 122: } 48,
{ 123: } 48,
{ 124: } 48,
{ 125: } 48,
{ 126: } 48,
{ 127: } 48,
{ 128: } 48,
{ 129: } 48,
{ 130: } 49,
{ 131: } 49,
{ 132: } 50,
{ 133: } 51,
{ 134: } 52,
{ 135: } 56,
{ 136: } 56,
{ 137: } 56,
{ 138: } 56,
{ 139: } 56,
{ 140: } 56,
{ 141: } 56,
{ 142: } 57,
{ 143: } 59,
{ 144: } 60,
{ 145: } 60,
{ 146: } 60,
{ 147: } 60,
{ 148: } 63,
{ 149: } 63,
{ 150: } 63,
{ 151: } 63,
{ 152: } 63,
{ 153: } 63,
{ 154: } 63,
{ 155: } 63,
{ 156: } 63,
{ 157: } 63,
{ 158: } 64,
{ 159: } 64,
{ 160: } 64,
{ 161: } 64,
{ 162: } 65,
{ 163: } 66,
{ 164: } 66,
{ 165: } 66,
{ 166: } 67,
{ 167: } 67,
{ 168: } 67,
{ 169: } 67,
{ 170: } 69,
{ 171: } 70,
{ 172: } 70,
{ 173: } 70,
{ 174: } 70,
{ 175: } 70,
{ 176: } 70,
{ 177: } 70,
{ 178: } 70,
{ 179: } 70,
{ 180: } 70,
{ 181: } 71,
{ 182: } 71,
{ 183: } 72,
{ 184: } 72,
{ 185: } 72,
{ 186: } 72,
{ 187: } 72,
{ 188: } 72,
{ 189: } 72,
{ 190: } 72,
{ 191: } 72,
{ 192: } 72,
{ 193: } 75,
{ 194: } 78,
{ 195: } 79,
{ 196: } 82,
{ 197: } 83,
{ 198: } 84,
{ 199: } 84,
{ 200: } 84,
{ 201: } 84,
{ 202: } 84,
{ 203: } 84,
{ 204: } 84,
{ 205: } 84,
{ 206: } 84,
{ 207: } 84,
{ 208: } 84,
{ 209: } 84,
{ 210: } 85,
{ 211: } 86,
{ 212: } 87,
{ 213: } 88,
{ 214: } 88,
{ 215: } 88,
{ 216: } 88,
{ 217: } 90,
{ 218: } 91,
{ 219: } 92,
{ 220: } 92,
{ 221: } 92,
{ 222: } 92,
{ 223: } 92,
{ 224: } 92,
{ 225: } 92,
{ 226: } 92,
{ 227: } 92,
{ 228: } 92,
{ 229: } 92,
{ 230: } 92,
{ 231: } 93,
{ 232: } 93,
{ 233: } 93,
{ 234: } 94,
{ 235: } 94,
{ 236: } 94,
{ 237: } 97,
{ 238: } 100,
{ 239: } 101,
{ 240: } 104,
{ 241: } 107,
{ 242: } 107,
{ 243: } 107,
{ 244: } 107,
{ 245: } 107,
{ 246: } 107,
{ 247: } 107,
{ 248: } 107,
{ 249: } 109,
{ 250: } 109,
{ 251: } 111,
{ 252: } 111,
{ 253: } 111,
{ 254: } 113,
{ 255: } 113,
{ 256: } 115,
{ 257: } 115,
{ 258: } 117,
{ 259: } 117
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
{ 8: } 6,
{ 9: } 7,
{ 10: } 7,
{ 11: } 8,
{ 12: } 10,
{ 13: } 10,
{ 14: } 10,
{ 15: } 11,
{ 16: } 11,
{ 17: } 11,
{ 18: } 11,
{ 19: } 12,
{ 20: } 12,
{ 21: } 13,
{ 22: } 14,
{ 23: } 15,
{ 24: } 15,
{ 25: } 16,
{ 26: } 16,
{ 27: } 16,
{ 28: } 17,
{ 29: } 18,
{ 30: } 19,
{ 31: } 19,
{ 32: } 19,
{ 33: } 19,
{ 34: } 19,
{ 35: } 22,
{ 36: } 22,
{ 37: } 22,
{ 38: } 22,
{ 39: } 22,
{ 40: } 22,
{ 41: } 22,
{ 42: } 23,
{ 43: } 24,
{ 44: } 24,
{ 45: } 24,
{ 46: } 24,
{ 47: } 24,
{ 48: } 25,
{ 49: } 28,
{ 50: } 28,
{ 51: } 28,
{ 52: } 28,
{ 53: } 28,
{ 54: } 28,
{ 55: } 28,
{ 56: } 29,
{ 57: } 30,
{ 58: } 31,
{ 59: } 32,
{ 60: } 33,
{ 61: } 33,
{ 62: } 34,
{ 63: } 35,
{ 64: } 36,
{ 65: } 36,
{ 66: } 36,
{ 67: } 36,
{ 68: } 36,
{ 69: } 37,
{ 70: } 38,
{ 71: } 39,
{ 72: } 40,
{ 73: } 41,
{ 74: } 42,
{ 75: } 43,
{ 76: } 44,
{ 77: } 44,
{ 78: } 45,
{ 79: } 45,
{ 80: } 46,
{ 81: } 46,
{ 82: } 46,
{ 83: } 46,
{ 84: } 47,
{ 85: } 47,
{ 86: } 47,
{ 87: } 47,
{ 88: } 47,
{ 89: } 47,
{ 90: } 47,
{ 91: } 47,
{ 92: } 47,
{ 93: } 47,
{ 94: } 47,
{ 95: } 47,
{ 96: } 47,
{ 97: } 47,
{ 98: } 47,
{ 99: } 47,
{ 100: } 47,
{ 101: } 47,
{ 102: } 47,
{ 103: } 47,
{ 104: } 47,
{ 105: } 47,
{ 106: } 47,
{ 107: } 47,
{ 108: } 47,
{ 109: } 47,
{ 110: } 47,
{ 111: } 47,
{ 112: } 47,
{ 113: } 47,
{ 114: } 47,
{ 115: } 47,
{ 116: } 47,
{ 117: } 47,
{ 118: } 47,
{ 119: } 47,
{ 120: } 47,
{ 121: } 47,
{ 122: } 47,
{ 123: } 47,
{ 124: } 47,
{ 125: } 47,
{ 126: } 47,
{ 127: } 47,
{ 128: } 47,
{ 129: } 48,
{ 130: } 48,
{ 131: } 49,
{ 132: } 50,
{ 133: } 51,
{ 134: } 55,
{ 135: } 55,
{ 136: } 55,
{ 137: } 55,
{ 138: } 55,
{ 139: } 55,
{ 140: } 55,
{ 141: } 56,
{ 142: } 58,
{ 143: } 59,
{ 144: } 59,
{ 145: } 59,
{ 146: } 59,
{ 147: } 62,
{ 148: } 62,
{ 149: } 62,
{ 150: } 62,
{ 151: } 62,
{ 152: } 62,
{ 153: } 62,
{ 154: } 62,
{ 155: } 62,
{ 156: } 62,
{ 157: } 63,
{ 158: } 63,
{ 159: } 63,
{ 160: } 63,
{ 161: } 64,
{ 162: } 65,
{ 163: } 65,
{ 164: } 65,
{ 165: } 66,
{ 166: } 66,
{ 167: } 66,
{ 168: } 66,
{ 169: } 68,
{ 170: } 69,
{ 171: } 69,
{ 172: } 69,
{ 173: } 69,
{ 174: } 69,
{ 175: } 69,
{ 176: } 69,
{ 177: } 69,
{ 178: } 69,
{ 179: } 69,
{ 180: } 70,
{ 181: } 70,
{ 182: } 71,
{ 183: } 71,
{ 184: } 71,
{ 185: } 71,
{ 186: } 71,
{ 187: } 71,
{ 188: } 71,
{ 189: } 71,
{ 190: } 71,
{ 191: } 71,
{ 192: } 74,
{ 193: } 77,
{ 194: } 78,
{ 195: } 81,
{ 196: } 82,
{ 197: } 83,
{ 198: } 83,
{ 199: } 83,
{ 200: } 83,
{ 201: } 83,
{ 202: } 83,
{ 203: } 83,
{ 204: } 83,
{ 205: } 83,
{ 206: } 83,
{ 207: } 83,
{ 208: } 83,
{ 209: } 84,
{ 210: } 85,
{ 211: } 86,
{ 212: } 87,
{ 213: } 87,
{ 214: } 87,
{ 215: } 87,
{ 216: } 89,
{ 217: } 90,
{ 218: } 91,
{ 219: } 91,
{ 220: } 91,
{ 221: } 91,
{ 222: } 91,
{ 223: } 91,
{ 224: } 91,
{ 225: } 91,
{ 226: } 91,
{ 227: } 91,
{ 228: } 91,
{ 229: } 91,
{ 230: } 92,
{ 231: } 92,
{ 232: } 92,
{ 233: } 93,
{ 234: } 93,
{ 235: } 93,
{ 236: } 96,
{ 237: } 99,
{ 238: } 100,
{ 239: } 103,
{ 240: } 106,
{ 241: } 106,
{ 242: } 106,
{ 243: } 106,
{ 244: } 106,
{ 245: } 106,
{ 246: } 106,
{ 247: } 106,
{ 248: } 108,
{ 249: } 108,
{ 250: } 110,
{ 251: } 110,
{ 252: } 110,
{ 253: } 112,
{ 254: } 112,
{ 255: } 114,
{ 256: } 114,
{ 257: } 116,
{ 258: } 116,
{ 259: } 116
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -54 ),
{ 2: } ( len: 0; sym: -55 ),
{ 3: } ( len: 5; sym: -53 ),
{ 4: } ( len: 7; sym: -2 ),
{ 5: } ( len: 3; sym: -5 ),
{ 6: } ( len: 2; sym: -6 ),
{ 7: } ( len: 1; sym: -6 ),
{ 8: } ( len: 3; sym: -7 ),
{ 9: } ( len: 1; sym: -8 ),
{ 10: } ( len: 1; sym: -8 ),
{ 11: } ( len: 1; sym: -8 ),
{ 12: } ( len: 0; sym: -57 ),
{ 13: } ( len: 0; sym: -58 ),
{ 14: } ( len: 7; sym: -9 ),
{ 15: } ( len: 3; sym: -56 ),
{ 16: } ( len: 2; sym: -56 ),
{ 17: } ( len: 2; sym: -59 ),
{ 18: } ( len: 1; sym: -59 ),
{ 19: } ( len: 1; sym: -10 ),
{ 20: } ( len: 1; sym: -10 ),
{ 21: } ( len: 1; sym: -10 ),
{ 22: } ( len: 10; sym: -3 ),
{ 23: } ( len: 0; sym: -11 ),
{ 24: } ( len: 3; sym: -11 ),
{ 25: } ( len: 2; sym: -13 ),
{ 26: } ( len: 1; sym: -13 ),
{ 27: } ( len: 3; sym: -14 ),
{ 28: } ( len: 3; sym: -15 ),
{ 29: } ( len: 1; sym: -15 ),
{ 30: } ( len: 3; sym: -12 ),
{ 31: } ( len: 0; sym: -16 ),
{ 32: } ( len: 6; sym: -16 ),
{ 33: } ( len: 0; sym: -17 ),
{ 34: } ( len: 3; sym: -17 ),
{ 35: } ( len: 2; sym: -18 ),
{ 36: } ( len: 1; sym: -18 ),
{ 37: } ( len: 1; sym: -19 ),
{ 38: } ( len: 1; sym: -19 ),
{ 39: } ( len: 1; sym: -19 ),
{ 40: } ( len: 3; sym: -20 ),
{ 41: } ( len: 1; sym: -20 ),
{ 42: } ( len: 4; sym: -21 ),
{ 43: } ( len: 2; sym: -22 ),
{ 44: } ( len: 1; sym: -22 ),
{ 45: } ( len: 0; sym: -23 ),
{ 46: } ( len: 2; sym: -23 ),
{ 47: } ( len: 3; sym: -24 ),
{ 48: } ( len: 2; sym: -25 ),
{ 49: } ( len: 2; sym: -25 ),
{ 50: } ( len: 0; sym: -26 ),
{ 51: } ( len: 8; sym: -26 ),
{ 52: } ( len: 0; sym: -27 ),
{ 53: } ( len: 2; sym: -27 ),
{ 54: } ( len: 6; sym: -28 ),
{ 55: } ( len: 0; sym: -29 ),
{ 56: } ( len: 2; sym: -29 ),
{ 57: } ( len: 3; sym: -30 ),
{ 58: } ( len: 2; sym: -31 ),
{ 59: } ( len: 2; sym: -31 ),
{ 60: } ( len: 2; sym: -31 ),
{ 61: } ( len: 2; sym: -31 ),
{ 62: } ( len: 2; sym: -31 ),
{ 63: } ( len: 2; sym: -31 ),
{ 64: } ( len: 1; sym: -32 ),
{ 65: } ( len: 1; sym: -32 ),
{ 66: } ( len: 1; sym: -32 ),
{ 67: } ( len: 1; sym: -32 ),
{ 68: } ( len: 1; sym: -32 ),
{ 69: } ( len: 2; sym: -32 ),
{ 70: } ( len: 2; sym: -32 ),
{ 71: } ( len: 2; sym: -32 ),
{ 72: } ( len: 2; sym: -32 ),
{ 73: } ( len: 0; sym: -33 ),
{ 74: } ( len: 3; sym: -33 ),
{ 75: } ( len: 2; sym: -34 ),
{ 76: } ( len: 1; sym: -34 ),
{ 77: } ( len: 1; sym: -35 ),
{ 78: } ( len: 13; sym: -35 ),
{ 79: } ( len: 0; sym: -36 ),
{ 80: } ( len: 1; sym: -36 ),
{ 81: } ( len: 0; sym: -38 ),
{ 82: } ( len: 1; sym: -38 ),
{ 83: } ( len: 0; sym: -37 ),
{ 84: } ( len: 1; sym: -37 ),
{ 85: } ( len: 0; sym: -39 ),
{ 86: } ( len: 5; sym: -39 ),
{ 87: } ( len: 0; sym: -40 ),
{ 88: } ( len: 2; sym: -40 ),
{ 89: } ( len: 3; sym: -41 ),
{ 90: } ( len: 2; sym: -42 ),
{ 91: } ( len: 2; sym: -42 ),
{ 92: } ( len: 2; sym: -42 ),
{ 93: } ( len: 2; sym: -42 ),
{ 94: } ( len: 2; sym: -42 ),
{ 95: } ( len: 0; sym: -43 ),
{ 96: } ( len: 2; sym: -43 ),
{ 97: } ( len: 3; sym: -44 ),
{ 98: } ( len: 2; sym: -45 ),
{ 99: } ( len: 5; sym: -4 ),
{ 100: } ( len: 0; sym: -46 ),
{ 101: } ( len: 2; sym: -46 ),
{ 102: } ( len: 3; sym: -47 ),
{ 103: } ( len: 2; sym: -48 ),
{ 104: } ( len: 2; sym: -48 ),
{ 105: } ( len: 2; sym: -48 ),
{ 106: } ( len: 2; sym: -48 ),
{ 107: } ( len: 2; sym: -48 ),
{ 108: } ( len: 2; sym: -48 ),
{ 109: } ( len: 2; sym: -48 ),
{ 110: } ( len: 2; sym: -48 ),
{ 111: } ( len: 2; sym: -48 ),
{ 112: } ( len: 2; sym: -48 ),
{ 113: } ( len: 2; sym: -48 ),
{ 114: } ( len: 2; sym: -48 ),
{ 115: } ( len: 2; sym: -48 ),
{ 116: } ( len: 2; sym: -48 ),
{ 117: } ( len: 2; sym: -48 ),
{ 118: } ( len: 2; sym: -48 ),
{ 119: } ( len: 2; sym: -48 ),
{ 120: } ( len: 2; sym: -48 ),
{ 121: } ( len: 2; sym: -48 ),
{ 122: } ( len: 2; sym: -48 ),
{ 123: } ( len: 2; sym: -48 ),
{ 124: } ( len: 2; sym: -48 ),
{ 125: } ( len: 1; sym: -49 ),
{ 126: } ( len: 1; sym: -49 ),
{ 127: } ( len: 1; sym: -49 ),
{ 128: } ( len: 1; sym: -49 ),
{ 129: } ( len: 1; sym: -49 ),
{ 130: } ( len: 1; sym: -49 ),
{ 131: } ( len: 1; sym: -50 ),
{ 132: } ( len: 1; sym: -50 ),
{ 133: } ( len: 1; sym: -50 ),
{ 134: } ( len: 1; sym: -50 ),
{ 135: } ( len: 1; sym: -50 ),
{ 136: } ( len: 1; sym: -51 ),
{ 137: } ( len: 1; sym: -51 ),
{ 138: } ( len: 1; sym: -51 ),
{ 139: } ( len: 1; sym: -51 ),
{ 140: } ( len: 1; sym: -51 ),
{ 141: } ( len: 1; sym: -51 ),
{ 142: } ( len: 1; sym: -51 ),
{ 143: } ( len: 1; sym: -51 ),
{ 144: } ( len: 1; sym: -52 ),
{ 145: } ( len: 1; sym: -52 )
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

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
const T_GENERAR_SALIDA_COMPACTA = 345;
const T_MOSTRAR_SALIDA_CONSOLA = 346;
const T_ARCHIVO_RESUMEN_VEROSIMILITUD = 347;
const T_ARCHIVO_RESUMEN_BETA = 348;
const T_ARCHIVO_RESUMEN_THETA = 349;
const T_ARCHIVO_RESUMEN_ITERACION = 350;

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
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_salida_compacta', yyv[yysp-0].yyBoolean); 
       end;
 126 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('mostrar_salida_consola', yyv[yysp-0].yyBoolean); 
       end;
 127 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_verosimilitud', yyv[yysp-0].yyShortString); 
       end;
 128 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_beta', yyv[yysp-0].yyShortString); 
       end;
 129 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_theta', yyv[yysp-0].yyShortString); 
       end;
 130 : begin
         yyval.yyTOpcion := TOpcionParametroString.Create('archivo_resumen_iteracion', yyv[yysp-0].yyShortString); 
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
         yyval := yyv[yysp-0];
       end;
 145 : begin
         yyval := yyv[yysp-0];
       end;
 146 : begin
         yyval := yyv[yysp-0];
       end;
 147 : begin
         yyval := yyv[yysp-0];
       end;
 148 : begin
         yyval := yyv[yysp-0];
       end;
 149 : begin
         yyval := yyv[yysp-0];
       end;
 150 : begin
         yyval.yyReal := yyv[yysp-0].yyInteger; 
       end;
 151 : begin
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

yynacts   = 292;
yyngotos  = 116;
yynstates = 272;
yynrules  = 151;

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
  ( sym: 345; act: 78 ),
  ( sym: 346; act: 79 ),
  ( sym: 347; act: 80 ),
  ( sym: 348; act: 81 ),
  ( sym: 349; act: 82 ),
  ( sym: 350; act: 83 ),
{ 43: }
  ( sym: 287; act: 85 ),
  ( sym: 258; act: -50 ),
  ( sym: 284; act: -50 ),
{ 44: }
  ( sym: 257; act: 86 ),
{ 45: }
  ( sym: 265; act: 87 ),
{ 46: }
{ 47: }
{ 48: }
  ( sym: 265; act: 89 ),
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
  ( sym: 265; act: 92 ),
{ 56: }
  ( sym: 307; act: 94 ),
  ( sym: 308; act: 95 ),
  ( sym: 309; act: 96 ),
  ( sym: 310; act: 97 ),
  ( sym: 311; act: 98 ),
  ( sym: 343; act: 99 ),
{ 57: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 58: }
  ( sym: 307; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 311; act: 106 ),
  ( sym: 314; act: 107 ),
  ( sym: 315; act: 108 ),
{ 59: }
  ( sym: 317; act: 110 ),
  ( sym: 318; act: 111 ),
  ( sym: 319; act: 112 ),
  ( sym: 320; act: 113 ),
  ( sym: 321; act: 114 ),
  ( sym: 322; act: 115 ),
  ( sym: 323; act: 116 ),
  ( sym: 324; act: 117 ),
{ 60: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 61: }
  ( sym: 267; act: 119 ),
{ 62: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 63: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 64: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 65: }
  ( sym: 271; act: 123 ),
{ 66: }
  ( sym: 271; act: 124 ),
{ 67: }
  ( sym: 267; act: 125 ),
{ 68: }
  ( sym: 267; act: 126 ),
{ 69: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 70: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 71: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 72: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 73: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 74: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 75: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 76: }
  ( sym: 261; act: 135 ),
{ 77: }
  ( sym: 270; act: 136 ),
{ 78: }
  ( sym: 271; act: 137 ),
{ 79: }
  ( sym: 271; act: 138 ),
{ 80: }
  ( sym: 270; act: 139 ),
{ 81: }
  ( sym: 270; act: 140 ),
{ 82: }
  ( sym: 270; act: 141 ),
{ 83: }
  ( sym: 270; act: 142 ),
{ 84: }
{ 85: }
  ( sym: 257; act: 144 ),
{ 86: }
  ( sym: 282; act: 146 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
{ 87: }
{ 88: }
{ 89: }
{ 90: }
  ( sym: 265; act: 147 ),
  ( sym: 267; act: 51 ),
  ( sym: 268; act: 52 ),
  ( sym: 269; act: 53 ),
{ 91: }
  ( sym: 258; act: 148 ),
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
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: 269; act: 150 ),
{ 136: }
{ 137: }
{ 138: }
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
  ( sym: 258; act: 152 ),
  ( sym: 284; act: 153 ),
{ 144: }
{ 145: }
{ 146: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 147: }
{ 148: }
  ( sym: 265; act: 162 ),
{ 149: }
  ( sym: 262; act: 163 ),
  ( sym: 264; act: 164 ),
{ 150: }
{ 151: }
{ 152: }
  ( sym: 265; act: 165 ),
{ 153: }
  ( sym: 304; act: 167 ),
{ 154: }
  ( sym: 282; act: 146 ),
  ( sym: 288; act: 170 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
  ( sym: 302; act: -33 ),
{ 155: }
  ( sym: 258; act: 172 ),
  ( sym: 284; act: 173 ),
{ 156: }
{ 157: }
  ( sym: 259; act: 174 ),
  ( sym: 265; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 283; act: -38 ),
{ 158: }
{ 159: }
  ( sym: 265; act: 176 ),
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 160: }
  ( sym: 266; act: 177 ),
  ( sym: 259; act: -41 ),
  ( sym: 265; act: -41 ),
  ( sym: 269; act: -41 ),
  ( sym: 283; act: -41 ),
{ 161: }
{ 162: }
{ 163: }
{ 164: }
  ( sym: 269; act: 178 ),
{ 165: }
{ 166: }
  ( sym: 265; act: 179 ),
{ 167: }
  ( sym: 269; act: 180 ),
{ 168: }
{ 169: }
{ 170: }
  ( sym: 257; act: 182 ),
{ 171: }
{ 172: }
  ( sym: 265; act: 183 ),
{ 173: }
  ( sym: 285; act: 185 ),
  ( sym: 286; act: 186 ),
{ 174: }
  ( sym: 269; act: 188 ),
{ 175: }
{ 176: }
{ 177: }
  ( sym: 269; act: 160 ),
{ 178: }
{ 179: }
{ 180: }
{ 181: }
  ( sym: 284; act: 192 ),
  ( sym: 302; act: 193 ),
  ( sym: 258; act: -85 ),
{ 182: }
  ( sym: 282; act: 146 ),
  ( sym: 258; act: -33 ),
  ( sym: 284; act: -33 ),
{ 183: }
{ 184: }
  ( sym: 265; act: 195 ),
{ 185: }
  ( sym: 271; act: 196 ),
{ 186: }
  ( sym: 271; act: 197 ),
{ 187: }
  ( sym: 260; act: 198 ),
  ( sym: 269; act: 199 ),
{ 188: }
{ 189: }
{ 190: }
  ( sym: 258; act: 200 ),
{ 191: }
{ 192: }
  ( sym: 285; act: 202 ),
  ( sym: 286; act: 203 ),
  ( sym: 289; act: 204 ),
  ( sym: 290; act: 205 ),
  ( sym: 291; act: 206 ),
  ( sym: 301; act: 207 ),
{ 193: }
  ( sym: 257; act: 208 ),
{ 194: }
{ 195: }
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
  ( sym: 265; act: 210 ),
{ 201: }
  ( sym: 265; act: 211 ),
{ 202: }
  ( sym: 271; act: 212 ),
{ 203: }
  ( sym: 271; act: 213 ),
{ 204: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 205: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 206: }
  ( sym: 292; act: 217 ),
  ( sym: 293; act: 218 ),
  ( sym: 294; act: 219 ),
  ( sym: 295; act: 220 ),
  ( sym: 296; act: 221 ),
  ( sym: 297; act: 222 ),
  ( sym: 298; act: 223 ),
  ( sym: 299; act: 224 ),
  ( sym: 300; act: 225 ),
{ 207: }
  ( sym: 259; act: 229 ),
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 208: }
{ 209: }
  ( sym: 258; act: 231 ),
  ( sym: 284; act: 192 ),
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
{ 217: }
{ 218: }
{ 219: }
{ 220: }
{ 221: }
  ( sym: 259; act: 233 ),
  ( sym: 265; act: -73 ),
{ 222: }
  ( sym: 259; act: 233 ),
  ( sym: 265; act: -73 ),
{ 223: }
  ( sym: 259; act: 233 ),
  ( sym: 265; act: -73 ),
{ 224: }
  ( sym: 259; act: 233 ),
  ( sym: 265; act: -73 ),
{ 225: }
{ 226: }
{ 227: }
{ 228: }
  ( sym: 259; act: 229 ),
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 265; act: -63 ),
{ 229: }
  ( sym: 267; act: 239 ),
  ( sym: 264; act: -79 ),
{ 230: }
  ( sym: 258; act: 241 ),
  ( sym: 284; act: 242 ),
{ 231: }
  ( sym: 265; act: 243 ),
{ 232: }
{ 233: }
  ( sym: 267; act: 244 ),
{ 234: }
{ 235: }
{ 236: }
{ 237: }
{ 238: }
  ( sym: 264; act: 245 ),
{ 239: }
{ 240: }
{ 241: }
  ( sym: 265; act: 246 ),
{ 242: }
  ( sym: 289; act: 248 ),
  ( sym: 290; act: 249 ),
  ( sym: 291; act: 250 ),
  ( sym: 301; act: 251 ),
  ( sym: 303; act: 252 ),
{ 243: }
{ 244: }
  ( sym: 260; act: 253 ),
{ 245: }
  ( sym: 269; act: 255 ),
  ( sym: 264; act: -81 ),
{ 246: }
{ 247: }
  ( sym: 265; act: 256 ),
{ 248: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 249: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 250: }
  ( sym: 292; act: 217 ),
  ( sym: 293; act: 218 ),
  ( sym: 294; act: 219 ),
  ( sym: 295; act: 220 ),
  ( sym: 296; act: 221 ),
  ( sym: 297; act: 222 ),
  ( sym: 298; act: 223 ),
  ( sym: 299; act: 224 ),
  ( sym: 300; act: 225 ),
{ 251: }
  ( sym: 259; act: 229 ),
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
{ 252: }
  ( sym: 269; act: 160 ),
  ( sym: 283; act: 161 ),
{ 253: }
{ 254: }
  ( sym: 264; act: 262 ),
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
  ( sym: 259; act: 229 ),
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 265; act: -94 ),
{ 261: }
{ 262: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 264; act: -83 ),
{ 263: }
{ 264: }
  ( sym: 264; act: 265 ),
{ 265: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 264; act: -83 ),
{ 266: }
  ( sym: 264; act: 267 ),
{ 267: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 264; act: -83 ),
{ 268: }
  ( sym: 264; act: 269 ),
{ 269: }
  ( sym: 267; act: 101 ),
  ( sym: 268; act: 102 ),
  ( sym: 260; act: -83 ),
{ 270: }
  ( sym: 260; act: 271 )
{ 271: }
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
  ( sym: -26; act: 84 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: -10; act: 88 ),
{ 49: }
  ( sym: -59; act: 90 ),
  ( sym: -58; act: 91 ),
  ( sym: -10; act: 50 ),
{ 50: }
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
{ 56: }
  ( sym: -49; act: 93 ),
{ 57: }
  ( sym: -52; act: 100 ),
{ 58: }
  ( sym: -50; act: 103 ),
{ 59: }
  ( sym: -51; act: 109 ),
{ 60: }
  ( sym: -52; act: 118 ),
{ 61: }
{ 62: }
  ( sym: -52; act: 120 ),
{ 63: }
  ( sym: -52; act: 121 ),
{ 64: }
  ( sym: -52; act: 122 ),
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
  ( sym: -52; act: 127 ),
{ 70: }
  ( sym: -52; act: 128 ),
{ 71: }
  ( sym: -52; act: 129 ),
{ 72: }
  ( sym: -52; act: 130 ),
{ 73: }
  ( sym: -52; act: 131 ),
{ 74: }
  ( sym: -52; act: 132 ),
{ 75: }
  ( sym: -52; act: 133 ),
{ 76: }
  ( sym: -14; act: 134 ),
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: -43; act: 143 ),
{ 85: }
{ 86: }
  ( sym: -17; act: 145 ),
{ 87: }
{ 88: }
{ 89: }
{ 90: }
  ( sym: -10; act: 88 ),
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
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: -15; act: 149 ),
{ 136: }
{ 137: }
{ 138: }
{ 139: }
{ 140: }
{ 141: }
{ 142: }
{ 143: }
  ( sym: -44; act: 151 ),
{ 144: }
  ( sym: -27; act: 154 ),
{ 145: }
  ( sym: -23; act: 155 ),
{ 146: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 158 ),
  ( sym: -18; act: 159 ),
{ 147: }
{ 148: }
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
  ( sym: -45; act: 166 ),
{ 154: }
  ( sym: -28; act: 168 ),
  ( sym: -17; act: 169 ),
{ 155: }
  ( sym: -24; act: 171 ),
{ 156: }
{ 157: }
{ 158: }
{ 159: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 175 ),
{ 160: }
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
{ 168: }
{ 169: }
  ( sym: -29; act: 181 ),
{ 170: }
{ 171: }
{ 172: }
{ 173: }
  ( sym: -25; act: 184 ),
{ 174: }
  ( sym: -22; act: 187 ),
{ 175: }
{ 176: }
{ 177: }
  ( sym: -20; act: 189 ),
{ 178: }
{ 179: }
{ 180: }
{ 181: }
  ( sym: -39; act: 190 ),
  ( sym: -30; act: 191 ),
{ 182: }
  ( sym: -17; act: 194 ),
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
  ( sym: -31; act: 201 ),
{ 193: }
{ 194: }
  ( sym: -29; act: 209 ),
{ 195: }
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 214 ),
{ 205: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 215 ),
{ 206: }
  ( sym: -32; act: 216 ),
{ 207: }
  ( sym: -52; act: 226 ),
  ( sym: -35; act: 227 ),
  ( sym: -34; act: 228 ),
{ 208: }
  ( sym: -40; act: 230 ),
{ 209: }
  ( sym: -30; act: 191 ),
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
{ 217: }
{ 218: }
{ 219: }
{ 220: }
{ 221: }
  ( sym: -33; act: 232 ),
{ 222: }
  ( sym: -33; act: 234 ),
{ 223: }
  ( sym: -33; act: 235 ),
{ 224: }
  ( sym: -33; act: 236 ),
{ 225: }
{ 226: }
{ 227: }
{ 228: }
  ( sym: -52; act: 226 ),
  ( sym: -35; act: 237 ),
{ 229: }
  ( sym: -36; act: 238 ),
{ 230: }
  ( sym: -41; act: 240 ),
{ 231: }
{ 232: }
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
  ( sym: -42; act: 247 ),
{ 243: }
{ 244: }
{ 245: }
  ( sym: -38; act: 254 ),
{ 246: }
{ 247: }
{ 248: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 257 ),
{ 249: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 258 ),
{ 250: }
  ( sym: -32; act: 259 ),
{ 251: }
  ( sym: -52; act: 226 ),
  ( sym: -35; act: 227 ),
  ( sym: -34; act: 260 ),
{ 252: }
  ( sym: -21; act: 156 ),
  ( sym: -20; act: 157 ),
  ( sym: -19; act: 261 ),
{ 253: }
{ 254: }
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
  ( sym: -52; act: 226 ),
  ( sym: -35; act: 237 ),
{ 261: }
{ 262: }
  ( sym: -52; act: 263 ),
  ( sym: -37; act: 264 ),
{ 263: }
{ 264: }
{ 265: }
  ( sym: -52; act: 263 ),
  ( sym: -37; act: 266 ),
{ 266: }
{ 267: }
  ( sym: -52; act: 263 ),
  ( sym: -37; act: 268 ),
{ 268: }
{ 269: }
  ( sym: -52; act: 263 ),
  ( sym: -37; act: 270 )
{ 270: }
{ 271: }
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
{ 78: } 0,
{ 79: } 0,
{ 80: } 0,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } -95,
{ 85: } 0,
{ 86: } 0,
{ 87: } -30,
{ 88: } -17,
{ 89: } -16,
{ 90: } 0,
{ 91: } 0,
{ 92: } -102,
{ 93: } -103,
{ 94: } -133,
{ 95: } -131,
{ 96: } -132,
{ 97: } -134,
{ 98: } -135,
{ 99: } -136,
{ 100: } -104,
{ 101: } -150,
{ 102: } -151,
{ 103: } -105,
{ 104: } -139,
{ 105: } -140,
{ 106: } -141,
{ 107: } -137,
{ 108: } -138,
{ 109: } -106,
{ 110: } -142,
{ 111: } -143,
{ 112: } -144,
{ 113: } -145,
{ 114: } -146,
{ 115: } -147,
{ 116: } -148,
{ 117: } -149,
{ 118: } -107,
{ 119: } -108,
{ 120: } -109,
{ 121: } -110,
{ 122: } -111,
{ 123: } -112,
{ 124: } -113,
{ 125: } -114,
{ 126: } -115,
{ 127: } -116,
{ 128: } -117,
{ 129: } -118,
{ 130: } -119,
{ 131: } -120,
{ 132: } -121,
{ 133: } -122,
{ 134: } -123,
{ 135: } 0,
{ 136: } -124,
{ 137: } -125,
{ 138: } -126,
{ 139: } -127,
{ 140: } -128,
{ 141: } -129,
{ 142: } -130,
{ 143: } 0,
{ 144: } -52,
{ 145: } -45,
{ 146: } 0,
{ 147: } -15,
{ 148: } 0,
{ 149: } 0,
{ 150: } -29,
{ 151: } -96,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } -39,
{ 157: } 0,
{ 158: } -36,
{ 159: } 0,
{ 160: } 0,
{ 161: } -37,
{ 162: } -14,
{ 163: } -27,
{ 164: } 0,
{ 165: } -22,
{ 166: } 0,
{ 167: } 0,
{ 168: } -53,
{ 169: } -55,
{ 170: } 0,
{ 171: } -46,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } -35,
{ 176: } -34,
{ 177: } 0,
{ 178: } -28,
{ 179: } -97,
{ 180: } -98,
{ 181: } 0,
{ 182: } 0,
{ 183: } -32,
{ 184: } 0,
{ 185: } 0,
{ 186: } 0,
{ 187: } 0,
{ 188: } -44,
{ 189: } -40,
{ 190: } 0,
{ 191: } -56,
{ 192: } 0,
{ 193: } 0,
{ 194: } -55,
{ 195: } -47,
{ 196: } -48,
{ 197: } -49,
{ 198: } -42,
{ 199: } -43,
{ 200: } 0,
{ 201: } 0,
{ 202: } 0,
{ 203: } 0,
{ 204: } 0,
{ 205: } 0,
{ 206: } 0,
{ 207: } 0,
{ 208: } -87,
{ 209: } 0,
{ 210: } -51,
{ 211: } -57,
{ 212: } -58,
{ 213: } -59,
{ 214: } -60,
{ 215: } -61,
{ 216: } -62,
{ 217: } -64,
{ 218: } -65,
{ 219: } -66,
{ 220: } -67,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } -68,
{ 226: } -77,
{ 227: } -76,
{ 228: } 0,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } -69,
{ 233: } 0,
{ 234: } -70,
{ 235: } -71,
{ 236: } -72,
{ 237: } -75,
{ 238: } 0,
{ 239: } -80,
{ 240: } -88,
{ 241: } 0,
{ 242: } 0,
{ 243: } -54,
{ 244: } 0,
{ 245: } 0,
{ 246: } -86,
{ 247: } 0,
{ 248: } 0,
{ 249: } 0,
{ 250: } 0,
{ 251: } 0,
{ 252: } 0,
{ 253: } -74,
{ 254: } 0,
{ 255: } -82,
{ 256: } -89,
{ 257: } -91,
{ 258: } -92,
{ 259: } -93,
{ 260: } 0,
{ 261: } -90,
{ 262: } 0,
{ 263: } -84,
{ 264: } 0,
{ 265: } 0,
{ 266: } 0,
{ 267: } 0,
{ 268: } 0,
{ 269: } 0,
{ 270: } 0,
{ 271: } -78
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
{ 43: } 67,
{ 44: } 70,
{ 45: } 71,
{ 46: } 72,
{ 47: } 72,
{ 48: } 72,
{ 49: } 76,
{ 50: } 80,
{ 51: } 80,
{ 52: } 80,
{ 53: } 80,
{ 54: } 80,
{ 55: } 80,
{ 56: } 81,
{ 57: } 87,
{ 58: } 89,
{ 59: } 94,
{ 60: } 102,
{ 61: } 104,
{ 62: } 105,
{ 63: } 107,
{ 64: } 109,
{ 65: } 111,
{ 66: } 112,
{ 67: } 113,
{ 68: } 114,
{ 69: } 115,
{ 70: } 117,
{ 71: } 119,
{ 72: } 121,
{ 73: } 123,
{ 74: } 125,
{ 75: } 127,
{ 76: } 129,
{ 77: } 130,
{ 78: } 131,
{ 79: } 132,
{ 80: } 133,
{ 81: } 134,
{ 82: } 135,
{ 83: } 136,
{ 84: } 137,
{ 85: } 137,
{ 86: } 138,
{ 87: } 141,
{ 88: } 141,
{ 89: } 141,
{ 90: } 141,
{ 91: } 145,
{ 92: } 146,
{ 93: } 146,
{ 94: } 146,
{ 95: } 146,
{ 96: } 146,
{ 97: } 146,
{ 98: } 146,
{ 99: } 146,
{ 100: } 146,
{ 101: } 146,
{ 102: } 146,
{ 103: } 146,
{ 104: } 146,
{ 105: } 146,
{ 106: } 146,
{ 107: } 146,
{ 108: } 146,
{ 109: } 146,
{ 110: } 146,
{ 111: } 146,
{ 112: } 146,
{ 113: } 146,
{ 114: } 146,
{ 115: } 146,
{ 116: } 146,
{ 117: } 146,
{ 118: } 146,
{ 119: } 146,
{ 120: } 146,
{ 121: } 146,
{ 122: } 146,
{ 123: } 146,
{ 124: } 146,
{ 125: } 146,
{ 126: } 146,
{ 127: } 146,
{ 128: } 146,
{ 129: } 146,
{ 130: } 146,
{ 131: } 146,
{ 132: } 146,
{ 133: } 146,
{ 134: } 146,
{ 135: } 146,
{ 136: } 147,
{ 137: } 147,
{ 138: } 147,
{ 139: } 147,
{ 140: } 147,
{ 141: } 147,
{ 142: } 147,
{ 143: } 147,
{ 144: } 149,
{ 145: } 149,
{ 146: } 149,
{ 147: } 151,
{ 148: } 151,
{ 149: } 152,
{ 150: } 154,
{ 151: } 154,
{ 152: } 154,
{ 153: } 155,
{ 154: } 156,
{ 155: } 161,
{ 156: } 163,
{ 157: } 163,
{ 158: } 167,
{ 159: } 167,
{ 160: } 170,
{ 161: } 175,
{ 162: } 175,
{ 163: } 175,
{ 164: } 175,
{ 165: } 176,
{ 166: } 176,
{ 167: } 177,
{ 168: } 178,
{ 169: } 178,
{ 170: } 178,
{ 171: } 179,
{ 172: } 179,
{ 173: } 180,
{ 174: } 182,
{ 175: } 183,
{ 176: } 183,
{ 177: } 183,
{ 178: } 184,
{ 179: } 184,
{ 180: } 184,
{ 181: } 184,
{ 182: } 187,
{ 183: } 190,
{ 184: } 190,
{ 185: } 191,
{ 186: } 192,
{ 187: } 193,
{ 188: } 195,
{ 189: } 195,
{ 190: } 195,
{ 191: } 196,
{ 192: } 196,
{ 193: } 202,
{ 194: } 203,
{ 195: } 203,
{ 196: } 203,
{ 197: } 203,
{ 198: } 203,
{ 199: } 203,
{ 200: } 203,
{ 201: } 204,
{ 202: } 205,
{ 203: } 206,
{ 204: } 207,
{ 205: } 209,
{ 206: } 211,
{ 207: } 220,
{ 208: } 223,
{ 209: } 223,
{ 210: } 225,
{ 211: } 225,
{ 212: } 225,
{ 213: } 225,
{ 214: } 225,
{ 215: } 225,
{ 216: } 225,
{ 217: } 225,
{ 218: } 225,
{ 219: } 225,
{ 220: } 225,
{ 221: } 225,
{ 222: } 227,
{ 223: } 229,
{ 224: } 231,
{ 225: } 233,
{ 226: } 233,
{ 227: } 233,
{ 228: } 233,
{ 229: } 237,
{ 230: } 239,
{ 231: } 241,
{ 232: } 242,
{ 233: } 242,
{ 234: } 243,
{ 235: } 243,
{ 236: } 243,
{ 237: } 243,
{ 238: } 243,
{ 239: } 244,
{ 240: } 244,
{ 241: } 244,
{ 242: } 245,
{ 243: } 250,
{ 244: } 250,
{ 245: } 251,
{ 246: } 253,
{ 247: } 253,
{ 248: } 254,
{ 249: } 256,
{ 250: } 258,
{ 251: } 267,
{ 252: } 270,
{ 253: } 272,
{ 254: } 272,
{ 255: } 273,
{ 256: } 273,
{ 257: } 273,
{ 258: } 273,
{ 259: } 273,
{ 260: } 273,
{ 261: } 277,
{ 262: } 277,
{ 263: } 280,
{ 264: } 280,
{ 265: } 281,
{ 266: } 284,
{ 267: } 285,
{ 268: } 288,
{ 269: } 289,
{ 270: } 292,
{ 271: } 293
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
{ 42: } 66,
{ 43: } 69,
{ 44: } 70,
{ 45: } 71,
{ 46: } 71,
{ 47: } 71,
{ 48: } 75,
{ 49: } 79,
{ 50: } 79,
{ 51: } 79,
{ 52: } 79,
{ 53: } 79,
{ 54: } 79,
{ 55: } 80,
{ 56: } 86,
{ 57: } 88,
{ 58: } 93,
{ 59: } 101,
{ 60: } 103,
{ 61: } 104,
{ 62: } 106,
{ 63: } 108,
{ 64: } 110,
{ 65: } 111,
{ 66: } 112,
{ 67: } 113,
{ 68: } 114,
{ 69: } 116,
{ 70: } 118,
{ 71: } 120,
{ 72: } 122,
{ 73: } 124,
{ 74: } 126,
{ 75: } 128,
{ 76: } 129,
{ 77: } 130,
{ 78: } 131,
{ 79: } 132,
{ 80: } 133,
{ 81: } 134,
{ 82: } 135,
{ 83: } 136,
{ 84: } 136,
{ 85: } 137,
{ 86: } 140,
{ 87: } 140,
{ 88: } 140,
{ 89: } 140,
{ 90: } 144,
{ 91: } 145,
{ 92: } 145,
{ 93: } 145,
{ 94: } 145,
{ 95: } 145,
{ 96: } 145,
{ 97: } 145,
{ 98: } 145,
{ 99: } 145,
{ 100: } 145,
{ 101: } 145,
{ 102: } 145,
{ 103: } 145,
{ 104: } 145,
{ 105: } 145,
{ 106: } 145,
{ 107: } 145,
{ 108: } 145,
{ 109: } 145,
{ 110: } 145,
{ 111: } 145,
{ 112: } 145,
{ 113: } 145,
{ 114: } 145,
{ 115: } 145,
{ 116: } 145,
{ 117: } 145,
{ 118: } 145,
{ 119: } 145,
{ 120: } 145,
{ 121: } 145,
{ 122: } 145,
{ 123: } 145,
{ 124: } 145,
{ 125: } 145,
{ 126: } 145,
{ 127: } 145,
{ 128: } 145,
{ 129: } 145,
{ 130: } 145,
{ 131: } 145,
{ 132: } 145,
{ 133: } 145,
{ 134: } 145,
{ 135: } 146,
{ 136: } 146,
{ 137: } 146,
{ 138: } 146,
{ 139: } 146,
{ 140: } 146,
{ 141: } 146,
{ 142: } 146,
{ 143: } 148,
{ 144: } 148,
{ 145: } 148,
{ 146: } 150,
{ 147: } 150,
{ 148: } 151,
{ 149: } 153,
{ 150: } 153,
{ 151: } 153,
{ 152: } 154,
{ 153: } 155,
{ 154: } 160,
{ 155: } 162,
{ 156: } 162,
{ 157: } 166,
{ 158: } 166,
{ 159: } 169,
{ 160: } 174,
{ 161: } 174,
{ 162: } 174,
{ 163: } 174,
{ 164: } 175,
{ 165: } 175,
{ 166: } 176,
{ 167: } 177,
{ 168: } 177,
{ 169: } 177,
{ 170: } 178,
{ 171: } 178,
{ 172: } 179,
{ 173: } 181,
{ 174: } 182,
{ 175: } 182,
{ 176: } 182,
{ 177: } 183,
{ 178: } 183,
{ 179: } 183,
{ 180: } 183,
{ 181: } 186,
{ 182: } 189,
{ 183: } 189,
{ 184: } 190,
{ 185: } 191,
{ 186: } 192,
{ 187: } 194,
{ 188: } 194,
{ 189: } 194,
{ 190: } 195,
{ 191: } 195,
{ 192: } 201,
{ 193: } 202,
{ 194: } 202,
{ 195: } 202,
{ 196: } 202,
{ 197: } 202,
{ 198: } 202,
{ 199: } 202,
{ 200: } 203,
{ 201: } 204,
{ 202: } 205,
{ 203: } 206,
{ 204: } 208,
{ 205: } 210,
{ 206: } 219,
{ 207: } 222,
{ 208: } 222,
{ 209: } 224,
{ 210: } 224,
{ 211: } 224,
{ 212: } 224,
{ 213: } 224,
{ 214: } 224,
{ 215: } 224,
{ 216: } 224,
{ 217: } 224,
{ 218: } 224,
{ 219: } 224,
{ 220: } 224,
{ 221: } 226,
{ 222: } 228,
{ 223: } 230,
{ 224: } 232,
{ 225: } 232,
{ 226: } 232,
{ 227: } 232,
{ 228: } 236,
{ 229: } 238,
{ 230: } 240,
{ 231: } 241,
{ 232: } 241,
{ 233: } 242,
{ 234: } 242,
{ 235: } 242,
{ 236: } 242,
{ 237: } 242,
{ 238: } 243,
{ 239: } 243,
{ 240: } 243,
{ 241: } 244,
{ 242: } 249,
{ 243: } 249,
{ 244: } 250,
{ 245: } 252,
{ 246: } 252,
{ 247: } 253,
{ 248: } 255,
{ 249: } 257,
{ 250: } 266,
{ 251: } 269,
{ 252: } 271,
{ 253: } 271,
{ 254: } 272,
{ 255: } 272,
{ 256: } 272,
{ 257: } 272,
{ 258: } 272,
{ 259: } 272,
{ 260: } 276,
{ 261: } 276,
{ 262: } 279,
{ 263: } 279,
{ 264: } 280,
{ 265: } 283,
{ 266: } 284,
{ 267: } 287,
{ 268: } 288,
{ 269: } 291,
{ 270: } 292,
{ 271: } 292
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
{ 79: } 45,
{ 80: } 45,
{ 81: } 45,
{ 82: } 45,
{ 83: } 45,
{ 84: } 45,
{ 85: } 46,
{ 86: } 46,
{ 87: } 47,
{ 88: } 47,
{ 89: } 47,
{ 90: } 47,
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
{ 130: } 48,
{ 131: } 48,
{ 132: } 48,
{ 133: } 48,
{ 134: } 48,
{ 135: } 48,
{ 136: } 49,
{ 137: } 49,
{ 138: } 49,
{ 139: } 49,
{ 140: } 49,
{ 141: } 49,
{ 142: } 49,
{ 143: } 49,
{ 144: } 50,
{ 145: } 51,
{ 146: } 52,
{ 147: } 56,
{ 148: } 56,
{ 149: } 56,
{ 150: } 56,
{ 151: } 56,
{ 152: } 56,
{ 153: } 56,
{ 154: } 57,
{ 155: } 59,
{ 156: } 60,
{ 157: } 60,
{ 158: } 60,
{ 159: } 60,
{ 160: } 63,
{ 161: } 63,
{ 162: } 63,
{ 163: } 63,
{ 164: } 63,
{ 165: } 63,
{ 166: } 63,
{ 167: } 63,
{ 168: } 63,
{ 169: } 63,
{ 170: } 64,
{ 171: } 64,
{ 172: } 64,
{ 173: } 64,
{ 174: } 65,
{ 175: } 66,
{ 176: } 66,
{ 177: } 66,
{ 178: } 67,
{ 179: } 67,
{ 180: } 67,
{ 181: } 67,
{ 182: } 69,
{ 183: } 70,
{ 184: } 70,
{ 185: } 70,
{ 186: } 70,
{ 187: } 70,
{ 188: } 70,
{ 189: } 70,
{ 190: } 70,
{ 191: } 70,
{ 192: } 70,
{ 193: } 71,
{ 194: } 71,
{ 195: } 72,
{ 196: } 72,
{ 197: } 72,
{ 198: } 72,
{ 199: } 72,
{ 200: } 72,
{ 201: } 72,
{ 202: } 72,
{ 203: } 72,
{ 204: } 72,
{ 205: } 75,
{ 206: } 78,
{ 207: } 79,
{ 208: } 82,
{ 209: } 83,
{ 210: } 84,
{ 211: } 84,
{ 212: } 84,
{ 213: } 84,
{ 214: } 84,
{ 215: } 84,
{ 216: } 84,
{ 217: } 84,
{ 218: } 84,
{ 219: } 84,
{ 220: } 84,
{ 221: } 84,
{ 222: } 85,
{ 223: } 86,
{ 224: } 87,
{ 225: } 88,
{ 226: } 88,
{ 227: } 88,
{ 228: } 88,
{ 229: } 90,
{ 230: } 91,
{ 231: } 92,
{ 232: } 92,
{ 233: } 92,
{ 234: } 92,
{ 235: } 92,
{ 236: } 92,
{ 237: } 92,
{ 238: } 92,
{ 239: } 92,
{ 240: } 92,
{ 241: } 92,
{ 242: } 92,
{ 243: } 93,
{ 244: } 93,
{ 245: } 93,
{ 246: } 94,
{ 247: } 94,
{ 248: } 94,
{ 249: } 97,
{ 250: } 100,
{ 251: } 101,
{ 252: } 104,
{ 253: } 107,
{ 254: } 107,
{ 255: } 107,
{ 256: } 107,
{ 257: } 107,
{ 258: } 107,
{ 259: } 107,
{ 260: } 107,
{ 261: } 109,
{ 262: } 109,
{ 263: } 111,
{ 264: } 111,
{ 265: } 111,
{ 266: } 113,
{ 267: } 113,
{ 268: } 115,
{ 269: } 115,
{ 270: } 117,
{ 271: } 117
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
{ 78: } 44,
{ 79: } 44,
{ 80: } 44,
{ 81: } 44,
{ 82: } 44,
{ 83: } 44,
{ 84: } 45,
{ 85: } 45,
{ 86: } 46,
{ 87: } 46,
{ 88: } 46,
{ 89: } 46,
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
{ 129: } 47,
{ 130: } 47,
{ 131: } 47,
{ 132: } 47,
{ 133: } 47,
{ 134: } 47,
{ 135: } 48,
{ 136: } 48,
{ 137: } 48,
{ 138: } 48,
{ 139: } 48,
{ 140: } 48,
{ 141: } 48,
{ 142: } 48,
{ 143: } 49,
{ 144: } 50,
{ 145: } 51,
{ 146: } 55,
{ 147: } 55,
{ 148: } 55,
{ 149: } 55,
{ 150: } 55,
{ 151: } 55,
{ 152: } 55,
{ 153: } 56,
{ 154: } 58,
{ 155: } 59,
{ 156: } 59,
{ 157: } 59,
{ 158: } 59,
{ 159: } 62,
{ 160: } 62,
{ 161: } 62,
{ 162: } 62,
{ 163: } 62,
{ 164: } 62,
{ 165: } 62,
{ 166: } 62,
{ 167: } 62,
{ 168: } 62,
{ 169: } 63,
{ 170: } 63,
{ 171: } 63,
{ 172: } 63,
{ 173: } 64,
{ 174: } 65,
{ 175: } 65,
{ 176: } 65,
{ 177: } 66,
{ 178: } 66,
{ 179: } 66,
{ 180: } 66,
{ 181: } 68,
{ 182: } 69,
{ 183: } 69,
{ 184: } 69,
{ 185: } 69,
{ 186: } 69,
{ 187: } 69,
{ 188: } 69,
{ 189: } 69,
{ 190: } 69,
{ 191: } 69,
{ 192: } 70,
{ 193: } 70,
{ 194: } 71,
{ 195: } 71,
{ 196: } 71,
{ 197: } 71,
{ 198: } 71,
{ 199: } 71,
{ 200: } 71,
{ 201: } 71,
{ 202: } 71,
{ 203: } 71,
{ 204: } 74,
{ 205: } 77,
{ 206: } 78,
{ 207: } 81,
{ 208: } 82,
{ 209: } 83,
{ 210: } 83,
{ 211: } 83,
{ 212: } 83,
{ 213: } 83,
{ 214: } 83,
{ 215: } 83,
{ 216: } 83,
{ 217: } 83,
{ 218: } 83,
{ 219: } 83,
{ 220: } 83,
{ 221: } 84,
{ 222: } 85,
{ 223: } 86,
{ 224: } 87,
{ 225: } 87,
{ 226: } 87,
{ 227: } 87,
{ 228: } 89,
{ 229: } 90,
{ 230: } 91,
{ 231: } 91,
{ 232: } 91,
{ 233: } 91,
{ 234: } 91,
{ 235: } 91,
{ 236: } 91,
{ 237: } 91,
{ 238: } 91,
{ 239: } 91,
{ 240: } 91,
{ 241: } 91,
{ 242: } 92,
{ 243: } 92,
{ 244: } 92,
{ 245: } 93,
{ 246: } 93,
{ 247: } 93,
{ 248: } 96,
{ 249: } 99,
{ 250: } 100,
{ 251: } 103,
{ 252: } 106,
{ 253: } 106,
{ 254: } 106,
{ 255: } 106,
{ 256: } 106,
{ 257: } 106,
{ 258: } 106,
{ 259: } 106,
{ 260: } 108,
{ 261: } 108,
{ 262: } 110,
{ 263: } 110,
{ 264: } 110,
{ 265: } 112,
{ 266: } 112,
{ 267: } 114,
{ 268: } 114,
{ 269: } 116,
{ 270: } 116,
{ 271: } 116
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
{ 125: } ( len: 2; sym: -48 ),
{ 126: } ( len: 2; sym: -48 ),
{ 127: } ( len: 2; sym: -48 ),
{ 128: } ( len: 2; sym: -48 ),
{ 129: } ( len: 2; sym: -48 ),
{ 130: } ( len: 2; sym: -48 ),
{ 131: } ( len: 1; sym: -49 ),
{ 132: } ( len: 1; sym: -49 ),
{ 133: } ( len: 1; sym: -49 ),
{ 134: } ( len: 1; sym: -49 ),
{ 135: } ( len: 1; sym: -49 ),
{ 136: } ( len: 1; sym: -49 ),
{ 137: } ( len: 1; sym: -50 ),
{ 138: } ( len: 1; sym: -50 ),
{ 139: } ( len: 1; sym: -50 ),
{ 140: } ( len: 1; sym: -50 ),
{ 141: } ( len: 1; sym: -50 ),
{ 142: } ( len: 1; sym: -51 ),
{ 143: } ( len: 1; sym: -51 ),
{ 144: } ( len: 1; sym: -51 ),
{ 145: } ( len: 1; sym: -51 ),
{ 146: } ( len: 1; sym: -51 ),
{ 147: } ( len: 1; sym: -51 ),
{ 148: } ( len: 1; sym: -51 ),
{ 149: } ( len: 1; sym: -51 ),
{ 150: } ( len: 1; sym: -52 ),
{ 151: } ( len: 1; sym: -52 )
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
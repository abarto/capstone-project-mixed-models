
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)


unit UnitParserConfiguracion;

{$H-}

interface

uses
  Classes, SysUtils,
  UaGeneradorNumerosAleatorios, UnitArregloEnteros, UnitArregloStrings,
  UnitCalculoSimbolico, UnitColumnaDatos, UnitConfiguracionDatos,
  UnitConfiguracionModelo, UnitEfecto, UnitEstructura, UnitGrupoEfectos,
  UnitOpcion, UnitParametros;

type
  TErrorSintaxis = class
  private
    FIndiceLinea, FIndiceColumna: Integer;
    FContexto, FMensaje: String;
  public
    constructor Create; overload;
    constructor Create(const AIndiceLinea, AIndiceColumna: Integer; const AContexto, AMensaje: String); overload;
    property IndiceLinea: Integer read FIndiceLinea write FIndiceLinea;
    property IndiceColumna: Integer read FIndiceColumna write FIndiceColumna;
    property Contexto: String read FContexto write FContexto;
    property Mensaje: String read FMensaje write FMensaje;
  end;

  TParserConfiguracion = class
  private
    FStreamEntrada, FStreamSalida: TStream;
    FErrorSintaxis: TErrorSintaxis;
    FConfiguracionDatos: TConfiguracionDatos;
    FConfiguracionModelo: TConfiguracionModelo;
  public
    constructor Create(const AStreamEntrada: TStream);  overload;
    constructor Create(const AStreamEntrada, AStreamSalida: TStream); overload;
    constructor Create(const NombreArchivo: TFileName); overload;
    destructor Destroy; override;
    function Parsear: Boolean;
    property StreamEntrada: TStream read FStreamEntrada write FStreamEntrada;
    property StreamSalida: TStream read FStreamSalida write FStreamSalida;
    property ErrorSintaxis: TErrorSintaxis read FErrorSintaxis;
    property ConfiguracionDatos: TConfiguracionDatos read FConfiguracionDatos;
    property ConfiguracionModelo: TConfiguracionModelo read FConfiguracionModelo;
  end;

implementation

uses
  YaccLib, LexLib,
  StreamIO;
  
type
  TTipoColumnaDatos = (ColumnaCategoricos, ColumnaReales, ColumnaEnteros);

  TTuplaTipoColumnaDatosParametro = record
    Tipo: TTipoColumnaDatos;
    case Integer of
      1 : (ParametroNiveles : TArregloStrings; ParametroCantidadObservacionesNivel : TArregloEnteros);
      2 : (ParametroExpresionReal : TCsExpresion);
      3 : (ParametroExpresionEntera : TCsExpresionEntera);
  end;

  TTuplaEfectosAleatorios = record
    EfectosAleatorios: TGrupoEfectos;
    ListaGruposEfectosAleatorios: TListaGruposEfectos;
    Error: TListaOpciones;
  end;

var
  ConfiguracionDatos: TConfiguracionDatos;
  ConfiguracionModelo: TConfiguracionModelo;
  IdentificarTokensPalabrasClave: Boolean;
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
const T_CRITERIO_ORDENAMIENTO = 329;
const T_CANTIDAD_OBSERVACIONES = 330;

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
                10 : ( yyTCsArregloExpresiones : TCsArregloExpresiones );
                11 : ( yyTCsExpresion : TCsExpresion );
                12 : ( yyTCsExpresionBooleana : TCsExpresionBooleana );
                13 : ( yyTCsFuncion : TCsFuncion );
                14 : ( yyTEfecto : TEfecto );
                15 : ( yyTEstructura : TEstructura );
                16 : ( yyTGrupoEfectos : TGrupoEfectos );
                17 : ( yyTListaColumnasDatos : TListaColumnasDatos );
                18 : ( yyTListaEfectosAnidados : TListaEfectosAnidados );
                19 : ( yyTListaGruposEfectos : TListaGruposEfectos );
                20 : ( yyTListaInformacionParametroIndice : TListaInformacionParametroIndice );
                21 : ( yyTListaOpciones : TListaOpciones );
                22 : ( yyTListaParesEfectoExpresionBooleana : TListaParesEfectoExpresionBooleana );
                23 : ( yyTOpcion : TOpcion );
                24 : ( yyTParEfectoExpresionBooleana : TParEfectoExpresionBooleana );
                25 : ( yyTStrings : TStrings );
                26 : ( yyTTuplaEfectosAleatorios : TTuplaEfectosAleatorios );
                27 : ( yyTTuplaTipoColumnaDatosParametro : TTuplaTipoColumnaDatosParametro );
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
         yyval.yyTConfiguracionDatos := TConfiguracionDatos.Create(yyv[yysp-5].yyShortString, yyv[yysp-3].yyTListaColumnasDatos, yyv[yysp-2].yyTListaOpciones); 
       end;
   4 : begin
         yyval.yyTListaColumnasDatos := yyv[yysp-1].yyTListaColumnasDatos; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   5 : begin
         yyval.yyTListaColumnasDatos := TListaColumnasDatos.Create; yyval.yyTListaColumnasDatos.Add(yyv[yysp-0].yyTColumnaDatos); 
       end;
   6 : begin
         case yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.Tipo of
         ColumnaCategoricos : yyval.yyTColumnaDatos := TColumnaDatosCategoricos.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroNiveles, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroCantidadObservacionesNivel);
         ColumnaReales : yyval.yyTColumnaDatos := TColumnaDatosReales.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroExpresionReal);
         ColumnaEnteros : yyval.yyTColumnaDatos := TColumnaDatosEnteros.Create(yyv[yysp-3].yyShortString, yyv[yysp-1].yyTTuplaTipoColumnaDatosParametro.ParametroExpresionEntera);
         end;
         WriteLn(yyval.yyTColumnaDatos.Texto); 
       end;
   7 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaCategoricos; yyval.yyTTuplaTipoColumnaDatosParametro.ParametroNiveles := yyv[yysp-1].yyTArregloStrings; yyval.yyTTuplaTipoColumnaDatosParametro.ParametroCantidadObservacionesNivel := yyv[yysp-0].yyTArregloEnteros; 
       end;
   8 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaReales; yyval.yyTTuplaTipoColumnaDatosParametro.ParametroExpresionReal := yyv[yysp-0].yyTCsExpresion; 
       end;
   9 : begin
         yyval.yyTTuplaTipoColumnaDatosParametro.Tipo := ColumnaEnteros; yyval.yyTTuplaTipoColumnaDatosParametro.ParametroExpresionEntera := TCsExpresionEntera.Create(yyv[yysp-0].yyTCsExpresion); 
       end;
  10 : begin
         yyval.yyTArregloEnteros := nil; 
       end;
  11 : begin
         yyval.yyTArregloEnteros := yyv[yysp-0].yyTArregloEnteros; 
       end;
  12 : begin
         yyval.yyTArregloStrings := yyv[yysp-1].yyTArregloStrings; 
       end;
  13 : begin
         yyval.yyTArregloStrings := yyv[yysp-2].yyTArregloStrings; yyval.yyTArregloStrings.Dimension := yyval.yyTArregloStrings.Dimension + 1; yyval.yyTArregloStrings [yyval.yyTArregloStrings.Alto] := yyv[yysp-0].yyShortString; 
       end;
  14 : begin
         yyval.yyTArregloStrings := TArregloStrings.Create(1); yyval.yyTArregloStrings [yyval.yyTArregloStrings.Bajo] := yyv[yysp-0].yyShortString; 
       end;
  15 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  16 : begin
         yyval.yyTStrings := TStringList.Create; yyval.yyTStrings.Add(yyv[yysp-0].yyShortString); 
       end;
  17 : begin
         yyval.yyTArregloEnteros := yyv[yysp-1].yyTArregloEnteros; 
       end;
  18 : begin
         yyval.yyTArregloEnteros := yyv[yysp-2].yyTArregloEnteros; yyval.yyTArregloEnteros.Dimension := yyval.yyTArregloEnteros.Dimension + 1; yyval.yyTArregloEnteros [yyval.yyTArregloEnteros.Alto] := yyv[yysp-0].yyInteger; 
       end;
  19 : begin
         yyval.yyTArregloEnteros := TArregloEnteros.Create(1); yyval.yyTArregloEnteros [yyval.yyTArregloEnteros.Bajo] := yyv[yysp-0].yyInteger; 
       end;
  20 : begin
         yyval.yyTCsExpresion := TCsSuma.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  21 : begin
         yyval.yyTCsExpresion := TCsResta.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  22 : begin
         yyval.yyTCsExpresion := TCsProducto.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  23 : begin
         yyval.yyTCsExpresion := TCsDivision.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  24 : begin
         yyval.yyTCsExpresion := TCsPotencia.Create(yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  25 : begin
         yyval.yyTCsExpresion := yyv[yysp-1].yyTCsExpresion; 
       end;
  26 : begin
         yyval.yyTCsExpresion := TCsAplicacionFuncion.Create(yyv[yysp-3].yyTCsFuncion, yyv[yysp-1].yyTCsArregloExpresiones); 
       end;
  27 : begin
         yyval.yyTCsExpresion := TCsAplicacionFuncionBooleanos.Create(CsFuncion1Si, yyv[yysp-1].yyTCsExpresionBooleana); 
       end;
  28 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionUniforme.Create(yyv[yysp-3].yyReal, yyv[yysp-1].yyReal)); 
       end;
  29 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionNormal.Create(yyv[yysp-3].yyReal, yyv[yysp-1].yyReal)); 
       end;
  30 : begin
         yyval.yyTCsExpresion := TCsVariableAleatoria.Create(TUaVariableDistribucionExponencial.Create(yyv[yysp-1].yyReal)); 
       end;
  31 : begin
         yyval.yyTCsExpresion := TCsVariable.Create(nil, yyv[yysp-0].yyInteger); 
       end;
  32 : begin
         yyval.yyTCsExpresion := TCsConstante.Create(yyv[yysp-0].yyReal); 
       end;
  33 : begin
         yyval.yyTCsFuncion := CsFuncionExponencial; 
       end;
  34 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoNatural; 
       end;
  35 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoBase2; 
       end;
  36 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmoBase10; 
       end;
  37 : begin
         yyval.yyTCsFuncion := CsFuncionLogaritmo; 
       end;
  38 : begin
         yyval.yyTCsFuncion := CsFuncionSeno; 
       end;
  39 : begin
         yyval.yyTCsFuncion := CsFuncionCoseno; 
       end;
  40 : begin
         yyval.yyTCsFuncion := CsFuncionTangente; 
       end;
  41 : begin
         yyval.yyTCsArregloExpresiones := yyv[yysp-2].yyTCsArregloExpresiones; yyval.yyTCsArregloExpresiones.Dimension := yyval.yyTCsArregloExpresiones.Dimension + 1; yyval.yyTCsArregloExpresiones [yyval.yyTCsArregloExpresiones.Alto] := yyv[yysp-0].yyTCsExpresion; 
       end;
  42 : begin
         yyval.yyTCsArregloExpresiones := TCsArregloExpresiones.Create(1); yyval.yyTCsArregloExpresiones [0] := yyv[yysp-0].yyTCsExpresion; 
       end;
  43 : begin
         yyval.yyTCsExpresionBooleana := TCsNegacion.Create(yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  44 : begin
         yyval.yyTCsExpresionBooleana := TCsConjuncion.Create(yyv[yysp-2].yyTCsExpresionBooleana, yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  45 : begin
         yyval.yyTCsExpresionBooleana := TCsDisyuncion.Create(yyv[yysp-2].yyTCsExpresionBooleana, yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  46 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  47 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMenorIgualQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  48 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoIgual, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  49 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoDistinto, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  50 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  51 : begin
         yyval.yyTCsExpresionBooleana := TCsAplicacionPredicadoExpresiones.Create(CsPredicadoMayorIgualQue, yyv[yysp-2].yyTCsExpresion, yyv[yysp-0].yyTCsExpresion); 
       end;
  52 : begin
         yyval.yyTCsExpresionBooleana := TCsConstanteBooleana.Create(yyv[yysp-0].yyBoolean); 
       end;
  53 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  54 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  55 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  56 : begin
         yyval.yyTOpcion := TOpcionParametroInteger.Create('cantidad_observaciones', yyv[yysp-0].yyInteger); 
       end;
  57 : begin
         yyval.yyTConfiguracionModelo := TConfiguracionModelo.Create(yyv[yysp-5].yyTStrings,
         yyv[yysp-4].yyShortString,
         yyv[yysp-3].yyTGrupoEfectos,
         yyv[yysp-2].yyTTuplaEfectosAleatorios.EfectosAleatorios,
         yyv[yysp-2].yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios,
         yyv[yysp-2].yyTTuplaEfectosAleatorios.Error); 
       end;
  58 : begin
         yyval.yyTStrings := yyv[yysp-1].yyTStrings; 
       end;
  59 : begin
         yyval.yyShortString := yyv[yysp-1].yyShortString; 
       end;
  60 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaParesEfectoExpresionBooleana, yyv[yysp-2].yyTListaOpciones); 
       end;
  61 : begin
         yyval.yyTListaParesEfectoExpresionBooleana := TListaParesEfectoExpresionBooleana.Create; 
       end;
  62 : begin
         yyval.yyTListaParesEfectoExpresionBooleana := yyv[yysp-1].yyTListaParesEfectoExpresionBooleana; 
       end;
  63 : begin
         yyval.yyTListaParesEfectoExpresionBooleana := yyv[yysp-1].yyTListaParesEfectoExpresionBooleana; yyval.yyTListaParesEfectoExpresionBooleana.Add(yyv[yysp-0].yyTParEfectoExpresionBooleana); 
       end;
  64 : begin
         yyval.yyTListaParesEfectoExpresionBooleana := TListaParesEfectoExpresionBooleana.Create; yyval.yyTListaParesEfectoExpresionBooleana.Add(yyv[yysp-0].yyTParEfectoExpresionBooleana); 
       end;
  65 : begin
         yyval.yyTParEfectoExpresionBooleana := TParEfectoExpresionBooleana.Create(yyv[yysp-1].yyTEfecto, yyv[yysp-0].yyTCsExpresionBooleana); 
       end;
  66 : begin
         yyval.yyTCsExpresionBooleana := TCsConstanteBooleana.Create(True); 
       end;
  67 : begin
         yyval.yyTCsExpresionBooleana := yyv[yysp-1].yyTCsExpresionBooleana; 
       end;
  68 : begin
         yyval.yyTEfecto := TIntercepto.Create; 
       end;
  69 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  70 : begin
         yyval.yyTEfecto := yyv[yysp-0].yyTEfecto; 
       end;
  71 : begin
         yyval.yyTEfecto := TCruzamiento.Create(TEfectoPrincipal.Create(yyv[yysp-2].yyShortString), yyv[yysp-0].yyTEfecto); 
       end;
  72 : begin
         yyval.yyTEfecto := TEfectoPrincipal.Create(yyv[yysp-0].yyShortString); 
       end;
  73 : begin
         yyval.yyTEfecto := TAnidamiento.Create(yyv[yysp-3].yyTEfecto, yyv[yysp-1].yyTListaEfectosAnidados); 
       end;
  74 : begin
         yyval.yyTListaEfectosAnidados := yyv[yysp-1].yyTListaEfectosAnidados; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  75 : begin
         yyval.yyTListaEfectosAnidados := TListaEfectosAnidados.Create; yyval.yyTListaEfectosAnidados.Add(TEfectoPrincipal.Create(yyv[yysp-0].yyShortString)); 
       end;
  76 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  77 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  78 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion 
       end;
  79 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
  80 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
       end;
  81 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := nil;
         yyval.yyTTuplaEfectosAleatorios.Error := nil; 
       end;
  82 : begin
         yyval.yyTTuplaEfectosAleatorios.EfectosAleatorios := TGrupoEfectos.Create(yyv[yysp-4].yyTListaParesEfectoExpresionBooleana, yyv[yysp-3].yyTListaOpciones);
         yyval.yyTTuplaEfectosAleatorios.ListaGruposEfectosAleatorios := yyv[yysp-5].yyTListaGruposEfectos;
         yyval.yyTTuplaEfectosAleatorios.Error := yyv[yysp-2].yyTListaOpciones; 
       end;
  83 : begin
         yyval.yyTListaGruposEfectos := TListaGruposEfectos.Create; 
       end;
  84 : begin
         yyval.yyTListaGruposEfectos := yyv[yysp-1].yyTListaGruposEfectos; yyval.yyTListaGruposEfectos.Add(yyv[yysp-0].yyTGrupoEfectos); 
       end;
  85 : begin
         yyval.yyTGrupoEfectos := TGrupoEfectos.Create(yyv[yysp-3].yyTListaParesEfectoExpresionBooleana, yyv[yysp-2].yyTListaOpciones); 
       end;
  86 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
  87 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
  88 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
  89 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('generar_columnas_nulas', yyv[yysp-0].yyBoolean); 
       end;
  90 : begin
         yyval.yyTOpcion := TOpcionParametroBoolean.Create('incluir_intercepto', yyv[yysp-0].yyBoolean); 
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
         yyval.yyTEstructura := TEstructura.Create('simetria_compuesta'); 
       end;
  96 : begin
         yyval.yyTEstructura := TEstructura.Create('componentes_varianza'); 
       end;
  97 : begin
         yyval.yyTEstructura := TEstructura.Create('general'); 
       end;
  98 : begin
         yyval.yyTEstructura := TEstructura.Create('diagonal_heterogenea'); 
       end;
  99 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic', yyv[yysp-0].yyInteger); 
       end;
 100 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_sin_diagonal', yyv[yysp-0].yyInteger); 
       end;
 101 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('factor_analytic_diagonal_escalar', yyv[yysp-0].yyInteger); 
       end;
 102 : begin
         yyval.yyTEstructura := TEstructuraOrden.Create('bandeada', yyv[yysp-0].yyInteger); 
       end;
 103 : begin
         yyval.yyInteger := 1; 
       end;
 104 : begin
         yyval.yyInteger := yyv[yysp-1].yyInteger; 
       end;
 105 : begin
         yyval.yyTListaInformacionParametroIndice := yyv[yysp-1].yyTListaInformacionParametroIndice; yyval.yyTListaInformacionParametroIndice.Add(TInformacionParametroIndice.Create(yyv[yysp-0].yyReal)); 
       end;
 106 : begin
         yyval.yyTListaInformacionParametroIndice := TListaInformacionParametroIndice.Create; yyval.yyTListaInformacionParametroIndice.Add(TInformacionParametroIndice.Create(yyv[yysp-0].yyReal)); 
       end;
 107 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 108 : begin
         yyval.yyTListaOpciones := yyv[yysp-0].yyTListaOpciones; 
       end;
 109 : begin
         yyval := yyv[yysp-5];
       end;
 110 : begin
         yyval.yyTListaOpciones := TListaOpciones.Create; 
       end;
 111 : begin
         yyval.yyTListaOpciones := yyv[yysp-1].yyTListaOpciones; yyval.yyTListaOpciones.Add(yyv[yysp-0].yyTOpcion); 
       end;
 112 : begin
         yyval.yyTOpcion := yyv[yysp-1].yyTOpcion; 
       end;
 113 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_ordenamiento', yyv[yysp-0].yyTEfecto); 
       end;
 114 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('criterio_agrupamiento_unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 115 : begin
         yyval.yyTOpcion := TOpcionParametroTEfecto.Create('unidad_experimental', yyv[yysp-0].yyTEfecto); 
       end;
 116 : begin
         yyval.yyTOpcion := TOpcionParametroTEstructura.Create('estructura', yyv[yysp-0].yyTEstructura); 
       end;
 117 : begin
         yyval.yyTOpcion := TOpcionParametroTObject.Create('parametros', yyv[yysp-0].yyTListaInformacionParametroIndice); 
       end;
 118 : begin
         yyval.yyReal := yyv[yysp-0].yyInteger; 
       end;
 119 : begin
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

yynacts   = 654;
yyngotos  = 157;
yynstates = 236;
yynrules  = 119;

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
  ( sym: 257; act: 9 ),
{ 8: }
  ( sym: 294; act: 12 ),
{ 9: }
  ( sym: 307; act: 14 ),
{ 10: }
{ 11: }
  ( sym: 294; act: 12 ),
  ( sym: 258; act: -53 ),
  ( sym: 311; act: -53 ),
{ 12: }
  ( sym: 288; act: 17 ),
{ 13: }
  ( sym: 308; act: 19 ),
{ 14: }
  ( sym: 288; act: 21 ),
{ 15: }
  ( sym: 258; act: 23 ),
  ( sym: 311; act: 24 ),
{ 16: }
{ 17: }
  ( sym: 263; act: 25 ),
{ 18: }
  ( sym: 309; act: 27 ),
{ 19: }
  ( sym: 288; act: 28 ),
{ 20: }
  ( sym: 265; act: 29 ),
  ( sym: 288; act: 30 ),
{ 21: }
{ 22: }
{ 23: }
  ( sym: 265; act: 31 ),
{ 24: }
  ( sym: 330; act: 33 ),
{ 25: }
  ( sym: 295; act: 35 ),
  ( sym: 296; act: 36 ),
  ( sym: 297; act: 37 ),
{ 26: }
  ( sym: 314; act: 39 ),
  ( sym: 258; act: -81 ),
{ 27: }
  ( sym: 257; act: 40 ),
{ 28: }
  ( sym: 265; act: 41 ),
{ 29: }
{ 30: }
{ 31: }
{ 32: }
  ( sym: 265; act: 42 ),
{ 33: }
  ( sym: 285; act: 43 ),
{ 34: }
  ( sym: 265; act: 44 ),
{ 35: }
  ( sym: 261; act: 46 ),
{ 36: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 37: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 38: }
  ( sym: 258; act: 67 ),
{ 39: }
  ( sym: 257; act: 68 ),
{ 40: }
  ( sym: 310; act: 70 ),
  ( sym: 258; act: -61 ),
  ( sym: 311; act: -61 ),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: 261; act: 73 ),
  ( sym: 265; act: -10 ),
{ 46: }
  ( sym: 288; act: 75 ),
{ 47: }
{ 48: }
  ( sym: 259; act: 76 ),
{ 49: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 265; act: -8 ),
{ 50: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 51: }
  ( sym: 259; act: 83 ),
{ 52: }
  ( sym: 259; act: 84 ),
{ 53: }
  ( sym: 259; act: 85 ),
{ 54: }
  ( sym: 259; act: 86 ),
{ 55: }
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
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 265; act: -9 ),
{ 67: }
  ( sym: 265; act: 87 ),
{ 68: }
{ 69: }
{ 70: }
  ( sym: 288; act: 96 ),
{ 71: }
{ 72: }
{ 73: }
  ( sym: 285; act: 98 ),
{ 74: }
  ( sym: 262; act: 99 ),
  ( sym: 264; act: 100 ),
{ 75: }
{ 76: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 77: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 78: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 79: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 80: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 81: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 82: }
  ( sym: 260; act: 108 ),
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
{ 83: }
  ( sym: 285; act: 55 ),
  ( sym: 287; act: 57 ),
{ 84: }
  ( sym: 285; act: 55 ),
  ( sym: 287; act: 57 ),
{ 85: }
  ( sym: 285; act: 55 ),
  ( sym: 287; act: 57 ),
{ 86: }
  ( sym: 259; act: 50 ),
  ( sym: 272; act: 114 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 290; act: 115 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 87: }
{ 88: }
  ( sym: 310; act: 70 ),
  ( sym: 315; act: 118 ),
  ( sym: 258; act: -61 ),
  ( sym: 311; act: -61 ),
  ( sym: 316; act: -61 ),
{ 89: }
  ( sym: 258; act: 120 ),
  ( sym: 311; act: 121 ),
{ 90: }
{ 91: }
{ 92: }
  ( sym: 259; act: 122 ),
  ( sym: 261; act: -69 ),
  ( sym: 265; act: -69 ),
  ( sym: 288; act: -69 ),
{ 93: }
  ( sym: 261; act: 124 ),
  ( sym: 265; act: -66 ),
  ( sym: 288; act: -66 ),
{ 94: }
{ 95: }
  ( sym: 265; act: 126 ),
  ( sym: 288; act: 96 ),
{ 96: }
  ( sym: 266; act: 127 ),
  ( sym: 259; act: -72 ),
  ( sym: 261; act: -72 ),
  ( sym: 265; act: -72 ),
  ( sym: 288; act: -72 ),
{ 97: }
  ( sym: 262; act: 128 ),
  ( sym: 264; act: 129 ),
{ 98: }
{ 99: }
{ 100: }
  ( sym: 288; act: 130 ),
{ 101: }
  ( sym: 260; act: 131 ),
  ( sym: 264; act: 132 ),
{ 102: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -42 ),
  ( sym: 264; act: -42 ),
{ 103: }
{ 104: }
  ( sym: 266; act: 77 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -20 ),
  ( sym: 262; act: -20 ),
  ( sym: 264; act: -20 ),
  ( sym: 265; act: -20 ),
  ( sym: 267; act: -20 ),
  ( sym: 268; act: -20 ),
  ( sym: 273; act: -20 ),
  ( sym: 274; act: -20 ),
  ( sym: 275; act: -20 ),
  ( sym: 277; act: -20 ),
  ( sym: 278; act: -20 ),
  ( sym: 279; act: -20 ),
{ 105: }
  ( sym: 266; act: 77 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -21 ),
  ( sym: 262; act: -21 ),
  ( sym: 264; act: -21 ),
  ( sym: 265; act: -21 ),
  ( sym: 267; act: -21 ),
  ( sym: 268; act: -21 ),
  ( sym: 273; act: -21 ),
  ( sym: 274; act: -21 ),
  ( sym: 275; act: -21 ),
  ( sym: 277; act: -21 ),
  ( sym: 278; act: -21 ),
  ( sym: 279; act: -21 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: 264; act: 133 ),
{ 110: }
  ( sym: 264; act: 134 ),
{ 111: }
  ( sym: 260; act: 135 ),
{ 112: }
  ( sym: 260; act: 136 ),
  ( sym: 273; act: 137 ),
  ( sym: 274; act: 138 ),
{ 113: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 275; act: 141 ),
  ( sym: 277; act: 142 ),
  ( sym: 278; act: 143 ),
  ( sym: 279; act: 144 ),
{ 114: }
  ( sym: 259; act: 50 ),
  ( sym: 272; act: 114 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 290; act: 115 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 115: }
{ 116: }
{ 117: }
{ 118: }
  ( sym: 257; act: 147 ),
{ 119: }
{ 120: }
  ( sym: 265; act: 148 ),
{ 121: }
  ( sym: 312; act: 150 ),
  ( sym: 313; act: 151 ),
{ 122: }
  ( sym: 288; act: 153 ),
{ 123: }
{ 124: }
  ( sym: 259; act: 50 ),
  ( sym: 272; act: 114 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 290; act: 115 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 125: }
{ 126: }
{ 127: }
  ( sym: 288; act: 96 ),
{ 128: }
{ 129: }
  ( sym: 285; act: 156 ),
{ 130: }
{ 131: }
{ 132: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 133: }
  ( sym: 285; act: 55 ),
  ( sym: 287; act: 57 ),
{ 134: }
  ( sym: 285; act: 55 ),
  ( sym: 287; act: 57 ),
{ 135: }
{ 136: }
{ 137: }
  ( sym: 259; act: 50 ),
  ( sym: 272; act: 114 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 290; act: 115 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 138: }
  ( sym: 259; act: 50 ),
  ( sym: 272; act: 114 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 290; act: 115 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 139: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 140: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 141: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 142: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 143: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 144: }
  ( sym: 259; act: 50 ),
  ( sym: 281; act: 51 ),
  ( sym: 282; act: 52 ),
  ( sym: 283; act: 53 ),
  ( sym: 284; act: 54 ),
  ( sym: 285; act: 55 ),
  ( sym: 286; act: 56 ),
  ( sym: 287; act: 57 ),
  ( sym: 298; act: 58 ),
  ( sym: 299; act: 59 ),
  ( sym: 300; act: 60 ),
  ( sym: 301; act: 61 ),
  ( sym: 302; act: 62 ),
  ( sym: 303; act: 63 ),
  ( sym: 304; act: 64 ),
  ( sym: 305; act: 65 ),
{ 145: }
{ 146: }
  ( sym: 311; act: 170 ),
  ( sym: 316; act: 171 ),
  ( sym: 258; act: -107 ),
{ 147: }
  ( sym: 310; act: 70 ),
  ( sym: 258; act: -61 ),
  ( sym: 311; act: -61 ),
{ 148: }
{ 149: }
  ( sym: 265; act: 173 ),
{ 150: }
  ( sym: 290; act: 174 ),
{ 151: }
  ( sym: 290; act: 175 ),
{ 152: }
  ( sym: 260; act: 176 ),
  ( sym: 288; act: 177 ),
{ 153: }
{ 154: }
  ( sym: 262; act: 178 ),
  ( sym: 273; act: 137 ),
  ( sym: 274; act: 138 ),
{ 155: }
{ 156: }
{ 157: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -41 ),
  ( sym: 264; act: -41 ),
{ 158: }
  ( sym: 260; act: 179 ),
{ 159: }
  ( sym: 260; act: 180 ),
{ 160: }
{ 161: }
  ( sym: 273; act: 137 ),
  ( sym: 260; act: -45 ),
  ( sym: 262; act: -45 ),
  ( sym: 274; act: -45 ),
{ 162: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -51 ),
  ( sym: 262; act: -51 ),
  ( sym: 273; act: -51 ),
  ( sym: 274; act: -51 ),
{ 163: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -47 ),
  ( sym: 262; act: -47 ),
  ( sym: 273; act: -47 ),
  ( sym: 274; act: -47 ),
{ 164: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -46 ),
  ( sym: 262; act: -46 ),
  ( sym: 273; act: -46 ),
  ( sym: 274; act: -46 ),
{ 165: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -48 ),
  ( sym: 262; act: -48 ),
  ( sym: 273; act: -48 ),
  ( sym: 274; act: -48 ),
{ 166: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -49 ),
  ( sym: 262; act: -49 ),
  ( sym: 273; act: -49 ),
  ( sym: 274; act: -49 ),
{ 167: }
  ( sym: 266; act: 77 ),
  ( sym: 267; act: 78 ),
  ( sym: 268; act: 79 ),
  ( sym: 269; act: 80 ),
  ( sym: 270; act: 81 ),
  ( sym: 260; act: -50 ),
  ( sym: 262; act: -50 ),
  ( sym: 273; act: -50 ),
  ( sym: 274; act: -50 ),
{ 168: }
  ( sym: 258; act: 181 ),
{ 169: }
{ 170: }
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 317; act: 185 ),
  ( sym: 318; act: 186 ),
  ( sym: 319; act: 187 ),
  ( sym: 320; act: 188 ),
{ 171: }
  ( sym: 257; act: 189 ),
{ 172: }
{ 173: }
{ 174: }
{ 175: }
{ 176: }
{ 177: }
{ 178: }
{ 179: }
{ 180: }
{ 181: }
  ( sym: 265; act: 191 ),
{ 182: }
  ( sym: 265; act: 192 ),
{ 183: }
  ( sym: 290; act: 193 ),
{ 184: }
  ( sym: 290; act: 194 ),
{ 185: }
  ( sym: 288; act: 96 ),
{ 186: }
  ( sym: 288; act: 96 ),
{ 187: }
  ( sym: 321; act: 198 ),
  ( sym: 322; act: 199 ),
  ( sym: 323; act: 200 ),
  ( sym: 324; act: 201 ),
  ( sym: 325; act: 202 ),
  ( sym: 326; act: 203 ),
  ( sym: 327; act: 204 ),
  ( sym: 328; act: 205 ),
{ 188: }
  ( sym: 287; act: 207 ),
{ 189: }
{ 190: }
  ( sym: 258; act: 209 ),
  ( sym: 311; act: 170 ),
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
  ( sym: 259; act: 211 ),
  ( sym: 265; act: -103 ),
{ 203: }
  ( sym: 259; act: 211 ),
  ( sym: 265; act: -103 ),
{ 204: }
  ( sym: 259; act: 211 ),
  ( sym: 265; act: -103 ),
{ 205: }
  ( sym: 259; act: 211 ),
  ( sym: 265; act: -103 ),
{ 206: }
  ( sym: 287; act: 215 ),
  ( sym: 265; act: -94 ),
{ 207: }
{ 208: }
  ( sym: 311; act: 218 ),
  ( sym: 258; act: -108 ),
{ 209: }
  ( sym: 265; act: 219 ),
{ 210: }
{ 211: }
  ( sym: 285; act: 220 ),
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
  ( sym: 258; act: 221 ),
{ 217: }
{ 218: }
  ( sym: 317; act: 223 ),
  ( sym: 318; act: 224 ),
  ( sym: 319; act: 225 ),
  ( sym: 320; act: 226 ),
  ( sym: 329; act: 227 ),
{ 219: }
{ 220: }
  ( sym: 260; act: 228 ),
{ 221: }
  ( sym: 265; act: 229 ),
{ 222: }
  ( sym: 265; act: 230 ),
{ 223: }
  ( sym: 288; act: 96 ),
{ 224: }
  ( sym: 288; act: 96 ),
{ 225: }
  ( sym: 321; act: 198 ),
  ( sym: 322; act: 199 ),
  ( sym: 323; act: 200 ),
  ( sym: 324; act: 201 ),
  ( sym: 325; act: 202 ),
  ( sym: 326; act: 203 ),
  ( sym: 327; act: 204 ),
  ( sym: 328; act: 205 ),
{ 226: }
  ( sym: 287; act: 207 ),
{ 227: }
  ( sym: 288; act: 96 ),
{ 228: }
{ 229: }
{ 230: }
{ 231: }
{ 232: }
{ 233: }
{ 234: }
  ( sym: 287; act: 215 ),
  ( sym: 265; act: -117 )
{ 235: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -48; act: 1 ),
  ( sym: -2; act: 2 ),
{ 1: }
{ 2: }
  ( sym: -49; act: 4 ),
{ 3: }
{ 4: }
  ( sym: -3; act: 6 ),
{ 5: }
{ 6: }
{ 7: }
{ 8: }
  ( sym: -5; act: 10 ),
  ( sym: -4; act: 11 ),
{ 9: }
  ( sym: -17; act: 13 ),
{ 10: }
{ 11: }
  ( sym: -44; act: 15 ),
  ( sym: -5; act: 16 ),
{ 12: }
{ 13: }
  ( sym: -18; act: 18 ),
{ 14: }
  ( sym: -10; act: 20 ),
{ 15: }
  ( sym: -45; act: 22 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: -23; act: 26 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( sym: -46; act: 32 ),
{ 25: }
  ( sym: -6; act: 34 ),
{ 26: }
  ( sym: -31; act: 38 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -8; act: 45 ),
{ 36: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 49 ),
{ 37: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 66 ),
{ 38: }
{ 39: }
{ 40: }
  ( sym: -19; act: 69 ),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: -11; act: 71 ),
  ( sym: -7; act: 72 ),
{ 46: }
  ( sym: -9; act: 74 ),
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 82 ),
{ 51: }
{ 52: }
{ 53: }
{ 54: }
{ 55: }
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
  ( sym: -32; act: 88 ),
{ 69: }
  ( sym: -28; act: 89 ),
{ 70: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 93 ),
  ( sym: -21; act: 94 ),
  ( sym: -20; act: 95 ),
{ 71: }
{ 72: }
{ 73: }
  ( sym: -12; act: 97 ),
{ 74: }
{ 75: }
{ 76: }
  ( sym: -47; act: 47 ),
  ( sym: -15; act: 101 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 102 ),
{ 77: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 103 ),
{ 78: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 104 ),
{ 79: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 105 ),
{ 80: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 106 ),
{ 81: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 107 ),
{ 82: }
{ 83: }
  ( sym: -47; act: 109 ),
{ 84: }
  ( sym: -47; act: 110 ),
{ 85: }
  ( sym: -47; act: 111 ),
{ 86: }
  ( sym: -47; act: 47 ),
  ( sym: -16; act: 112 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 113 ),
{ 87: }
{ 88: }
  ( sym: -33; act: 116 ),
  ( sym: -19; act: 117 ),
{ 89: }
  ( sym: -29; act: 119 ),
{ 90: }
{ 91: }
{ 92: }
{ 93: }
  ( sym: -22; act: 123 ),
{ 94: }
{ 95: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 93 ),
  ( sym: -21; act: 125 ),
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
  ( sym: -51; act: 139 ),
  ( sym: -50; act: 140 ),
{ 114: }
  ( sym: -47; act: 47 ),
  ( sym: -16; act: 145 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 113 ),
{ 115: }
{ 116: }
{ 117: }
  ( sym: -34; act: 146 ),
{ 118: }
{ 119: }
{ 120: }
{ 121: }
  ( sym: -30; act: 149 ),
{ 122: }
  ( sym: -27; act: 152 ),
{ 123: }
{ 124: }
  ( sym: -47; act: 47 ),
  ( sym: -16; act: 154 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 113 ),
{ 125: }
{ 126: }
{ 127: }
  ( sym: -25; act: 155 ),
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 157 ),
{ 133: }
  ( sym: -47; act: 158 ),
{ 134: }
  ( sym: -47; act: 159 ),
{ 135: }
{ 136: }
{ 137: }
  ( sym: -47; act: 47 ),
  ( sym: -16; act: 160 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 113 ),
{ 138: }
  ( sym: -47; act: 47 ),
  ( sym: -16; act: 161 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 113 ),
{ 139: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 162 ),
{ 140: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 163 ),
{ 141: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 164 ),
{ 142: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 165 ),
{ 143: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 166 ),
{ 144: }
  ( sym: -47; act: 47 ),
  ( sym: -14; act: 48 ),
  ( sym: -13; act: 167 ),
{ 145: }
{ 146: }
  ( sym: -40; act: 168 ),
  ( sym: -35; act: 169 ),
{ 147: }
  ( sym: -19; act: 172 ),
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
{ 166: }
{ 167: }
{ 168: }
{ 169: }
{ 170: }
  ( sym: -36; act: 182 ),
{ 171: }
{ 172: }
  ( sym: -34; act: 190 ),
{ 173: }
{ 174: }
{ 175: }
{ 176: }
{ 177: }
{ 178: }
{ 179: }
{ 180: }
{ 181: }
{ 182: }
{ 183: }
{ 184: }
{ 185: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 195 ),
{ 186: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 196 ),
{ 187: }
  ( sym: -37; act: 197 ),
{ 188: }
  ( sym: -38; act: 206 ),
{ 189: }
  ( sym: -41; act: 208 ),
{ 190: }
  ( sym: -35; act: 169 ),
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
{ 197: }
{ 198: }
{ 199: }
{ 200: }
{ 201: }
{ 202: }
  ( sym: -39; act: 210 ),
{ 203: }
  ( sym: -39; act: 212 ),
{ 204: }
  ( sym: -39; act: 213 ),
{ 205: }
  ( sym: -39; act: 214 ),
{ 206: }
{ 207: }
{ 208: }
  ( sym: -53; act: 216 ),
  ( sym: -42; act: 217 ),
{ 209: }
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
{ 217: }
{ 218: }
  ( sym: -43; act: 222 ),
{ 219: }
{ 220: }
{ 221: }
{ 222: }
{ 223: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 231 ),
{ 224: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 232 ),
{ 225: }
  ( sym: -37; act: 233 ),
{ 226: }
  ( sym: -38; act: 234 ),
{ 227: }
  ( sym: -52; act: 90 ),
  ( sym: -26; act: 91 ),
  ( sym: -25; act: 92 ),
  ( sym: -24; act: 235 )
{ 228: }
{ 229: }
{ 230: }
{ 231: }
{ 232: }
{ 233: }
{ 234: }
{ 235: }
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
{ 10: } -5,
{ 11: } 0,
{ 12: } 0,
{ 13: } 0,
{ 14: } 0,
{ 15: } 0,
{ 16: } -4,
{ 17: } 0,
{ 18: } 0,
{ 19: } 0,
{ 20: } 0,
{ 21: } -16,
{ 22: } -54,
{ 23: } 0,
{ 24: } 0,
{ 25: } 0,
{ 26: } 0,
{ 27: } 0,
{ 28: } 0,
{ 29: } -58,
{ 30: } -15,
{ 31: } -3,
{ 32: } 0,
{ 33: } 0,
{ 34: } 0,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
{ 38: } 0,
{ 39: } 0,
{ 40: } 0,
{ 41: } -59,
{ 42: } -55,
{ 43: } -56,
{ 44: } -6,
{ 45: } 0,
{ 46: } 0,
{ 47: } -32,
{ 48: } 0,
{ 49: } 0,
{ 50: } 0,
{ 51: } 0,
{ 52: } 0,
{ 53: } 0,
{ 54: } 0,
{ 55: } -118,
{ 56: } -31,
{ 57: } -119,
{ 58: } -33,
{ 59: } -34,
{ 60: } -35,
{ 61: } -36,
{ 62: } -37,
{ 63: } -38,
{ 64: } -39,
{ 65: } -40,
{ 66: } 0,
{ 67: } 0,
{ 68: } -83,
{ 69: } -76,
{ 70: } 0,
{ 71: } -11,
{ 72: } -7,
{ 73: } 0,
{ 74: } 0,
{ 75: } -14,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } 0,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } -57,
{ 88: } 0,
{ 89: } 0,
{ 90: } -68,
{ 91: } -70,
{ 92: } 0,
{ 93: } 0,
{ 94: } -64,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } -19,
{ 99: } -12,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } -22,
{ 104: } 0,
{ 105: } 0,
{ 106: } -23,
{ 107: } -24,
{ 108: } -25,
{ 109: } 0,
{ 110: } 0,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } -52,
{ 116: } -84,
{ 117: } -86,
{ 118: } 0,
{ 119: } -77,
{ 120: } 0,
{ 121: } 0,
{ 122: } 0,
{ 123: } -65,
{ 124: } 0,
{ 125: } -63,
{ 126: } -62,
{ 127: } 0,
{ 128: } -17,
{ 129: } 0,
{ 130: } -13,
{ 131: } -26,
{ 132: } 0,
{ 133: } 0,
{ 134: } 0,
{ 135: } -30,
{ 136: } -27,
{ 137: } 0,
{ 138: } 0,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } 0,
{ 143: } 0,
{ 144: } 0,
{ 145: } -43,
{ 146: } 0,
{ 147: } 0,
{ 148: } -60,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } -75,
{ 154: } 0,
{ 155: } -71,
{ 156: } -18,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } -44,
{ 161: } 0,
{ 162: } 0,
{ 163: } 0,
{ 164: } 0,
{ 165: } 0,
{ 166: } 0,
{ 167: } 0,
{ 168: } 0,
{ 169: } -87,
{ 170: } 0,
{ 171: } 0,
{ 172: } -86,
{ 173: } -78,
{ 174: } -79,
{ 175: } -80,
{ 176: } -73,
{ 177: } -74,
{ 178: } -67,
{ 179: } -28,
{ 180: } -29,
{ 181: } 0,
{ 182: } 0,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } 0,
{ 187: } 0,
{ 188: } 0,
{ 189: } -110,
{ 190: } 0,
{ 191: } -82,
{ 192: } -88,
{ 193: } -89,
{ 194: } -90,
{ 195: } -91,
{ 196: } -92,
{ 197: } -93,
{ 198: } -95,
{ 199: } -96,
{ 200: } -97,
{ 201: } -98,
{ 202: } 0,
{ 203: } 0,
{ 204: } 0,
{ 205: } 0,
{ 206: } 0,
{ 207: } -106,
{ 208: } 0,
{ 209: } 0,
{ 210: } -99,
{ 211: } 0,
{ 212: } -100,
{ 213: } -101,
{ 214: } -102,
{ 215: } -105,
{ 216: } 0,
{ 217: } -111,
{ 218: } 0,
{ 219: } -85,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } 0,
{ 226: } 0,
{ 227: } 0,
{ 228: } -104,
{ 229: } -109,
{ 230: } -112,
{ 231: } -114,
{ 232: } -115,
{ 233: } -116,
{ 234: } 0,
{ 235: } -113
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
{ 11: } 9,
{ 12: } 12,
{ 13: } 13,
{ 14: } 14,
{ 15: } 15,
{ 16: } 17,
{ 17: } 17,
{ 18: } 18,
{ 19: } 19,
{ 20: } 20,
{ 21: } 22,
{ 22: } 22,
{ 23: } 22,
{ 24: } 23,
{ 25: } 24,
{ 26: } 27,
{ 27: } 29,
{ 28: } 30,
{ 29: } 31,
{ 30: } 31,
{ 31: } 31,
{ 32: } 31,
{ 33: } 32,
{ 34: } 33,
{ 35: } 34,
{ 36: } 35,
{ 37: } 51,
{ 38: } 67,
{ 39: } 68,
{ 40: } 69,
{ 41: } 72,
{ 42: } 72,
{ 43: } 72,
{ 44: } 72,
{ 45: } 72,
{ 46: } 74,
{ 47: } 75,
{ 48: } 75,
{ 49: } 76,
{ 50: } 82,
{ 51: } 98,
{ 52: } 99,
{ 53: } 100,
{ 54: } 101,
{ 55: } 102,
{ 56: } 102,
{ 57: } 102,
{ 58: } 102,
{ 59: } 102,
{ 60: } 102,
{ 61: } 102,
{ 62: } 102,
{ 63: } 102,
{ 64: } 102,
{ 65: } 102,
{ 66: } 102,
{ 67: } 108,
{ 68: } 109,
{ 69: } 109,
{ 70: } 109,
{ 71: } 110,
{ 72: } 110,
{ 73: } 110,
{ 74: } 111,
{ 75: } 113,
{ 76: } 113,
{ 77: } 129,
{ 78: } 145,
{ 79: } 161,
{ 80: } 177,
{ 81: } 193,
{ 82: } 209,
{ 83: } 215,
{ 84: } 217,
{ 85: } 219,
{ 86: } 221,
{ 87: } 239,
{ 88: } 239,
{ 89: } 244,
{ 90: } 246,
{ 91: } 246,
{ 92: } 246,
{ 93: } 250,
{ 94: } 253,
{ 95: } 253,
{ 96: } 255,
{ 97: } 260,
{ 98: } 262,
{ 99: } 262,
{ 100: } 262,
{ 101: } 263,
{ 102: } 265,
{ 103: } 272,
{ 104: } 272,
{ 105: } 287,
{ 106: } 302,
{ 107: } 302,
{ 108: } 302,
{ 109: } 302,
{ 110: } 303,
{ 111: } 304,
{ 112: } 305,
{ 113: } 308,
{ 114: } 317,
{ 115: } 335,
{ 116: } 335,
{ 117: } 335,
{ 118: } 335,
{ 119: } 336,
{ 120: } 336,
{ 121: } 337,
{ 122: } 339,
{ 123: } 340,
{ 124: } 340,
{ 125: } 358,
{ 126: } 358,
{ 127: } 358,
{ 128: } 359,
{ 129: } 359,
{ 130: } 360,
{ 131: } 360,
{ 132: } 360,
{ 133: } 376,
{ 134: } 378,
{ 135: } 380,
{ 136: } 380,
{ 137: } 380,
{ 138: } 398,
{ 139: } 416,
{ 140: } 432,
{ 141: } 448,
{ 142: } 464,
{ 143: } 480,
{ 144: } 496,
{ 145: } 512,
{ 146: } 512,
{ 147: } 515,
{ 148: } 518,
{ 149: } 518,
{ 150: } 519,
{ 151: } 520,
{ 152: } 521,
{ 153: } 523,
{ 154: } 523,
{ 155: } 526,
{ 156: } 526,
{ 157: } 526,
{ 158: } 533,
{ 159: } 534,
{ 160: } 535,
{ 161: } 535,
{ 162: } 539,
{ 163: } 548,
{ 164: } 557,
{ 165: } 566,
{ 166: } 575,
{ 167: } 584,
{ 168: } 593,
{ 169: } 594,
{ 170: } 594,
{ 171: } 600,
{ 172: } 601,
{ 173: } 601,
{ 174: } 601,
{ 175: } 601,
{ 176: } 601,
{ 177: } 601,
{ 178: } 601,
{ 179: } 601,
{ 180: } 601,
{ 181: } 601,
{ 182: } 602,
{ 183: } 603,
{ 184: } 604,
{ 185: } 605,
{ 186: } 606,
{ 187: } 607,
{ 188: } 615,
{ 189: } 616,
{ 190: } 616,
{ 191: } 618,
{ 192: } 618,
{ 193: } 618,
{ 194: } 618,
{ 195: } 618,
{ 196: } 618,
{ 197: } 618,
{ 198: } 618,
{ 199: } 618,
{ 200: } 618,
{ 201: } 618,
{ 202: } 618,
{ 203: } 620,
{ 204: } 622,
{ 205: } 624,
{ 206: } 626,
{ 207: } 628,
{ 208: } 628,
{ 209: } 630,
{ 210: } 631,
{ 211: } 631,
{ 212: } 632,
{ 213: } 632,
{ 214: } 632,
{ 215: } 632,
{ 216: } 632,
{ 217: } 633,
{ 218: } 633,
{ 219: } 638,
{ 220: } 638,
{ 221: } 639,
{ 222: } 640,
{ 223: } 641,
{ 224: } 642,
{ 225: } 643,
{ 226: } 651,
{ 227: } 652,
{ 228: } 653,
{ 229: } 653,
{ 230: } 653,
{ 231: } 653,
{ 232: } 653,
{ 233: } 653,
{ 234: } 653,
{ 235: } 655
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
{ 10: } 8,
{ 11: } 11,
{ 12: } 12,
{ 13: } 13,
{ 14: } 14,
{ 15: } 16,
{ 16: } 16,
{ 17: } 17,
{ 18: } 18,
{ 19: } 19,
{ 20: } 21,
{ 21: } 21,
{ 22: } 21,
{ 23: } 22,
{ 24: } 23,
{ 25: } 26,
{ 26: } 28,
{ 27: } 29,
{ 28: } 30,
{ 29: } 30,
{ 30: } 30,
{ 31: } 30,
{ 32: } 31,
{ 33: } 32,
{ 34: } 33,
{ 35: } 34,
{ 36: } 50,
{ 37: } 66,
{ 38: } 67,
{ 39: } 68,
{ 40: } 71,
{ 41: } 71,
{ 42: } 71,
{ 43: } 71,
{ 44: } 71,
{ 45: } 73,
{ 46: } 74,
{ 47: } 74,
{ 48: } 75,
{ 49: } 81,
{ 50: } 97,
{ 51: } 98,
{ 52: } 99,
{ 53: } 100,
{ 54: } 101,
{ 55: } 101,
{ 56: } 101,
{ 57: } 101,
{ 58: } 101,
{ 59: } 101,
{ 60: } 101,
{ 61: } 101,
{ 62: } 101,
{ 63: } 101,
{ 64: } 101,
{ 65: } 101,
{ 66: } 107,
{ 67: } 108,
{ 68: } 108,
{ 69: } 108,
{ 70: } 109,
{ 71: } 109,
{ 72: } 109,
{ 73: } 110,
{ 74: } 112,
{ 75: } 112,
{ 76: } 128,
{ 77: } 144,
{ 78: } 160,
{ 79: } 176,
{ 80: } 192,
{ 81: } 208,
{ 82: } 214,
{ 83: } 216,
{ 84: } 218,
{ 85: } 220,
{ 86: } 238,
{ 87: } 238,
{ 88: } 243,
{ 89: } 245,
{ 90: } 245,
{ 91: } 245,
{ 92: } 249,
{ 93: } 252,
{ 94: } 252,
{ 95: } 254,
{ 96: } 259,
{ 97: } 261,
{ 98: } 261,
{ 99: } 261,
{ 100: } 262,
{ 101: } 264,
{ 102: } 271,
{ 103: } 271,
{ 104: } 286,
{ 105: } 301,
{ 106: } 301,
{ 107: } 301,
{ 108: } 301,
{ 109: } 302,
{ 110: } 303,
{ 111: } 304,
{ 112: } 307,
{ 113: } 316,
{ 114: } 334,
{ 115: } 334,
{ 116: } 334,
{ 117: } 334,
{ 118: } 335,
{ 119: } 335,
{ 120: } 336,
{ 121: } 338,
{ 122: } 339,
{ 123: } 339,
{ 124: } 357,
{ 125: } 357,
{ 126: } 357,
{ 127: } 358,
{ 128: } 358,
{ 129: } 359,
{ 130: } 359,
{ 131: } 359,
{ 132: } 375,
{ 133: } 377,
{ 134: } 379,
{ 135: } 379,
{ 136: } 379,
{ 137: } 397,
{ 138: } 415,
{ 139: } 431,
{ 140: } 447,
{ 141: } 463,
{ 142: } 479,
{ 143: } 495,
{ 144: } 511,
{ 145: } 511,
{ 146: } 514,
{ 147: } 517,
{ 148: } 517,
{ 149: } 518,
{ 150: } 519,
{ 151: } 520,
{ 152: } 522,
{ 153: } 522,
{ 154: } 525,
{ 155: } 525,
{ 156: } 525,
{ 157: } 532,
{ 158: } 533,
{ 159: } 534,
{ 160: } 534,
{ 161: } 538,
{ 162: } 547,
{ 163: } 556,
{ 164: } 565,
{ 165: } 574,
{ 166: } 583,
{ 167: } 592,
{ 168: } 593,
{ 169: } 593,
{ 170: } 599,
{ 171: } 600,
{ 172: } 600,
{ 173: } 600,
{ 174: } 600,
{ 175: } 600,
{ 176: } 600,
{ 177: } 600,
{ 178: } 600,
{ 179: } 600,
{ 180: } 600,
{ 181: } 601,
{ 182: } 602,
{ 183: } 603,
{ 184: } 604,
{ 185: } 605,
{ 186: } 606,
{ 187: } 614,
{ 188: } 615,
{ 189: } 615,
{ 190: } 617,
{ 191: } 617,
{ 192: } 617,
{ 193: } 617,
{ 194: } 617,
{ 195: } 617,
{ 196: } 617,
{ 197: } 617,
{ 198: } 617,
{ 199: } 617,
{ 200: } 617,
{ 201: } 617,
{ 202: } 619,
{ 203: } 621,
{ 204: } 623,
{ 205: } 625,
{ 206: } 627,
{ 207: } 627,
{ 208: } 629,
{ 209: } 630,
{ 210: } 630,
{ 211: } 631,
{ 212: } 631,
{ 213: } 631,
{ 214: } 631,
{ 215: } 631,
{ 216: } 632,
{ 217: } 632,
{ 218: } 637,
{ 219: } 637,
{ 220: } 638,
{ 221: } 639,
{ 222: } 640,
{ 223: } 641,
{ 224: } 642,
{ 225: } 650,
{ 226: } 651,
{ 227: } 652,
{ 228: } 652,
{ 229: } 652,
{ 230: } 652,
{ 231: } 652,
{ 232: } 652,
{ 233: } 652,
{ 234: } 654,
{ 235: } 654
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 3,
{ 2: } 3,
{ 3: } 4,
{ 4: } 4,
{ 5: } 5,
{ 6: } 5,
{ 7: } 5,
{ 8: } 5,
{ 9: } 7,
{ 10: } 8,
{ 11: } 8,
{ 12: } 10,
{ 13: } 10,
{ 14: } 11,
{ 15: } 12,
{ 16: } 13,
{ 17: } 13,
{ 18: } 13,
{ 19: } 14,
{ 20: } 14,
{ 21: } 14,
{ 22: } 14,
{ 23: } 14,
{ 24: } 14,
{ 25: } 15,
{ 26: } 16,
{ 27: } 17,
{ 28: } 17,
{ 29: } 17,
{ 30: } 17,
{ 31: } 17,
{ 32: } 17,
{ 33: } 17,
{ 34: } 17,
{ 35: } 17,
{ 36: } 18,
{ 37: } 21,
{ 38: } 24,
{ 39: } 24,
{ 40: } 24,
{ 41: } 25,
{ 42: } 25,
{ 43: } 25,
{ 44: } 25,
{ 45: } 25,
{ 46: } 27,
{ 47: } 28,
{ 48: } 28,
{ 49: } 28,
{ 50: } 28,
{ 51: } 31,
{ 52: } 31,
{ 53: } 31,
{ 54: } 31,
{ 55: } 31,
{ 56: } 31,
{ 57: } 31,
{ 58: } 31,
{ 59: } 31,
{ 60: } 31,
{ 61: } 31,
{ 62: } 31,
{ 63: } 31,
{ 64: } 31,
{ 65: } 31,
{ 66: } 31,
{ 67: } 31,
{ 68: } 31,
{ 69: } 32,
{ 70: } 33,
{ 71: } 39,
{ 72: } 39,
{ 73: } 39,
{ 74: } 40,
{ 75: } 40,
{ 76: } 40,
{ 77: } 44,
{ 78: } 47,
{ 79: } 50,
{ 80: } 53,
{ 81: } 56,
{ 82: } 59,
{ 83: } 59,
{ 84: } 60,
{ 85: } 61,
{ 86: } 62,
{ 87: } 66,
{ 88: } 66,
{ 89: } 68,
{ 90: } 69,
{ 91: } 69,
{ 92: } 69,
{ 93: } 69,
{ 94: } 70,
{ 95: } 70,
{ 96: } 75,
{ 97: } 75,
{ 98: } 75,
{ 99: } 75,
{ 100: } 75,
{ 101: } 75,
{ 102: } 75,
{ 103: } 75,
{ 104: } 75,
{ 105: } 75,
{ 106: } 75,
{ 107: } 75,
{ 108: } 75,
{ 109: } 75,
{ 110: } 75,
{ 111: } 75,
{ 112: } 75,
{ 113: } 75,
{ 114: } 77,
{ 115: } 81,
{ 116: } 81,
{ 117: } 81,
{ 118: } 82,
{ 119: } 82,
{ 120: } 82,
{ 121: } 82,
{ 122: } 83,
{ 123: } 84,
{ 124: } 84,
{ 125: } 88,
{ 126: } 88,
{ 127: } 88,
{ 128: } 89,
{ 129: } 89,
{ 130: } 89,
{ 131: } 89,
{ 132: } 89,
{ 133: } 92,
{ 134: } 93,
{ 135: } 94,
{ 136: } 94,
{ 137: } 94,
{ 138: } 98,
{ 139: } 102,
{ 140: } 105,
{ 141: } 108,
{ 142: } 111,
{ 143: } 114,
{ 144: } 117,
{ 145: } 120,
{ 146: } 120,
{ 147: } 122,
{ 148: } 123,
{ 149: } 123,
{ 150: } 123,
{ 151: } 123,
{ 152: } 123,
{ 153: } 123,
{ 154: } 123,
{ 155: } 123,
{ 156: } 123,
{ 157: } 123,
{ 158: } 123,
{ 159: } 123,
{ 160: } 123,
{ 161: } 123,
{ 162: } 123,
{ 163: } 123,
{ 164: } 123,
{ 165: } 123,
{ 166: } 123,
{ 167: } 123,
{ 168: } 123,
{ 169: } 123,
{ 170: } 123,
{ 171: } 124,
{ 172: } 124,
{ 173: } 125,
{ 174: } 125,
{ 175: } 125,
{ 176: } 125,
{ 177: } 125,
{ 178: } 125,
{ 179: } 125,
{ 180: } 125,
{ 181: } 125,
{ 182: } 125,
{ 183: } 125,
{ 184: } 125,
{ 185: } 125,
{ 186: } 129,
{ 187: } 133,
{ 188: } 134,
{ 189: } 135,
{ 190: } 136,
{ 191: } 137,
{ 192: } 137,
{ 193: } 137,
{ 194: } 137,
{ 195: } 137,
{ 196: } 137,
{ 197: } 137,
{ 198: } 137,
{ 199: } 137,
{ 200: } 137,
{ 201: } 137,
{ 202: } 137,
{ 203: } 138,
{ 204: } 139,
{ 205: } 140,
{ 206: } 141,
{ 207: } 141,
{ 208: } 141,
{ 209: } 143,
{ 210: } 143,
{ 211: } 143,
{ 212: } 143,
{ 213: } 143,
{ 214: } 143,
{ 215: } 143,
{ 216: } 143,
{ 217: } 143,
{ 218: } 143,
{ 219: } 144,
{ 220: } 144,
{ 221: } 144,
{ 222: } 144,
{ 223: } 144,
{ 224: } 148,
{ 225: } 152,
{ 226: } 153,
{ 227: } 154,
{ 228: } 158,
{ 229: } 158,
{ 230: } 158,
{ 231: } 158,
{ 232: } 158,
{ 233: } 158,
{ 234: } 158,
{ 235: } 158
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 2,
{ 1: } 2,
{ 2: } 3,
{ 3: } 3,
{ 4: } 4,
{ 5: } 4,
{ 6: } 4,
{ 7: } 4,
{ 8: } 6,
{ 9: } 7,
{ 10: } 7,
{ 11: } 9,
{ 12: } 9,
{ 13: } 10,
{ 14: } 11,
{ 15: } 12,
{ 16: } 12,
{ 17: } 12,
{ 18: } 13,
{ 19: } 13,
{ 20: } 13,
{ 21: } 13,
{ 22: } 13,
{ 23: } 13,
{ 24: } 14,
{ 25: } 15,
{ 26: } 16,
{ 27: } 16,
{ 28: } 16,
{ 29: } 16,
{ 30: } 16,
{ 31: } 16,
{ 32: } 16,
{ 33: } 16,
{ 34: } 16,
{ 35: } 17,
{ 36: } 20,
{ 37: } 23,
{ 38: } 23,
{ 39: } 23,
{ 40: } 24,
{ 41: } 24,
{ 42: } 24,
{ 43: } 24,
{ 44: } 24,
{ 45: } 26,
{ 46: } 27,
{ 47: } 27,
{ 48: } 27,
{ 49: } 27,
{ 50: } 30,
{ 51: } 30,
{ 52: } 30,
{ 53: } 30,
{ 54: } 30,
{ 55: } 30,
{ 56: } 30,
{ 57: } 30,
{ 58: } 30,
{ 59: } 30,
{ 60: } 30,
{ 61: } 30,
{ 62: } 30,
{ 63: } 30,
{ 64: } 30,
{ 65: } 30,
{ 66: } 30,
{ 67: } 30,
{ 68: } 31,
{ 69: } 32,
{ 70: } 38,
{ 71: } 38,
{ 72: } 38,
{ 73: } 39,
{ 74: } 39,
{ 75: } 39,
{ 76: } 43,
{ 77: } 46,
{ 78: } 49,
{ 79: } 52,
{ 80: } 55,
{ 81: } 58,
{ 82: } 58,
{ 83: } 59,
{ 84: } 60,
{ 85: } 61,
{ 86: } 65,
{ 87: } 65,
{ 88: } 67,
{ 89: } 68,
{ 90: } 68,
{ 91: } 68,
{ 92: } 68,
{ 93: } 69,
{ 94: } 69,
{ 95: } 74,
{ 96: } 74,
{ 97: } 74,
{ 98: } 74,
{ 99: } 74,
{ 100: } 74,
{ 101: } 74,
{ 102: } 74,
{ 103: } 74,
{ 104: } 74,
{ 105: } 74,
{ 106: } 74,
{ 107: } 74,
{ 108: } 74,
{ 109: } 74,
{ 110: } 74,
{ 111: } 74,
{ 112: } 74,
{ 113: } 76,
{ 114: } 80,
{ 115: } 80,
{ 116: } 80,
{ 117: } 81,
{ 118: } 81,
{ 119: } 81,
{ 120: } 81,
{ 121: } 82,
{ 122: } 83,
{ 123: } 83,
{ 124: } 87,
{ 125: } 87,
{ 126: } 87,
{ 127: } 88,
{ 128: } 88,
{ 129: } 88,
{ 130: } 88,
{ 131: } 88,
{ 132: } 91,
{ 133: } 92,
{ 134: } 93,
{ 135: } 93,
{ 136: } 93,
{ 137: } 97,
{ 138: } 101,
{ 139: } 104,
{ 140: } 107,
{ 141: } 110,
{ 142: } 113,
{ 143: } 116,
{ 144: } 119,
{ 145: } 119,
{ 146: } 121,
{ 147: } 122,
{ 148: } 122,
{ 149: } 122,
{ 150: } 122,
{ 151: } 122,
{ 152: } 122,
{ 153: } 122,
{ 154: } 122,
{ 155: } 122,
{ 156: } 122,
{ 157: } 122,
{ 158: } 122,
{ 159: } 122,
{ 160: } 122,
{ 161: } 122,
{ 162: } 122,
{ 163: } 122,
{ 164: } 122,
{ 165: } 122,
{ 166: } 122,
{ 167: } 122,
{ 168: } 122,
{ 169: } 122,
{ 170: } 123,
{ 171: } 123,
{ 172: } 124,
{ 173: } 124,
{ 174: } 124,
{ 175: } 124,
{ 176: } 124,
{ 177: } 124,
{ 178: } 124,
{ 179: } 124,
{ 180: } 124,
{ 181: } 124,
{ 182: } 124,
{ 183: } 124,
{ 184: } 124,
{ 185: } 128,
{ 186: } 132,
{ 187: } 133,
{ 188: } 134,
{ 189: } 135,
{ 190: } 136,
{ 191: } 136,
{ 192: } 136,
{ 193: } 136,
{ 194: } 136,
{ 195: } 136,
{ 196: } 136,
{ 197: } 136,
{ 198: } 136,
{ 199: } 136,
{ 200: } 136,
{ 201: } 136,
{ 202: } 137,
{ 203: } 138,
{ 204: } 139,
{ 205: } 140,
{ 206: } 140,
{ 207: } 140,
{ 208: } 142,
{ 209: } 142,
{ 210: } 142,
{ 211: } 142,
{ 212: } 142,
{ 213: } 142,
{ 214: } 142,
{ 215: } 142,
{ 216: } 142,
{ 217: } 142,
{ 218: } 143,
{ 219: } 143,
{ 220: } 143,
{ 221: } 143,
{ 222: } 143,
{ 223: } 147,
{ 224: } 151,
{ 225: } 152,
{ 226: } 153,
{ 227: } 157,
{ 228: } 157,
{ 229: } 157,
{ 230: } 157,
{ 231: } 157,
{ 232: } 157,
{ 233: } 157,
{ 234: } 157,
{ 235: } 157
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 0; sym: -49 ),
{ 2: } ( len: 3; sym: -48 ),
{ 3: } ( len: 7; sym: -2 ),
{ 4: } ( len: 2; sym: -4 ),
{ 5: } ( len: 1; sym: -4 ),
{ 6: } ( len: 5; sym: -5 ),
{ 7: } ( len: 3; sym: -6 ),
{ 8: } ( len: 2; sym: -6 ),
{ 9: } ( len: 2; sym: -6 ),
{ 10: } ( len: 0; sym: -7 ),
{ 11: } ( len: 1; sym: -7 ),
{ 12: } ( len: 3; sym: -8 ),
{ 13: } ( len: 3; sym: -9 ),
{ 14: } ( len: 1; sym: -9 ),
{ 15: } ( len: 2; sym: -10 ),
{ 16: } ( len: 1; sym: -10 ),
{ 17: } ( len: 3; sym: -11 ),
{ 18: } ( len: 3; sym: -12 ),
{ 19: } ( len: 1; sym: -12 ),
{ 20: } ( len: 3; sym: -13 ),
{ 21: } ( len: 3; sym: -13 ),
{ 22: } ( len: 3; sym: -13 ),
{ 23: } ( len: 3; sym: -13 ),
{ 24: } ( len: 3; sym: -13 ),
{ 25: } ( len: 3; sym: -13 ),
{ 26: } ( len: 4; sym: -13 ),
{ 27: } ( len: 4; sym: -13 ),
{ 28: } ( len: 6; sym: -13 ),
{ 29: } ( len: 6; sym: -13 ),
{ 30: } ( len: 4; sym: -13 ),
{ 31: } ( len: 1; sym: -13 ),
{ 32: } ( len: 1; sym: -13 ),
{ 33: } ( len: 1; sym: -14 ),
{ 34: } ( len: 1; sym: -14 ),
{ 35: } ( len: 1; sym: -14 ),
{ 36: } ( len: 1; sym: -14 ),
{ 37: } ( len: 1; sym: -14 ),
{ 38: } ( len: 1; sym: -14 ),
{ 39: } ( len: 1; sym: -14 ),
{ 40: } ( len: 1; sym: -14 ),
{ 41: } ( len: 3; sym: -15 ),
{ 42: } ( len: 1; sym: -15 ),
{ 43: } ( len: 2; sym: -16 ),
{ 44: } ( len: 3; sym: -16 ),
{ 45: } ( len: 3; sym: -16 ),
{ 46: } ( len: 3; sym: -16 ),
{ 47: } ( len: 3; sym: -16 ),
{ 48: } ( len: 3; sym: -16 ),
{ 49: } ( len: 3; sym: -16 ),
{ 50: } ( len: 3; sym: -16 ),
{ 51: } ( len: 3; sym: -16 ),
{ 52: } ( len: 1; sym: -16 ),
{ 53: } ( len: 0; sym: -44 ),
{ 54: } ( len: 2; sym: -44 ),
{ 55: } ( len: 3; sym: -45 ),
{ 56: } ( len: 2; sym: -46 ),
{ 57: } ( len: 8; sym: -3 ),
{ 58: } ( len: 3; sym: -17 ),
{ 59: } ( len: 3; sym: -18 ),
{ 60: } ( len: 6; sym: -23 ),
{ 61: } ( len: 0; sym: -19 ),
{ 62: } ( len: 3; sym: -19 ),
{ 63: } ( len: 2; sym: -20 ),
{ 64: } ( len: 1; sym: -20 ),
{ 65: } ( len: 2; sym: -21 ),
{ 66: } ( len: 0; sym: -22 ),
{ 67: } ( len: 3; sym: -22 ),
{ 68: } ( len: 1; sym: -24 ),
{ 69: } ( len: 1; sym: -24 ),
{ 70: } ( len: 1; sym: -24 ),
{ 71: } ( len: 3; sym: -25 ),
{ 72: } ( len: 1; sym: -25 ),
{ 73: } ( len: 4; sym: -26 ),
{ 74: } ( len: 2; sym: -27 ),
{ 75: } ( len: 1; sym: -27 ),
{ 76: } ( len: 0; sym: -28 ),
{ 77: } ( len: 2; sym: -28 ),
{ 78: } ( len: 3; sym: -29 ),
{ 79: } ( len: 2; sym: -30 ),
{ 80: } ( len: 2; sym: -30 ),
{ 81: } ( len: 0; sym: -31 ),
{ 82: } ( len: 8; sym: -31 ),
{ 83: } ( len: 0; sym: -32 ),
{ 84: } ( len: 2; sym: -32 ),
{ 85: } ( len: 6; sym: -33 ),
{ 86: } ( len: 0; sym: -34 ),
{ 87: } ( len: 2; sym: -34 ),
{ 88: } ( len: 3; sym: -35 ),
{ 89: } ( len: 2; sym: -36 ),
{ 90: } ( len: 2; sym: -36 ),
{ 91: } ( len: 2; sym: -36 ),
{ 92: } ( len: 2; sym: -36 ),
{ 93: } ( len: 2; sym: -36 ),
{ 94: } ( len: 2; sym: -36 ),
{ 95: } ( len: 1; sym: -37 ),
{ 96: } ( len: 1; sym: -37 ),
{ 97: } ( len: 1; sym: -37 ),
{ 98: } ( len: 1; sym: -37 ),
{ 99: } ( len: 2; sym: -37 ),
{ 100: } ( len: 2; sym: -37 ),
{ 101: } ( len: 2; sym: -37 ),
{ 102: } ( len: 2; sym: -37 ),
{ 103: } ( len: 0; sym: -39 ),
{ 104: } ( len: 3; sym: -39 ),
{ 105: } ( len: 2; sym: -38 ),
{ 106: } ( len: 1; sym: -38 ),
{ 107: } ( len: 0; sym: -40 ),
{ 108: } ( len: 0; sym: -53 ),
{ 109: } ( len: 6; sym: -40 ),
{ 110: } ( len: 0; sym: -41 ),
{ 111: } ( len: 2; sym: -41 ),
{ 112: } ( len: 3; sym: -42 ),
{ 113: } ( len: 2; sym: -43 ),
{ 114: } ( len: 2; sym: -43 ),
{ 115: } ( len: 2; sym: -43 ),
{ 116: } ( len: 2; sym: -43 ),
{ 117: } ( len: 2; sym: -43 ),
{ 118: } ( len: 1; sym: -47 ),
{ 119: } ( len: 1; sym: -47 )
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
  CloseFile(yyinput);
  CloseFile(yyoutput);
end { TParserConfiguracion.Parsear };

end { UnitParserConfiguracion }.
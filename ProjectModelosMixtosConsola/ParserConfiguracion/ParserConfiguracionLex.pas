
(* lexical analyzer template (TP Lex V3.0), V1.0 3-2-91 AG *)

(* global definitions: *)

var
  {** Caracter utilizado como buffer para la eliminacion de los comentarios
      de la fuente de configuracion. }
  C: Char;
  {** Token del ultimo lexeme escaneado. }
  Token: Integer;

const
  {** Cantidad de palabras clave. }
  CantidadPalabrasClave = 80;
  {** Identificadores de palabras clave. }
  PalabrasClave: array [1 .. CantidadPalabrasClave] of ShortString = (
    'true', 'false',  'datos', 'columnas', 'categoricos',
    'enteros', 'reales', 'valores', 'modelo', 'variables_clasificacion',
    'variable_dependiente', 'efectos_fijos', 'efectos', 'intercepto',
    'opcion', 'generar_columnas_nulas', 'incluir_intercepto',
    'efectos_aleatorios', 'grupo',
    'criterio_agrupamiento_unidad_experimental', 'unidad_experimental',
    'estructura', 'simetria_compuesta', 'componentes_varianza', 'general',
    'diagonal_heterogenea', 'factor_analytic', 'factor_analytic_sin_diagonal',
    'factor_analytic_diagonal_escalar', 'bandeada', 'autoregresiva',
    'parametros', 'error', 'criterio_ordenamiento', 'peso', 'procedimiento',
    'metodo', 'mivque0', 'ml', 'reml', 'grilla_busqueda_ml',
    'grilla_busqueda_reml', 'valor_inicial_parametros',
    'fuente_valores_iniciales_parametros', 'valores_por_defecto',
    'valores_configuracion', 'criterio_convergencia',
    'convergencia_parametros_absoluta', 'convergencia_parametros_relativa',
    'convergencia_verosimilitud_absoluta',
    'convergencia_verosimilitud_relativa',
    'convergencia_gradiente_absoluta', 'convergencia_gradiente_relativa',
    'convergencia_hessiano_absoluta', 'convergencia_hessiano_relativa',
    'tolerancia_convergencia', 'cantidad_pasos_scoring',
    'cota_inferior_parametros', 'cota_superior_parametros',
    'paso_grilla_parametros', 'verificar_cota_inferior_parametros',
    'verificar_cota_superior_parametros', 'cantidad_maxima_iteraciones',
    'cantidad_maxima_evaluaciones_funcion_objetivo',
    'tolerancia_singularidad', 'tolerancia_cholesky',
    'valor_inicial_factor_ridging', 'factor_incremento_factor_ridging',
    'factor_decremento_factor_ridging', 'valor_minimo_factor_ridging',
    'valor_maximo_factor_ridging', 'salida', 'depuracion',
    'archivo_salida', 'generar_salida_compacta', 'mostrar_salida_consola',
    'archivo_resumen_verosimilitud', 'archivo_resumen_beta',
    'archivo_resumen_theta', 'archivo_resumen_iteracion' );
  {** Tokens asociados a las palabras clave. Notar que el arreglo se
      encuentra ordenado de la misma manera que @link(PalabrasClave). }
  TokensPalabrasClave:  array [1 .. CantidadPalabrasClave] of Integer = (
    T_BOOLEAN, T_BOOLEAN, T_DATOS, T_COLUMNAS,
    T_CATEGORICOS, T_ENTEROS, T_REALES, T_VALORES, T_MODELO,
    T_VARIABLES_CLASIFICACION, T_VARIABLE_DEPENDIENTE, T_EFECTOS_FIJOS,
    T_EFECTOS, T_INTERCEPTO, T_OPCION, T_GENERAR_COLUMNAS_NULAS,
    T_INCLUIR_INTERCEPTO, T_EFECTOS_ALEATORIOS, T_GRUPO,
    T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL, T_UNIDAD_EXPERIMENTAL,
    T_ESTRUCTURA, T_SIMETRIA_COMPUESTA, T_COMPONENTES_VARIANZA, T_GENERAL,
    T_DIAGONAL_HETEROGENEA, T_FACTOR_ANALYTIC, T_FACTOR_ANALYTIC_SIN_DIAGONAL,
    T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR, T_BANDEADA, T_AUTOREGRESIVA,
    T_PARAMETROS, T_ERROR, T_CRITERIO_ORDENAMIENTO, T_PESO, T_PROCEDIMIENTO,
    T_METODO, T_MIVQUE0, T_ML, T_REML, T_GRILLA_BUSQUEDA_ML,
    T_GRILLA_BUSQUEDA_REML, T_VALOR_INICIAL_PARAMETROS,
    T_FUENTE_VALORES_INICIALES_PARAMETROS, T_VALORES_POR_DEFECTO,
    T_VALORES_CONFIGURACION, T_CRITERIO_CONVERGENCIA,
    T_CONVERGENCIA_PARAMETROS_ABSOLUTA, T_CONVERGENCIA_PARAMETROS_RELATIVA,
    T_CONVERGENCIA_VEROSIMILITUD_ABSOLUTA,
    T_CONVERGENCIA_VEROSIMILITUD_RELATIVA, T_CONVERGENCIA_GRADIENTE_ABSOLUTA,
    T_CONVERGENCIA_GRADIENTE_RELATIVA, T_CONVERGENCIA_HESSIANO_ABSOLUTA,
    T_CONVERGENCIA_HESSIANO_RELATIVA, T_TOLERANCIA_CONVERGENCIA,
    T_CANTIDAD_PASOS_SCORING, T_COTA_INFERIOR_PARAMETROS,
    T_COTA_SUPERIOR_PARAMETROS, T_PASO_GRILLA_PARAMETROS,
    T_VERIFICAR_COTA_INFERIOR_PARAMETROS,
    T_VERIFICAR_COTA_SUPERIOR_PARAMETROS, T_CANTIDAD_MAXIMA_ITERACIONES,
    T_CANTIDAD_MAXIMA_EVALUACIONES_FUNCION_OBJETIVO,
    T_TOLERANCIA_SINGULARIDAD, T_TOLERANCIA_CHOLEKSY,
    T_VALOR_INICIAL_FACTOR_RIDGING, T_FACTOR_INCREMENTO_FACTOR_RIDGING,
    T_FACTOR_DECREMENTO_FACTOR_RIDGING, T_VALOR_MINIMO_FACTOR_RIDGING,
    T_VALOR_MAXIMO_FACTOR_RIDGING, T_SALIDA, T_DEPURACION,
    T_ARCHIVO_SALIDA, T_GENERAR_SALIDA_COMPACTA, T_MOSTRAR_SALIDA_CONSOLA,
    T_ARCHIVO_RESUMEN_VEROSIMILITUD, T_ARCHIVO_RESUMEN_BETA,
    T_ARCHIVO_RESUMEN_THETA, T_ARCHIVO_RESUMEN_ITERACION );

{** Funcion que asocia identificadores a tokens de acuerdo a
    @link(PalabrasClave) y @link(TokensPalabrasClave), buscando en el
    primero. Solo se realiza la busqueda si IdentificarTokensPalabrasClave
    es @true.
    @param(Identificador Cadena de caracteres a buscar en la tabla de
    palabras reservadas.)
    @param(Devuelve el token (valor entero) correspondiente al identificador
    en caso que el mismo se encontrara en la tabla de palabras reservadas
    o T_IDENTIFICADOR en caso contrario.) }
function TokenPalabraClave(const Identificador: ShortString): Integer;
var
  I, Token: Integer;
  Listo: Boolean;
  S: ShortString;
begin { TokenPalabraClave }
  S := LowerCase(Identificador);
  
  Listo := False;
  I := 1;
  Token := T_IDENTIFICADOR;
  while (I <= CantidadPalabrasClave) and (not Listo) do
  begin
    if S = PalabrasClave [I] then
    begin
      Listo := True;
      Token := TokensPalabrasClave [I];
    end;
    
    Inc(I);
  end;
  
  TokenPalabraClave := Token;
end { TokenPalabraClave };



function yylex : Integer;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)

begin
  (* actions: *)
  case yyruleno of
  1:
     begin
       while not Eof(yyinput) do
       begin
         C := get_char;
         if C = '*' then
         begin
           C := get_char;
           if C = '/' then
             exit
           else
             unget_char(C);
         end;
       end;
     end;

  2:
            ;

  3:
    return(T_LLAVE_APERTURA);
  4:
    return(T_LLAVE_CLAUSURA);
  5:
    return(T_PARENTESIS_APERTURA);
  6:
    return(T_PARENTESIS_CLAUSURA);
  7:
    return(T_CORCHETE_APERTURA);
  8:
    return(T_CORCHETE_CLAUSURA);
  9:
    return(T_DOS_PUNTOS);
  10:
    return(T_COMA);
  11:
    return(T_PUNTO_Y_COMA);
  12:
    return(T_ASTERISCO);

  13:
                                         begin
                                           yylval.yyInteger := StrToInt(yytext);
                                           return(T_ENTERO);
                                         end;

  14:
                                         begin
                                           yylval.yyReal := StrToFloat(yytext);
                                           return(T_REAL);
                                         end;

  15:
                                         begin
                                           if IdentificarTokensPalabrasClave then
                                           begin
                                             Token := TokenPalabraClave(yytext);

                                             if (Token = T_BOOLEAN) then
                                               yylval.yyBoolean := StrToBool(yytext)
                                             else if (Token = T_IDENTIFICADOR) then
                                               yylval.yyShortString := Copy(yytext, 1, Length(yytext))
                                             else
                                               yylval.yyShortString := Lowercase(Copy(yytext, 1, Length(yytext)));

                                             return(Token);
                                           end
                                           else
                                           begin
                                             yylval.yyShortString := Copy(yytext, 1, Length(yytext));
                                             return(T_IDENTIFICADOR);
                                           end;
                                         end;

  16:
                                         begin
                                           yylval.yyShortString := Copy(yytext, 2, Length(yytext) - 2);
                                           return(T_LITERAL_CADENAS);
                                         end;

  17:
                                       ;
  18:
                                       ;

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 42;
yynmatches = 42;
yyntrans   = 67;
yynstates  = 35;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  18,
  { 3: }
  3,
  18,
  { 4: }
  4,
  18,
  { 5: }
  5,
  18,
  { 6: }
  6,
  18,
  { 7: }
  7,
  18,
  { 8: }
  8,
  18,
  { 9: }
  9,
  18,
  { 10: }
  10,
  18,
  { 11: }
  11,
  18,
  { 12: }
  12,
  18,
  { 13: }
  18,
  { 14: }
  13,
  14,
  18,
  { 15: }
  15,
  18,
  { 16: }
  18,
  { 17: }
  18,
  { 18: }
  17,
  { 19: }
  17,
  18,
  { 20: }
  18,
  { 21: }
  1,
  { 22: }
  { 23: }
  13,
  14,
  { 24: }
  { 25: }
  { 26: }
  15,
  { 27: }
  { 28: }
  16,
  { 29: }
  { 30: }
  2,
  { 31: }
  14,
  { 32: }
  { 33: }
  14,
  { 34: }
  2
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  18,
{ 3: }
  3,
  18,
{ 4: }
  4,
  18,
{ 5: }
  5,
  18,
{ 6: }
  6,
  18,
{ 7: }
  7,
  18,
{ 8: }
  8,
  18,
{ 9: }
  9,
  18,
{ 10: }
  10,
  18,
{ 11: }
  11,
  18,
{ 12: }
  12,
  18,
{ 13: }
  18,
{ 14: }
  13,
  14,
  18,
{ 15: }
  15,
  18,
{ 16: }
  18,
{ 17: }
  18,
{ 18: }
  17,
{ 19: }
  17,
  18,
{ 20: }
  18,
{ 21: }
  1,
{ 22: }
{ 23: }
  13,
  14,
{ 24: }
{ 25: }
{ 26: }
  15,
{ 27: }
{ 28: }
  16,
{ 29: }
{ 30: }
  2,
{ 31: }
  14,
{ 32: }
{ 33: }
  14,
{ 34: }
  2
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11..#31,'!','#'..'&','.','<'..'@',
            '\','^','`','|','~'..#255 ]; s: 20),
  ( cc: [ #9,' ' ]; s: 19),
  ( cc: [ #10 ]; s: 18),
  ( cc: [ '"' ]; s: 16),
  ( cc: [ '''' ]; s: 17),
  ( cc: [ '(' ]; s: 5),
  ( cc: [ ')' ]; s: 6),
  ( cc: [ '*' ]; s: 12),
  ( cc: [ '+','-' ]; s: 13),
  ( cc: [ ',' ]; s: 10),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0'..'9' ]; s: 14),
  ( cc: [ ':' ]; s: 9),
  ( cc: [ ';' ]; s: 11),
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 15),
  ( cc: [ '[' ]; s: 7),
  ( cc: [ ']' ]; s: 8),
  ( cc: [ '{' ]; s: 3),
  ( cc: [ '}' ]; s: 4),
{ 1: }
  ( cc: [ #1..#8,#11..#31,'!','#'..'&','.','<'..'@',
            '\','^','`','|','~'..#255 ]; s: 20),
  ( cc: [ #9,' ' ]; s: 19),
  ( cc: [ #10 ]; s: 18),
  ( cc: [ '"' ]; s: 16),
  ( cc: [ '''' ]; s: 17),
  ( cc: [ '(' ]; s: 5),
  ( cc: [ ')' ]; s: 6),
  ( cc: [ '*' ]; s: 12),
  ( cc: [ '+','-' ]; s: 13),
  ( cc: [ ',' ]; s: 10),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0'..'9' ]; s: 14),
  ( cc: [ ':' ]; s: 9),
  ( cc: [ ';' ]; s: 11),
  ( cc: [ 'A'..'Z','_','a'..'z' ]; s: 15),
  ( cc: [ '[' ]; s: 7),
  ( cc: [ ']' ]; s: 8),
  ( cc: [ '{' ]; s: 3),
  ( cc: [ '}' ]; s: 4),
{ 2: }
  ( cc: [ '*' ]; s: 21),
  ( cc: [ '/' ]; s: 22),
{ 3: }
{ 4: }
{ 5: }
{ 6: }
{ 7: }
{ 8: }
{ 9: }
{ 10: }
{ 11: }
{ 12: }
{ 13: }
  ( cc: [ '0'..'9' ]; s: 23),
{ 14: }
  ( cc: [ '.' ]; s: 24),
  ( cc: [ '0'..'9' ]; s: 23),
  ( cc: [ 'E','e' ]; s: 25),
{ 15: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 26),
{ 16: }
  ( cc: [ #1..#9,#11..'!','#'..'&','('..#255 ]; s: 27),
  ( cc: [ '"' ]; s: 28),
{ 17: }
  ( cc: [ #1..#9,#11..'!','#'..'&','('..#255 ]; s: 29),
  ( cc: [ '''' ]; s: 28),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( cc: [ #1..#9,#11..#255 ]; s: 22),
  ( cc: [ #10 ]; s: 30),
{ 23: }
  ( cc: [ '.' ]; s: 24),
  ( cc: [ '0'..'9' ]; s: 23),
  ( cc: [ 'E','e' ]; s: 25),
{ 24: }
  ( cc: [ '0'..'9' ]; s: 31),
{ 25: }
  ( cc: [ '+','-' ]; s: 32),
  ( cc: [ '0'..'9' ]; s: 33),
{ 26: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 26),
{ 27: }
  ( cc: [ #1..#9,#11..'!','#'..'&','('..#255 ]; s: 27),
  ( cc: [ '"' ]; s: 28),
{ 28: }
{ 29: }
  ( cc: [ #1..#9,#11..'!','#'..'&','('..#255 ]; s: 29),
  ( cc: [ '''' ]; s: 28),
{ 30: }
  ( cc: [ #13 ]; s: 34),
{ 31: }
  ( cc: [ '0'..'9' ]; s: 31),
  ( cc: [ 'E','e' ]; s: 25),
{ 32: }
  ( cc: [ '0'..'9' ]; s: 33),
{ 33: }
  ( cc: [ '0'..'9' ]; s: 33)
{ 34: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 2,
{ 4: } 4,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 12,
{ 9: } 14,
{ 10: } 16,
{ 11: } 18,
{ 12: } 20,
{ 13: } 22,
{ 14: } 23,
{ 15: } 26,
{ 16: } 28,
{ 17: } 29,
{ 18: } 30,
{ 19: } 31,
{ 20: } 33,
{ 21: } 34,
{ 22: } 35,
{ 23: } 35,
{ 24: } 37,
{ 25: } 37,
{ 26: } 37,
{ 27: } 38,
{ 28: } 38,
{ 29: } 39,
{ 30: } 39,
{ 31: } 40,
{ 32: } 41,
{ 33: } 41,
{ 34: } 42
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 22,
{ 14: } 25,
{ 15: } 27,
{ 16: } 28,
{ 17: } 29,
{ 18: } 30,
{ 19: } 32,
{ 20: } 33,
{ 21: } 34,
{ 22: } 34,
{ 23: } 36,
{ 24: } 36,
{ 25: } 36,
{ 26: } 37,
{ 27: } 37,
{ 28: } 38,
{ 29: } 38,
{ 30: } 39,
{ 31: } 40,
{ 32: } 40,
{ 33: } 41,
{ 34: } 42
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 2,
{ 4: } 4,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 12,
{ 9: } 14,
{ 10: } 16,
{ 11: } 18,
{ 12: } 20,
{ 13: } 22,
{ 14: } 23,
{ 15: } 26,
{ 16: } 28,
{ 17: } 29,
{ 18: } 30,
{ 19: } 31,
{ 20: } 33,
{ 21: } 34,
{ 22: } 35,
{ 23: } 35,
{ 24: } 37,
{ 25: } 37,
{ 26: } 37,
{ 27: } 38,
{ 28: } 38,
{ 29: } 39,
{ 30: } 39,
{ 31: } 40,
{ 32: } 41,
{ 33: } 41,
{ 34: } 42
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 15,
{ 10: } 17,
{ 11: } 19,
{ 12: } 21,
{ 13: } 22,
{ 14: } 25,
{ 15: } 27,
{ 16: } 28,
{ 17: } 29,
{ 18: } 30,
{ 19: } 32,
{ 20: } 33,
{ 21: } 34,
{ 22: } 34,
{ 23: } 36,
{ 24: } 36,
{ 25: } 36,
{ 26: } 37,
{ 27: } 37,
{ 28: } 38,
{ 29: } 38,
{ 30: } 39,
{ 31: } 40,
{ 32: } 40,
{ 33: } 41,
{ 34: } 42
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 20,
{ 2: } 39,
{ 3: } 41,
{ 4: } 41,
{ 5: } 41,
{ 6: } 41,
{ 7: } 41,
{ 8: } 41,
{ 9: } 41,
{ 10: } 41,
{ 11: } 41,
{ 12: } 41,
{ 13: } 41,
{ 14: } 42,
{ 15: } 45,
{ 16: } 46,
{ 17: } 48,
{ 18: } 50,
{ 19: } 50,
{ 20: } 50,
{ 21: } 50,
{ 22: } 50,
{ 23: } 52,
{ 24: } 55,
{ 25: } 56,
{ 26: } 58,
{ 27: } 59,
{ 28: } 61,
{ 29: } 61,
{ 30: } 63,
{ 31: } 64,
{ 32: } 66,
{ 33: } 67,
{ 34: } 68
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 19,
{ 1: } 38,
{ 2: } 40,
{ 3: } 40,
{ 4: } 40,
{ 5: } 40,
{ 6: } 40,
{ 7: } 40,
{ 8: } 40,
{ 9: } 40,
{ 10: } 40,
{ 11: } 40,
{ 12: } 40,
{ 13: } 41,
{ 14: } 44,
{ 15: } 45,
{ 16: } 47,
{ 17: } 49,
{ 18: } 49,
{ 19: } 49,
{ 20: } 49,
{ 21: } 49,
{ 22: } 51,
{ 23: } 54,
{ 24: } 55,
{ 25: } 57,
{ 26: } 58,
{ 27: } 60,
{ 28: } 60,
{ 29: } 62,
{ 30: } 63,
{ 31: } 65,
{ 32: } 66,
{ 33: } 67,
{ 34: } 67
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not (yyactchar in yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap then
    begin
      yyclear;
      return(0);
    end;

  if not yydone then goto start;

  yylex := yyretval;

end(*yylex*);



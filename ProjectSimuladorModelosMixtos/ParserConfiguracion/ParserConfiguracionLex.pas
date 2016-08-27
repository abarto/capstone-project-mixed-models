
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
  CantidadPalabrasClave = 52;
  {** Identificadores de palabras clave. } 
  PalabrasClave: array [1 .. CantidadPalabrasClave] of ShortString = (
    'true', 'false', 'datos', 'columna', 'categoricos', 'reales',
    'enteros', 'exp', 'ln', 'log2', 'log10', 'log', 'sen', 'cos',
    'tan', 'modelo', 'variables_clasificacion', 'opcion',
    'variable_dependiente', 'efectos_fijos', 'efectos',
    'generar_columnas_nulas', 'incluir_intercepto',
    'efectos_aleatorios', 'grupo', 'error',
    'criterio_agrupamiento_unidad_experimental',
    'unidad_experimental', 'estructura', 'parametros',
    'simetria_compuesta', 'componentes_varianza',
    'general', 'diagonal_heterogenea', 'factor_analytic',
    'factor_analytic_sin_diagonal',
    'factor_analytic_diagonal_escalar', 'bandeada', 'autoregresiva',
    'criterio_ordenamiento', 'cantidad_observaciones',
    'cantidad_ejecuciones', 'archivo_salida',
    'funcion_caracteristica', 'semilla_generador_numeros_aleatorios',
    'beta', 'procedimiento', 'opciones_procedimiento_ajuste',
    'comentar_valores_parametros', 'archivo_resumen_verosimilitud',
    'archivo_resumen_beta', 'archivo_resumen_theta' );
  {** Tokens asociados a las palabras clave. Notar que el arreglo se
      encuentra ordenado de la misma manera que @link(PalabrasClave). }
  TokensPalabrasClave:  array [1 .. CantidadPalabrasClave] of Integer = (
    T_BOOLEAN, T_BOOLEAN, T_DATOS, T_COLUMNA, T_CATEGORICOS, T_REALES,
    T_ENTEROS, T_FUNCION_EXPONENCIAL, T_FUNCION_LOGARITMO_NATURAL,
    T_FUNCION_LOGARITMO_BASE_2, T_FUNCION_LOGARITMO_BASE_10,
    T_FUNCION_LOGARITMO, T_FUNCION_SENO, T_FUNCION_COSENO,
    T_FUNCION_TANGENTE, T_MODELO, T_VARIABLES_CLASIFICACION,
    T_OPCION, T_VARIABLE_DEPENDIENTE, T_EFECTOS_FIJOS, T_EFECTOS,
    T_GENERAR_COLUMNAS_NULAS, T_INCLUIR_INTERCEPTO,
    T_EFECTOS_ALEATORIOS, T_GRUPO, T_ERROR,
    T_CRITERIO_AGRUPAMIENTO_UNIDAD_EXPERIMENTAL,
    T_UNIDAD_EXPERIMENTAL, T_ESTRUCTURA, T_PARAMETROS,
    T_SIMETRIA_COMPUESTA, T_COMPONENTES_VARIANZA, T_GENERAL, 
    T_DIAGONAL_HETEROGENEA, T_FACTOR_ANALYTIC,
    T_FACTOR_ANALYTIC_SIN_DIAGONAL, T_FACTOR_ANALYTIC_DIAGONAL_ESCALAR,
    T_BANDEADA, T_AUTOREGRESIVA, T_CRITERIO_ORDENAMIENTO,
    T_CANTIDAD_OBSERVACIONES, T_CANTIDAD_EJECUCIONES,
    T_ARCHIVO_SALIDA, T_FUNCION_CARACTERISTICA,
    T_SEMILLA_GENERADOR_NUMEROS_ALEATORIOS, T_BETA, T_PROCEDIMIENTO,
    T_OPCIONES_PROCEDIMIENTO_AJUSTE, T_COMENTAR_VALORES_PARAMETROS,
    T_ARCHIVO_RESUMEN_VEROSIMILITUD, T_ARCHIVO_RESUMEN_BETA,
    T_ARCHIVO_RESUMEN_THETA );

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
    return(T_MAS);
  14:
    return(T_MENOS);
  15:
    return(T_BARRA_DERECHA);
  16:
    return(T_CIRCUNFLEJO);
  17:
    return(T_PESOS);

  18:
          return(T_FUNCION_1SI);
  19:
          return(T_OPERADOR_MENOR_QUE);
  20:
          return(T_OPERADOR_MENOR_O_IGUAL_QUE);
  21:
          return(T_OPERADOR_DISTINTO);
  22:
          return(T_OPERADOR_IGUAL);
  23:
          return(T_OPERADOR_MAYOR_QUE);
  24:
          return(T_OPERADOR_MAYOR_O_IGUAL_QUE);

  25:
          return(T_OPERADOR_NEGACION);
  26:
          return(T_OPERADOR_CONJUNCION);
  27:
          return(T_OPERADOR_DISYUNCION);

  28:
           return(T_SIMBOLO_VARIABLE_UNIFORME);
  29:
           return(T_SIMBOLO_VARIABLE_NORMAL);
  30:
           return(T_SIMBOLO_VARIABLE_EXPONENCIAL);

  31:
                                         begin
                                           yylval.yyInteger := StrToInt(yytext);
                                           return(T_ENTERO);
                                         end;

  32:
                                         begin
                                           yylval.yyReal := StrToFloat(yytext);
                                           return(T_REAL);
                                         end;

  33:
                                         begin
                                           if IdentificarTokensPalabrasClave then
                                           begin
                                             Token := TokenPalabraClave(yytext);
                                             
                                             if Token = T_BOOLEAN then
                                               yylval.yyBoolean := StrToBool(yytext)
                                             else if Token = T_IDENTIFICADOR then
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

  34:
                                     begin
                                       yylval.yyShortString := Copy(yytext, 2, Length(yytext) - 2);
                                       return(T_LITERAL_CADENAS);
                                     end;

  35:
                                         begin
                                           yylval.yyInteger := StrToInt(Copy(yytext, 2, Length(yytext) - 1));
                                           return(T_SIMBOLO_VARIABLE);
                                         end;

  36:
                                       ;
  37:
                                       ;

  end;
end(*yyaction*);

(* DFA table: *)

type YYTRec = record
                cc : set of Char;
                s  : Integer;
              end;

const

yynmarks   = 79;
yynmatches = 79;
yyntrans   = 107;
yynstates  = 54;

yyk : array [1..yynmarks] of Integer = (
  { 0: }
  { 1: }
  { 2: }
  15,
  37,
  { 3: }
  3,
  37,
  { 4: }
  4,
  37,
  { 5: }
  5,
  37,
  { 6: }
  6,
  37,
  { 7: }
  7,
  37,
  { 8: }
  8,
  37,
  { 9: }
  9,
  37,
  { 10: }
  10,
  37,
  { 11: }
  11,
  37,
  { 12: }
  12,
  37,
  { 13: }
  13,
  37,
  { 14: }
  14,
  37,
  { 15: }
  16,
  37,
  { 16: }
  17,
  37,
  { 17: }
  31,
  32,
  37,
  { 18: }
  19,
  37,
  { 19: }
  22,
  37,
  { 20: }
  23,
  37,
  { 21: }
  25,
  37,
  { 22: }
  26,
  37,
  { 23: }
  27,
  37,
  { 24: }
  28,
  33,
  37,
  { 25: }
  29,
  33,
  37,
  { 26: }
  30,
  33,
  37,
  { 27: }
  31,
  32,
  37,
  { 28: }
  33,
  37,
  { 29: }
  37,
  { 30: }
  37,
  { 31: }
  36,
  { 32: }
  36,
  37,
  { 33: }
  37,
  { 34: }
  1,
  { 35: }
  { 36: }
  31,
  32,
  { 37: }
  35,
  { 38: }
  { 39: }
  { 40: }
  { 41: }
  20,
  { 42: }
  21,
  { 43: }
  24,
  { 44: }
  33,
  { 45: }
  { 46: }
  34,
  { 47: }
  { 48: }
  2,
  { 49: }
  18,
  { 50: }
  32,
  { 51: }
  { 52: }
  32,
  { 53: }
  2
);

yym : array [1..yynmatches] of Integer = (
{ 0: }
{ 1: }
{ 2: }
  15,
  37,
{ 3: }
  3,
  37,
{ 4: }
  4,
  37,
{ 5: }
  5,
  37,
{ 6: }
  6,
  37,
{ 7: }
  7,
  37,
{ 8: }
  8,
  37,
{ 9: }
  9,
  37,
{ 10: }
  10,
  37,
{ 11: }
  11,
  37,
{ 12: }
  12,
  37,
{ 13: }
  13,
  37,
{ 14: }
  14,
  37,
{ 15: }
  16,
  37,
{ 16: }
  17,
  37,
{ 17: }
  31,
  32,
  37,
{ 18: }
  19,
  37,
{ 19: }
  22,
  37,
{ 20: }
  23,
  37,
{ 21: }
  25,
  37,
{ 22: }
  26,
  37,
{ 23: }
  27,
  37,
{ 24: }
  28,
  33,
  37,
{ 25: }
  29,
  33,
  37,
{ 26: }
  30,
  33,
  37,
{ 27: }
  31,
  32,
  37,
{ 28: }
  33,
  37,
{ 29: }
  37,
{ 30: }
  37,
{ 31: }
  36,
{ 32: }
  36,
  37,
{ 33: }
  37,
{ 34: }
  1,
{ 35: }
{ 36: }
  31,
  32,
{ 37: }
  35,
{ 38: }
{ 39: }
{ 40: }
{ 41: }
  20,
{ 42: }
  21,
{ 43: }
  24,
{ 44: }
  33,
{ 45: }
{ 46: }
  34,
{ 47: }
{ 48: }
  2,
{ 49: }
  18,
{ 50: }
  32,
{ 51: }
{ 52: }
  32,
{ 53: }
  2
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11..#31,'#','%','.','?','@','\','`',
            '~'..#255 ]; s: 33),
  ( cc: [ #9,' ' ]; s: 32),
  ( cc: [ #10 ]; s: 31),
  ( cc: [ '!' ]; s: 21),
  ( cc: [ '"' ]; s: 29),
  ( cc: [ '$' ]; s: 16),
  ( cc: [ '&' ]; s: 22),
  ( cc: [ '''' ]; s: 30),
  ( cc: [ '(' ]; s: 5),
  ( cc: [ ')' ]; s: 6),
  ( cc: [ '*' ]; s: 12),
  ( cc: [ '+' ]; s: 13),
  ( cc: [ ',' ]; s: 10),
  ( cc: [ '-' ]; s: 14),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0','2'..'9' ]; s: 27),
  ( cc: [ '1' ]; s: 17),
  ( cc: [ ':' ]; s: 9),
  ( cc: [ ';' ]; s: 11),
  ( cc: [ '<' ]; s: 18),
  ( cc: [ '=' ]; s: 19),
  ( cc: [ '>' ]; s: 20),
  ( cc: [ 'A'..'D','F'..'M','O'..'T','V'..'Z','_','a'..'d',
            'f'..'m','o'..'t','v'..'z' ]; s: 28),
  ( cc: [ 'E','e' ]; s: 26),
  ( cc: [ 'N','n' ]; s: 25),
  ( cc: [ 'U','u' ]; s: 24),
  ( cc: [ '[' ]; s: 7),
  ( cc: [ ']' ]; s: 8),
  ( cc: [ '^' ]; s: 15),
  ( cc: [ '{' ]; s: 3),
  ( cc: [ '|' ]; s: 23),
  ( cc: [ '}' ]; s: 4),
{ 1: }
  ( cc: [ #1..#8,#11..#31,'#','%','.','?','@','\','`',
            '~'..#255 ]; s: 33),
  ( cc: [ #9,' ' ]; s: 32),
  ( cc: [ #10 ]; s: 31),
  ( cc: [ '!' ]; s: 21),
  ( cc: [ '"' ]; s: 29),
  ( cc: [ '$' ]; s: 16),
  ( cc: [ '&' ]; s: 22),
  ( cc: [ '''' ]; s: 30),
  ( cc: [ '(' ]; s: 5),
  ( cc: [ ')' ]; s: 6),
  ( cc: [ '*' ]; s: 12),
  ( cc: [ '+' ]; s: 13),
  ( cc: [ ',' ]; s: 10),
  ( cc: [ '-' ]; s: 14),
  ( cc: [ '/' ]; s: 2),
  ( cc: [ '0','2'..'9' ]; s: 27),
  ( cc: [ '1' ]; s: 17),
  ( cc: [ ':' ]; s: 9),
  ( cc: [ ';' ]; s: 11),
  ( cc: [ '<' ]; s: 18),
  ( cc: [ '=' ]; s: 19),
  ( cc: [ '>' ]; s: 20),
  ( cc: [ 'A'..'D','F'..'M','O'..'T','V'..'Z','_','a'..'d',
            'f'..'m','o'..'t','v'..'z' ]; s: 28),
  ( cc: [ 'E','e' ]; s: 26),
  ( cc: [ 'N','n' ]; s: 25),
  ( cc: [ 'U','u' ]; s: 24),
  ( cc: [ '[' ]; s: 7),
  ( cc: [ ']' ]; s: 8),
  ( cc: [ '^' ]; s: 15),
  ( cc: [ '{' ]; s: 3),
  ( cc: [ '|' ]; s: 23),
  ( cc: [ '}' ]; s: 4),
{ 2: }
  ( cc: [ '*' ]; s: 34),
  ( cc: [ '/' ]; s: 35),
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
  ( cc: [ '0'..'9' ]; s: 36),
{ 14: }
  ( cc: [ '0'..'9' ]; s: 36),
{ 15: }
{ 16: }
  ( cc: [ '0'..'9' ]; s: 37),
{ 17: }
  ( cc: [ '.' ]; s: 39),
  ( cc: [ '0'..'9' ]; s: 36),
  ( cc: [ 'E','e' ]; s: 40),
  ( cc: [ 'S','s' ]; s: 38),
{ 18: }
  ( cc: [ '=' ]; s: 41),
  ( cc: [ '>' ]; s: 42),
{ 19: }
{ 20: }
  ( cc: [ '=' ]; s: 43),
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 44),
{ 25: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 44),
{ 26: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 44),
{ 27: }
  ( cc: [ '.' ]; s: 39),
  ( cc: [ '0'..'9' ]; s: 36),
  ( cc: [ 'E','e' ]; s: 40),
{ 28: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 44),
{ 29: }
  ( cc: [ #1..#9,#11..'!','#'..#255 ]; s: 45),
  ( cc: [ '"' ]; s: 46),
{ 30: }
  ( cc: [ #1..#9,#11..'&','('..#255 ]; s: 47),
  ( cc: [ '''' ]; s: 46),
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( cc: [ #1..#9,#11..#255 ]; s: 35),
  ( cc: [ #10 ]; s: 48),
{ 36: }
  ( cc: [ '.' ]; s: 39),
  ( cc: [ '0'..'9' ]; s: 36),
  ( cc: [ 'E','e' ]; s: 40),
{ 37: }
  ( cc: [ '0'..'9' ]; s: 37),
{ 38: }
  ( cc: [ 'I','i' ]; s: 49),
{ 39: }
  ( cc: [ '0'..'9' ]; s: 50),
{ 40: }
  ( cc: [ '+','-' ]; s: 51),
  ( cc: [ '0'..'9' ]; s: 52),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z' ]; s: 44),
{ 45: }
  ( cc: [ #1..#9,#11..'!','#'..#255 ]; s: 45),
  ( cc: [ '"' ]; s: 46),
{ 46: }
{ 47: }
  ( cc: [ #1..#9,#11..'&','('..#255 ]; s: 47),
  ( cc: [ '''' ]; s: 46),
{ 48: }
  ( cc: [ #13 ]; s: 53),
{ 49: }
{ 50: }
  ( cc: [ '0'..'9' ]; s: 50),
  ( cc: [ 'E','e' ]; s: 40),
{ 51: }
  ( cc: [ '0'..'9' ]; s: 52),
{ 52: }
  ( cc: [ '0'..'9' ]; s: 52)
{ 53: }
);

yykl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
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
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 34,
{ 19: } 36,
{ 20: } 38,
{ 21: } 40,
{ 22: } 42,
{ 23: } 44,
{ 24: } 46,
{ 25: } 49,
{ 26: } 52,
{ 27: } 55,
{ 28: } 58,
{ 29: } 60,
{ 30: } 61,
{ 31: } 62,
{ 32: } 63,
{ 33: } 65,
{ 34: } 66,
{ 35: } 67,
{ 36: } 67,
{ 37: } 69,
{ 38: } 70,
{ 39: } 70,
{ 40: } 70,
{ 41: } 70,
{ 42: } 71,
{ 43: } 72,
{ 44: } 73,
{ 45: } 74,
{ 46: } 74,
{ 47: } 75,
{ 48: } 75,
{ 49: } 76,
{ 50: } 77,
{ 51: } 78,
{ 52: } 78,
{ 53: } 79
);

yykh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 12,
{ 8: } 14,
{ 9: } 16,
{ 10: } 18,
{ 11: } 20,
{ 12: } 22,
{ 13: } 24,
{ 14: } 26,
{ 15: } 28,
{ 16: } 30,
{ 17: } 33,
{ 18: } 35,
{ 19: } 37,
{ 20: } 39,
{ 21: } 41,
{ 22: } 43,
{ 23: } 45,
{ 24: } 48,
{ 25: } 51,
{ 26: } 54,
{ 27: } 57,
{ 28: } 59,
{ 29: } 60,
{ 30: } 61,
{ 31: } 62,
{ 32: } 64,
{ 33: } 65,
{ 34: } 66,
{ 35: } 66,
{ 36: } 68,
{ 37: } 69,
{ 38: } 69,
{ 39: } 69,
{ 40: } 69,
{ 41: } 70,
{ 42: } 71,
{ 43: } 72,
{ 44: } 73,
{ 45: } 73,
{ 46: } 74,
{ 47: } 74,
{ 48: } 75,
{ 49: } 76,
{ 50: } 77,
{ 51: } 77,
{ 52: } 78,
{ 53: } 79
);

yyml : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 1,
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
{ 13: } 23,
{ 14: } 25,
{ 15: } 27,
{ 16: } 29,
{ 17: } 31,
{ 18: } 34,
{ 19: } 36,
{ 20: } 38,
{ 21: } 40,
{ 22: } 42,
{ 23: } 44,
{ 24: } 46,
{ 25: } 49,
{ 26: } 52,
{ 27: } 55,
{ 28: } 58,
{ 29: } 60,
{ 30: } 61,
{ 31: } 62,
{ 32: } 63,
{ 33: } 65,
{ 34: } 66,
{ 35: } 67,
{ 36: } 67,
{ 37: } 69,
{ 38: } 70,
{ 39: } 70,
{ 40: } 70,
{ 41: } 70,
{ 42: } 71,
{ 43: } 72,
{ 44: } 73,
{ 45: } 74,
{ 46: } 74,
{ 47: } 75,
{ 48: } 75,
{ 49: } 76,
{ 50: } 77,
{ 51: } 78,
{ 52: } 78,
{ 53: } 79
);

yymh : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 12,
{ 8: } 14,
{ 9: } 16,
{ 10: } 18,
{ 11: } 20,
{ 12: } 22,
{ 13: } 24,
{ 14: } 26,
{ 15: } 28,
{ 16: } 30,
{ 17: } 33,
{ 18: } 35,
{ 19: } 37,
{ 20: } 39,
{ 21: } 41,
{ 22: } 43,
{ 23: } 45,
{ 24: } 48,
{ 25: } 51,
{ 26: } 54,
{ 27: } 57,
{ 28: } 59,
{ 29: } 60,
{ 30: } 61,
{ 31: } 62,
{ 32: } 64,
{ 33: } 65,
{ 34: } 66,
{ 35: } 66,
{ 36: } 68,
{ 37: } 69,
{ 38: } 69,
{ 39: } 69,
{ 40: } 69,
{ 41: } 70,
{ 42: } 71,
{ 43: } 72,
{ 44: } 73,
{ 45: } 73,
{ 46: } 74,
{ 47: } 74,
{ 48: } 75,
{ 49: } 76,
{ 50: } 77,
{ 51: } 77,
{ 52: } 78,
{ 53: } 79
);

yytl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 33,
{ 2: } 65,
{ 3: } 67,
{ 4: } 67,
{ 5: } 67,
{ 6: } 67,
{ 7: } 67,
{ 8: } 67,
{ 9: } 67,
{ 10: } 67,
{ 11: } 67,
{ 12: } 67,
{ 13: } 67,
{ 14: } 68,
{ 15: } 69,
{ 16: } 69,
{ 17: } 70,
{ 18: } 74,
{ 19: } 76,
{ 20: } 76,
{ 21: } 77,
{ 22: } 77,
{ 23: } 77,
{ 24: } 77,
{ 25: } 78,
{ 26: } 79,
{ 27: } 80,
{ 28: } 83,
{ 29: } 84,
{ 30: } 86,
{ 31: } 88,
{ 32: } 88,
{ 33: } 88,
{ 34: } 88,
{ 35: } 88,
{ 36: } 90,
{ 37: } 93,
{ 38: } 94,
{ 39: } 95,
{ 40: } 96,
{ 41: } 98,
{ 42: } 98,
{ 43: } 98,
{ 44: } 98,
{ 45: } 99,
{ 46: } 101,
{ 47: } 101,
{ 48: } 103,
{ 49: } 104,
{ 50: } 104,
{ 51: } 106,
{ 52: } 107,
{ 53: } 108
);

yyth : array [0..yynstates-1] of Integer = (
{ 0: } 32,
{ 1: } 64,
{ 2: } 66,
{ 3: } 66,
{ 4: } 66,
{ 5: } 66,
{ 6: } 66,
{ 7: } 66,
{ 8: } 66,
{ 9: } 66,
{ 10: } 66,
{ 11: } 66,
{ 12: } 66,
{ 13: } 67,
{ 14: } 68,
{ 15: } 68,
{ 16: } 69,
{ 17: } 73,
{ 18: } 75,
{ 19: } 75,
{ 20: } 76,
{ 21: } 76,
{ 22: } 76,
{ 23: } 76,
{ 24: } 77,
{ 25: } 78,
{ 26: } 79,
{ 27: } 82,
{ 28: } 83,
{ 29: } 85,
{ 30: } 87,
{ 31: } 87,
{ 32: } 87,
{ 33: } 87,
{ 34: } 87,
{ 35: } 89,
{ 36: } 92,
{ 37: } 93,
{ 38: } 94,
{ 39: } 95,
{ 40: } 97,
{ 41: } 97,
{ 42: } 97,
{ 43: } 97,
{ 44: } 98,
{ 45: } 100,
{ 46: } 100,
{ 47: } 102,
{ 48: } 103,
{ 49: } 103,
{ 50: } 105,
{ 51: } 106,
{ 52: } 107,
{ 53: } 107
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



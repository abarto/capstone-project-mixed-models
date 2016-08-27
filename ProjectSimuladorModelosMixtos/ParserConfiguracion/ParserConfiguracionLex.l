%{

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

%}

%%

"/*" begin
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

"//".*\n\r? ;

"{" return(T_LLAVE_APERTURA);
"}" return(T_LLAVE_CLAUSURA);
"(" return(T_PARENTESIS_APERTURA);
")" return(T_PARENTESIS_CLAUSURA);
"[" return(T_CORCHETE_APERTURA);
"]" return(T_CORCHETE_CLAUSURA);
":" return(T_DOS_PUNTOS);
"," return(T_COMA);
";" return(T_PUNTO_Y_COMA);
"*" return(T_ASTERISCO);
"+" return(T_MAS);
"-" return(T_MENOS);
"/" return(T_BARRA_DERECHA);
"^" return(T_CIRCUNFLEJO);
"$" return(T_PESOS);

1[Ss][Ii] return(T_FUNCION_1SI);
"<"       return(T_OPERADOR_MENOR_QUE);
"<="      return(T_OPERADOR_MENOR_O_IGUAL_QUE);
"<>"      return(T_OPERADOR_DISTINTO);
"="       return(T_OPERADOR_IGUAL);
">"       return(T_OPERADOR_MAYOR_QUE);
">="      return(T_OPERADOR_MAYOR_O_IGUAL_QUE);

"!"       return(T_OPERADOR_NEGACION);
"&"       return(T_OPERADOR_CONJUNCION);
"|"       return(T_OPERADOR_DISYUNCION);

[Uu]       return(T_SIMBOLO_VARIABLE_UNIFORME);
[Nn]       return(T_SIMBOLO_VARIABLE_NORMAL);
[Ee]       return(T_SIMBOLO_VARIABLE_EXPONENCIAL);

[+-]?[0-9]+                              begin
                                           yylval.yyInteger := StrToInt(yytext);
                                           return(T_ENTERO);
                                         end;

[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)? begin
                                           yylval.yyReal := StrToFloat(yytext);
                                           return(T_REAL);
                                         end;

[a-zA-Z_][a-zA-Z_0-9]*                   begin
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

(\"[^\"\n]*\")|(\'[^\'\n]*\')        begin
                                       yylval.yyShortString := Copy(yytext, 2, Length(yytext) - 2);
                                       return(T_LITERAL_CADENAS);
                                     end;

\$[0-9]+                                 begin
                                           yylval.yyInteger := StrToInt(Copy(yytext, 2, Length(yytext) - 1));
                                           return(T_SIMBOLO_VARIABLE);
                                         end;

[ \t\n]                                ;
.                                      ;

%%
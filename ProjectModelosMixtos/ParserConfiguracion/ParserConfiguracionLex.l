%{

var
  {** Caracter utilizado como buffer para la eliminacion de los comentarios
      de la fuente de configuracion. }
  C: Char;
  {** Token del ultimo lexeme escaneado. }
  Token: Integer;

const
  {** Cantidad de palabras clave. }
  CantidadPalabrasClave = 74;
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
    'archivo_salida' );
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
    T_ARCHIVO_SALIDA );

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

(\"[^\"\'\n]*\")|(\'[^\"\'\n]*\')        begin
                                           yylval.yyShortString := Copy(yytext, 2, Length(yytext) - 2);
                                           return(T_LITERAL_CADENAS);
                                         end;

[ \t\n]                                ;
.                                      ;

%%
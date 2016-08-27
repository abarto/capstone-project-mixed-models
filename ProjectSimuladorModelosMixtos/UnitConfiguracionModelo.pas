{**
@abstract(Configuracion del modelo mixto.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 10, 2005)
Este modulo contiene la clase que almacena la informacion respecto del modelo
mixto del cual se generar datos simulados.
}

unit UnitConfiguracionModelo;

interface

uses
  Dialogs, Classes, Contnrs, SysUtils,
  UnitColumnaDatos, UnitConfiguracionDatos, UnitEfecto, UnitEstructura,
  UnitGrupoEfectos, UnitOpcion;

type
  {** Configuracion respecto de un modelo mixto. Esta informacion sera utilizada
      en conjunto con otros componentes de configuracion para llevar a cabo
      analisis de varianza o simulacion de observaciones.
      @abstract(Configuracion de modelo mixto.) }
  TConfiguracionModelo = class
  private
    {** Configuracion sobre la estructura de observaciones a generar. Estos
        datos incluyen informacion sobre las variables que intervienen. }
    FDatos: TConfiguracionDatos;
    {** Etiqueta. }
    FEtiqueta: String;
    {** Informacion sobre efectos aleatorios y opciones de configuracion
        relacionadas. }
    FEfectosAleatorios: TGrupoEfectos;
    {** Informacion sobre efectos fijos y opciones de configuracion
        relacionadas. }
    FEfectosFijos: TGrupoEfectos;
    {** Opciones de configuracion sobre el error del modelo. }
    FError: TListaOpciones;
    {** Lista con informacion sobre grupos de efectos aleatorios y opciones de
        configuracion relacionadas. }
    FGruposEfectosAleatorios: TListaGruposEfectos;
    {** Opciones del modelo. }
    FOpciones: TListaOpciones;
    {** Indica si el modelo posee efectos aleatorios (sin agrupar). }
    FTieneEfectosAleatorios: Boolean;
    {** Indica si el modelo posee efectos aleatorios definidos en algun grupo. }
    FTieneEfectosAleatoriosGrupos: Boolean;
    {** Indica si el modelo posee efectos fijos. }
    FTieneEfectosFijos: Boolean;
    {** Variables de clasificacion del modelo. }
    FVariablesClasificacion: TStrings;
    {** Variable dependiente del modelo. }
    FVariableDependiente: String;
    {** Genera un orden total de las variables definidas en el modelo. Las
        covariables son menores que las variables de clasificacion. Las
        covariables se ordenan de acuerdo a su declaracion en la fuente de
        datos. Las variables de clasificacion se ordenan de acuerdo a la lista
        respectiva y en caso de no existir tal lista, de acuerdo al orden de
        declaracion en la fuente de datos.
        @param(U Primera variable a comparar.)
        @param(V Segunda variable a comparar.)
        @returns(>0 si U es menor que V, 0 si U = V y <0 si U > V.) }
    function CompararVariables(const U, V: String): Integer;
    {** Valida la configuracion de la fuente de datos del modelo (Ver
        @link(UnitConfiguracionDatos.TConfiguracionDatos.Validar)).
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si los datos son validos o @false en caso
        contrario.) }
    function ValidarDatos(var Bitacora: TStrings): Boolean;
    {** Valida un efecto en funcion de la configuracion del modelo.
        @param(Efecto Efecto que se desea validar.)
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @param(VerificarRepeticionesVariables Indica si se deben buscar
        cierto tipo de variables repetidas en el efecto. Es util, bajo ciertas
        circunstancias, permitir que ciertos efectos sean validos a pesar de
        tener variables repetidas.)
        @returns(Devuelve @true si el efecto es valido o @false en caso
        contrario.) }
    function ValidarEfecto(const Efecto: TEfecto; var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean;
    {** Valida una estructura en funcion de la configuracion del modelo.
        @param(Estructura Estructura que se desea validar.)
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la estructura es valida o @false en caso
        contrario.) }
    function ValidarEstructura(const Estructura: TEstructura; var Bitacora: TStrings): Boolean;
    {** Valida los efectos aleatorios del modelo. Lo hace tanto para los efectos
        aleatorios no agrupados como para los contenidos en algun grupo. Tambien
        se encarga de llamar al metodo de validacion de las opciones del error
        del modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la configuracion de efectos aleatorios es
        valida en si y respecto del resto del modelo, o @false en caso
        contrario.) }
    function ValidarEfectosAleatorios(var Bitacora: TStrings): Boolean;
    {** Valida los efectos fijos del modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la configuracion de efectos fijos es
        valida en si y respecto del resto del modelo, o @false en caso
        contrario.) }
    function ValidarEfectosFijos(var Bitacora: TStrings): Boolean;
    {** Valida las opciones de error del modelo. Este es un metodo auxiliar de
        @link(ValidarEfectosAleatorios) dado que el error es considerado un
        efecto aleatorio.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve true si las opciones del error son validas o @false
        en caso contrario.) }
    function ValidarError(var Bitacora: TStrings): Boolean;
    {** Valida la lista de grupos de efectos aleatorios. Es un metodo auxiliar
        de @link(ValidarEfectosAleatorios).
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la declaracion de grupos de efectos
        aleatorios es valida o @false en caso contrario.) }
    function ValidarGruposEfectosAleatorios(var Bitacora: TStrings): Boolean;
    {** Valida las opciones del modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si las opciones son validas o @false en caso
        contrario.) }
    function ValidarOpciones(var Bitacora: TStrings): Boolean;
    {** Valida la lista de declaracion de variables de clasificacion en funcion
        de lo declarado en la fuente de datos y en el modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la lista de variables de clasificacion es
        valida o @false en caso contrario.) }
    function ValidarVariablesClasificacion(var Bitacora: TStrings): Boolean;
    {** Valida la declaracion de variable dependiente en funcion de lo declarado
        en la fuente de datos y en el modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la declaracion de variable dependiente es
        valida o @false en caso contrario.) }
    function ValidarVariableDependiente(var Bitacora: TStrings): Boolean;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AEtiqueta Etiqueta descriptiva.)
        @param(AVariablesClasficacion Lista de variables de clasificacion.)
        @param(AVariableDependiente Variable dependiente.)
        @param(AEfectosFijos Efectos fijos.)
        @param(AEfectosAleatorios Efectos aleatorios no agrupados.)
        @param(AGruposEfectosAleatorios Lista de grupos de efectos aleatorios.)
        @param(AError Lista de opciones del error.)
        @param(AOpciones Lista de opciones del modelo.) }
    constructor Create(AEtiqueta: String; AVariablesClasificacion: TStrings; AVariableDependiente: String; AEfectosFijos: TGrupoEfectos; AEfectosAleatorios: TGrupoEfectos; AGruposEfectosAleatorios: TListaGruposEfectos; AError: TListaOpciones; AOpciones: TListaOpciones);  overload;
    {** Constructor.
        @param(ADatos Configuracion de la fuente de datos.)
        @param(AEtiqueta Etiqueta descriptiva.)
        @param(AVariablesClasficacion Lista de variables de clasificacion.)
        @param(AVariableDependiente Variable dependiente.)
        @param(AEfectosFijos Efectos fijos.)
        @param(AEfectosAleatorios Efectos aleatorios no agrupados.)
        @param(AGruposEfectosAleatorios Lista de grupos de efectos aleatorios.)
        @param(AError Lista de opciones del error.) }
    constructor Create(ADatos: TConfiguracionDatos; AEtiqueta: String; AVariablesClasificacion: TStrings; AVariableDependiente: String; AEfectosFijos: TGrupoEfectos; AEfectosAleatorios: TGrupoEfectos; AGruposEfectosAleatorios: TListaGruposEfectos; AError: TListaOpciones; AOpciones: TListaOpciones);  overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Valida la configuracion del modelo mixto en si y en relacion a la fuente
        de datos. Es posible que el modelo sea valido pero que aun asi se
        produzcan errores, estos deben interpretarse como advertencias.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si el modelo es valido o @false en caso
        contrario.) }
    function Validar(var Bitacora: TStrings): Boolean;
    {** Inicializa el modelo para su validacion y utilizacion. Es imperativo
        inicializar el modelo previo a su validacion. Esta tarea no se realiza
        durante la construccion de la instancia dado que es potencialmente
        costoza dependiente de la complejidad del modelo.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de inicializacion.) }
    function Inicializar(var Bitacora: TStrings): Boolean;

    {** Configuracion de la fuente de datos. }
    property Datos: TConfiguracionDatos read FDatos write FDatos;
    {** Etiqueta descriptiva. }
    property Etiqueta: String read FEtiqueta write FEtiqueta;
    {** Efectos aleatorios no agrupados y opciones relacionadas. }
    property EfectosAleatorios: TGrupoEfectos read FEfectosAleatorios write FEfectosAleatorios;
    {** Efectos fijos y opciones relacionadas. }
    property EfectosFijos: TGrupoEfectos read FEfectosFijos write FEfectosFijos;
    {** Opciones del error del modelo. }
    property Error: TListaOpciones read FError write FError;
    {** Lista de grupos de efectos aleatorios y opciones relacionadas. }
    property GruposEfectosAleatorios: TListaGruposEfectos read FGruposEfectosAleatorios write FGruposEfectosAleatorios;
    {** Opciones del modelo. }
    property Opciones: TListaOpciones read FOpciones write FOpciones;
    {** Indica si el modelo tiene efectos aleatorios sin agrupar. }
    property TieneEfectosAleatorios: Boolean read FTieneEfectosAleatorios;
    {** Indica si el modelo tiene efectos aleatorios en grupo. }
    property TieneEfectosAleatoriosGrupos: Boolean read FTieneEfectosAleatoriosGrupos;
    {** Indica si el modelo tiene efectos aleatorios en grupo. }
    property TieneEfectosFijos: Boolean read FTieneEfectosFijos;
    {** Lista de variables de clasificacion. }
    property VariablesClasificacion: TStrings read FVariablesClasificacion write FVariablesClasificacion;
    {** Variable dependiente. }
    property VariableDependiente: String read FVariableDependiente write FVariableDependiente;
  end;

implementation

uses
  UnitParametros;

// -----------------------------------------------------------------------------
// TConfiguracionModelo
// -----------------------------------------------------------------------------

function TConfiguracionModelo.ValidarDatos(var Bitacora: TStrings): Boolean;
var
  I: Integer;
begin { TConfiguracionModelo.ValidarDatos }
  Assert(Assigned(FDatos), 'TConfiguracionModelo.ValidarDatos: Assigned(FDatos)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarDatos: Assigned(Bitacora)');

  Result := true;

  // Verificar que la variable dependiente no haya sido especificada como una
  // columna a generar mediante una expresion
  I := FDatos.Columnas.IndexOf(FVariableDependiente);
  if (I <> -1) then
  begin
    Bitacora.Add('La variable dependiente fue definida como una columna a generar mediante una expresion (' + FDatos.Columnas [I].Texto + ').');
    Result := False;
  end;
end { TConfiguracionModelo.ValidarDatos };

function TConfiguracionModelo.ValidarEfecto(const Efecto: TEfecto; var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean): Boolean;
var
  Listo: Boolean;
  I: Integer;
  ListaEfectosAnidados: TListaEfectosAnidados;
  Strings: TStrings;
begin { TConfiguracionModelo.ValidarEfecto }
  Assert(Assigned(Efecto), 'TConfiguracionModelo.ValidarEfecto: Assigned(Efecto)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarEfecto: Assigned(Bitacora)');

  // Validar el efecto
  Result := Efecto.Validar(Bitacora, VerificarRepeticionesVariables);

  // Validar el efecto de acuerdo al modelo
  if Efecto is TEfectoPrincipal then
  begin
    // Variable de efecto principal no corresponde a ninguna definicion de columna
    if FDatos.Columnas.IndexOf((Efecto as TEfectoPrincipal).Variable) = -1 then
    begin
      Bitacora.Add('Variable de efecto principal no corresponde a ninguna definici�n de columna');
      Result := false;
    end;
  end
  else if Efecto is TCruzamiento then
  begin
    // Variable en cruzamiento no corresponde a ninguna definicion de columna
    Strings := Efecto.ListaVariables;
    I := 0;
    Listo := false;
    while (I < Strings.Count) and (not Listo) do
    begin
      if FDatos.Columnas.IndexOf(Strings [I]) = -1 then
      begin
        Bitacora.Add('Variable en cruzamiento no corresponde a ninguna definici�n de columna: ' + Strings [I]);
        Result := false;
        Listo := true;
      end;

      Inc(I);
    end;

    // Variable de clasificacion en cabecera duplicada en cruzamiento
    if FVariablesClasificacion.IndexOf((Efecto as TCruzamiento).Cabecera.Variable) <> - 1 then
    begin
      if (Efecto as TCruzamiento).Resto.VariableOcurre((Efecto as TCruzamiento).Cabecera.Variable) then
      begin
        Bitacora.Add('Variable de clasificacion en cabecera duplicada en cruzamiento: ' + (Efecto as TCruzamiento).Cabecera.Variable);
        Result := false;
      end;
    end;
  end
  else if Efecto is TAnidamiento then
  begin
    // Variable en anidamiento no corresponde a ninguna definicion de columna
    Strings := Efecto.ListaVariables;
    I := 0;
    Listo := false;
    while (I < Strings.Count) and (not Listo) do
    begin
      if FDatos.Columnas.IndexOf(Strings [I]) = -1 then
      begin
        Bitacora.Add('Variable en anidamiento no corresponde a ninguna definici�n de columna: ' + Strings [I]);
        Result := false;
        Listo := true;
      end;

      Inc(I);
    end;

    ListaEfectosAnidados := (Efecto as TAnidamiento).EfectosAnidados;

    // Variable en lista de efectos anidados no corresponde a ninguna variable de clasificacion
    I := 0;
    Listo := false;
    while (I < ListaEfectosAnidados.Count) and (not Listo) do
    begin
      if FVariablesClasificacion.IndexOf(ListaEfectosAnidados [I].Variable) = -1 then
      begin
        Bitacora.Add('Variable en lista de efectos anidados no corresponde a ninguna variable de clasificaci�n: ' + ListaEfectosAnidados [I].Variable);
        Result := false;
        Listo := true;
      end;

      Inc(I);
    end;
  end;
end { TConfiguracionModelo.ValidarEfecto };

function TConfiguracionModelo.ValidarEstructura(const Estructura: TEstructura; var Bitacora: TStrings): Boolean;
begin { TConfiguracionModelo.ValidarEstructura }
  Assert(Assigned(Estructura), 'TConfiguracionModelo.ValidarEstructura: Assigned(Estructura)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarEstructura: Assigned(Bitacora)');

  Result := Estructura.Validar(Bitacora);
end { TConfiguracionModelo.ValidarEstructura };

function TConfiguracionModelo.ValidarEfectosAleatorios(var Bitacora: TStrings): Boolean;
var
  Listo: Boolean;
  Opcion: TOpcion;
  I, J: Integer;
  Strings: TStrings;
begin { TConfiguracionModelo.ValidarEfectosAleatorios }
  Assert(Assigned(FEfectosAleatorios), 'TConfiguracionModelo.ValidarEfectosAleatorios: Assigned(FEfectosAleatorios)');
  Assert(Assigned(FGruposEfectosAleatorios), 'TConfiguracionModelo.ValidarEfectosAleatorios: Assigned(FGruposEfectosAleatorios)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarEfectosAleatorios: Assigned(Bitacora)');

  Result := true;

  Strings := TStringList.Create;

  // No se definio ningun efecto aleatorio
  if (FGruposEfectosAleatorios.Count = 0) and (FEfectosAleatorios.Efectos.Count = 0) then
  begin
    Bitacora.Add('No se defini� ning�n efecto aleatorio.');
    (* Result := false; *)
  end;

  // Definicion de efecto aleatorio no agrupado invalida o inconsistente
  I := 0;
  Listo := false;
  while (I < FEfectosAleatorios.Efectos.Count) and (not Listo) do
  begin
    Strings.Clear;
    if not ValidarEfecto(FEfectosAleatorios.Efectos [I], Strings) then
    begin
      Bitacora.Add('Definici�n de efecto aleatorio no agrupado inv�lida o inconsistente: #' + IntToStr(I) + ' = ' + FEfectosAleatorios.Efectos [I].Texto + ' (' + Strings.CommaText + ').');
      Listo := true;
      Result := false;
    end;

    Inc(I);
  end;

  // Definicion de efecto aleatorio duplicada
  I := 0;
  Listo := false;
  while (I < FEfectosAleatorios.Efectos.Count - 1) and (not Listo) do
  begin
    J := I + 1;
    while (J < FEfectosAleatorios.Efectos.Count) and (not Listo) do
    begin
      if FEfectosAleatorios.Efectos [I].IgualA(FEfectosAleatorios.Efectos [J]) then
      begin
        Bitacora.Add('Definici�n de efecto aleatorio duplicada o inconsistente: #' + IntToStr(I) + ' = ' + FEfectosFijos.Efectos [I].Texto + ', #' + IntToStr(J) + ' = ' + FEfectosFijos.Efectos [J].Texto + '.');
        Listo := true;
        Result := false;
      end;

      Inc(J);
    end;

    Inc(I);
  end;

  // Validar opciones de efectos aleatorios

  I := 0;
  Listo := false;
  while (I < FEfectosAleatorios.Opciones.Count) and (not Listo) do
  begin
    { TODO: Completar validacion de opciones de efectos aleatorios globales. }
    Opcion := FEfectosAleatorios.Opciones [I];

    if Opcion.Nombre = 'incluir_intercepto' then
    begin
      // Se especifico intercepto en grupo de efectos aleatorios y en opcion
      if FEfectosAleatorios.Efectos.IncluyeIntercepto then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: Se indic� opcion de inclusi�n intercepto y especific� su inclusi�n en la definici�n de efectos.');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'criterio_agrupamiento_unidad_experimental' then
    begin
      // Efecto invalido en criterio de agrupamiento
      Strings.Clear;
      if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      // Intercepto como criterio de agrupamiento
      if (Opcion as TOpcionParametroTEfecto).Parametro is TIntercepto then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;

      // Variable dependiente ocurre en criterio de agrupamiento
      if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: La variable dependiente ocurre en criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;
     end
    else if Opcion.Nombre = 'unidad_experimental' then
    begin
      // Efecto invalido en unidad experimental
      Strings.Clear;
      if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      // Intercepto como unidad experimental
      if (Opcion as TOpcionParametroTEfecto).Parametro is TIntercepto then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;

      // Variable dependiente ocurre en unidad experimental
      if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: La variable dependiente ocurre en unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'estructura' then
    begin
      Strings.Clear;
      if not ValidarEstructura((Opcion as TOpcionParametroTEstructura).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: estructura ' + (Opcion as TOpcionParametroTEstructura).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'parametros' then
    begin
      // No es necesario validar los parametros
    end;

    Inc(I);
  end;

  FreeAndNil(Strings);

  // Validar grupos de efectos aleatorios y el error
  if Result then
    Result := ValidarGruposEfectosAleatorios(Bitacora) and ValidarError(Bitacora);
end { TConfiguracionModelo.ValidarEfectosAleatorios };

function TConfiguracionModelo.ValidarEfectosFijos(var Bitacora: TStrings): Boolean;
var
  C, Listo: Boolean;
  I, J, K, L: Integer;
  Strings: TStrings;
begin { TConfiguracionModelo.ValidarEfectosFijos }
  Assert(Assigned(FEfectosFijos), 'TConfiguracionModelo.ValidarEfectosFijos: Assigned(FEfectosFijos)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarEfectosFijos: Assigned(Bitacora)');

  Result := true;

  Strings := TStringList.Create;

  // Lista de definicion de efectos fijos vacia
  if FEfectosFijos.Efectos.Count > 0 then
  begin
    // Definicion de efecto fijo invalida o inconsistente
    I := 0;
    Listo := false;
    while (I < FEfectosFijos.Efectos.Count) and (not Listo) do
    begin
      Strings.Clear;
      if not ValidarEfecto(FEfectosFijos.Efectos [I], Strings) then
      begin
        Bitacora.Add('Definici�n de efecto fijo inv�lida o inconsistente: #' + IntToStr(I) + ' = ' + FEfectosFijos.Efectos [I].Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      Inc(I);
    end;

    // Definicion de efecto fijo duplicada
    I := 0;
    Listo := false;
    while (I < FEfectosFijos.Efectos.Count - 1) and (not Listo) do
    begin
      J := I + 1;
      while (J < FEfectosFijos.Efectos.Count) and (not Listo) do
      begin
        if FEfectosFijos.Efectos [I].IgualA(FEfectosFijos.Efectos [J]) then
        begin
          Bitacora.Add('Definici�n de efecto fijo duplicada o inconsistente: #' + IntToStr(I) + ' = ' + FEfectosFijos.Efectos [I].Texto + ', #' + IntToStr(J) + ' = ' + FEfectosFijos.Efectos [J].Texto + '.');
          Listo := true;
          Result := false;
        end;

        Inc(J);
      end;

      Inc(I);
    end;

    // Definicion de efecto fijo ocurre en definicion de efecto aleatorio
    I := 0;
    Listo := false;
    while (I < FEfectosFijos.Efectos.Count) and (not Listo) do
    begin
      // Los interceptos pueden repetirse
      C := false;
      if not (FEfectosFijos.Efectos [I] is TIntercepto) then
      begin
        // Variables numericas que no son de clasificaci�n son consideradas covariables y pueden duplicarse cono efectos principales
        if FEfectosFijos.Efectos [I] is TEfectoPrincipal then
        begin
          L := FDatos.Columnas.IndexOf((FEfectosFijos.Efectos [I] as TEfectoPrincipal).Variable);
          if not (FDatos.Columnas [L] is TColumnaDatosCategoricos) and (FVariablesClasificacion.IndexOf(FDatos.Columnas [L].Nombre) = -1) then
            C := true;
        end;
      end;

      // Definicion de efecto fijo ocurre en definicion de efecto en grupo de efecto de efectos aleatorios
      J := 0;
      while (J < FGruposEfectosAleatorios.Count) and (not Listo) do
      begin
        K := 0;
        while (K < FGruposEfectosAleatorios [J].Efectos.Count) and (not Listo) do
        begin
          // Las medias generales pueden repetirse
          if (not C) and FEfectosFijos.Efectos [I].IgualA(FGruposEfectosAleatorios [J].Efectos [K]) then
          begin
            Bitacora.Add('Definici�n de efecto fijo ocurre en definici�n de efecto en grupo de efecto de efectos aleatorios #' + IntToStr(J) + ': ' + FEfectosFijos.Efectos [I].Texto + '.');
            Listo := true;
            Result := false;
          end;

          Inc(K);
        end;
        Inc(J);
      end;

      // Definicion de efecto fijo ocurre en definicion de efectos aleatorios
      J := 0;
      Listo := false;
      while (J < FEfectosAleatorios.Efectos.Count) and (not Listo) do
      begin
        if (not C) and FEfectosFijos.Efectos [I].IgualA(FEfectosAleatorios.Efectos [J]) then
        begin
          Bitacora.Add('Definici�n de efecto fijo ocurre en definici�n de efectos aleatorios: ' + FEfectosFijos.Efectos [I].Texto + '.');
          Listo := true;
          Result := false;
        end;

        Inc(J);
      end;

      Inc(I);
    end;
  end
  else
  begin
    Bitacora.Add('Lista de definici�n de efectos fijos vac�a.');
    (* Result := false; *)
  end;

  // Validar opciones de efectos fijos

  { TODO: Completar validacion de opciones de efectos fijos. }

  I := 0;
  Listo := false;
  while (I < FEfectosFijos.Opciones.Count) and (not Listo) do
  begin
    if FEfectosFijos.Opciones [I].Nombre = 'incluir_intercepto' then
    begin
      // Se especifico intercepto en efectos fijos y en opcion
      if FEfectosFijos.Efectos.IncluyeIntercepto then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': Se indic� opcion de inclusi�n intercepto y especific� su inclusi�n en la definici�n de efectos.');
        Listo := true;
        Result := false;
      end;
    end;

    Inc(I);
  end;

  FreeAndNil(Strings);
end { TConfiguracionModelo.ValidarEfectosFijos };

function TConfiguracionModelo.ValidarError(var Bitacora: TStrings): Boolean;
var
  Listo, Listo1: Boolean;
  I, Indice, J: Integer;
  Opcion: TOpcion;
  ListaInformacionParametroIndice: TListaInformacionParametroIndice;
  Strings, Strings1: TStrings;
begin { TConfiguracionModelo.ValidarError }
  Assert(Assigned(FError), 'TConfiguracionModelo.ValidarError: Assigned(FError)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarError: Assigned(Bitacora)');

  Result := true;

  Strings := TStringList.Create;

  { TODO: Completar validacion de opciones de error. }

  I := 0;
  Listo := false;
  while (I < FError.Count) and (not Listo) do
  begin
    Opcion := FError.Opciones [I];

    if Opcion.Nombre = 'criterio_ordenamiento' then
    begin
      // Efecto invalido en criterio ordenamiento
      Strings.Clear;
      if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: criterio_ordenamiento ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      // La opcion criterio ordenamiento contiene una variable que no es de clasificacion
      Strings1 := (Opcion as TOpcionParametroTEfecto).Parametro.ListaVariables;
      J := 0;
      Indice := -1;
      Listo1 := false;
      while (J <= Strings1.Count - 1) and (not Listo1) do
      begin
        if VariablesClasificacion.IndexOf(Strings1 [J]) = -1 then
        begin
          Indice := J;
          Listo1 := true;
        end;

        Inc(J);
      end;

      if Listo1 then
      begin
        Bitacora.Add('El parametro de la opcion criterio_ordenamiento del error posee una variable que no es variable de clasificacion: ' + Strings1 [Indice] + '.');
        Listo := true;
        Result := false;
      end;

      // Variable dependiente ocurre en efecto
      if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: La variable dependiente ocurre en criterio_ordenamiento ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'criterio_agrupamiento_unidad_experimental' then
    begin
      // Efecto invalido en criterio de agrupamiento
      Strings.Clear;
      if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      // Variable dependiente ocurre en criterio de agrupamiento
      if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: La variable dependiente ocurre en criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'unidad_experimental' then
    begin
      // Efecto invalido en unidad experimental
      Strings.Clear;
      if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      // Variable dependiente ocurre en unidad experimental
      if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: La variable dependiente ocurre en unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'estructura' then
    begin
      Strings.Clear;
      if not ValidarEstructura((Opcion as TOpcionParametroTEstructura).Parametro, Strings) then
      begin
        Bitacora.Add('Opci�n inv�lida o inconsistente en error: estructura ' + (Opcion as TOpcionParametroTEstructura).Parametro.Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;
    end
    else if Opcion.Nombre = 'parametros' then
    begin
      ListaInformacionParametroIndice := (Opcion as TOpcionParametroTObject).Parametro as TListaInformacionParametroIndice;

      for J := 0 to ListaInformacionParametroIndice.Count - 1 do
      begin
        with ListaInformacionParametroIndice [J] do
        begin
          if TieneCotaInferior and TieneCotaSuperior then
          begin
            if CotaInferior > CotaSuperior then
            begin
              Bitacora.Add('Opci�n inv�lida o inconsistente en error: Cota inferior mayor que cota superior en el parametro #' + IntToStr(J) + '.');
              Listo := true;
              Result := false;
            end
            else if TienePaso then
            begin
              if Paso > Abs(CotaSuperior - CotaInferior) then
              begin
                Bitacora.Add('Opci�n inv�lida o inconsistente en error: Valor de paso de grilla de busqueda mayor que el rango permitido por las cotas en el parametro #' + IntToStr(J) + '.');
                Listo := true;
                Result := false;
              end;
            end;
          end;
        end;
      end;
    end;

    Inc(I);
  end;

  FreeAndNil(Strings);
end { TConfiguracionModelo.ValidarError };

function TConfiguracionModelo.ValidarGruposEfectosAleatorios(var Bitacora: TStrings): Boolean;
var
  I, J, K, L: Integer;
  C, Listo: Boolean;
  Opcion: TOpcion;
  Strings: TStrings;
begin { TConfiguracionModelo.ValidarGruposEfectosAleatorios }
  Assert(Assigned(FGruposEfectosAleatorios), 'TConfiguracionModelo.ValidarGruposEfectosAleatorios: Assigned(FGruposEfectosAleatorios)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarGruposEfectosAleatorios: Assigned(Bitacora)');

  Result := true;

  Strings := TStringList.Create;

  // Definicion de efecto aleatorio invalida o inconsistente en definicion de grupo de efectos aleatorios
  I := 0;
  Listo := false;
  while (I < FGruposEfectosAleatorios.Count) and (not Listo) do
  begin
    // Definicion de efecto aleatorio invalida o inconsistente definicion de de efectos aleatorios
    J := 0;
    while (J < FGruposEfectosAleatorios [I].Efectos.Count) and (not Listo) do
    begin
      Strings.Clear;
      if not ValidarEfecto(FGruposEfectosAleatorios [I].Efectos [J], Strings) then
      begin
        Bitacora.Add('Definicion de efecto aleatorio invalida o inconsistente en definicion de grupo de efectos aleatorios #' + IntToStr(I) + ': #' + IntToStr(J) + ' = ' + FGruposEfectosAleatorios [I].Efectos [J].Texto + ' (' + Strings.CommaText + ').');
        Listo := true;
        Result := false;
      end;

      Inc(J);
    end;

    // Definici�n duplicada en el mismo grupo
    J := 0;
    while (J < FGruposEfectosAleatorios [I].Efectos.Count - 1) and (not Listo) do
    begin
      K := J + 1;
      while (K < FGruposEfectosAleatorios [I].Efectos.Count) and (not Listo) do
      begin
        if FGruposEfectosAleatorios [I].Efectos [J].IgualA(FGruposEfectosAleatorios [I].Efectos [K]) then
        begin
          Bitacora.Add('Definicion de efecto aleatorio duplicada en definicion de grupo de efectos aleatorios #' + IntToStr(I) + ': #' + IntToStr(J) + ' = ' + FGruposEfectosAleatorios [I].Efectos [J].Texto + ', #' + IntToStr(K) + ' = ' + FGruposEfectosAleatorios [I].Efectos [K].Texto + '.');
          Listo := true;
          Result := false;
        end;

        Inc(K);
      end;

      Inc(J);
    end;

    Inc(I);
  end;

  // Definicion de efecto aleatorio duplicada en definicion de grupo de efectos aleatorios
  I := 0;
  Listo := false;
  while (I < FGruposEfectosAleatorios.Count - 1) and (not Listo) do
  begin
    // Definicion duplicada en otros grupos y en definicion de efectos aleatorios
    J := 0;
    while (J < FGruposEfectosAleatorios [I].Efectos.Count) and (not Listo) do
    begin
      // Los interceptos pueden repetirse
      C := false;
      if not (FGruposEfectosAleatorios [I].Efectos [J] is TIntercepto) then
      begin
        // Variables numericas que no son de clasificaci�n son consideradas covariables y pueden duplicarse cono efectos principales
        if FGruposEfectosAleatorios [I].Efectos [J] is TEfectoPrincipal then
        begin
          L := FDatos.Columnas.IndexOf((FGruposEfectosAleatorios [I].Efectos [J] as TEfectoPrincipal).Variable);
          if not (FDatos.Columnas [L] is TColumnaDatosCategoricos) and (FVariablesClasificacion.IndexOf(FDatos.Columnas [L].Nombre) = -1) then
            C := true;
        end;
      end;

      // Definicion duplicada en definicion de efectos aleatorios
      K := 0;
      while (K < FEfectosAleatorios.Efectos.Count) and (not Listo) do
      begin
        if (not C) and FGruposEfectosAleatorios [I].Efectos [J].IgualA(FEfectosAleatorios.Efectos [K]) then
        begin
          Bitacora.Add('Definicion de efecto aleatorio duplicada en definicion de grupos de efectos aleatorios #' + IntToStr(I) + ' y en definici�n de efectos aleatorios globales: ' + FGruposEfectosAleatorios [I].Efectos [J].Texto + '.');
          Listo := true;
          Result := false;
        end;

        Inc(K);
      end;

      // Definicion duplicada en otros grupos
      K := I + 1;
      while (K < FGruposEfectosAleatorios.Count) and (not Listo) do
      begin
        L := 0;
        while (L < FGruposEfectosAleatorios [K].Efectos.Count) and (not Listo) do
        begin
          if (not C) and FGruposEfectosAleatorios [I].Efectos [J].IgualA(FGruposEfectosAleatorios [K].Efectos [L]) then
          begin
            Bitacora.Add('Definicion de efecto aleatorio duplicada en definicion de grupos de efectos aleatorios #' + IntToStr(I) + ' y #' + IntToStr(K) + ': ' + FGruposEfectosAleatorios [I].Efectos [J].Texto + '.');
            Listo := true;
            Result := false;
          end;

          Inc(L);
        end;

        Inc(K);
      end;

      Inc(J);
    end;

    Inc(I);
  end;

  // Opciones en grupos de efectos aleatorios

  { TODO: Completar verificacion opciones de grupos efectos aleatorios. }

  I := 0;
  Listo := false;
  while (I < FGruposEfectosAleatorios.Count) and (not Listo) do
  begin
    J := 0;
    while (J < FGruposEfectosAleatorios [I].Opciones.Count) and (not Listo) do
    begin
      Opcion := FGruposEfectosAleatorios [I].Opciones [J];

      if Opcion.Nombre = 'incluir_intercepto' then
      begin
        if FGruposEfectosAleatorios [I].Efectos.IncluyeIntercepto then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': Se indic� opcion de inclusi�n intercepto y especific� su inclusi�n en la definici�n de efectos.');
          Listo := true;
          Result := false;
        end;
      end
      else if Opcion.Nombre = 'criterio_agrupamiento_unidad_experimental' then
      begin
        // Efecto invalido en criterio de agrupamiento
        Strings.Clear;
        if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
          Listo := true;
          Result := false;
        end;

        // Intercepto como criterio de agrupamiento
        if (Opcion as TOpcionParametroTEfecto).Parametro is TIntercepto then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
          Listo := true;
          Result := false;
        end;

        // Variable dependiente ocurre en criterio de agrupamiento
        if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': La variable dependiente ocurre en criterio_agrupamiento_unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
          Listo := true;
          Result := false;
        end;
      end
      else if Opcion.Nombre = 'unidad_experimental' then
      begin
        // Efecto invalido en unidad experimental
        Strings.Clear;
        if not ValidarEfecto((Opcion as TOpcionParametroTEfecto).Parametro, Strings) then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + ' (' + Strings.CommaText + ').');
          Listo := true;
          Result := false;
        end;

        // Intercepto como unidad experimental
        if (Opcion as TOpcionParametroTEfecto).Parametro is TIntercepto then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
          Listo := true;
          Result := false;
        end;

        // Variable dependiente ocurre en unidad_experimental
        if (Opcion as TOpcionParametroTEfecto).Parametro.VariableOcurre(FVariableDependiente) then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en grupo de efectos aleatorios #' + IntToStr(I) + ': La variable dependiente ocurre en unidad_experimental ' + (Opcion as TOpcionParametroTEfecto).Parametro.Texto + '.');
          Listo := true;
          Result := false;
        end;
      end
      else if Opcion.Nombre = 'estructura' then
      begin
        Strings.Clear;
        if not ValidarEstructura((Opcion as TOpcionParametroTEstructura).Parametro, Strings) then
        begin
          Bitacora.Add('Opci�n inv�lida o inconsistente en efectos aleatorios: estructura ' + (Opcion as TOpcionParametroTEstructura).Parametro.Texto + ' (' + Strings.CommaText + ').');
          Listo := true;
          Result := false;
        end;
      end
      else if Opcion.Nombre = 'parametros' then
      begin
        // No es necesario validar los parametros
      end;

      Inc(J);
    end;

    Inc(I);
  end;

  FreeAndNil(Strings);
end { TConfiguracionModelo.ValidarGruposEfectosAleatorios };

function TConfiguracionModelo.ValidarOpciones(var Bitacora: TStrings): Boolean;
begin { TConfiguracionModelo.ValidarOpciones }
  Assert(Assigned(FOpciones), 'TConfiguracionModelo.ValidarOpciones: Assigned(FOpciones)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarOpciones: Assigned(Bitacora)');

  Result := True;
end { TConfiguracionModelo.ValidarOpciones };

function TConfiguracionModelo.ValidarVariablesClasificacion(var Bitacora: TStrings): Boolean;
var
  Listo: Boolean;
  I, J: Integer;
begin { TConfiguracionModelo.ValidarVariablesClasificacion }
  Assert(Assigned(FVariablesClasificacion), 'TConfiguracionModelo.ValidarVariablesClasificacion: Assigned(FVariablesClasificacion)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarVariablesClasificacion: Assigned(Bitacora)');

  Result := true;

  // Lista de variables de clasificacion vacia
  if FVariablesClasificacion.Count > 0 then
  begin
    // Variable de clasificaci�n no corresponde a ninguna definici�n de columna
    // o corresponde a una variable numerica.
    I := 0;
    Listo := false;
    while (I < FVariablesClasificacion.Count) and (not Listo) do
    begin
      if FDatos.Columnas.IndexOf(FVariablesClasificacion [I]) = -1 then
      begin
        Bitacora.Add('Variable de clasificaci�n no corresponde a ninguna definici�n de columnas (' + FVariablesClasificacion [I] + ').');
        Result := false;
        Listo := true;
      end;

      Inc(I);
    end;

    // Definicion de variable de clasificacion duplicada
    I := 0;
    Listo := false;
    while (I < FVariablesClasificacion.Count - 1) and (not Listo) do
    begin
      J := I + 1;
      while (J < FVariablesClasificacion.Count) and (not Listo) do
      begin
        if FVariablesClasificacion [I] = FVariablesClasificacion [J] then
        begin
          Bitacora.Add('Definici�n de variable de clasificacion duplicada: #' + IntToStr(I) + ' = ' + FVariablesClasificacion [I] + ', #' + IntToStr(J) + ' = ' + FVariablesClasificacion [J] + '.');
          Result := false;
          Listo := true;
        end;

        Inc(J);
      end;

      Inc(I);
    end;
  end
  else
  begin
    Bitacora.Add('Lista de variables de clasificaci�n vac�a.');
    (* Result := false; *)
  end;
end { TConfiguracionModelo.ValidarVariablesClasificacion };

function TConfiguracionModelo.ValidarVariableDependiente(var Bitacora: TStrings): Boolean;
var
  Listo: Boolean;
  I, J, K: Integer;
begin { TConfiguracionModelo.ValidarVariableDependiente }
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.ValidarVariableDependiente: Assigned(Bitacora)');

  Result := true;

  // La variable dependiente ocurre en la definicion de variables de clasificacion
  I := FVariablesClasificacion.IndexOf(FVariableDependiente);
  if I <> -1 then
  begin
    Bitacora.Add('La variable dependiente ocurre en la definici�n de variables de clasificaci�n: #' + IntToStr(I) + ' = ' + FVariablesClasificacion [I] + '.');
    Result := false;
  end;

  // La variable dependiente ocurre en la definici�n de efectos fijos
  I := 0;
  Listo := false;
  while (I < FEfectosFijos.Efectos.Count) and (not Listo) do
  begin
    J := FEfectosFijos.Efectos [I].ListaVariables.IndexOf(FVariableDependiente);
    if J <> -1 then
    begin
      Bitacora.Add('La variable dependiente ocurre en la definici�n de efectos fijos: #' + IntToStr(I) + ' = ' + FEfectosFijos.Efectos [I].Texto + '.');
      Result := false;
      Listo := true;
    end;

    Inc(I);
  end;

  // La variable dependiente ocurre en la definicion de grupos de efectos aleatorios
  I := 0;
  Listo := false;
  while (I < FGruposEfectosAleatorios.Count) and (not Listo) do
  begin
    J := 0;
    while (J < FGruposEfectosAleatorios [I].Efectos.Count) and (not Listo) do
    begin
      K := FGruposEfectosAleatorios [I].Efectos [J].ListaVariables.IndexOf(FVariableDependiente);
      if K <> -1 then
      begin
        Bitacora.Add('La variable dependiente ocurre en la definici�n de grupos de efectos aleatorios #' + IntToStr(I) + ': #' + IntToStr(J) + ' = ' + FGruposEfectosAleatorios [I].Efectos [J].Texto + '.');
        Result := false;
        Listo := true;
      end;

      Inc(J);
    end;

    Inc(I);
  end;

  // La variable dependiente ocurre en la definici�n de efectos aleatorios no agrupados
  I := 0;
  Listo := false;
  while (I < FEfectosAleatorios.Efectos.Count) and (not Listo) do
  begin
    J := FEfectosAleatorios.Efectos [I].ListaVariables.IndexOf(FVariableDependiente);
    if J <> -1 then
    begin
      Bitacora.Add('La variable dependiente ocurre en la definici�n de efectos aleatorios globales: #' + IntToStr(I) + ' = ' + FEfectosAleatorios.Efectos [I].Texto + '.');
      Result := false;
      Listo := true;
    end;

    Inc(I);
  end;
end { TConfiguracionModelo.ValidarVariableDependiente };

function TConfiguracionModelo.CompararVariables(const U, V: String): Integer;
var
  I1, J1, I2, J2: Integer;
begin { TConfiguracionModelo.CompararVariables }
  I1 := FDatos.Columnas.IndexOf(U);
  J1 := FDatos.Columnas.IndexOf(V);

  Assert(I1 >= 0, 'TConfiguracionModelo.CompararVariables: FDatos.Columnas.IndexOf(U) >= 0');
  Assert(J1 >= 0, 'TConfiguracionModelo.CompararVariables: FDatos.Columnas.IndexOf(V) >= 0');

  if I1 = J1 then
    Result := 0
  else
  begin
    // Verificar si alguna de las variables es de clasificacion
    I2 := FVariablesClasificacion.IndexOf(U);
    J2 := FVariablesClasificacion.IndexOf(V);

    if (I2 >=0) and (J2 >= 0) then
    begin
      // Ambas son variables de clasificacion
      if I2 < J2 then
        Result := -1
      else
        Result := 1;
    end
    else if (I2 >= 0) and (J2 < 0) then
      // U es variable de clasificacion y V no
      Result := 1
    else if (I2 < 0) and (J2 >= 0) then
      // U no es variable de clasificacion y V si
      Result := -1
    else
    begin
      // Ninguna es variable de clasificacion
      if I1 < J1 then
        Result := -1
      else
        Result := 1;
    end;
  end;
end { TConfiguracionModelo.CompararVariables };

constructor TConfiguracionModelo.Create;
begin { TConfiguracionModelo.Create }
  Create(nil, '', nil, '', nil, nil, nil, nil, nil);
end { TConfiguracionModelo.Create };

constructor TConfiguracionModelo.Create(AEtiqueta: String; AVariablesClasificacion: TStrings; AVariableDependiente: String; AEfectosFijos: TGrupoEfectos; AEfectosAleatorios: TGrupoEfectos; AGruposEfectosAleatorios: TListaGruposEfectos; AError: TListaOpciones; AOpciones: TListaOpciones);
begin { TConfiguracionModelo.Create }
  Create(nil, AEtiqueta, AVariablesClasificacion, AVariableDependiente, AEfectosFijos, AEfectosAleatorios, AGruposEfectosAleatorios, AError, AOpciones);
end { TConfiguracionModelo.Create };

constructor TConfiguracionModelo.Create(ADatos: TConfiguracionDatos; AEtiqueta: String; AVariablesClasificacion: TStrings; AVariableDependiente: String; AEfectosFijos: TGrupoEfectos; AEfectosAleatorios: TGrupoEfectos; AGruposEfectosAleatorios: TListaGruposEfectos; AError: TListaOpciones; AOpciones: TListaOpciones);
begin { TConfiguracionModelo.Create }
  FDatos := ADatos;

  FEtiqueta := AEtiqueta;

  if Assigned(AVariablesClasificacion) then
    FVariablesClasificacion := AVariablesClasificacion
  else
    FVariablesClasificacion := TStringList.Create;

  FVariableDependiente := AVariableDependiente;

  if Assigned(AEfectosFijos) then
    FEfectosFijos := AEfectosFijos
  else
    FEfectosFijos := TGrupoEfectos.Create;

  if Assigned(AEfectosAleatorios) then
    FEfectosAleatorios := AEfectosAleatorios
  else
    FEfectosAleatorios := TGrupoEfectos.Create;

  if Assigned(AGruposEfectosAleatorios) then
    FGruposEfectosAleatorios := AGruposEfectosAleatorios
  else
    FGruposEfectosAleatorios := TListaGruposEfectos.Create;

  if Assigned(AError) then
    FError := AError
  else
    FError := TListaOpciones.Create;

  if Assigned(AOpciones) then
    FOpciones := AOpciones
  else
    FOpciones := TListaOpciones.Create;
end { TConfiguracionModelo.Create };

destructor TConfiguracionModelo.Destroy;
begin { TConfiguracionModelo.Destroy }
  try
    FError.Clear;
    FVariablesClasificacion.Clear;
    FGruposEfectosAleatorios.Clear;
    FOpciones.Clear;
  finally
    FreeAndNil(FEfectosAleatorios);
    FreeAndNil(FEfectosFijos);
    FreeAndNil(Ferror);
    FreeAndNil(FGruposEfectosAleatorios);
    FreeAndNil(FOpciones);

    inherited Destroy;
  end;
end { TConfiguracionModelo.Destroy };

function TConfiguracionModelo.Validar(var Bitacora: TStrings): Boolean;
begin { TConfiguracionModelo.Validar }
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.Validar: Assigned(Bitacora)');

  Result := true;

  // Validar individualmente cada componente
  Result := Result and ValidarDatos(Bitacora);
  Result := Result and ValidarVariablesClasificacion(Bitacora);
  Result := Result and ValidarVariableDependiente(Bitacora);
  Result := Result and ValidarEfectosFijos(Bitacora);
  Result := Result and ValidarEfectosAleatorios(Bitacora);
  Result := Result and ValidarOpciones(Bitacora);

  // Verificar la existencia de definiciones de efectos fijos o aleatorios
  if not (FTieneEfectosFijos or FTieneEfectosAleatorios or FTieneEfectosAleatoriosGrupos) then
  begin
    Result := false;
    Bitacora.Add('El modelo no posee ninguna definicion de efectos.');
  end;
end { TConfiguracionModelo.Validar };

function TConfiguracionModelo.Inicializar(var Bitacora: TStrings): Boolean;
var
  Listo: Boolean;
  I, J: Integer;
begin { TConfiguracionModelo.Inicializar }
  Assert(Assigned(FDatos), 'TConfiguracionModelo.Inicializar: Assigned(FDatos)');
  Assert(Assigned(FVariablesClasificacion), 'TConfiguracionModelo.Inicializar: Assigned(FVariablesClasificacion)');
  Assert(Assigned(FEfectosFijos), 'TConfiguracionModelo.Inicializar: Assigned(FEfectosFijos)');
  Assert(Assigned(FEfectosAleatorios), 'TConfiguracionModelo.Inicializar: Assigned(FEfectosAleatorios)');
  Assert(Assigned(FGruposEfectosAleatorios), 'TConfiguracionModelo.Inicializar: Assigned(FGruposEfectosAleatorios)');
  Assert(Assigned(FError), 'TConfiguracionModelo.Inicializar: Assigned(FError)');
  Assert(Assigned(Bitacora), 'TConfiguracionModelo.Inicializar: Assigned(Bitacora)');

  Result := True;

  try
    // Incluir columnas categoricas como variables de clasificacion
    for I := 0 to FDatos.Columnas.Count - 1 do
    begin
      if FDatos.Columnas [I] is TColumnaDatosCategoricos then
      begin
        if FVariablesClasificacion.IndexOf(FDatos.Columnas [I].Nombre) = -1 then
          FVariablesClasificacion.Add(FDatos.Columnas [I].Nombre);
      end;
    end;

    // Reordenar efectos fijos
    for I := 0 to FEfectosFijos.Efectos.Count - 1 do
      FEfectosFijos.Efectos [I].Ordenar(CompararVariables);

    // Reordenar efectos en grupos de efectos aleatorios (y opciones relacionadas)
    for I := 0 to FGruposEfectosAleatorios.Count - 1 do
    begin
      // Reordenar efectos
      for J := 0 to FGruposEfectosAleatorios [I].Efectos.Count - 1 do
        FGruposEfectosAleatorios [I].Efectos [J].Ordenar(CompararVariables);

      // Reordenar efectos en opciones
      for J := 0 to FGruposEfectosAleatorios [I].Opciones.Count - 1 do
      begin
        if FGruposEfectosAleatorios [I].Opciones [J] is TOpcionParametroTEfecto then
          (FGruposEfectosAleatorios [I].Opciones [J] as TOpcionParametroTEfecto).Parametro.Ordenar(CompararVariables);
      end;
    end;

    // Reordenar efectos aleatorios
    for I := 0 to FEfectosAleatorios.Efectos.Count - 1 do
      FEfectosAleatorios.Efectos [I].Ordenar(CompararVariables);

    // Reordenar efectos en opciones de efectos aleatorios
    for I := 0 to FEfectosAleatorios.Opciones.Count - 1 do
    begin
      if FEfectosAleatorios.Opciones [I] is TOpcionParametroTEfecto then
        (FEfectosAleatorios.Opciones [I] as TOpcionParametroTEfecto).Parametro.Ordenar(CompararVariables);
    end;

    // Reordenar efectos en opciones de error
    for I := 0 to FError.Count - 1 do
    begin
      if FError [I] is TOpcionParametroTEfecto then
        (FError [I] as TOpcionParametroTEfecto).Parametro.Ordenar(CompararVariables);
    end;

    // Verificar existencia de efectos fijos
    if FEfectosFijos.Efectos.Count > 0 then
      FTieneEfectosFijos := true
    else
    begin
      // No posee definicion de efectos pero podria incluir intercepto
      I := FEfectosFijos.Opciones.IndexOf('incluir_intercepto');
      if I <> -1 then
        FTieneEfectosFijos := (FEfectosFijos.Opciones [I] as TOpcionParametroBoolean).Parametro
      else
        // Los efectos fijos por defecto poseen intercepto
        FTieneEfectosFijos := true;
    end;

    // Verificar la existencia de efectos aleatorios
    if FEfectosAleatorios.Efectos.Count > 0 then
      FTieneEfectosAleatorios := true
    else
    begin
      // No posee definicion de efectos pero podria incluir intercepto
      I := FEfectosAleatorios.Opciones.IndexOf('incluir_intercepto');
      if I <> -1 then
        FTieneEfectosAleatorios := (FEfectosAleatorios.Opciones [I] as TOpcionParametroBoolean).Parametro
      else
        // Los efectos aleatorios por defecto no poseen intercepto
        FTieneEfectosAleatorios := false;
    end;

    // Verificar la existencia de efectos aleatorios en grupos
    I := 0;
    Listo := false;
    while (I <= FGruposEfectosAleatorios.Count - 1) and (not Listo) do
    begin
      if FGruposEfectosAleatorios [I].Efectos.Count > 0 then
      begin
        Listo := true;
        FTieneEfectosAleatoriosGrupos := true;
      end
      else
      begin
        J := FGruposEfectosAleatorios [I].Opciones.IndexOf('incluir_intercepto');
        if J <> - 1 then
          FTieneEfectosAleatoriosGrupos := (FGruposEfectosAleatorios [I].Opciones [J] as TOpcionParametroBoolean).Parametro;
      end;
    end;
  except on E: Exception do
    begin
      Result := False;
      Bitacora.Add('TConfiguracionModelo.Inicializar: ' + E.Message);
    end;
  end;
end { TConfiguracionModelo.Inicializar };

end { UnitConfiguracionModelo }. 

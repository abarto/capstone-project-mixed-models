{**
@abstract(Configuracion de los datos del simulador de modelos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(March 7, 2005)
@lastmod(March 9, 2005)
Este modulo contiene la clase que almacena la configuracion de los datos
a generar por el simulador de modelos.
}

unit UnitConfiguracionDatos;

interface

uses
  Classes, SysUtils,
  UnitCalculoSimbolico, UnitColumnaDatos, UnitOpcion;

type
  {** Conjunto de indices columnas. }
  TConjuntoIndicesColumnas = set of 0..255;

  {** Configuracion de los datos del simulador. La informacion incluye la
      definicion de columnas y una etiqueta identificatoria.
      @abstract(Configuracion de los datos del simulador.) }
  TConfiguracionDatos = class
  private
    {** Lista de descriptores de columna de datos. }
    FColumnas: TListaColumnasDatos;
    {** Etiqueta descriptiva de los datos. }
    FEtiqueta: String;
    {** Opciones de datos. }
    FOpciones: TListaOpciones;
  public
    {** Constructor.
        @param(AEtiqueta Descripcion de la fuente de datos.)
        @param(AColumnas Lista de descriptores de columnas de datos.)
        @param(AOpciones Lista de opciones sobre los datos.) }
    constructor Create(const AEtiqueta: String; const AColumnas: TListaColumnasDatos; const AOpciones: TListaOpciones);
    {** Destructor. }
    destructor Destroy; override;
    {** Validar expresion numerica. Verifica que una expresion numerica de
        generacion de valores de columna sea valida en terminos de la
        configuracion de columnas existente.
        @param(Expresion Expresion numerica a validar.)
        @param(IndicesColumnasValidos Indices de las que son validos para
        utilizar como indice de variable en una expresion numerica o booleana.)
        @param(Bitacora Registro de posibles errores y advertencias producidas.)
        @returns(Devuelve @true si la expresion es valida o @false en caso
        contrario.) }
    function ValidarExpresion(const Expresion: TCsExpresion; const IndicesColumnasValidos: TConjuntoIndicesColumnas; var Bitacora: TStrings): Boolean;
    {** Validar expresion booleana. Verifica que una expresion booleana de
        generacion de valores de columna sea valida en terminos de la
        configuracion de columnas existente.
        @param(Expresion Expresion booleana a validar.)
        @param(IndicesColumnasValidos Indices de las que son validos para
        utilizar como indice de variable en una expresion numerica o booleana.)
        @param(Bitacora Registro de posibles errores y advertencias producidas.)
        @returns(Devuelve @true si la expresion es valida o @false en caso
        contrario.) }
    function ValidarExpresionBooleana(const Expresion: TCsExpresionBooleana; const IndicesColumnasValidos: TConjuntoIndicesColumnas; var Bitacora: TStrings): Boolean;
    {** Valida el contenido de la configuracion de los datos.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @returns(Devuelve @true si la configuracion de datos es valida, o
        @false en caso contrario.) }
    function Validar(var Bitacora: TStrings): Boolean;
    {** Etiqueta descriptiva de los datos. }
    property Etiqueta: String read FEtiqueta write FEtiqueta;
    {** Descriptores de las columnas de datos. }
    property Columnas: TListaColumnasDatos read FColumnas write FColumnas;
    {** Opciones de datos. }
    property Opciones: TListaOpciones read FOpciones write FOpciones;
  end;

implementation

// -----------------------------------------------------------------------------
// TConfiguracionDatos
// -----------------------------------------------------------------------------

function TConfiguracionDatos.ValidarExpresion(const Expresion: TCsExpresion; const IndicesColumnasValidos: TConjuntoIndicesColumnas; var Bitacora: TStrings): Boolean;
var
  I: Integer;
  AplicacionFuncion: TCsAplicacionFuncion;
  AplicacionFuncionBooleanos: TCsAplicacionFuncionBooleanos;
begin { TConfiguracionDatos.ValidarExpresion }
  Result := True;

  // Solo se validan las expresion susceptibles de contener variables
  if Expresion is TCsVariable then
  begin
    if not ((Expresion as TCsVariable).Indice in IndicesColumnasValidos) then
    begin
      Bitacora.Add('Indice de variable (' + IntToStr((Expresion as TCsVariable).Indice) + ') no corresponde a columna numerica en ' + Expresion.Texto + '.');
      Result := False;
    end;
  end
  else if Expresion is TCsSuma then
  begin
    Result := ValidarExpresion((Expresion as TCsSuma).TerminoIzquierdo, IndicesColumnasValidos, Bitacora) and
              ValidarExpresion((Expresion as TCsSuma).TerminoDerecho, IndicesColumnasValidos, Bitacora);
  end
  else if Expresion is TCsResta then
  begin
    Result := ValidarExpresion((Expresion as TCsResta).TerminoIzquierdo, IndicesColumnasValidos, Bitacora) and
              ValidarExpresion((Expresion as TCsResta).TerminoDerecho, IndicesColumnasValidos, Bitacora);
  end
  else if Expresion is TCsProducto then
  begin
    Result := ValidarExpresion((Expresion as TCsProducto).FactorIzquierdo, IndicesColumnasValidos, Bitacora) and
              ValidarExpresion((Expresion as TCsProducto).FactorDerecho, IndicesColumnasValidos, Bitacora);
  end
  else if Expresion is TCsDivision then
  begin
    Result := ValidarExpresion((Expresion as TCsDivision).Dividendo, IndicesColumnasValidos, Bitacora) and
              ValidarExpresion((Expresion as TCsDivision).Divisor, IndicesColumnasValidos, Bitacora);
  end
  else if Expresion is TCsPotencia then
  begin
    Result := ValidarExpresion((Expresion as TCsPotencia).Base, IndicesColumnasValidos, Bitacora) and
              ValidarExpresion((Expresion as TCsPotencia).Exponente, IndicesColumnasValidos, Bitacora);
  end
  else if Expresion is TCsAplicacionFuncion then
  begin
    AplicacionFuncion := Expresion as TCsAplicacionFuncion;
    I := AplicacionFuncion.Parametros.Bajo;
    while (I <= AplicacionFuncion.Parametros.Alto) and Result do
    begin
      Result := ValidarExpresion(AplicacionFuncion.Parametros [I], IndicesColumnasValidos, Bitacora);
      Inc(I);
    end;
  end
  else if Expresion is TCsAplicacionFuncionBooleanos then
  begin
    AplicacionFuncionBooleanos := Expresion as TCsAplicacionFuncionBooleanos;
    I := AplicacionFuncionBooleanos.Parametros.Bajo;
    while (I <= AplicacionFuncionBooleanos.Parametros.Alto) and Result do
    begin
      Result := ValidarExpresionBooleana(AplicacionFuncionBooleanos.Parametros [I], IndicesColumnasValidos, Bitacora);
      Inc(I);
    end;
  end;
end { TConfiguracionDatos.ValidarExpresion };

function TConfiguracionDatos.ValidarExpresionBooleana(const Expresion: TCsExpresionBooleana; const IndicesColumnasValidos: TConjuntoIndicesColumnas; var Bitacora: TStrings): Boolean;
var
  I: Integer;
  AplicacionPredicadoExpresiones: TCsAplicacionPredicadoExpresiones;
begin { TConfiguracionDatos.ValidarExpresionBooleana }
  Result := True;

  if Expresion is TCsAplicacionPredicadoExpresiones then
  begin
    AplicacionPredicadoExpresiones := Expresion as TCsAplicacionPredicadoExpresiones;
    I := AplicacionPredicadoExpresiones.Parametros.Bajo;
    while (I <= AplicacionPredicadoExpresiones.Parametros.Alto) and Result do
    begin
      Result := ValidarExpresion(AplicacionPredicadoExpresiones.Parametros [I], IndicesColumnasValidos, Bitacora);
      Inc(I);
    end;
  end;
end { TConfiguracionDatos.ValidarExpresionBooleana };

constructor TConfiguracionDatos.Create(const AEtiqueta: String; const AColumnas: TListaColumnasDatos; const AOpciones: TListaOpciones);
begin { TConfiguracionDatos.Create }
  Assert(Assigned(AColumnas), 'TConfiguracionDatos.Create: Assigned(AColumnas)');
  Assert(Assigned(AColumnas), 'TConfiguracionDatos.Create: Assigned(AOpciones)');

  FEtiqueta := AEtiqueta;
  FColumnas := AColumnas;
  FOpciones := AOpciones;
end { TConfiguracionDatos.Create };

destructor TConfiguracionDatos.Destroy;
begin { TConfiguracionDatos.Destroy }
  try
    FColumnas.Clear;
    FOpciones.Clear;
  finally
    FreeAndNil(FColumnas);
    FreeAndNil(FOpciones);
    inherited Destroy;
  end;
end { TConfiguracionDatos.Destroy };

function TConfiguracionDatos.Validar(var Bitacora: TStrings): Boolean;
var
  Listo: Boolean;
  I, J: Integer;
  Columna: TColumnaDatos;
  Opcion: TOpcion;
  IndicesColumnasValidos: TConjuntoIndicesColumnas;
  Expresion: TCsExpresion;
begin { TConfiguracionDatos.Validar }
  Assert(Assigned(FColumnas), 'TConfiguracionDatos.Validar: Assigned(FColumnas)');
  Assert(Assigned(Bitacora), 'TConfiguracionDatos.Validar: Assigned(Bitacora)');

  Result := true;
  Listo := False;

  // Inicializar conjunto de indices validos
  IndicesColumnasValidos := [0 .. FColumnas.Count + 1];

  // Lista de definiciones de columnas vacia
  if FColumnas.Count > 0 then
  begin
    // Definicion de columna duplicada o inconsistente
    I := 0;
    while (I < FColumnas.Count - 1) and (not Listo) do
    begin
      Columna := FColumnas [I];

      // Buscar repeticiones
      J := I + 1;
      while (J < FColumnas.Count) and (not Listo) do
      begin
        if Columna.Nombre = FColumnas [J].Nombre then
        begin
          Bitacora.Add('Definici�n de columna duplicada o inconsistente: #' + IntToStr(I) + ' = ' + Columna.Texto + ', #' + IntToStr(J) + ' = ' + FColumnas [J].Texto + '.');
          Result := false;
          Listo := true;
        end;

        Inc(J);
      end;

      // Validar expresion generadora, si la columna es numerica o que la
      // dimension del arreglo de niveles coincida con la dimension de cantidad
      // de observaciones por nivel.
      if (Columna is TColumnaDatosReales) or
         (Columna is TColumnaDatosEnteros) then
      begin
        if Columna is TColumnaDatosEnteros then
          Expresion := (Columna as TColumnaDatosEnteros).Expresion.ExpresionReal
        else
          Expresion := (Columna as TColumnaDatosReales).Expresion;

        if not ValidarExpresion(Expresion, IndicesColumnasValidos, Bitacora) then
        begin
          Result := false;
          Listo := true;
        end;
      end
      else
      begin
        if (Columna as TColumnaDatosCategoricos).Niveles.Dimension <> (Columna as TColumnaDatosCategoricos).CantidadObservacionesNivel.Dimension then
        begin
          Bitacora.Add('La cantidad dimension del vector de niveles difiere de la dimension del vector de cantidad de observaciones por nivel de la columna ' + Columna.Texto + '.'); 
          Result := false;
          Listo := true;
        end;
      end;

      Inc(I);
    end;
  end
  else
  begin
    Bitacora.Add('Lista de definiciones de columnas vac�a.');
    Result := False;
  end;

  // Validar opciones
  I := 0;
  while (I < FOpciones.Count) and (not Listo) do
  begin
    Opcion := FOpciones [I];

    if (Opcion.Nombre = 'cantidad_observaciones') then
    begin
      if (Opcion as TOpcionParametroInteger).Parametro <= 0 then
      begin
        Bitacora.Add('Cantidad de observaciones invalidas.');
        Listo := True;
        Result := false;
      end;
    end
    else if (Opcion.Nombre = 'archivo_salida') then
    begin
      if (Opcion as TOpcionParametroString).Parametro = '' then
      begin
        Bitacora.Add('Nombre de archivo de salida invalido.');
        Listo := True;
        Result := false;
      end;
    end
    else if (Opcion.Nombre = 'function_caracteristica') then
    begin
      if not ValidarExpresionBooleana((Opcion as TOpcionParametroTObject).Parametro as TCsExpresionBooleana, IndicesColumnasValidos, Bitacora) then
      begin
        Result := false;
        Listo := true;
      end;
    end;

    Inc(I);
  end;
end { TConfiguracionDatos.Validar };

end { UnitConfiguracionDatos }. 
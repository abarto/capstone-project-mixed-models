{**
@abstract(Clases de efectos de modelos mixtos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de efectos de modelos mixtos y diferentes
estructuras de datos para almacenarlas.
}

unit UnitEfecto;

interface

uses
  Classes, Contnrs;

type
  {** Tipo de funciones para comparar dos variables. Se utiliza para reordenar
      los efectos y las lista de efectos anidados. }
  TCompararVariables = function (const Item1, Item2: String): Integer of object;

  {** Definicion de un efecto (generico). Esta clase contiene la funcionalidad
      basica necesaria sobre la informacion de un efecto y define una serie de
      metodos abstractos los cuales deben ser implementados por las subclases
      para dar la funcionalidad particular de cada tipo de efecto.
      @abstract(Efecto generico (abstracto).) }
  TEfecto = class
  protected
    {** Metodo de lectura de la propiedad @link(ListaVariables). Devuelve la
        lista de variables involucradas en el proceso.
        @returns(Lista de variables del efecto.) }
    function GetListaVariables: TStrings; virtual; abstract;
    {** Metodo de lectura de la propiedad @link(Texto). Devuelve la
        representacion como cadena de caracteres del efecto.
        @returns(Representacion de cadena de caracteres del efecto.) }
    function GetTexto: String; virtual; abstract;
  public
    {** Compara el efecto referido con el parametro.
        @param(Efecto Efecto a comparar.)
        @returns(Devuelve @true si el efecto referido y el parametro son iguales
        y @false en caso contrario.) }
    function IgualA(const Efecto: TEfecto): Boolean; virtual; abstract;
    {** Reordena el efecto de acuerdo a una funcion de comparacion de
        variables.
        @param(Comparar Funcion de comparacion de variables.) }
    procedure Ordenar(const Comparar: TCompararVariables); virtual;
    {** Valida la estructura de un efecto. La validacion del contenido del
        efecto depende de informacion del modelo y de la fuente de datos, esta
        funcion solo verifica que el efecto este construido de manera valida.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @param(VerificarRepeticionesVariables Indica si se debe chequer
        repeticiones de variables dentro del efecto.)
        @returns(Devuelve @true si el efecto es valido o @false en caso
        contrario.) }
    function Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean; virtual;
    {** Indica si una variable ocurre dentro de un efecto. Este metodo es util
        durante la validacion de los modelos para verificar que ciertas
        variables (como la variable dependiente) no ocurra dentro del efecto.
        @param(AVariable Variable a buscar en el efecto.)
        @returns(@true si la variable ocurre en el efecto, o @false en caso
        contrario.) }
    function VariableOcurre(const AVariable: String): Boolean; virtual; abstract;
    {** Lista de variables involucradas en el efecto. }
    property ListaVariables: TStrings read GetListaVariables;
    {** Representacion de cadena de caracteres del efecto. }
    property Texto: String read GetTexto;
  end { TEfecto };

  {** Definicion de un intercepto.
      @abstract(Intercepto.) }
  TIntercepto = class(TEfecto)
  protected
    {** Ver @link(TEfecto.GetListaVariables). }
    function GetListaVariables: TStrings; override;
    {** Ver @link(TEfecto.GetTexto). }
    function GetTexto: String; override;
  public
    {** Ver @link(TEfecto.IgualA). }
    function IgualA(const Efecto: TEfecto): Boolean; override;
    {** Ver @link(TEfecto.VariableOcurre). }
    function VariableOcurre(const AVariable: String): Boolean; override;
  end { TIntercepto };

  {** Definicion de un efecto principal en un modelo mixto.
      @abstract(Efecto principal.) }
  TEfectoPrincipal = class(TEfecto)
  protected
    {** Ver @link(TEfecto.GetListaVariables). }
    function GetListaVariables: TStrings; override;
    {** Ver @link(TEfecto.GetTexto). }
    function GetTexto: String; override;
  private
    {** Variable del efecto principal. }
    FVariable: String;
  public
    {** Constructor.
        @param(AVariable Variable del efecto principal.) }
    constructor Create(const AVariable: String);
    {** Ver @link(TEfecto.IgualA). }
    function IgualA(const Efecto: TEfecto): Boolean; override;
    {** Ver @link(TEfecto.VariableOcurre). }
    function VariableOcurre(const AVariable: String): Boolean; override;
    {** Variable del efecto principal. }
    property Variable: String read FVariable write FVariable;
  end { TEfectoPrincipal };

  {** Definicion de efecto de cruzamiento. Funciona de la misma manera que una
      lista enlazada con un primer efecto principal como cabecera y otro efecto
      como resto. Por ejemplo, si el cruzamiento A*B*C es visto como <A, B*C> el
      cual puede verse como <A, <B, C>>.
      @abstract(Efecto de cruzamiento.) }
  TCruzamiento = class(TEfecto)
  protected
    {** Ver @link(TEfecto.GetListaVariables). }
    function GetListaVariables: TStrings; override;
    {** Ver @link(TEfecto.GetTexto). }
    function GetTexto: String; override;
  private
    {** Primer efecto cruzado. }
    FCabecera: TEfectoPrincipal;
    {** Resto del cruzamiento. }
    FResto: TEfecto;
  public
    {** Constructor.
        @param(ACabecera Primer efecto principal del cruzamiento.)
        @param(AResto Resto del efecto de cruzamiento.) }
    constructor Create(const ACabecera: TEfectoPrincipal; const AResto: TEfecto);
    {** Destructor. }
    destructor Destroy; override;
    {** Ver @link(TEfecto.IgualA). }
    function IgualA(const Efecto: TEfecto): Boolean; override;
    {** Ver @link(TEfecto.Ordenar). }
    procedure Ordenar(const Comparar: TCompararVariables); override;
    {** Ver @link(TEfecto.Validar). }
    function Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean; override;
    {** Ver @link(TEfecto.VariableOcurre). }
    function VariableOcurre(const AVariable: String): Boolean; override;
    {** Primer efecto cruzado. }
    property Cabecera: TEfectoPrincipal read FCabecera write FCabecera;
    {** Resto del cruzamiento. }
    property Resto: TEfecto read FResto write FResto;
  end { TCruzamiento };

  {** Lista de efectos.
      @abstract(Lista de efectos.) }
  TListaEfectos = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad @link(Efectos). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(const Index: Integer): TEfecto;
    {** Metodo de escritura de la propiedad @link(Efectos). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(const Index: Integer; const Item: TEfecto);
    {** Metodo de lectura de la pripiedad @link(Texto). Devuelve la
        representacion de cadena de caracteres de la lista de efectos.
        @returns(Representacion de cadena de caracteres del efecto.) }
    function GetTexto: String;
    {** Metodo de lectura de la pripiedad @link(IncluyeIntercepto). Indica si
        la lista posee un @link(TIntercepto). Es util para determinar si el
        usuario desea incluir un intercepto en el modelo.
        @returns(Devuelve @true si la lista posee un intercepto o @false en caso
        contrario.) }
    function GetIncluyeIntercepto: Boolean;
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(const Item: TEfecto): Integer;
    {** Compara la lista referida con el parametro para deterimnar si poseen los
        mismos elementos y en el mismo orden. A tal fin, hace uso de
        @link(TEfecto.IgualA).
        @param(Lista Lista a comparar con la referida.)
        @returns(Devuelve @true si las listas son iguales o @false en caso
        contrario.) }
    function Equals(const Lista: TListaEfectos): Boolean;
    {** Busca un elemento dentro de la lista utilizando
        @link(TEfecto.IgualA).
        @param(Efecto Efecto a buscar.)
        @returns(Devuelve el indice del efecto en caso de encontrarlo, o -1 en
        caso contrario.) }
    function IndexOf(const Efecto: TEfecto): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(const Index: Integer; const Item: TEfecto);
    {** Efectos almacenados en la lista. }
    property Efectos [const Index: Integer]: TEfecto read GetItem write SetItem; default;
    {** Indica si la lista posee un intercepto. }
    property IncluyeIntercepto: Boolean read GetIncluyeIntercepto;
    {** Representacion de cadena de caracteres de la lista. }
    property Texto: String read GetTexto;
  end { TListaEfectos };

  {** Lista de efectos anidados. Se utiliza para almacenar la lista de efecto
      principales anidados en un efecto de anidamiento. Es una especializacion
      de @link(TListaEfectos) para restringir al uso de efectos principales y
      permitir la implementacion de un algoritmo de ordenacion.
      @abstract(Lista de efectos (principales) andidados.) }
  TListaEfectosAnidados = class(TListaEfectos)
  protected
    {** Metodo de lectura de la propiedad @link(EfectosAnidados). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(const Index: Integer): TEfectoPrincipal;
    {** Metodo de escritura de la propiedad @link(EfectosAnidados). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(const Index: Integer; const Item: TEfectoPrincipal);
  public
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(const Item: TEfectoPrincipal): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(const Index: Integer; const Item: TEfectoPrincipal);
    {** Ordena la lista de acuerdo a una funcion de comparacion de variables.
        @param(Compare Funcion de comparacion de variables.) }
    procedure Sort(const Compare: TCompararVariables);
    {** Efectos almacenados en la lista. }
    property EfectosAnidados [const Index: Integer]: TEfectoPrincipal read GetItem write SetItem; default;
  end { TListaEfectosAnidados };

  {** Definicion de efecto de anidamiento. El mismo consta de un efecto generico
      como cabecera y una lista de efectos (principales) anidados. Por ejemplo,
      si el efecto es A*B(C D E), entonces el cruzamiento A*B seria la cabecera
      y (C, D, E) la lista de efectos principales anidados.
      @abstract(Efecto de anidamiento.) }
  TAnidamiento = class(TEfecto)
  protected
    {** Ver @link(TEfecto.GetListaVariables). }
    function GetListaVariables: TStrings; override;
    {** Ver @link(TEfecto.GetTexto). }
    function GetTexto: String; override;
  private
    {** Cabecera del anidamiento. Notar que puede ser cualquier clase de efecto
        por lo que deben tomarse recaudos para no construir efectos invalidos. }
    FCabecera: TEfecto;
    {** Lista de efectos (principales) andidados. }
    FEfectosAnidados: TListaEfectosAnidados;
    {** Metodo de lectura de la propiedad @link(ListaVariablesEfectosAnidados).
        Devuelve la lista de variables de los efectos principales anidados.
        @returns(Lista de variables de los efectos anidados.) }
    function GetListaVariablesEfectosAnidados: TStrings;
  public
    {** Constructor.
        @param(ACabecera Efecto de cabecera.)
        @param(AEfectosAnidados Lista de efectos anidados.) }
    constructor Create(const ACabecera: TEfecto; const AEfectosAnidados: TListaEfectosAnidados);
    {** Destructor. }
    destructor Destroy; override;
    {** Ver @link(TEfecto.IgualA). }
    function IgualA(const Efecto: TEfecto): Boolean; override;
    {** Ver @link(TEfecto.Ordenar). }
    procedure Ordenar(const Comparar: TCompararVariables); override;
    {** Ver @link(TEfecto.Validar). }
    function Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean; override;
    {** Ver @link(TEfecto.VariableOcurre). }
    function VariableOcurre(const AVariable: String): Boolean; override;
    {** Cabecera del anidamiento. }
    property Cabecera: TEfecto read FCabecera write FCabecera;
    {** Efectos anidados. }
    property EfectosAnidados: TListaEfectosAnidados read FEfectosAnidados write FEfectosAnidados;
    {** Lista de variables de efectos anidados. }
    property ListaVariablesEfectosAnidados: TStrings read GetListaVariablesEfectosAnidados;
  end { TAnidamiento };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TEfecto
// -----------------------------------------------------------------------------

procedure TEfecto.Ordenar(const Comparar: TCompararVariables);
begin { TEfecto.Ordenar }
end { TEfecto.Ordenar };

function TEfecto.Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean;
begin { TEfecto.Validar }
  Assert(Assigned(Bitacora), 'TEfecto.Validar: Assigned(Bitacora)');
  Result := true;
end { TEfecto.Validar };

// -----------------------------------------------------------------------------
// TIntercepto
// -----------------------------------------------------------------------------

function TIntercepto.GetListaVariables: TStrings;
begin { TIntercepto.GetListaVariables }
  Result := TStringList.Create;
end { TIntercepto.GetListaVariables };

function TIntercepto.GetTexto: String;
begin { TIntercepto.GetTexto }
  Result := 'Intercepto';
end { TIntercepto.GetTexto };

function TIntercepto.IgualA(const Efecto: TEfecto): Boolean;
begin { TIntercepto.IgualA }
  Assert(Assigned(Efecto), 'TIntercepto.IgualA: Assigned(Efecto)');

  Result := Efecto is TIntercepto;
end { TIntercepto.IgualA };

function TIntercepto.VariableOcurre(const AVariable: String): Boolean;
begin { TIntercepto.VariableOcurre }
  Result := false;
end { TIntercepto.VariableOcurre };

// -----------------------------------------------------------------------------
// TEfectoPrincipal
// -----------------------------------------------------------------------------

function TEfectoPrincipal.GetListaVariables: TStrings;
begin { TEfectoPrincipal.GetListaVariables }
  Result := TStringList.Create;
  Result.Add(FVariable);
end { TEfectoPrincipal.GetListaVariables };

function TEfectoPrincipal.GetTexto: String;
begin { TEfectoPrincipal.GetTexto }
  Result := FVariable;
end { TEfectoPrincipal.GetTexto };

constructor TEfectoPrincipal.Create(const AVariable: String);
begin { TEfectoPrincipal.Create }
  FVariable := AVariable;
end { TEfectoPrincipal.Create };

function TEfectoPrincipal.IgualA(const Efecto: TEfecto): Boolean;
begin { TEfectoPrincipal.IgualA }
  Assert(Assigned(Efecto), 'TEfectoPrincipal.IgualA: Assigned(Efecto)');

  if Efecto is TEfectoPrincipal then
    Result := (Efecto as TEfectoPrincipal).FVariable = FVariable
  else
    Result := False;
end { TEfectoPrincipal.IgualA };

function TEfectoPrincipal.VariableOcurre(const AVariable: String): Boolean;
begin
  Result := Variable = AVariable;
end; // TEfectoPrincipal.VariableOcurre

// -----------------------------------------------------------------------------
// TCruzamiento
// -----------------------------------------------------------------------------

function TCruzamiento.GetListaVariables: TStrings;
begin { TCruzamiento.GetListaVariables }
  Result := TStringList.Create;

  // Notar que el orden de las variables es el de construccion
  Result.Add(FCabecera.Variable);
  Result.AddStrings(FResto.ListaVariables);
end { TCruzamiento.GetListaVariables };

function TCruzamiento.GetTexto: String;
begin { TCruzamiento.GetTexto }
  Result := FCabecera.Texto + '*' + FResto.Texto;
end { TCruzamiento.GetTexto };

constructor TCruzamiento.Create(const ACabecera: TEfectoPrincipal; const AResto: TEfecto);
begin { TCruzamiento.Create }
  Assert(Assigned(ACabecera), 'TCruzamiento.Create: Assigned(ACabecera)');
  Assert(Assigned(AResto), 'TCruzamiento.Create: Assigned(AResto)');

  FCabecera := ACabecera;
  FResto := AResto;
end { TCruzamiento.Create };

destructor TCruzamiento.Destroy;
begin { TCruzamiento.Destroy }
  try
    FreeAndNil(FCabecera);
    FreeAndNil(FResto);
  finally
    inherited Destroy;
  end;
end { TCruzamiento.Destroy };

function TCruzamiento.IgualA(const Efecto: TEfecto): Boolean;
begin { TCruzamiento.IgualA }
  Assert(Assigned(Efecto), 'TCruzamiento.IgualA: Assigned(Efecto)');

  if Efecto is TCruzamiento then
    Result := FCabecera.IgualA((Efecto as TCruzamiento).Cabecera) and FResto.IgualA((Efecto as TCruzamiento).Resto)
  else
    Result := false;
end { TCruzamiento.IgualA };

procedure TCruzamiento.Ordenar(const Comparar: TCompararVariables);
var
  Variable1: String;
  Cruzamiento: TCruzamiento;
  Listo: Boolean;
begin { TEfectoCruzamiento.Ordenar }
  // Ordenar resto
  FResto.Ordenar(Comparar);

  // Insertar ordenada la cabecera
  Cruzamiento := Self;
  Listo := False;
  while (not Listo) do
  begin
    if Cruzamiento.FResto is TEfectoPrincipal then
    begin
      // Caso base
      if Comparar(Cruzamiento.FCabecera.Variable, (Cruzamiento.FResto as TEfectoPrincipal).Variable) > 0 then
      begin
        Variable1 := Cruzamiento.FCabecera.Variable;
        Cruzamiento.FCabecera.Variable := (Cruzamiento.FResto as TEfectoPrincipal).Variable;
        (Cruzamiento.FResto as TEfectoPrincipal).Variable := Variable1;
      end;
      Listo := True;
    end
    else if Cruzamiento.FResto is TCruzamiento then
    begin
      // Paso inductivo
      if Comparar(Cruzamiento.FCabecera.Variable, (Cruzamiento.FResto as TCruzamiento).FCabecera.Variable) > 0 then
      begin
        Variable1 := Cruzamiento.FCabecera.Variable;
        Cruzamiento.FCabecera.Variable := (Cruzamiento.FResto as TCruzamiento).FCabecera.Variable;
        (Cruzamiento.FResto as TCruzamiento).FCabecera.Variable := Variable1;
      end
      else
        Listo := True;
    end;
  end;
end { TEfectoCruzamiento.Ordenar };

function TCruzamiento.Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean): Boolean;
begin { TCruzamiento.Validar }
  Assert(Assigned(Bitacora), 'TCruzamiento.Validar: Assigned(Bitacora)');

  Result := FResto.Validar(Bitacora, VerificarRepeticionesVariables);

  // Error de estructura en resto cruzamiento
  if not ((FResto is TCruzamiento) or (FResto is TEfectoPrincipal)) then
  begin
    Bitacora.Add('Error de estructura en resto cruzamiento');
    Result := false;
  end;

  // Intercepto ocurre en cruzamiento
  if Result and (FResto is TIntercepto) then
  begin
    Bitacora.Add('Intercepto ocurre en cruzamiento');
    Result := false;
  end;

  if Result and VerificarRepeticionesVariables then
  begin
    // Variable de cabecera duplicada en cruzamiento
    if FResto.VariableOcurre(FCabecera.Variable) then
    begin
      Bitacora.Add('Variable de cabecera duplicada en cruzamiento: ' + FCabecera.Variable);
      Result := false;
    end;
  end;
end { TCruzamiento.Validar };

function TCruzamiento.VariableOcurre(const AVariable: String): Boolean;
begin { TCruzamiento.VariableOcurre }
  Result := (FCabecera.Variable = AVariable) or FResto.VariableOcurre(AVariable);
end { TCruzamiento.VariableOcurre };

// -----------------------------------------------------------------------------
// TListaEfectos
// -----------------------------------------------------------------------------

function TListaEfectos.GetIncluyeIntercepto: Boolean;
var
  I: Integer;
begin { TListaEfectos.GetIncluyeIntercepto }
  I := 0;
  Result := false;
  while (I < Count) and (not Result) do
  begin
    Result := Result or (GetItem(I) is TIntercepto);
    Inc(I);
  end;
end { TListaEfectos.GetIncluyeIntercepto };

function TListaEfectos.GetItem(const Index: Integer): TEfecto;
begin { TListaEfectos.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaEfectos.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TEfecto;
end { TListaEfectos.GetItem };

function TListaEfectos.GetTexto: String;
var
  I: Integer;
begin { TListaEfectos.GetTexto }
  Result := '';

  if Count > 0 then
  begin
    for I := 0 to Count - 2 do
      Result := Result + GetItem(I).Texto + ' ';
    Result := Result + GetItem(Count - 1).Texto;
  end;
end { TListaEfectos.GetTexto };

procedure TListaEfectos.SetItem(const Index: Integer; const Item: TEfecto);
begin { ListaEfectos.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaEfectos.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaEfectos.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { ListaEfectos.SetItem };

destructor TListaEfectos.Destroy;
begin { TListaEfectos.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaEfectos.Destroy };

function TListaEfectos.Add(const Item: TEfecto): Integer;
begin { TListaEfectos.Add }
  Assert(Assigned(Item), 'TListaEfectos.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaEfectos.Add };

function TListaEfectos.Equals(const Lista: TListaEfectos): Boolean;
var
  I: Integer;
begin { TListaEfectos.Equals }
  Assert(Assigned(Lista), 'TListaEfectos.Equals: Assigned(Lista)');

  if Count = Lista.Count then
  begin
    Result := true;
    I := 0;
    while (I < Count) and Result do
    begin
      if not Efectos [I].IgualA(Lista [I]) then
        Result := false;
      Inc(I);
    end;
  end
  else
    Result := false;
end { TListaEfectos.Equals };

function TListaEfectos.IndexOf(const Efecto: TEfecto): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TListaEfectos.IndexOf }
  Assert(Assigned(Efecto), 'TListaEfectos.IndexOf: Assigned(Efecto)');

  Result := -1;
  I := 0;
  Listo := false;
  while (I <= Count - 1) and (not Listo) do
  begin
    if (GetItem(I) as TEfecto).IgualA(Efecto) then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TListaEfectos.IndexOf };

procedure TListaEfectos.Insert(const Index: Integer; const Item: TEfecto);
begin { TListaEfectos.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaEfectos.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaEfectos.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaEfectos.Insert };

// -----------------------------------------------------------------------------
// TListaEfectosAnidados
// -----------------------------------------------------------------------------

function TListaEfectosAnidados.GetItem(const Index: Integer): TEfectoPrincipal;
begin { TListaEfectosAnidados.GetItem }
  Result := inherited GetItem(Index) as TEfectoPrincipal;
end { TListaEfectosAnidados.GetItem };

procedure TListaEfectosAnidados.SetItem(const Index: Integer; const Item: TEfectoPrincipal);
begin { TListaEfectosAnidados.SetItem }
  inherited SetItem(Index, Item);
end { TListaEfectosAnidados.SetItem };

function TListaEfectosAnidados.Add(const Item: TEfectoPrincipal): Integer;
begin { TListaEfectosAnidados.Add }
  Result := inherited Add(Item);
end { TListaEfectosAnidados.Add };

procedure TListaEfectosAnidados.Insert(const Index: Integer; const Item: TEfectoPrincipal);
begin { TListaEfectosAnidados.Insert }
  inherited Insert(Index, Item);
end { TListaEfectosAnidados.Insert };

procedure TListaEfectosAnidados.Sort(const Compare: TCompararVariables);
var
  I, J, IndiceMinimo: Integer;
  Efecto: TEfectoPrincipal;
begin { TListaEfectosAnidados.Sort }
  IndiceMinimo := 0;
  for I := 1 to Count - 1 do
  begin
    if Compare(GetItem(I).Variable, GetItem(IndiceMinimo).Variable) < 0 then
      IndiceMinimo := I;
  end;

  if IndiceMinimo <> 0 then
    Exchange(0, IndiceMinimo);

  for I := 2 to Count - 1 do
  begin
    Efecto := GetItem(I);

    J := I;
    while (Compare(Efecto.Variable, GetItem(J - 1).Variable) < 0) do
    begin
      SetItem(J, GetItem(J - 1));
      Dec(J);
    end;

    SetItem(J, Efecto);
  end;
end { TListaEfectosAnidados.Sort };

// -----------------------------------------------------------------------------
// TAnidamiento
// -----------------------------------------------------------------------------

function TAnidamiento.GetListaVariables: TStrings;
var
  I: Integer;
begin { TAnidamiento.GetListaVariables }
  // Construir lista de variables de la cabecera
  Result := FCabecera.ListaVariables;

  // Agregar la lista de variables de los efectos anidados
  for I := 0 to FEfectosAnidados.Count - 1 do
    Result.Add(FEfectosAnidados [I].Variable);
end { TAnidamiento.GetListaVariables };

function TAnidamiento.GetListaVariablesEfectosAnidados: TStrings;
var
  I: Integer;
begin { TAnidamiento.GetListaVariablesEfectosAnidados }
  Result := TStringList.Create;

  for I := 0 to FEfectosAnidados.Count - 1 do
    Result.Add(FEfectosAnidados [I].Variable);
end { TAnidamiento.GetListaVariablesEfectosAnidados };

function TAnidamiento.GetTexto: String;
var
  I: Integer;
begin { TAnidamiento.GetTexto }
  Result := FCabecera.Texto + '(';
  if FEfectosAnidados.Count > 0 then
  begin
    for I := 0 to FEfectosAnidados.Count - 2 do
      Result := Result + (FEfectosAnidados [I].Texto) + ' ';
    Result := Result + FEfectosAnidados [FEfectosAnidados.Count - 1].Texto;
  end;
  Result := Result + ')';
end { TAnidamiento.GetTexto };

constructor TAnidamiento.Create(const ACabecera: TEfecto; const AEfectosAnidados: TListaEfectosAnidados);
begin { TAnidamiento.Create }
  Assert(Assigned(ACabecera), 'TAnidamiento.Create: Assigned(ACabecera)');
  Assert(Assigned(AEfectosAnidados), 'TAnidamiento.Create: Assigned(AEfectosAnidados)');

  FCabecera := ACabecera;
  FEfectosAnidados := AEfectosAnidados;
end { TAnidamiento.Create };

destructor TAnidamiento.Destroy;
begin { TAnidamiento.Destroy }
  try
    FreeAndNil(FCabecera);
    FreeAndNil(FEfectosAnidados);
  finally
    inherited Destroy;
  end;
end { TAnidamiento.Destroy };

function TAnidamiento.IgualA(const Efecto: TEfecto): Boolean;
begin { TAnidamiento.IgualA }
  Assert(Assigned(Efecto), 'TAnidamiento.IgualA: Assigned(Efecto)');

  if Efecto is TAnidamiento then
    Result := FCabecera.IgualA((Efecto as TAnidamiento).Cabecera) and FEfectosAnidados.Equals((Efecto as TAnidamiento).EfectosAnidados)
  else
    Result := false;
end { TAnidamiento.IgualA };

procedure TAnidamiento.Ordenar(const Comparar: TCompararVariables);
begin { TAnidamiento.Ordenar }
  FCabecera.Ordenar(Comparar);
  FEfectosAnidados.Sort(Comparar);
end { TAnidamiento.Ordenar };

function TAnidamiento.Validar(var Bitacora: TStrings; const VerificarRepeticionesVariables: Boolean = false): Boolean;
var
  Done: Boolean;
  I, J: Integer;
begin { TAnidamiento.Validar }
  Assert(Assigned(Bitacora), 'TAnidamiento.Validar: Assigned(Bitacora)');

  // Validar cabecera
  Result := FCabecera.Validar(Bitacora, VerificarRepeticionesVariables);

  // Variable en cabecera de efecto anidado ocurre en la lista de efectos anidados
  I := 0;
  Done := false;
  while (I < FEfectosAnidados.Count) and (not Done) do
  begin
    if FCabecera.VariableOcurre(FEfectosAnidados [I].Variable) then
    begin
      Bitacora.Add('Variable en cabecera de efecto anidado ocurre en la lista de efectos anidados: ' + (FEfectosAnidados [I] as TEfectoPrincipal).Variable);
      Result := false;
      Done := true;
    end;

    Inc(I);
  end;

  // Lista de efectos anidados vac�a
  if (EfectosAnidados.Count > 0) then
  begin
    // Variable duplicada en lista de efectos anidados
    if (EfectosAnidados.Count > 1) then
    begin
      I := 0;
      Done := false;
      while (I < EfectosAnidados.Count - 1) and (not Done) do
      begin
        J := I + 1;
        while (J < EfectosAnidados.Count) and (not Done) do
        begin
          if EfectosAnidados [I].Variable = EfectosAnidados [J].Variable then
          begin
            Bitacora.Add('Variable duplicada en lista de efectos anidados: ' + (EfectosAnidados [I] as TEfectoPrincipal).Variable);
            Result := false;
            Done := true;
          end;

          Inc(J);
        end;

        Inc(I);
      end;
    end;
  end
  else
  begin
    Bitacora.Add('Lista de efectos anidados vac�a');
    Result := false;
  end;
end { TAnidamiento.Validar };

function TAnidamiento.VariableOcurre(const AVariable: String): Boolean;
var
  I: Integer;
begin { TAnidamiento.VariableOcurre }
  Result := FCabecera.VariableOcurre(AVariable);
  I := 0;

  while (I <= EfectosAnidados.Count - 1) and (not Result) do
  begin
    if EfectosAnidados [I].Variable = AVariable then
      Result := true;
    Inc(I);
  end;
end { TAnidamiento.VariableOcurre };

end { UnitEfecto }.

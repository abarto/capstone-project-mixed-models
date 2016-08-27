{**
@abstract(Clases y subrutinas sobre parametros de modelos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(March 22, 2005)
Este modulo contiene la definicion del tipo basico de parametros de modelos
y clases de almacenamiento y manipulacion de informacion sobre los mismos.
}
unit UnitParametros;

interface

uses
  Classes, Contnrs,
  UaComun, UaVector;

type
  {** Definicion del tipo basico de parametros de modelos. Se utiliza esta
      forma de definicion para permitir la posibilidad de redefinir el tipo en
      caso de necesitar una mayor o menor precision numerica. }
  TValorParametro = TUaReal;

type
  {** Tupla que contiene informacion sobre restricciones de un parametro. La
      misma consta de un nombre, un valor inicial, y cotas inferior y
      superior y paso de grilla de busqueda.
      @abstract(Tupla que contiene informacion sobre un parametro.) }
  TInformacionParametro = class
  protected
    {** Cota inferior. }
    FCotaInferior: TValorParametro;
    {** Cota superior. }
    FCotaSuperior: TValorParametro;
    {** Nombre del parametro. }
    FNombre: String;
    {** Paso de recorrido de grilla de busqueda. }
    FPaso: TValorParametro;
    {** Indicador de presencia de cota inferior. }
    FTieneCotaInferior: Boolean;
    {** Indicador de presencia de cota superior. }
    FTieneCotaSuperior: Boolean;
    {** Indicador de presencia de paso. }
    FTienePaso: Boolean;
    {** Indicador de presencia de valor inicial. }
    FTieneValorInicial: Boolean;
    {** Valor inicial. }
    FValorInicial: TValorParametro;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion textual de la tupla.
        @returns(Reperesentacion como cadena de caracteres de la tupla.) }
    function GetTexto: String; virtual;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AValorInicial Valor inicial del parametro.) }
    constructor Create(const AValorInicial: TValorParametro); overload;
    {** Constructor.
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.) }
    constructor Create(const ANombre: String; const AValorInicial: TValorParametro); overload;
    {** Constructor.
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.)
        @param(ACotaInferior Cota inferior del parametro.)
        @param(ACotaSuperior Cota superior del parametro.)
        @param(APaso Paso de recorrido de grilla de busqueda.)
        @param(ATieneValorInicial Indica si se debe respetar el valor inicial.)
        @param(ATieneCotaInferior Indica si se debe respetar la cota inferior.)
        @param(ATieneCotaSuperior Indica si se debe respetar la cota superior.)
        @param(ATienePaso Indica si se debe respetar el paso de busqueda.) }
    constructor Create(const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro; const ATieneValorInicial, ATieneCotaInferior, ATieneCotaSuperior, ATienePaso: Boolean); overload;
    {** Constructor.
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.)
        @param(ACotaInferior Cota inferior del parametro.)
        @param(ACotaSuperior Cota superior del parametro.)
        @param(APaso Paso de recorrido de grilla de busqueda.) }
    constructor Create(const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro); overload;
    {** Constructor. Esta version del constructor genera una copia del
        parametro.
        @param(AInformacion Tupla de informacion de parametro a copiar.) }
    constructor Create(const Informacion: TInformacionParametro); overload;
    {** Asignar los campos de acuerdo a la informacion de una tupla.
        @param(Informacion Tupla de la cual obtener los valores de
        asginacion.) }
    procedure Asignar(const Informacion: TInformacionParametro);
    {** Construye una copia por valor de la tupla.
        @returns(Una tupla conteniendo la misma informacion que la
        referenciada.) }
    function Clonar: TInformacionParametro;
    {** Compara el parametro (del metodo) con la tupla referido. Solo se utiliza
        el nombre para determinar si dos tuplas son iguales.
        @param(Informacion Tupla a comparar.)
        @returns(@true si las tuplas son iguales o @false en caso contrario.) }
    function IgualA(const Informacion: TInformacionParametro): Boolean;
    {** Cota inferior. Todo valor del parametro debe ser superior a esta cota.
        Es responsabilidad del usuario que el valor respete esta cota. }
    property CotaInferior: TValorParametro read FCotaInferior write FCotaInferior;
    {** Cota superior. Todo valor del parametro debe ser inferior a esta cota.
        Es responsabilidad del usuario que el valor respete esta cota. }
    property CotaSuperior: TValorParametro read FCotaSuperior write FCotaSuperior;
    {** Nombre del parametro. }
    property Nombre: String read FNombre write FNombre;
    {** Paso de busqueda en grilla. }
    property Paso: TValorParametro read FPaso write FPaso;
    {** Representacion textual de la tupla }
    property Texto: String read GetTexto;
    {** Indicador de presencia de cota inferior. }
    property TieneCotaInferior: Boolean read FTieneCotaInferior write FTieneCotaInferior;
    {** Indicador de presencia de cota superior. }
    property TieneCotaSuperior: Boolean read FTieneCotaSuperior write FTieneCotaSuperior;
    {** Indicador de presencia de paso de busqueda. }
    property TienePaso: Boolean read FTienePaso write FTienePaso;
    {** Indicador de presencia de paso de busqueda. }
    property TieneValorInicial: Boolean read FTieneValorInicial write FTieneValorInicial;
    {** Valor inicial del parametro. }
    property ValorInicial: TValorParametro read FValorInicial write FValorInicial;
  end;

  {** Extension de la tupla de informacion sobre parametro
      (Ver @link(TInformacionParametro)). Esta nueva tupla incorpora un indice
      que puede utilizarse para indicar un orden deseado en una estructura
      de datos indexable.
      @abstract(Tupla de informacion sobre parametro extendida para incorporar
      un indice.) }
  TInformacionParametroIndice = class(TInformacionParametro)
  protected
    {** Indice de la tupla en alguna estructura. }
    FIndice: Integer;

    {** Ver @link(TInformacionParametro.GetTexto). }
    function GetTexto: String; override;
  public
    constructor Create; overload;
    {** @param(AValorInicial Valor inicial del parametro.) }
    constructor Create(const AValorInicial: TValorParametro); overload;
    {** @param(AIndice Indice de la tupla.)
        @param(AValorInicial Valor inicial del parametro.) }
    constructor Create(const AIndice: Integer; const AValorInicial: TValorParametro); overload;
    {** @param(AIndice Indice de la tupla.)
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.) }
    constructor Create(const AIndice: Integer; const ANombre: String; const AValorInicial: TValorParametro); overload;
    {** @param(AIndice Indice de la tupla.)
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.)
        @param(ACotaInferior Cota inferior del parametro.)
        @param(ACotaSuperior Cota superior del parametro.)
        @param(APaso Paso de busqueda en grilla.) }
    constructor Create(const AIndice: Integer; const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro); overload;
    {** Constructor.
        @param(AIndice Indice de la tupla.)
        @param(ANombre Nombre del parametro.)
        @param(AValorInicial Valor inicial del parametro.)
        @param(ACotaInferior Cota inferior del parametro.)
        @param(ACotaSuperior Cota superior del parametro.)
        @param(APaso Paso de recorrido de grilla de busqueda.)
        @param(ATieneValorInicial Indica si se debe respetar el valor inicial.)
        @param(ATieneCotaInferior Indica si se debe respetar la cota inferior.)
        @param(ATieneCotaSuperior Indica si se debe respetar la cota superior.)
        @param(ATienePaso Indica si se debe respetar el paso de busqueda.) }
    constructor Create(const AIndice: Integer; const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro; const ATieneValorInicial, ATieneCotaInferior, ATieneCotaSuperior, ATienePaso: Boolean); overload;
    {** @param(AIndice Indice de la tupla.)
        @param(AInformacion Tupla que contiene el resto de la informacion.) }
    constructor Create(const AIndice: Integer; const AInformacion: TInformacionParametro); overload;
    {** Asignar los campos de acuerdo a la informacion de una tupla.
        @param(Informacion Tupla de la cual obtener los valores de
        asginacion.) }
    procedure Asignar(const Informacion: TInformacionParametroIndice);
    {** Construye una copia por valor de la tupla.
        @returns(Una tupla conteniendo la misma informacion que la
        referenciada.) }
    function Clonar: TInformacionParametroIndice;
    {** Indice de la tupla. }
    property Indice: Integer read FIndice write FIndice;
  end;

  {** Lista de tuplas con indice de informacion de parametros bajo un indice.
      (ver @link(TInformacionParametroIndice). Es una clase envoltura de
      TObjectList. Esto fue hecho asi para poder realizar chequeo de tipos de
      los elementos almacenados y aprovechar la funcionalidad provista por
      TObjectList).
      @abstract(Lista de tuplas extendidas de informacion sobre parametros.) }
  TListaInformacionParametroIndice = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad @link(Tuplas). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TInformacionParametroIndice;
    {** Metodo de escritura de la propiedad @link(Tuplas). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TInformacionParametroIndice);
  public
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    function Add(Item: TInformacionParametroIndice): Integer;
    {** Busca un elemento dentro de la lista de acuerdo a su nombre.
        @param(Nombre Nombre del par a buscar.)
        @returns(Devuelve el indice del par en caso de encontrarlo, o -1 en
        caso contrario.) }
    function IndexOf(Nombre: String): Integer;
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    procedure Insert(Index: Integer; Item: TInformacionParametroIndice);
    {** Pares (Nombre, Parametro) almacenados. }
    property Tuplas [Index: Integer]: TInformacionParametroIndice read GetItem write SetItem; default;
  end { TListaInformacionParametroIndice };

  {** Arreglo dinamico de tuplas de informacion de parametros. Funciona
      como un arreglo dinamico de @link(TInformacionParametroIndice).
      @abstract(Arreglo dinamico de @link(TInformacionParametroIndice).) }
  TArregloParametros = class
  protected
    {** Arreglo de informacion de parametros. }
    FInformacionParametros: array of TInformacionParametroIndice;
    {** Valores actuales de los parametos. }
    FValores: TUaVector;

    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo.
        @returns(El mayor indice del arreglo.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo arreglo.
        @returns(El menor indice del arreglo.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(CotasInferiores). Devuelve la
        cota inferior de a un parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(Cota inferior del parametro.) }
    function GetCotaInferior(const Indice: Integer): TValorParametro;
    {** Metodo de lectura de la propiedad @link(CotasSuperiores). Devuelve la
        cota superior de a un parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(Cota inferior del parametro.) }
    function GetCotaSuperior(const Indice: Integer): TValorParametro;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension (longitud) del arreglo, o lo que es lo mismo, la cantidad de
        parametros almacenados.
        @returns(Dimension del arreglo.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Indices). Devuelve el indice
        a la estructura de valores de a un parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(Indice a la estructura de valores.) }
    function GetIndice(const Indice: Integer): Integer;
    {** Metodo de lectura de la propiedad @link(InformacionParametros).
        Devuelve la tupla asociada a un indice.
        @param(Indice Indice de la tupla que deseamos acceder.)
        @returns(Tupla de informacion de parametro asociada al indice.) }
    function GetInformacion(const Indice: Integer): TInformacionParametroIndice;
    {** Metodo de lectura de la propiedad @link(Nombres). Devuelve el nombre del
        parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(Nombre del parametro asociado al indice.) }
    function GetNombre(const Indice: Integer): String;
    {** Metodo de lectura de la propiedad @link(Pasos). Devuelve la
        el paso de busqueda de a un parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(Paso de busqueda en grilla del parametro.) }
    function GetPaso(const Indice: Integer): TValorParametro;
    {** Metodo de lectura de la propiedad @link(ValoresActuales). Devuelve el
        valor del parametro asociado a un indice. Nota que se utiliza el indice
        almacenado en la tupla para acceder al vector de valores actuales.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(El valor del parametro asociado al indice.) }
    function GetValorActual(const Indice: Integer): TValorParametro;
    {** Metodo de lectura de la propiedad @link(Valores). Devuelve el valor
        del vector de valores y por consiguiente el valor actual de todos los
        parametros.
        @returns(Vector con los valores de los parametros.) }
    function GetValores: TUaVector;
    {** Metodo de lectura de la propiedad @link(ValoresIniciales). Devuelve el
        valor inicial del parametro asociado a un indice.
        @param(Indice Indice del parametro que deseamos acceder.)
        @returns(El valor del parametro asociado al indice.) }
    function GetValorInicial(const Indice: Integer): TValorParametro;
    {** Metdodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo.
        @returns(Representacion textual del arreglo de pares.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(CotasInferiores). Actualiza
        la cota inferior de un parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(CotaInferior Nuevo valor la cota inferior de la tupla.) }
    procedure SetCotaInferior(const Indice: Integer; const CotaInferior: TValorParametro);
    {** Metodo de escritura de la propiedad @link(CotasSuperiores). Actualiza
        la cota superior de un parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(CotaInferior Nuevo valor la cota superior de la tupla.) }
    procedure SetCotaSuperior(const Indice: Integer; const CotaSuperior: TValorParametro);
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el arreglo.
        @param(ADimension Nueva dimension del arreglo.) }
    procedure SetDimension(const ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Indices). Actualiza
        el indice a la estructura de valores de un parametro asociado a un
        indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(ValorIndice Nuevo valor para el indice de la de la tupla.) }
    procedure SetIndice(const Indice, ValorIndice: Integer);
    {** Metodo de escritura de la propiedad @link(InformacionParametros).
        Actualiza el valor de una tupla asociada a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(Informacion Nuevo valor de la tupla.) }
    procedure SetInformacion(const Indice: Integer; const Informacion: TInformacionParametroIndice);
    {** Metodo de escritura de la propiedad @link(Nombres). Actualiza el
        nombre del parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(Nombre Nuevo valor del nombre del parametro.) }
    procedure SetNombre(const Indice: Integer; const Nombre: String);
    {** Metodo de escritura de la propiedad @link(Pasos). Actualiza
        el paso de busqueda en grilla de un parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(Paso Nuevo valor del paso de busqueda de la tupla.) }
    procedure SetPaso(const Indice: Integer; const Paso: TValorParametro);
    {** Metodo de escritura de la propiedad @link(ValoresActuales). Actualiza el
        valor de un parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(Valor Nuevo valor del parametro.) }
    procedure SetValorActual(const Indice: Integer; const ValorActual: TValorParametro);
    {** Metodo de escritura de la propiedad @link(Valores). Actualiza el valor
        del vector de valores y por consiguiente el valor actual de todos los
        parametros.
        @param(AValores Vector con los valores de los parametros.) }
    procedure SetValores(const AValores: TUaVector);
    {** Metodo de escritura de la propiedad @link(ValoresIniciales). Actualiza
        el valor inicial de un parametro asociado a un indice.
        @param(Indice Indice de la tupla que deseamos actualizar.)
        @param(Valor Nuevo valor del parametro.) }
    procedure SetValorInicial(const Indice: Integer; const ValorInicial: TValorParametro);
  public
    {** Constructor. Crea un arreglo de una dimension dada.
        @param(ADimension Dimension inicial del arreglo.) }
    constructor Create(const ADimension: Integer); overload;
    {** Constructor. Crea un arreglo de una dimension e inicializa cada entrada
        a un valor de parametro establecido.
        @param(ADimension Dimension inicial del arreglo.)
        @param(ValorInicial Valor inicial de los parametros almacenados.) }
    constructor Create(const ADimension: Integer; const ValorInicial: TValorParametro); overload;
    {** Destructor. }
    destructor Destroy; override;
    {** Concatena el arreglo referenciado con el parametro.
        @param(Arreglo Arreglo a concatenar).
        @returns(El resultado de concatenar el arreglo referenciadon con el
        parametro.) }
    function Concatenar(const Arreglo: TArregloParametros): TArregloParametros;
    {** Devuelve el indice de la tupla que contiene un nombre.
        @param(Nombre Nombre de la tupla de la cual deseamos conocer el indice.)
        @returns(Indice de la tupla con el nombre en caso de encontrarlo, o -1
        en caso contrario.) }
    function IndiceDe(const Nombre: String): Integer;
    {** Similar a la funcion @link(Concatenar) pero almacena el resultado en el
        arreglo referenciado.
        @param(Arreglo Arreglo a concatenar). }
    procedure ConcatenarA(const Arreglo: TArregloParametros);
    {** Valida el vector de parametros verificando si los valores de los mismos
        se encuentran dentro de las cotas especificadas.
        @param(Bitacora Registro de posibles errores y advertencias producidas
        durante el proceso de validacion.)
        @param(VerificarCotasInferiores Indica si se debe verificar que los
        valores actuales de los parametros son mayores que las cotas
        inferiores.)
        @param(VerificarCotasSuperiores Indica si se debe verificar que los
        valores actuales de los parametros son menores que las cotas
        superiores.)
        @returns(Devuelve @true si los valores del arreglo estan dentro de las
        cotas especificadas o @false en caso contrario.) }
    function Validar(var Bitacora: TStrings; const VerificarCotasInferiores: Boolean; const VerificarCotasSuperiores: Boolean): Boolean; virtual;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Cotas inferiores de los parametros almacenados. }
    property CotasInferiores [const Indice: Integer]: TValorParametro read GetCotaInferior write SetCotaInferior;
    {** Cotas superiores de los parametros almacenados. }
    property CotasSuperiores [const Indice: Integer]: TValorParametro read GetCotaSuperior write SetCotaSuperior;
    {** Dimension del arreglo. }
    property Dimension: Integer read GetDimension;
    {** Indices (de las tuplas) de los parametros almacenados. }
    property Indices [const Indice: Integer]: Integer read GetIndice write SetIndice;
    {** Tuplas de informacion de parametros almacenadas. }
    property InformacionParametros [const Indice: Integer]: TInformacionParametroIndice read GetInformacion write SetInformacion;
    {** Nombres de los pares almacenados. }
    property Nombres [const Indice: Integer]: String read GetNombre write SetNombre;
    {** Pasos de busqueda en grilla de los parametros almacenados.}
    property Pasos [const Indice: Integer]: TValorParametro read GetPaso write SetPaso;
    {** Representacion textual del arreglo de tuplas. }
    property Texto: String read GetTexto;
    {** Vector de valores actuales de parametros. }
    property Valores: TUaVector read GetValores write SetValores;
    {** Valores actuales de los parametros de acuerdo a la configuracion. }
    property ValoresActuales [const Indice: Integer]: TValorParametro read GetValorActual write SetValorActual; default;
    {** Valores iniciales de los parametros almacenados. }
    property ValoresIniciales [const Indice: Integer]: TValorParametro read GetValorInicial write SetValorInicial;
  end { TArregloParametros };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TInformacionParametro
// -----------------------------------------------------------------------------

function TInformacionParametro.GetTexto: String;
begin { TInformacionParametro.GetTexto }
  Result := '(' + FNombre + ', ' + FloatToStr(FValorInicial) + ', ';
  if not FTieneCotaInferior then
    Result := Result + '*';
  Result := Result + FloatToStr(FCotaInferior) + ', ';
  if not FTieneCotaSuperior then
    Result := Result + '*';
  Result := Result + FloatToStr(FCotaSuperior) + ', ';
  if not FTienePaso then
    Result := Result + '*';
  Result := Result + FloatToStr(FPaso) + ')';
end { TInformacionParametro.GetTexto };

constructor TInformacionParametro.Create;
begin { TInformacionParametro.Create }
  Create('', 0.0, 0.0, 0.0, 0.0, False, False, False, False);
end { TInformacionParametro.Create };

constructor TInformacionParametro.Create(const AValorInicial: TValorParametro);
begin { TInformacionParametro.Create }
  Create('', AValorInicial, 0.0, 0.0, 0.0, True, False, False, False);
end { TInformacionParametro.Create };

constructor TInformacionParametro.Create(const ANombre: String; const AValorInicial: TValorParametro);
begin { TInformacionParametro.Create }
  Create(ANombre, AValorInicial, 0.0, 0.0, 0.0, True, False, False, False);
end { TInformacionParametro.Create };

constructor TInformacionParametro.Create(const ANombre: String; const AValorInicial: TValorParametro; const ACotaInferior, ACotaSuperior, APaso: TValorParametro);
begin { TInformacionParametro.Create }
  Create(ANombre, AValorInicial, ACotaInferior, ACotaSuperior, APaso, True, True, True, True);
end { TInformacionParametro.Create };

constructor TInformacionParametro.Create(const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro; const ATieneValorInicial, ATieneCotaInferior, ATieneCotaSuperior, ATienePaso: Boolean);
begin { TInformacionParametro.Create }
  FNombre := ANombre;
  FValorInicial := AValorInicial;
  FCotaInferior := ACotaInferior;
  FCotaSuperior := ACotaSuperior;
  FPaso := APaso;
  FTieneCotaInferior := ATieneCotaInferior;
  FTieneCotaSuperior := ATieneCotaSuperior;
  FTienePaso := ATienePaso;
  FTieneValorInicial := ATieneValorInicial;
end { TInformacionParametro.Create };

constructor TInformacionParametro.Create(const Informacion: TInformacionParametro);
begin { TInformacionParametro.Create }
  Assert(Assigned(Informacion), 'TInformacionParametro.Create: Assigned(Informacion)');

  Create(Informacion.FNombre, Informacion.FValorInicial, Informacion.FCotaInferior, Informacion.FCotaSuperior, Informacion.FPaso, Informacion.FTieneValorInicial, Informacion.FTieneCotaInferior, Informacion.FTieneCotaSuperior, Informacion.FTienePaso);
end { TInformacionParametro.Create };

procedure TInformacionParametro.Asignar(const Informacion: TInformacionParametro);
begin { TInformacionParametro.Asignar }
  Assert(Assigned(Informacion), 'TInformacionParametro.Asignar: Assigned(Informacion)');

  FCotaInferior := Informacion.FCotaInferior;
  FCotaSuperior := Informacion.FCotaSuperior;
  FNombre := Informacion.FNombre;
  FPaso := Informacion.FPaso;
  FTieneValorInicial := Informacion.FTieneValorInicial;
  FTieneCotaInferior := Informacion.FTieneCotaInferior;
  FTieneCotaSuperior := Informacion.FTieneCotaSuperior;
  FTienePaso := Informacion.FTienePaso;
  FValorInicial := Informacion.FValorInicial;
end { TInformacionParametro.Asignar };

function TInformacionParametro.Clonar: TInformacionParametro;
begin { TInformacionParametro.Clonar }
  Result := TInformacionParametro.Create(Self);
end { TInformacionParametro.Clonar };

function TInformacionParametro.IgualA(const Informacion: TInformacionParametro): Boolean;
begin { TInformacionParametro.IgualA }
  Assert(Assigned(Informacion), 'TInformacionParametro.IgualA: Assigned(Informacion)');

  Result := FNombre = Informacion.FNombre;
end { TInformacionParametro.IgualA };

// -----------------------------------------------------------------------------
// TInformacionParametroIndice
// -----------------------------------------------------------------------------

function TInformacionParametroIndice.GetTexto: String;
begin { TInformacionParametroIndice.GetTexto }
  Result := '(' + IntToStr(FIndice) + ', ' + FNombre + ', ' + FloatToStr(FValorInicial) + ', ';
  if not FTieneCotaInferior then
    Result := Result + '*';
  Result := Result + FloatToStr(FCotaInferior) + ', ';
  if not FTieneCotaSuperior then
    Result := Result + '*';
  Result := Result + FloatToStr(FCotaSuperior) + ', ';
  if not FTienePaso then
    Result := Result + '*';
  Result := Result + FloatToStr(FPaso) + ')';
end { TInformacionParametroIndice.GetTexto };

constructor TInformacionParametroIndice.Create;
begin { TInformacionParametroIndice }
  inherited Create;
  FIndice := -1;
end { TInformacionParametroIndice };

constructor TInformacionParametroIndice.Create(const AValorInicial: TValorParametro);
begin { TInformacionParametroIndice.Create }
  inherited Create(AValorInicial);
  FIndice := -1;
end { TInformacionParametroIndice.Create };

constructor TInformacionParametroIndice.Create(const AIndice: Integer; const AValorInicial: TValorParametro);
begin { TInformacionParametroIndice.Create }
  inherited Create(AValorInicial);
  FIndice := AIndice;
end { TInformacionParametroIndice.Create };

constructor TInformacionParametroIndice.Create(const AIndice: Integer; const ANombre: String; const AValorInicial: TValorParametro);
begin { TInformacionParametroIndice.Create }
  inherited Create(ANombre, AValorInicial);
  FIndice := AIndice;
end { TInformacionParametroIndice.Create };

constructor TInformacionParametroIndice.Create(const AIndice: Integer; const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro);
begin { TInformacionParametroIndice.Create }
  inherited Create(ANombre, AValorInicial, ACotaInferior, ACotaSuperior, APaso);
  FIndice := AIndice;
end { TInformacionParametroIndice.Create };

constructor TInformacionParametroIndice.Create(const AIndice: Integer; const ANombre: String; const AValorInicial, ACotaInferior, ACotaSuperior, APaso: TValorParametro; const ATieneValorInicial, ATieneCotaInferior, ATieneCotaSuperior, ATienePaso: Boolean);
begin { TInformacionParametroIndice.Create }
  inherited Create(ANombre, AValorInicial, ACotaInferior, ACotaSuperior, APaso, ATieneValorInicial, ATieneCotaInferior, ATieneCotaSuperior, ATienePaso);
  FIndice := AIndice;
end { TInformacionParametroIndice.Create };

constructor TInformacionParametroIndice.Create(const AIndice: Integer; const AInformacion: TInformacionParametro);
begin { TInformacionParametroIndice.Create }
  inherited Create(AInformacion);
  FIndice := AIndice;
end { TInformacionParametroIndice.Create };

procedure TInformacionParametroIndice.Asignar(const Informacion: TInformacionParametroIndice);
begin { TInformacionParametroIndice.Asignar }
  Assert(Assigned(Informacion), 'TInformacionParametroIndice.Asignar: Assigned(Informacion)');

  inherited Asignar(Informacion);
  FIndice := Informacion.FIndice;
end { TInformacionParametroIndice.Asignar };

function TInformacionParametroIndice.Clonar: TInformacionParametroIndice;
begin { TInformacionParametroIndice.Clonar }
  Result := TInformacionParametroIndice.Create(Self);    
end { TInformacionParametroIndice.Clonar };

// -----------------------------------------------------------------------------
// TListaInformacionParametroIndice
// -----------------------------------------------------------------------------

function TListaInformacionParametroIndice.GetItem(Index: Integer): TInformacionParametroIndice;
begin { TListaInformacionParametroIndice.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionParametroIndice.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TInformacionParametroIndice;
end { TListaInformacionParametroIndice.GetItem };

procedure TListaInformacionParametroIndice.SetItem(Index: Integer; Item: TInformacionParametroIndice);
begin { TListaInformacionParametroIndice.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionParametroIndice.SetItem: Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionParametroIndice.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaInformacionParametroIndice.SetItem };

destructor TListaInformacionParametroIndice.Destroy;
begin { TListaInformacionParametroIndice.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaInformacionParametroIndice.Destroy };

function TListaInformacionParametroIndice.Add(Item: TInformacionParametroIndice): Integer;
begin { TListaInformacionParametroIndice.Add }
  Assert(Assigned(Item), 'TListaInformacionParametroIndice.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaInformacionParametroIndice.Add };

function TListaInformacionParametroIndice.IndexOf(Nombre: String): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TListaInformacionParametroIndice.IndexOf }
  I := 0;
  Result := -1;
  Listo := false;

  while (I <= Count - 1) and (not Listo) do
  begin
    if (GetItem (I) as TInformacionParametroIndice).Nombre = Nombre then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TListaInformacionParametroIndice.IndexOf };

procedure TListaInformacionParametroIndice.Insert(Index: Integer; Item: TInformacionParametroIndice);
begin { TListaInformacionParametroIndice.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaInformacionParametroIndice.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaInformacionParametroIndice.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaInformacionParametroIndice.Insert };

// -----------------------------------------------------------------------------
// TArregloParametros
// -----------------------------------------------------------------------------

function TArregloParametros.GetAlto: Integer;
begin { TArregloParametros.GetAlto }
  Result := High(FInformacionParametros);
end { TArregloParametros.GetAlto };

function TArregloParametros.GetBajo: Integer;
begin { TArregloParametros.GetBajo }
  Result := Low(FInformacionParametros);
end { TArregloParametros.GetBajo };

function TArregloParametros.GetCotaInferior(const Indice: Integer): TValorParametro;
begin { TArregloParametros.GetCotaInferior }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetCotaInferior: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].CotaInferior;
end { TArregloParametros.GetCotaInferior };

function TArregloParametros.GetCotaSuperior(const Indice: Integer): TValorParametro;
begin { TArregloParametros.GetCotaSuperior }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetCotaSuperior: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].CotaSuperior;
end { TArregloParametros.GetCotaSuperior };

function TArregloParametros.GetDimension: Integer;
begin { TArregloParametros.GetDimension }
  Result := Length(FInformacionParametros);
end { TArregloParametros.GetDimension };

function TArregloParametros.GetIndice(const Indice: Integer): Integer;
begin { TArregloParametros.GetIndice }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetIndice: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].Indice;
end { TArregloParametros.GetIndice };

function TArregloParametros.GetInformacion(const Indice: Integer): TInformacionParametroIndice;
begin { TArregloParametros.GetInformacion }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetInformacion: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)');

  Result := FInformacionParametros [Indice];
end { TArregloParametros.GetInformacion };

function TArregloParametros.GetNombre(const Indice: Integer): String;
begin { TArregloParametros.GetNombre }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetNombre: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].Nombre;
end { TArregloParametros.GetNombre };

function TArregloParametros.GetPaso(const Indice: Integer): TValorParametro;
begin { TArregloParametros.GetPaso }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetPaso: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].Paso;
end { TArregloParametros.GetPaso };

function TArregloParametros.GetValorActual(const Indice: Integer): TValorParametro;
begin { TArregloParametros.GetValorActual }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetValorActual: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');
  Assert(Assigned(FValores), 'TArregloParametros.GetValorActual: Assigned(FValores)');

  Result := FValores [FInformacionParametros [Indice].Indice];
end { TArregloParametros.GetValorActual };

function TArregloParametros.GetValores: TUaVector;
begin { TArregloParametros.GetValores }
  Result := FValores;
end { TArregloParametros.GetValores };

function TArregloParametros.GetValorInicial(const Indice: Integer): TValorParametro;
begin { TArregloParametros.GetValorInicial }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.GetValorInicial: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  Result := FInformacionParametros [Indice].ValorInicial;
end { TArregloParametros.GetValorInicial };

function TArregloParametros.GetTexto: String;
var
  I: Integer;
begin { TArregloParametros.GetTexto }
  Result := '[';
  for I := Low(FInformacionParametros) to (High(FInformacionParametros) - 1) do
  begin
    Result := Result + '(' + IntToStr(I) + ', ' + FInformacionParametros [I].Texto;
    if Assigned(FValores) then
      Result := Result + ', ' + FloatToStr(FValores [FInformacionParametros [I].Indice]);
    Result := Result + '), '
  end;
  Result := Result + '(' + IntToStr(High(FInformacionParametros)) + ', ' + FInformacionParametros [High(FInformacionParametros)].Texto;
  if Assigned(FValores) then
    Result := Result + ', ' + FloatToStr(FValores [FInformacionParametros [High(FInformacionParametros)].Indice]);
  Result := Result + ')]';
end { TArregloParametros.GetTexto };

procedure TArregloParametros.SetCotaInferior(const Indice: Integer; const CotaInferior: TValorParametro);
begin { TArregloParametros.SetCotaInferior }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetCotaInferior: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].CotaInferior := CotaInferior;
end { TArregloParametros.SetCotaInferior };

procedure TArregloParametros.SetCotaSuperior(const Indice: Integer; const CotaSuperior: TValorParametro);
begin { TArregloParametros.SetCotaSuperior }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetCotaSuperior: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].CotaSuperior := CotaSuperior;
end { TArregloParametros.SetCotaSuperior };

procedure TArregloParametros.SetDimension(const ADimension: Integer);
var
  I, DimensionAnterior: Integer;
begin { TArregloParametros.SetDimension }
  Assert(ADimension > 0, 'TArregloInformacionParametroIndice.SetDimension: ADimension > 0');

  DimensionAnterior := Length(FInformacionParametros);

  if DimensionAnterior > Length(FInformacionParametros) then
  begin
    for I := DimensionAnterior to High(FInformacionParametros) do
      FreeAndNil(FInformacionParametros [I]);
    SetLength(FInformacionParametros, ADimension);
  end
  else if DimensionAnterior < Length(FInformacionParametros) then
  begin
    SetLength(FInformacionParametros, ADimension);

    for I := DimensionAnterior to High(FInformacionParametros) do
    begin
      FInformacionParametros [I] := TInformacionParametroIndice.Create;
      FInformacionParametros [I].Indice := I;
    end;
  end;
end { TArregloInformacionParametroIndice.SetDimension };

procedure TArregloParametros.SetIndice(const Indice, ValorIndice: Integer);
begin { TArregloParametros.SetIndice }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetIndice: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].Indice := ValorIndice;
end { TArregloParametros.SetIndice };

procedure TArregloParametros.SetInformacion(const Indice: Integer; const Informacion: TInformacionParametroIndice);
begin { TArregloParametros.SetInformacion }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetInformacion: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');
  Assert(Assigned(Informacion), 'TArregloParametros.SetInformacion: Assigned(Informacion)');

  FInformacionParametros [Indice] := Informacion;
end { TArregloParametros.SetInformacion };

procedure TArregloParametros.SetNombre(const Indice: Integer; const Nombre: String);
begin { TArregloParametros.SetNombre }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetNombre: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].Nombre := Nombre;
end { TArregloParametros.SetNombre };

procedure TArregloParametros.SetPaso(const Indice: Integer; const Paso: TValorParametro);
begin { TArregloParametros.SetNombre }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetPaso: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].Paso := Paso;
end { TArregloParametros.SetPaso };

procedure TArregloParametros.SetValorActual(const Indice: Integer; const ValorActual: TValorParametro);
begin { TArregloParametros.SetValorActual }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetValorActual: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');
  Assert(Assigned(FValores), 'TArregloParametros.SetValorActual: Assigned(FValores)');

  FValores [FInformacionParametros [Indice].Indice] := ValorActual;
end { TArregloParametros.SetValorActual };

procedure TArregloParametros.SetValores(const AValores: TUaVector);
begin { TArregloParametros.SetValores }
  FValores := AValores;
end { TArregloParametros.SetValores };

procedure TArregloParametros.SetValorInicial(const Indice: Integer; const ValorInicial: TValorParametro);
begin { TArregloParametros.SetValorInicial }
  Assert((Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros)), 'TArregloParametros.SetValorInicial: (Indice >= Low(FInformacionParametros)) and (Indice <= High(FInformacionParametros))');

  FInformacionParametros [Indice].ValorInicial := ValorInicial;
end { TArregloParametros.SetValorInicial };

constructor TArregloParametros.Create(const ADimension: Integer);
var
  I: Integer;
begin { TArregloInformacionParametroIndice.Create }
  Assert(ADimension > 0, 'TArregloParametros.Create: ADimension > 0');

  SetLength(FInformacionParametros, ADimension);
  for I := Low(FInformacionParametros) to High(FInformacionParametros) do
  begin
    FInformacionParametros [I] := TInformacionParametroIndice.Create;
    FInformacionParametros [I].Indice := I;
  end;

  FValores := nil;
end { TArregloParametros.Create };

constructor TArregloParametros.Create(const ADimension: Integer; const ValorInicial: TValorParametro);
var
  I: Integer;
begin { TArregloInformacionParametroIndice.Create }
  Assert(ADimension > 0, 'TArregloParametros.Create: ADimension > 0');

  SetLength(FInformacionParametros, ADimension);
  for I := Low(FInformacionParametros) to High(FInformacionParametros) do
    FInformacionParametros [I] := TInformacionParametroIndice.Create(I, ValorInicial);

  FValores := nil;
end { TArregloParametros.Create };

destructor TArregloParametros.Destroy;
var
  I: Integer;
begin { TArregloParametros.Destroy }
  try
    for I := Low(FInformacionParametros) to High(FInformacionParametros) do
      FreeAndNil(FInformacionParametros [I]);
    FInformacionParametros := nil;
  finally
    inherited Destroy;
  end;
end { TArregloParametros.Destroy };

function TArregloParametros.Concatenar(const Arreglo: TArregloParametros): TArregloParametros;
var
  I, J: Integer;
begin { TArregloParametros.Concatenar }
  Assert(Assigned(Arreglo), 'TArregloParametros.Concatenar: Assigned(Arreglo)');

  Result := TArregloParametros.Create(Length(FInformacionParametros) + Length(Arreglo.FInformacionParametros));

  J := Low(Result.FInformacionParametros);
  for I := Low(FInformacionParametros) to High(FInformacionParametros) do
  begin
    Result.FInformacionParametros [J] := FInformacionParametros [I].Clonar;
    Inc(J);
  end;

  for I := Low(Arreglo.FInformacionParametros) to High(Arreglo.FInformacionParametros) do
  begin
    Result.FInformacionParametros [J] := Arreglo.FInformacionParametros [I].Clonar;
    Inc(J);
  end;
end { TArregloParametros.Concatenar };

function TArregloParametros.IndiceDe(const Nombre: String): Integer;
var
  I: Integer;
  Listo: Boolean;
begin { TArregloParametros.IndiceDe }
  I := Low(FInformacionParametros);
  Result := -1;
  Listo := false;

  while (I <= High(FInformacionParametros)) and (not Listo) do
  begin
    if FInformacionParametros [I].Nombre = Nombre then
    begin
      Result := I;
      Listo := true;
    end;

    Inc(I);
  end;
end { TArregloParametros.IndiceDe };

procedure TArregloParametros.ConcatenarA(const Arreglo: TArregloParametros);
var
  I, J: Integer;
begin { TArregloParametros.ConcatenarA }
  Assert(Assigned(Arreglo), 'TArregloParametros.ConcatenarA: Assigned(Arreglo)');

  J := High(FInformacionParametros) + 1;

  SetDimension(Length(FInformacionParametros) + Length(Arreglo.FInformacionParametros));

  for I := Low(Arreglo.FInformacionParametros) to High(Arreglo.FInformacionParametros) do
  begin
    FInformacionParametros [J] := Arreglo.FInformacionParametros [I].Clonar;
    Inc(J);
  end;
end { TArregloParametros.ConcatenarA };

function TArregloParametros.Validar(var Bitacora: TStrings; const VerificarCotasInferiores: Boolean; const VerificarCotasSuperiores: Boolean): Boolean;
var
  I: Integer;
begin { TArregloParametros.Validar }
  Assert(Assigned(Bitacora), 'TArregloParametros.Validar: Assigned(Bitacora)');

  Result := True;

  if VerificarCotasInferiores then
  begin
    I := Low(FInformacionParametros);

    while (I <= High(FInformacionParametros)) and Result do
    begin
      if (FInformacionParametros [I].TieneCotaInferior) and
         (FValores [FInformacionParametros [I].Indice] < FInformacionParametros [I].CotaInferior) then
      begin
        Bitacora.Add('El valor del parametro ' + IntToStr(I) + ' ("' + FInformacionParametros [I].Nombre + '") ' + FloatToStr(FValores [FInformacionParametros [I].Indice]) + ' viola su cota inferior: ' + FInformacionParametros [I].Texto);
        Result := False;
      end;

      Inc(I);
    end;
  end;

  if Result and VerificarCotasSuperiores then
  begin
    I := Low(FInformacionParametros);

    while (I <= High(FInformacionParametros)) and Result do
    begin
      if (FInformacionParametros [I].TieneCotaSuperior) and
         (FValores [FInformacionParametros [I].Indice] > FInformacionParametros [I].CotaSuperior) then
      begin
        Bitacora.Add('El parametro "' + FInformacionParametros [I].Nombre + '" (' + IntToStr(I) + ') viola su cota superior: ' + FInformacionParametros [I].Texto);
        Result := False;
      end;

      Inc(I);
    end;
  end;
end { TArregloParametros.Validar };

end { UnitParametros }.

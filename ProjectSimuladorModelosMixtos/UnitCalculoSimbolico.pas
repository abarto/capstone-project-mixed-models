{**
@abstract(Modulo de calculo simbolico.)
@author(Agustin Barto <abarto@gmail.com>)
@created(March 1, 2005)
@lastmod(March 10, 2005)
Este modulo contiene definiciones de clase, metodos y constantes utilizados
para evaluar expresiones simbolicas reales y booleanas.
}
unit UnitCalculoSimbolico;

{$WARNINGS OFF}

interface

uses
  Contnrs, Variants,
  UaComun, UaGeneradorNumerosAleatorios;

type
  {** Tipo basico de valor de expresiones reales.}
  TCsValor = TUaReal;

  {** Tipo basico de valor de expresiones enteras.}
  TCsValorEntero = Integer;

  {** Etiquetas (identificadores) de simbolos. Utilizo un ShortString para
      poder utilizar el tipo dentre de un record en el parser. }
  TCsEtiqueta = ShortString;

  {** Ubicacion de operadores (reales o booleanos). }
  TCsUbicacionOperador = (
    {** Operador infijo. Ej: a + b. }
    Infijo,
    {** Operador sufijo. Ej: a++. }
    Sufijo,
    {** Operador prefijo. Ej: -a. }
    Prefijo,
    {** Operador funciona. Ej: f(a, b). }
    Funcional
  );

  {** Almacenamiento de valores de variables. Se utiliza para dar una
      interpretacion a las variables, identificadas con un numero entero
      (indice).
      @abstract(Almacenamiento de valores de variables.) }
  TCsContexto = class
  private
    {** Valores de variables propiamente dichos. }
    FValores: array of TCsValor;

    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo.
        @returns(Mayor indice del arreglo.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo.
        @returns(Menor indice del arreglo.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del alamacenamiento.
        @returns(Representacion de cadena de caracteres del objeto.) }
    function GetTexto: String;
    {** Metodo de lectura de la propiedad @link(Valores). Devuelve el valor
        real almacenado bajo un indice, asociado a la variable.
        @param(Indice Indice de la variable a acceder.)
        @returns(Valor actual de la variable indexada.) }
    function GetValor(const Indice: Integer): TCsValor;
    {** Metodo de escritura de la propiedad @link(Valores). Actualiza el valor
        real almacenado bajo un indice, asociado a la variable.
        @param(Indice Indice de la variable a actualizar.)
        @param(Valor Nuevo valor de la variable.) }
    procedure SetValor(const Indice: Integer; const Valor: TCsValor);
  public
    {** Constructor.
        @param(Dimension Dimension (cantidad de variables almacenadas) del
        contexto.) }
    constructor Create(Dimension: Integer);
    {** Destructor. }
    destructor Destroy; override;
    {** Mayor indice de variable del contexto. }
    property Alto: Integer read GetAlto;
    {** Menor indice de variable del contexto. }
    property Bajo: Integer read GetBajo;
    {** Representacion de cadena de caracteres del contexto. }
    property Texto: String read GetTexto;
    {** Valores de variables. }
    property Valores [const Indice: Integer]: TCsValor read GetValor write SetValor; default;
  end { TCsContexto };

  {** Unidad basica del calculo simbolico de expresiones reales. Indica las
      propiedades basicas a cumplir por el resto de las expresiones.
      @abstract(Unidad basica del calculo simbolico de expresiones reales.) }
  TCsExpresion = class
  protected
    {** Contexto de evaluacion. }
    FContexto: TCsContexto;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres de la expresion.
        @returns(Representacion de cadena de caracteres de la expresion.) }
    function GetTexto: String; virtual; abstract;
    {** Metodo de lectura de la propiedad @link(Valor). Este metodo es el
        centro del sistema de calculo simbolico, dado que es el encargado de
        evaluar las expresiones.
        @returns(Valor de la expresion.) }
    function GetValor: TCsValor; virtual; abstract;
    {** Metodo de escritura de la propiedad @link(Contexto). Actualiza el
        valor del contexto de evaluacion de la expresion.
        @param(AContexto Nuevo valor del contexto.) }
    procedure SetContexto(const AContexto: TCsContexto); virtual; abstract;
  public
    {** Constructor.
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AContexto: TCsContexto);
    {** Contexto de evaluacion. }
    property Contexto: TCsContexto read FContexto write SetContexto;
    {** Valor de la expresion. }
    property Valor: TCsValor read GetValor;
    {** Representacion como cadena de caracteres de la expresion. }
    property Texto: String read GetTexto;
  end { TCsExpresion };

  {** Arreglo dinamico de @link(TCsExpresion). Es una clase envoltura de un
      array of @link(TCsExpresion). Se utiliza una clase envolvente para
      facilitar la manipulacion de los arreglos dinamicos y proveer de una
      interfaz consistente.
      @abstract(Arreglo dinamico de expresiones reales.) }
  TCsArregloExpresiones = class
  private
    {** El arreglo de expresiones propiamente dicho. }
    FExpresiones: array of TCsExpresion;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Expresiones). Devuelve la expresion
        almacenada bajo un indice.
        @param(I Indice de la expresion que deseamos acceder.)
        @returns(Expresion bajo el indice.) }
    function GetExpresion(const Indice: Integer): TCsExpresion;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FExpresiones).
        @param(ADimension Nueva dimension del arreglo dinamico.) }
    procedure SetDimension(const ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Expresiones). Actualiza la
        expresion almacenada bajo un indice.
        @param(I Indice de la expresion a actualizar.)
        @param(Expresion Nuevo valor para la expresion bajo el indice.) }
    procedure SetExpresion(const Indice: Integer; const Expresion: TCsExpresion);
  public
    {** Constructor. Crea una instancia con la dimension indicada.
        @param(ADimension Dimension inicial del arreglo.) }
    constructor Create(const ADimension: Integer);
    {** Destructor. }
    destructor Destroy; override;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension (o longitud) del arreglo. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Enteros almacenados en el arreglo. }
    property Expresiones [const Indice: Integer]: TCsExpresion read GetExpresion write SetExpresion; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TCsArregloExpresiones };

  {** Expresion constante. El valor de la expresion no cambia con el tiempo y
      depende de un parametro de construccion.
      @abstract(Expresion constante.) }
  TCsConstante = class(TCsExpresion)
  private
    {** Valor (constante) de la constante. }
    FValor: TCsValor;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(AValor Valor numerico de la constante.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AValor: TCsValor; const AContexto: TCsContexto = nil);
  end { TCsConstante };

  {** Expresion variable. El valor de la expresion depende de un contexto y de
      un indice asociado a la variable, por ende puede cambiar con el tiempo.
      @abstract(Expresion variable.) }
  TCsVariable = class(TCsExpresion)
  private
    {** Indice de la variable en el contexto. }
    FIndice: Integer;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(AIndice Indice de la variable en el contexto.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AIndice: Integer; const AContexto: TCsContexto = nil);
    {** Indice de la variable en el contexto. }
    property Indice: Integer read FIndice write FIndice;
  end { TCsVariable };

  {** Resta de dos expresiones. El valor de esta expresion depende del valor de
      sus dos operandos.
      @abstract(Suma de dos expresiones.) }
  TCsSuma = class(TCsExpresion)
  private
    {** Termino izquierdo de la suma. }
    FTerminoIzquierdo: TCsExpresion;
    {** Termino derecho de la suma. }
    FTerminoDerecho: TCsExpresion;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(ATerminoIzquierdo Termino izquierdo de la suma.)
        @param(ATerminoDerecho Termino derecho de la suma.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const ATerminoIzquierdo, ATerminoDerecho: TCsExpresion; const AContexto: TCsContexto = nil);
    {** Termino izquierdo de la suma. }
    property TerminoIzquierdo: TCsExpresion read FTerminoIzquierdo;
    {** Termino derecho de la suma. }
    property TerminoDerecho: TCsExpresion read FTerminoDerecho;
  end { TCsSuma };
  
  {** Resta de dos expresiones. El valor de esta expresion depende del valor de
      sus dos operandos.
      @abstract(Resta de dos expresiones.) }
  TCsResta = class(TCsExpresion)
  private
    {** Termino izquierdo de la resta. }
    FTerminoIzquierdo: TCsExpresion;
    {** Termino derecho de la resta. }
    FTerminoDerecho: TCsExpresion;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(ATerminoIzquierdo Termino izquierdo de la resta.)
        @param(ATerminoDerecho Termino derecho de la resta.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const ATerminoIzquierdo, ATerminoDerecho: TCsExpresion; const AContexto: TCsContexto = nil);
    {** Termino izquierdo de la resta. }
    property TerminoIzquierdo: TCsExpresion read FTerminoIzquierdo;
    {** Termino derecho de la resta. }
    property TerminoDerecho: TCsExpresion read FTerminoDerecho;
  end { TCsResta };

  {** Producto de dos expresiones. El valor de esta expresion depende del valor
      de sus dos operandos.
      @abstract(Producto de dos expresiones.) }
  TCsProducto = class(TCsExpresion)
  private
    {** Factor izquierdo del producto. }
    FFactorIzquierdo: TCsExpresion;
    {** Factor derecho del producto. }
    FFactorDerecho: TCsExpresion;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(AFactorIzquierdo Factor izquierdo del producto.)
        @param(AFactorDerecho Factor derecho del producto.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AFactorIzquierdo, AFactorDerecho: TCsExpresion; const AContexto: TCsContexto = nil);
    {** Factor izquierdo del producto. }
    property FactorIzquierdo: TCsExpresion read FFactorIzquierdo;
    {** Factor derecho del producto. }
    property FactorDerecho: TCsExpresion read FFactorDerecho;
  end { TCsProducto };

  {** Division entre dos expresiones. El valor de esta expresion depende del
      valor de sus operandos.
      @abstract(Division entre dos expresiones.) }
  TCsDivision = class(TCsExpresion)
  private
    {** Dividendo de la division. }
    FDividendo: TCsExpresion;
    {** Divisor de la division. }
    FDivisor: TCsExpresion;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(ADividendo Dividendo de la division.)
        @param(ADivisor Divisor de la division.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const ADividendo, ADivisor: TCsExpresion; const AContexto: TCsContexto = nil);
    {** Dividendo de la division. }
    property Dividendo: TCsExpresion read FDivisor;
    {** Divisor de la division. }
    property Divisor: TCsExpresion read FDivisor;
  end { TCsDivision };

  {** Potencia entre una base y un exponente. El valor de esta expresion
      depende del valor de sus parametros.
      @abstract(Potencia entre una base y un exponente.) }
  TCsPotencia = class(TCsExpresion)
  private
    {** Base de la potencia. }
    FBase: TCsExpresion;
    {** Exponente de la potencia. }
    FExponente: TCsExpresion;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(ABase Base de la potencia.)
        @param(AExponente Exponente de la potenia.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const ABase, AExponente: TCsExpresion; const AContexto: TCsContexto = nil);
    {** Base de la potencia. }
    property Base: TCsExpresion read FBase;
    {** Exponente de la potencia. }
    property Exponente: TCsExpresion read FExponente;
  end { TCsPotencia };

  {** Tipo de metodos que implementa una funcion real sobre valores de
      expresiones reales. }
  TCsMetodoFuncion = function (const Parametros: Variant): TCsValor;

  {** Registro que agrupa las propiedades que describen a una funcion u
      operador real, sobre valores de expresiones reales.
      @abstract(Registro descriptor de funcion.) }
  TCsFuncion = record
    {** Aridad de la funcion. }
    Aridad: Integer;
    {** Etiqueta (simbolo) que identifica al operador o funcion. }
    Etiqueta: TCsEtiqueta;
    {** Ubicacion del simbolo. }
    Ubicacion: TCsUbicacionOperador;
    {** Metodo que implementa al operador o funcion. }
    Metodo: TCsMetodoFuncion;
  end { TCsFuncion };

  {** Aplicacion de una funcion real de valores reales a un conjunto de
      expresiones reales. El valor de esta expresion depende tanto del valor de
      sus parametros (otras expresiones reales) como del metodo que implementa
      a la funcion.
      @abstract(Aplicacion de una funcion real sobre valores de expresiones
      reales.) }
  TCsAplicacionFuncion = class(TCsExpresion)
  private
    {** Informacion sobre la funcion. }
    FFuncion: TCsFuncion;
    {** Arreglo de expresiones parametros de la aplicacion. }
    FParametros: TCsArregloExpresiones;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor de una aplicacion de funcion 0-aria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AFuncion: TCsFuncion; const AContexto: TCsContexto = nil); overload;
    {** Constructor de una aplicacion de funcion unaria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(Parametro Expresion parametro de la aplicacion.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AFuncion: TCsFuncion; const Parametro: TCsExpresion; const AContexto: TCsContexto = nil); overload;
    {** Constructor de una aplicacion de funcion binaria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(OperandoIzquierdo Expresion parametro izquierdo de la
        aplicacion.)
        @param(OperandoDerecho Expresion parametro derecho de la
        aplicacion.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AFuncion: TCsFuncion; const OperandoIzquierdo, OperandoDerecho: TCsExpresion; const AContexto: TCsContexto = nil); overload;
    {** Constructor de una aplicacion de funcion n-aria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(AParametros Arreglo de expresiones parametros de la
        aplicacion.)
        @param(AContexto Contexto de evaluacion de la expresion.) }
    constructor Create(const AFuncion: TCsFuncion; const AParametros: TCsArregloExpresiones; const AContexto: TCsContexto = nil); overload;

    {** Informacion sobre la funcion. }
    property Funcion: TCsFuncion read FFuncion;
    {** Expresiones parametro de la aplicacion. }
    property Parametros: TCsArregloExpresiones read FParametros;
  end { TCsAplicacionFuncion };

  {** Expresion de variable aleatoria. El valor de la expresion depende de los
      valores producidos por un generador de observaciones de variable
      aleatoria (Ver @link(TUaVariableAleatoria).
      @abstract(Expresion variable aleatoria.) }
  TCsVariableAleatoria = class(TCsExpresion)
  private
    {** Generador de observaciones de la variable aleatoria. }
    FVariableAleatoria: TUaVariableAleatoria;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor.
        @param(AVariableAleatoria Generador de observaciones de la variable
        aleatoria.) }
    constructor Create(const AVariableAleatoria: TUaVariableAleatoria);
    {** Generador de observaciones de la variable aleatoria. }
    property VariableAleatoria: TUaVariableAleatoria read FVariableAleatoria;
  end { TCsVariableAleatoria };

  {** Expresion entera. Es sencillamente una clase envolvente para una
      expresion real. Al momento de evaluar la expresion se evalua el campo
      real y se devuelve el mayor entero menor que el valor real.
      @abstract(Expresion de valor entero.) }
  TCsExpresionEntera = class
  private
    {** Expresion real envuelta. }
    FExpresionReal: TCsExpresion;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion como cadena de caracteres de la expresion entera. La
        cadena generada es la de la expresion envuelta rodeada de corchetes.
        @returns(Representacion de cadena de caracteres de la expresion.) }
    function GetTexto: String;
    {** Evalua la expresion entera. Calcula el valor de la expresion real
        envuelta y le aplica Floor al resultado.
        @returns(Valor entero de la expresion.) }
    function GetValor: TCsValorEntero;
  public
    {** Constructor.
        @abstract(AExpresion Expresion real a envolver.) }
    constructor Create(const AExpresionReal: TCsExpresion);
    {** Valor entero de la expresion. }
    property Valor: TCsValorEntero read GetValor;
    {** Representacion de cadena de caracteres de la expresion. }
    property Texto: String read GetTexto;
    {** Expresion real envuelta. }
    property ExpresionReal: TCsExpresion read FExpresionReal;
  end { TCsExpresionEntera };

  {** Unidad basica del calculo simbolico de expresiones booleanas. Indica las
      propiedades basicas a cumplir por el resto de las expresiones.
      @abstract(Unidad basica del calculo simbolico de expresiones booleanas.) }
  TCsExpresionBooleana = class
  protected
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres de la expresion.
        @returns(Representacion de cadena de caracteres de la expresion.) }
    function GetTexto: String; virtual; abstract;
    {** Metodo de lectura de la propiedad @link(Valor). Este metodo es el
        centro del sistema de calculo simbolico, dado que es el encargado de
        evaluar las expresiones.
        @returns(Valor de la expresion.) }
    function GetValor: Boolean; virtual; abstract;
  public
    {** Modifica el contexto de las expresiones reales contenidas en la
        expresion booleana.
        @param(AContexto Nuevo valor del contexto de las expresiones reales
        contenidas dentro de la expresion entera.) }
    procedure AsignarContexto(const AContexto: TCsContexto); virtual; abstract;
    {** Valor de la expresion. }
    property Valor: Boolean read GetValor;
    {** Representacion de cadena de caracteres de la expresion. }
    property Texto: String read GetTexto;
  end { TCsExpresionBooleana };

  {** Arreglo dinamico de @link(TCsExpresionBooleana). Es una clase
      envoltura de un array of @link(TCsExpresionBooleana). Se utiliza una
      clase envolvente para facilitar la manipulacion de los arreglos
      dinamicos y proveer de una interfaz consistente.
      @abstract(Arreglo dinamico de expresiones booleanas.) }
  TCsArregloExpresionesBooleanas = class
  private
    {** El arreglo de expresiones propiamente dicho. }
    FExpresiones: array of TCsExpresionBooleana;
    {** Metodo de lectura de la propiedad @link(Alto). Devuelve el mayor indice
        del arreglo dinamico.
        @returns(El mayor indice del arreglo dinamico.) }
    function GetAlto: Integer;
    {** Metodo de lectura de la propiedad @link(Bajo). Devuelve el menor indice
        del arreglo dinamico.
        @returns(El menor indice del arreglo dinamico.) }
    function GetBajo: Integer;
    {** Metodo de lectura de la propiedad @link(Dimension). Devuelve la
        dimension de arreglo dinamico.
        @returns(Dimension (o longitud) del arreglo dinamico.) }
    function GetDimension: Integer;
    {** Metodo de lectura de la propiedad @link(Expresiones). Devuelve la expresion
        almacenada bajo un indice.
        @param(I Indice de la expresion que deseamos acceder.)
        @returns(Expresion bajo el indice.) }
    function GetExpresion(const Indice: Integer): TCsExpresionBooleana;
    {** Metodo de lectura de la propiedad @link(Texto). Genera una
        representacion de cadena de caracteres del arreglo.
        @returns(Representacion textual del arreglo.) }
    function GetTexto: String;
    {** Metodo de escritura de la propiedad @link(Dimension). Redimensiona el
        arreglo @link(FExpresiones).
        @param(ADimension Nueva dimension del arreglo dinamico.) }
    procedure SetDimension(const ADimension: Integer);
    {** Metodo de escritura de la propiedad @link(Expresiones). Actualiza la
        expresion almacenada bajo un indice.
        @param(I Indice de la expresion a actualizar.)
        @param(Expresion Nuevo valor para la expresion bajo el indice.) }
    procedure SetExpresion(const Indice: Integer; const Expresion: TCsExpresionBooleana);
  public
    {** Constructor. Crea una instancia con la dimension indicada.
        @param(ADimension Dimension inicial del arreglo.) }
    constructor Create(const ADimension: Integer);
    {** Destructor. }
    destructor Destroy; override;
    {** Mayor indice del arreglo. }
    property Alto: Integer read GetAlto;
    {** Menor indice del arreglo. }
    property Bajo: Integer read GetBajo;
    {** Dimension (o longitud) del arreglo. }
    property Dimension: Integer read GetDimension write SetDimension;
    {** Enteros almacenados en el arreglo. }
    property Expresiones [const Indice: Integer]: TCsExpresionBooleana read GetExpresion write SetExpresion; default;
    {** Representacion textual del arreglo. }
    property Texto: String read GetTexto;
  end { TCsArregloExpresionesBooleanas };

  {** Expresion booleana constante. El valor de la expresion no cambia con el
      tiempo, depende de un parametro de construccion y solo puede ser
      verdadero o falso.
      @abstract(Expresion booleana constante.) }
  TCsConstanteBooleana = class(TCsExpresionBooleana)
  private
    {** Valor (constante) de la expresion. }
    FValor: Boolean;
  protected
    {** Ver @link(TCsExpresionBooleana.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresionBooleana.GetValor). }
    function GetValor: Boolean; override;
  public
    {** Constructor.
        @param(AValor Valor de la expresion consante.) }
    constructor Create(const AValor: Boolean);
    {** Ver @link(TCsExpresionBooleana.AsignarContexto). }
    procedure AsignarContexto(const AContexto: TCsContexto); override;
  end { TCsConstanteBooleana };

  {** Negacion de una expresion booleana. El valor de esta expresion depende
      del operando.
      @abstract(Negacion de una expresion booleana.) }
  TCsNegacion = class(TCsExpresionBooleana)
  private
    {** Operando de la negacion. }
    FOperando: TCsExpresionBooleana;
  protected
    {** Ver @link(TCsExpresionBooleana.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresionBooleana.GetValor). }
    function GetValor: Boolean; override;
  public
    {** Constructor.
        @param(AOperando Operando de la negacion.) }
    constructor Create(const AOperando: TCsExpresionBooleana);
    {** Ver @link(TCsExpresionBooleana.AsignarContexto). }
    procedure AsignarContexto(const AContexto: TCsContexto); override;
    {** Operando de la negacion. }
    property Operando: TCsExpresionBooleana read FOperando;
  end { TCsNegacion };

  {** Conjuncion de dos expresiones booleanas. El valor de esta expresion
      depende de los valores de sus operandos.
      @abstract(Conjuncion de dos expresiones booleanas.) }
  TCsConjuncion = class(TCsExpresionBooleana)
  private
    {** Operando izquierdo de la conjuncion. }
    FOperandoIzquierdo: TCsExpresionBooleana;
    {** Operando derecho de la conjuncion. }
    FOperandoDerecho: TCsExpresionBooleana;
  protected
    {** Ver @link(TCsExpresionBooleana.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresionBooleana.GetValor). }
    function GetValor: Boolean; override;
  public
    {** Constructor.
        @param(AOperandoIzquierdo Operando izquierdo de la conjuncion.)
        @param(AOperandoDerecho Operando derecho de la conjuncion.) }
    constructor Create(const AOperandoIzquierdo, AOperandoDerecho: TCsExpresionBooleana);
    {** Ver @link(TCsExpresionBooleana.AsignarContexto). }
    procedure AsignarContexto(const AContexto: TCsContexto); override;
    {** Operando izquierdo de la conjuncion. }
    property OperandoIzquierdo: TCsExpresionBooleana read FOperandoIzquierdo;
    {** Operando derecho de la conjuncion. }
    property OperandoDerecho: TCsExpresionBooleana read FOperandoDerecho;
  end { TCsConjuncion };

  {** Disyuncion de dos expresiones booleanas. El valor de esta expresion
      depende de los valores de sus operandos.
      @abstract(Disyuncion de dos expresiones booleanas.) }
  TCsDisyuncion = class(TCsExpresionBooleana)
  private
    {** Operando izquierdo de la disyuncion. }
    FOperandoIzquierdo: TCsExpresionBooleana;
    {** Operando derecho de la disyuncion. }
    FOperandoDerecho: TCsExpresionBooleana;
  protected
    {** Ver @link(TCsExpresionBooleana.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresionBooleana.GetValor). }
    function GetValor: Boolean; override;
  public
    {** Constructor.
        @param(AOperandoIzquierdo Operando izquierdo de la disyuncion.)
        @param(AOperandoDerecho Operando derecho de la disyuncion.) }
    constructor Create(const AOperandoIzquierdo, AOperandoDerecho: TCsExpresionBooleana);
    {** Ver @link(TCsExpresionBooleana.AsignarContexto). }
    procedure AsignarContexto(const AContexto: TCsContexto); override;
    {** Operando izquierdo de la disyuncion. }
    property OperandoIzquierdo: TCsExpresionBooleana read FOperandoIzquierdo;
    {** Operando derecho de la disyuncion. }
    property OperandoDerecho: TCsExpresionBooleana read FOperandoDerecho;
  end { TCsDisyuncion };

  {** Tipo de metodos que implementa un predicado sobre valores de expresiones
      reales. }
  TCsMetodoPredicado = function (const Parametros: Variant): Boolean;

  {** Registro que agrupa las propiedades que describen a un predicado sobre
  valores de expresiones reales.
  @abstract(Descriptor de predicado sobre expresiones reales). }
  TCsPredicado = record
    {** Aridad de la funcion. }
    Aridad: Integer;
    {** Etiqueta (simbolo) que identifica al operador o funcion. }
    Etiqueta: TCsEtiqueta;
    {** Ubicacion del simbolo. }
    Ubicacion: TCsUbicacionOperador;
    {** Metodo que implementa al operador o funcion. }
    Metodo: TCsMetodoPredicado;
  end { TCsPredicado };

  {** Expresion booleana que representa la aplicacion de un predicado sobre un
      conjunto de expresiones reales como parametros.
      @abstract(Aplicacion de un predicado sobre parametros reales.) }
  TCsAplicacionPredicadoExpresiones = class(TCsExpresionBooleana)
  private
    {** Informacion sobre el predicado. }
    FPredicado: TCsPredicado;
    {** Arreglo de expresiones parametros de la aplicacion. }
    FParametros: TCsArregloExpresiones;
  protected
    {** Ver @link(TCsExpresionBooleana.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresionBooleana.GetValor). }
    function GetValor: Boolean; override;
  public
    {** Constructor de una aplicacion de predicado 0-ario.
        @param(APredicado Informacion sobre el predicado.) }
    constructor Create(const APredicado: TCsPredicado); overload;
    {** Constructor de una aplicacion de predicado unario.
        @param(APredicado Informacion sobre el predicado.)
        @param(Parametro Expresion parametro de la aplicacion.) }
    constructor Create(const APredicado: TCsPredicado; const Parametro: TCsExpresion); overload;
    {** Constructor de una aplicacion de predicado binario.
        @param(APredicado Informacion sobre el predicado.)
        @param(OperandoIzquierdo Expresion parametro izquierdo de la
        aplicacion.)
        @param(OperandoDerecho Expresion parametro izquierdo de la
        aplicacion.) }
    constructor Create(const APredicado: TCsPredicado; const OperandoIzquierdo, OperandoDerecho: TCsExpresion); overload;
    {** Constructor de una aplicacion de predicado n-ario.
        @param(APredicado Informacion sobre el predicado.)
        @param(AParametros Arreglo de expresiones parametros.) }
    constructor Create(const APredicado: TCsPredicado; const AParametros: TCsArregloExpresiones); overload;
    {** Ver @link(TCsExpresionBooleana.AsignarContexto). }
    procedure AsignarContexto(const AContexto: TCsContexto); override;
    {** Informacion sobre el predicado. }
    property Predicado: TCsPredicado read FPredicado;
    {** Arreglo de expresiones parametros de la aplicacion. }
    property Parametros: TCsArregloExpresiones read FParametros;
  end { TCsAplicacionPredicadoExpresiones };

  {** Esta es una expresion real que respresenta la aplicacion de una funcion
      de resultado real sobre parametros reales.
      @abstract(Aplicacion de funcion real sobre valores de expresiones
      booleanas.) }
  TCsAplicacionFuncionBooleanos = class(TCsExpresion)
  private
    {** Informacion sobre la funcion. }
    FFuncion: TCsFuncion;
    {** Arreglo de expresiones booleanas parametros de la aplicacion. }
    FParametros: TCsArregloExpresionesBooleanas;
  protected
    {** Ver @link(TCsExpresion.GetTexto). }
    function GetTexto: String; override;
    {** Ver @link(TCsExpresion.GetValor). }
    function GetValor: TCsValor; override;
    {** Ver @link(TCsExpresion.SetContexto). }
    procedure SetContexto(const AContexto: TCsContexto); override;
  public
    {** Constructor de una aplicacion de funcion 0-aria.
        @param(AFuncion Informacion sobre la funcion.) }
    constructor Create(const AFuncion: TCsFuncion); overload;
    {** Constructor de una aplicacion de funcion unaria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(Parametro Expresion parametro de la aplicacion.) }
    constructor Create(const AFuncion: TCsFuncion; const Parametro: TCsExpresionBooleana); overload;
    {** Constructor de una aplicacion de funcion binaria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(OperandoIzquierdo Expresion parametro izquierdo de la
        aplicacion.)
        @param(OperandoDerecho Expresion parametro derecho de la
        aplicacion.) }
    constructor Create(const AFuncion: TCsFuncion; const OperandoIzquierdo, OperandoDerecho: TCsExpresionBooleana); overload;
    {** Constructor de una aplicacion de funcion n-aria.
        @param(AFuncion Informacion sobre la funcion.)
        @param(AParametros Arreglo de expresiones parametros de la
        aplicacion.) }
    constructor Create(const AFuncion: TCsFuncion; const AParametros: TCsArregloExpresionesBooleanas); overload;
    {** Informacion sobre la funcion. }
    property Funcion: TCsFuncion read FFuncion;
    {** Arreglo de expresiones booleanas parametros de la aplicacion. }
    property Parametros: TCsArregloExpresionesBooleanas read FParametros;
  end { TCsAplicacionFuncionBooleanos };

// Libreria de implementaciones e informacion sobre funciones y predicados
// utiles al momento de evaluar expresiones reales.

{** Funcion exponencial.
    @param(Parametro Potencia de la base natural.)
    @returns(El valor de la base natural (e) elevada al parametro.) }
function CsMetodoExponencial(const Parametro: Variant): TCsValor;
{** Funcion logaritmo natural.
    @param(Parametro Argumento del logaritmo.)
    @returns(El logaritmo en base natural (e) del parametro.) }
function CsMetodoLogaritmoNatural(const Parametro: Variant): TCsValor;
{** Funcion logaritmo en base 2.
    @param(Parametro Argumento del logaritmo.)
    @returns(El logaritmo en base 2 del parametro.) }
function CsMetodoLogaritmoBase2(const Parametro: Variant): TCsValor;
{** Funcion logaritmo en base 10.
    @param(Parametro Argumento del logaritmo.)
    @returns(El logaritmo en base 10 del parametro.) }
function CsMetodoLogaritmoBase10(const Parametro: Variant): TCsValor;
{** Funcion logaritmo en base arbitraria.
    @param(Parametro Argumentos del logaritmo.)
    @returns(El logaritmo en base del segundo parametro del parametro.) }
function CsMetodoLogaritmo(const Parametro: Variant): TCsValor;
{** Funcion seno.
    @param(Parametro Argumento del seno.)
    @returns(El seno del parametro.) }
function CsMetodoSeno(const Parametro: Variant): TCsValor;
{** Funcion coseno.
    @param(Parametro Argumento del coseno.)
    @returns(El coseno del parametro.) }
function CsMetodoCoseno(const Parametro: Variant): TCsValor;
{** Funcion tangente.
    @param(Parametro Argumento de la tangente.)
    @returns(La tangente del parametro.) }
function CsMetodoTangente(const Parametro: Variant): TCsValor;

{** Predicado menor que. Toma dos parametros y devuelve true si el primero (en
    orden del arreglo parametro) es menor estricto que el segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es menor estricto que el segundo, o
    @false en caso contrario.) }
function CsMetodoMenorQue(const Parametro: Variant): Boolean;
{** Predicado menor o igual que. Toma dos parametros y devuelve true si el
    primero (en orden del arreglo parametro) es menor o igual que el segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es menor o igual que el segundo, o
    @false en caso contrario.) }
function CsMetodoMenorIgualQue(const Parametro: Variant): Boolean;
{** Predicado igual a. Toma dos parametros y devuelve true si el primero (en
    orden del arreglo parametro) es igual al segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es igual al segundo, o @false en caso
    contrario.) }
function CsMetodoIgual(const Parametro: Variant): Boolean;
{** Predicado distinto a. Toma dos parametros y devuelve true si el primero
    (en orden del arreglo parametro) es distinto al segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es distinto al segundo, o @false en
    caso contrario.) }
function CsMetodoDistinto(const Parametro: Variant): Boolean;
{** Predicado mayor que. Toma dos parametros y devuelve true si el primero (en
    orden del arreglo parametro) es menor estricto que el segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es mayor estricto que el segundo, o
    @false en caso contrario.) }
function CsMetodoMayorQue(const Parametro: Variant): Boolean;
{** Predicado mayor o igual que. Toma dos parametros y devuelve true si el
    primero (en orden del arreglo parametro) es mayor o igual que el segundo.
    @param(Parametro Arreglo de parametros.)
    @returns(@true si el primer parametro es mayor o igual que el segundo, o
    @false en caso contrario.) }
function CsMetodoMayorIgualQue(const Parametro: Variant): Boolean;

{** Funcion sobre boolenos que devuelve el valor 1.0 si el predicado es
    @true o 0.0 en caso contrario.
    @param(Parametro Argumento booleano de la funcion.)
    @returns(1.0 si el parametro es @true, o 0.0 en caso contrario.) }
function CsMetodo1Si(const Parametro: Variant): TCsValor;

// Constantes con la informacion sobre las funciones y predicados de la
// libreria arriba declarada.

const
  CsFuncionExponencial: TCsFuncion = (Aridad: 1; Etiqueta: 'exp'; Ubicacion: Funcional; Metodo: CsMetodoExponencial);
  CsFuncionLogaritmoNatural: TCsFuncion = (Aridad: 1; Etiqueta: 'ln'; Ubicacion: Funcional; Metodo: CsMetodoLogaritmoNatural);
  CsFuncionLogaritmoBase2: TCsFuncion = (Aridad: 1; Etiqueta: 'log2'; Ubicacion: Funcional; Metodo: CsMetodoLogaritmoBase2);
  CsFuncionLogaritmoBase10: TCsFuncion = (Aridad: 1; Etiqueta: 'log10'; Ubicacion: Funcional; Metodo: CsMetodoLogaritmoBase10);
  CsFuncionLogaritmo: TCsFuncion = (Aridad: 2; Etiqueta: 'log'; Ubicacion: Funcional; Metodo: CsMetodoLogaritmo);
  CsFuncionSeno: TCsFuncion = (Aridad: 1; Etiqueta: 'sen'; Ubicacion: Funcional; Metodo: CsMetodoSeno);
  CsFuncionCoseno: TCsFuncion = (Aridad: 1; Etiqueta: 'cos'; Ubicacion: Funcional; Metodo: CsMetodoCoseno);
  CsFuncionTangente: TCsFuncion = (Aridad: 1; Etiqueta: 'tan'; Ubicacion: Funcional; Metodo: CsMetodoTangente);

  CsPredicadoMenorQue: TCsPredicado = (Aridad: 2; Etiqueta: '<'; Ubicacion: Infijo; Metodo: CsMetodoMenorQue);
  CsPredicadoMenorIgualQue: TCsPredicado = (Aridad: 2; Etiqueta: '<='; Ubicacion: Infijo; Metodo: CsMetodoMenorIgualQue);
  CsPredicadoIgual: TCsPredicado = (Aridad: 2; Etiqueta: '='; Ubicacion: Infijo; Metodo: CsMetodoIgual);
  CsPredicadoDistinto: TCsPredicado = (Aridad: 2; Etiqueta: '<>'; Ubicacion: Infijo; Metodo: CsMetodoDistinto);
  CsPredicadoMayorQue: TCsPredicado = (Aridad: 2; Etiqueta: '>'; Ubicacion: Infijo; Metodo: CsMetodoMayorQue);
  CsPredicadoMayorIgualQue: TCsPredicado = (Aridad: 2; Etiqueta: '>='; Ubicacion: Infijo; Metodo: CsMetodoMayorIgualQue);

  CsFuncion1Si: TCsFuncion = (Aridad: 1; Etiqueta: '1si'; Ubicacion: Funcional; Metodo: CsMetodo1Si);

implementation

uses
  Math, SysUtils;

// -----------------------------------------------------------------------------
// TCsContexto
// -----------------------------------------------------------------------------

function TCsContexto.GetTexto: String;
var
  I: Integer;
begin { TCsContexto.GetTexto }
  Result := '[';
  if Length(FValores) > 0 then
  begin
    for I := Low(FValores) to High(FValores) - 1 do
      Result := Result + '$' + IntToStr(I) + ' => ' + FloatToStr(FValores [I]) + ', ';
    Result := Result + '$' + IntToStr(High(FValores)) + ' => ' + FloatToStr(FValores [High(FValores)]);
  end;
  Result := Result + ']';
end { TCsContexto.GetTexto };

function TCsContexto.GetValor(const Indice: Integer): TCsValor;
begin { TCsContexto.GetValor }
  Assert((Indice >= Low(FValores)) and (Indice <= High(FValores)), 'TCsContexto.GetValor: (Indice >= Low(FValores)) and (Indice <= High(FValores))');

  Result := FValores [Indice];
end { TCsContexto.GetValor };

procedure TCsContexto.SetValor(const Indice: Integer; const Valor: TCsValor);
begin { TCsContexto.SetValor }
  Assert((Indice >= Low(FValores)) and (Indice <= High(FValores)), 'TCsContexto.SetValor: (Indice >= Low(FValores)) and (Indice <= High(FValores))');

  FValores [Indice] := Valor;
end { TCsContexto.SetValor };

function TCsContexto.GetBajo;
begin { TCsContexto.GetBajo }
  Result := Low(FValores);
end { TCsContexto.GetBajo };

function TCsContexto.GetAlto;
begin { TCsContexto.GetAlto }
  Result := High(FValores);
end { TCsContexto.GetAlto };

constructor TCsContexto.Create(Dimension: Integer);
begin { TCsContexto.Create }
  Assert(Dimension > 0, 'TCsContexto.Create: Dimension > 0');

  SetLength(FValores, Dimension);
end { TCsContexto.Create };

destructor TCsContexto.Destroy;
begin { TCsContexto.Destroy }
  FValores := nil;
  inherited Destroy;
end { TCsContexto.Destroy };

// -----------------------------------------------------------------------------
// TCsExpresion
// -----------------------------------------------------------------------------

constructor TCsExpresion.Create(const AContexto: TCsContexto);
begin { TCsExpresion.Create }
  FContexto := AContexto;
end { TCsExpresion.Create };

// -----------------------------------------------------------------------------
// TCsArregloExpresiones
// -----------------------------------------------------------------------------

function TCsArregloExpresiones.GetAlto: Integer;
begin { TCsArregloExpresiones.GetAlto }
  Result := High(FExpresiones);
end { TCsArregloExpresiones.GetAlto };

function TCsArregloExpresiones.GetBajo: Integer;
begin { TCsArregloExpresiones.GetBajo }
  Result := Low(FExpresiones);
end { TCsArregloExpresiones.GetBajo };

function TCsArregloExpresiones.GetDimension: Integer;
begin { TCsArregloExpresiones.GetDimension }
  Result := Length(FExpresiones);
end { TCsArregloExpresiones.GetDimension };

function TCsArregloExpresiones.GetExpresion(const Indice: Integer): TCsExpresion;
begin { TCsArregloExpresiones.GetExpresion }
  Assert(Length(FExpresiones) > 0, 'TCsArregloExpresiones.GetExpresion: Length(FExpresiones) > 0');
  Assert((Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones)), 'TCsArregloExpresiones.GetExpresion: (Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones))');
  
  Result := FExpresiones [Indice];
end { TCsArregloExpresiones.GetExpresion };

function TCsArregloExpresiones.GetTexto: String;
var
  I: Integer;
begin { TCsArregloExpresiones.GetTexto }
  Result := '[';
  if Length(FExpresiones) > 0 then
  begin
    for I := Low(FExpresiones) to High(FExpresiones) - 1 do
      Result := Result + FExpresiones [I].Texto + ', ';
    Result := Result + FExpresiones [High(FExpresiones)].Texto;
  end;
  Result := Result + ']';
end { TCsArregloExpresiones.GetTexto };

procedure TCsArregloExpresiones.SetDimension(const ADimension: Integer);
begin { TCsArregloExpresiones.SetDimension }
  Assert(ADimension >= 0, 'TCsArregloExpresiones.SetDimension: ADimension >= 0');

  SetLength(FExpresiones, ADimension);  
end { TCsArregloExpresiones.SetDimension };

procedure TCsArregloExpresiones.SetExpresion(const Indice: Integer; const Expresion: TCsExpresion);
begin { TCsArregloExpresiones.SetExpresion }
  Assert(Length(FExpresiones) > 0, 'TCsArregloExpresiones.GetExpresion: Length(FExpresiones) > 0');
  Assert((Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones)), 'TCsArregloExpresiones.GetExpresion: (Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones))');
  Assert(Assigned(Expresion), 'TCsArregloExpresiones.SetExpresion: Assigned(Expresion)');

  FExpresiones [Indice] := Expresion;
end { TCsArregloExpresiones.SetExpresion };

constructor TCsArregloExpresiones.Create(const ADimension: Integer);
begin { TCsArregloExpresiones.Create }
  Assert(ADimension >= 0, 'TCsArregloExpresiones.Create: ADimension >= 0');

  SetLength(FExpresiones, ADimension);  
end { TCsArregloExpresiones.Create };

destructor TCsArregloExpresiones.Destroy;
begin { TCsArregloExpresiones.Create }
  FExpresiones := nil;
  inherited Destroy;
end { TCsArregloExpresiones.Create };

// -----------------------------------------------------------------------------
// TCsConstante
// -----------------------------------------------------------------------------

function TCsConstante.GetTexto: String;
begin { TCsConstante.GetTexto }
  Result := FloatToStr(FValor);
end { TCsConstante.GetTexto };

function TCsConstante.GetValor: TCsValor;
begin { TCsConstante.GetValor }
  Result := FValor;
end { TCsConstante.GetValor };

procedure TCsConstante.SetContexto(const AContexto: TCsContexto);
begin { TCsConstante.SetContexto }
  FContexto := AContexto;
end { TCsConstante.SetContexto };

constructor TCsConstante.Create(const AValor: TCsValor; const AContexto: TCsContexto);
begin { TCsConstante.Create }
  inherited Create(AContexto);

  FValor := AValor;
end { TCsConstante.Create };

// -----------------------------------------------------------------------------
// TCsVariable
// -----------------------------------------------------------------------------

function TCsVariable.GetTexto: String;
begin { TCsVariable.GetTexto }
  Result := '$' + IntToStr(FIndice);
end { TCsVariable.GetTexto };

function TCsVariable.GetValor: TCsValor;
begin { TCsVariable.GetValor }
  Assert(Assigned(FContexto));

  Result := FContexto [FIndice];
end { TCsVariable.GetValor };

procedure TCsVariable.SetContexto(const AContexto: TCsContexto);
begin { TCsVariable.SetContexto }
  FContexto := AContexto;
end { TCsVariable.SetContexto };

constructor TCsVariable.Create(const AIndice: Integer; const AContexto: TCsContexto);
begin { TCsVariable.Create }
  inherited Create(AContexto);

  Assert(AIndice >= 0, 'TCsVariable.Create: AIndice >= 0');

  FIndice := AIndice;
end { TCsVariable.Create };

// -----------------------------------------------------------------------------
// TCsSuma
// -----------------------------------------------------------------------------

function TCsSuma.GetTexto: String;
begin { TCsSuma.GetTexto }
  Result := '+ ' + FTerminoIzquierdo.Texto + ' ' + FTerminoDerecho.Texto;
end { TCsSuma.GetTexto };

function TCsSuma.GetValor: TCsValor;
begin { TCsSuma.GetValor }
  Result := FTerminoIzquierdo.Valor + FTerminoDerecho.Valor;
end { TCsSuma.GetValor };

procedure TCsSuma.SetContexto(const AContexto: TCsContexto);
begin { TCsSuma.SetContexto }
  FContexto := AContexto;

  FTerminoIzquierdo.Contexto := AContexto;
  FTerminoDerecho.Contexto := AContexto;
end { TCsSuma.SetContexto };

constructor TCsSuma.Create(const ATerminoIzquierdo, ATerminoDerecho: TCsExpresion; const AContexto: TCsContexto);
begin { TCsSuma.Create }
  inherited Create(AContexto);

  Assert(Assigned(ATerminoIzquierdo), 'TCsSuma.Create: Assigned(ATerminoIzquierdo)');
  Assert(Assigned(ATerminoDerecho), 'TCsSuma.Create: Assigned(ATerminoDerecho)');

  FTerminoIzquierdo := ATerminoIzquierdo;
  FTerminoDerecho := ATerminoDerecho;
end { TCsSuma.Create };

// -----------------------------------------------------------------------------
// TCsResta
// -----------------------------------------------------------------------------

function TCsResta.GetTexto: String;
begin { TCsResta.GetTexto }
  Result := '- ' + FTerminoIzquierdo.Texto + ' ' + FTerminoDerecho.Texto;
end { TCsResta.GetTexto };

function TCsResta.GetValor: TCsValor;
begin { TCsResta.GetValor }
  Result := FTerminoIzquierdo.Valor + FTerminoDerecho.Valor;
end { TCsResta.GetValor };

procedure TCsResta.SetContexto(const AContexto: TCsContexto);
begin { TCsResta.SetContexto }
  FContexto := AContexto;

  FTerminoIzquierdo.Contexto := AContexto;
  FTerminoDerecho.Contexto := AContexto;
end { TCsResta.SetContexto };

constructor TCsResta.Create(const ATerminoIzquierdo, ATerminoDerecho: TCsExpresion; const AContexto: TCsContexto);
begin { TCsResta.Create }
  inherited Create(AContexto);

  Assert(Assigned(ATerminoIzquierdo), 'TCsResta.Create: Assigned(ATerminoIzquierdo)');
  Assert(Assigned(ATerminoDerecho), 'TCsResta.Create: Assigned(ATerminoDerecho)');

  FTerminoIzquierdo := ATerminoIzquierdo;
  FTerminoDerecho := ATerminoDerecho;
end { TCsResta.Create };

// -----------------------------------------------------------------------------
// TCsProducto
// -----------------------------------------------------------------------------

function TCsProducto.GetTexto: String;
begin { TCsProducto.GetTexto }
  Result := '* ' + FFactorIzquierdo.Texto + ' ' + FFactorDerecho.Texto;
end { TCsProducto.GetTexto };

function TCsProducto.GetValor: TCsValor;
begin { TCsProducto.GetValor }
  Result := FFactorIzquierdo.Valor * FFactorDerecho.Valor;
end { TCsProducto.GetValor };

procedure TCsProducto.SetContexto(const AContexto: TCsContexto);
begin { TCsProducto.SetContexto }
  FContexto := AContexto;

  FFactorIzquierdo.Contexto := AContexto;
  FFactorDerecho.Contexto := AContexto;
end { TCsProducto.SetContexto };

constructor TCsProducto.Create(const AFactorIzquierdo, AFactorDerecho: TCsExpresion; const AContexto: TCsContexto);
begin { TCsProducto.Create }
  inherited Create(AContexto);

  Assert(Assigned(AFactorIzquierdo), 'TCsProducto.Create: Assigned(AFactorIzquierdo)');
  Assert(Assigned(AFactorDerecho), 'TCsProducto.Create: Assigned(AFactorDerecho)');

  FFactorIzquierdo := AFactorIzquierdo;
  FFactorDerecho := AFactorDerecho;
end { TCsProducto.Create };

// -----------------------------------------------------------------------------
// TCsDivision
// -----------------------------------------------------------------------------

function TCsDivision.GetTexto: String;
begin { TCsDivision.GetTexto }
  Result := '/ ' + FDividendo.Texto + ' ' + FDivisor.Texto;
end { TCsDivision.GetTexto };

function TCsDivision.GetValor: TCsValor;
begin { TCsDivision.GetValor }
  Result := FDividendo.Valor / FDivisor.Valor;
end { TCsDivision.GetValor };

procedure TCsDivision.SetContexto(const AContexto: TCsContexto);
begin { TCsDivision.SetContexto }
  FContexto := AContexto;

  FDividendo.Contexto := AContexto;
  FDivisor.Contexto := AContexto;
end { TCsDivision.SetContexto };

constructor TCsDivision.Create(const ADividendo, ADivisor: TCsExpresion; const AContexto: TCsContexto);
begin { TCsDivision.Create }
  inherited Create(AContexto);

  Assert(Assigned(ADividendo), 'TCsDivision.Create: Assigned(ADividendo)');
  Assert(Assigned(ADivisor), 'TCsDivision.Create: Assigned(ADivisor)');

  FDividendo := ADividendo;
  FDivisor := ADivisor;
end { TCsDivision.Create };

// -----------------------------------------------------------------------------
// TCsPotencia
// -----------------------------------------------------------------------------

function TCsPotencia.GetTexto: String;
begin { TCsPotencia.GetTexto }
  Result := '^ ' + FBase.Texto + ' ' + FExponente.Texto;
end { TCsPotencia.GetTexto };

function TCsPotencia.GetValor: TCsValor;
begin { TCsPotencia.GetValor }
  Result := Power(FBase.Valor, FExponente.Valor);
end { TCsPotencia.GetValor };

procedure TCsPotencia.SetContexto(const AContexto: TCsContexto);
begin { TCsPotencia.SetContexto }
  FContexto := AContexto;

  FBase.Contexto := AContexto;
  FExponente.Contexto := AContexto;
end { TCsPotencia.SetContexto };

constructor TCsPotencia.Create(const ABase, AExponente: TCsExpresion; const AContexto: TCsContexto);
begin { TCsDivision.Create }
  inherited Create(AContexto);

  Assert(Assigned(ABase), 'TCsPotencia.Create: Assigned(ABase)');
  Assert(Assigned(AExponente), 'TCsPotencia.Create: Assigned(AExponente)');

  FBase := ABase;
  FExponente := AExponente;
end { TCsPotencia.Create };

// -----------------------------------------------------------------------------
// TCsAplicacionFuncion
// -----------------------------------------------------------------------------

function TCsAplicacionFuncion.GetTexto: String;
var
  I: Integer;
begin { TCsAplicacionFuncion.GetTexto }
  Result := FFuncion.Etiqueta;
  if FFuncion.Aridad > 0 then
  begin
    Result := Result + ' ';
    for I := 0 to FFuncion.Aridad - 2 do
      Result := Result + FParametros [I].Texto + ' ';
    Result := Result + FParametros [FFuncion.Aridad - 1].Texto;
  end;
end { TCsAplicacionFuncion.GetTexto };

function TCsAplicacionFuncion.GetValor: TCsValor;
var
  I: Integer;
  Parametro: Variant;
begin { TCsAplicacionFuncion.GetValor }
  if FFuncion.Aridad = 1 then
    Parametro := FParametros [0].Valor
  else if FFuncion.Aridad > 1 then
  begin
    Parametro := VarArrayCreate([0, FFuncion.Aridad - 1], varDouble);

    for I := FParametros.Bajo to FParametros.Alto do
      Parametro [I] := FParametros [I].Valor;
  end;

  Result := FFuncion.Metodo(Parametro);
end { TCsAplicacionFuncion.GetValor };

procedure TCsAplicacionFuncion.SetContexto(const AContexto: TCsContexto);
var
  I: Integer;
begin { TCsAplicacionFuncion.SetContexto }
  FContexto := AContexto;

  for I := FParametros.Bajo to FParametros.Alto do
    FParametros [I].Contexto := AContexto;
end { TCsAplicacionFuncion.SetContexto };

constructor TCsAplicacionFuncion.Create(const AFuncion: TCsFuncion; const AContexto: TCsContexto);
begin { TCsAplicacionFuncion.Create }
  inherited Create(AContexto);

  Assert(AFuncion.Aridad = 0, 'TCsAplicacionFuncion.Create: AFuncion.Aridad = 0');

  FFuncion := AFuncion;
  FParametros := nil;
end { TCsAplicacionFuncion.Create };

constructor TCsAplicacionFuncion.Create(const AFuncion: TCsFuncion; const Parametro: TCsExpresion; const AContexto: TCsContexto);
begin { TCsAplicacionFuncion.Create }
  inherited Create(AContexto);

  Assert(Assigned(Parametro), 'TCsAplicacionFuncion.Create: Assigned(Parametro)');
  Assert(AFuncion.Aridad <= 1, 'TCsAplicacionFuncion.Create: AFuncion.Aridad <= 1');

  FFuncion := AFuncion;

  FParametros := TCsArregloExpresiones.Create(1);
  FParametros [0] := Parametro;
end { TCsAplicacionFuncion.Create };

constructor TCsAplicacionFuncion.Create(const AFuncion: TCsFuncion; const OperandoIzquierdo, OperandoDerecho: TCsExpresion; const AContexto: TCsContexto);
begin { TCsAplicacionFuncion.Create }
  inherited Create(AContexto);

  Assert(Assigned(OperandoIzquierdo), 'TCsAplicacionFuncion.Create: Assigned(OperandoIzquierdo)');
  Assert(Assigned(OperandoDerecho), 'TCsAplicacionFuncion.Create: Assigned(OperandoDerecho)');
  Assert(AFuncion.Aridad <= 2, 'TCsAplicacionFuncion.Create: AFuncion.Aridad <= 2');

  FFuncion := AFuncion;

  FParametros := TCsArregloExpresiones.Create(2);
  FParametros [0] := OperandoIzquierdo;
  FParametros [1] := OperandoDerecho;
end { TCsAplicacionFuncion.Create };

constructor TCsAplicacionFuncion.Create(const AFuncion: TCsFuncion; const AParametros: TCsArregloExpresiones; const AContexto: TCsContexto);
begin { TCsAplicacionFuncion.Create }
  inherited Create(AContexto);

  Assert(Assigned(AParametros), 'TCsAplicacionFuncion.Create: Assigned(AParametros)');
  Assert(AFuncion.Aridad <= AParametros.Dimension, 'TCsAplicacionFuncion.Create: AFuncion.Aridad <= AParametros.Dimension');

  FFuncion := AFuncion;
  FParametros := AParametros;
end { TCsAplicacionFuncion.Create };

// -----------------------------------------------------------------------------
// TCsVariableAleatoria
// -----------------------------------------------------------------------------

function TCsVariableAleatoria.GetTexto: String;
begin { TCsVariableAleatoria.GetTexto }
  Result := FVariableAleatoria.Texto;
end { TCsVariableAleatoria.GetTexto };

function TCsVariableAleatoria.GetValor: TCsValor;
begin { TCsVariableAleatoria.GetValor }
  Result := FVariableAleatoria.Siguiente;
end { TCsVariableAleatoria.GetValor };

procedure TCsVariableAleatoria.SetContexto(const AContexto: TCsContexto);
begin { TCsVariableAleatoria.SetContexto }
  FContexto := AContexto;
end { TCsVariableAleatoria.SetContexto };

constructor TCsVariableAleatoria.Create(const AVariableAleatoria: TUaVariableAleatoria);
begin { TCsVariableAleatoria.Create }
  Assert(Assigned(AVariableAleatoria), 'TCsVariableAleatoria.Create: Assigned(AVariableAleatoria)');

  FVariableAleatoria := AVariableAleatoria;
end { TCsVariableAleatoria.Create };

// -----------------------------------------------------------------------------
// TCsExpresionEntera
// -----------------------------------------------------------------------------

function TCsExpresionEntera.GetTexto: String;
begin { TCsExpresionEntera.GetTexto }
  Result := '[' + FExpresionReal.Texto + ']';
end { TCsExpresionEntera.GetTexto };

function TCsExpresionEntera.GetValor: TCsValorEntero;
begin { TCsExpresionEntera.GetValor }
  Result := Floor(FExpresionReal.Valor);
end { TCsExpresionEntera.GetValor };

constructor TCsExpresionEntera.Create(const AExpresionReal: TCsExpresion);
begin { TCsExpresionEntera.Create }
  Assert(Assigned(AExpresionReal), 'TCsExpresionEntera.Create: Assigned(AExpresion)');

  FExpresionReal := AExpresionReal;
end { TCsExpresionEntera.Create };

// -----------------------------------------------------------------------------
// TCsArregloExpresionesBooleanas
// -----------------------------------------------------------------------------

function TCsArregloExpresionesBooleanas.GetAlto: Integer;
begin { TCsArregloExpresionesBooleanas.GetAlto }
  Result := High(FExpresiones);
end { TCsArregloExpresionesBooleanas.GetAlto };

function TCsArregloExpresionesBooleanas.GetBajo: Integer;
begin { TCsArregloExpresionesBooleanas.GetBajo }
  Result := Low(FExpresiones);
end { TCsArregloExpresionesBooleanas.GetBajo };

function TCsArregloExpresionesBooleanas.GetDimension: Integer;
begin { TCsArregloExpresionesBooleanas.GetDimension }
  Result := Length(FExpresiones);
end { TCsArregloExpresionesBooleanas.GetDimension };

function TCsArregloExpresionesBooleanas.GetExpresion(const Indice: Integer): TCsExpresionBooleana;
begin { TCsArregloExpresionesBooleanas.GetExpresion }
  Assert(Length(FExpresiones) > 0, 'TCsArregloExpresionesBooleanas.GetExpresion: Length(FExpresiones) > 0');
  Assert((Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones)), 'TCsArregloExpresionesBooleanas.GetExpresion: (Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones))');
  
  Result := FExpresiones [Indice];
end { TCsArregloExpresionesBooleanas.GetExpresion };

function TCsArregloExpresionesBooleanas.GetTexto: String;
var
  I: Integer;
begin { TCsArregloExpresionesBooleanas.GetTexto }
  Result := '[';
  if Length(FExpresiones) > 0 then
  begin
    for I := Low(FExpresiones) to High(FExpresiones) - 1 do
      Result := Result + FExpresiones [I].Texto + ', ';
    Result := Result + FExpresiones [High(FExpresiones)].Texto;
  end;
  Result := Result + ']';
end { TCsArregloExpresionesBooleanas.GetTexto };

procedure TCsArregloExpresionesBooleanas.SetDimension(const ADimension: Integer);
begin { TCsArregloExpresionesBooleanas.SetDimension }
  Assert(ADimension >= 0, 'TCsArregloExpresionesBooleanas.SetDimension: ADimension >= 0');

  SetLength(FExpresiones, ADimension);  
end { TCsArregloExpresionesBooleanas.SetDimension };

procedure TCsArregloExpresionesBooleanas.SetExpresion(const Indice: Integer; const Expresion: TCsExpresionBooleana);
begin { TCsArregloExpresionesBooleanas.SetExpresion }
  Assert(Length(FExpresiones) > 0, 'TCsArregloExpresionesBooleanas.GetExpresion: Length(FExpresiones) > 0');
  Assert((Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones)), 'TCsArregloExpresionesBooleanas.GetExpresion: (Indice >= Low(FExpresiones)) and (Indice <= High(FExpresiones))');
  Assert(Assigned(Expresion), 'TCsArregloExpresionesBooleanas.SetExpresion: Assigned(Expresion)');

  FExpresiones [Indice] := Expresion;
end { TCsArregloExpresionesBooleanas.SetExpresion };

constructor TCsArregloExpresionesBooleanas.Create(const ADimension: Integer);
begin { TCsArregloExpresionesBooleanas.Create }
  Assert(ADimension >= 0, 'TCsArregloExpresionesBooleanas.Create: ADimension >= 0');

  SetLength(FExpresiones, ADimension);  
end { TCsArregloExpresionesBooleanas.Create };

destructor TCsArregloExpresionesBooleanas.Destroy;
begin { TCsArregloExpresionesBooleanas.Create }
  FExpresiones := nil;
  inherited Destroy;
end { TCsArregloExpresionesBooleanas.Create };

// -----------------------------------------------------------------------------
// TCsConstanteBooleana
// -----------------------------------------------------------------------------

function TCsConstanteBooleana.GetTexto: String;
begin { TCsConstanteBooleana.GetTexto }
  if FValor then
    Result := 'True'
  else
    Result := 'False';
end { TCsConstanteBooleana.GetTexto };

function TCsConstanteBooleana.GetValor: Boolean;
begin { TCsConstanteBooleana.GetValor }
  Result := FValor;
end { TCsConstanteBooleana.GetValor };

constructor TCsConstanteBooleana.Create(const AValor: Boolean);
begin { TCsConstanteBooleana.Create }
  FValor := AValor;
end { TCsConstanteBooleana.Create };

procedure TCsConstanteBooleana.AsignarContexto(const AContexto: TCsContexto);
begin { TCsConstanteBooleana.AsignarContexto }
end { TCsConstanteBooleana.AsignarContexto };

// -----------------------------------------------------------------------------
// TCsNegacion
// -----------------------------------------------------------------------------

function TCsNegacion.GetTexto: String;
begin { TCsNegacion.GetTexto }
  Result := '! ' + FOperando.Texto;
end { TCsNegacion.GetTexto };

function TCsNegacion.GetValor: Boolean;
begin { TCsNegacion.GetValor }
  Result := not FOperando.Valor;
end { TCsNegacion.GetValor };

constructor TCsNegacion.Create(const AOperando: TCsExpresionBooleana);
begin { TCsNegacion.Create }
  Assert(Assigned(AOperando), 'TCsNegacion.Create: Assigned(AOperando)');

  FOperando := AOperando;
end { TCsNegacion.Create };

procedure TCsNegacion.AsignarContexto(const AContexto: TCsContexto);
begin { TCsNegacion.AsignarContexto }
  FOperando.AsignarContexto(AContexto);
end { TCsNegacion.AsignarContexto };

// -----------------------------------------------------------------------------
// TCsConjuncion
// -----------------------------------------------------------------------------

function TCsConjuncion.GetTexto: String;
begin { TCsConjuncion.GetTexto }
  Result := '& ' + FOperandoIzquierdo.Texto + ' ' + FOperandoDerecho.Texto;
end { TCsConjuncion.GetTexto };

function TCsConjuncion.GetValor: Boolean;
begin { TCsConjuncion.GetValor }
  Result := FOperandoIzquierdo.Valor and FOperandoDerecho.Valor;
end { TCsConjuncion.GetValor };

constructor TCsConjuncion.Create(const AOperandoIzquierdo, AOperandoDerecho: TCsExpresionBooleana);
begin { TCsConjuncion.Create }
  Assert(Assigned(AOperandoIzquierdo), 'TCsConjuncion.Create: Assigned(AOperandoIzquierdo)');
  Assert(Assigned(AOperandoDerecho), 'TCsConjuncion.Create: Assigned(AOperandoDerecho)');

  FOperandoIzquierdo := AOperandoIzquierdo;
  FOperandoDerecho := AOperandoDerecho;
end { TCsConjuncion.Create };

procedure TCsConjuncion.AsignarContexto(const AContexto: TCsContexto);
begin { TCsConjuncion.AsignarContexto }
  FOperandoIzquierdo.AsignarContexto(AContexto);
  FOperandoDerecho.AsignarContexto(AContexto);
end { TCsConjuncion.AsignarContexto };

// -----------------------------------------------------------------------------
// TCsDisyuncion
// -----------------------------------------------------------------------------

function TCsDisyuncion.GetTexto: String;
begin { TCsDisyuncion.GetTexto }
  Result := '| ' + FOperandoIzquierdo.Texto + ' ' + FOperandoDerecho.Texto;
end { TCsDisyuncion.GetTexto };

function TCsDisyuncion.GetValor: Boolean;
begin { TCsDisyuncion.GetValor }
  Result := FOperandoIzquierdo.Valor or FOperandoDerecho.Valor;
end { TCsDisyuncion.GetValor };

constructor TCsDisyuncion.Create(const AOperandoIzquierdo, AOperandoDerecho: TCsExpresionBooleana);
begin { TCsDisyuncion.Create }
  Assert(Assigned(AOperandoIzquierdo), 'TCsDisyuncion.Create: Assigned(AOperandoIzquierdo)');
  Assert(Assigned(AOperandoDerecho), 'TCsDisyuncion.Create: Assigned(AOperandoDerecho)');

  FOperandoIzquierdo := AOperandoIzquierdo;
  FOperandoDerecho := AOperandoDerecho;
end { TCsDisyuncion.Create };

procedure TCsDisyuncion.AsignarContexto(const AContexto: TCsContexto);
begin { TCsDisyuncion.AsignarContexto }
  FOperandoIzquierdo.AsignarContexto(AContexto);
  FOperandoDerecho.AsignarContexto(AContexto);
end { TCsDisyuncion.AsignarContexto };

// -----------------------------------------------------------------------------
// TCsAplicacionPredicadoExpresiones
// -----------------------------------------------------------------------------

function TCsAplicacionPredicadoExpresiones.GetTexto: String;
var
  I: Integer;
begin { TCsAplicacionPredicadoExpresiones.GetTexto }
  Result := FPredicado.Etiqueta;
  if FPredicado.Aridad > 0 then
  begin
    Result := Result + ' ';
    for I := 0 to FPredicado.Aridad - 2 do
      Result := Result + FParametros [I].Texto + ' ';
    Result := Result + FParametros [FPredicado.Aridad - 1].Texto;
  end;
end { TCsAplicacionPredicadoExpresiones.GetTexto };

function TCsAplicacionPredicadoExpresiones.GetValor: Boolean;
var
  I: Integer;
  Parametro: Variant;
begin { TCsAplicacionPredicadoExpresiones.GetValor }
  if FPredicado.Aridad = 1 then
    Parametro := FParametros [0].Valor
  else if FPredicado.Aridad > 1 then
  begin
    Parametro := VarArrayCreate([0, FPredicado.Aridad - 1], varDouble);

    for I := FParametros.Bajo to FParametros.Alto do
      Parametro [I] := FParametros [I].Valor;
  end;

  Result := FPredicado.Metodo(Parametro);
end { TCsAplicacionPredicadoExpresiones.GetValor };

constructor TCsAplicacionPredicadoExpresiones.Create(const APredicado: TCsPredicado);
begin { TCsAplicacionPredicadoExpresiones.Create }
  Assert(APredicado.Aridad = 0, 'TCsAplicacionPredicadoExpresiones.Create: APredicado.Aridad = 0');

  FPredicado := APredicado;
  FParametros := nil;
end { TCsAplicacionPredicadoExpresiones.Create };

constructor TCsAplicacionPredicadoExpresiones.Create(const APredicado: TCsPredicado; const Parametro: TCsExpresion);
begin { TCsAplicacionPredicadoExpresiones.Create }
  Assert(Assigned(Parametro), 'TCsAplicacionPredicadoExpresiones.Create: Assigned(Parametro)');
  Assert(APredicado.Aridad <= 1, 'TCsAplicacionPredicadoExpresiones.Create: APredicado.Aridad <= 1');

  FPredicado := APredicado;

  FParametros := TCsArregloExpresiones.Create(1);
  FParametros [0] := Parametro;
end { TCsAplicacionPredicadoExpresiones.Create };

constructor TCsAplicacionPredicadoExpresiones.Create(const APredicado: TCsPredicado; const OperandoIzquierdo, OperandoDerecho: TCsExpresion);
begin { TCsAplicacionPredicadoExpresiones.Create }
  Assert(APredicado.Aridad <= 2, 'TCsAplicacionPredicadoExpresiones.Create: APredicado.Aridad <= 2');
  Assert(Assigned(OperandoIzquierdo), 'TCsAplicacionPredicadoExpresiones.Create: Assigned(OperandoIzquierdo)');
  Assert(Assigned(OperandoDerecho), 'TCsAplicacionPredicadoExpresiones.Create: Assigned(OperandoDerecho)');

  FPredicado := APredicado;

  FParametros := TCsArregloExpresiones.Create(2);
  FParametros [0] := OperandoIzquierdo;
  FParametros [1] := OperandoDerecho;
end { TCsAplicacionPredicadoExpresiones.Create };

constructor TCsAplicacionPredicadoExpresiones.Create(const APredicado: TCsPredicado; const AParametros: TCsArregloExpresiones);
begin { TCsAplicacionPredicadoExpresiones.Create }
  Assert(Assigned(AParametros), 'TCsAplicacionPredicadoExpresiones.Create: Assigned(AParametros)');
  Assert(APredicado.Aridad <= AParametros.Dimension, 'TCsAplicacionPredicadoExpresiones.Create: APredicado.Aridad <= AParametros.Dimension');

  FPredicado := APredicado;
  FParametros := AParametros;
end { TCsAplicacionPredicadoExpresiones.Create };

procedure TCsAplicacionPredicadoExpresiones.AsignarContexto(const AContexto: TCsContexto);
var
  I: Integer;
begin { TCsAplicacionPredicadoExpresiones.AsignarContexto }
  for I := FParametros.Bajo to FParametros.Alto do
    FParametros [I].Contexto := AContexto;
end { TCsAplicacionPredicadoExpresiones.AsignarContexto };

// -----------------------------------------------------------------------------
// TCsAplicacionFuncionBooleanos
// -----------------------------------------------------------------------------

function TCsAplicacionFuncionBooleanos.GetTexto: String;
var
  I: Integer;
begin { TCsAplicacionFuncionBooleanos.GetTexto }
  Result := FFuncion.Etiqueta;
  if FFuncion.Aridad > 0 then
  begin
    Result := Result + ' ';
    for I := 0 to FFuncion.Aridad - 2 do
      Result := Result + FParametros [I].Texto + ' ';
    Result := Result + FParametros [FFuncion.Aridad - 1].Texto;
  end;
end { TCsAplicacionFuncionBooleanos.GetTexto };

function TCsAplicacionFuncionBooleanos.GetValor: TCsValor;
var
  I: Integer;
  Parametro: Variant;
begin { TCsAplicacionFuncionBooleanos.GetValor }
  if FFuncion.Aridad = 1 then
    Parametro := FParametros [0].Valor
  else if FFuncion.Aridad > 1 then
  begin
    Parametro := VarArrayCreate([0, FFuncion.Aridad - 1], varBoolean);

    for I := FParametros.Bajo to FParametros.Alto do
      Parametro [I] := FParametros [I].Valor;
  end;

  Result := FFuncion.Metodo(Parametro);
end { TCsAplicacionFuncionBooleanos.GetValor };

procedure TCsAplicacionFuncionBooleanos.SetContexto(const AContexto: TCsContexto);
var
  I: Integer;
begin { TCsAplicacionFuncionBooleanos.SetContexto }
  FContexto := AContexto;

  for I := FParametros.Bajo to FParametros.Alto do
    FParametros [I].AsignarContexto(AContexto);
end { TCsAplicacionFuncionBooleanos.SetContexto };

constructor TCsAplicacionFuncionBooleanos.Create(const AFuncion: TCsFuncion);
begin { TCsAplicacionFuncionBooleanos.Create }
  Assert(AFuncion.Aridad = 0, 'TCsAplicacionFuncionBooleanos.Create: AFuncion.Aridad = 0');

  FFuncion := AFuncion;
  FParametros := nil;
end { TCsAplicacionFuncionBooleanos.Create };

constructor TCsAplicacionFuncionBooleanos.Create(const AFuncion: TCsFuncion; const Parametro: TCsExpresionBooleana);
begin { TCsAplicacionFuncionBooleanos.Create }
  Assert(Assigned(Parametro), 'TCsAplicacionFuncionBooleanos.Create: Assigned(Parametro)');
  Assert(AFuncion.Aridad <= 1, 'TCsAplicacionFuncionBooleanos.Create: AFuncion.Aridad <= 1');

  FFuncion := AFuncion;

  FParametros := TCsArregloExpresionesBooleanas.Create(1);
  FParametros [0] := Parametro;
end { TCsAplicacionFuncionBooleanos.Create };

constructor TCsAplicacionFuncionBooleanos.Create(const AFuncion: TCsFuncion; const OperandoIzquierdo, OperandoDerecho: TCsExpresionBooleana);
begin { TCsAplicacionFuncionBooleanos.Create }
  Assert(Assigned(OperandoIzquierdo), 'TCsAplicacionFuncionBooleanos.Create: Assigned(OperandoIzquierdo)');
  Assert(Assigned(OperandoDerecho), 'TCsAplicacionFuncionBooleanos.Create: Assigned(OperandoDerecho)');
  Assert(AFuncion.Aridad <= 2, 'TCsAplicacionFuncionBooleanos.Create: AFuncion.Aridad <= 2');

  FFuncion := AFuncion;

  FParametros := TCsArregloExpresionesBooleanas.Create(2);
  FParametros [0] := OperandoIzquierdo;
  FParametros [1] := OperandoDerecho;
end { TCsAplicacionFuncionBooleanos.Create };

constructor TCsAplicacionFuncionBooleanos.Create(const AFuncion: TCsFuncion; const AParametros: TCsArregloExpresionesBooleanas);
begin { TCsAplicacionFuncionBooleanos.Create }
  Assert(Assigned(AParametros), 'TCsAplicacionFuncionBooleanos.Create: Assigned(AParametros)');
  Assert(AFuncion.Aridad <= AParametros.Dimension, 'TCsAplicacionFuncionBooleanos.Create: AFuncion.Aridad <= Length(AParametros)');

  FFuncion := AFuncion;
  FParametros := AParametros;
end { TCsAplicacionFuncionBooleanos.Create };

// -----------------------------------------------------------------------------
// Libreria de funciones y predicados
// -----------------------------------------------------------------------------

function CsMetodoExponencial(const Parametro: Variant): TCsValor;
begin { CsMetodoExponencial }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoExponencial: VarType(Parametro) = varDouble');
  Result := Exp(Parametro);
end { CsMetodoExponencial };

function CsMetodoLogaritmoNatural(const Parametro: Variant): TCsValor;
begin { CsMetodoLogaritmoNatural }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoLogaritmoNatural: VarType(Parametro) = varDouble');
  Result := Ln(Parametro);
end { CsMetodoLogaritmoNatural };

function CsMetodoLogaritmoBase2(const Parametro: Variant): TCsValor;
begin { CsMetodoLogaritmoBase2 }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoLogaritmoBase2: VarType(Parametro) = varDouble');
  Result := Log2(Parametro);
end { CsMetodoLogaritmoBase2 };

function CsMetodoLogaritmoBase10(const Parametro: Variant): TCsValor;
begin { CsMetodoLogaritmoBase10 }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoLogaritmoBase10: VarType(Parametro) = varDouble');
  Result := Log10(Parametro);
end { CsMetodoLogaritmoBase10 };

function CsMetodoLogaritmo(const Parametro: Variant): TCsValor;
begin { CsMetodoLogaritmoBase10 }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoLogaritmo: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoLogaritmo: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoLogaritmo: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoLogaritmo: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoLogaritmo: VarArrayHighBound(Parametro) >= 1');

  Result := LogN(Parametro [0], Parametro [1]);
end { CsMetodoLogaritmoBase10 };

function CsMetodoSeno(const Parametro: Variant): TCsValor;
begin { CsMetodoSeno }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoSeno: VarType(Parametro) = varDouble');
  Result := Sin(Parametro);
end { CsMetodoSeno };

function CsMetodoCoseno(const Parametro: Variant): TCsValor;
begin { CsMetodoCoseno }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoCoseno: VarType(Parametro) = varDouble');
  Result := Cos(Parametro);
end { CsMetodoCoseno };

function CsMetodoTangente(const Parametro: Variant): TCsValor;
begin { CsMetodoTangente }
  Assert(VarType(Parametro) = varDouble, 'CsMetodoTangente: VarType(Parametro) = varDouble');
  Result := Tan(Parametro);
end { CsMetodoTangente };

function CsMetodoMenorQue(const Parametro: Variant): Boolean;
begin { CsMetodoMenorQue }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoMenorQue: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoMenorQue: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoMenorQue: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoMenorQue: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoMenorQue: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] < Parametro [1];
end { CsMetodoMenorQue };

function CsMetodoMenorIgualQue(const Parametro: Variant): Boolean;
begin { CsMetodoMenorIgualQue }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoMenorIgualQue: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoMenorIgualQue: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoMenorIgualQue: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoMenorIgualQue: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoMenorIgualQue: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] <= Parametro [1];
end { CsMetodoMenorIgualQue };

function CsMetodoIgual(const Parametro: Variant): Boolean;
begin { CsMetodoIgual }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoIgual: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoIgual: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoIgual: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoIgual: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoIgual: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] = Parametro [1];
end { CsMetodoIgual };

function CsMetodoDistinto(const Parametro: Variant): Boolean;
begin { CsMetodoDistinto }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoDistinto: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoDistinto: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoDistinto: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoDistinto: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoDistinto: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] <> Parametro [1];
end { CsMetodoDistinto };

function CsMetodoMayorQue(const Parametro: Variant): Boolean;
begin { CsMetodoMayorQue }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoIgual: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoIgual: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoIgual: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoIgual: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoIgual: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] > Parametro [1];
end { CsMetodoMayorQue };

function CsMetodoMayorIgualQue(const Parametro: Variant): Boolean;
begin { CsMetodoMayorIgualQue }
  Assert(VarType(Parametro) and varArray = varArray, 'CsMetodoMayorIgualQue: VarType(Parametro) and varArray = varArray');
  Assert(VarType(Parametro) and VarTypeMask = varDouble, 'CsMetodoMayorIgualQue: VarType(Parametro) and VarTypeMask = varDouble');
  Assert(VarArrayDimCount(Parametro) = 1, 'CsMetodoMayorIgualQue: VarArrayDimCount(Parametro) = 1');
  Assert(VarArrayLowBound(Parametro, 1) = 0, 'CsMetodoMayorIgualQue: VarArrayLowBound(Parametro) = 0');
  Assert(VarArrayHighBound(Parametro, 1) >= 1, 'CsMetodoMayorIgualQue: VarArrayHighBound(Parametro) >= 1');

  Result := Parametro [0] >= Parametro [1];
end { CsMetodoMayorIgualQue };

function CsMetodo1Si(const Parametro: Variant): TCsValor;
begin { CsMetodo1Si }
  Assert(VarType(Parametro) = varBoolean, 'CsMetodo1Si: VarType(Parametro) = varBoolean');

  if Parametro then
    Result := 1.0
  else
    Result := 0.0;
end { CsMetodo1Si };

end { UnitCalculoSimbolico }.

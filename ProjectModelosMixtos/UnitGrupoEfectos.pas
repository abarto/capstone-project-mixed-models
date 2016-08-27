{**
@abstract(Grupo de efectos de modelos mixtos.)
@author(Agustin Barto <abarto@gmail.com>)
@created(January 1, 2003)
@lastmod(December 27, 2004)
Este modulo contiene la definicion de grupos de efectos de modelos mixtos. Los
mismos contienen la lista de efectos y opciones asociadas a los mismos.
}
unit UnitGrupoEfectos;

interface

uses
  Contnrs,
  UnitEfecto, UnitOpcion;

type
  {** Grupo de efectos de modelos mixtos. Es utilizado para describir tanto el
      bloque de efectos fijos, como los bloques de los efectos aleatorios y
      grupos de los mismos. Cada grupo contiene una lista de efectos y una
      lista de opciones asociada al bloque.
      @abstract(Grupo de efectos (y opciones asociadas).) }
  TGrupoEfectos = class
  private
    {** Efectos del grupo. }
    FEfectos: TListaEfectos;
    {** Opciones del grupo. }
    FOpciones: TListaOpciones;
  public
    {** Constructor. }
    constructor Create; overload;
    {** Constructor.
        @param(AEfectos Efectos iniciales del grupo.)
        @param(AOpciones Opciones iniciales del grupo.) }
    constructor Create(AEfectos: TListaEfectos; AOpciones: TListaOpciones); overload;
    {** Destructor. }
    destructor Destroy; override;

    {** Efectos del grupo. }
    property Efectos: TListaEfectos read FEfectos write FEfectos;
    {** Opciones del grupo. }
    property Opciones: TListaOpciones read FOpciones write FOpciones;
  end { TGrupoEfectos };

  {** Lista de @link(TGrupoEfectos). Es una clase envoltura de TObjectList. Se
      utiliza esta arquitectura para proveer cheque de tipos de las instancias
      almacenadas y aprovechar la funcionalidad de TObjectList.
      @abstract(Lista de @link(TGrupoEfectos).) }
  TListaGruposEfectos = class(TObjectList)
  protected
    {** Metodo de lectura de la propiedad @link(Grupos). Devuelve el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento.)
        @returns(El elemento almacenado bajo el indice especificado.) }
    function GetItem(Index: Integer): TGrupoEfectos;
    {** Metodo de escritura de la propiedad @link(Grupos). Actualiza el
        elemento almacenado bajo un indice.
        @param(Index Indice del elemento a actualizar.)
        @param(Item Nuevo valor para el elemento.) }
    procedure SetItem(Index: Integer; Item: TGrupoEfectos);
  public
    {** Destructor. }
    destructor Destroy; override;
    {** Agrega una elemento al final de la lista.
        @param(Item Elemento a almacenar.)
        @returns(Indice del elemento recien agregado.) }
    procedure Insert(Index: Integer; Item: TGrupoEfectos);
    {** Inserta un elemento en un puesto en particular, desplazando el resto de
        los elementos subsiguientes.
        @param(Index Indice desdeado para el elemento.)
        @param(Item Elemento a almacenar.) }
    function Add(Item: TGrupoEfectos): Integer;
    {** Grupos de efectos almacenados. }
    property Grupos [Index: Integer]: TGrupoEfectos read GetItem write SetItem; default;
  end { TListaGruposEfectos };

implementation

uses
  SysUtils;

// -----------------------------------------------------------------------------
// TGrupoEfectos
// -----------------------------------------------------------------------------

constructor TGrupoEfectos.Create;
begin { TGrupoEfectos.Create }
  Create(TListaEfectos.Create, TListaOpciones.Create);
end { TGrupoEfectos.Create };

constructor TGrupoEfectos.Create(AEfectos: TListaEfectos; AOpciones: TListaOpciones);
begin { TGrupoEfectos.Create }
  Assert(Assigned(AEfectos), 'TGrupoEfectos.Create: Assigned(AEfectos)');
  Assert(Assigned(AOpciones), 'TGrupoEfectos.Create: Assigned(AOpciones)');

  FEfectos := AEfectos;
  FOpciones := AOpciones;
end { TGrupoEfectos.Create };

destructor TGrupoEfectos.Destroy;
begin { TGrupoEfectos.Destroy }
  try
    FEfectos.Clear;
    FOpciones.Clear;
  finally
    FreeAndNil(FEfectos);
    FreeAndNil(FOpciones);
    inherited Destroy;
  end;
end { TGrupoEfectos.Destroy };

// -----------------------------------------------------------------------------
// TListaGruposEfectos
// -----------------------------------------------------------------------------

destructor TListaGruposEfectos.Destroy;
begin { TListaGruposEfectos.Destroy }
  try
    Clear;
  finally
    inherited Destroy;
  end;
end { TListaGruposEfectos.Destroy };

procedure TListaGruposEfectos.SetItem(Index: Integer; Item: TGrupoEfectos);
begin { TListaGruposEfectos.SetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaGruposEfectos.SetItem: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaGruposEfectos.SetItem: Assigned(Item)');

  inherited SetItem(Index, Item);
end { TListaGruposEfectos.SetItem };

function TListaGruposEfectos.GetItem(Index: Integer): TGrupoEfectos;
begin { TListaGruposEfectos.GetItem }
  Assert((Index >= 0) and (Index < Count), 'TListaGruposEfectos.GetItem: (Index >= 0) and (Index < Count)');

  Result := inherited GetItem(Index) as TGrupoEfectos;
end { TListaGruposEfectos.GetItem };

function TListaGruposEfectos.Add(Item: TGrupoEfectos): Integer;
begin { TListaGruposEfectos.Add }
  Assert(Assigned(Item), 'TListaGruposEfectos.Add: Assigned(Item)');

  Result := inherited Add(Item);
end { TListaGruposEfectos.Add };

procedure TListaGruposEfectos.Insert(Index: Integer; Item: TGrupoEfectos);
begin { TListaGruposEfectos.Insert }
  Assert((Index >= 0) and (Index < Count), 'TListaGruposEfectos.Insert: (Index >= 0) and (Index < Count)');
  Assert(Assigned(Item), 'TListaGruposEfectos.Insert: Assigned(Item)');

  inherited Insert(Index, Item);
end { TListaGruposEfectos.Insert };

end { UnitGrupoEfectos }.

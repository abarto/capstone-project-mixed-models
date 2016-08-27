{**
@abstract(Subrutinas auxiliares de Utilidades Algebraicas.)
@author(Agustin Barto <abarto@gmail.com>)
@created(November 29, 2004)
@lastmod(December 24, 2004)
Este modulo contiene definiciones auxiliares de constantes y subrutinas
auxiliares utilizadas por otros modulos de Utilidades Algebraicas.
}

unit UaMiscelanea;

interface

uses
  UaComun, UaConstantes;

{** Basado en codigo de Numerical Recipes in C - 2nd Edition.

    Given a matrix a[1..n][1..n], this routine replaces it by the LU
    decomposition of a rowwise permutation of itself. a and n are input. a is
    output arranged as in equation (2.3.14) above; indx[1..n] is an output
    vector that records the row permutation effected by the partial pivoting; d
    is output as +-1 depending on whether the number of row interchanges was
    even or odd, respectively. This routine is used in combination with lubksb
    to solve linear equations or invert a matrix.
}
procedure ludcmp(var a: PUaArregloArregloReal; n: Integer; var indx: PUaArregloEntero; var d: TUaReal);
{** Basado en codigo de Numerical Recipes in C - 2nd Edition.

    Solves the set of n linear equations A.X = B. Here a[1..n][1..n] is input,
    not as the matrix A but rather as its LU decomposition, determined by the
    routine ludcmp. indx[1..n] is input as th permutation vector returned by
    ludcmp. b[1..n] is input as the right-hand side vector B, and returns with
    the solution vector X. a, n, and indx are not modified by this routine and
    can be left in place for sccessive calls with different right-hand sides b.
    This routine takes into account the possibility that b will begin with many
    zero elements, so it is efficient for use in matrix inversion.
}
procedure lubksb(var a: PUaArregloArregloReal; n: Integer; var indx: PUaArregloEntero; var b: PUaArregloReal);

{** Basado en codigo de Numerical Recipes in C - 2nd Edition.

    Given a matrix a[1..m][1..n], this routine computes its singular value
    decomposition, A = U.W.VT. The matrix U replaces a on output. The diagonal
    matrix of singular values W is output as a vector w[1..n]. The matrix V (not
    the transpose VT) is output as v[1..n][1..n]. m must be greater or equal
    to n; if it is smaller, the a should be filled up to square with zero
    rows.) }
procedure svdcmp(var a: PUaArregloArregloReal; m,n: Integer; var w: PUaArregloReal; var v: PUaArregloArregloReal);

implementation

uses
  SysUtils;

procedure ludcmp(var a: PUaArregloArregloReal; n: Integer; var indx: PUaArregloEntero; var d: TUaReal);
var
  k, j, imax, i: Integer;
  sum, dum, big: TUaReal;
  vv: PUaArregloReal;
begin { ludcmp }
  Assert(Assigned(a), 'ludcmp: Assigned(a)');
  Assert(Assigned(indx), 'ludcmp: Assigned(indx)');
  Assert(n > 0, 'ludcmp: n > 0');

  vv := UaAsignarPUaArregloReal(n);
  d := 1.0;

  for i := 1 to n do
  begin
    big := 0.0;
    for j := 1 to n do
      if Abs(a^ [i]^ [j]) > big then
        big := Abs(a^ [i]^ [j]);

    if big = 0.0 then
      raise EUaError.Create('ludcmp: Matriz singular.');

    vv^ [i] := 1.0 / big;
  end;

  for j := 1 to n do
  begin
    for i := 1 to j - 1 do
    begin
      sum := a^ [i]^ [j];
      for k := 1 to i - 1 do
        sum := sum - a^ [i]^ [k] * a^ [k]^ [j];
      a^ [i]^ [j] := sum;
    end;

    big := 0.0;

    for i := j to n do
    begin
      sum := a^ [i]^ [j];
      for k := 1 to j - 1 do
        sum := sum - a^ [i]^ [k] * a^ [k]^ [j];
      a^ [i]^ [j] := sum;

      dum := vv^ [i] * Abs(sum);

      if dum >= big then
      begin
        big := dum;
        imax := i;
      end;
    end;

    if j <> imax then
    begin
      for k := 1 to n do
      begin
        dum := a^ [imax]^ [k];
        a^ [imax]^ [k] := a^ [j]^ [k];
        a^ [j]^ [K] := dum;
      end;

      d := -d;
      vv^ [imax] := vv^ [j];
    end;

    indx^ [j] := imax;

    if a^ [j]^ [j] = 0.0 then
      a^ [j]^ [j] := CUaTinyDescomposicionLU;

    if j <> n then
    begin
      dum := 1.0 / a^ [j]^ [j];
      for i := j + 1 to n do
        a^ [i]^ [j] := a^ [i]^ [j] * dum;
    end;
  end;

  UaLiberarPUaArregloReal(vv, n);
end { ludcmp };

procedure lubksb(var a: PUaArregloArregloReal; n: Integer; var indx: PUaArregloEntero; var b: PUaArregloReal);
var
  j, ip, ii, i: Integer;
  sum: TUaReal;
begin { lubksb }
  Assert(Assigned(a), 'lubksb: Assigned(a)');
  Assert(Assigned(b), 'lubksb: Assigned(b)');
  Assert(Assigned(indx), 'lubksb: Assigned(indx)');
  Assert(n > 0, 'lubksb: n > 0');

  ii := 0;

  for i := 1 to n do
  begin
    ip := indx^ [i];
    sum := b^ [ip];
    b^ [ip] := b^ [i];

    if ii <> 0 then
    begin
      for j := ii to i - 1 do
        sum := sum - a^ [i]^ [j] * b^ [j];
    end
    else if sum <> 0.0 then
      ii := i;

    b^ [i] := sum;
  end;

  for i := n downto 1 do
  begin
    sum := b^ [i];
    for j := i + 1 to n do
      sum := sum - a^ [i]^ [j] * b^ [j];
    b^ [i] := sum / a^ [i]^ [i];
  end;
end { lubksb };

procedure svdcmp(var a: PUaArregloArregloReal; m,n: Integer; var w: PUaArregloReal; var v: PUaArregloArregloReal);
var
   nm,l,k,j,jj,its,i,mnmin,flag: Integer;
   z,y,x,scale,s,h,g,f,c,anorm: TUaReal;
   rv1: PUaArregloReal;
begin { svdcmp }
  Assert(Assigned(a), 'svdcmp: Assigned(a)');
  Assert(Assigned(w), 'svdcmp: Assigned(w)');
  Assert(Assigned(v), 'svdcmp: Assigned(v)');
  Assert(m > 0, 'svdcmp: m > 0');
  Assert(n > 0, 'svdcmp: n > 0');

  if m < n then
    raise EUaError.Create('svdcmp: Debe aumentar la matriz con filas de ceros adicionales.')
  else
  begin
    rv1 := UaAsignarPUaArregloReal(n);
    g := 0.0;
    scale := 0.0;
    anorm := 0.0;
    for i := 1 to n do
    begin
      l := i+1;
      rv1^ [i] := scale * g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      if i <= m then
      begin
        for k := i to m do
          scale := scale + Abs(a^ [k]^ [i]);
        if scale <> 0.0 then
        begin
          for k := i to m do
          begin
            a^ [k]^ [i] := a^ [k] ^[i] / scale;
            s := s + a^ [k]^ [i] * a [k]^ [i]
          end;
          f := a^ [i]^ [i];
          g := -UaSigno(Sqrt(s), f);
          h := f * g - s;
          a^ [i]^ [i] := f - g;
          for j := l to n do
          begin
            s := 0.0;
            for k := i to m do
              s := s + a^ [k]^ [i] * a^ [k]^ [j];
            f := s / h;
            for k := i to m do
              a^ [k]^ [j] := a^ [k]^ [j] + f * a^ [k]^ [i]
          end;
          for k := i to m do
            a^ [k]^ [i] := scale * a^ [k]^ [i]
        end
      end;
      w^ [i] := scale * g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      if (i <= m) and (i <> n) then
      begin
        for k := l to n do
           scale := scale + Abs(a^ [i]^ [k]);
        if scale <> 0.0 then
        begin
          for k := l to n do
          begin
            a^ [i]^ [k] := a^ [i]^ [k] / scale;
            s := s + a^ [i]^ [k] * a^ [i]^ [k]
          end;
          f := a^ [i]^ [l];
          g := -UaSigno(Sqrt(s), f);
          h := f * g - s;
          a^ [i]^ [l] := f - g;
          for k := l to n do
            rv1^ [k] := a^ [i]^ [k] / h;
          for j := l to m do
          begin
            s := 0.0;
            for k := l to n do
              s := s + a^ [j]^ [k] * a^ [i]^ [k];
            for k := l to n do
              a^ [j]^ [k] := a^ [j]^ [k] + s * rv1^ [k]
          end;
          for k := l to n do
            a^ [i]^ [k] := scale * a^ [i]^ [k]
        end
      end;
      anorm := UaMaximo(anorm, (Abs(w^ [i]) + Abs(rv1^ [i])))
    end;
    for i := n downto 1 do
    begin
      if i < n then
      begin
        if g <> 0.0 then
        begin
          for j := l to n do
            v^ [j]^ [i] := (a^ [i]^ [j] / a^ [i]^ [l]) / g;
          for j := l to n do
          begin
            s := 0.0;
            for k := l to n do
              s := s + a^ [i]^ [k] * v^ [k]^ [j];
            for k := l to n do
              v^ [k]^ [j] := v [k]^ [j] + s * v^ [k]^ [i]
          end
        end;
        for j := l to n do
        begin
          v^ [i]^ [j] := 0.0;
          v^ [j]^ [i] := 0.0
        end
      end;
      v^ [i]^ [i] := 1.0;
      g := rv1^ [i];
      l := i
    end;
    for i := UaMinimo(m, n) downto 1 do
    begin
      l := i + 1;
      g := w^ [i];
      for j := l to n do
        a^ [i]^ [j] := 0.0;
      if g <> 0.0 then
      begin
        g := 1.0 / g;
        for j := l to n do
        begin
          s := 0.0;
          for k := l to m do
            s := s + a^ [k]^ [i] * a^ [k]^ [j];
          f := (s / a^ [i]^ [i]) * g;
          for k := i to m do
            a^ [k]^ [j] := a^ [k]^ [j] + f * a^ [k]^ [i]
        end;
        for j := i to m do
          a^ [j]^ [i] := a^ [j]^ [i] * g
      end
      else
        for j := i to m do
          a^ [j]^ [i] := 0.0;
      a^ [i]^ [i] := a^ [i]^ [i] + 1.0
    end;
    for k := n downto 1 do
    begin
      for its := 1 to CUaCantidadIteracionesSVD do
      begin
        flag := 1;
        for l := k downto 1 do
        begin
          nm := l-1;
          if Abs(rv1^ [l]) + anorm = anorm then
          begin
            flag := 0;
            break;
          end;
          if Abs(w^ [nm]) + anorm = anorm then
            break;
        end;
        if flag = 0 then
        begin
          c := 0.0;
          s := 1.0;
          for i := l to k do
          begin
            f := s * rv1 ^[i];
            rv1^ [i] := c * rv1^ [i];
            if Abs(f) + anorm = anorm then
              break;
            g := w^ [i];
            h := UaPitagoras(f,g);
            w^ [i] := h;
            h := 1.0 / h;
            c := g * h;
            s := -f * h;
            for j := 1 to m do
            begin
              y := a^ [j]^ [nm];
              z := a^ [j]^ [i];
              a^ [j]^ [nm] := y * c + z * s;
              a^ [j]^ [i] := z * c - y * s
            end
          end;
        end;
        z := w^ [k];
        if l = k then
        begin
          if z < 0.0 then
          begin
            w^ [k] := -z;
            for j := 1 to n do
              v^ [j]^ [k] := -v^ [j]^ [k]
          end;
          break;
        end;
        if its = CUaCantidadIteracionesSVD then
          raise EUaError.Create('svdcmp: No hubo convergencia luego de ' + IntToStr(CUaCantidadIteracionesSVD) + ' iteraciones.');
        x := w^ [l];
        nm := k - 1;
        y := w^ [nm];
        g := rv1^ [nm];
        h := rv1^ [k];
        f := ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
        g := UaPitagoras(f, 1.0);
        f := ((x - z) * (x + z) + h * ((y / (f + UaSigno(g, f))) - h)) / x;
        c := 1.0;
        s := 1.0;
        for j := l to nm do
        begin
          i := j + 1;
          g := rv1^ [i];
          y := w^ [i];
          h := s * g;
          g := c * g;
          z := UaPitagoras(f, h);
          rv1^ [j] := z;
          c := f / z;
          s := h / z;
          f := x * c + g * s;
          g := g * c - x * s;
          h := y * s;
          y := y * c;
          for jj := 1 to n do
          begin
            x := v^ [jj, j];
            z := v^ [jj, i];
            v^ [jj, j] := x * c + z * s;
            v^ [jj, i] := z * c - x * s
          end;
          z := UaPitagoras(f, h);
          w^ [j] := z;
          if z <> 0.0 then
          begin
            z := 1.0 / z;
            c := f * z;
            s := h * z
          end;
          f := c * g + s * y;
          x := c * y - s * g;
          for jj := 1 to m do
          begin
            y := a^ [jj, j];
            z := a^ [jj, i];
            a^ [jj,j] := y * c + z * s;
            a^ [jj,i] := z * c - y * s
          end
        end;
        rv1^ [l] := 0.0;
        rv1^ [k] := f;
        w^ [k] := x
      end;
    end;
    UaLiberarPUaArregloReal(rv1, n);
  end;
end { svdcmp };

end { UaMiscelanea }.

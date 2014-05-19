#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <complex.h>
#include <math.h>

typedef uint32_t Int;
typedef float Flt;
typedef bool  Bol;
typedef float complex Cmx;

Cmx cmx(Flt r , Flt i)
{
  return (r + i * I);
}
  
Flt realPart(Cmx c)
{
  return (crealf(c));
}

Flt imagPart(Cmx c)
{
  return (cimagf(c));
}

Bol eqlBol(Bol l , Bol r)
{
  return (l == r);
} 

Bol eqlInt(Int l , Int r)
{
  return (l == r);
} 

Bol eqlFlt(Flt l , Flt r)
{
  return (l == r);
} 

Bol ltdBol(Bol l , Bol r)
{
  return (l < r);
}

Bol ltdInt(Int l , Int r)
{
  return (l < r);
} 

Bol ltdFlt(Flt l , Flt r)
{
  return (l < r);
}

Int addInt(Int l , Int r)
{
  return (l + r);
} 

Int subInt(Int l , Int r)
{
  return (l - r);
} 

Int mulInt(Int l , Int r)
{
  return (l * r);
} 

Int divInt (Int l , Int r)
{
  return (l / r);
}

Flt addFlt(Flt l , Flt r)
{
  return (l + r);
} 

Flt subFlt(Flt l , Flt r)
{
  return (l - r);
} 

Flt mulFlt(Flt l , Flt r)
{
  return (l * r);
} 

Flt divFlt (Flt l , Flt r)
{
  return (l / r);
}

Cmx addCmx(Cmx l , Cmx r)
{
  return (l + r);
} 

Cmx subCmx(Cmx l , Cmx r)
{
  return (l - r);
} 

Cmx mulCmx(Cmx l , Cmx r)
{
  return (l * r);
} 

Cmx divCmx (Cmx l , Cmx r)
{
  return (l / r);
}

Int andInt (Int l , Int r)
{
  return (l & r);
}

Int orInt (Int l , Int r)
{
  return (l | r);
}

Int xorInt (Int l , Int r)
{
  return (l ^ r);
}

Int shrInt (Int l , Int r)
{
  return (l >> r);
}

Int shlInt (Int l , Int r)
{
  return (l << r);
}

Int cmpInt (Int i)
{
  return (~ i);
}

Flt i2f (Int i)
{
  return (Flt) i;
}

Cmx cis (Flt f)
{
  return cosf(f) + sinf(f) * I;
}

Int ilog2 (Int i)
{
  return ((Int)(floorf(log2f(i2f(i)))));
}

Flt sqrtFlt (Flt f)
{
  return sqrtf(f);
}

typedef struct 
{
  Int fst;
  Int snd;
} TplIntInt;

TplIntInt newTplIntInt(Int f , Int s) 
{
  return ((TplIntInt) {.fst = f , .snd = s});
}

Int fstTplIntInt(TplIntInt t)
{
  return t.fst;
}

Int sndTplIntInt(TplIntInt t)
{
  return t.snd;
}

typedef struct 
{
  Int fst;
  Flt snd;
} TplIntFlt;

TplIntFlt newTplIntFlt(Int f , Flt s) 
{
  return ((TplIntFlt) {.fst = f , .snd = s});
}

Int fstTplIntFlt(TplIntFlt t)
{
  return t.fst;
}

Flt sndTplIntFlt(TplIntFlt t)
{
  return t.snd;
}

typedef struct
{
  Int   size;
  Int*  elems;
} AryInt;

Int lenAryInt(AryInt a)
{
  return a.size;
}

Int indAryInt(AryInt a , Int i)
{
  return a.elems[i];
}

AryInt setAryInt(AryInt a , Int i , Int e)
{
  a.elems [i] = e;
  return a;
}

AryInt newAryInt(Int s)
{
  return ((AryInt) {.size = s , .elems = malloc(s * sizeof(Int))});
}

typedef struct
{
  Int   size;
  Cmx*  elems;
} AryCmx;

Int lenAryCmx(AryCmx a)
{
  return a.size;
}

Cmx indAryCmx(AryCmx a , Int i)
{
  return a.elems[i];
}

AryCmx setAryCmx(AryCmx a , Int i , Cmx e)
{
  a.elems [i] = e;
  return a;
}

AryCmx newAryCmx(Int s)
{
  return ((AryCmx) {.size = s , .elems = malloc(s * sizeof(Cmx))});
}

typedef struct 
{
  Int fst;
  AryCmx snd;
} TplIntAryCmx;

TplIntAryCmx newTplIntAryCmx(Int f , AryCmx s) 
{
  return ((TplIntAryCmx) {.fst = f , .snd = s});
}

Int fstTplIntAryCmx(TplIntAryCmx t)
{
  return t.fst;
}

AryCmx sndTplIntAryCmx(TplIntAryCmx t)
{
  return t.snd;
}

typedef struct
{
  Int   size;
  Flt*  elems;
} AryFlt;

Int lenAryFlt(AryFlt a)
{
  return a.size;
}

Flt indAryFlt(AryFlt a , Int i)
{
  return a.elems[i];
}

AryFlt setAryFlt(AryFlt a , Int i , Flt e)
{
  a.elems [i] = e;
  return a;
}

AryFlt newAryFlt(Int s)
{
  return ((AryFlt) {.size = s , .elems = malloc(s * sizeof(Flt))});
}

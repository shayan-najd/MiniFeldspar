#include "header.h"
#include "ppm.h"

void func (AryCmx v0, AryCmx *out)
{
  Bol v10;
  Cmx v9;
  Cmx v8;
  AryCmx v7;
  Int v6;
  TplIntAryCmx v5;
  Cmx v4;
  AryCmx v3;
  Int v2;
  TplIntAryCmx v1;
  {
    {
      v5 = newTplIntAryCmx (0u, v0);
      while (ltdInt (fstTplIntAryCmx (v5), addInt (subInt (ilog2 (lenAryCmx (v0)), 1u), 1u)))
      {
        {
          v6 = lenAryCmx (v0);
          v7 = newAryCmx (v6);
          v8 = 0u;
          while (ltdInt (v8, v6))
          {
            {
              if (eqlInt (andInt (v8, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v5)))), 0u))
              v10 = false;
              else
              v10 = true;
              if (v10)
              v9 = mulCmx (cis (divFlt (mulFlt (-3.1415927f, i2f (andInt (v8, cmpInt (shlInt (cmpInt (0u), subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v5))))))), i2f (shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v5)))))), subCmx (indAryCmx (sndTplIntAryCmx (v5), xorInt (v8, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v5))))), indAryCmx (sndTplIntAryCmx (v5), v8)));
              else
              v9 = addCmx (indAryCmx (sndTplIntAryCmx (v5), v8), indAryCmx (sndTplIntAryCmx (v5), xorInt (v8, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v5))))));
            }
            v7 = setAryCmx (v7, v8, v9);
            v8 = addInt (v8, 1u);
          }
        }
        v5 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v5), 1u), v7);
      }
    }
    v1 = newTplIntAryCmx (0u, sndTplIntAryCmx (v5));
    while (ltdInt (fstTplIntAryCmx (v1), subInt (ilog2 (lenAryCmx (v0)), 1u)))
    {
      {
        v2 = lenAryCmx (sndTplIntAryCmx (v1));
        v3 = newAryCmx (v2);
        v4 = 0u;
        while (ltdInt (v4, v2))
        {
          v3 = setAryCmx (v3, v4, indAryCmx (sndTplIntAryCmx (v1), orInt (shlInt (orInt (shlInt (shrInt (shrInt (v4, 1u), addInt (fstTplIntAryCmx (v1), 1u)), 1u), andInt (v4, 1u)), addInt (fstTplIntAryCmx (v1), 1u)), andInt (shrInt (v4, 1u), cmpInt (shlInt (cmpInt (0u), addInt (fstTplIntAryCmx (v1), 1u)))))));
          v4 = addInt (v4, 1u);
        }
      }
      v1 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v1), 1u), v3);
    }
  }
  *out = sndTplIntAryCmx (v1);
}

int main()
{
  Image   imgIn = readImage("Image.pgm");
  AryCmx  aryIn = newAryCmx(size(imgIn)); 
  for (Int i = 0; i < size(imgIn); i++)
    aryIn = setAryCmx(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));
  AryCmx aryOut;
  func(aryIn , &aryOut);
  Image imgOut = {.sizeX = imgIn.sizeX, 
                  .sizeY = imgIn.sizeY,
                  .type  = 2,
                  .data  = malloc(lenAryCmx(aryOut) * sizeof(Int))}; 
  for(Int i = 0; i < lenAryCmx(aryOut); i++)
    imgOut.data[i] = (Int)(floorf(cabsf(indAryCmx(aryOut , i))));
  writeImage ("ImageFFT.pgm" , imgOut);
  return 0;
}

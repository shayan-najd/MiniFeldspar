#include"ppm.h"
#include "header.h"

void func (AryCmx v0, AryCmx *out)
{
  Cmx v19;
  Bol v18;
  Cmx v17;
  Cmx v16;
  AryCmx v15;
  Int v14;
  Cmx v13;
  Bol v12;
  Cmx v11;
  Cmx v10;
  AryCmx v9;
  Int v8;
  AryCmx v7;
  Bol v6;
  Int v5;
  Int v4;
  Int v3;
  Int v2;
  Int v1;
  v1 = lenAryCmx (v0);
  v2 = divInt (v1, 4u);
  v3 = subInt (v1, v2);
  v4 = addInt (v3, v1);
  v5 = lenAryCmx (v0);
  v6 = ltdInt (v4, v5);
  if (v6)
  {
    v8 = v4;
    v9 = newAryCmx (v8);
    v10 = 0u;
    while (ltdInt (v10, v8))
    {
      v11 = indAryCmx (v0, v10);
      v12 = ltdInt (v10, v3);
      if (v12)
      {
        v13 = mulCmx (cmx (1.0f, 0.0f), v11);
      }
      else
      {
        v13 = mulCmx (cmx (0.0f, 0.0f), v11);
      }
      v9 = setAryCmx (v9, v10, v13);
      v10 = addInt (v10, 1u);
    }
    v7 = v9;
  }
  else
  {
    v14 = v5;
    v15 = newAryCmx (v14);
    v16 = 0u;
    while (ltdInt (v16, v14))
    {
      v17 = indAryCmx (v0, v16);
      v18 = ltdInt (v16, v3);
      if (v18)
      {
        v19 = mulCmx (cmx (1.0f, 0.0f), v17);
      }
      else
      {
        v19 = mulCmx (cmx (0.0f, 0.0f), v17);
      }
      v15 = setAryCmx (v15, v16, v19);
      v16 = addInt (v16, 1u);
    }
    v7 = v15;
  }
  *out = v7;
}
int main()
{
  Image   imgIn = readImage("ImageBig.pgm");
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
  writeImage ("ImageWindowing.pgm" , imgOut);
  return 0;
}
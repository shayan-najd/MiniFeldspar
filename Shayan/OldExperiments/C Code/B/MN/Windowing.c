#include"ppm.h"
#include "header.h"

void func (AryCmx v0, AryCmx *out)
{
  Cmx v29;
  Cmx v28;
  Cmx v27;
  Bol v26;
  Int v25;
  Int v24;
  Cmx v23;
  AryCmx v22;
  Int v21;
  Int v20;
  Cmx v19;
  Cmx v18;
  Cmx v17;
  Bol v16;
  Int v15;
  Int v14;
  Cmx v13;
  AryCmx v12;
  Int v11;
  Int v10;
  Int v9;
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
    v8 = divInt (v1, 4u);
    v9 = subInt (v1, v8);
    v10 = addInt (v9, v1);
    v11 = v10;
    v12 = newAryCmx (v11);
    v13 = 0u;
    while (ltdInt (v13, v11))
    {
      v14 = divInt (v1, 4u);
      v15 = subInt (v1, v14);
      v16 = ltdInt (v13, v15);
      if (v16)
      {
        v18 = indAryCmx (v0, v13);
        v17 = mulCmx (cmx (1.0f, 0.0f), v18);
      }
      else
      {
        v19 = indAryCmx (v0, v13);
        v17 = mulCmx (cmx (0.0f, 0.0f), v19);
      }
      v12 = setAryCmx (v12, v13, v17);
      v13 = addInt (v13, 1u);
    }
    v7 = v12;
  }
  else
  {
    v20 = lenAryCmx (v0);
    v21 = v20;
    v22 = newAryCmx (v21);
    v23 = 0u;
    while (ltdInt (v23, v21))
    {
      v24 = divInt (v1, 4u);
      v25 = subInt (v1, v24);
      v26 = ltdInt (v23, v25);
      if (v26)
      {
        v28 = indAryCmx (v0, v23);
        v27 = mulCmx (cmx (1.0f, 0.0f), v28);
      }
      else
      {
        v29 = indAryCmx (v0, v23);
        v27 = mulCmx (cmx (0.0f, 0.0f), v29);
      }
      v22 = setAryCmx (v22, v23, v27);
      v23 = addInt (v23, 1u);
    }
    v7 = v22;
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
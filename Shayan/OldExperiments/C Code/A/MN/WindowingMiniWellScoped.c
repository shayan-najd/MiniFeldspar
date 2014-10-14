#include"ppm.h"
#include "header.h"

void func (AryCmx v0, AryCmx *out)
{
  Cmx v38;
  Cmx v37;
  Cmx v36;
  Bol v35;
  Int v34;
  Int v33;
  Int v32;
  Int v31;
  Cmx v30;
  AryCmx v29;
  Int v28;
  Int v27;
  Cmx v26;
  Cmx v25;
  Cmx v24;
  Bol v23;
  Int v22;
  Int v21;
  Int v20;
  Int v19;
  Cmx v18;
  AryCmx v17;
  Int v16;
  Int v15;
  Int v14;
  Int v13;
  Int v12;
  Int v11;
  Int v10;
  AryCmx v9;
  Bol v8;
  Int v7;
  Int v6;
  Int v5;
  Int v4;
  Int v3;
  Int v2;
  Int v1;
  v1 = lenAryCmx (v0);
  v2 = lenAryCmx (v0);
  v3 = divInt (v2, 4u);
  v4 = subInt (v1, v3);
  v5 = lenAryCmx (v0);
  v6 = addInt (v4, v5);
  v7 = lenAryCmx (v0);
  v8 = ltdInt (v6, v7);
  if (v8)
  {
    v10 = lenAryCmx (v0);
    v11 = lenAryCmx (v0);
    v12 = divInt (v11, 4u);
    v13 = subInt (v10, v12);
    v14 = lenAryCmx (v0);
    v15 = addInt (v13, v14);
    v16 = v15;
    v17 = newAryCmx (v16);
    v18 = 0u;
    while (ltdInt (v18, v16))
    {
      v19 = lenAryCmx (v0);
      v20 = lenAryCmx (v0);
      v21 = divInt (v20, 4u);
      v22 = subInt (v19, v21);
      v23 = ltdInt (v18, v22);
      if (v23)
      {
        v25 = indAryCmx (v0, v18);
        v24 = mulCmx (cmx (1.0f, 0.0f), v25);
      }
      else
      {
        v26 = indAryCmx (v0, v18);
        v24 = mulCmx (cmx (0.0f, 0.0f), v26);
      }
      v17 = setAryCmx (v17, v18, v24);
      v18 = addInt (v18, 1u);
    }
    v9 = v17;
  }
  else
  {
    v27 = lenAryCmx (v0);
    v28 = v27;
    v29 = newAryCmx (v28);
    v30 = 0u;
    while (ltdInt (v30, v28))
    {
      v31 = lenAryCmx (v0);
      v32 = lenAryCmx (v0);
      v33 = divInt (v32, 4u);
      v34 = subInt (v31, v33);
      v35 = ltdInt (v30, v34);
      if (v35)
      {
        v37 = indAryCmx (v0, v30);
        v36 = mulCmx (cmx (1.0f, 0.0f), v37);
      }
      else
      {
        v38 = indAryCmx (v0, v30);
        v36 = mulCmx (cmx (0.0f, 0.0f), v38);
      }
      v29 = setAryCmx (v29, v30, v36);
      v30 = addInt (v30, 1u);
    }
    v9 = v29;
  }
  *out = v9;
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
#include"ppm.h"
#include "header.h"

void func (AryCmx v0, AryCmx *out)
{
  Int v59;
  Int v58;
  Int v57;
  Int v56;
  Int v55;
  Int v54;
  Int v53;
  Int v52;
  Int v51;
  Int v50;
  Int v49;
  Int v48;
  Int v47;
  Cmx v46;
  AryCmx v45;
  Int v44;
  Int v43;
  Int v42;
  AryCmx v41;
  Int v40;
  Int v39;
  Int v38;
  TplIntAryCmx v37;
  TplIntAryCmx v36;
  AryCmx v35;
  Cmx v34;
  Cmx v33;
  Bol v32;
  Int v31;
  Int v30;
  Cmx v29;
  Int v28;
  Cmx v27;
  Cmx v26;
  Flt v25;
  Flt v24;
  Flt v23;
  Int v22;
  Int v21;
  Int v20;
  Int v19;
  Flt v18;
  Int v17;
  Int v16;
  Cmx v15;
  AryCmx v14;
  Int v13;
  Int v12;
  Int v11;
  AryCmx v10;
  Int v9;
  Int v8;
  Int v7;
  TplIntAryCmx v6;
  TplIntAryCmx v5;
  Int v4;
  Int v3;
  Int v2;
  Int v1;
  v1 = lenAryCmx (v0);
  v2 = ilog2 (v1);
  v3 = subInt (v2, 1u);
  v4 = addInt (v3, 1u);
  v6 = newTplIntAryCmx (0u, v0);
  v7 = fstTplIntAryCmx (v6);
  while (ltdInt (v7, v4))
  {
    v8 = fstTplIntAryCmx (v6);
    v9 = addInt (v8, 1u);
    v10 = sndTplIntAryCmx (v6);
    v11 = fstTplIntAryCmx (v6);
    v12 = lenAryCmx (v0);
    v13 = v12;
    v14 = newAryCmx (v13);
    v15 = 0u;
    while (ltdInt (v15, v13))
    {
      v16 = subInt (v3, v11);
      v17 = shlInt (1u, v16);
      v18 = i2f (v17);
      v19 = cmpInt (0u);
      v20 = shlInt (v19, v16);
      v21 = cmpInt (v20);
      v22 = andInt (v15, v21);
      v23 = i2f (v22);
      v24 = mulFlt (-3.1415927f, v23);
      v25 = divFlt (v24, v18);
      v26 = cis (v25);
      v27 = indAryCmx (v10, v15);
      v28 = xorInt (v15, v17);
      v29 = indAryCmx (v10, v28);
      v30 = shlInt (1u, v16);
      v31 = andInt (v15, v30);
      v32 = eqlInt (v31, 0u);
      if (v32)
      {
        v33 = addCmx (v27, v29);
      }
      else
      {
        v34 = subCmx (v29, v27);
        v33 = mulCmx (v26, v34);
      }
      v14 = setAryCmx (v14, v15, v33);
      v15 = addInt (v15, 1u);
    }
    v6 = newTplIntAryCmx (v9, v14);
    v7 = fstTplIntAryCmx (v6);
  }
  v5 = v6;
  v35 = sndTplIntAryCmx (v5);
  v37 = newTplIntAryCmx (0u, v35);
  v38 = fstTplIntAryCmx (v37);
  while (ltdInt (v38, v3))
  {
    v39 = fstTplIntAryCmx (v37);
    v40 = addInt (v39, 1u);
    v41 = sndTplIntAryCmx (v37);
    v42 = fstTplIntAryCmx (v37);
    v43 = lenAryCmx (v41);
    v44 = v43;
    v45 = newAryCmx (v44);
    v46 = 0u;
    while (ltdInt (v46, v44))
    {
      v47 = addInt (v42, 1u);
      v48 = shrInt (v46, 1u);
      v49 = cmpInt (0u);
      v50 = shlInt (v49, v47);
      v51 = cmpInt (v50);
      v52 = andInt (v48, v51);
      v53 = andInt (v46, 1u);
      v54 = shrInt (v46, 1u);
      v55 = shrInt (v54, v47);
      v56 = shlInt (v55, 1u);
      v57 = orInt (v56, v53);
      v58 = shlInt (v57, v47);
      v59 = orInt (v58, v52);
      v45 = setAryCmx (v45, v46, indAryCmx (v41, v59));
      v46 = addInt (v46, 1u);
    }
    v37 = newTplIntAryCmx (v40, v45);
    v38 = fstTplIntAryCmx (v37);
  }
  v36 = v37;
  *out = sndTplIntAryCmx (v36);
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
  writeImage ("ImageFFT.pgm" , imgOut);
  return 0;
}
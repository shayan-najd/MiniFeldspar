#include"ppm.h"
#include "header.h"

void func (AryCmx v0, AryCmx *out)
{
  Cmx v96;
  AryCmx v95;
  Int v94;
  Int v93;
  AryCmx v92;
  Cmx v91;
  AryCmx v90;
  Int v89;
  Int v88;
  Int v87;
  Int v86;
  Int v85;
  Int v84;
  Int v83;
  Int v82;
  Int v81;
  Int v80;
  Int v79;
  Int v78;
  Int v77;
  Int v76;
  Int v75;
  Int v74;
  Int v73;
  Int v72;
  Int v71;
  Cmx v70;
  AryCmx v69;
  Int v68;
  Int v67;
  AryCmx v66;
  Int v65;
  Int v64;
  Int v63;
  TplIntAryCmx v62;
  TplIntAryCmx v61;
  Int v60;
  AryCmx v59;
  Cmx v58;
  AryCmx v57;
  Int v56;
  Cmx v55;
  Cmx v54;
  Flt v53;
  Flt v52;
  Int v51;
  Int v50;
  Int v49;
  Int v48;
  Int v47;
  Int v46;
  Flt v45;
  Flt v44;
  Int v43;
  Int v42;
  Int v41;
  Int v40;
  Int v39;
  Int v38;
  Int v37;
  Int v36;
  Int v35;
  Cmx v34;
  Bol v33;
  Int v32;
  Int v31;
  Int v30;
  Int v29;
  Cmx v28;
  Int v27;
  Int v26;
  Int v25;
  Int v24;
  Int v23;
  Int v22;
  Int v21;
  AryCmx v20;
  Cmx v19;
  AryCmx v18;
  Cmx v17;
  AryCmx v16;
  Int v15;
  Int v14;
  Int v13;
  Int v12;
  Int v11;
  Int v10;
  TplIntAryCmx v9;
  TplIntAryCmx v8;
  Int v7;
  Int v6;
  Int v5;
  Int v4;
  Int v3;
  Int v2;
  Int v1;
  v1 = lenAryCmx (v0);
  v2 = ilog2 (v1);
  v3 = subInt (v2, 1u);
  v4 = lenAryCmx (v0);
  v5 = ilog2 (v4);
  v6 = subInt (v5, 1u);
  v7 = lenAryCmx (v0);
  v56 = v7;
  v57 = newAryCmx (v56);
  v58 = 0u;
  while (ltdInt (v58, v56))
  {
    v57 = setAryCmx (v57, v58, indAryCmx (v0, v58));
    v58 = addInt (v58, 1u);
  }
  v9 = newTplIntAryCmx (0u, v57);
  v10 = fstTplIntAryCmx (v9);
  v11 = addInt (v6, 1u);
  while (ltdInt (v10, v11))
  {
    v12 = fstTplIntAryCmx (v9);
    v13 = addInt (v12, 1u);
    v14 = lenAryCmx (v0);
    v15 = v14;
    v16 = newAryCmx (v15);
    v17 = 0u;
    while (ltdInt (v17, v15))
    {
      v18 = sndTplIntAryCmx (v9);
      v19 = indAryCmx (v18, v17);
      v20 = sndTplIntAryCmx (v9);
      v21 = lenAryCmx (v0);
      v22 = ilog2 (v21);
      v23 = subInt (v22, 1u);
      v24 = fstTplIntAryCmx (v9);
      v25 = subInt (v23, v24);
      v26 = shlInt (1u, v25);
      v27 = xorInt (v17, v26);
      v28 = indAryCmx (v20, v27);
      v29 = fstTplIntAryCmx (v9);
      v30 = subInt (v6, v29);
      v31 = shlInt (1u, v30);
      v32 = andInt (v17, v31);
      v33 = eqlInt (v32, 0u);
      if (v33)
      {
        v34 = addCmx (v19, v28);
      }
      else
      {
        v35 = cmpInt (0u);
        v36 = lenAryCmx (v0);
        v37 = ilog2 (v36);
        v38 = subInt (v37, 1u);
        v39 = fstTplIntAryCmx (v9);
        v40 = subInt (v38, v39);
        v41 = shlInt (v35, v40);
        v42 = cmpInt (v41);
        v43 = andInt (v17, v42);
        v44 = i2f (v43);
        v45 = mulFlt (-3.1415927f, v44);
        v46 = lenAryCmx (v0);
        v47 = ilog2 (v46);
        v48 = subInt (v47, 1u);
        v49 = fstTplIntAryCmx (v9);
        v50 = subInt (v48, v49);
        v51 = shlInt (1u, v50);
        v52 = i2f (v51);
        v53 = divFlt (v45, v52);
        v54 = cis (v53);
        v55 = subCmx (v28, v19);
        v34 = mulCmx (v54, v55);
      }
      v16 = setAryCmx (v16, v17, v34);
      v17 = addInt (v17, 1u);
    }
    v9 = newTplIntAryCmx (v13, v16);
    v10 = fstTplIntAryCmx (v9);
    v11 = addInt (v6, 1u);
  }
  v8 = v9;
  v59 = sndTplIntAryCmx (v8);
  v60 = lenAryCmx (v59);
  v89 = v60;
  v90 = newAryCmx (v89);
  v91 = 0u;
  while (ltdInt (v91, v89))
  {
    v90 = setAryCmx (v90, v91, indAryCmx (v59, v91));
    v91 = addInt (v91, 1u);
  }
  v62 = newTplIntAryCmx (0u, v90);
  v63 = fstTplIntAryCmx (v62);
  while (ltdInt (v63, v3))
  {
    v64 = fstTplIntAryCmx (v62);
    v65 = addInt (v64, 1u);
    v66 = sndTplIntAryCmx (v62);
    v67 = lenAryCmx (v66);
    v68 = v67;
    v69 = newAryCmx (v68);
    v70 = 0u;
    while (ltdInt (v70, v68))
    {
      v71 = shrInt (v70, 1u);
      v72 = fstTplIntAryCmx (v62);
      v73 = addInt (v72, 1u);
      v74 = shrInt (v71, v73);
      v75 = shlInt (v74, 1u);
      v76 = andInt (v70, 1u);
      v77 = orInt (v75, v76);
      v78 = fstTplIntAryCmx (v62);
      v79 = addInt (v78, 1u);
      v80 = shlInt (v77, v79);
      v81 = shrInt (v70, 1u);
      v82 = cmpInt (0u);
      v83 = fstTplIntAryCmx (v62);
      v84 = addInt (v83, 1u);
      v85 = shlInt (v82, v84);
      v86 = cmpInt (v85);
      v87 = andInt (v81, v86);
      v88 = orInt (v80, v87);
      v69 = setAryCmx (v69, v70, indAryCmx (v66, v88));
      v70 = addInt (v70, 1u);
    }
    v62 = newTplIntAryCmx (v65, v69);
    v63 = fstTplIntAryCmx (v62);
  }
  v61 = v62;
  v92 = sndTplIntAryCmx (v61);
  v93 = lenAryCmx (v92);
  v94 = v93;
  v95 = newAryCmx (v94);
  v96 = 0u;
  while (ltdInt (v96, v94))
  {
    v95 = setAryCmx (v95, v96, indAryCmx (v92, v96));
    v96 = addInt (v96, 1u);
  }
  *out = v95;
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
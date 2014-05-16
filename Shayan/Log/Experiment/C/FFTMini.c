#include "header.h"
#include "ppm.h"

void func (AryCmx v0, AryCmx *out)
{
  Cmx v53;
  AryCmx v52;
  Int v51;
  Bol v50;
  Cmx v49;
  Cmx v48;
  AryCmx v47;
  Int v46;
  TplIntAryCmx v45;
  Cmx v44;
  AryCmx v43;
  Int v42;
  Bol v41;
  Cmx v40;
  Cmx v39;
  AryCmx v38;
  Int v37;
  TplIntAryCmx v36;
  Cmx v35;
  AryCmx v34;
  Int v33;
  Cmx v32;
  AryCmx v31;
  Int v30;
  TplIntAryCmx v29;
  Cmx v28;
  AryCmx v27;
  Int v26;
  Bol v25;
  Cmx v24;
  Cmx v23;
  AryCmx v22;
  Int v21;
  TplIntAryCmx v20;
  Cmx v19;
  AryCmx v18;
  Int v17;
  Bol v16;
  Cmx v15;
  Cmx v14;
  AryCmx v13;
  Int v12;
  TplIntAryCmx v11;
  Cmx v10;
  AryCmx v9;
  Int v8;
  Cmx v7;
  AryCmx v6;
  Int v5;
  TplIntAryCmx v4;
  Cmx v3;
  AryCmx v2;
  Int v1;
  {
    {
      {
        {
          {
            v17 = lenAryCmx (v0);
            v18 = newAryCmx (v17);
            v19 = 0u;
            while (ltdInt (v19, v17))
            {
              v18 = setAryCmx (v18, v19, indAryCmx (v0, v19));
              v19 = addInt (v19, 1u);
            }
          }
          v11 = newTplIntAryCmx (0u, v18);
          while (ltdInt (fstTplIntAryCmx (v11), addInt (subInt (ilog2 (lenAryCmx (v0)), 1u), 1u)))
          {
            {
              v12 = lenAryCmx (v0);
              v13 = newAryCmx (v12);
              v14 = 0u;
              while (ltdInt (v14, v12))
              {
                {
                  if (eqlInt (andInt (v14, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v11)))), 0u))
                  v16 = false;
                  else
                  v16 = true;
                  if (v16)
                  v15 = mulCmx (cis (divFlt (mulFlt (-3.1415927f, i2f (andInt (v14, cmpInt (shlInt (cmpInt (0u), subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v11))))))), i2f (shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v11)))))), subCmx (indAryCmx (sndTplIntAryCmx (v11), xorInt (v14, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v11))))), indAryCmx (sndTplIntAryCmx (v11), v14)));
                  else
                  v15 = addCmx (indAryCmx (sndTplIntAryCmx (v11), v14), indAryCmx (sndTplIntAryCmx (v11), xorInt (v14, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v11))))));
                }
                v13 = setAryCmx (v13, v14, v15);
                v14 = addInt (v14, 1u);
              }
            }
            v11 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v11), 1u), v13);
          }
        }
        v8 = lenAryCmx (sndTplIntAryCmx (v11));
        v9 = newAryCmx (v8);
        v10 = 0u;
        while (ltdInt (v10, v8))
        {
          {
            {
              v26 = lenAryCmx (v0);
              v27 = newAryCmx (v26);
              v28 = 0u;
              while (ltdInt (v28, v26))
              {
                v27 = setAryCmx (v27, v28, indAryCmx (v0, v28));
                v28 = addInt (v28, 1u);
              }
            }
            v20 = newTplIntAryCmx (0u, v27);
            while (ltdInt (fstTplIntAryCmx (v20), addInt (subInt (ilog2 (lenAryCmx (v0)), 1u), 1u)))
            {
              {
                v21 = lenAryCmx (v0);
                v22 = newAryCmx (v21);
                v23 = 0u;
                while (ltdInt (v23, v21))
                {
                  {
                    if (eqlInt (andInt (v23, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v20)))), 0u))
                    v25 = false;
                    else
                    v25 = true;
                    if (v25)
                    v24 = mulCmx (cis (divFlt (mulFlt (-3.1415927f, i2f (andInt (v23, cmpInt (shlInt (cmpInt (0u), subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v20))))))), i2f (shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v20)))))), subCmx (indAryCmx (sndTplIntAryCmx (v20), xorInt (v23, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v20))))), indAryCmx (sndTplIntAryCmx (v20), v23)));
                    else
                    v24 = addCmx (indAryCmx (sndTplIntAryCmx (v20), v23), indAryCmx (sndTplIntAryCmx (v20), xorInt (v23, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v20))))));
                  }
                  v22 = setAryCmx (v22, v23, v24);
                  v23 = addInt (v23, 1u);
                }
              }
              v20 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v20), 1u), v22);
            }
          }
          v9 = setAryCmx (v9, v10, indAryCmx (sndTplIntAryCmx (v20), v10));
          v10 = addInt (v10, 1u);
        }
      }
      v4 = newTplIntAryCmx (0u, v9);
      while (ltdInt (fstTplIntAryCmx (v4), subInt (ilog2 (lenAryCmx (v0)), 1u)))
      {
        {
          v5 = lenAryCmx (sndTplIntAryCmx (v4));
          v6 = newAryCmx (v5);
          v7 = 0u;
          while (ltdInt (v7, v5))
          {
            v6 = setAryCmx (v6, v7, indAryCmx (sndTplIntAryCmx (v4), orInt (shlInt (orInt (shlInt (shrInt (shrInt (v7, 1u), addInt (fstTplIntAryCmx (v4), 1u)), 1u), andInt (v7, 1u)), addInt (fstTplIntAryCmx (v4), 1u)), andInt (shrInt (v7, 1u), cmpInt (shlInt (cmpInt (0u), addInt (fstTplIntAryCmx (v4), 1u)))))));
            v7 = addInt (v7, 1u);
          }
        }
        v4 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v4), 1u), v6);
      }
    }
    v1 = lenAryCmx (sndTplIntAryCmx (v4));
    v2 = newAryCmx (v1);
    v3 = 0u;
    while (ltdInt (v3, v1))
    {
      {
        {
          {
            {
              v42 = lenAryCmx (v0);
              v43 = newAryCmx (v42);
              v44 = 0u;
              while (ltdInt (v44, v42))
              {
                v43 = setAryCmx (v43, v44, indAryCmx (v0, v44));
                v44 = addInt (v44, 1u);
              }
            }
            v36 = newTplIntAryCmx (0u, v43);
            while (ltdInt (fstTplIntAryCmx (v36), addInt (subInt (ilog2 (lenAryCmx (v0)), 1u), 1u)))
            {
              {
                v37 = lenAryCmx (v0);
                v38 = newAryCmx (v37);
                v39 = 0u;
                while (ltdInt (v39, v37))
                {
                  {
                    if (eqlInt (andInt (v39, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v36)))), 0u))
                    v41 = false;
                    else
                    v41 = true;
                    if (v41)
                    v40 = mulCmx (cis (divFlt (mulFlt (-3.1415927f, i2f (andInt (v39, cmpInt (shlInt (cmpInt (0u), subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v36))))))), i2f (shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v36)))))), subCmx (indAryCmx (sndTplIntAryCmx (v36), xorInt (v39, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v36))))), indAryCmx (sndTplIntAryCmx (v36), v39)));
                    else
                    v40 = addCmx (indAryCmx (sndTplIntAryCmx (v36), v39), indAryCmx (sndTplIntAryCmx (v36), xorInt (v39, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v36))))));
                  }
                  v38 = setAryCmx (v38, v39, v40);
                  v39 = addInt (v39, 1u);
                }
              }
              v36 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v36), 1u), v38);
            }
          }
          v33 = lenAryCmx (sndTplIntAryCmx (v36));
          v34 = newAryCmx (v33);
          v35 = 0u;
          while (ltdInt (v35, v33))
          {
            {
              {
                v51 = lenAryCmx (v0);
                v52 = newAryCmx (v51);
                v53 = 0u;
                while (ltdInt (v53, v51))
                {
                  v52 = setAryCmx (v52, v53, indAryCmx (v0, v53));
                  v53 = addInt (v53, 1u);
                }
              }
              v45 = newTplIntAryCmx (0u, v52);
              while (ltdInt (fstTplIntAryCmx (v45), addInt (subInt (ilog2 (lenAryCmx (v0)), 1u), 1u)))
              {
                {
                  v46 = lenAryCmx (v0);
                  v47 = newAryCmx (v46);
                  v48 = 0u;
                  while (ltdInt (v48, v46))
                  {
                    {
                      if (eqlInt (andInt (v48, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v45)))), 0u))
                      v50 = false;
                      else
                      v50 = true;
                      if (v50)
                      v49 = mulCmx (cis (divFlt (mulFlt (-3.1415927f, i2f (andInt (v48, cmpInt (shlInt (cmpInt (0u), subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v45))))))), i2f (shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v45)))))), subCmx (indAryCmx (sndTplIntAryCmx (v45), xorInt (v48, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v45))))), indAryCmx (sndTplIntAryCmx (v45), v48)));
                      else
                      v49 = addCmx (indAryCmx (sndTplIntAryCmx (v45), v48), indAryCmx (sndTplIntAryCmx (v45), xorInt (v48, shlInt (1u, subInt (subInt (ilog2 (lenAryCmx (v0)), 1u), fstTplIntAryCmx (v45))))));
                    }
                    v47 = setAryCmx (v47, v48, v49);
                    v48 = addInt (v48, 1u);
                  }
                }
                v45 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v45), 1u), v47);
              }
            }
            v34 = setAryCmx (v34, v35, indAryCmx (sndTplIntAryCmx (v45), v35));
            v35 = addInt (v35, 1u);
          }
        }
        v29 = newTplIntAryCmx (0u, v34);
        while (ltdInt (fstTplIntAryCmx (v29), subInt (ilog2 (lenAryCmx (v0)), 1u)))
        {
          {
            v30 = lenAryCmx (sndTplIntAryCmx (v29));
            v31 = newAryCmx (v30);
            v32 = 0u;
            while (ltdInt (v32, v30))
            {
              v31 = setAryCmx (v31, v32, indAryCmx (sndTplIntAryCmx (v29), orInt (shlInt (orInt (shlInt (shrInt (shrInt (v32, 1u), addInt (fstTplIntAryCmx (v29), 1u)), 1u), andInt (v32, 1u)), addInt (fstTplIntAryCmx (v29), 1u)), andInt (shrInt (v32, 1u), cmpInt (shlInt (cmpInt (0u), addInt (fstTplIntAryCmx (v29), 1u)))))));
              v32 = addInt (v32, 1u);
            }
          }
          v29 = newTplIntAryCmx (addInt (fstTplIntAryCmx (v29), 1u), v31);
        }
      }
      v2 = setAryCmx (v2, v3, indAryCmx (sndTplIntAryCmx (v29), v3));
      v3 = addInt (v3, 1u);
    }
  }
  *out = v2;
}

int main() 
{
  Cmx*   cms   = (Cmx []) {-4.712389f + 0.0f * I , -3.1415927f + 0.0f * I , -1.5707964f + 0.0f * I , 0.0f       + 0.0f * I , 
                           1.5707964f + 0.0f * I ,  3.1415927f + 0.0f * I , 4.712389f   + 0.0f * I , 6.2831855f + 0.0f * I};
  AryCmx inAry = (AryCmx) {.size = 8 , .elems = cms};
  AryCmx outAry;
  func(inAry,&outAry);
  for (Int i = 0; i < outAry.size ; i++)
      printf("%f\n",cabsf(outAry.elems[i])); 
  return 0;
}


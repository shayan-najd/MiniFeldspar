#include"ppm.h"
#include "header.h"

void func (AryInt v0, Int *out)
{
  Int v18;
  Int v17;
  Int v16;
  Int v15;
  Int v14;
  Int v13;
  Int v12;
  Int v11;
  Int v10;
  Int v9;
  Int v8;
  Int v7;
  Int v6;
  Int v5;
  Int v4;
  Int v3;
  TplIntInt v2;
  TplIntInt v1;
  v2 = newTplIntInt (0u, 0u);
  v3 = fstTplIntInt (v2);
  v4 = lenAryInt (v0);
  while (ltdInt (v3, v4))
  {
    v5 = fstTplIntInt (v2);
    v6 = addInt (v5, 1u);
    v7 = sndTplIntInt (v2);
    v8 = xorInt (v7, 4294967295u);
    v9 = fstTplIntInt (v2);
    v10 = indAryInt (v0, v9);
    v11 = xorInt (v8, v10);
    v12 = andInt (v11, 255u);
    v13 = indAryInt (hshTbl, v12);
    v14 = sndTplIntInt (v2);
    v15 = xorInt (v14, 4294967295u);
    v16 = shrInt (v15, 8u);
    v17 = xorInt (v13, v16);
    v18 = xorInt (v17, 4294967295u);
    v2 = newTplIntInt (v6, v18);
    v3 = fstTplIntInt (v2);
    v4 = lenAryInt (v0);
  }
  v1 = v2;
  *out = sndTplIntInt (v1);
}
int main()
{
  Image   imgIn = readImage("ImageBig.pgm");
  AryInt  aryIn = newAryInt(size(imgIn));
  for (Int i = 0; i < size(imgIn); i++)
    aryIn = setAryInt(aryIn , i , imgIn.data[i]);
  Int out;
  func(aryIn , &out);
  printf("%u",out);
  return 0;
}
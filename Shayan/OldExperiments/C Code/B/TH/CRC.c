#include"ppm.h"
#include "header.h"

void func (AryInt v0, Int *out)
{
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
  TplIntInt v3;
  TplIntInt v2;
  Int v1;
  v1 = lenAryInt (v0);
  v3 = newTplIntInt (0u, 0u);
  v4 = fstTplIntInt (v3);
  while (ltdInt (v4, v1))
  {
    v5 = fstTplIntInt (v3);
    v6 = addInt (v5, 1u);
    v7 = sndTplIntInt (v3);
    v8 = fstTplIntInt (v3);
    v9 = indAryInt (v0, v8);
    v10 = xorInt (v7, 4294967295u);
    v11 = shrInt (v10, 8u);
    v12 = xorInt (v7, 4294967295u);
    v13 = xorInt (v12, v9);
    v14 = andInt (v13, 255u);
    v15 = indAryInt (hshTbl, v14);
    v16 = xorInt (v15, v11);
    v17 = xorInt (v16, 4294967295u);
    v3 = newTplIntInt (v6, v17);
    v4 = fstTplIntInt (v3);
  }
  v2 = v3;
  *out = sndTplIntInt (v2);
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
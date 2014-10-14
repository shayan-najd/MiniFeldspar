#include"ppm.h"
#include "header.h"

void func (AryInt v0, AryInt *out)
{
  Int v7;
  Bol v6;
  Int v5;
  Int v4;
  AryInt v3;
  Int v2;
  Int v1;
  v1 = lenAryInt (v0);
  v2 = v1;
  v3 = newAryInt (v2);
  v4 = 0u;
  while (ltdInt (v4, v2))
  {
    v5 = indAryInt (v0, v4);
    v6 = ltdInt (v5, 135u);
    if (v6)
    {
      v7 = 1u;
    }
    else
    {
      v7 = 0u;
    }
    v3 = setAryInt (v3, v4, v7);
    v4 = addInt (v4, 1u);
  }
  *out = v3;
}
int main()
{
  Image   imgIn = readImage("ImageBig.pgm");
  AryInt  aryIn = newAryInt(size(imgIn)); 
  for (Int i = 0; i < size(imgIn); i++)
    aryIn = setAryInt(aryIn , i , imgIn.data[i]);
  AryInt aryOut;
  func(aryIn , &aryOut);
  Image imgOut = {.sizeX = imgIn.sizeX, 
                  .sizeY = imgIn.sizeY,
                  .type  = 1,
                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; 
  for(Int i = 0; i < lenAryInt(aryOut); i++)
    imgOut.data[i] = indAryInt(aryOut , i);
  writeImage ("ImageIPBW.pbm" , imgOut);
  return 0;
}
#include"ppm.h"
#include "header.h"

void func (AryInt v0, AryInt *out)
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
  AryInt v4;
  Int v3;
  Int v2;
  Int v1;
  v1 = lenAryInt (v0);
  v2 = divInt (v1, 3u);
  v3 = v2;
  v4 = newAryInt (v3);
  v5 = 0u;
  while (ltdInt (v5, v3))
  {
    v6 = mulInt (v5, 3u);
    v7 = indAryInt (v0, v6);
    v8 = mulInt (v7, 30u);
    v9 = mulInt (v5, 3u);
    v10 = addInt (v9, 1u);
    v11 = indAryInt (v0, v10);
    v12 = mulInt (v11, 59u);
    v13 = addInt (v8, v12);
    v14 = mulInt (v5, 3u);
    v15 = addInt (v14, 2u);
    v16 = indAryInt (v0, v15);
    v17 = mulInt (v16, 11u);
    v18 = addInt (v13, v17);
    v4 = setAryInt (v4, v5, divInt (v18, 100u));
    v5 = addInt (v5, 1u);
  }
  *out = v4;
}
int main()
{
  Image   imgIn = readImage("Image.ppm");
  AryInt  aryIn = newAryInt(size(imgIn)); 
  for (Int i = 0; i < size(imgIn); i++)
    aryIn = setAryInt(aryIn , i , imgIn.data[i]);
  AryInt aryOut;
  func(aryIn , &aryOut);
  Image imgOut = {.sizeX = imgIn.sizeX, 
                  .sizeY = imgIn.sizeY,
                  .type  = 2,
                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; 
  for(Int i = 0; i < lenAryInt(aryOut); i++)
    imgOut.data[i] = indAryInt(aryOut , i);
  writeImage ("ImageIPGray.pgm" , imgOut);
  return 0;
}
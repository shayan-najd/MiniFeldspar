#include "IPMiniWellScoped.c"
#include "ppm.h"

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
                  .type  = 1,
                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; 
  for(Int i = 0; i < lenAryInt(aryOut); i++)
    imgOut.data[i] = indAryInt(aryOut , i);
  writeImage ("Image.pbm" , imgOut);
  return 0;
}


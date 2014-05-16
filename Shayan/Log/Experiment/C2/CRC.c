#include "CRCMini.c"
#include "ppm.h"

int main()
{
  Image   imgIn = readImage("Image.pgm");
  AryInt  aryIn = newAryInt(size(imgIn)); 
  for (Int i = 0; i < size(imgIn); i++)
    aryIn = setAryInt(aryIn , i , imgIn.data[i]);
  Int out;
  func(aryIn , &out);
  printf("%u\n",out);
  return 0;
}

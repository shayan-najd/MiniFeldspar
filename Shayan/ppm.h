#include <stdio.h>
#include <stdlib.h>
#include "header.h"

#ifndef PPM
#define PPM

typedef struct
{ Int  sizeX;
  Int  sizeY;
  Int  type;
  Int* data;
} Image;

Int size(Image img)
{
  if (img.type == 3)
    {
      return (img.sizeX) * (img.sizeY) * 3;
    }
  else
    {
      return (img.sizeX) * (img.sizeY);
    }
}

Image readImage (const char* filename)
{
  FILE* fp = fopen(filename, "rb");
  Image img;
  fscanf(fp , "P%d\n%d %d\n255\n" , &img.type , &img.sizeX , &img.sizeY);
  img.data = malloc(size(img) * sizeof(Int));
  for (Int i = 0; i < size(img); i++)
    fscanf(fp , "%d\n" , &(img.data[i]));
  fclose(fp);
  return img;
}

void writeImage (const char* filename, Image img)
{
  FILE* fp = fopen(filename,"wb");
  fprintf(fp , "P%d\n%d %d\n255\n" , img.type , img.sizeX , img.sizeY);
  for(Int i = 0; i < size(img); i++)
    fprintf(fp , "%d\n" , img.data[i]);
  fclose(fp);
}

#endif

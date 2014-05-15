#include "header.h"

void func (AryInt v0, AryInt *out)
{
  Int v4;
  Int v3;
  AryInt v2;
  Int v1;
  {
    v1 = divInt (lenAryInt (v0), 3u);
    v2 = newAryInt (v1);
    v3 = 0u;
    while (ltdInt (v3, v1))
    {
      if (ltdInt (divInt (addInt (addInt (mulInt (indAryInt (v0, mulInt (v3, 3u)), 30u), mulInt (indAryInt (v0, addInt (mulInt (v3, 3u), 1u)), 59u)), mulInt (indAryInt (v0, addInt (mulInt (v3, 3u), 2u)), 11u)), 100u), 135u))
      v4 = 1u;
      else
      v4 = 0u;
      v2 = setAryInt (v2, v3, v4);
      v3 = addInt (v3, 1u);
    }
  }
  *out = v2;
}
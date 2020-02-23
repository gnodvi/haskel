// ------------------------------------------------------------------------------

#include "RS_mso.h"

// ------------------------------------------------------------------------------
unsigned int RS_mso (n)
     unsigned int n;
{
  unsigned int p = sizeof(unsigned int) << 2;
  unsigned int w = p;

  while (w > 1) {
    w >>= 1;
    if (n & ((~(unsigned int)0x0) << p))
      p += w;
    else
      p -= w;
  }

  if (n & ((~(unsigned int)0x0) << p))
    return 0x1 << p;
  else
    return 0x1 << (p - 1);
}
// ------------------------------------------------------------------------------

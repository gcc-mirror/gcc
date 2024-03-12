#include "libgcc_tm.h"

/* This shim is special, it needs to be built for Mac OSX 10.6
   regardless of the current system version.
   We must also build it to use the unwinder layout that was
   present for 10.6 (and not update that).
   So we copy the referenced structures from unwind-dw2-fde.h
   to avoid pulling in newer system headers and/or changed
   layouts.  */
struct dwarf_eh_bases
{
  void *tbase;
  void *dbase;
  void *func;
};

typedef          int  sword __attribute__ ((mode (SI)));
typedef unsigned int  uword __attribute__ ((mode (SI)));

/* The first few fields of an FDE.  */
struct dwarf_fde
{
  uword length;
  sword CIE_delta;
  unsigned char pc_begin[];
} __attribute__ ((packed, aligned (__alignof__ (void *))));

typedef struct dwarf_fde fde;

extern const fde * _Unwind_Find_FDE (void *, struct dwarf_eh_bases *);

void *
_darwin10_Unwind_FindEnclosingFunction (void *pc)
{
  struct dwarf_eh_bases bases;
  const struct dwarf_fde *fde = _Unwind_Find_FDE (pc-1, &bases);
  if (fde)
    return bases.func;
  return (void *) 0;
}

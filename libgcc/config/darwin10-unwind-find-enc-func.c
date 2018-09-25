#include "tconfig.h"
#include "tsystem.h"
#include "unwind-dw2-fde.h"

void *
_darwin10_Unwind_FindEnclosingFunction (void *pc)
{
  struct dwarf_eh_bases bases;
  const struct dwarf_fde *fde = _Unwind_Find_FDE (pc-1, &bases);
  if (fde)
    return bases.func;
  return NULL;
}

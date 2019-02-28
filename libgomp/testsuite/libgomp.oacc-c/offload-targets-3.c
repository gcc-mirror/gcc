#define OFFLOAD_TARGETS_ADD "XYZ"
#define OFFLOAD_TARGETS_ADD_EARLY
#include "offload-targets-1.c"

/*
  { dg-output "CheCKpOInT1(\n|\r\n|\r)+" }
  { dg-output "libgomp: Unknown offload target: XYZ(\n|\r\n|\r)+" }
  { dg-output "$" }
  { dg-shouldfail ""  }
*/

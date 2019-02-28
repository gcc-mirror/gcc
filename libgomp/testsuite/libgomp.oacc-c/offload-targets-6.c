#define OFFLOAD_TARGETS_SAME_AGAIN
#define OFFLOAD_TARGETS_ADD "XYZ"
#define OFFLOAD_TARGETS_ADD_LATE
#include "offload-targets-1.c"

/*
  { dg-output "CheCKpOInT2(\n|\r\n|\r)+" }
  { dg-output "libgomp: Can't satisfy request for offload targets: XYZ; have loaded: \[a-z-\]*(\n|\r\n|\r)+" }
  { dg-output "$" }
  { dg-shouldfail ""  }
*/

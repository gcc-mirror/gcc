/* { dg-require-effective-target offload_target_amdgcn_with_xnack } */
/* { dg-additional-options "-foffload-options=amdgcn-amdhsa=-mxnack=on" } */

#include "target-is-accessible-1.c"

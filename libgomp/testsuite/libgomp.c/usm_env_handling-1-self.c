/* { dg-do run }  */

/* Check that a global static variable is consistently updated on the device. */

// ICE for A$linkptr in nvptx lto1 with ipa_reference_var_uid
// when compiling with -fopenmp -O1 or higher
// {Segmentation [Ff]ault}{ offload_target_nvptx } }
// { dg-ice "PR target/127273" { offload_target_nvptx } }
// { dg-excess-errors "'mkoffload' failure etc." { xfail { offload_target_nvptx } } } */


#define USE_SELF_MAPS 1
#include "usm_env_handling-1.c"

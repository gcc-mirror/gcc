/* PR libgomp/109875  */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL 9 } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV 7 } */

#define MY_OMP_NUM_TEAMS_ALL 9
#define MY_OMP_NUM_TEAMS_DEV 7

#include "teams-nteams-icv-1.c"

/* PR libgomp/109875  */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL 7 } */
/* { dg-set-target-env-var OMP_NUM_TEAMS 8 } */

#define MY_OMP_NUM_TEAMS_ALL 7
#define MY_OMP_NUM_TEAMS 8

#include "teams-nteams-icv-1.c"

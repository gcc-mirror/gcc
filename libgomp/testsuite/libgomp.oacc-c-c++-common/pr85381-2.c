/* { dg-do run { target openacc_nvidia_accel_selected } }
   { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-additional-options "-foffload=-fdump-rtl-mach" } */

int
main (void)
{
  int v1;

  #pragma acc parallel
  #pragma acc loop worker
  for (v1 = 0; v1 < 20; v1 += 2)
    ;

  return 0;
}

/* Todo: Boths bar.syncs can be removed.
   Atm we generate this dead code inbetween forked and joining:

                     mov.u32 %r28, %ntid.y;
                     mov.u32 %r29, %tid.y;
                     add.u32 %r30, %r29, %r29;
                     setp.gt.s32     %r31, %r30, 19;
             @%r31   bra     $L2;
                     add.u32 %r25, %r28, %r28;
                     mov.u32 %r24, %r30;
     $L3:
                     add.u32 %r24, %r24, %r25;
                     setp.le.s32     %r33, %r24, 19;
             @%r33   bra     $L3;
     $L2:

   so the loop is not recognized as empty loop (which we detect by seeing if
   joining immediately follows forked).  */
/* { dg-final { scan-offload-rtl-dump-times "nvptx_barsync" 2 "mach" } } */

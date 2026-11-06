/* { dg-do run } */
/* { dg-require-effective-target offload_device } */

/* Ensure that we correctly perform target offloading even if we have many
   variables to map.  */

#define C(x, y) x##y
#define R4(x) C(x, 1), C(x, 2), C(x, 3), C(x, 4)
#define R16(x) R4(C(x, 1)), R4(C(x, 2)), R4(C(x, 3)), R4(C(x, 4))
#define R64(x) R16(C(x, 1)), R16(C(x, 2)), R16(C(x, 3)), R16(C(x, 4))
#define R256(x) R64(C(x, 1)), R64(C(x, 2)), R64(C(x, 3)), R64(C(x, 4))
#define R1024(x) R256(C(x, 1)), R256(C(x, 2)), R256(C(x, 3)), R256(C(x, 4))
#define R4096(x) R1024(C(x, 1)), R1024(C(x, 2)), R1024(C(x, 3)), R1024(C(x, 4))

#define P4(x) C(x, 1) = C(x, 2) = C(x, 3) = C(x, 4)
#define P16(x) R4(C(x, 1)) = R4(C(x, 2)) = R4(C(x, 3)) = R4(C(x, 4))
#define P64(x) R16(C(x, 1)) = R16(C(x, 2)) = R16(C(x, 3)) = R16(C(x, 4))
#define P256(x) R64(C(x, 1)) = R64(C(x, 2)) = R64(C(x, 3)) = R64(C(x, 4))
#define P1024(x) R256(C(x, 1)) = R256(C(x, 2)) = R256(C(x, 3)) = R256(C(x, 4))
#define P4096(x) R1024(C(x, 1)) = R1024(C(x, 2)) = R1024(C(x, 3)) = R1024(C(x, 4))

static int R4096(x);

int
main ()
{
#pragma omp target map(tofrom: R4096(x))
  {
    volatile int v;
    P4096(x) = v;
  }
}

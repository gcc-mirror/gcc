/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

int
main (int argc, char *argv[])
{
  int i;

#pragma acc data create (i)
#pragma acc parallel copyin (i)
  ++i;

  return 0;
}

/* { dg-shouldfail "" }
   { dg-output "Trying to map into device .* object when .* is already mapped" } */

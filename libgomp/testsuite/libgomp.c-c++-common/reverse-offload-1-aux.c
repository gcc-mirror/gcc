/* { dg-do compile { target skip-all-targets } }  */

/* Declare the following function in a separare translation unit
   to ensure it won't have a device version.  */

int
add_3 (int x)
{
  return x + 3;
}

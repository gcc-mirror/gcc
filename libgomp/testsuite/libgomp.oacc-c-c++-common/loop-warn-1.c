
/* Check warnings about suboptimal partitioning choices.  */

int main ()
{
  int ary[10];

#pragma acc parallel copy(ary) num_gangs (1) /* { dg-warning "is not gang partitioned" } */
  {
    #pragma acc loop gang
    for (int  i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel copy(ary) num_workers (1) /* { dg-warning "is not worker partitioned" } */
  {
    #pragma acc loop worker
    for (int  i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel copy(ary) num_gangs (8) /* { dg-warning "is gang partitioned" } */
  {
    #pragma acc loop worker
    for (int  i = 0; i < 10; i++)
      ary[i] = i;
  }

#pragma acc parallel copy(ary) num_workers (8) /* { dg-warning "is worker partitioned" } */
  {
    #pragma acc loop gang
    for (int  i = 0; i < 10; i++)
      ary[i] = i;
  }

  return 0;
}

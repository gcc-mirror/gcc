const int n = 100;
    
// Check async over parallel construct with reduction
    
int
async_sum (int c)
{
  int s = 0;
    
#pragma acc parallel loop num_gangs (10) gang reduction (+:s) async
  for (int i = 0; i < n; i++)
    s += i+c;
    
#pragma acc wait
  return s;
}
    
int
main()
{
  int result = 0;
    
  for (int i = 0; i < n; i++)
    result += i+1;
    
  if (async_sum (1) != result)
    __builtin_abort ();
    
  return 0;
}

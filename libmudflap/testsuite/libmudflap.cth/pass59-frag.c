#include <stdio.h>
#include <pthread.h>

/* PR 28578 */

void* test_thread(void* arg)
{
  printf("Hello from thread!\n");
  pthread_exit(NULL);
  return 0;
}

int main()
{
  pthread_t thread;
  void *arg = NULL;
  pthread_create(&thread, NULL, test_thread, arg);
  pthread_join(thread, NULL);
  pthread_exit(NULL);
  return 0;
}

/* { dg-output "Hello from thread!\n" } */

#if 0

/* Even this test case replicates the problem.  However, when built in
   static mode, it blows up during __mf_init (?!?!?!) with a
   pthread_mutex_lock deadlock error. */

#include <stdio.h>
#include <pthread.h>

int main ()
{
      pthread_exit(NULL);
      return 0;
}
#endif

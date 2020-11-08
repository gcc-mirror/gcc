#ifndef COUNTER_BARRIER_HDR
#define COUNTER_BARRIER_HDR

#include <pthread.h>

struct waitable_counter;

typedef struct counter_barrier
{
  pthread_cond_t cond;
  struct waitable_counter *count;
  volatile struct counter_barrier *next;
  volatile int wait_count;
  volatile int curr_wait_group;
} counter_barrier;

typedef struct waitable_counter
{
  pthread_mutex_t m;
  volatile counter_barrier *b;
  volatile int val;
} waitable_counter;

void waitable_counter_init (waitable_counter *c, int val);
internal_proto(waitable_counter_init);
int waitable_counter_add (waitable_counter *c, int val);
internal_proto(waitable_counter_add);
int waitable_counter_get_val (waitable_counter *c);
internal_proto(waitable_counter_get_val);

void bind_counter_barrier (counter_barrier *b, waitable_counter *c); 
internal_proto(bind_counter_barrier);
void counter_barrier_wait (counter_barrier *b);
internal_proto(counter_barrier_wait);

#endif

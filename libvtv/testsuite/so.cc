#include <dlfcn.h>
#include <assert.h>
#include <unistd.h>
#include <vtv_fail.h>

extern "C" int printf(const char *, ...);
extern "C" int sprintf(char *, const char*, ...);

static int counter = 0;
extern int failures;

template <int i> struct base
{
  virtual char * whoami() {
    static char sl[100];
    sprintf(sl, "I am base %d", i);
    return sl;
  }
  virtual void inc() { counter += i; }
};

template <int i> struct derived: base<i>
{
  virtual char * whoami() {
    static char sl[100];
    sprintf(sl, "I am derived %d", i);
    return sl;
  }
  virtual void inc() { counter += (10*i); }
};

// We don't use this class. It is just here so that the
// compiler does not devirtualize calls to derived::inc()
template <int i> struct derived2: derived<i>
{
  virtual void inc() { counter += (20*i); }
};

static base<TPID> * bp = new base<TPID>();
static derived<TPID> * dp = new derived<TPID>();
static base<TPID> * dbp = new derived<TPID>();


// Given 2 pointers to C++ objects (non PODs), exchange the pointers to vtable
static void exchange_vtptr(void * object1_ptr, void * object2_ptr)
{
  void ** object1_vtptr_ptr = (void **)object1_ptr;
  void ** object2_vtptr_ptr = (void **)object2_ptr;
  void * object1_vtptr = *object1_vtptr_ptr;
  void * object2_vtptr = *object2_vtptr_ptr;
  *object1_vtptr_ptr = object2_vtptr;
  *object2_vtptr_ptr = object1_vtptr;
}

#define BUILD_NAME(NAME,ID) NAME##ID
#define EXPAND(NAME,X) BUILD_NAME(NAME,X)
extern "C" void EXPAND(so_entry_,TPID)(void)
{
  int prev_counter;
  int prev_failures;

  counter = 0;
  bp->inc();
  dp->inc();
  dbp->inc();
  assert(counter == (TPID + 10*TPID + 10*TPID));

  prev_counter = counter;
  exchange_vtptr(bp, dp);
  bp->inc(); // This one should succeed but it is calling the wrong member
  if (counter != (prev_counter + 10*TPID))
  {
    printf("TPID=%d whoami=%s wrong counter value prev_counter=%d counter=%d\n", TPID, bp->whoami(), prev_counter, counter);
    sleep(2);
  }
  assert(counter == (prev_counter + 10*TPID));
  //  printf("Pass first attack!\n");

 // This one should fail verification!. So it should jump to __vtv_verify_fail above.
  prev_failures = failures;
  dp->inc();
  // this code may be executed by multiple threads at the same time. So, just verify the number of failures has
  // increased as opposed to check for increase by 1.
  assert(failures > prev_failures);
  assert(counter == (prev_counter + 10*TPID + TPID));
  //  printf("TPDI=%d counter %d\n", TPID, counter);
  //  printf("Pass second attack!\n");

  // restore the vtable pointers to the original state.
  // This is very important. For some reason the dlclose is not "really" closing the library so when we reopen it we are
  // getting the old memory state.
  exchange_vtptr(bp, dp);
}

// Compile with /home/llozano/local2/proj/vtable/gcc-root/usr/local/bin/g++ -m32 -fvtable-verify=std -fpic -rdynamic -Wl,-R,/home/llozano/local2/proj/vtable/gcc-root/usr/local/lib32:./lib32 -I/home/llozano/local2/proj/vtable/vt2/gcc-4_6-mobile-vtable-security//libstdc++-v3/libsupc++ temp_deriv.cc -O0 -ldl -lpthread -Wl,--whole-archive,-lvtv_init,--no-whole-archive,-z,relro -DTPID=0 -g
// Look at assembly with: objdump -drl a.out

#include <dlfcn.h>
#include <assert.h>

extern "C" int printf(const char *, ...);

static int counter = 0;

int i = TPID;
struct base
{
  virtual void inc() { counter += i; }
};

struct derived: public base
{
  virtual void inc() { counter += (10*i); }
};

// We don't use this class. It is just here so that the
// compiler does not devirtualize calls to derived::inc()
struct derived2: public derived
{
  virtual void inc() { counter += (20*i); }
};

static base * bp = new base();
static derived * dp = new derived();
static base * dbp = new derived();

// Given 2 pointers to C++ objects (non PODs), exchange the pointers to vtable
void exchange_vtptr(void * object1_ptr, void * object2_ptr)
{
  typedef void * vtptr;
  vtptr * object1_vtptr_ptr = (vtptr *)object1_ptr;
  vtptr * object2_vtptr_ptr = (vtptr *)object2_ptr;
  vtptr object1_vtptr = *object1_vtptr_ptr;
  vtptr object2_vtptr = *object2_vtptr_ptr;
  *object1_vtptr_ptr = object2_vtptr;
  *object2_vtptr_ptr = object1_vtptr;
}

main()
{
  int prev_counter;

  exchange_vtptr(bp, dp);
  exchange_vtptr(bp, dp);
  exchange_vtptr(bp, dbp);
  exchange_vtptr(bp, dbp);

  counter = 0;
  bp->inc();
  dp->inc();
  dbp->inc();
  assert(counter == (TPID + 10*TPID + 10*TPID));

  prev_counter = counter;
  exchange_vtptr(bp, dp);
  bp->inc(); // This one should succeed but it is calling the wrong member
  assert(counter == (prev_counter + 10*TPID));
  printf("Pass first attack!\n");
  dp->inc();
  printf("TPDI=%d counter %d\n", TPID, counter);
  printf("Pass second attack!\n");

}

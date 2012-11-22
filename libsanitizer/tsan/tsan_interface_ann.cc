//===-- tsan_interface_ann.cc ---------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of ThreadSanitizer (TSan), a race detector.
//
//===----------------------------------------------------------------------===//
#include "sanitizer_common/sanitizer_libc.h"
#include "sanitizer_common/sanitizer_placement_new.h"
#include "tsan_interface_ann.h"
#include "tsan_mutex.h"
#include "tsan_report.h"
#include "tsan_rtl.h"
#include "tsan_mman.h"
#include "tsan_flags.h"
#include "tsan_platform.h"

#define CALLERPC ((uptr)__builtin_return_address(0))

using namespace __tsan;  // NOLINT

namespace __tsan {

class ScopedAnnotation {
 public:
  ScopedAnnotation(ThreadState *thr, const char *aname, const char *f, int l,
                   uptr pc)
      : thr_(thr)
      , in_rtl_(thr->in_rtl) {
    CHECK_EQ(thr_->in_rtl, 0);
    FuncEntry(thr_, pc);
    thr_->in_rtl++;
    DPrintf("#%d: annotation %s() %s:%d\n", thr_->tid, aname, f, l);
  }

  ~ScopedAnnotation() {
    thr_->in_rtl--;
    CHECK_EQ(in_rtl_, thr_->in_rtl);
    FuncExit(thr_);
  }
 private:
  ThreadState *const thr_;
  const int in_rtl_;
};

#define SCOPED_ANNOTATION(typ) \
    if (!flags()->enable_annotations) \
      return; \
    ThreadState *thr = cur_thread(); \
    const uptr pc = (uptr)__builtin_return_address(0); \
    StatInc(thr, StatAnnotation); \
    StatInc(thr, Stat##typ); \
    ScopedAnnotation sa(thr, __FUNCTION__, f, l, \
        (uptr)__builtin_return_address(0)); \
    (void)pc; \
/**/

static const int kMaxDescLen = 128;

struct ExpectRace {
  ExpectRace *next;
  ExpectRace *prev;
  int hitcount;
  uptr addr;
  uptr size;
  char *file;
  int line;
  char desc[kMaxDescLen];
};

struct DynamicAnnContext {
  Mutex mtx;
  ExpectRace expect;
  ExpectRace benign;

  DynamicAnnContext()
    : mtx(MutexTypeAnnotations, StatMtxAnnotations) {
  }
};

static DynamicAnnContext *dyn_ann_ctx;
static char dyn_ann_ctx_placeholder[sizeof(DynamicAnnContext)] ALIGNED(64);

static void AddExpectRace(ExpectRace *list,
    char *f, int l, uptr addr, uptr size, char *desc) {
  ExpectRace *race = list->next;
  for (; race != list; race = race->next) {
    if (race->addr == addr && race->size == size)
      return;
  }
  race = (ExpectRace*)internal_alloc(MBlockExpectRace, sizeof(ExpectRace));
  race->hitcount = 0;
  race->addr = addr;
  race->size = size;
  race->file = f;
  race->line = l;
  race->desc[0] = 0;
  if (desc) {
    int i = 0;
    for (; i < kMaxDescLen - 1 && desc[i]; i++)
      race->desc[i] = desc[i];
    race->desc[i] = 0;
  }
  race->prev = list;
  race->next = list->next;
  race->next->prev = race;
  list->next = race;
}

static ExpectRace *FindRace(ExpectRace *list, uptr addr, uptr size) {
  for (ExpectRace *race = list->next; race != list; race = race->next) {
    uptr maxbegin = max(race->addr, addr);
    uptr minend = min(race->addr + race->size, addr + size);
    if (maxbegin < minend)
      return race;
  }
  return 0;
}

static bool CheckContains(ExpectRace *list, uptr addr, uptr size) {
  ExpectRace *race = FindRace(list, addr, size);
  if (race == 0 && AlternativeAddress(addr))
    race = FindRace(list, AlternativeAddress(addr), size);
  if (race == 0)
    return false;
  DPrintf("Hit expected/benign race: %s addr=%zx:%d %s:%d\n",
      race->desc, race->addr, (int)race->size, race->file, race->line);
  race->hitcount++;
  return true;
}

static void InitList(ExpectRace *list) {
  list->next = list;
  list->prev = list;
}

void InitializeDynamicAnnotations() {
  dyn_ann_ctx = new(dyn_ann_ctx_placeholder) DynamicAnnContext;
  InitList(&dyn_ann_ctx->expect);
  InitList(&dyn_ann_ctx->benign);
}

bool IsExpectedReport(uptr addr, uptr size) {
  Lock lock(&dyn_ann_ctx->mtx);
  if (CheckContains(&dyn_ann_ctx->expect, addr, size))
    return true;
  if (CheckContains(&dyn_ann_ctx->benign, addr, size))
    return true;
  return false;
}

}  // namespace __tsan

using namespace __tsan;  // NOLINT

extern "C" {
SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateHappensBefore(char *f, int l, uptr addr) {
  SCOPED_ANNOTATION(AnnotateHappensBefore);
  Release(cur_thread(), CALLERPC, addr);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateHappensAfter(char *f, int l, uptr addr) {
  SCOPED_ANNOTATION(AnnotateHappensAfter);
  Acquire(cur_thread(), CALLERPC, addr);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateCondVarSignal(char *f, int l, uptr cv) {
  SCOPED_ANNOTATION(AnnotateCondVarSignal);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateCondVarSignalAll(char *f, int l, uptr cv) {
  SCOPED_ANNOTATION(AnnotateCondVarSignalAll);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateMutexIsNotPHB(char *f, int l, uptr mu) {
  SCOPED_ANNOTATION(AnnotateMutexIsNotPHB);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateCondVarWait(char *f, int l, uptr cv, uptr lock) {
  SCOPED_ANNOTATION(AnnotateCondVarWait);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateRWLockCreate(char *f, int l, uptr m) {
  SCOPED_ANNOTATION(AnnotateRWLockCreate);
  MutexCreate(thr, pc, m, true, true, false);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateRWLockCreateStatic(char *f, int l, uptr m) {
  SCOPED_ANNOTATION(AnnotateRWLockCreateStatic);
  MutexCreate(thr, pc, m, true, true, true);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateRWLockDestroy(char *f, int l, uptr m) {
  SCOPED_ANNOTATION(AnnotateRWLockDestroy);
  MutexDestroy(thr, pc, m);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateRWLockAcquired(char *f, int l, uptr m, uptr is_w) {
  SCOPED_ANNOTATION(AnnotateRWLockAcquired);
  if (is_w)
    MutexLock(thr, pc, m);
  else
    MutexReadLock(thr, pc, m);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateRWLockReleased(char *f, int l, uptr m, uptr is_w) {
  SCOPED_ANNOTATION(AnnotateRWLockReleased);
  if (is_w)
    MutexUnlock(thr, pc, m);
  else
    MutexReadUnlock(thr, pc, m);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateTraceMemory(char *f, int l, uptr mem) {
  SCOPED_ANNOTATION(AnnotateTraceMemory);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateFlushState(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateFlushState);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateNewMemory(char *f, int l, uptr mem, uptr size) {
  SCOPED_ANNOTATION(AnnotateNewMemory);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateNoOp(char *f, int l, uptr mem) {
  SCOPED_ANNOTATION(AnnotateNoOp);
}

static void ReportMissedExpectedRace(ExpectRace *race) {
  TsanPrintf("==================\n");
  TsanPrintf("WARNING: ThreadSanitizer: missed expected data race\n");
  TsanPrintf("  %s addr=%zx %s:%d\n",
      race->desc, race->addr, race->file, race->line);
  TsanPrintf("==================\n");
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateFlushExpectedRaces(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateFlushExpectedRaces);
  Lock lock(&dyn_ann_ctx->mtx);
  while (dyn_ann_ctx->expect.next != &dyn_ann_ctx->expect) {
    ExpectRace *race = dyn_ann_ctx->expect.next;
    if (race->hitcount == 0) {
      CTX()->nmissed_expected++;
      ReportMissedExpectedRace(race);
    }
    race->prev->next = race->next;
    race->next->prev = race->prev;
    internal_free(race);
  }
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateEnableRaceDetection(char *f, int l, int enable) {
  SCOPED_ANNOTATION(AnnotateEnableRaceDetection);
  // FIXME: Reconsider this functionality later. It may be irrelevant.
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateMutexIsUsedAsCondVar(char *f, int l, uptr mu) {
  SCOPED_ANNOTATION(AnnotateMutexIsUsedAsCondVar);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotatePCQGet(char *f, int l, uptr pcq) {
  SCOPED_ANNOTATION(AnnotatePCQGet);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotatePCQPut(char *f, int l, uptr pcq) {
  SCOPED_ANNOTATION(AnnotatePCQPut);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotatePCQDestroy(char *f, int l, uptr pcq) {
  SCOPED_ANNOTATION(AnnotatePCQDestroy);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotatePCQCreate(char *f, int l, uptr pcq) {
  SCOPED_ANNOTATION(AnnotatePCQCreate);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateExpectRace(char *f, int l, uptr mem, char *desc) {
  SCOPED_ANNOTATION(AnnotateExpectRace);
  Lock lock(&dyn_ann_ctx->mtx);
  AddExpectRace(&dyn_ann_ctx->expect,
                f, l, mem, 1, desc);
  DPrintf("Add expected race: %s addr=%zx %s:%d\n", desc, mem, f, l);
}

static void BenignRaceImpl(char *f, int l, uptr mem, uptr size, char *desc) {
  Lock lock(&dyn_ann_ctx->mtx);
  AddExpectRace(&dyn_ann_ctx->benign,
                f, l, mem, size, desc);
  DPrintf("Add benign race: %s addr=%zx %s:%d\n", desc, mem, f, l);
}

// FIXME: Turn it off later. WTF is benign race?1?? Go talk to Hans Boehm.
// SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateBenignRaceSized(char *f, int l, uptr mem, uptr size, char *desc) {
  SCOPED_ANNOTATION(AnnotateBenignRaceSized);
  BenignRaceImpl(f, l, mem, size, desc);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateBenignRace(char *f, int l, uptr mem, char *desc) {
  SCOPED_ANNOTATION(AnnotateBenignRace);
  BenignRaceImpl(f, l, mem, 1, desc);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateIgnoreReadsBegin(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateIgnoreReadsBegin);
  IgnoreCtl(cur_thread(), false, true);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateIgnoreReadsEnd(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateIgnoreReadsEnd);
  IgnoreCtl(cur_thread(), false, false);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateIgnoreWritesBegin(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateIgnoreWritesBegin);
  IgnoreCtl(cur_thread(), true, true);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateIgnoreWritesEnd(char *f, int l) {
  SCOPED_ANNOTATION(AnnotateIgnoreWritesEnd);
  IgnoreCtl(cur_thread(), true, false);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotatePublishMemoryRange(char *f, int l, uptr addr, uptr size) {
  SCOPED_ANNOTATION(AnnotatePublishMemoryRange);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateUnpublishMemoryRange(char *f, int l, uptr addr, uptr size) {
  SCOPED_ANNOTATION(AnnotateUnpublishMemoryRange);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void AnnotateThreadName(char *f, int l, char *name) {
  SCOPED_ANNOTATION(AnnotateThreadName);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void WTFAnnotateHappensBefore(char *f, int l, uptr addr) {
  SCOPED_ANNOTATION(AnnotateHappensBefore);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void WTFAnnotateHappensAfter(char *f, int l, uptr addr) {
  SCOPED_ANNOTATION(AnnotateHappensAfter);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
void WTFAnnotateBenignRaceSized(char *f, int l, uptr mem, uptr sz, char *desc) {
  SCOPED_ANNOTATION(AnnotateBenignRaceSized);
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
int RunningOnValgrind() {
  return flags()->running_on_valgrind;
}

double __attribute__((weak)) ValgrindSlowdown(void) {
  return 10.0;
}

SANITIZER_WEAK_ATTRIBUTE SANITIZER_INTERFACE_ATTRIBUTE
const char *ThreadSanitizerQuery(const char *query) {
  if (internal_strcmp(query, "pure_happens_before") == 0)
    return "1";
  else
    return "0";
}
}  // extern "C"

/* Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Contributed by Torvald Riegel <triegel@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef DISPATCH_H
#define DISPATCH_H 1

#include "libitm.h"
#include "common.h"

// Creates ABI load/store methods (can be made virtual or static using M,
// use M2 to create separate methods names for virtual and static)
// The _PV variants are for the pure-virtual methods in the base class.
#define ITM_READ_M(T, LSMOD, M, M2)                                         \
  M _ITM_TYPE_##T ITM_REGPARM ITM_##LSMOD##T##M2 (const _ITM_TYPE_##T *ptr) \
  {                                                                         \
    return load(ptr, abi_dispatch::LSMOD);                                  \
  }

#define ITM_READ_M_PV(T, LSMOD, M, M2)                                      \
  M _ITM_TYPE_##T ITM_REGPARM ITM_##LSMOD##T##M2 (const _ITM_TYPE_##T *ptr) \
  = 0;

#define ITM_WRITE_M(T, LSMOD, M, M2)                         \
  M void ITM_REGPARM ITM_##LSMOD##T##M2 (_ITM_TYPE_##T *ptr, \
					 _ITM_TYPE_##T val)  \
  {                                                          \
    store(ptr, val, abi_dispatch::LSMOD);                    \
  }

#define ITM_WRITE_M_PV(T, LSMOD, M, M2)                      \
  M void ITM_REGPARM ITM_##LSMOD##T##M2 (_ITM_TYPE_##T *ptr, \
					 _ITM_TYPE_##T val)  \
  = 0;

// Creates ABI load/store methods for all load/store modifiers for a particular
// type.
#define CREATE_DISPATCH_METHODS_T(T, M, M2) \
  ITM_READ_M(T, R, M, M2)                \
  ITM_READ_M(T, RaR, M, M2)              \
  ITM_READ_M(T, RaW, M, M2)              \
  ITM_READ_M(T, RfW, M, M2)              \
  ITM_WRITE_M(T, W, M, M2)               \
  ITM_WRITE_M(T, WaR, M, M2)             \
  ITM_WRITE_M(T, WaW, M, M2)
#define CREATE_DISPATCH_METHODS_T_PV(T, M, M2) \
  ITM_READ_M_PV(T, R, M, M2)                \
  ITM_READ_M_PV(T, RaR, M, M2)              \
  ITM_READ_M_PV(T, RaW, M, M2)              \
  ITM_READ_M_PV(T, RfW, M, M2)              \
  ITM_WRITE_M_PV(T, W, M, M2)               \
  ITM_WRITE_M_PV(T, WaR, M, M2)             \
  ITM_WRITE_M_PV(T, WaW, M, M2)

// Creates ABI load/store methods for all types.
// See CREATE_DISPATCH_FUNCTIONS for comments.
#define CREATE_DISPATCH_METHODS(M, M2)  \
  CREATE_DISPATCH_METHODS_T (U1, M, M2) \
  CREATE_DISPATCH_METHODS_T (U2, M, M2) \
  CREATE_DISPATCH_METHODS_T (U4, M, M2) \
  CREATE_DISPATCH_METHODS_T (U8, M, M2) \
  CREATE_DISPATCH_METHODS_T (F, M, M2)  \
  CREATE_DISPATCH_METHODS_T (D, M, M2)  \
  CREATE_DISPATCH_METHODS_T (E, M, M2)  \
  CREATE_DISPATCH_METHODS_T (CF, M, M2) \
  CREATE_DISPATCH_METHODS_T (CD, M, M2) \
  CREATE_DISPATCH_METHODS_T (CE, M, M2)
#define CREATE_DISPATCH_METHODS_PV(M, M2)  \
  CREATE_DISPATCH_METHODS_T_PV (U1, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (U2, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (U4, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (U8, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (F, M, M2)  \
  CREATE_DISPATCH_METHODS_T_PV (D, M, M2)  \
  CREATE_DISPATCH_METHODS_T_PV (E, M, M2)  \
  CREATE_DISPATCH_METHODS_T_PV (CF, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (CD, M, M2) \
  CREATE_DISPATCH_METHODS_T_PV (CE, M, M2)

// Creates memcpy/memmove/memset methods.
#define CREATE_DISPATCH_METHODS_MEM()  \
virtual void memtransfer(void *dst, const void* src, size_t size,    \
    bool may_overlap, ls_modifier dst_mod, ls_modifier src_mod)       \
{                                                                     \
  if (size > 0)                                                       \
    memtransfer_static(dst, src, size, may_overlap, dst_mod, src_mod); \
}                                                                     \
virtual void memset(void *dst, int c, size_t size, ls_modifier mod)  \
{                                                                     \
  if (size > 0)                                                       \
    memset_static(dst, c, size, mod);                                 \
}

#define CREATE_DISPATCH_METHODS_MEM_PV()  \
virtual void memtransfer(void *dst, const void* src, size_t size,       \
    bool may_overlap, ls_modifier dst_mod, ls_modifier src_mod) = 0;     \
virtual void memset(void *dst, int c, size_t size, ls_modifier mod) = 0;


// Creates ABI load/store functions that can target either a class or an
// object.
#define ITM_READ(T, LSMOD, TARGET, M2)                                 \
  _ITM_TYPE_##T ITM_REGPARM _ITM_##LSMOD##T (const _ITM_TYPE_##T *ptr) \
  {                                                                    \
    return TARGET ITM_##LSMOD##T##M2(ptr);                            \
  }

#define ITM_WRITE(T, LSMOD, TARGET, M2)                                    \
  void ITM_REGPARM _ITM_##LSMOD##T (_ITM_TYPE_##T *ptr, _ITM_TYPE_##T val) \
  {                                                                        \
    TARGET ITM_##LSMOD##T##M2(ptr, val);                                  \
  }

// Creates ABI load/store functions for all load/store modifiers for a
// particular type.
#define CREATE_DISPATCH_FUNCTIONS_T(T, TARGET, M2) \
  ITM_READ(T, R, TARGET, M2)                \
  ITM_READ(T, RaR, TARGET, M2)              \
  ITM_READ(T, RaW, TARGET, M2)              \
  ITM_READ(T, RfW, TARGET, M2)              \
  ITM_WRITE(T, W, TARGET, M2)               \
  ITM_WRITE(T, WaR, TARGET, M2)             \
  ITM_WRITE(T, WaW, TARGET, M2)

// Creates ABI memcpy/memmove/memset functions.
#define ITM_MEMTRANSFER_DEF(TARGET, M2, NAME, READ, WRITE) \
void ITM_REGPARM _ITM_memcpy##NAME(void *dst, const void *src, size_t size)  \
{                                                                            \
  TARGET memtransfer##M2 (dst, src, size,                                   \
	     false, GTM::abi_dispatch::WRITE, GTM::abi_dispatch::READ);      \
}                                                                            \
void ITM_REGPARM _ITM_memmove##NAME(void *dst, const void *src, size_t size) \
{                                                                            \
  TARGET memtransfer##M2 (dst, src, size,                                   \
      GTM::abi_dispatch::memmove_overlap_check(dst, src, size,               \
	  GTM::abi_dispatch::WRITE, GTM::abi_dispatch::READ),                \
      GTM::abi_dispatch::WRITE, GTM::abi_dispatch::READ);                    \
}

#define ITM_MEMSET_DEF(TARGET, M2, WRITE) \
void ITM_REGPARM _ITM_memset##WRITE(void *dst, int c, size_t size) \
{                                                                  \
  TARGET memset##M2 (dst, c, size, GTM::abi_dispatch::WRITE);     \
}                                                                  \


// ??? The number of virtual methods is large (7*4 for integers, 7*6 for FP,
// 7*3 for vectors). Is the cache footprint so costly that we should go for
// a small table instead (i.e., only have two virtual load/store methods for
// each supported type)? Note that this doesn't affect custom code paths at
// all because these use only direct calls.
// A large cache footprint could especially decrease HTM performance (due
// to HTM capacity). We could add the modifier (RaR etc.) as parameter, which
// would give us just 4*2+6*2+3*2 functions (so we'd just need one line for
// the integer loads/stores), but then the modifier can be checked only at
// runtime.
// For memcpy/memmove/memset, we just have two virtual methods (memtransfer
// and memset).
#define CREATE_DISPATCH_FUNCTIONS(TARGET, M2)  \
  CREATE_DISPATCH_FUNCTIONS_T (U1, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (U2, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (U4, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (U8, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (F, TARGET, M2)  \
  CREATE_DISPATCH_FUNCTIONS_T (D, TARGET, M2)  \
  CREATE_DISPATCH_FUNCTIONS_T (E, TARGET, M2)  \
  CREATE_DISPATCH_FUNCTIONS_T (CF, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (CD, TARGET, M2) \
  CREATE_DISPATCH_FUNCTIONS_T (CE, TARGET, M2) \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RnWt,     NONTXNAL, W)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RnWtaR,   NONTXNAL, WaR)    \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RnWtaW,   NONTXNAL, WaW)    \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtWn,     R,      NONTXNAL) \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtWt,     R,      W)        \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtWtaR,   R,      WaR)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtWtaW,   R,      WaW)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaRWn,   RaR,    NONTXNAL) \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaRWt,   RaR,    W)        \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaRWtaR, RaR,    WaR)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaRWtaW, RaR,    WaW)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaWWn,   RaW,    NONTXNAL) \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaWWt,   RaW,    W)        \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaWWtaR, RaW,    WaR)      \
  ITM_MEMTRANSFER_DEF(TARGET, M2, RtaWWtaW, RaW,    WaW)      \
  ITM_MEMSET_DEF(TARGET, M2, W)   \
  ITM_MEMSET_DEF(TARGET, M2, WaR) \
  ITM_MEMSET_DEF(TARGET, M2, WaW)


// Creates ABI load/store functions that delegate to a transactional memcpy.
#define ITM_READ_MEMCPY(T, LSMOD, TARGET, M2)                         \
  _ITM_TYPE_##T ITM_REGPARM _ITM_##LSMOD##T (const _ITM_TYPE_##T *ptr)\
  {                                                                   \
    _ITM_TYPE_##T v;                                                  \
    TARGET memtransfer##M2(&v, ptr, sizeof(_ITM_TYPE_##T), false,    \
	GTM::abi_dispatch::NONTXNAL, GTM::abi_dispatch::LSMOD);       \
    return v;                                                         \
  }

#define ITM_WRITE_MEMCPY(T, LSMOD, TARGET, M2)                            \
  void ITM_REGPARM _ITM_##LSMOD##T (_ITM_TYPE_##T *ptr, _ITM_TYPE_##T val)\
  {                                                                       \
    TARGET memtransfer##M2(ptr, &val, sizeof(_ITM_TYPE_##T), false,      \
	GTM::abi_dispatch::LSMOD, GTM::abi_dispatch::NONTXNAL);           \
  }

#define CREATE_DISPATCH_FUNCTIONS_T_MEMCPY(T, TARGET, M2) \
  ITM_READ_MEMCPY(T, R, TARGET, M2)                \
  ITM_READ_MEMCPY(T, RaR, TARGET, M2)              \
  ITM_READ_MEMCPY(T, RaW, TARGET, M2)              \
  ITM_READ_MEMCPY(T, RfW, TARGET, M2)              \
  ITM_WRITE_MEMCPY(T, W, TARGET, M2)               \
  ITM_WRITE_MEMCPY(T, WaR, TARGET, M2)             \
  ITM_WRITE_MEMCPY(T, WaW, TARGET, M2)


namespace GTM HIDDEN {

struct gtm_transaction_cp;

struct method_group
{
  // Start using a TM method from this group. This constructs required meta
  // data on demand when this method group is actually used. Will be called
  // either on first use or after a previous call to fini().
  virtual void init() = 0;
  // Stop using any method from this group for now. This can be used to
  // destruct meta data as soon as this method group is not used anymore.
  virtual void fini() = 0;
  // This can be overriden to implement more light-weight re-initialization.
  virtual void reinit()
  {
    fini();
    init();
  }
};


// This is the base interface that all TM methods have to implement.
struct abi_dispatch
{
public:
  enum ls_modifier { NONTXNAL, R, RaR, RaW, RfW, W, WaR, WaW };

private:
  // Disallow copies
  abi_dispatch(const abi_dispatch &) = delete;
  abi_dispatch& operator=(const abi_dispatch &) = delete;

public:
  // Starts or restarts a transaction. Is called right before executing the
  // transactional application code (by either returning from
  // gtm_thread::begin_transaction or doing the longjmp when restarting).
  // Returns NO_RESTART if the transaction started successfully. Returns
  // a real restart reason if it couldn't start and does need to abort. This
  // allows TM methods to just give up and delegate ensuring progress to the
  // restart mechanism. If it returns a restart reason, this call must be
  // idempotent because it will trigger the restart mechanism, which could
  // switch to a different TM method.
  virtual gtm_restart_reason begin_or_restart() = 0;
  // Tries to commit the transaction. Iff this returns true, the transaction
  // got committed and all per-transaction data will have been reset.
  // Currently, this is called only for the commit of the outermost
  // transaction, or when switching to serial mode (which can happen in a
  // nested transaction).
  // If privatization safety must be ensured in a quiescence-based way, set
  // priv_time to a value different to 0. Nontransactional code will not be
  // executed after this commit until all registered threads' shared_state is
  // larger than or equal to this value.
  virtual bool trycommit(gtm_word& priv_time) = 0;
  // Rolls back a transaction. Called on abort or after trycommit() returned
  // false.
  virtual void rollback(gtm_transaction_cp *cp = 0) = 0;
  // Returns true iff the snapshot is most recent, which will be the case if
  // this transaction cannot be the reason why other transactions cannot
  // ensure privatization safety.
  virtual bool snapshot_most_recent() = 0;

  // Return an alternative method that is compatible with the current
  // method but supports closed nesting. Return zero if there is none.
  // Note that too be compatible, it must be possible to switch to this other
  // method on begin of a nested transaction without committing or restarting
  // the parent method.
  virtual abi_dispatch* closed_nesting_alternative() { return 0; }
  // Returns true iff this method group supports the current situation.
  // NUMBER_OF_THREADS is the current number of threads that might execute
  // transactions.
  virtual bool supports(unsigned number_of_threads) { return true; }

  bool read_only () const { return m_read_only; }
  bool write_through() const { return m_write_through; }
  bool can_run_uninstrumented_code() const
  {
    return m_can_run_uninstrumented_code;
  }
  // Returns true iff this TM method supports closed nesting.
  bool closed_nesting() const { return m_closed_nesting; }
  // Returns STATE_SERIAL or STATE_SERIAL | STATE_IRREVOCABLE iff the TM
  // method only works for serial-mode transactions.
  uint32_t requires_serial() const { return m_requires_serial; }
  method_group* get_method_group() const { return m_method_group; }

  static void *operator new(size_t s) { return xmalloc (s); }
  static void operator delete(void *p) { free (p); }

public:
  static bool memmove_overlap_check(void *dst, const void *src, size_t size,
      ls_modifier dst_mod, ls_modifier src_mod);

  // Creates the ABI dispatch methods for loads and stores.
  // ??? Should the dispatch table instead be embedded in the dispatch object
  // to avoid the indirect lookup in the vtable?
  CREATE_DISPATCH_METHODS_PV(virtual, )
  // Creates the ABI dispatch methods for memcpy/memmove/memset.
  CREATE_DISPATCH_METHODS_MEM_PV()

protected:
  const bool m_read_only;
  const bool m_write_through;
  const bool m_can_run_uninstrumented_code;
  const bool m_closed_nesting;
  const uint32_t m_requires_serial;
  method_group* const m_method_group;
  abi_dispatch(bool ro, bool wt, bool uninstrumented, bool closed_nesting,
      uint32_t requires_serial, method_group* mg) :
    m_read_only(ro), m_write_through(wt),
    m_can_run_uninstrumented_code(uninstrumented),
    m_closed_nesting(closed_nesting), m_requires_serial(requires_serial),
    m_method_group(mg)
  { }
};

}

#endif // DISPATCH_H

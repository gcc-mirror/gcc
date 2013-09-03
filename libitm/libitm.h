/* Copyright (C) 2008-2013 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* The external interface of this library follows the specification described
   in version 1 of http://www.intel.com/some/path/here.pdf.  */

#ifndef LIBITM_H
#define LIBITM_H 1

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __i386__
/* Only for 32-bit x86.  */
# define ITM_REGPARM	__attribute__((regparm(2)))
#else
# define ITM_REGPARM
#endif

#define ITM_NORETURN	__attribute__((noreturn))
#define ITM_PURE __attribute__((transaction_pure))

/* The following are externally visible definitions and functions, though
   only very few of these should be called by user code.  */

/* Values used as arguments to abort. */
typedef enum {
    userAbort = 1,
    userRetry = 2,
    TMConflict= 4,
    exceptionBlockAbort = 8,
    outerAbort = 16
} _ITM_abortReason;

/* Arguments to changeTransactionMode */
typedef enum
{
    modeSerialIrrevocable,
} _ITM_transactionState;

/* Results from inTransaction */
typedef enum
{
    outsideTransaction = 0,    /* So "if (inTransaction(td))" works */
    inRetryableTransaction,
    inIrrevocableTransaction
} _ITM_howExecuting;

/* Values to describe properties of code, passed in to beginTransaction.
   Some of these constants are duplicated in some of the ITM_beginTransaction
   implementations, so update those too when applying any changes.  */
typedef enum
{
   pr_instrumentedCode		= 0x0001,
   pr_uninstrumentedCode	= 0x0002,
   pr_multiwayCode		= pr_instrumentedCode | pr_uninstrumentedCode,
   /* Called pr_hasNoXMMUpdate in the Intel document, used for
      avoiding vector register save/restore for any target.  */
   pr_hasNoVectorUpdate		= 0x0004,
   pr_hasNoAbort		= 0x0008,
   /* Not present in the Intel document, used for avoiding
      floating point register save/restore for any target.  */
   pr_hasNoFloatUpdate		= 0x0010,
   pr_hasNoIrrevocable		= 0x0020,
   pr_doesGoIrrevocable		= 0x0040,
   pr_aWBarriersOmitted		= 0x0100,
   pr_RaRBarriersOmitted	= 0x0200,
   pr_undoLogCode		= 0x0400,
   pr_preferUninstrumented	= 0x0800,
   /* Exception blocks are not used nor supported. */
   pr_exceptionBlock		= 0x1000,
   pr_hasElse			= 0x2000,
   pr_readOnly			= 0x4000,
   pr_hasNoSimpleReads		= 0x400000,
   /* These are not part of the ABI but used for custom HTM fast paths.  See
      ITM_beginTransaction and gtm_thread::begin_transaction.  */
   pr_HTMRetryableAbort		= 0x800000,
   pr_HTMRetriedAfterAbort	= 0x1000000
} _ITM_codeProperties;

/* Result from startTransaction that describes what actions to take.
   Some of these constants are duplicated in some of the ITM_beginTransaction
   implementations, so update those too when applying any changes.  */
typedef enum
{
   a_runInstrumentedCode       = 0x01,
   a_runUninstrumentedCode     = 0x02,
   a_saveLiveVariables         = 0x04,
   a_restoreLiveVariables      = 0x08,
   a_abortTransaction          = 0x10,
   a_tryHTMFastPath            = 0x20
} _ITM_actions;

typedef struct
{
    uint32_t reserved_1;
    uint32_t flags;
    uint32_t reserved_2;
    uint32_t reserved_3;
    const char *psource;
} _ITM_srcLocation;

typedef void (* _ITM_userUndoFunction)(void *);
typedef void (* _ITM_userCommitFunction) (void *);

#define _ITM_VERSION "0.90 (Feb 29 2008)"
#define _ITM_VERSION_NO 90

extern int _ITM_versionCompatible (int) ITM_REGPARM;
extern const char * _ITM_libraryVersion (void) ITM_REGPARM;

void _ITM_error(const _ITM_srcLocation *, int errorCode)
  ITM_REGPARM ITM_NORETURN;

extern _ITM_howExecuting _ITM_inTransaction(void) ITM_REGPARM;

typedef uint64_t _ITM_transactionId_t;	/* Transaction identifier */
#define _ITM_noTransactionId 1		/* Id for non-transactional code. */

extern _ITM_transactionId_t _ITM_getTransactionId(void) ITM_REGPARM;

extern uint32_t _ITM_beginTransaction(uint32_t, ...) ITM_REGPARM;

extern void _ITM_abortTransaction(_ITM_abortReason) ITM_REGPARM ITM_NORETURN;

extern void _ITM_commitTransaction (void) ITM_REGPARM;

extern void _ITM_changeTransactionMode (_ITM_transactionState) ITM_REGPARM;

extern void _ITM_addUserCommitAction(_ITM_userCommitFunction,
				     _ITM_transactionId_t, void *) ITM_REGPARM;

extern void _ITM_addUserUndoAction(_ITM_userUndoFunction, void *) ITM_REGPARM;

extern void _ITM_dropReferences (void *, size_t) ITM_REGPARM ITM_PURE;

extern void *_ITM_malloc (size_t)
       __attribute__((__malloc__)) ITM_PURE;

extern void *_ITM_calloc (size_t, size_t)
       __attribute__((__malloc__)) ITM_PURE;

extern  void _ITM_free (void *) ITM_PURE;


/* The following typedefs exist to make the macro expansions below work
   properly.  They are not part of any API.  */
typedef uint8_t  _ITM_TYPE_U1;
typedef uint16_t _ITM_TYPE_U2;
typedef uint32_t _ITM_TYPE_U4;
typedef uint64_t _ITM_TYPE_U8;
typedef float    _ITM_TYPE_F;
typedef double   _ITM_TYPE_D;
typedef long double _ITM_TYPE_E;
typedef float _Complex _ITM_TYPE_CF;
typedef double _Complex _ITM_TYPE_CD;
typedef long double _Complex _ITM_TYPE_CE;

#define ITM_BARRIERS(T) \
  extern _ITM_TYPE_##T _ITM_R##T(const _ITM_TYPE_##T *) ITM_REGPARM;	\
  extern _ITM_TYPE_##T _ITM_RaR##T(const _ITM_TYPE_##T *) ITM_REGPARM;	\
  extern _ITM_TYPE_##T _ITM_RaW##T(const _ITM_TYPE_##T *) ITM_REGPARM;	\
  extern _ITM_TYPE_##T _ITM_RfW##T(const _ITM_TYPE_##T *) ITM_REGPARM;	\
  extern void _ITM_W##T (_ITM_TYPE_##T *, _ITM_TYPE_##T) ITM_REGPARM;	\
  extern void _ITM_WaR##T (_ITM_TYPE_##T *, _ITM_TYPE_##T) ITM_REGPARM;	\
  extern void _ITM_WaW##T (_ITM_TYPE_##T *, _ITM_TYPE_##T) ITM_REGPARM;

ITM_BARRIERS(U1)
ITM_BARRIERS(U2)
ITM_BARRIERS(U4)
ITM_BARRIERS(U8)
ITM_BARRIERS(F)
ITM_BARRIERS(D)
ITM_BARRIERS(E)
ITM_BARRIERS(CF)
ITM_BARRIERS(CD)
ITM_BARRIERS(CE)

#define ITM_LOG(T) \
  extern void _ITM_L##T (const _ITM_TYPE_##T *) ITM_REGPARM;

ITM_LOG(U1)
ITM_LOG(U2)
ITM_LOG(U4)
ITM_LOG(U8)
ITM_LOG(F)
ITM_LOG(D)
ITM_LOG(E)
ITM_LOG(CF)
ITM_LOG(CD)
ITM_LOG(CE)

#if defined(__i386__) || defined(__x86_64__)
# ifdef __MMX__
  typedef int _ITM_TYPE_M64 __attribute__((vector_size(8), may_alias));
  ITM_BARRIERS(M64)
  ITM_LOG(M64)
# endif
# ifdef __SSE__
  typedef float _ITM_TYPE_M128 __attribute__((vector_size(16), may_alias));
  ITM_BARRIERS(M128)
  ITM_LOG(M128)
# endif
# ifdef __AVX__
  typedef float _ITM_TYPE_M256 __attribute__((vector_size(32), may_alias));
  ITM_BARRIERS(M256)
  ITM_LOG(M256)
# endif
#endif /* i386 */

#undef ITM_BARRIERS
#undef ITM_LOG

extern void _ITM_LB (const void *, size_t) ITM_REGPARM;

extern void _ITM_memcpyRnWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRnWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRnWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaRWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaRWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaRWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaRWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaWWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaWWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaWWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memcpyRtaWWtaW(void *, const void *, size_t) ITM_REGPARM;

extern void _ITM_memmoveRnWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRnWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRnWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaRWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaRWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaRWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaRWtaW(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaWWn(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaWWt(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaWWtaR(void *, const void *, size_t) ITM_REGPARM;
extern void _ITM_memmoveRtaWWtaW(void *, const void *, size_t) ITM_REGPARM;

extern void _ITM_memsetW(void *, int, size_t) ITM_REGPARM;
extern void _ITM_memsetWaR(void *, int, size_t) ITM_REGPARM;
extern void _ITM_memsetWaW(void *, int, size_t) ITM_REGPARM;

// ??? These are not yet in the official spec; still work-in-progress.

extern void *_ITM_getTMCloneOrIrrevocable (void *) ITM_REGPARM;
extern void *_ITM_getTMCloneSafe (void *) ITM_REGPARM;
extern void _ITM_registerTMCloneTable (void *, size_t);
extern void _ITM_deregisterTMCloneTable (void *);

extern void *_ITM_cxa_allocate_exception (size_t);
extern void _ITM_cxa_throw (void *obj, void *tinfo, void *dest);
extern void *_ITM_cxa_begin_catch (void *exc_ptr);
extern void _ITM_cxa_end_catch (void);
extern void _ITM_commitTransactionEH(void *exc_ptr) ITM_REGPARM;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LIBITM_H */

/* go-unwind.c -- unwind the stack for panic/recover.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <stdlib.h>
#include <unistd.h>

#include "unwind.h"

#include "runtime.h"

/* These constants are documented here:
   https://refspecs.linuxfoundation.org/LSB_3.0.0/LSB-PDA/LSB-PDA/dwarfext.html
 */

#define DW_EH_PE_omit     0xff
#define DW_EH_PE_absptr   0x00
#define DW_EH_PE_uleb128  0x01
#define DW_EH_PE_udata2   0x02
#define DW_EH_PE_udata4   0x03
#define DW_EH_PE_udata8   0x04
#define DW_EH_PE_sleb128  0x09
#define DW_EH_PE_sdata2   0x0A
#define DW_EH_PE_sdata4   0x0B
#define DW_EH_PE_sdata8   0x0C
#define DW_EH_PE_pcrel    0x10
#define DW_EH_PE_textrel  0x20
#define DW_EH_PE_datarel  0x30
#define DW_EH_PE_funcrel  0x40
#define DW_EH_PE_aligned  0x50
#define DW_EH_PE_indirect 0x80

/* The code for a Go exception.  */

#ifdef __ARM_EABI_UNWINDER__
static const _Unwind_Exception_Class __go_exception_class =
  { 'G', 'N', 'U', 'C', 'G', 'O', '\0', '\0' };
#else
static const _Unwind_Exception_Class __go_exception_class =
  ((((((((_Unwind_Exception_Class) 'G' 
         << 8 | (_Unwind_Exception_Class) 'N')
        << 8 | (_Unwind_Exception_Class) 'U')
       << 8 | (_Unwind_Exception_Class) 'C')
      << 8 | (_Unwind_Exception_Class) 'G')
     << 8 | (_Unwind_Exception_Class) 'O')
    << 8 | (_Unwind_Exception_Class) '\0')
   << 8 | (_Unwind_Exception_Class) '\0');
#endif

/* Rethrow an exception.  */

void rethrowException (void) __asm__(GOSYM_PREFIX "runtime.rethrowException");

void
rethrowException ()
{
  struct _Unwind_Exception *hdr;

  hdr = (struct _Unwind_Exception *) runtime_g()->exception;

#ifdef __USING_SJLJ_EXCEPTIONS__
  _Unwind_SjLj_Resume_or_Rethrow (hdr);
#else
#if defined(_LIBUNWIND_STD_ABI)
  _Unwind_RaiseException (hdr);
#else
  _Unwind_Resume_or_Rethrow (hdr);
#endif
#endif

  /* Rethrowing the exception should not return.  */
  abort();
}

/* Return the size of the type that holds an exception header, so that
   it can be allocated by Go code.  */

uintptr unwindExceptionSize(void)
  __asm__ (GOSYM_PREFIX "runtime.unwindExceptionSize");

uintptr
unwindExceptionSize ()
{
  uintptr ret, align;

  ret = sizeof (struct _Unwind_Exception);
  /* Adjust the size fo make sure that we can get an aligned value.  */
  align = __alignof__ (struct _Unwind_Exception);
  if (align > __alignof__ (uintptr))
    ret += align - __alignof__ (uintptr);
  return ret;
}

/* Throw an exception.  This is called with g->exception pointing to
   an uninitialized _Unwind_Exception instance.  */

void throwException (void) __asm__(GOSYM_PREFIX "runtime.throwException");

void
throwException ()
{
  struct _Unwind_Exception *hdr;
  uintptr align;

  hdr = (struct _Unwind_Exception *)runtime_g ()->exception;

  /* Make sure the value is correctly aligned.  It will be large
     enough, because of unwindExceptionSize.  */
  align = __alignof__ (struct _Unwind_Exception);
  hdr = ((struct _Unwind_Exception *)
	 (((uintptr) hdr + align - 1) &~ (align - 1)));

  __builtin_memcpy (&hdr->exception_class, &__go_exception_class,
		    sizeof hdr->exception_class);
  hdr->exception_cleanup = NULL;

#ifdef __USING_SJLJ_EXCEPTIONS__
  _Unwind_SjLj_RaiseException (hdr);
#else
  _Unwind_RaiseException (hdr);
#endif

  /* Raising an exception should not return.  */
  abort ();
}

static inline _Unwind_Ptr
encoded_value_base (uint8_t encoding, struct _Unwind_Context *context)
{
  if (encoding == DW_EH_PE_omit)
    return 0;
  switch (encoding & 0x70)
    {
      case DW_EH_PE_absptr:
      case DW_EH_PE_pcrel:
      case DW_EH_PE_aligned:
        return 0;
      case DW_EH_PE_textrel:
        return _Unwind_GetTextRelBase(context);
      case DW_EH_PE_datarel:
        return _Unwind_GetDataRelBase(context);
      case DW_EH_PE_funcrel:
        return _Unwind_GetRegionStart(context);
    }
  abort ();
}

/* Read an unsigned leb128 value.  */

static inline const uint8_t *
read_uleb128 (const uint8_t *p, _uleb128_t *val)
{
  unsigned int shift = 0;
  _uleb128_t result = 0;
  uint8_t byte;

  do
    {
      byte = *p++;
      result |= ((_uleb128_t)byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  *val = result;
  return p;
}

/* Similar, but read a signed leb128 value.  */

static inline const uint8_t *
read_sleb128 (const uint8_t *p, _sleb128_t *val)
{
  unsigned int shift = 0;
  _uleb128_t result = 0;
  uint8_t byte;

  do
    {
      byte = *p++;
      result |= ((_uleb128_t)byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  /* sign extension */
  if (shift < (8 * sizeof(result)) && (byte & 0x40) != 0)
    result |= (((_uleb128_t)~0) << shift);

  *val = (_sleb128_t)result;
  return p;
}

#define ROUND_UP_TO_PVB(x) (x + sizeof(void *) - 1) &- sizeof(void *)

static inline const uint8_t *
read_encoded_value (struct _Unwind_Context *context, uint8_t encoding,
                    const uint8_t *p, _Unwind_Ptr *val)
{
  _Unwind_Ptr base = encoded_value_base (encoding, context);
  _Unwind_Internal_Ptr decoded = 0;
  const uint8_t *origp = p;

  if (encoding == DW_EH_PE_aligned)
    {
      _Unwind_Internal_Ptr uip = (_Unwind_Internal_Ptr)p;
      uip = ROUND_UP_TO_PVB (uip);
      decoded = *(_Unwind_Internal_Ptr *)uip;
      p = (const uint8_t *)(uip + sizeof(void *));
    }
  else
    {
      switch (encoding & 0x0f)
        {
          case DW_EH_PE_sdata2:
            {
              int16_t result;
              __builtin_memcpy (&result, p, sizeof(int16_t));
              decoded = result;
              p += sizeof(int16_t);
              break;
            }
          case DW_EH_PE_udata2:
            {
              uint16_t result;
              __builtin_memcpy (&result, p, sizeof(uint16_t));
              decoded = result;
              p += sizeof(uint16_t);
              break;
            }
          case DW_EH_PE_sdata4:
            {
              int32_t result;
              __builtin_memcpy (&result, p, sizeof(int32_t));
              decoded = result;
              p += sizeof(int32_t);
              break;
            }
          case DW_EH_PE_udata4:
            {
              uint32_t result;
              __builtin_memcpy (&result, p, sizeof(uint32_t));
              decoded = result;
              p += sizeof(uint32_t);
              break;
            }
          case DW_EH_PE_sdata8:
            {
              int64_t result;
              __builtin_memcpy (&result, p, sizeof(int64_t));
              decoded = result;
              p += sizeof(int64_t);
              break;
            }
          case DW_EH_PE_udata8:
            {
              uint64_t result;
              __builtin_memcpy (&result, p, sizeof(uint64_t));
              decoded = result;
              p += sizeof(uint64_t);
              break;
            }
          case DW_EH_PE_uleb128:
            {
              _uleb128_t value;
              p = read_uleb128 (p, &value);
              decoded = (_Unwind_Internal_Ptr)value;
              break;
            }
          case DW_EH_PE_sleb128:
            {
              _sleb128_t value;
              p = read_sleb128 (p, &value);
              decoded = (_Unwind_Internal_Ptr)value;
              break;
            }
          case DW_EH_PE_absptr:
            __builtin_memcpy (&decoded, (const void *)p, sizeof(const void*));
            p += sizeof(void *);
            break;
          default:
            abort ();
        }

      if (decoded == 0)
        {
          *val = decoded;
          return p;
        }

      if ((encoding & 0x70) == DW_EH_PE_pcrel)
        decoded += ((_Unwind_Internal_Ptr)origp);
      else
        decoded += base;

      if ((encoding & DW_EH_PE_indirect) != 0)
        decoded = *(_Unwind_Internal_Ptr *)decoded;
    }
  *val = decoded;
  return p;
}

static inline int
value_size (uint8_t encoding)
{
  switch (encoding & 0x0f)
    {
      case DW_EH_PE_sdata2:
      case DW_EH_PE_udata2:
        return 2;
      case DW_EH_PE_sdata4:
      case DW_EH_PE_udata4:
        return 4;
      case DW_EH_PE_sdata8:
      case DW_EH_PE_udata8:
        return 8;
      case DW_EH_PE_absptr:
        return sizeof(uintptr);
      default:
        break;
    }
  abort ();
}

/* The rest of this code is really similar to gcc/unwind-c.c and
   libjava/exception.cc.  */

typedef struct
{
  _Unwind_Ptr Start;
  _Unwind_Ptr LPStart;
  _Unwind_Ptr ttype_base;
  const unsigned char *TType;
  const unsigned char *action_table;
  unsigned char ttype_encoding;
  unsigned char call_site_encoding;
} lsda_header_info;

static const unsigned char *
parse_lsda_header (struct _Unwind_Context *context, const unsigned char *p,
		   lsda_header_info *info)
{
  _uleb128_t tmp;
  unsigned char lpstart_encoding;

  info->Start = (context ? _Unwind_GetRegionStart (context) : 0);

  /* Find @LPStart, the base to which landing pad offsets are relative.  */
  lpstart_encoding = *p++;
  if (lpstart_encoding != DW_EH_PE_omit)
    p = read_encoded_value (context, lpstart_encoding, p, &info->LPStart);
  else
    info->LPStart = info->Start;

  /* Find @TType, the base of the handler and exception spec type data.  */
  info->ttype_encoding = *p++;
  if (info->ttype_encoding != DW_EH_PE_omit)
    {
      p = read_uleb128 (p, &tmp);
      info->TType = p + tmp;
    }
  else
    info->TType = 0;

  /* The encoding and length of the call-site table; the action table
     immediately follows.  */
  info->call_site_encoding = *p++;
  p = read_uleb128 (p, &tmp);
  info->action_table = p + tmp;

  return p;
}

/* The personality function is invoked when unwinding the stack due to
   a panic.  Its job is to find the cleanup and exception handlers to
   run.  We can't split the stack here, because we won't be able to
   unwind from that split.  */

#ifdef __ARM_EABI_UNWINDER__
/* ARM EABI personality routines must also unwind the stack.  */
#define CONTINUE_UNWINDING \
  do								\
    {								\
      if (__gnu_unwind_frame (ue_header, context) != _URC_OK)	\
	return _URC_FAILURE;					\
      return _URC_CONTINUE_UNWIND;				\
    }								\
  while (0)
#else
#define CONTINUE_UNWINDING return _URC_CONTINUE_UNWIND
#endif

#ifdef __ARM_EABI_UNWINDER__
#define STOP_UNWINDING _URC_FAILURE
#else
#define STOP_UNWINDING _URC_NORMAL_STOP
#endif

#ifdef __USING_SJLJ_EXCEPTIONS__
#define PERSONALITY_FUNCTION    __gccgo_personality_sj0
#define __builtin_eh_return_data_regno(x) x
#else
#define PERSONALITY_FUNCTION    __gccgo_personality_v0
#endif

#ifdef __ARM_EABI_UNWINDER__
_Unwind_Reason_Code
PERSONALITY_FUNCTION (_Unwind_State, struct _Unwind_Exception *,
		      struct _Unwind_Context *)
  __attribute__ ((no_split_stack, flatten, target ("general-regs-only")));

_Unwind_Reason_Code
PERSONALITY_FUNCTION (_Unwind_State state,
		      struct _Unwind_Exception * ue_header,
		      struct _Unwind_Context * context)
#else
_Unwind_Reason_Code
PERSONALITY_FUNCTION (int, _Unwind_Action, _Unwind_Exception_Class,
		      struct _Unwind_Exception *, struct _Unwind_Context *)
  __attribute__ ((no_split_stack, flatten));

_Unwind_Reason_Code
PERSONALITY_FUNCTION (int version,
		      _Unwind_Action actions,
		      _Unwind_Exception_Class exception_class,
		      struct _Unwind_Exception *ue_header,
		      struct _Unwind_Context *context)
#endif
{
  lsda_header_info info;
  const unsigned char *language_specific_data, *p, *action_record;
  _Unwind_Ptr landing_pad, ip;
  int ip_before_insn = 0;
  _Bool is_foreign;
  G *g;

#ifdef __ARM_EABI_UNWINDER__
  _Unwind_Action actions;

  switch (state & _US_ACTION_MASK)
    {
    case _US_VIRTUAL_UNWIND_FRAME:
      if (state & _UA_FORCE_UNWIND)
        /* We are called from _Unwind_Backtrace.  No handler to run.  */
        CONTINUE_UNWINDING;
      actions = _UA_SEARCH_PHASE;
      break;

    case _US_UNWIND_FRAME_STARTING:
      actions = _UA_CLEANUP_PHASE;
      if (!(state & _US_FORCE_UNWIND)
	  && ue_header->barrier_cache.sp == _Unwind_GetGR(context, 13))
	actions |= _UA_HANDLER_FRAME;
      break;

    case _US_UNWIND_FRAME_RESUME:
      CONTINUE_UNWINDING;
      break;

    default:
      abort();
    }
  actions |= state & _US_FORCE_UNWIND;

  is_foreign = 0;

  /* The dwarf unwinder assumes the context structure holds things like the
     function and LSDA pointers.  The ARM implementation caches these in
     the exception header (UCB).  To avoid rewriting everything we make the
     virtual IP register point at the UCB.  */
  ip = (_Unwind_Ptr) ue_header;
  _Unwind_SetGR (context, 12, ip);
#else
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;

  is_foreign = exception_class != __go_exception_class;
#endif

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there are no handlers or cleanups.  */
  if (! language_specific_data)
    CONTINUE_UNWINDING;

  /* Parse the LSDA header.  */
  p = parse_lsda_header (context, language_specific_data, &info);
#ifdef HAVE_GETIPINFO
  ip = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  ip = _Unwind_GetIP (context);
#endif
  if (! ip_before_insn)
    --ip;
  landing_pad = 0;
  action_record = NULL;

#ifdef __USING_SJLJ_EXCEPTIONS__
  /* The given "IP" is an index into the call-site table, with two
     exceptions -- -1 means no-action, and 0 means terminate.  But
     since we're using uleb128 values, we've not got random access
     to the array.  */
  if ((int) ip <= 0)
    return _URC_CONTINUE_UNWIND;
  else
    {
      _uleb128_t cs_lp, cs_action;
      do
	{
	  p = read_uleb128 (p, &cs_lp);
	  p = read_uleb128 (p, &cs_action);
	}
      while (--ip);

      /* Can never have null landing pad for sjlj -- that would have
	 been indicated by a -1 call site index.  */
      landing_pad = (_Unwind_Ptr)cs_lp + 1;
      if (cs_action)
	action_record = info.action_table + cs_action - 1;
      goto found_something;
    }
#else
  /* Search the call-site table for the action associated with this IP.  */
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _uleb128_t cs_action;

      /* Note that all call-site encodings are "absolute" displacements.  */
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      /* The table is sorted, so if we've passed the ip, stop.  */
      if (ip < info.Start + cs_start)
	p = info.action_table;
      else if (ip < info.Start + cs_start + cs_len)
	{
	  if (cs_lp)
	    landing_pad = info.LPStart + cs_lp;
	  if (cs_action)
	    action_record = info.action_table + cs_action - 1;
	  goto found_something;
	}
    }
#endif

  /* IP is not in table.  No associated cleanups.  */
  CONTINUE_UNWINDING;

 found_something:
  if (landing_pad == 0)
    {
      /* IP is present, but has a null landing pad.
	 No handler to be run.  */
      CONTINUE_UNWINDING;
    }

  if (actions & _UA_SEARCH_PHASE)
    {
      if (action_record == 0)
	{
	  /* This indicates a cleanup rather than an exception
	     handler.  */
	  CONTINUE_UNWINDING;
	}

      return _URC_HANDLER_FOUND;
    }

  /* It's possible for g to be NULL here for an exception thrown by a
     language other than Go.  */
  g = runtime_g ();
  if (g == NULL)
    {
      if (!is_foreign)
	abort ();
    }
  else
    {
      g->exception = ue_header;
      g->isforeign = is_foreign;
    }

  _Unwind_SetGR (context, __builtin_eh_return_data_regno (0),
		 (_Unwind_Ptr) ue_header);
  _Unwind_SetGR (context, __builtin_eh_return_data_regno (1), 0);
  _Unwind_SetIP (context, landing_pad);
  return _URC_INSTALL_CONTEXT;
}

// A dummy personality function, which doesn't capture any exception
// and simply passes by. This is used for functions that don't
// capture exceptions but need LSDA for stack maps.
_Unwind_Reason_Code
__gccgo_personality_dummy (int, _Unwind_Action, _Unwind_Exception_Class,
		      struct _Unwind_Exception *, struct _Unwind_Context *)
  __attribute__ ((no_split_stack));

_Unwind_Reason_Code
#ifdef __ARM_EABI_UNWINDER__
__attribute__ ((target ("general-regs-only")))
#endif
__gccgo_personality_dummy (int version __attribute__ ((unused)),
		      _Unwind_Action actions __attribute__ ((unused)),
		      _Unwind_Exception_Class exception_class __attribute__ ((unused)),
		      struct _Unwind_Exception *ue_header __attribute__ ((unused)),
		      struct _Unwind_Context *context __attribute__ ((unused)))
{
  CONTINUE_UNWINDING;
}

// A sentinel value for Go functions.
// A function is a Go function if it has LSDA, which has type info,
// and the first (dummy) landing pad's type info is a pointer to
// this value.
#define GO_FUNC_SENTINEL ((uint64)'G' | ((uint64)'O'<<8) | \
                          ((uint64)'.'<<16) | ((uint64)'.'<<24) | \
                          ((uint64)'F'<<32) | ((uint64)'U'<<40) | \
                          ((uint64)'N'<<48) | ((uint64)'C'<<56))

struct _stackmap {
  uint32 len;
  uint8 data[1]; // variabe length
};

extern void
  runtime_scanstackblockwithmap (uintptr ip, uintptr sp, uintptr size, uint8 *ptrmask, void* gcw)
  __asm__ (GOSYM_PREFIX "runtime.scanstackblockwithmap");

#define FOUND        0
#define NOTFOUND_OK  1
#define NOTFOUND_BAD 2

// Helper function to search for stack maps in the unwinding records of a frame.
// If found, populate ip, sp, and stackmap. Returns the #define'd values above.
static int
findstackmaps (struct _Unwind_Context *context, _Unwind_Ptr *ip, _Unwind_Ptr *sp, struct _stackmap **stackmap)
{
  lsda_header_info info;
  const unsigned char *language_specific_data, *p, *action_record;
  bool first;
  struct _stackmap *stackmap1;
  _Unwind_Ptr ip1;
  int ip_before_insn = 0;
  _sleb128_t index;
  int size;

#ifdef HAVE_GETIPINFO
  ip1 = _Unwind_GetIPInfo (context, &ip_before_insn);
#else
  ip1 = _Unwind_GetIP (context);
#endif
  if (! ip_before_insn)
    --ip1;

  if (ip != NULL)
    *ip = ip1;
  if (sp != NULL)
    *sp = _Unwind_GetCFA (context);

#ifdef __ARM_EABI_UNWINDER__
  {
    _Unwind_Control_Block *ucbp;
    ucbp = (_Unwind_Control_Block *) _Unwind_GetGR (context, 12);
    if (*ucbp->pr_cache.ehtp & (1u << 31))
      // The "compact" model is used, with one of the predefined
      // personality functions. It doesn't have standard LSDA.
      return NOTFOUND_OK;
  }
#endif

  language_specific_data = (const unsigned char *)
    _Unwind_GetLanguageSpecificData (context);

  /* If no LSDA, then there is no stack maps.  */
  if (! language_specific_data)
    return NOTFOUND_OK;

  p = parse_lsda_header (context, language_specific_data, &info);

  if (info.TType == NULL)
    return NOTFOUND_OK;

  size = value_size (info.ttype_encoding);

  action_record = NULL;
  first = true;

  /* Search the call-site table for the action associated with this IP.  */
  while (p < info.action_table)
    {
      _Unwind_Ptr cs_start, cs_len, cs_lp;
      _uleb128_t cs_action;

      /* Note that all call-site encodings are "absolute" displacements.  */
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_start);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_len);
      p = read_encoded_value (0, info.call_site_encoding, p, &cs_lp);
      p = read_uleb128 (p, &cs_action);

      if (first)
        {
          // For a Go function, the first entry points to the sentinel value.
          // Check this here.
          const unsigned char *p1, *action1;
          uint64 *x;

          if (!cs_action)
            return NOTFOUND_OK;

          action1 = info.action_table + cs_action - 1;
          read_sleb128 (action1, &index);
          p1 = info.TType - index*size;
          read_encoded_value (context, info.ttype_encoding, p1, (_Unwind_Ptr*)&x);
          if (x == NULL || *x != GO_FUNC_SENTINEL)
            return NOTFOUND_OK;

          first = false;
          continue;
        }

      /* The table is sorted, so if we've passed the ip, stop.  */
      if (ip1 < info.Start + cs_start)
        return NOTFOUND_BAD;
      else if (ip1 < info.Start + cs_start + cs_len)
        {
          if (cs_action)
            action_record = info.action_table + cs_action - 1;
          break;
        }
    }

  if (action_record == NULL)
    return NOTFOUND_BAD;

  read_sleb128 (action_record, &index);
  p = info.TType - index*size;
  read_encoded_value (context, info.ttype_encoding, p, (_Unwind_Ptr*)&stackmap1);
  if (stackmap1 == NULL)
    return NOTFOUND_BAD;

  if (stackmap != NULL)
    *stackmap = stackmap1;
  return FOUND;
}

struct scanstate {
  void* gcw;      // the GC worker, passed into scanstackwithmap_callback
  uintptr lastsp; // the last (outermost) SP of Go function seen in a traceback, set by the callback
};

// Callback function to scan a stack frame with stack maps.
// It skips non-Go functions.
static _Unwind_Reason_Code
scanstackwithmap_callback (struct _Unwind_Context *context, void *arg)
{
  struct _stackmap *stackmap;
  _Unwind_Ptr ip, sp;
  G* gp;
  struct scanstate* state = (struct scanstate*) arg;
  void *gcw;

  gp = runtime_g ();
  gcw = state->gcw;

  switch (findstackmaps (context, &ip, &sp, &stackmap))
    {
      case NOTFOUND_OK:
        // Not a Go function. Skip this frame.
        return _URC_NO_REASON;
      case NOTFOUND_BAD:
        {
          // No stack map found.
          // If we're scanning from the signal stack, the goroutine
          // may be not stopped at a safepoint. Allow this case.
          if (gp != gp->m->gsignal)
            {
              // TODO: print gp, pc, sp
              runtime_throw ("no stack map");
            }
          return STOP_UNWINDING;
        }
      case FOUND:
        break;
      default:
        abort ();
    }

  state->lastsp = sp;
  runtime_scanstackblockwithmap (ip, sp, (uintptr)(stackmap->len) * sizeof(uintptr), stackmap->data, gcw);

  return _URC_NO_REASON;
}

// Scan the stack with stack maps. Return whether the scan
// succeeded.
bool
scanstackwithmap (void *gcw)
{
  _Unwind_Reason_Code code;
  bool ret;
  struct scanstate state;
  G* gp;
  G* curg;

  state.gcw = gcw;
  state.lastsp = 0;
  gp = runtime_g ();
  curg = gp->m->curg;

  runtime_xadd (&__go_runtime_in_callers, 1);
  code = _Unwind_Backtrace (scanstackwithmap_callback, (void*)&state);
  runtime_xadd (&__go_runtime_in_callers, -1);
  ret = (code == _URC_END_OF_STACK);
  if (ret && gp == gp->m->gsignal)
    {
      // For signal-triggered scan, the unwinder may not be able to unwind
      // the whole stack while it still reports _URC_END_OF_STACK (e.g.
      // signal is delivered in vdso). Check that we actually reached the
      // the end of the stack, that is, the SP on entry.
      if (state.lastsp != curg->entrysp)
        ret = false;
    }
  return ret;
}

// Returns whether stack map is enabled.
bool
usestackmaps ()
{
  return runtime_usestackmaps;
}

// Callback function to probe if a stack frame has stack maps.
static _Unwind_Reason_Code
probestackmaps_callback (struct _Unwind_Context *context,
                         void *arg __attribute__ ((unused)))
{
  switch (findstackmaps (context, NULL, NULL, NULL))
    {
      case NOTFOUND_OK:
      case NOTFOUND_BAD:
        return _URC_NO_REASON;
      case FOUND:
        break;
      default:
        abort ();
    }

  // Found a stack map. No need to keep unwinding.
  runtime_usestackmaps = true;
  return STOP_UNWINDING;
}

// Try to find a stack map, store the result in global variable runtime_usestackmaps.
// Called in start-up time from Go code, so there is a Go frame on the stack.
bool
probestackmaps ()
{
  runtime_usestackmaps = false;
  _Unwind_Backtrace (probestackmaps_callback, NULL);
  return runtime_usestackmaps;
}

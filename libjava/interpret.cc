// interpret.cc - Code for the interpreter

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>
#include <platform.h>

#pragma implementation "java-interp.h"

#include <jvm.h>
#include <java-cpool.h>
#include <java-interp.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/Integer.h>
#include <java/lang/Long.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Class.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/InternalError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArithmeticException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/InstantiationException.h>
#include <java/lang/Thread.h>
#include <java-insns.h>
#include <java-signal.h>
#include <java/lang/ClassFormatError.h>
#include <execution.h>
#include <java/lang/reflect/Modifier.h>

#include <jvmti.h>
#include "jvmti-int.h"

#include <gnu/gcj/jvmti/Breakpoint.h>
#include <gnu/gcj/jvmti/BreakpointManager.h>

// Execution engine for interpreted code.
_Jv_InterpreterEngine _Jv_soleInterpreterEngine;

#include <stdlib.h>

using namespace gcj;

static void throw_internal_error (const char *msg)
  __attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
  __attribute__ ((__noreturn__));
static void throw_null_pointer_exception ()
  __attribute__ ((__noreturn__));

static void throw_class_format_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (const char *msg)
	__attribute__ ((__noreturn__));

static void find_catch_location (jthrowable, jthread, jmethodID *, jlong *);

// A macro to facilitate JVMTI exception reporting
#define REPORT_EXCEPTION(Jthrowable)			\
  do {							\
    if (JVMTI_REQUESTED_EVENT (Exception))		\
      _Jv_ReportJVMTIExceptionThrow (Jthrowable);	\
  }							\
  while (0)

#ifdef DIRECT_THREADED
// Lock to ensure that methods are not compiled concurrently.
// We could use a finer-grained lock here, however it is not safe to use
// the Class monitor as user code in another thread could hold it.
static _Jv_Mutex_t compile_mutex;

// See class ThreadCountAdjuster and REWRITE_INSN for how this is
// used.
_Jv_Mutex_t _Jv_InterpMethod::rewrite_insn_mutex;

void
_Jv_InitInterpreter()
{
  _Jv_MutexInit (&compile_mutex);
  _Jv_MutexInit (&_Jv_InterpMethod::rewrite_insn_mutex);
}
#else
void _Jv_InitInterpreter() {}
#endif

// The breakpoint instruction. For the direct threaded case,
// _Jv_InterpMethod::compile will initialize breakpoint_insn
// the first time it is called.
#ifdef DIRECT_THREADED
insn_slot _Jv_InterpMethod::bp_insn_slot;
pc_t _Jv_InterpMethod::breakpoint_insn = NULL;
#else
unsigned char _Jv_InterpMethod::bp_insn_opcode
  = static_cast<unsigned char> (op_breakpoint);
pc_t _Jv_InterpMethod::breakpoint_insn = &_Jv_InterpMethod::bp_insn_opcode;
#endif

extern "C" double __ieee754_fmod (double,double);

static inline void dupx (_Jv_word *sp, int n, int x)
{
  // first "slide" n+x elements n to the right
  int top = n-1;
  for (int i = 0; i < n+x; i++)
    {
      sp[(top-i)] = sp[(top-i)-n];
    }
  
  // next, copy the n top elements, n+x down
  for (int i = 0; i < n; i++)
    {
      sp[top-(n+x)-i] = sp[top-i];
    }
}

// Used to convert from floating types to integral types.
template<typename TO, typename FROM>
static inline TO
convert (FROM val, TO min, TO max)
{
  TO ret;
  if (val >= (FROM) max)
    ret = max;
  else if (val <= (FROM) min)
    ret = min;
  else if (val != val)
    ret = 0;
  else
    ret = (TO) val;
  return ret;
}

#define PUSHA(V)  (sp++)->o = (V)
#define PUSHI(V)  (sp++)->i = (V)
#define PUSHF(V)  (sp++)->f = (V)
#if SIZEOF_VOID_P == 8
# define PUSHL(V)   (sp->l = (V), sp += 2)
# define PUSHD(V)   (sp->d = (V), sp += 2)
#else
# define PUSHL(V)  do { _Jv_word2 w2; w2.l=(V); \
                        (sp++)->ia[0] = w2.ia[0]; \
                        (sp++)->ia[0] = w2.ia[1]; } while (0)
# define PUSHD(V)  do { _Jv_word2 w2; w2.d=(V); \
                        (sp++)->ia[0] = w2.ia[0]; \
                        (sp++)->ia[0] = w2.ia[1]; } while (0)
#endif

#define POPA()    ((--sp)->o)
#define POPI()    ((jint) (--sp)->i) // cast since it may be promoted
#define POPF()    ((jfloat) (--sp)->f)
#if SIZEOF_VOID_P == 8
# define POPL()	  (sp -= 2, (jlong) sp->l)
# define POPD()	  (sp -= 2, (jdouble) sp->d)
#else
# define POPL()    ({ _Jv_word2 w2; \
                     w2.ia[1] = (--sp)->ia[0]; \
                     w2.ia[0] = (--sp)->ia[0]; w2.l; })
# define POPD()    ({ _Jv_word2 w2; \
                     w2.ia[1] = (--sp)->ia[0]; \
                     w2.ia[0] = (--sp)->ia[0]; w2.d; })
#endif

#define LOADA(I)  (sp++)->o = locals[I].o
#define LOADI(I)  (sp++)->i = locals[I].i
#define LOADF(I)  (sp++)->f = locals[I].f
#if SIZEOF_VOID_P == 8
# define LOADL(I)  (sp->l = locals[I].l, sp += 2)
# define LOADD(I)  (sp->d = locals[I].d, sp += 2)
#else
# define LOADL(I)  do { jint __idx = (I); \
    			(sp++)->ia[0] = locals[__idx].ia[0]; \
    			(sp++)->ia[0] = locals[__idx+1].ia[0]; \
 		   } while (0)
# define LOADD(I)  LOADL(I)
#endif

#define STOREA(I)			\
  do					\
    {					\
      jint __idx = (I);			\
      DEBUG_LOCALS_INSN (__idx, 'o');	\
      locals[__idx].o = (--sp)->o;	\
    }					\
  while (0)
#define STOREI(I)		       	\
  do					\
    {					\
      jint __idx = (I);			\
      DEBUG_LOCALS_INSN (__idx, 'i');	\
      locals[__idx].i = (--sp)->i;	\
  } while (0)
#define STOREF(I)			\
  do					\
    {					\
      jint __idx = (I);			\
      DEBUG_LOCALS_INSN (__idx, 'f');	\
      locals[__idx].f = (--sp)->f;	\
    }					\
  while (0)
#if SIZEOF_VOID_P == 8
# define STOREL(I) \
  do						\
    {						\
      jint __idx = (I);				\
      DEBUG_LOCALS_INSN (__idx, 'l');		\
      DEBUG_LOCALS_INSN (__idx + 1, 'x');	\
      (sp -= 2, locals[__idx].l = sp->l);	\
    }						\
  while (0)
# define STORED(I)				\
  do						\
    {						\
      jint __idx = (I);				\
      DEBUG_LOCALS_INSN (__idx, 'd');		\
      DEBUG_LOCALS_INSN (__idx + 1, 'x');	\
      (sp -= 2, locals[__idx].d = sp->d);	\
    }						\
  while (0)

#else
# define STOREL(I)				\
  do						\
    {						\
      jint __idx = (I);				\
      DEBUG_LOCALS_INSN (__idx, 'l');		\
      DEBUG_LOCALS_INSN (__idx + 1, 'x');	\
      locals[__idx + 1].ia[0] = (--sp)->ia[0];	\
      locals[__idx].ia[0] = (--sp)->ia[0];	\
    }						\
  while (0)
# define STORED(I)				\
  do {						\
    jint __idx = (I);				\
    DEBUG_LOCALS_INSN (__idx, 'd');		\
    DEBUG_LOCALS_INSN (__idx + 1, 'x');		\
    locals[__idx + 1].ia[0] = (--sp)->ia[0];	\
    locals[__idx].ia[0] = (--sp)->ia[0];	\
  } while (0)
#endif

#define PEEKI(I)  (locals+(I))->i
#define PEEKA(I)  (locals+(I))->o

#define POKEI(I,V)			\
  do					\
    {					\
      jint __idx = (I);			\
      DEBUG_LOCALS_INSN (__idx, 'i');	\
      ((locals + __idx)->i = (V));	\
    }					\
  while (0)


#define BINOPI(OP) { \
   jint value2 = POPI(); \
   jint value1 = POPI(); \
   PUSHI(value1 OP value2); \
}

#define BINOPF(OP) { \
   jfloat value2 = POPF(); \
   jfloat value1 = POPF(); \
   PUSHF(value1 OP value2); \
}

#define BINOPL(OP) { \
   jlong value2 = POPL(); \
   jlong value1 = POPL(); \
   PUSHL(value1 OP value2); \
}

#define BINOPD(OP) { \
   jdouble value2 = POPD(); \
   jdouble value1 = POPD(); \
   PUSHD(value1 OP value2); \
}

static inline jint
get1s (unsigned char* loc)
{
  return *(signed char*)loc;
}

static inline jint
get1u (unsigned char* loc)
{
  return *loc;
}

static inline jint
get2s(unsigned char* loc)
{
  return (((jint)*(signed char*)loc) << 8) | ((jint)*(loc+1));
}

static inline jint
get2u (unsigned char* loc)
{
  return (((jint)(*loc)) << 8) | ((jint)*(loc+1));
}

static jint
get4 (unsigned char* loc)
{
  return (((jint)(loc[0])) << 24) 
       | (((jint)(loc[1])) << 16) 
       | (((jint)(loc[2])) << 8) 
       | (((jint)(loc[3])) << 0);
}

#define SAVE_PC() frame_desc.pc = pc

// We used to define this conditionally, depending on HANDLE_SEGV.
// However, that runs into a problem if a chunk in low memory is
// mapped and we try to look at a field near the end of a large
// object.  See PR 26858 for details.  It is, most likely, relatively
// inexpensive to simply do this check always.
#define NULLCHECK(X) \
  do { SAVE_PC(); if ((X)==NULL) throw_null_pointer_exception (); } while (0)

// Note that we can still conditionally define NULLARRAYCHECK, since
// we know that all uses of an array will first reference the length
// field, which is first -- and thus will trigger a SEGV.
#ifdef HANDLE_SEGV
#define NULLARRAYCHECK(X) SAVE_PC()
#else
#define NULLARRAYCHECK(X)					\
  do								\
    {								\
      SAVE_PC();						\
      if ((X) == NULL) { throw_null_pointer_exception (); }	\
    } while (0)
#endif

#define ARRAYBOUNDSCHECK(array, index)				\
  do								\
    {								\
      if (((unsigned) index) >= (unsigned) (array->length))	\
	_Jv_ThrowBadArrayIndex (index);				\
    } while (0)

void
_Jv_InterpMethod::run_normal (ffi_cif *,
			      void *ret,
			      INTERP_FFI_RAW_TYPE *args,
			      void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  run (ret, args, _this);
}

void
_Jv_InterpMethod::run_normal_debug (ffi_cif *,
				    void *ret,
				    INTERP_FFI_RAW_TYPE *args,
				    void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  run_debug (ret, args, _this);
}

void
_Jv_InterpMethod::run_synch_object (ffi_cif *,
				    void *ret,
				    INTERP_FFI_RAW_TYPE *args,
				    void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jobject rcv = (jobject) args[0].ptr;
  JvSynchronize mutex (rcv);

  run (ret, args, _this);
}

void
_Jv_InterpMethod::run_synch_object_debug (ffi_cif *,
					  void *ret,
					  INTERP_FFI_RAW_TYPE *args,
					  void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jobject rcv = (jobject) args[0].ptr;
  JvSynchronize mutex (rcv);

  run_debug (ret, args, _this);
}

void
_Jv_InterpMethod::run_class (ffi_cif *,
			     void *ret,
			     INTERP_FFI_RAW_TYPE *args,
			     void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  _Jv_InitClass (_this->defining_class);
  run (ret, args, _this);
}

void
_Jv_InterpMethod::run_class_debug (ffi_cif *,
				   void *ret,
				   INTERP_FFI_RAW_TYPE *args,
				   void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  _Jv_InitClass (_this->defining_class);
  run_debug (ret, args, _this);
}

void
_Jv_InterpMethod::run_synch_class (ffi_cif *,
				   void *ret,
				   INTERP_FFI_RAW_TYPE *args,
				   void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jclass sync = _this->defining_class;
  _Jv_InitClass (sync);
  JvSynchronize mutex (sync);

  run (ret, args, _this);
}

void
_Jv_InterpMethod::run_synch_class_debug (ffi_cif *,
					 void *ret,
					 INTERP_FFI_RAW_TYPE *args,
					 void *__this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jclass sync = _this->defining_class;
  _Jv_InitClass (sync);
  JvSynchronize mutex (sync);

  run_debug (ret, args, _this);
}

#ifdef DIRECT_THREADED
// "Compile" a method by turning it from bytecode to direct-threaded
// code.
void
_Jv_InterpMethod::compile (const void * const *insn_targets)
{
  insn_slot *insns = NULL;
  int next = 0;
  unsigned char *codestart = bytecode ();
  unsigned char *end = codestart + code_length;
  _Jv_word *pool_data = defining_class->constants.data;

#define SET_ONE(Field, Value)						      \
  do									      \
    {									      \
      if (first_pass)							      \
	++next;								      \
      else								      \
	insns[next++].Field = Value;					      \
    }									      \
  while (0)

#define SET_INSN(Value) SET_ONE (insn, (void *) Value)
#define SET_INT(Value) SET_ONE (int_val, Value)
#define SET_DATUM(Value) SET_ONE (datum, Value)

  // Map from bytecode PC to slot in INSNS.
  int *pc_mapping = (int *) __builtin_alloca (sizeof (int) * code_length);
  for (int i = 0; i < code_length; ++i)
    pc_mapping[i] = -1;

  for (int i = 0; i < 2; ++i)
    {
      jboolean first_pass = i == 0;

      if (! first_pass)
	{
	  insns = (insn_slot *) _Jv_AllocBytes (sizeof (insn_slot) * next);
	  number_insn_slots = next;
	  next = 0;
	}

      unsigned char *pc = codestart;
      while (pc < end)
	{
	  int base_pc_val = pc - codestart;
	  if (first_pass)
	    pc_mapping[base_pc_val] = next;

	  java_opcode opcode = (java_opcode) *pc++;
	  // Just elide NOPs.
	  if (opcode == op_nop)
	    continue;
	  SET_INSN (insn_targets[opcode]);

	  switch (opcode)
	    {
	    case op_nop:
	    case op_aconst_null:
	    case op_iconst_m1:
	    case op_iconst_0:
	    case op_iconst_1:
	    case op_iconst_2:
	    case op_iconst_3:
	    case op_iconst_4:
	    case op_iconst_5:
	    case op_lconst_0:
	    case op_lconst_1:
	    case op_fconst_0:
	    case op_fconst_1:
	    case op_fconst_2:
	    case op_dconst_0:
	    case op_dconst_1:
	    case op_iload_0:
	    case op_iload_1:
	    case op_iload_2:
	    case op_iload_3:
	    case op_lload_0:
	    case op_lload_1:
	    case op_lload_2:
	    case op_lload_3:
	    case op_fload_0:
	    case op_fload_1:
	    case op_fload_2:
	    case op_fload_3:
	    case op_dload_0:
	    case op_dload_1:
	    case op_dload_2:
	    case op_dload_3:
	    case op_aload_0:
	    case op_aload_1:
	    case op_aload_2:
	    case op_aload_3:
	    case op_iaload:
	    case op_laload:
	    case op_faload:
	    case op_daload:
	    case op_aaload:
	    case op_baload:
	    case op_caload:
	    case op_saload:
	    case op_istore_0:
	    case op_istore_1:
	    case op_istore_2:
	    case op_istore_3:
	    case op_lstore_0:
	    case op_lstore_1:
	    case op_lstore_2:
	    case op_lstore_3:
	    case op_fstore_0:
	    case op_fstore_1:
	    case op_fstore_2:
	    case op_fstore_3:
	    case op_dstore_0:
	    case op_dstore_1:
	    case op_dstore_2:
	    case op_dstore_3:
	    case op_astore_0:
	    case op_astore_1:
	    case op_astore_2:
	    case op_astore_3:
	    case op_iastore:
	    case op_lastore:
	    case op_fastore:
	    case op_dastore:
	    case op_aastore:
	    case op_bastore:
	    case op_castore:
	    case op_sastore:
	    case op_pop:
	    case op_pop2:
	    case op_dup:
	    case op_dup_x1:
	    case op_dup_x2:
	    case op_dup2:
	    case op_dup2_x1:
	    case op_dup2_x2:
	    case op_swap:
	    case op_iadd:
	    case op_isub:
	    case op_imul:
	    case op_idiv:
	    case op_irem:
	    case op_ishl:
	    case op_ishr:
	    case op_iushr:
	    case op_iand:
	    case op_ior:
	    case op_ixor:
	    case op_ladd:
	    case op_lsub:
	    case op_lmul:
	    case op_ldiv:
	    case op_lrem:
	    case op_lshl:
	    case op_lshr:
	    case op_lushr:
	    case op_land:
	    case op_lor:
	    case op_lxor:
	    case op_fadd:
	    case op_fsub:
	    case op_fmul:
	    case op_fdiv:
	    case op_frem:
	    case op_dadd:
	    case op_dsub:
	    case op_dmul:
	    case op_ddiv:
	    case op_drem:
	    case op_ineg:
	    case op_i2b:
	    case op_i2c:
	    case op_i2s:
	    case op_lneg:
	    case op_fneg:
	    case op_dneg:
	    case op_i2l:
	    case op_i2f:
	    case op_i2d:
	    case op_l2i:
	    case op_l2f:
	    case op_l2d:
	    case op_f2i:
	    case op_f2l:
	    case op_f2d:
	    case op_d2i:
	    case op_d2l:
	    case op_d2f:
	    case op_lcmp:
	    case op_fcmpl:
	    case op_fcmpg:
	    case op_dcmpl:
	    case op_dcmpg:
	    case op_monitorenter:
	    case op_monitorexit:
	    case op_ireturn:
	    case op_lreturn:
	    case op_freturn:
	    case op_dreturn:
	    case op_areturn:
	    case op_return:
	    case op_athrow:
	    case op_arraylength:
	      // No argument, nothing else to do.
	      break;

	    case op_bipush:
	      SET_INT (get1s (pc));
	      ++pc;
	      break;

	    case op_ldc:
	      {
		int index = get1u (pc);
		++pc;
		// For an unresolved class we want to delay resolution
		// until execution.
		if (defining_class->constants.tags[index] == JV_CONSTANT_Class)
		  {
		    --next;
		    SET_INSN (insn_targets[int (op_jsr_w) + 1]);
		    SET_INT (index);
		  }
		else
		  SET_DATUM (pool_data[index].o);
	      }
	      break;

	    case op_ret:
	    case op_iload:
	    case op_lload:
	    case op_fload:
	    case op_dload:
	    case op_aload:
	    case op_istore:
	    case op_lstore:
	    case op_fstore:
	    case op_dstore:
	    case op_astore:
	    case op_newarray:
	      SET_INT (get1u (pc));
	      ++pc;
	      break;

	    case op_iinc:
	      SET_INT (get1u (pc));
	      SET_INT (get1s (pc + 1));
	      pc += 2;
	      break;

	    case op_ldc_w:
	      {
		int index = get2u (pc);
		pc += 2;
		// For an unresolved class we want to delay resolution
		// until execution.
		if (defining_class->constants.tags[index] == JV_CONSTANT_Class)
		  {
		    --next;
		    SET_INSN (insn_targets[int (op_jsr_w) + 1]);
		    SET_INT (index);
		  }
		else
		  SET_DATUM (pool_data[index].o);
	      }
	      break;

	    case op_ldc2_w:
	      {
		int index = get2u (pc);
		pc += 2;
		SET_DATUM (&pool_data[index]);
	      }
	      break;

	    case op_sipush:
	      SET_INT (get2s (pc));
	      pc += 2;
	      break;

	    case op_new:
	    case op_getstatic:
	    case op_getfield:
	    case op_putfield:
	    case op_putstatic:
	    case op_anewarray:
	    case op_instanceof:
	    case op_checkcast:
	    case op_invokespecial:
	    case op_invokestatic:
	    case op_invokevirtual:
	      SET_INT (get2u (pc));
	      pc += 2;
	      break;

	    case op_multianewarray:
	      SET_INT (get2u (pc));
	      SET_INT (get1u (pc + 2));
	      pc += 3;
	      break;

	    case op_jsr:
	    case op_ifeq:
	    case op_ifne:
	    case op_iflt:
	    case op_ifge:
	    case op_ifgt:
	    case op_ifle:
	    case op_if_icmpeq:
	    case op_if_icmpne:
	    case op_if_icmplt:
	    case op_if_icmpge:
	    case op_if_icmpgt:
	    case op_if_icmple:
	    case op_if_acmpeq:
	    case op_if_acmpne:
	    case op_ifnull:
	    case op_ifnonnull:
	    case op_goto:
	      {
		int offset = get2s (pc);
		pc += 2;

		int new_pc = base_pc_val + offset;

		bool orig_was_goto = opcode == op_goto;

		// Thread jumps.  We limit the loop count; this lets
		// us avoid infinite loops if the bytecode contains
		// such.  `10' is arbitrary.
		int count = 10;
		while (codestart[new_pc] == op_goto && count-- > 0)
		  new_pc += get2s (&codestart[new_pc + 1]);

		// If the jump takes us to a `return' instruction and
		// the original branch was an unconditional goto, then
		// we hoist the return.
		opcode = (java_opcode) codestart[new_pc];
		if (orig_was_goto
		    && (opcode == op_ireturn || opcode == op_lreturn
			|| opcode == op_freturn || opcode == op_dreturn
			|| opcode == op_areturn || opcode == op_return))
		  {
		    --next;
		    SET_INSN (insn_targets[opcode]);
		  }
		else
		  SET_DATUM (&insns[pc_mapping[new_pc]]);
	      }
	      break;

	    case op_tableswitch:
	      {
		while ((pc - codestart) % 4 != 0)
		  ++pc;

		jint def = get4 (pc);
		SET_DATUM (&insns[pc_mapping[base_pc_val + def]]);
		pc += 4;

		int low = get4 (pc);
		SET_INT (low);
		pc += 4;
		int high = get4 (pc);
		SET_INT (high);
		pc += 4;

		for (int i = low; i <= high; ++i)
		  {
		    SET_DATUM (&insns[pc_mapping[base_pc_val + get4 (pc)]]);
		    pc += 4;
		  }
	      }
	      break;

	    case op_lookupswitch:
	      {
		while ((pc - codestart) % 4 != 0)
		  ++pc;

		jint def = get4 (pc);
		SET_DATUM (&insns[pc_mapping[base_pc_val + def]]);
		pc += 4;

		jint npairs = get4 (pc);
		pc += 4;
		SET_INT (npairs);

		while (npairs-- > 0)
		  {
		    jint match = get4 (pc);
		    jint offset = get4 (pc + 4);
		    SET_INT (match);
		    SET_DATUM (&insns[pc_mapping[base_pc_val + offset]]);
		    pc += 8;
		  }
	      }
	      break;

	    case op_invokeinterface:
	      {
		jint index = get2u (pc);
		pc += 2;
		// We ignore the next two bytes.
		pc += 2;
		SET_INT (index);
	      }
	      break;

	    case op_wide:
	      {
		opcode = (java_opcode) get1u (pc);
		pc += 1;
		jint val = get2u (pc);
		pc += 2;

		// We implement narrow and wide instructions using the
		// same code in the interpreter.  So we rewrite the
		// instruction slot here.
		if (! first_pass)
		  insns[next - 1].insn = (void *) insn_targets[opcode];
		SET_INT (val);

		if (opcode == op_iinc)
		  {
		    SET_INT (get2s (pc));
		    pc += 2;
		  }
	      }
	      break;

	    case op_jsr_w:
	    case op_goto_w:
	      {
		jint offset = get4 (pc);
		pc += 4;
		SET_DATUM (&insns[pc_mapping[base_pc_val + offset]]);
	      }
	      break;

	    // Some "can't happen" cases that we include for
	    // error-checking purposes.
	    case op_putfield_1:
	    case op_putfield_2:
	    case op_putfield_4:
	    case op_putfield_8:
	    case op_putfield_a:
	    case op_putstatic_1:
	    case op_putstatic_2:
	    case op_putstatic_4:
	    case op_putstatic_8:
	    case op_putstatic_a:
	    case op_getfield_1:
	    case op_getfield_2s:
	    case op_getfield_2u:
	    case op_getfield_4:
	    case op_getfield_8:
	    case op_getfield_a:
	    case op_getstatic_1:
	    case op_getstatic_2s:
	    case op_getstatic_2u:
	    case op_getstatic_4:
	    case op_getstatic_8:
	    case op_getstatic_a:
	    case op_breakpoint:
	    default:
	      // Fail somehow.
	      break;
	    }
	}
    }

  // Now update exceptions.
  _Jv_InterpException *exc = exceptions ();
  for (int i = 0; i < exc_count; ++i)
    {
      exc[i].start_pc.p = &insns[pc_mapping[exc[i].start_pc.i]];
      exc[i].end_pc.p = &insns[pc_mapping[exc[i].end_pc.i]];
      exc[i].handler_pc.p = &insns[pc_mapping[exc[i].handler_pc.i]];
      // FIXME: resolve_pool_entry can throw - we shouldn't be doing this
      // during compilation.
      jclass handler
	= (_Jv_Linker::resolve_pool_entry (defining_class,
					     exc[i].handler_type.i)).clazz;
      exc[i].handler_type.p = handler;
    }

  // Translate entries in the LineNumberTable from bytecode PC's to direct
  // threaded interpreter instruction values.
  for (int i = 0; i < line_table_len; i++)
    {
      int byte_pc = line_table[i].bytecode_pc;
      // It isn't worth throwing an exception if this table is
      // corrupted, but at the same time we don't want a crash.
      if (byte_pc < 0 || byte_pc >= code_length)
	byte_pc = 0;
      line_table[i].pc = &insns[pc_mapping[byte_pc]];
    }  

  prepared = insns;

  // Now remap the variable table for this method.
  for (int i = 0; i < local_var_table_len; ++i)
    {
      int start_byte = local_var_table[i].bytecode_pc;
      if (start_byte < 0 || start_byte >= code_length)
	start_byte = 0;
      jlocation start =  pc_mapping[start_byte];

      int end_byte = start_byte + local_var_table[i].length;
      if (end_byte < 0)
	end_byte = 0;
      jlocation end = ((end_byte >= code_length)
		       ? number_insn_slots
		       : pc_mapping[end_byte]);

      local_var_table[i].pc = &insns[start];
      local_var_table[i].length = end - start + 1;
    }
  
  if (breakpoint_insn == NULL)
    {
      bp_insn_slot.insn = const_cast<void *> (insn_targets[op_breakpoint]);
      breakpoint_insn = &bp_insn_slot;
    }
}
#endif /* DIRECT_THREADED */

/* Run the given method.
   When args is NULL, don't run anything -- just compile it. */
void
_Jv_InterpMethod::run (void *retp, INTERP_FFI_RAW_TYPE *args,
		       _Jv_InterpMethod *meth)
{
#undef DEBUG
#undef DEBUG_LOCALS_INSN
#define DEBUG_LOCALS_INSN(s, t) do {} while (0)

#include "interpret-run.cc"
}

void
_Jv_InterpMethod::run_debug (void *retp, INTERP_FFI_RAW_TYPE *args,
			     _Jv_InterpMethod *meth)
{
#define DEBUG
#undef DEBUG_LOCALS_INSN
#define DEBUG_LOCALS_INSN(s, t)  \
  do    \
    {   \
      frame_desc.locals_type[s] = t;  \
    }   \
  while (0)

#include "interpret-run.cc"
}

static void
throw_internal_error (const char *msg)
{
  jthrowable t = new java::lang::InternalError (JvNewStringLatin1 (msg));
  REPORT_EXCEPTION (t);
  throw t;
}

static void 
throw_incompatible_class_change_error (jstring msg)
{
  jthrowable t = new java::lang::IncompatibleClassChangeError (msg);
  REPORT_EXCEPTION (t);
  throw t;
}

static void 
throw_null_pointer_exception ()
{
  jthrowable t = new java::lang::NullPointerException;
  REPORT_EXCEPTION (t);
  throw t;
}

/* Look up source code line number for given bytecode (or direct threaded
   interpreter) PC. */
int
_Jv_InterpMethod::get_source_line(pc_t mpc)
{
  int line = line_table_len > 0 ? line_table[0].line : -1;
  for (int i = 1; i < line_table_len; i++)
    if (line_table[i].pc > mpc)
      break;
    else
      line = line_table[i].line;

  return line;
}

/** Do static initialization for fields with a constant initializer */
void
_Jv_InitField (jobject obj, jclass klass, int index)
{
  using namespace java::lang::reflect;

  if (obj != 0 && klass == 0)
    klass = obj->getClass ();

  if (!_Jv_IsInterpretedClass (klass))
    return;

  _Jv_InterpClass *iclass = (_Jv_InterpClass*)klass->aux_info;

  _Jv_Field * field = (&klass->fields[0]) + index;

  if (index > klass->field_count)
    throw_internal_error ("field out of range");

  int init = iclass->field_initializers[index];
  if (init == 0)
    return;

  _Jv_Constants *pool = &klass->constants;
  int tag = pool->tags[init];

  if (! field->isResolved ())
    throw_internal_error ("initializing unresolved field");

  if (obj==0 && ((field->flags & Modifier::STATIC) == 0))
    throw_internal_error ("initializing non-static field with no object");

  void *addr = 0;

  if ((field->flags & Modifier::STATIC) != 0)
    addr = (void*) field->u.addr;
  else
    addr = (void*) (((char*)obj) + field->u.boffset);

  switch (tag)
    {
    case JV_CONSTANT_String:
      {
	jstring str;
	str = _Jv_NewStringUtf8Const (pool->data[init].utf8);
	pool->data[init].string = str;
	pool->tags[init] = JV_CONSTANT_ResolvedString;
      }
      /* fall through */

    case JV_CONSTANT_ResolvedString:
      if (! (field->type == &java::lang::String::class$
 	     || field->type == &java::lang::Class::class$))
	throw_class_format_error ("string initialiser to non-string field");

      *(jstring*)addr = pool->data[init].string;
      break;

    case JV_CONSTANT_Integer:
      {
	int value = pool->data[init].i;

	if (field->type == JvPrimClass (boolean))
	  *(jboolean*)addr = (jboolean)value;
	
	else if (field->type == JvPrimClass (byte))
	  *(jbyte*)addr = (jbyte)value;
	
	else if (field->type == JvPrimClass (char))
	  *(jchar*)addr = (jchar)value;

	else if (field->type == JvPrimClass (short))
	  *(jshort*)addr = (jshort)value;
	
	else if (field->type == JvPrimClass (int))
	  *(jint*)addr = (jint)value;

	else
	  throw_class_format_error ("erroneous field initializer");
      }  
      break;

    case JV_CONSTANT_Long:
      if (field->type != JvPrimClass (long))
	throw_class_format_error ("erroneous field initializer");

      *(jlong*)addr = _Jv_loadLong (&pool->data[init]);
      break;

    case JV_CONSTANT_Float:
      if (field->type != JvPrimClass (float))
	throw_class_format_error ("erroneous field initializer");

      *(jfloat*)addr = pool->data[init].f;
      break;

    case JV_CONSTANT_Double:
      if (field->type != JvPrimClass (double))
	throw_class_format_error ("erroneous field initializer");

      *(jdouble*)addr = _Jv_loadDouble (&pool->data[init]);
      break;

    default:
      throw_class_format_error ("erroneous field initializer");
    }
}

inline static unsigned char*
skip_one_type (unsigned char* ptr)
{
  int ch = *ptr++;

  while (ch == '[')
    { 
      ch = *ptr++;
    }
  
  if (ch == 'L')
    {
      do { ch = *ptr++; } while (ch != ';');
    }

  return ptr;
}

static ffi_type*
get_ffi_type_from_signature (unsigned char* ptr)
{
  switch (*ptr) 
    {
    case 'L':
    case '[':
      return &ffi_type_pointer;
      break;

    case 'Z':
      // On some platforms a bool is a byte, on others an int.
      if (sizeof (jboolean) == sizeof (jbyte))
	return &ffi_type_sint8;
      else
	{
	  JvAssert (sizeof (jbyte) == sizeof (jint));
	  return &ffi_type_sint32;
	}
      break;

    case 'B':
      return &ffi_type_sint8;
      break;
      
    case 'C':
      return &ffi_type_uint16;
      break;
	  
    case 'S': 
      return &ffi_type_sint16;
      break;
	  
    case 'I':
      return &ffi_type_sint32;
      break;
	  
    case 'J':
      return &ffi_type_sint64;
      break;
	  
    case 'F':
      return &ffi_type_float;
      break;
	  
    case 'D':
      return &ffi_type_double;
      break;

    case 'V':
      return &ffi_type_void;
      break;
    }

  throw_internal_error ("unknown type in signature");
}

/* this function yields the number of actual arguments, that is, if the
 * function is non-static, then one is added to the number of elements
 * found in the signature */

int 
_Jv_count_arguments (_Jv_Utf8Const *signature,
		     jboolean staticp)
{
  unsigned char *ptr = (unsigned char*) signature->chars();
  int arg_count = staticp ? 0 : 1;

  /* first, count number of arguments */

  // skip '('
  ptr++;

  // count args
  while (*ptr != ')')
    {
      ptr = skip_one_type (ptr);
      arg_count += 1;
    }

  return arg_count;
}

/* This beast will build a cif, given the signature.  Memory for
 * the cif itself and for the argument types must be allocated by the
 * caller.
 */

int 
_Jv_init_cif (_Jv_Utf8Const* signature,
	      int arg_count,
	      jboolean staticp,
	      ffi_cif *cif,
	      ffi_type **arg_types,
	      ffi_type **rtype_p)
{
  unsigned char *ptr = (unsigned char*) signature->chars();

  int arg_index = 0;		// arg number
  int item_count = 0;		// stack-item count

  // setup receiver
  if (!staticp)
    {
      arg_types[arg_index++] = &ffi_type_pointer;
      item_count += 1;
    }

  // skip '('
  ptr++;

  // assign arg types
  while (*ptr != ')')
    {
      arg_types[arg_index++] = get_ffi_type_from_signature (ptr);

      if (*ptr == 'J' || *ptr == 'D')
	item_count += 2;
      else
	item_count += 1;

      ptr = skip_one_type (ptr);
    }

  // skip ')'
  ptr++;
  ffi_type *rtype = get_ffi_type_from_signature (ptr);

  ptr = skip_one_type (ptr);
  if (ptr != (unsigned char*)signature->chars() + signature->len())
    throw_internal_error ("did not find end of signature");

  if (ffi_prep_cif (cif, FFI_DEFAULT_ABI,
		    arg_count, rtype, arg_types) != FFI_OK)
    throw_internal_error ("ffi_prep_cif failed");

  if (rtype_p != NULL)
    *rtype_p = rtype;

  return item_count;
}

/* we put this one here, and not in interpret.cc because it
 * calls the utility routines _Jv_count_arguments 
 * which are static to this module.  The following struct defines the
 * layout we use for the stubs, it's only used in the ncode method. */

#if FFI_NATIVE_RAW_API
#   define FFI_PREP_RAW_CLOSURE ffi_prep_raw_closure_loc
#   define FFI_RAW_SIZE ffi_raw_size
typedef struct {
  ffi_raw_closure  closure;
  _Jv_ClosureList list;
  ffi_cif   cif;
  ffi_type *arg_types[0];
} ncode_closure;
typedef void (*ffi_closure_fun) (ffi_cif*,void*,INTERP_FFI_RAW_TYPE*,void*);
#else
#   define FFI_PREP_RAW_CLOSURE ffi_prep_java_raw_closure_loc
#   define FFI_RAW_SIZE ffi_java_raw_size
typedef struct {
  ffi_java_raw_closure  closure;
  _Jv_ClosureList list;
  ffi_cif   cif;
  ffi_type *arg_types[0];
} ncode_closure;
typedef void (*ffi_closure_fun) (ffi_cif*,void*,ffi_java_raw*,void*);
#endif

void *
_Jv_InterpMethod::ncode (jclass klass)
{
  using namespace java::lang::reflect;

  if (self->ncode != 0)
    return self->ncode;

  jboolean staticp = (self->accflags & Modifier::STATIC) != 0;
  int arg_count = _Jv_count_arguments (self->signature, staticp);

  void *code;
  ncode_closure *closure =
    (ncode_closure*)ffi_closure_alloc (sizeof (ncode_closure)
				       + arg_count * sizeof (ffi_type*),
				       &code);
  closure->list.registerClosure (klass, closure);

  _Jv_init_cif (self->signature,
		arg_count,
		staticp,
		&closure->cif,
		&closure->arg_types[0],
		NULL);

  ffi_closure_fun fun;

  args_raw_size = FFI_RAW_SIZE (&closure->cif);

  JvAssert ((self->accflags & Modifier::NATIVE) == 0);

  if ((self->accflags & Modifier::SYNCHRONIZED) != 0)
    {
      if (staticp)
        {
	  if (JVMTI::enabled)
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_class_debug;
	  else
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_class;
        }
      else
        {
	  if (JVMTI::enabled)
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_object_debug;
	  else
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_synch_object;
        }
    }
  else
    {
      if (staticp)
        {
	  if (JVMTI::enabled)
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_class_debug;
	  else
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_class;
        }
      else
        {
	  if (JVMTI::enabled)
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_normal_debug;
	  else
	    fun = (ffi_closure_fun)&_Jv_InterpMethod::run_normal;
        }
    }

  FFI_PREP_RAW_CLOSURE (&closure->closure,
		        &closure->cif, 
		        fun,
		        (void*)this,
			code);

  self->ncode = code;

  return self->ncode;
}

/* Find the index of the given insn in the array of insn slots
   for this method. Returns -1 if not found. */
jlong
_Jv_InterpMethod::insn_index (pc_t pc)
{
  jlong left = 0;
#ifdef DIRECT_THREADED
  jlong right = number_insn_slots;
  pc_t insns = prepared;
#else
  jlong right = code_length;
  pc_t insns = bytecode ();
#endif

  while (right >= 0)
    {
      jlong mid = (left + right) / 2;
      if (&insns[mid] == pc)
	return mid;

      if (pc < &insns[mid])
	right = mid - 1;
      else
        left = mid + 1;
    }

  return -1;
}

// Method to check if an exception is caught at some location in a method
// (meth).  Returns true if this method (meth) contains a catch block for the
// exception (ex). False otherwise.  If there is a catch block, it sets the pc
// to the location of the beginning of the catch block.
jboolean
_Jv_InterpMethod::check_handler (pc_t *pc, _Jv_InterpMethod *meth,
                                java::lang::Throwable *ex)
{
#ifdef DIRECT_THREADED
  void *logical_pc = (void *) ((insn_slot *) (*pc) - 1);
#else
  int logical_pc = (*pc) - 1 - meth->bytecode ();
#endif
  _Jv_InterpException *exc = meth->exceptions ();
  jclass exc_class = ex->getClass ();

  for (int i = 0; i < meth->exc_count; i++)
    {
      if (PCVAL (exc[i].start_pc) <= logical_pc
          && logical_pc < PCVAL (exc[i].end_pc))
        {
#ifdef DIRECT_THREADED
              jclass handler = (jclass) exc[i].handler_type.p;
#else
              jclass handler = NULL;
              if (exc[i].handler_type.i != 0)
                    handler
                      = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
                                                                             ex$
#endif /* DIRECT_THREADED */
              if (handler == NULL || handler->isAssignableFrom (exc_class))
                {
#ifdef DIRECT_THREADED
                  (*pc) = (insn_slot *) exc[i].handler_pc.p;
#else
                  (*pc) = meth->bytecode () + exc[i].handler_pc.i;
#endif /* DIRECT_THREADED */
                  return true;
                }
          }
      }
  return false;
}


void
_Jv_InterpMethod::get_line_table (jlong& start, jlong& end,
				  jintArray& line_numbers,
				  jlongArray& code_indices)
{
#ifdef DIRECT_THREADED
  /* For the DIRECT_THREADED case, if the method has not yet been
   * compiled, the linetable will change to insn slots instead of
   * bytecode PCs. It is probably easiest, in this case, to simply
   * compile the method and guarantee that we are using insn
   * slots.
   */
  _Jv_CompileMethod (this);

  if (line_table_len > 0)
    {
      start = 0;
      end = number_insn_slots;
      line_numbers = JvNewIntArray (line_table_len);
      code_indices = JvNewLongArray (line_table_len);

      jint* lines = elements (line_numbers);
      jlong* indices = elements (code_indices);
      for (int i = 0; i < line_table_len; ++i)
	{
	  lines[i] = line_table[i].line;
	  indices[i] = insn_index (line_table[i].pc);
	}
    }
#else // !DIRECT_THREADED
  if (line_table_len > 0)
    {
      start = 0;
      end = code_length;
      line_numbers = JvNewIntArray (line_table_len);
      code_indices = JvNewLongArray (line_table_len);

      jint* lines = elements (line_numbers);
      jlong* indices = elements (code_indices);
      for (int i = 0; i < line_table_len; ++i)
	{
	  lines[i] = line_table[i].line;
	  indices[i] = (jlong) line_table[i].bytecode_pc;
	}
    }
#endif // !DIRECT_THREADED
}

int 
_Jv_InterpMethod::get_local_var_table (char **name, char **sig, 
                                       char **generic_sig, jlong *startloc,
                                       jint *length, jint *slot, 
                                       int table_slot)
{
#ifdef DIRECT_THREADED
  _Jv_CompileMethod (this);
#endif

  if (local_var_table == NULL)
    return -2;
  if (table_slot >= local_var_table_len)
    return -1;
  else
    {
      *name = local_var_table[table_slot].name;
      *sig = local_var_table[table_slot].descriptor;
      *generic_sig = local_var_table[table_slot].descriptor;

#ifdef DIRECT_THREADED
      *startloc = insn_index (local_var_table[table_slot].pc);
#else
      *startloc = static_cast<jlong> (local_var_table[table_slot].bytecode_pc);
#endif
      *length = static_cast<jint> (local_var_table[table_slot].length);
      *slot = static_cast<jint> (local_var_table[table_slot].slot);
    }
  return local_var_table_len - table_slot - 1;
}

pc_t
_Jv_InterpMethod::install_break (jlong index)
{
  return set_insn (index, breakpoint_insn);
}

pc_t
_Jv_InterpMethod::get_insn (jlong index)
{
  pc_t code;

#ifdef DIRECT_THREADED
  if (index >= number_insn_slots || index < 0)
    return NULL;

  code = prepared;
#else // !DIRECT_THREADED
  if (index >= code_length || index < 0)
    return NULL;

  code = reinterpret_cast<pc_t> (bytecode ());
#endif // !DIRECT_THREADED

  return &code[index];
}

pc_t
_Jv_InterpMethod::set_insn (jlong index, pc_t insn)
{
#ifdef DIRECT_THREADED
  if (index >= number_insn_slots || index < 0)
    return NULL;

  pc_t code = prepared;
  code[index].insn = insn->insn;
#else // !DIRECT_THREADED
  if (index >= code_length || index < 0)
    return NULL;

  pc_t code = reinterpret_cast<pc_t> (bytecode ());
  code[index] = *insn;
#endif // !DIRECT_THREADED

  return &code[index];
}

bool
_Jv_InterpMethod::breakpoint_at (jlong index)
{
  pc_t insn = get_insn (index);
  if (insn != NULL)
    {
#ifdef DIRECT_THREADED
      return (insn->insn == breakpoint_insn->insn);
#else
      pc_t code = reinterpret_cast<pc_t> (bytecode ());
      return (code[index] == breakpoint_insn);
#endif
    }

  return false;
}

void *
_Jv_JNIMethod::ncode (jclass klass)
{
  using namespace java::lang::reflect;

  if (self->ncode != 0)
    return self->ncode;

  jboolean staticp = (self->accflags & Modifier::STATIC) != 0;
  int arg_count = _Jv_count_arguments (self->signature, staticp);

  void *code;
  ncode_closure *closure =
    (ncode_closure*)ffi_closure_alloc (sizeof (ncode_closure)
				       + arg_count * sizeof (ffi_type*),
				       &code);
  closure->list.registerClosure (klass, closure);

  ffi_type *rtype;
  _Jv_init_cif (self->signature,
		arg_count,
		staticp,
		&closure->cif,
		&closure->arg_types[0],
		&rtype);

  ffi_closure_fun fun;

  args_raw_size = FFI_RAW_SIZE (&closure->cif);

  // Initialize the argument types and CIF that represent the actual
  // underlying JNI function.
  int extra_args = 1;
  if ((self->accflags & Modifier::STATIC))
    ++extra_args;
  jni_arg_types = (ffi_type **) _Jv_AllocBytes ((extra_args + arg_count)
						* sizeof (ffi_type *));
  int offset = 0;
  jni_arg_types[offset++] = &ffi_type_pointer;
  if ((self->accflags & Modifier::STATIC))
    jni_arg_types[offset++] = &ffi_type_pointer;
  memcpy (&jni_arg_types[offset], &closure->arg_types[0],
	  arg_count * sizeof (ffi_type *));

  if (ffi_prep_cif (&jni_cif, _Jv_platform_ffi_abi,
		    extra_args + arg_count, rtype,
		    jni_arg_types) != FFI_OK)
    throw_internal_error ("ffi_prep_cif failed for JNI function");

  JvAssert ((self->accflags & Modifier::NATIVE) != 0);

  // FIXME: for now we assume that all native methods for
  // interpreted code use JNI.
  fun = (ffi_closure_fun) &_Jv_JNIMethod::call;

  FFI_PREP_RAW_CLOSURE (&closure->closure,
			&closure->cif, 
			fun,
			(void*) this,
			code);

  self->ncode = code;
  return self->ncode;
}

static void
throw_class_format_error (jstring msg)
{
  jthrowable t = (msg
	 ? new java::lang::ClassFormatError (msg)
	 : new java::lang::ClassFormatError);
  REPORT_EXCEPTION (t);
  throw t;
}

static void
throw_class_format_error (const char *msg)
{
  throw_class_format_error (JvNewStringLatin1 (msg));
}

/* This function finds the method and location where the exception EXC
   is caught in the stack frame. On return, it sets CATCH_METHOD and
   CATCH_LOCATION with the method and location where the catch will
   occur. If the exception is not caught, these are set to 0.

   This function should only be used with the DEBUG interpreter. */
static void
find_catch_location (::java::lang::Throwable *exc, jthread thread,
		     jmethodID *catch_method, jlong *catch_loc)
{
  *catch_method = 0;
  *catch_loc = 0;

  _Jv_InterpFrame *frame
    = reinterpret_cast<_Jv_InterpFrame *> (thread->interp_frame);
  while (frame != NULL)
    {
      pc_t pc = frame->get_pc ();
      _Jv_InterpMethod *imeth
	= reinterpret_cast<_Jv_InterpMethod *> (frame->self);
      if (imeth->check_handler (&pc, imeth, exc))
	{
	  // This method handles the exception.
	  *catch_method = imeth->get_method ();
	  *catch_loc = imeth->insn_index (pc);
	  return;
	}

      frame = frame->next_interp;
    }
}

/* This method handles JVMTI notifications of thrown exceptions. It
   calls find_catch_location to figure out where the exception is
   caught (if it is caught).
   
   Like find_catch_location, this should only be called with the
   DEBUG interpreter. Since a few exceptions occur outside the
   interpreter proper, it is important to not call this function
   without checking JVMTI_REQUESTED_EVENT(Exception) first. */
void
_Jv_ReportJVMTIExceptionThrow (jthrowable ex)
{
  jthread thread = ::java::lang::Thread::currentThread ();
  _Jv_Frame *frame = reinterpret_cast<_Jv_Frame *> (thread->frame);
  jmethodID throw_meth = frame->self->get_method ();
  jlocation throw_loc = -1;
  if (frame->frame_type == frame_interpreter)
    {
      _Jv_InterpFrame * iframe
	= reinterpret_cast<_Jv_InterpFrame *> (frame);
      _Jv_InterpMethod *imeth
	= reinterpret_cast<_Jv_InterpMethod *> (frame->self);
      throw_loc = imeth->insn_index (iframe->get_pc ());
    }

  jlong catch_loc;
  jmethodID catch_method;
  find_catch_location (ex, thread, &catch_method, &catch_loc);
  _Jv_JVMTI_PostEvent (JVMTI_EVENT_EXCEPTION, thread,
		       _Jv_GetCurrentJNIEnv (), throw_meth, throw_loc,
		       ex, catch_method, catch_loc);
}



void
_Jv_InterpreterEngine::do_verify (jclass klass)
{
  _Jv_InterpClass *iclass = (_Jv_InterpClass *) klass->aux_info;
  for (int i = 0; i < klass->method_count; i++)
    {
      using namespace java::lang::reflect;
      _Jv_MethodBase *imeth = iclass->interpreted_methods[i];
      _Jv_ushort accflags = klass->methods[i].accflags;
      if ((accflags & (Modifier::NATIVE | Modifier::ABSTRACT)) == 0)
	{
	  _Jv_InterpMethod *im = reinterpret_cast<_Jv_InterpMethod *> (imeth);
	  _Jv_VerifyMethod (im);
	}
    }
}

void
_Jv_InterpreterEngine::do_create_ncode (jclass klass)
{
  _Jv_InterpClass *iclass = (_Jv_InterpClass *) klass->aux_info;
  for (int i = 0; i < klass->method_count; i++)
    {
      // Just skip abstract methods.  This is particularly important
      // because we don't resize the interpreted_methods array when
      // miranda methods are added to it.
      if ((klass->methods[i].accflags
	   & java::lang::reflect::Modifier::ABSTRACT)
	  != 0)
	continue;

      _Jv_MethodBase *imeth = iclass->interpreted_methods[i];

      if ((klass->methods[i].accflags & java::lang::reflect::Modifier::NATIVE)
	  != 0)
	{
	  // You might think we could use a virtual `ncode' method in
	  // the _Jv_MethodBase and unify the native and non-native
	  // cases.  Well, we can't, because we don't allocate these
	  // objects using `new', and thus they don't get a vtable.
	  _Jv_JNIMethod *jnim = reinterpret_cast<_Jv_JNIMethod *> (imeth);
	  klass->methods[i].ncode = jnim->ncode (klass);
	}
      else if (imeth != 0)		// it could be abstract
	{
	  _Jv_InterpMethod *im = reinterpret_cast<_Jv_InterpMethod *> (imeth);
	  klass->methods[i].ncode = im->ncode (klass);
	}
    }
}

_Jv_ClosureList **
_Jv_InterpreterEngine::do_get_closure_list (jclass klass)
{
  _Jv_InterpClass *iclass = (_Jv_InterpClass *) klass->aux_info;

  if (!iclass->closures)
    iclass->closures = _Jv_ClosureListFinalizer ();

  return iclass->closures;
}

void
_Jv_InterpreterEngine::do_allocate_static_fields (jclass klass,
						  int pointer_size,
						  int other_size)
{
  _Jv_InterpClass *iclass = (_Jv_InterpClass *) klass->aux_info;

  // Splitting the allocations here lets us scan reference fields and
  // avoid scanning non-reference fields.  How reference fields are
  // scanned is a bit tricky: we allocate using _Jv_AllocRawObj, which
  // means that this memory will be scanned conservatively (same
  // difference, since we know all the contents here are pointers).
  // Then we put pointers into this memory into the 'fields'
  // structure.  Most of these are interior pointers, which is ok (but
  // even so the pointer to the first reference field will be used and
  // that is not an interior pointer).  The 'fields' array is also
  // allocated with _Jv_AllocRawObj (see defineclass.cc), so it will
  // be scanned.  A pointer to this array is held by Class and thus
  // seen by the collector.
  char *reference_fields = (char *) _Jv_AllocRawObj (pointer_size);
  char *non_reference_fields = (char *) _Jv_AllocBytes (other_size);

  for (int i = 0; i < klass->field_count; i++)
    {
      _Jv_Field *field = &klass->fields[i];

      if ((field->flags & java::lang::reflect::Modifier::STATIC) == 0)
	continue;

      char *base = field->isRef() ? reference_fields : non_reference_fields;
      field->u.addr  = base + field->u.boffset;

      if (iclass->field_initializers[i] != 0)
	{
	  _Jv_Linker::resolve_field (field, klass->loader);
	  _Jv_InitField (0, klass, i);
	}
    }

  // Now we don't need the field_initializers anymore, so let the
  // collector get rid of it.
  iclass->field_initializers = 0;
}

_Jv_ResolvedMethod *
_Jv_InterpreterEngine::do_resolve_method (_Jv_Method *method, jclass klass,
					  jboolean staticp)
{
  int arg_count = _Jv_count_arguments (method->signature, staticp);

  _Jv_ResolvedMethod* result = (_Jv_ResolvedMethod*)
    _Jv_AllocBytes (sizeof (_Jv_ResolvedMethod)
		    + arg_count*sizeof (ffi_type*));

  result->stack_item_count
    = _Jv_init_cif (method->signature,
		    arg_count,
		    staticp,
		    &result->cif,
		    &result->arg_types[0],
		    NULL);

  result->method              = method;
  result->klass               = klass;

  return result;
}

void
_Jv_InterpreterEngine::do_post_miranda_hook (jclass klass)
{
  _Jv_InterpClass *iclass = (_Jv_InterpClass *) klass->aux_info;
  for (int i = 0; i < klass->method_count; i++)
    {
      // Just skip abstract methods.  This is particularly important
      // because we don't resize the interpreted_methods array when
      // miranda methods are added to it.
      if ((klass->methods[i].accflags
	   & java::lang::reflect::Modifier::ABSTRACT)
	  != 0)
	continue;
      // Miranda method additions mean that the `methods' array moves.
      // We cache a pointer into this array, so we have to update.
      iclass->interpreted_methods[i]->self = &klass->methods[i];
    }
}

#ifdef DIRECT_THREADED
void
_Jv_CompileMethod (_Jv_InterpMethod* method)
{
  if (method->prepared == NULL)
    {
      if (JVMTI::enabled)
	_Jv_InterpMethod::run_debug (NULL, NULL, method);
      else
      _Jv_InterpMethod::run (NULL, NULL, method);
    }
}
#endif // DIRECT_THREADED

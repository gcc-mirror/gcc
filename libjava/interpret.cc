// interpret.cc - Code for the interpreter

/* Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

// Define this to get the direct-threaded interpreter.  If undefined,
// we revert to a basic bytecode interpreter.  The former is faster
// but uses more memory.
#define DIRECT_THREADED

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
#include <java/lang/ClassCastException.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/InternalError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArithmeticException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/Thread.h>
#include <java-insns.h>
#include <java-signal.h>

#ifdef INTERPRETER

#include <stdlib.h>

using namespace gcj;

static void throw_internal_error (char *msg)
  __attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
  __attribute__ ((__noreturn__));
#ifndef HANDLE_SEGV
static void throw_null_pointer_exception ()
  __attribute__ ((__noreturn__));
#endif

#ifdef DIRECT_THREADED
// Lock to ensure that methods are not compiled concurrently.
// We could use a finer-grained lock here, however it is not safe to use
// the Class monitor as user code in another thread could hold it.
static _Jv_Mutex_t compile_mutex;

void
_Jv_InitInterpreter()
{
  _Jv_MutexInit (&compile_mutex);
}
#else
void _Jv_InitInterpreter() {}
#endif

extern "C" double __ieee754_fmod (double,double);

// This represents a single slot in the "compiled" form of the
// bytecode.
union insn_slot
{
  // Address of code.
  void *insn;
  // An integer value used by an instruction.
  jint int_val;
  // A pointer value used by an instruction.
  void *datum;
};

// The type of the PC depends on whether we're doing direct threading
// or a more ordinary bytecode interpreter.
#ifdef DIRECT_THREADED
typedef insn_slot *pc_t;
#else
typedef unsigned char *pc_t;
#endif

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

#define STOREA(I) locals[I].o = (--sp)->o
#define STOREI(I) locals[I].i = (--sp)->i
#define STOREF(I) locals[I].f = (--sp)->f
#if SIZEOF_VOID_P == 8
# define STOREL(I) (sp -= 2, locals[I].l = sp->l)
# define STORED(I) (sp -= 2, locals[I].d = sp->d)
#else
# define STOREL(I) do { jint __idx = (I); \
    		       locals[__idx+1].ia[0] = (--sp)->ia[0]; \
    		       locals[__idx].ia[0] = (--sp)->ia[0]; \
		   } while (0)
# define STORED(I) STOREL(I)
#endif

#define PEEKI(I)  (locals+(I))->i
#define PEEKA(I)  (locals+(I))->o

#define POKEI(I,V)  ((locals+(I))->i = (V))


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

static inline jint get1s(unsigned char* loc) {
  return *(signed char*)loc;
}

static inline jint get1u(unsigned char* loc) {
  return *loc;
}

static inline jint get2s(unsigned char* loc) {
  return (((jint)*(signed char*)loc) << 8) | ((jint)*(loc+1));
}

static inline jint get2u(unsigned char* loc) {
  return (((jint)(*loc)) << 8) | ((jint)*(loc+1));
}

static jint get4(unsigned char* loc) {
  return (((jint)(loc[0])) << 24) 
       | (((jint)(loc[1])) << 16) 
       | (((jint)(loc[2])) << 8) 
       | (((jint)(loc[3])) << 0);
}


#ifdef HANDLE_SEGV
#define NULLCHECK(X) 
#define NULLARRAYCHECK(X)
#else
#define NULLCHECK(X) \
  do { if ((X)==NULL) throw_null_pointer_exception (); } while (0)
#define NULLARRAYCHECK(X) \
  do { if ((X)==NULL) { throw_null_pointer_exception (); } } while (0)
#endif

#define ARRAYBOUNDSCHECK(array, index)					      \
  do									      \
    {									      \
      if (((unsigned) index) >= (unsigned) (array->length))		      \
	_Jv_ThrowBadArrayIndex (index);					      \
    }									      \
  while (0)

void
_Jv_InterpMethod::run_normal (ffi_cif *,
			      void* ret,
			      ffi_raw * args,
			      void* __this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  _this->run (ret, args);
}

void
_Jv_InterpMethod::run_synch_object (ffi_cif *,
				    void* ret,
				    ffi_raw * args,
				    void* __this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jobject rcv = (jobject) args[0].ptr;
  JvSynchronize mutex (rcv);

  _this->run (ret, args);
}

void
_Jv_InterpMethod::run_class (ffi_cif *,
			     void* ret,
			     ffi_raw * args,
			     void* __this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;
  _Jv_InitClass (_this->defining_class);
  _this->run (ret, args);
}

void
_Jv_InterpMethod::run_synch_class (ffi_cif *,
				   void* ret,
				   ffi_raw * args,
				   void* __this)
{
  _Jv_InterpMethod *_this = (_Jv_InterpMethod *) __this;

  jclass sync = _this->defining_class;
  _Jv_InitClass (sync);
  JvSynchronize mutex (sync);

  _this->run (ret, args);
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
      jclass handler = (_Jv_ResolvePoolEntry (defining_class,
					      exc[i].handler_type.i)).clazz;
      exc[i].handler_type.p = handler;
    }

  prepared = insns;
}
#endif /* DIRECT_THREADED */

// This function exists so that the stack-tracing code can find the
// boundaries of the interpreter.
void
_Jv_StartOfInterpreter (void)
{
}

void
_Jv_InterpMethod::run (void *retp, ffi_raw *args)
{
  using namespace java::lang::reflect;

  // FRAME_DESC registers this particular invocation as the top-most
  // interpreter frame.  This lets the stack tracing code (for
  // Throwable) print information about the method being interpreted
  // rather than about the interpreter itself.  FRAME_DESC has a
  // destructor so it cleans up automatically when the interpreter
  // returns.
  java::lang::Thread *thread = java::lang::Thread::currentThread();
  _Jv_MethodChain frame_desc (this,
			      (_Jv_MethodChain **) &thread->interp_frame);

  _Jv_word stack[max_stack];
  _Jv_word *sp = stack;

  _Jv_word locals[max_locals];

  /* Go straight at it!  the ffi raw format matches the internal
     stack representation exactly.  At least, that's the idea.
  */
  memcpy ((void*) locals, (void*) args, args_raw_size);

  _Jv_word *pool_data = defining_class->constants.data;

  /* These three are temporaries for common code used by several
     instructions.  */
  void (*fun)();
  _Jv_ResolvedMethod* rmeth;
  int tmpval;

#define INSN_LABEL(op) &&insn_##op

  static const void *const insn_target[] = 
  {
    INSN_LABEL(nop),
    INSN_LABEL(aconst_null),
    INSN_LABEL(iconst_m1),
    INSN_LABEL(iconst_0),
    INSN_LABEL(iconst_1),
    INSN_LABEL(iconst_2),
    INSN_LABEL(iconst_3),
    INSN_LABEL(iconst_4),
    INSN_LABEL(iconst_5),
    INSN_LABEL(lconst_0),
    INSN_LABEL(lconst_1),
    INSN_LABEL(fconst_0),
    INSN_LABEL(fconst_1),
    INSN_LABEL(fconst_2),
    INSN_LABEL(dconst_0),
    INSN_LABEL(dconst_1),
    INSN_LABEL(bipush),
    INSN_LABEL(sipush),
    INSN_LABEL(ldc),
    INSN_LABEL(ldc_w),
    INSN_LABEL(ldc2_w),
    INSN_LABEL(iload),
    INSN_LABEL(lload),
    INSN_LABEL(fload),
    INSN_LABEL(dload),
    INSN_LABEL(aload),
    INSN_LABEL(iload_0),
    INSN_LABEL(iload_1),
    INSN_LABEL(iload_2),
    INSN_LABEL(iload_3),
    INSN_LABEL(lload_0),
    INSN_LABEL(lload_1),
    INSN_LABEL(lload_2),
    INSN_LABEL(lload_3),
    INSN_LABEL(fload_0),
    INSN_LABEL(fload_1),
    INSN_LABEL(fload_2),
    INSN_LABEL(fload_3),
    INSN_LABEL(dload_0),
    INSN_LABEL(dload_1),
    INSN_LABEL(dload_2),
    INSN_LABEL(dload_3),
    INSN_LABEL(aload_0),
    INSN_LABEL(aload_1),
    INSN_LABEL(aload_2),
    INSN_LABEL(aload_3),
    INSN_LABEL(iaload),
    INSN_LABEL(laload),
    INSN_LABEL(faload),
    INSN_LABEL(daload),
    INSN_LABEL(aaload),
    INSN_LABEL(baload),
    INSN_LABEL(caload),
    INSN_LABEL(saload),
    INSN_LABEL(istore),
    INSN_LABEL(lstore),
    INSN_LABEL(fstore),
    INSN_LABEL(dstore),
    INSN_LABEL(astore),
    INSN_LABEL(istore_0),
    INSN_LABEL(istore_1),
    INSN_LABEL(istore_2),
    INSN_LABEL(istore_3),
    INSN_LABEL(lstore_0),
    INSN_LABEL(lstore_1),
    INSN_LABEL(lstore_2),
    INSN_LABEL(lstore_3),
    INSN_LABEL(fstore_0),
    INSN_LABEL(fstore_1),
    INSN_LABEL(fstore_2),
    INSN_LABEL(fstore_3),
    INSN_LABEL(dstore_0),
    INSN_LABEL(dstore_1),
    INSN_LABEL(dstore_2),
    INSN_LABEL(dstore_3),
    INSN_LABEL(astore_0),
    INSN_LABEL(astore_1),
    INSN_LABEL(astore_2),
    INSN_LABEL(astore_3),
    INSN_LABEL(iastore),
    INSN_LABEL(lastore),
    INSN_LABEL(fastore),
    INSN_LABEL(dastore),
    INSN_LABEL(aastore),
    INSN_LABEL(bastore),
    INSN_LABEL(castore),
    INSN_LABEL(sastore),
    INSN_LABEL(pop),
    INSN_LABEL(pop2),
    INSN_LABEL(dup),
    INSN_LABEL(dup_x1),
    INSN_LABEL(dup_x2),
    INSN_LABEL(dup2),
    INSN_LABEL(dup2_x1),
    INSN_LABEL(dup2_x2),
    INSN_LABEL(swap),
    INSN_LABEL(iadd),
    INSN_LABEL(ladd),
    INSN_LABEL(fadd),
    INSN_LABEL(dadd),
    INSN_LABEL(isub),
    INSN_LABEL(lsub),
    INSN_LABEL(fsub),
    INSN_LABEL(dsub),
    INSN_LABEL(imul),
    INSN_LABEL(lmul),
    INSN_LABEL(fmul),
    INSN_LABEL(dmul),
    INSN_LABEL(idiv),
    INSN_LABEL(ldiv),
    INSN_LABEL(fdiv),
    INSN_LABEL(ddiv),
    INSN_LABEL(irem),
    INSN_LABEL(lrem),
    INSN_LABEL(frem),
    INSN_LABEL(drem),
    INSN_LABEL(ineg),
    INSN_LABEL(lneg),
    INSN_LABEL(fneg),
    INSN_LABEL(dneg),
    INSN_LABEL(ishl),
    INSN_LABEL(lshl),
    INSN_LABEL(ishr),
    INSN_LABEL(lshr),
    INSN_LABEL(iushr),
    INSN_LABEL(lushr),
    INSN_LABEL(iand),
    INSN_LABEL(land),
    INSN_LABEL(ior),
    INSN_LABEL(lor),
    INSN_LABEL(ixor),
    INSN_LABEL(lxor),
    INSN_LABEL(iinc),
    INSN_LABEL(i2l),
    INSN_LABEL(i2f),
    INSN_LABEL(i2d),
    INSN_LABEL(l2i),
    INSN_LABEL(l2f),
    INSN_LABEL(l2d),
    INSN_LABEL(f2i),
    INSN_LABEL(f2l),
    INSN_LABEL(f2d),
    INSN_LABEL(d2i),
    INSN_LABEL(d2l),
    INSN_LABEL(d2f),
    INSN_LABEL(i2b),
    INSN_LABEL(i2c),
    INSN_LABEL(i2s),
    INSN_LABEL(lcmp),
    INSN_LABEL(fcmpl),
    INSN_LABEL(fcmpg),
    INSN_LABEL(dcmpl),
    INSN_LABEL(dcmpg),
    INSN_LABEL(ifeq),
    INSN_LABEL(ifne),
    INSN_LABEL(iflt),
    INSN_LABEL(ifge),
    INSN_LABEL(ifgt),
    INSN_LABEL(ifle),
    INSN_LABEL(if_icmpeq),
    INSN_LABEL(if_icmpne),
    INSN_LABEL(if_icmplt),
    INSN_LABEL(if_icmpge),
    INSN_LABEL(if_icmpgt),
    INSN_LABEL(if_icmple),
    INSN_LABEL(if_acmpeq),
    INSN_LABEL(if_acmpne),
    INSN_LABEL(goto), 
    INSN_LABEL(jsr),
    INSN_LABEL(ret),
    INSN_LABEL(tableswitch),
    INSN_LABEL(lookupswitch),
    INSN_LABEL(ireturn),
    INSN_LABEL(lreturn),
    INSN_LABEL(freturn),
    INSN_LABEL(dreturn),
    INSN_LABEL(areturn),
    INSN_LABEL(return),
    INSN_LABEL(getstatic),
    INSN_LABEL(putstatic),
    INSN_LABEL(getfield),
    INSN_LABEL(putfield),
    INSN_LABEL(invokevirtual),
    INSN_LABEL(invokespecial),
    INSN_LABEL(invokestatic),
    INSN_LABEL(invokeinterface),
    0, /* Unused.  */
    INSN_LABEL(new),
    INSN_LABEL(newarray),
    INSN_LABEL(anewarray),
    INSN_LABEL(arraylength),
    INSN_LABEL(athrow),
    INSN_LABEL(checkcast),
    INSN_LABEL(instanceof),
    INSN_LABEL(monitorenter),
    INSN_LABEL(monitorexit),
#ifdef DIRECT_THREADED
    0, // wide
#else
    INSN_LABEL(wide),
#endif
    INSN_LABEL(multianewarray),
    INSN_LABEL(ifnull),
    INSN_LABEL(ifnonnull),
    INSN_LABEL(goto_w),
    INSN_LABEL(jsr_w),
    0
  };

  pc_t pc;

#ifdef DIRECT_THREADED

#define NEXT_INSN goto *((pc++)->insn)
#define INTVAL() ((pc++)->int_val)
#define AVAL() ((pc++)->datum)

#define GET1S() INTVAL ()
#define GET2S() INTVAL ()
#define GET1U() INTVAL ()
#define GET2U() INTVAL ()
#define AVAL1U() AVAL ()
#define AVAL2U() AVAL ()
#define AVAL2UP() AVAL ()
#define SKIP_GOTO ++pc
#define GOTO_VAL() (insn_slot *) pc->datum
#define PCVAL(unionval) unionval.p
#define AMPAMP(label) &&label

  // Compile if we must. NOTE: Double-check locking.
  if (prepared == NULL)
    {
      _Jv_MutexLock (&compile_mutex);
      if (prepared == NULL)
	compile (insn_target);
      _Jv_MutexUnlock (&compile_mutex);
    }
  pc = (insn_slot *) prepared;

#else

#define NEXT_INSN goto *(insn_target[*pc++])

#define GET1S() get1s (pc++)
#define GET2S() (pc += 2, get2s (pc- 2))
#define GET1U() get1u (pc++)
#define GET2U() (pc += 2, get2u (pc - 2))
#define AVAL1U() ({ int index = get1u (pc++); pool_data[index].o; })
#define AVAL2U() ({ int index = get2u (pc); pc += 2; pool_data[index].o; })
#define AVAL2UP() ({ int index = get2u (pc); pc += 2; &pool_data[index]; })
#define SKIP_GOTO pc += 2
#define GOTO_VAL() pc - 1 + get2s (pc)
#define PCVAL(unionval) unionval.i
#define AMPAMP(label) NULL

  pc = bytecode ();

#endif /* DIRECT_THREADED */

#define TAKE_GOTO pc = GOTO_VAL ()

  try
    {
      // We keep nop around.  It is used if we're interpreting the
      // bytecodes and not doing direct threading.
    insn_nop:
      NEXT_INSN;

      /* The first few instructions here are ordered according to their
	 frequency, in the hope that this will improve code locality a
	 little.  */

    insn_aload_0:		// 0x2a
      LOADA (0);
      NEXT_INSN;

    insn_iload:		// 0x15
      LOADI (GET1U ());
      NEXT_INSN;

    insn_iload_1:		// 0x1b
      LOADI (1);
      NEXT_INSN;

    insn_invokevirtual:	// 0xb6
      {
	int index = GET2U ();

	/* _Jv_ResolvePoolEntry returns immediately if the value already
	 * is resolved.  If we want to clutter up the code here to gain
	 * a little performance, then we can check the corresponding bit
	 * JV_CONSTANT_ResolvedFlag in the tag directly.  For now, I
	 * don't think it is worth it.  */

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;
	// We don't use NULLCHECK here because we can't rely on that
	// working if the method is final.  So instead we do an
	// explicit test.
	if (! sp[0].o)
	  throw new java::lang::NullPointerException;

	if (rmeth->vtable_index == -1)
	  {
	    // final methods do not appear in the vtable,
	    // if it does not appear in the superclass.
	    fun = (void (*)()) rmeth->method->ncode;
	  }
	else
	  {
	    jobject rcv = sp[0].o;
	    _Jv_VTable *table = *(_Jv_VTable**) rcv;
	    fun = (void (*)()) table->get_method (rmeth->vtable_index);
	  }

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	pc[-2].insn = &&invokevirtual_resolved;
	pc[-1].datum = rmeth;
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokevirtual_resolved:
      {
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	// We don't use NULLCHECK here because we can't rely on that
	// working if the method is final.  So instead we do an
	// explicit test.
	if (! sp[0].o)
	  throw new java::lang::NullPointerException;

	if (rmeth->vtable_index == -1)
	  {
	    // final methods do not appear in the vtable,
	    // if it does not appear in the superclass.
	    fun = (void (*)()) rmeth->method->ncode;
	  }
	else
	  {
	    jobject rcv = sp[0].o;
	    _Jv_VTable *table = *(_Jv_VTable**) rcv;
	    fun = (void (*)()) table->get_method (rmeth->vtable_index);
	  }
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    perform_invoke:
      {
	/* here goes the magic again... */
	ffi_cif *cif = &rmeth->cif;
	ffi_raw *raw = (ffi_raw*) sp;

	jdouble rvalue;

#if FFI_NATIVE_RAW_API
	/* We assume that this is only implemented if it's correct	*/
	/* to use it here.  On a 64 bit machine, it never is.		*/
	ffi_raw_call (cif, fun, (void*)&rvalue, raw);
#else
	ffi_java_raw_call (cif, fun, (void*)&rvalue, raw);
#endif

	int rtype = cif->rtype->type;

	/* the likelyhood of object, int, or void return is very high,
	 * so those are checked before the switch */
	if (rtype == FFI_TYPE_POINTER)
	  {
	    PUSHA (*(jobject*)&rvalue);
	  }
	else if (rtype == FFI_TYPE_SINT32)
	  {
	    PUSHI (*(jint*)&rvalue);
	  }
	else if (rtype == FFI_TYPE_VOID)
	  {
	    /* skip */
	  }
	else
	  {
	    switch (rtype)
	      {
	      case FFI_TYPE_SINT8:
		{
		  jbyte value = (*(jint*)&rvalue) & 0xff;
		  PUSHI (value);
		}
		break;

	      case FFI_TYPE_SINT16:
		{
		  jshort value = (*(jint*)&rvalue) & 0xffff;
		  PUSHI (value);
		}
		break;

	      case FFI_TYPE_UINT16:
		{
		  jint value = (*(jint*)&rvalue) & 0xffff;
		  PUSHI (value);
		}
		break;

	      case FFI_TYPE_FLOAT:
		PUSHF (*(jfloat*)&rvalue);
		break;

	      case FFI_TYPE_DOUBLE:
		PUSHD (rvalue);
		break;

	      case FFI_TYPE_SINT64:
		PUSHL (*(jlong*)&rvalue);
		break;

	      default:
		throw_internal_error ("unknown return type in invokeXXX");
	      }
	  }
      }
      NEXT_INSN;

    insn_aconst_null:
      PUSHA (NULL);
      NEXT_INSN;

    insn_iconst_m1:
      PUSHI (-1);
      NEXT_INSN;

    insn_iconst_0:
      PUSHI (0);
      NEXT_INSN;

    insn_iconst_1:
      PUSHI (1);
      NEXT_INSN;

    insn_iconst_2:
      PUSHI (2);
      NEXT_INSN;

    insn_iconst_3:
      PUSHI (3);
      NEXT_INSN;

    insn_iconst_4:
      PUSHI (4);
      NEXT_INSN;

    insn_iconst_5:
      PUSHI (5);
      NEXT_INSN;

    insn_lconst_0:
      PUSHL (0);
      NEXT_INSN;

    insn_lconst_1:
      PUSHL (1);
      NEXT_INSN;

    insn_fconst_0:
      PUSHF (0);
      NEXT_INSN;

    insn_fconst_1:
      PUSHF (1);
      NEXT_INSN;

    insn_fconst_2:
      PUSHF (2);
      NEXT_INSN;

    insn_dconst_0:
      PUSHD (0);
      NEXT_INSN;

    insn_dconst_1:
      PUSHD (1);
      NEXT_INSN;

    insn_bipush:
      // For direct threaded, bipush and sipush are the same.
#ifndef DIRECT_THREADED
      PUSHI (GET1S ());
      NEXT_INSN;
#endif /* DIRECT_THREADED */
    insn_sipush:
      PUSHI (GET2S ());
      NEXT_INSN;

    insn_ldc:
      // For direct threaded, ldc and ldc_w are the same.
#ifndef DIRECT_THREADED
      PUSHA ((jobject) AVAL1U ());
      NEXT_INSN;
#endif /* DIRECT_THREADED */
    insn_ldc_w:
      PUSHA ((jobject) AVAL2U ());
      NEXT_INSN;

    insn_ldc2_w:
      {
	void *where = AVAL2UP ();
	memcpy (sp, where, 2*sizeof (_Jv_word));
	sp += 2;
      }
      NEXT_INSN;

    insn_lload:
      LOADL (GET1U ());
      NEXT_INSN;

    insn_fload:
      LOADF (GET1U ());
      NEXT_INSN;

    insn_dload:
      LOADD (GET1U ());
      NEXT_INSN;

    insn_aload:
      LOADA (GET1U ());
      NEXT_INSN;

    insn_iload_0:
      LOADI (0);
      NEXT_INSN;

    insn_iload_2:
      LOADI (2);
      NEXT_INSN;

    insn_iload_3:
      LOADI (3);
      NEXT_INSN;

    insn_lload_0:
      LOADL (0);
      NEXT_INSN;

    insn_lload_1:
      LOADL (1);
      NEXT_INSN;

    insn_lload_2:
      LOADL (2);
      NEXT_INSN;

    insn_lload_3:
      LOADL (3);
      NEXT_INSN;

    insn_fload_0:
      LOADF (0);
      NEXT_INSN;

    insn_fload_1:
      LOADF (1);
      NEXT_INSN;

    insn_fload_2:
      LOADF (2);
      NEXT_INSN;

    insn_fload_3:
      LOADF (3);
      NEXT_INSN;

    insn_dload_0:
      LOADD (0);
      NEXT_INSN;

    insn_dload_1:
      LOADD (1);
      NEXT_INSN;

    insn_dload_2:
      LOADD (2);
      NEXT_INSN;

    insn_dload_3:
      LOADD (3);
      NEXT_INSN;

    insn_aload_1:
      LOADA(1);
      NEXT_INSN;

    insn_aload_2:
      LOADA(2);
      NEXT_INSN;

    insn_aload_3:
      LOADA(3);
      NEXT_INSN;

    insn_iaload:
      {
	jint index = POPI();
	jintArray arr = (jintArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_laload:
      {
	jint index = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHL( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_faload:
      {
	jint index = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHF( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_daload:
      {
	jint index = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHD( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_aaload:
      {
	jint index = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHA( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_baload:
      {
	jint index = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_caload:
      {
	jint index = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_saload:
      {
	jint index = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

    insn_istore:
      STOREI (GET1U ());
      NEXT_INSN;

    insn_lstore:
      STOREL (GET1U ());
      NEXT_INSN;

    insn_fstore:
      STOREF (GET1U ());
      NEXT_INSN;

    insn_dstore:
      STORED (GET1U ());
      NEXT_INSN;

    insn_astore:
      STOREA (GET1U ());
      NEXT_INSN;

    insn_istore_0:
      STOREI (0);
      NEXT_INSN;

    insn_istore_1:
      STOREI (1);
      NEXT_INSN;

    insn_istore_2:
      STOREI (2);
      NEXT_INSN;

    insn_istore_3:
      STOREI (3);
      NEXT_INSN;

    insn_lstore_0:
      STOREL (0);
      NEXT_INSN;

    insn_lstore_1:
      STOREL (1);
      NEXT_INSN;

    insn_lstore_2:
      STOREL (2);
      NEXT_INSN;

    insn_lstore_3:
      STOREL (3);
      NEXT_INSN;

    insn_fstore_0:
      STOREF (0);
      NEXT_INSN;

    insn_fstore_1:
      STOREF (1);
      NEXT_INSN;

    insn_fstore_2:
      STOREF (2);
      NEXT_INSN;

    insn_fstore_3:
      STOREF (3);
      NEXT_INSN;

    insn_dstore_0:
      STORED (0);
      NEXT_INSN;

    insn_dstore_1:
      STORED (1);
      NEXT_INSN;

    insn_dstore_2:
      STORED (2);
      NEXT_INSN;

    insn_dstore_3:
      STORED (3);
      NEXT_INSN;

    insn_astore_0:
      STOREA(0);
      NEXT_INSN;

    insn_astore_1:
      STOREA(1);
      NEXT_INSN;

    insn_astore_2:
      STOREA(2);
      NEXT_INSN;

    insn_astore_3:
      STOREA(3);
      NEXT_INSN;

    insn_iastore:
      {
	jint value = POPI();
	jint index  = POPI();
	jintArray arr = (jintArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_lastore:
      {
	jlong value = POPL();
	jint index  = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_fastore:
      {
	jfloat value = POPF();
	jint index  = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_dastore:
      {
	jdouble value = POPD();
	jint index  = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_aastore:
      {
	jobject value = POPA();
	jint index  = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	_Jv_CheckArrayStore (arr, value);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_bastore:
      {
	jbyte value = (jbyte) POPI();
	jint index  = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_castore:
      {
	jchar value = (jchar) POPI();
	jint index  = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_sastore:
      {
	jshort value = (jshort) POPI();
	jint index  = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLARRAYCHECK (arr);
	ARRAYBOUNDSCHECK (arr, index);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

    insn_pop:
      sp -= 1;
      NEXT_INSN;

    insn_pop2:
      sp -= 2;
      NEXT_INSN;

    insn_dup:
      sp[0] = sp[-1];
      sp += 1;
      NEXT_INSN;

    insn_dup_x1:
      dupx (sp, 1, 1); sp+=1;
      NEXT_INSN;

    insn_dup_x2:
      dupx (sp, 1, 2); sp+=1;
      NEXT_INSN;

    insn_dup2:
      sp[0] = sp[-2];
      sp[1] = sp[-1];
      sp += 2;
      NEXT_INSN;

    insn_dup2_x1:
      dupx (sp, 2, 1); sp+=2;
      NEXT_INSN;

    insn_dup2_x2:
      dupx (sp, 2, 2); sp+=2;
      NEXT_INSN;

    insn_swap:
      {
	jobject tmp1 = POPA();
	jobject tmp2 = POPA();
	PUSHA (tmp1);
	PUSHA (tmp2);
      }
      NEXT_INSN;

    insn_iadd:
      BINOPI(+);
      NEXT_INSN;

    insn_ladd:
      BINOPL(+);
      NEXT_INSN;

    insn_fadd:
      BINOPF(+);
      NEXT_INSN;

    insn_dadd:
      BINOPD(+);
      NEXT_INSN;

    insn_isub:
      BINOPI(-);
      NEXT_INSN;

    insn_lsub:
      BINOPL(-);
      NEXT_INSN;

    insn_fsub:
      BINOPF(-);
      NEXT_INSN;

    insn_dsub:
      BINOPD(-);
      NEXT_INSN;

    insn_imul:
      BINOPI(*);
      NEXT_INSN;

    insn_lmul:
      BINOPL(*);
      NEXT_INSN;

    insn_fmul:
      BINOPF(*);
      NEXT_INSN;

    insn_dmul:
      BINOPD(*);
      NEXT_INSN;

    insn_idiv:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	jint res = _Jv_divI (value1, value2);
	PUSHI (res);
      }
      NEXT_INSN;

    insn_ldiv:
      {
	jlong value2 = POPL();
	jlong value1 = POPL();
	jlong res = _Jv_divJ (value1, value2);
	PUSHL (res);
      }
      NEXT_INSN;

    insn_fdiv:
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	jfloat res = value1 / value2;
	PUSHF (res);
      }
      NEXT_INSN;

    insn_ddiv:
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	jdouble res = value1 / value2;
	PUSHD (res);
      }
      NEXT_INSN;

    insn_irem:
      {
	jint value2 = POPI();
	jint value1 =  POPI();
	jint res = _Jv_remI (value1, value2);
	PUSHI (res);
      }
      NEXT_INSN;

    insn_lrem:
      {
	jlong value2 = POPL();
	jlong value1 = POPL();
	jlong res = _Jv_remJ (value1, value2);
	PUSHL (res);
      }
      NEXT_INSN;

    insn_frem:
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	jfloat res    = __ieee754_fmod (value1, value2);
	PUSHF (res);
      }
      NEXT_INSN;

    insn_drem:
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	jdouble res    = __ieee754_fmod (value1, value2);
	PUSHD (res);
      }
      NEXT_INSN;

    insn_ineg:
      {
	jint value = POPI();
	PUSHI (value * -1);
      }
      NEXT_INSN;

    insn_lneg:
      {
	jlong value = POPL();
	PUSHL (value * -1);
      }
      NEXT_INSN;

    insn_fneg:
      {
	jfloat value = POPF();
	PUSHF (value * -1);
      }
      NEXT_INSN;

    insn_dneg:
      {
	jdouble value = POPD();
	PUSHD (value * -1);
      }
      NEXT_INSN;

    insn_ishl:
      {
	jint shift = (POPI() & 0x1f);
	jint value = POPI();
	PUSHI (value << shift);
      }
      NEXT_INSN;

    insn_lshl:
      {
	jint shift = (POPI() & 0x3f);
	jlong value = POPL();
	PUSHL (value << shift);
      }
      NEXT_INSN;

    insn_ishr:
      {
	jint shift = (POPI() & 0x1f);
	jint value = POPI();
	PUSHI (value >> shift);
      }
      NEXT_INSN;

    insn_lshr:
      {
	jint shift = (POPI() & 0x3f);
	jlong value = POPL();
	PUSHL (value >> shift);
      }
      NEXT_INSN;

    insn_iushr:
      {
	jint shift = (POPI() & 0x1f);
	_Jv_uint value = (_Jv_uint) POPI();
	PUSHI ((jint) (value >> shift));
      }
      NEXT_INSN;

    insn_lushr:
      {
	jint shift = (POPI() & 0x3f);
	_Jv_ulong value = (_Jv_ulong) POPL();
	PUSHL ((jlong) (value >> shift));
      }
      NEXT_INSN;

    insn_iand:
      BINOPI (&);
      NEXT_INSN;

    insn_land:
      BINOPL (&);
      NEXT_INSN;

    insn_ior:
      BINOPI (|);
      NEXT_INSN;

    insn_lor:
      BINOPL (|);
      NEXT_INSN;

    insn_ixor:
      BINOPI (^);
      NEXT_INSN;

    insn_lxor:
      BINOPL (^);
      NEXT_INSN;

    insn_iinc:
      {
	jint index  = GET1U ();
	jint amount = GET1S ();
	locals[index].i += amount;
      }
      NEXT_INSN;

    insn_i2l:
      {jlong value = POPI(); PUSHL (value);}
      NEXT_INSN;

    insn_i2f:
      {jfloat value = POPI(); PUSHF (value);}
      NEXT_INSN;

    insn_i2d:
      {jdouble value = POPI(); PUSHD (value);}
      NEXT_INSN;

    insn_l2i:
      {jint value = POPL(); PUSHI (value);}
      NEXT_INSN;

    insn_l2f:
      {jfloat value = POPL(); PUSHF (value);}
      NEXT_INSN;

    insn_l2d:
      {jdouble value = POPL(); PUSHD (value);}
      NEXT_INSN;

    insn_f2i:
      {
	using namespace java::lang;
	jint value = convert (POPF (), Integer::MIN_VALUE, Integer::MAX_VALUE);
	PUSHI(value);
      }
      NEXT_INSN;

    insn_f2l:
      {
	using namespace java::lang;
	jlong value = convert (POPF (), Long::MIN_VALUE, Long::MAX_VALUE);
	PUSHL(value);
      }
      NEXT_INSN;

    insn_f2d:
      { jdouble value = POPF (); PUSHD(value); }
      NEXT_INSN;

    insn_d2i:
      {
	using namespace java::lang;
	jint value = convert (POPD (), Integer::MIN_VALUE, Integer::MAX_VALUE);
	PUSHI(value);
      }
      NEXT_INSN;

    insn_d2l:
      {
	using namespace java::lang;
	jlong value = convert (POPD (), Long::MIN_VALUE, Long::MAX_VALUE);
	PUSHL(value);
      }
      NEXT_INSN;

    insn_d2f:
      { jfloat value = POPD (); PUSHF(value); }
      NEXT_INSN;

    insn_i2b:
      { jbyte value = POPI (); PUSHI(value); }
      NEXT_INSN;

    insn_i2c:
      { jchar value = POPI (); PUSHI(value); }
      NEXT_INSN;

    insn_i2s:
      { jshort value = POPI (); PUSHI(value); }
      NEXT_INSN;

    insn_lcmp:
      {
	jlong value2 = POPL ();
	jlong value1 = POPL ();
	if (value1 > value2)
	  { PUSHI (1); }
	else if (value1 == value2)
	  { PUSHI (0); }
	else
	  { PUSHI (-1); }
      }
      NEXT_INSN;

    insn_fcmpl:
      tmpval = -1;
      goto fcmp;

    insn_fcmpg:
      tmpval = 1;

    fcmp:
      {
	jfloat value2 = POPF ();
	jfloat value1 = POPF ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else
	  PUSHI (tmpval);
      }
      NEXT_INSN;

    insn_dcmpl:
      tmpval = 1;
      goto dcmp;

    insn_dcmpg:
      tmpval = -1;

    dcmp:
      {
	jdouble value2 = POPD ();
	jdouble value1 = POPD ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else
	  PUSHI (tmpval);
      }
      NEXT_INSN;

    insn_ifeq:
      {
	if (POPI() == 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_ifne:
      {
	if (POPI() != 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_iflt:
      {
	if (POPI() < 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_ifge:
      {
	if (POPI() >= 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_ifgt:
      {
	if (POPI() > 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_ifle:
      {
	if (POPI() <= 0)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmpeq:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 == value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmpne:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 != value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmplt:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 < value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmpge:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 >= value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmpgt:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 > value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_icmple:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 <= value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_acmpeq:
      {
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 == value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_if_acmpne:
      {
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 != value2)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_goto_w:
#ifndef DIRECT_THREADED
      // For direct threaded, goto and goto_w are the same.
      pc = pc - 1 + get4 (pc);
      NEXT_INSN;
#endif /* DIRECT_THREADED */
    insn_goto:
      TAKE_GOTO;
      NEXT_INSN;

    insn_jsr_w:
#ifndef DIRECT_THREADED
      // For direct threaded, jsr and jsr_w are the same.
      {
	pc_t next = pc - 1 + get4 (pc);
	pc += 4;
	PUSHA ((jobject) pc);
	pc = next;
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */
    insn_jsr:
      {
	pc_t next = GOTO_VAL();
	SKIP_GOTO;
	PUSHA ((jobject) pc);
	pc = next;
      }
      NEXT_INSN;

    insn_ret:
      {
	jint index = GET1U ();
	pc = (pc_t) PEEKA (index);
      }
      NEXT_INSN;

    insn_tableswitch:
      {
#ifdef DIRECT_THREADED
	void *def = (pc++)->datum;

	int index = POPI();

	jint low = INTVAL ();
	jint high = INTVAL ();

	if (index < low || index > high)
	  pc = (insn_slot *) def;
	else
	  pc = (insn_slot *) ((pc + index - low)->datum);
#else
	pc_t base_pc = pc - 1;
	int index = POPI ();

	pc_t base = (pc_t) bytecode ();
	while ((pc - base) % 4 != 0)
	  ++pc;

	jint def = get4 (pc);
	jint low = get4 (pc + 4);
	jint high = get4 (pc + 8);
	if (index < low || index > high)
	  pc = base_pc + def;
	else
	  pc = base_pc + get4 (pc + 4 * (index - low + 3));
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

    insn_lookupswitch:
      {
#ifdef DIRECT_THREADED
	void *def = (pc++)->insn;

	int index = POPI();

	jint npairs = INTVAL ();

	int max = npairs - 1;
	int min = 0;

	// Simple binary search...
	while (min < max)
	  {
	    int half = (min + max) / 2;
	    int match = pc[2 * half].int_val;

	    if (index == match)
	      {
		// Found it.
		pc = (insn_slot *) pc[2 * half + 1].datum;
		NEXT_INSN;
	      }
	    else if (index < match)
	      // We can use HALF - 1 here because we check again on
	      // loop exit.
	      max = half - 1;
	    else
	      // We can use HALF + 1 here because we check again on
	      // loop exit.
	      min = half + 1;
	  }
	if (index == pc[2 * min].int_val)
	  pc = (insn_slot *) pc[2 * min + 1].datum;
	else
	  pc = (insn_slot *) def;
#else
	unsigned char *base_pc = pc-1;
	int index = POPI();

	unsigned char* base = bytecode ();
	while ((pc-base) % 4 != 0)
	  ++pc;

	jint def     = get4 (pc);
	jint npairs  = get4 (pc+4);

	int max = npairs-1;
	int min = 0;

	// Simple binary search...
	while (min < max)
	  {
	    int half = (min+max)/2;
	    int match = get4 (pc+ 4*(2 + 2*half));

	    if (index == match)
	      min = max = half;
	    else if (index < match)
	      // We can use HALF - 1 here because we check again on
	      // loop exit.
	      max = half - 1;
	    else
	      // We can use HALF + 1 here because we check again on
	      // loop exit.
	      min = half + 1;
	  }

	if (index == get4 (pc+ 4*(2 + 2*min)))
	  pc = base_pc + get4 (pc+ 4*(2 + 2*min + 1));
	else
	  pc = base_pc + def;    
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

    insn_areturn:
      *(jobject *) retp = POPA ();
      return;

    insn_lreturn:
      *(jlong *) retp = POPL ();
      return;

    insn_freturn:
      *(jfloat *) retp = POPF ();
      return;

    insn_dreturn:
      *(jdouble *) retp = POPD ();
      return;

    insn_ireturn:
      *(jint *) retp = POPI ();
      return;

    insn_return:
      return;

    insn_getstatic:
      {
	jint fieldref_index = GET2U ();
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & Modifier::STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	jclass type = field->type;

	// We rewrite the instruction once we discover what it refers
	// to.
	void *newinsn = NULL;
	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		PUSHI (*(jbyte*) (field->u.addr));
		newinsn = AMPAMP (getstatic_resolved_1);
		break;

	      case 2:
		if (type == JvPrimClass (char))
		  {
		    PUSHI(*(jchar*) (field->u.addr));
		    newinsn = AMPAMP (getstatic_resolved_char);
		  }
		else
		  {
		    PUSHI(*(jshort*) (field->u.addr));
		    newinsn = AMPAMP (getstatic_resolved_short);
		  }
		break;

	      case 4:
		PUSHI(*(jint*) (field->u.addr));
		newinsn = AMPAMP (getstatic_resolved_4);
		break;

	      case 8:
		PUSHL(*(jlong*) (field->u.addr));
		newinsn = AMPAMP (getstatic_resolved_8);
		break;
	      }
	  }
	else
	  {
	    PUSHA(*(jobject*) (field->u.addr));
	    newinsn = AMPAMP (getstatic_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	pc[-2].insn = newinsn;
	pc[-1].datum = field->u.addr;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    getstatic_resolved_1:
      PUSHI (*(jbyte *) AVAL ());
      NEXT_INSN;

    getstatic_resolved_char:
      PUSHI (*(jchar *) AVAL ());
      NEXT_INSN;

    getstatic_resolved_short:
      PUSHI (*(jshort *) AVAL ());
      NEXT_INSN;

    getstatic_resolved_4:
      PUSHI (*(jint *) AVAL ());
      NEXT_INSN;

    getstatic_resolved_8:
      PUSHL (*(jlong *) AVAL ());
      NEXT_INSN;

    getstatic_resolved_obj:
      PUSHA (*(jobject *) AVAL ());
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_getfield:
      {
	jint fieldref_index = GET2U ();
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jclass type = field->type;
	jint field_offset = field->u.boffset;
	if (field_offset > 0xffff)
	  throw new java::lang::VirtualMachineError;

	jobject obj   = POPA();
	NULLCHECK(obj);

	void *newinsn = NULL;
	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		PUSHI (*(jbyte*) ((char*)obj + field_offset));
		newinsn = AMPAMP (getfield_resolved_1);
		break;

	      case 2:
		if (type == JvPrimClass (char))
		  {
		    PUSHI (*(jchar*) ((char*)obj + field_offset));
		    newinsn = AMPAMP (getfield_resolved_char);
		  }
		else
		  {
		    PUSHI (*(jshort*) ((char*)obj + field_offset));
		    newinsn = AMPAMP (getfield_resolved_short);
		  }
		break;

	      case 4:
		PUSHI (*(jint*) ((char*)obj + field_offset));
		newinsn = AMPAMP (getfield_resolved_4);
		break;

	      case 8:
		PUSHL(*(jlong*) ((char*)obj + field_offset));
		newinsn = AMPAMP (getfield_resolved_8);
		break;
	      }
	  }
	else
	  {
	    PUSHA(*(jobject*) ((char*)obj + field_offset));
	    newinsn = AMPAMP (getfield_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	pc[-2].insn = newinsn;
	pc[-1].int_val = field_offset;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    getfield_resolved_1:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHI (*(jbyte *) (obj + INTVAL ()));
      }
      NEXT_INSN;

    getfield_resolved_char:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHI (*(jchar *) (obj + INTVAL ()));
      }
      NEXT_INSN;

    getfield_resolved_short:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHI (*(jshort *) (obj + INTVAL ()));
      }
      NEXT_INSN;

    getfield_resolved_4:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHI (*(jint *) (obj + INTVAL ()));
      }
      NEXT_INSN;

    getfield_resolved_8:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHL (*(jlong *) (obj + INTVAL ()));
      }
      NEXT_INSN;

    getfield_resolved_obj:
      {
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	PUSHA (*(jobject *) (obj + INTVAL ()));
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_putstatic:
      {
	jint fieldref_index = GET2U ();
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	// ResolvePoolEntry cannot check this
	if ((field->flags & Modifier::STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	void *newinsn = NULL;
	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes) 
	      {
	      case 1:
		{
		  jint value = POPI();
		  *(jbyte*) (field->u.addr) = value;
		  newinsn = AMPAMP (putstatic_resolved_1);
		  break;
		}

	      case 2:
		{
		  jint value = POPI();
		  *(jchar*) (field->u.addr) = value;
		  newinsn = AMPAMP (putstatic_resolved_2);
		  break;
		}

	      case 4:
		{
		  jint value = POPI();
		  *(jint*) (field->u.addr) = value;
		  newinsn = AMPAMP (putstatic_resolved_4);
		  break;
		}

	      case 8:
		{
		  jlong value = POPL();
		  *(jlong*) (field->u.addr) = value;
		  newinsn = AMPAMP (putstatic_resolved_8);
		  break;
		}
	      }
	  }
	else
	  {
	    jobject value = POPA();
	    *(jobject*) (field->u.addr) = value;
	    newinsn = AMPAMP (putstatic_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	pc[-2].insn = newinsn;
	pc[-1].datum = field->u.addr;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    putstatic_resolved_1:
      *(jbyte *) AVAL () = POPI ();
      NEXT_INSN;

    putstatic_resolved_2:
      *(jchar *) AVAL () = POPI ();
      NEXT_INSN;

    putstatic_resolved_4:
      *(jint *) AVAL () = POPI ();
      NEXT_INSN;

    putstatic_resolved_8:
      *(jlong *) AVAL () = POPL ();
      NEXT_INSN;

    putstatic_resolved_obj:
      *(jobject *) AVAL () = POPA ();
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_putfield:
      {
	jint fieldref_index = GET2U ();
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jint field_offset = field->u.boffset;
	if (field_offset > 0xffff)
	  throw new java::lang::VirtualMachineError;

	void *newinsn = NULL;
	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes) 
	      {
	      case 1:
		{
		  jint    value = POPI();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jbyte*) ((char*)obj + field_offset) = value;
		  newinsn = AMPAMP (putfield_resolved_1);
		  break;
		}

	      case 2:
		{
		  jint    value = POPI();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jchar*) ((char*)obj + field_offset) = value;
		  newinsn = AMPAMP (putfield_resolved_2);
		  break;
		}

	      case 4:
		{
		  jint    value = POPI();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jint*) ((char*)obj + field_offset) = value;
		  newinsn = AMPAMP (putfield_resolved_4);
		  break;
		}

	      case 8:
		{
		  jlong   value = POPL();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jlong*) ((char*)obj + field_offset) = value;
		  newinsn = AMPAMP (putfield_resolved_8);
		  break;
		}
	      }
	  }
	else
	  {
	    jobject value = POPA();
	    jobject obj   = POPA();
	    NULLCHECK(obj);
	    *(jobject*) ((char*)obj + field_offset) = value;
	    newinsn = AMPAMP (putfield_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	pc[-2].insn = newinsn;
	pc[-1].int_val = field_offset;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    putfield_resolved_1:
      {
	jint val = POPI ();
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	*(jbyte *) (obj + INTVAL ()) = val;
      }
      NEXT_INSN;

    putfield_resolved_2:
      {
	jint val = POPI ();
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	*(jchar *) (obj + INTVAL ()) = val;
      }
      NEXT_INSN;

    putfield_resolved_4:
      {
	jint val = POPI ();
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	*(jint *) (obj + INTVAL ()) = val;
      }
      NEXT_INSN;

    putfield_resolved_8:
      {
	jlong val = POPL ();
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	*(jlong *) (obj + INTVAL ()) = val;
      }
      NEXT_INSN;

    putfield_resolved_obj:
      {
	jobject val = POPA ();
	char *obj = (char *) POPA ();
	NULLCHECK (obj);
	*(jobject *) (obj + INTVAL ()) = val;
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_invokespecial:
      {
	int index = GET2U ();

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;

	// We don't use NULLCHECK here because we can't rely on that
	// working for <init>.  So instead we do an explicit test.
	if (! sp[0].o)
	  throw new java::lang::NullPointerException;

	fun = (void (*)()) rmeth->method->ncode;

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	pc[-2].insn = &&invokespecial_resolved;
	pc[-1].datum = rmeth;
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokespecial_resolved:
      {
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	// We don't use NULLCHECK here because we can't rely on that
	// working for <init>.  So instead we do an explicit test.
	if (! sp[0].o)
	  throw new java::lang::NullPointerException;
	fun = (void (*)()) rmeth->method->ncode;
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    insn_invokestatic:
      {
	int index = GET2U ();

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;

	fun = (void (*)()) rmeth->method->ncode;

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	pc[-2].insn = &&invokestatic_resolved;
	pc[-1].datum = rmeth;
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokestatic_resolved:
      {
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	fun = (void (*)()) rmeth->method->ncode;
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    insn_invokeinterface:
      {
	int index = GET2U ();

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;

	jobject rcv = sp[0].o;

	NULLCHECK (rcv);

	fun = (void (*)())
	  _Jv_LookupInterfaceMethod (rcv->getClass (),
				     rmeth->method->name,
				     rmeth->method->signature);

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	pc[-2].insn = &&invokeinterface_resolved;
	pc[-1].datum = rmeth;
#else
	// Skip dummy bytes.
	pc += 2;
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokeinterface_resolved:
      {
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	jobject rcv = sp[0].o;
	NULLCHECK (rcv);
	fun = (void (*)())
	  _Jv_LookupInterfaceMethod (rcv->getClass (),
				     rmeth->method->name,
				     rmeth->method->signature);
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    insn_new:
      {
	int index = GET2U ();
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	// We initialize here because otherwise `size_in_bytes' may
	// not be set correctly, leading us to pass `0' as the size.
	// FIXME: fix in the allocator?  There is a PR for this.
	_Jv_InitClass (klass);
	jobject res = _Jv_AllocObject (klass, klass->size_in_bytes);
	PUSHA (res);

#ifdef DIRECT_THREADED
	pc[-2].insn = &&new_resolved;
	pc[-1].datum = klass;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    new_resolved:
      {
	jclass klass = (jclass) AVAL ();
	jobject res = _Jv_AllocObject (klass, klass->size_in_bytes);
	PUSHA (res);
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_newarray:
      {
	int atype = GET1U ();
	int size  = POPI();
	jobject result = _Jv_NewArray (atype, size);
	PUSHA (result);
      }
      NEXT_INSN;

    insn_anewarray:
      {
	int index = GET2U ();
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	int size  = POPI();
	jobject result = _Jv_NewObjectArray (size, klass, 0);
	PUSHA (result);

#ifdef DIRECT_THREADED
	pc[-2].insn = &&anewarray_resolved;
	pc[-1].datum = klass;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    anewarray_resolved:
      {
	jclass klass = (jclass) AVAL ();
	int size = POPI ();
	jobject result = _Jv_NewObjectArray (size, klass, 0);
	PUSHA (result);
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_arraylength:
      {
	__JArray *arr = (__JArray*)POPA();
	NULLARRAYCHECK (arr);
	PUSHI (arr->length);
      }
      NEXT_INSN;

    insn_athrow:
      {
	jobject value = POPA();
	throw static_cast<jthrowable>(value);
      }
      NEXT_INSN;

    insn_checkcast:
      {
	jobject value = POPA();
	jint index = GET2U ();
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;

	if (value != NULL && ! to->isInstance (value))
	  throw new java::lang::ClassCastException (to->getName());

	PUSHA (value);

#ifdef DIRECT_THREADED
	pc[-2].insn = &&checkcast_resolved;
	pc[-1].datum = to;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    checkcast_resolved:
      {
	jobject value = POPA ();
	jclass to = (jclass) AVAL ();
	if (value != NULL && ! to->isInstance (value))
	  throw new java::lang::ClassCastException (to->getName());
	PUSHA (value);
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_instanceof:
      {
	jobject value = POPA();
	jint index = GET2U ();
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	PUSHI (to->isInstance (value));

#ifdef DIRECT_THREADED
	pc[-2].insn = &&instanceof_resolved;
	pc[-1].datum = to;
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    instanceof_resolved:
      {
	jobject value = POPA ();
	jclass to = (jclass) AVAL ();
	PUSHI (to->isInstance (value));
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_monitorenter:
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorEnter (value);
      }
      NEXT_INSN;

    insn_monitorexit:
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorExit (value);
      }
      NEXT_INSN;

    insn_ifnull:
      {
	jobject val = POPA();
	if (val == NULL)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_ifnonnull:
      {
	jobject val = POPA();
	if (val != NULL)
	  TAKE_GOTO;
	else
	  SKIP_GOTO;
      }
      NEXT_INSN;

    insn_multianewarray:
      {
	int kind_index = GET2U ();
	int dim        = GET1U ();

	jclass type    
	  = (_Jv_ResolvePoolEntry (defining_class, kind_index)).clazz;
	jint *sizes    = (jint*) __builtin_alloca (sizeof (jint)*dim);

	for (int i = dim - 1; i >= 0; i--)
	  {
	    sizes[i] = POPI ();
	  }

	jobject res    = _Jv_NewMultiArray (type,dim, sizes);

	PUSHA (res);
      }
      NEXT_INSN;

#ifndef DIRECT_THREADED
    insn_wide:
      {
	jint the_mod_op = get1u (pc++);
	jint wide       = get2u (pc); pc += 2;

	switch (the_mod_op)
	  {
	  case op_istore:
	    STOREI (wide);
	    NEXT_INSN;

	  case op_fstore:
	    STOREF (wide);
	    NEXT_INSN;

	  case op_astore:
	    STOREA (wide);
	    NEXT_INSN;

	  case op_lload:
	    LOADL (wide);
	    NEXT_INSN;

	  case op_dload:
	    LOADD (wide);
	    NEXT_INSN;

	  case op_iload:
	    LOADI (wide);
	    NEXT_INSN;

	  case op_aload:
	    LOADA (wide);
	    NEXT_INSN;

	  case op_lstore:
	    STOREL (wide);
	    NEXT_INSN;

	  case op_dstore:
	    STORED (wide);
	    NEXT_INSN;

	  case op_ret:
	    pc = (unsigned char*) PEEKA (wide);
	    NEXT_INSN;

	  case op_iinc:
	    {
	      jint amount = get2s (pc); pc += 2;
	      jint value = PEEKI (wide);
	      POKEI (wide, value+amount);
	    }
	    NEXT_INSN;

	  default:
	    throw_internal_error ("illegal bytecode modified by wide");
	  }

      }
#endif /* DIRECT_THREADED */
    }
  catch (java::lang::Throwable *ex)
    {
#ifdef DIRECT_THREADED
      void *logical_pc = (void *) ((insn_slot *) pc - 1);
#else
      int logical_pc = pc - 1 - bytecode ();
#endif
      _Jv_InterpException *exc = exceptions ();
      jclass exc_class = ex->getClass ();

      for (int i = 0; i < exc_count; i++)
	{
	  if (PCVAL (exc[i].start_pc) <= logical_pc
	      && logical_pc < PCVAL (exc[i].end_pc))
	    {
#ifdef DIRECT_THREADED
	      jclass handler = (jclass) exc[i].handler_type.p;
#else
	      jclass handler = NULL;
	      if (exc[i].handler_type.i != 0)
		handler = (_Jv_ResolvePoolEntry (defining_class,
						 exc[i].handler_type.i)).clazz;
#endif /* DIRECT_THREADED */

	      if (handler == NULL || handler->isAssignableFrom (exc_class))
		{
#ifdef DIRECT_THREADED
		  pc = (insn_slot *) exc[i].handler_pc.p;
#else
		  pc = bytecode () + exc[i].handler_pc.i;
#endif /* DIRECT_THREADED */
		  sp = stack;
		  sp++->o = ex; // Push exception.
		  NEXT_INSN;
		}
	    }
	}

      // No handler, so re-throw.
      throw ex;
    }
}

// This function exists so that the stack-tracing code can find the
// boundaries of the interpreter.
void
_Jv_EndOfInterpreter (void)
{
}

static void
throw_internal_error (char *msg)
{
  throw new java::lang::InternalError (JvNewStringLatin1 (msg));
}

static void 
throw_incompatible_class_change_error (jstring msg)
{
  throw new java::lang::IncompatibleClassChangeError (msg);
}

#ifndef HANDLE_SEGV
static java::lang::NullPointerException *null_pointer_exc;
static void 
throw_null_pointer_exception ()
{
  if (null_pointer_exc == NULL)
    null_pointer_exc = new java::lang::NullPointerException;

  throw null_pointer_exc;
}
#endif

#endif // INTERPRETER

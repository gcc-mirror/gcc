// interpret.cc - Code for the interpreter

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

#include <config.h>

#pragma implementation "java-interp.h"

#include <jvm.h>
#include <java-cpool.h>
#include <java-interp.h>
// #include <java/lang/fdlibm.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/Integer.h>
#include <java/lang/StringBuffer.h>
#include <java/lang/Class.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/ClassCastException.h>
#include <java/lang/VirtualMachineError.h>
#include <java/lang/InternalError.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/ArithmeticException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java-insns.h>
#include <java-signal.h>

#ifdef INTERPRETER

#define ClassError _CL_Q34java4lang5Error
extern java::lang::Class ClassError;

static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);

static void throw_internal_error (char *msg)
  __attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
  __attribute__ ((__noreturn__));
#ifndef HANDLE_SEGV
static void throw_null_pointer_exception ()
  __attribute__ ((__noreturn__));
#endif
#ifndef HANDLE_FPE
static void throw_arithmetic_exception ()
  __attribute__ ((__noreturn__));
#endif


extern "C" double __ieee754_fmod __P((double,double));

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
  
};


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
#else
#define NULLCHECK(X) \
  do { if ((X)==NULL) throw_null_pointer_exception (); } while (0)
#endif

#ifdef HANDLE_FPE
#define ZEROCHECK(X)
#else
#define ZEROCHECK(X) \
  do { if ((X) == 0) throw_arithmetic_exception (); } while (0)
#endif

// this method starts the actual running of the method.  It is inlined
// in three different variants in the static methods run_normal,
// run_sync_object and run_sync_class (see below).  Those static methods
// are installed directly in the stub for this method (by
// _Jv_InterpMethod::ncode, in resolve.cc).

inline jobject
_Jv_InterpMethod::run (ffi_cif* cif,
		       void *retp,
		       ffi_raw *args,
		       _Jv_InterpMethodInvocation *inv)
{
  inv->running  = this;
  inv->pc       = bytecode ();
  inv->sp       = inv->stack_base ();
  _Jv_word *locals = inv->local_base ();

  /* Go straight at it!  the ffi raw format matches the internal
     stack representation exactly.  At least, that's the idea.
  */
  memcpy ((void*) locals, (void*) args, args_raw_size);

 next_segment:

  jobject ex = NULL;

  try
    {
      continue1 (inv);
    }
  catch (java::lang::Throwable *ex2)
    {
      ex = ex2;
    }

  if (ex == 0)			// no exception...
    {
      /* define sp locally, so the POP? macros will pick it up */
      _Jv_word *sp = inv->sp;
      int rtype = cif->rtype->type;

      if (rtype == FFI_TYPE_POINTER)
	{
	  jobject r = POPA();
	  *(jobject*) retp = r;
	  return 0;
	}
      else if (rtype == FFI_TYPE_SINT32)
	{
	  jint r = POPI();
	  *(jint*)retp = r;
	  return 0;
	}
      else if (rtype == FFI_TYPE_VOID)
	{
	  return 0;
	}
      else switch (rtype)
	{
	case FFI_TYPE_FLOAT:
	  {
	    jfloat r = POPF();
	    *(jfloat*)retp = r;
	    return 0;
	  }
      
	case FFI_TYPE_DOUBLE:
	  {
	    jdouble r = POPD();
	    *(jdouble*)retp = r;
	    return 0;
	  }

	case FFI_TYPE_UINT8:
	case FFI_TYPE_UINT16:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT8:
	case FFI_TYPE_SINT16:
	  {
	    jint r = POPI();
	    *(jint*)retp = r;
	    return 0;
	  }
      
	case FFI_TYPE_SINT64:
	  {
	    jlong r = POPL();
	    *(jlong*)retp = r;
	    return 0;
	  }
	
	default:
	  throw_internal_error ("unknown return type");
	}
    }

  /** handle an exception */
  if ( find_exception (ex, inv) )
    goto next_segment;

  return ex;
}

bool _Jv_InterpMethod::find_exception (jobject ex,
				       _Jv_InterpMethodInvocation *inv)
{
  int logical_pc = inv->pc - bytecode ();
  _Jv_InterpException *exc = exceptions ();
  jclass exc_class = ex->getClass ();

  for (int i = 0; i < exc_count; i++)
    {
      if (exc[i].start_pc <= logical_pc && logical_pc < exc[i].end_pc)
	{	
	  jclass handler;

	  if (exc[i].handler_type != 0)
	    handler = (_Jv_ResolvePoolEntry (defining_class, 
					     exc[i].handler_type)).clazz;
	  else
	    handler = NULL;
	  
	  if (handler==NULL || handler->isAssignableFrom (exc_class))
	    {
	      inv->pc = bytecode () + exc[i].handler_pc;
	      inv->sp = inv->stack_base (); // reset stack
	      (inv->sp++)->o = ex; // push exception
	      return true;
	    }
	}
    }
  return false;
}

void _Jv_InterpMethod::run_normal (ffi_cif* cif,
				   void* ret,
				   ffi_raw * args,
				   void* __this)
{
  _Jv_InterpMethod* _this = (_Jv_InterpMethod*)__this;

  // we do the alloca of the method invocation here, to allow the method
  // "run" ro be inlined.  Otherwise gcc will ignore the inline directive.
  int storage_size = _this->max_stack+_this->max_locals;
  _Jv_InterpMethodInvocation* inv = (_Jv_InterpMethodInvocation*) 
    alloca (sizeof (_Jv_InterpMethodInvocation)
	    + storage_size * sizeof (_Jv_word));

  jobject ex = _this->run (cif, ret, args, inv);
  if (ex != 0) _Jv_Throw (ex);
}

void _Jv_InterpMethod::run_synch_object (ffi_cif* cif,
					 void* ret,
					 ffi_raw * args,
					 void* __this)
{
  _Jv_InterpMethod* _this = (_Jv_InterpMethod*)__this;
  jobject rcv = (jobject)args[0].ptr;

  int storage_size = _this->max_stack+_this->max_locals;
  _Jv_InterpMethodInvocation* inv = (_Jv_InterpMethodInvocation*) 
    alloca (sizeof (_Jv_InterpMethodInvocation)
	    + storage_size * sizeof (_Jv_word));

  _Jv_MonitorEnter (rcv);
  jobject ex = _this->run (cif, ret, args, inv);
  _Jv_MonitorExit (rcv);

  if (ex != 0) _Jv_Throw (ex);
}

void _Jv_InterpMethod::run_synch_class (ffi_cif* cif,
					void* ret,
					ffi_raw * args,
					void* __this)
{
  _Jv_InterpMethod* _this = (_Jv_InterpMethod*)__this;
  jclass  sync = _this->defining_class;

  int storage_size = _this->max_stack+_this->max_locals;
  _Jv_InterpMethodInvocation* inv = (_Jv_InterpMethodInvocation*) 
    alloca (sizeof (_Jv_InterpMethodInvocation)
	    + storage_size * sizeof (_Jv_word));

  _Jv_MonitorEnter (sync);
  jobject ex = _this->run (cif, ret, args, inv);
  _Jv_MonitorExit (sync);

  if (ex != 0) _Jv_Throw (ex);
}

/*
  This proceeds execution, as designated in "inv".  If an exception
  happens, then it is simply thrown, and handled in Java.  Thus, the pc
  needs to be stored in the inv->pc at all times, so we can figure
  out which handler (if any) to invoke.

  One design issue, which I have not completely considered, is if it
  should be possible to have interpreted classes linked in!  Seldom used
  (or non-critical) classes could reasonably be interpreted.  
*/


void _Jv_InterpMethod::continue1 (_Jv_InterpMethodInvocation *inv)
{
  using namespace java::lang::reflect;

  register _Jv_word      *sp     = inv->sp;
  register unsigned char *pc     = inv->pc;
  _Jv_word               *locals = inv->local_base ();

  _Jv_word *pool_data   = defining_class->constants.data;
  
  /* these two are used in the invokeXXX instructions */
  void (*fun)(...);
  _Jv_ResolvedMethod* rmeth;

#define INSN_LABEL(op) &&insn_##op
#define GOTO_INSN(op) goto *(insn_target[op])

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
    0, /* op_xxxunusedxxx1, */
    INSN_LABEL(new),
    INSN_LABEL(newarray),
    INSN_LABEL(anewarray),
    INSN_LABEL(arraylength),
    INSN_LABEL(athrow),
    INSN_LABEL(checkcast),
    INSN_LABEL(instanceof),
    INSN_LABEL(monitorenter),
    INSN_LABEL(monitorexit),
    INSN_LABEL(wide),
    INSN_LABEL(multianewarray),
    INSN_LABEL(ifnull),
    INSN_LABEL(ifnonnull),
    INSN_LABEL(goto_w),
    INSN_LABEL(jsr_w),
  };

#define SAVE_PC   inv->pc = pc-1

  /* If the macro INLINE_SWITCH is not defined, then the main loop
     operates as one big (normal) switch statement.  If it is defined,
     then the case selection is performed `inline' in the end of the
     code for each case.  The latter saves a native branch instruction
     for each java-instruction, but expands the code size somewhat.

     NOTE: On i386 defining INLINE_SWITCH improves over all
     performance approximately seven percent, but it may be different
     for other machines.  At some point, this may be made into a proper
     configuration parameter.  */

#define INLINE_SWITCH 

#ifdef  INLINE_SWITCH

#define NEXT_INSN do { GOTO_INSN(*pc++); } while (0)


  NEXT_INSN;
#else

#define NEXT_INSN goto next_insn

 next_insn:
  GOTO_INSN (*pc++);

#endif

  /* The first few instructions here are ordered according to their
     frequency, in the hope that this will improve code locality a
     little.  */

     insn_aload_0:		// 0x2a
      LOADA(0);
      NEXT_INSN;

     insn_iload:		// 0x15
      LOADI (get1u (pc++));
      NEXT_INSN;

     insn_iload_1:		// 0x1b
      LOADI (1);
      NEXT_INSN;

     insn_invokevirtual:	// 0xb6
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;

	/* _Jv_ResolvePoolEntry returns immediately if the value already
	 * is resolved.  If we want to clutter up the code here to gain
	 * a little performance, then we can check the corresponding bit
	 * JV_CONSTANT_ResolvedFlag in the tag directly.  For now, I
	 * don't think it is worth it.  */

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;
	NULLCHECK(sp[0]);

	if (rmeth->vtable_index == -1)
	  {
	    // final methods do not appear in the vtable,
	    // if it does not appear in the superclass.
	    fun = (void (*) (...)) rmeth->method->ncode;
	  }
	else
	  {
	    jobject rcv = sp[0].o;
	    _Jv_VTable *table = *(_Jv_VTable**)rcv;
	    fun = (void (*) (...))table->method[rmeth->vtable_index];
	  }
      }
      goto perform_invoke;

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
	else switch (rtype) 
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
      NEXT_INSN;


     insn_nop:
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
      PUSHI (get1s(pc++));
      NEXT_INSN;

     insn_sipush:
      PUSHI (get2s(pc)); pc += 2;
      NEXT_INSN;

     insn_ldc:
      {
	int index = get1u (pc++);
	PUSHA(pool_data[index].o);
      }
      NEXT_INSN;

     insn_ldc_w:
      {
	int index = get2u (pc); pc += 2;
	PUSHA(pool_data[index].o);
      }
      NEXT_INSN;

     insn_ldc2_w:
      {
	int index = get2u (pc); pc += 2;
	memcpy (sp, &pool_data[index], 2*sizeof (_Jv_word));
	sp += 2;
      }
      NEXT_INSN;

     insn_lload:
      LOADL (get1u (pc++));
      NEXT_INSN;

     insn_fload:
      LOADF (get1u (pc++));
      NEXT_INSN;

     insn_dload:
      LOADD (get1u (pc++));
      NEXT_INSN;

     insn_aload:
      LOADA (get1u (pc++));
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
      SAVE_PC;
      {
	jint index = POPI();
	jintArray arr = (jintArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_laload:
      SAVE_PC;
      {
	jint index = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHL( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_faload:
      SAVE_PC;
      {
	jint index = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHF( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_daload:
      SAVE_PC;
      {
	jint index = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHD( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_aaload:
      SAVE_PC;
      {
	jint index = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHA( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_baload:
      SAVE_PC;
      {
	jint index = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_caload:
      SAVE_PC;
      {
	jint index = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_saload:
      SAVE_PC;
      {
	jint index = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      NEXT_INSN;

     insn_istore:
      STOREI (get1u (pc++));
      NEXT_INSN;

     insn_lstore:
      STOREL (get1u (pc++));
      NEXT_INSN;

     insn_fstore:
      STOREF (get1u (pc++));
      NEXT_INSN;

     insn_dstore:
      STORED (get1u (pc++));
      NEXT_INSN;

     insn_astore:
      STOREA (get1u (pc++));
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
      SAVE_PC;
      {
	jint value = POPI();
	jint index  = POPI();
	jintArray arr = (jintArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_lastore:
      SAVE_PC;
      {
	jlong value = POPL();
	jint index  = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_fastore:
      SAVE_PC;
      {
	jfloat value = POPF();
	jint index  = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_dastore:
      SAVE_PC;
      {
	jdouble value = POPD();
	jint index  = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_aastore:
      SAVE_PC;
      {
	jobject value = POPA();
	jint index  = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	_Jv_CheckArrayStore (arr, value);
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_bastore:
      SAVE_PC;
      {
	jbyte value = (jbyte) POPI();
	jint index  = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_castore:
      SAVE_PC;
      {
	jchar value = (jchar) POPI();
	jint index  = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      NEXT_INSN;

     insn_sastore:
      SAVE_PC;
      {
	jshort value = (jshort) POPI();
	jint index  = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
	  {
	    _Jv_ThrowBadArrayIndex (index);
	  }
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
      SAVE_PC;
      {
	jint value2 = POPI();
	jint value1 = POPI();
	ZEROCHECK (value2);
	jint res = value1 / value2;
	PUSHI (res);
      }
      NEXT_INSN;

     insn_ldiv:
      SAVE_PC;
      {
	jlong value2 = POPL();
	jlong value1 = POPL();
	ZEROCHECK (value2);
	jlong res = value1 / value2;
	PUSHL (res);
      }
      NEXT_INSN;

     insn_fdiv:
      SAVE_PC;
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	ZEROCHECK (value2);
	jfloat res = value1 / value2;
	PUSHF (res);
      }
      NEXT_INSN;

     insn_ddiv:
      SAVE_PC;
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	ZEROCHECK (value2);
	jdouble res = value1 / value2;
	PUSHD (res);
      }
      NEXT_INSN;

     insn_irem:
      SAVE_PC;
      {
	jint value2 = POPI();
	jint value1 = POPI();
	ZEROCHECK (value2);	
	jint res = value1 % value2;
	PUSHI (res);
      }
      NEXT_INSN;

     insn_lrem:
      SAVE_PC;
      {
	jlong value2 = POPL();
	jlong value1 = POPL();
	ZEROCHECK (value2);
	jlong res = value1 % value2;
	PUSHL (res);
      }
      NEXT_INSN;

     insn_frem:
      SAVE_PC;
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	ZEROCHECK (value2);
	jfloat res    = __ieee754_fmod (value1, value2);
	PUSHF (res);
      }
      NEXT_INSN;

     insn_drem:
      SAVE_PC;
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	ZEROCHECK (value2);
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
	unsigned long value = POPI();
	PUSHI ((jint) (value >> shift));
      }
      NEXT_INSN;

     insn_lushr:
      {
	jint shift = (POPI() & 0x3f);
	UINT64 value = (UINT64) POPL();
	PUSHL ((value >> shift));
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
	jint index  = get1u (pc++);
	jint amount = get1s (pc++);
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
      { jint value = (jint)POPF (); PUSHI(value); }
      NEXT_INSN;

     insn_f2l:
      { jlong value = (jlong)POPF (); PUSHL(value); }
      NEXT_INSN;

     insn_f2d:
      { jdouble value = POPF (); PUSHD(value); }
      NEXT_INSN;

     insn_d2i:
      { jint value = (jint)POPD (); PUSHI(value); }
      NEXT_INSN;

     insn_d2l:
      { jlong value = (jlong)POPD (); PUSHL(value); }
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
     insn_fcmpg:
      {
	jfloat value2 = POPF ();
	jfloat value1 = POPF ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else if ((*(pc-1)) == op_fcmpg)
	  PUSHI (1);
	else
	  PUSHI (-1);
      }
      NEXT_INSN;

     insn_dcmpl:
     insn_dcmpg:
      {
	jdouble value2 = POPD ();
	jdouble value1 = POPD ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else if ((*(pc-1)) == op_dcmpg)
	  PUSHI (1);
	else
	  PUSHI (-1);
      }
      NEXT_INSN;

     insn_ifeq:
      {
	jint offset = get2s (pc); 
	if (POPI() == 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_ifne:
      {
	jint offset = get2s (pc); 
	if (POPI() != 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_iflt:
      {
	jint offset = get2s (pc); 
	if (POPI() < 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_ifge:
      {
	jint offset = get2s (pc); 
	if (POPI() >= 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_ifgt:
      {
	jint offset = get2s (pc); 
	if (POPI() > 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_ifle:
      {
	jint offset = get2s (pc); 
	if (POPI() <= 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmpeq:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 == value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmpne:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 != value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmplt:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 < value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmpge:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 >= value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmpgt:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 > value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_icmple:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 <= value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_acmpeq:
      {
	jint offset = get2s (pc); 
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 == value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_if_acmpne:
      {
	jint offset = get2s (pc); 
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 != value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      NEXT_INSN;

     insn_goto: 
      {
	jint offset = get2s (pc);
	pc = pc-1+offset;
      }
      NEXT_INSN;

     insn_jsr:
      {
	unsigned char *base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	PUSHA ((jobject)pc);
	pc = base_pc+offset;
      }
      NEXT_INSN;

     insn_ret:
      {
	jint index = get1u (pc);
	pc = (unsigned char*) PEEKA (index);
      }
      NEXT_INSN;

     insn_tableswitch:
      {
	unsigned char *base_pc = pc-1;
	int index = POPI();

	unsigned char* base = bytecode ();
	while ((pc-base) % 4 != 0)
	  pc++;

	jint def     = get4 (pc);
	jint low     = get4 (pc+4);
	jint high    = get4 (pc+8);

	if (index < low || index > high)
	  pc = base_pc + def;    
	else
	  pc = base_pc + get4 (pc+4*(index-low+3));
      }
      NEXT_INSN;

     insn_lookupswitch:
      {
	unsigned char *base_pc = pc-1;
	int index = POPI();

	unsigned char* base = bytecode ();
	while ((pc-base) % 4 != 0)
	  pc++;

	jint def     = get4 (pc);
	jint npairs  = get4 (pc+4);

	int max = npairs-1;
	int min = 0;

	// simple binary search...
	while (min < max)
	  {
	    int half = (min+max)/2;
	    int match = get4 (pc+ 4*(2 + 2*half));

	    if (index == match)
	      min = max = half;

	    else if (index < match)
	      max = half-1;

	    else
	      min = half+1;
	  }

	if (index == get4 (pc+ 4*(2 + 2*min)))
	  pc = base_pc + get4 (pc+ 4*(2 + 2*min + 1));
	else
	  pc = base_pc + def;    
      }
      NEXT_INSN;

      /* on return, just save the sp and return to caller */
     insn_ireturn:
     insn_lreturn:
     insn_freturn:
     insn_dreturn:
     insn_areturn:
     insn_return:
      inv->sp = sp;
      return;

     insn_getstatic:
      SAVE_PC;
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & Modifier::STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	jclass type = field->type;

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		PUSHI (*(jbyte*) (field->u.addr));

	      case 2:
		if (type == JvPrimClass (char))
		  PUSHI(*(jchar*) (field->u.addr));
		else
		  PUSHI(*(jshort*) (field->u.addr));
		break;

	      case 4:
		PUSHI(*(jint*) (field->u.addr));
		break;

	      case 8:
		PUSHL(*(jlong*) (field->u.addr));
		break;
	      }
	  }
	else
	  {
	    PUSHA(*(jobject*) (field->u.addr));
	  }
      }
      NEXT_INSN;

     insn_getfield:
      SAVE_PC;
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jclass type = field->type;
	jint field_offset = field->u.boffset;
	if (field_offset > 0xffff)
	  JvThrow (new java::lang::VirtualMachineError);

	jobject obj   = POPA();
	NULLCHECK(obj);

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		PUSHI (*(jbyte*) ((char*)obj + field_offset));
		break;

	      case 2:
		if (type == JvPrimClass (char))
		  PUSHI (*(jchar*) ((char*)obj + field_offset));
		else
		  PUSHI (*(jshort*) ((char*)obj + field_offset));
		break;

	      case 4:
		PUSHI (*(jint*) ((char*)obj + field_offset));
		break;

	      case 8:
		PUSHL(*(jlong*) ((char*)obj + field_offset));
		break;
	      }
	  }
	else
	  {
	    PUSHA(*(jobject*) ((char*)obj + field_offset));
	  }
      }
      NEXT_INSN;

     insn_putstatic:
      SAVE_PC;
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	// ResolvePoolEntry cannot check this
	if ((field->flags & Modifier::STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes) 
	      {
	      case 1:
		{
		  jint value = POPI();
		  *(jbyte*) (field->u.addr) = value;
		  break;
		}

	      case 2:
		{
		  jint value = POPI();
		  *(jchar*) (field->u.addr) = value;
		  break;
		}

	      case 4:
		{
		  jint value = POPI();
		  *(jint*) (field->u.addr) = value;
		  break;
		}

	      case 8:
		{
		  jlong value = POPL();
		  *(jlong*) (field->u.addr) = value;
		  break;
		}
	      }
	  }
	else
	  {
	    jobject value = POPA();
	    *(jobject*) (field->u.addr) = value;
	  }
      }
      NEXT_INSN;


     insn_putfield:
      SAVE_PC;
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jint field_offset = field->u.boffset;
	if (field_offset > 0xffff)
	  JvThrow (new java::lang::VirtualMachineError);

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
		  break;
		}

	      case 2:
		{
		  jint    value = POPI();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jchar*) ((char*)obj + field_offset) = value;
		  break;
		}

	      case 4:
		{
		  jint    value = POPI();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jint*) ((char*)obj + field_offset) = value;
		  break;
		}

	      case 8:
		{
		  jlong   value = POPL();
		  jobject obj   = POPA();
		  NULLCHECK(obj);
		  *(jlong*) ((char*)obj + field_offset) = value;
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
	  }
      }
      NEXT_INSN;

     insn_invokespecial:
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;

	NULLCHECK(sp[0]);

	fun = (void (*) (...))rmeth->method->ncode;
      }
      goto perform_invoke;

     insn_invokestatic:
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;

	_Jv_InitClass (rmeth->klass);
	fun = (void (*) (...))rmeth->method->ncode;
      }
      goto perform_invoke;

     insn_invokeinterface:
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;

	// invokeinterface has two unused bytes...
	pc += 2;

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;
	NULLCHECK(sp[0]);

	jobject rcv = sp[0].o;

	fun = (void (*) (...))
	  _Jv_LookupInterfaceMethod (rcv->getClass (),
				     rmeth->method->name,
				     rmeth->method->signature);
      }
      goto perform_invoke;


     insn_new:
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	_Jv_InitClass (klass);
	jobject res = _Jv_AllocObject (klass, klass->size_in_bytes);
	PUSHA (res);
      }
      NEXT_INSN;

     insn_newarray:
      SAVE_PC;
      {
	int atype = get1u (pc++);
	int size  = POPI();
	jobject result = _Jv_NewArray (atype, size);
	PUSHA (result);
      }
      NEXT_INSN;

     insn_anewarray:
      SAVE_PC;
      {
	int index = get2u (pc); pc += 2;
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	int size  = POPI();
	_Jv_InitClass (klass);
	jobject result = _Jv_NewObjectArray (size, klass, 0);
	PUSHA (result);
      }
      NEXT_INSN;

     insn_arraylength:
      SAVE_PC;
      {
	__JArray *arr = (__JArray*)POPA();
	PUSHI (arr->length);
      }
      NEXT_INSN;

     insn_athrow:
      SAVE_PC;
      {
	jobject value = POPA();
	JvThrow (value);
      }
      NEXT_INSN;

     insn_checkcast:
      SAVE_PC;
      {
	jobject value = POPA();
	jint index = get2u (pc); pc += 2;
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;

	if (value != NULL && ! to->isInstance (value))
	  {
	    JvThrow (new java::lang::ClassCastException
		     (to->getName()));
	  }

	PUSHA (value);
      }
      NEXT_INSN;

     insn_instanceof:
      SAVE_PC;
      {
	jobject value = POPA();
	jint index = get2u (pc); pc += 2;
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	PUSHI (to->isInstance (value));
      }
      NEXT_INSN;

     insn_monitorenter:
      SAVE_PC;
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorEnter (value);
      }
      NEXT_INSN;

     insn_monitorexit:
      SAVE_PC;
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorExit (value);
      }
      NEXT_INSN;

     insn_ifnull:
      {
	unsigned char* base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	jobject val = POPA();
	if (val == NULL)
	  pc = base_pc+offset;
      }
      NEXT_INSN;

     insn_ifnonnull:
      {
	unsigned char* base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	jobject val = POPA();
	if (val != NULL)
	  pc = base_pc+offset;
      }
      NEXT_INSN;

     insn_wide:
      SAVE_PC;
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

     insn_multianewarray:
      SAVE_PC;
      {
	int kind_index = get2u (pc); pc += 2;
	int dim        = get1u (pc); pc += 1;

	jclass type    
	  = (_Jv_ResolvePoolEntry (defining_class, kind_index)).clazz;
	_Jv_InitClass (type);
	jint *sizes    = (jint*) alloca (sizeof (jint)*dim);

	for (int i = dim - 1; i >= 0; i--)
	  {
	    sizes[i] = POPI ();
	  }

	jobject res    = _Jv_NewMultiArray (type,dim, sizes);

	PUSHA (res);
      }
      NEXT_INSN;

     insn_goto_w:
      {
	unsigned char* base_pc = pc-1;
	int offset = get4 (pc); pc += 4;
	pc = base_pc+offset;
      }
      NEXT_INSN;

     insn_jsr_w:
      {
	unsigned char* base_pc = pc-1;
	int offset = get4 (pc); pc += 4;
	PUSHA((jobject)pc);
	pc = base_pc+offset;
      }
      NEXT_INSN;
}


static void
throw_internal_error (char *msg)
{
  JvThrow (new java::lang::InternalError (JvNewStringLatin1 (msg)));
}

static void 
throw_incompatible_class_change_error (jstring msg)
{
  JvThrow (new java::lang::IncompatibleClassChangeError (msg));
}

#ifndef HANDLE_SEGV
static java::lang::NullPointerException *null_pointer_exc;
static void 
throw_null_pointer_exception ()
{
  if (null_pointer_exc == NULL)
    null_pointer_exc = new java::lang::NullPointerException;

  JvThrow (null_pointer_exc);
}
#endif

#ifndef HANDLE_FPE
static java::lang::ArithmeticException *arithmetic_exc;
static void 
throw_arithmetic_exception ()
{
  if (arithmetic_exc == NULL)
    arithmetic_exc = new java::lang::ArithmeticException
      (JvNewStringLatin1 ("/ by zero"));

  JvThrow (arithmetic_exc);
}
#endif


#endif // INTERPRETER

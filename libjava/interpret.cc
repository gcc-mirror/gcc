// interpret.cc - Code for the interpreter

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

/* define this to get instruction timings.  */
/* #define TIME_MAINLOOP  */

#include <config.h>

#pragma implementation "java-interp.h"

#include <cni.h>
#include <jvm.h>
#include <java-field.h>
#include <java-cpool.h>
#include <java-interp.h>
#include <java/lang/fdlibm.h>
#include <java/lang/System.h>
#include <java/lang/String.h>
#include <java/lang/Integer.h>
#include <java/lang/StringBuffer.h>
#include <java/io/PrintStream.h>
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
#ifdef TIME_MAINLOOP
#include <sys/time.h>
#include <stdio.h>
#endif

#ifndef INTERPRETER

#include <gnu/gcj/runtime/MethodInvocation.h>

/* This should never happen. */
void 
gnu::gcj::runtime::MethodInvocation::continue1 (gnu::gcj::RawData *,
						gnu::gcj::RawData *)
{
  JvFail ("no interpreter");
}

#else

#define ClassError _CL_Q34java4lang5Error
extern java::lang::Class ClassError;

static const int PUBLIC       = 0x001;
static const int PRIVATE      = 0x002;
static const int PROTECTED    = 0x004;
static const int STATIC       = 0x008;
static const int FINAL        = 0x010;
static const int SYNCHRONIZED = 0x020;
static const int VOLATILE     = 0x040;
static const int TRANSIENT    = 0x080;
static const int NATIVE       = 0x100;
static const int INTERFACE    = 0x200;
static const int ABSTRACT     = 0x400;
static const int ALL_FLAGS    = 0x7FF; 

static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);

static void throw_internal_error (char *msg)
  __attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
  __attribute__ ((__noreturn__));
#if !HANDLE_SEGV
static void throw_null_pointer_exception ()
  __attribute__ ((__noreturn__));
#endif
#if !HANDLE_FPE
static void throw_arithmetic_exception ()
  __attribute__ ((__noreturn__));
#endif


static inline void dupx (_Jv_word *&sp, int n, int x)
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
  
  // the net effect
  sp += n;
};


#define PUSHA(V)  \
 ({ jobject __v=(V); (sp++)->o = __v; })
#define PUSHI(V)  \
 ({ jint __v=(V); (sp++)->i = __v; })
#define PUSHF(V)  \
 ({ jfloat __v=(V); (sp++)->f = __v; })
#define PUSHL(V)  \
 ({ jlong __v=(V); _Jv_storeLong(sp,__v); sp+=2; })
#define PUSHD(V)  \
 ({ jdouble __v=(V); _Jv_storeDouble(sp,__v); sp+=2; })

#define POPA()    ((--sp)->o)
#define POPI()    ((jint) (--sp)->i) // cast since it may be promoted
#define POPF()    ((jfloat) (--sp)->f)
#define POPL()    ({ sp-=2; _Jv_loadLong (sp); })
#define POPD()    ({ sp-=2; _Jv_loadDouble (sp); })

#define LOADA(I)  (sp++)->o = locals[I].o
#define LOADI(I)  (sp++)->i = locals[I].i
#define LOADF(I)  (sp++)->f = locals[I].f
#define LOADL(I)  ({ jint __idx = (I); \
    (sp++)->ia[0] = locals[__idx].ia[0]; \
    (sp++)->ia[0] = locals[__idx+1].ia[0]; \
 })
#define LOADD(I)  LOADL(I)


#define STOREA(I) locals[I].o = (--sp)->o
#define STOREI(I) locals[I].i = (--sp)->i
#define STOREF(I) locals[I].f = (--sp)->f
#define STOREL(I) ({ jint __idx = (I); \
    locals[__idx+1].ia[0] = (--sp)->ia[0]; \
    locals[__idx].ia[0] = (--sp)->ia[0]; \
 })
#define STORED(I) STOREL(I)

#define PEEKI(I)  (locals+(I))->i
#define PEEKA(I)  (locals+(I))->o

#define POKEI(I,V)  (*(jint*) (locals+(I)) = (V))


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


#if HANDLE_SEGV
#define NULLCHECK(X) 
#else
#define NULLCHECK(X) \
  do { if ((X)==NULL) throw_null_pointer_exception (); } while (0)
#endif

#if HANDLE_FPE
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
     stack representation exactly.  At leat, that's the idea.
  */
  memcpy ((void*) locals, (void*) args, args_raw_size);

 next_segment:
  /* this will call the method _Jv_InterpMethod::continue0, see below */
  jobject ex = 
    gnu::gcj::runtime::MethodInvocation::continue0
    ((gnu::gcj::RawData *)this, (gnu::gcj::RawData *)inv);

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

  java::lang::System::out->println 
    (_Jv_NewStringUTF (self->name->data));

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

/* this is the exception handler hack, for the interpreter */
void 
gnu::gcj::runtime::MethodInvocation::continue1 (gnu::gcj::RawData *meth,
						gnu::gcj::RawData *inv)
{
  _Jv_InterpMethod           *meth0 = (_Jv_InterpMethod*)meth;
  _Jv_InterpMethodInvocation *inv0  = (_Jv_InterpMethodInvocation*)inv;
  meth0->continue1 (inv0);
}

/*
  This proceeds execution, as designated in "inv".  If an exception
  happens, then it is simply thrown, and handled in Java.  Thus, the pc
  needs to be stored in the invocation at all times, so we can figure
  out which handler (if any) to invoke.

  One design issue, which I have not completely considered, is if it
  should be possible to have interpreted classes linked in!  Seldom used
  (or non-critical) classes could reasonably be interpreted.  
*/


#ifdef TIME_MAINLOOP
static jlong insn_time [256] = { 0 };
static jlong insn_count[256] = { 0 };

static void
dump_time ()
{
  double total_all = 0;
  for (int i = 0; i < 256; i++)
    {
      total_all += insn_time[i];
    }

  for (int i = 0; i < 256; i++)
    {
      jlong total  = insn_time[i];
      jlong count  = insn_count[i];

      if (count == 0) continue;

      jlong amount = total/count;

      printf ("in 0x%02x: %7Li %7Li %7Li %2.1f%%\n", i,
	      (long long)count, (long long)total, (long long)amount,
	      (float) (100.0*(double)total/total_all)
	      );
    }
}
#endif
  
void _Jv_InterpMethod::continue1 (_Jv_InterpMethodInvocation *inv)
{
  /* for some reason, which I do not understand, the compiler on x86
   * allocates almost 4k stack space for this function!  Even though
   * there are many local variables, they are all nicely contained
   * within a block scope, except for the few declared right below
   * here.  What's going on??  It could well be, that there in fact is
   * on the order of 1000 local variables, including all those inlined
   * and expanded from macros...   Compiling with -O0, it allocates a
   * "modest" 300 bytes of stack space.   Among all those options of
   * gcc, why isn't there a -fpack-stack, allowing reuse of stack
   * locations?  */
  
  _Jv_word      *sp     = inv->sp;
  unsigned char *pc     = inv->pc;
  _Jv_word      *locals = inv->local_base ();
  int            opcode;

  jclass defining_class = this->defining_class;
  _Jv_word *pool_data   = defining_class->constants.data;
  
  /* these two are used in the invokeXXX instructions */
  void (*fun)(...);
  _Jv_ResolvedMethod* rmeth;

#ifdef TIME_MAINLOOP
  struct timeval tv;
  int   last_opcode;
  jlong last_time;
  static jlong time_warp = 0;

#define USEC(TV) \
   ((jlong) (TV).tv_sec * 1000000LL + (jlong)(TV).tv_usec)


  if (time_warp == 0) 
    {
      struct timeval tv2;

      gettimeofday (&tv, 0); 
      for (int i = 0; i < 100; i++)
	gettimeofday (&tv2, 0); 
      
      jlong then = USEC(tv); 
      jlong now = USEC(tv2);
      time_warp = (now - then) / 100;

      if (time_warp == 0)
	time_warp = 1;
    }    

#define TIME_SUSPEND do { \
  gettimeofday (&tv, 0); \
  jlong now = USEC(tv); \
  insn_time[last_opcode] += (now - last_time) - time_warp; \
} while(0)

#define TIME_RESUME do { \
  gettimeofday (&tv, 0); \
  last_time = USEC(tv); \
} while(0)

  last_opcode = 0; 
  gettimeofday (&tv, 0); 
  last_time = (jlong)tv.tv_sec * 1000000LL + (jlong)tv.tv_usec; 

#else

#define TIME_SUSPEND 
#define TIME_RESUME

#endif

 next_insn:
  inv->pc = pc;

#ifdef TIME_MAINLOOP

  gettimeofday (&tv, 0); 
  jlong now = USEC(tv); 
  insn_time[last_opcode] += (now - last_time) - time_warp; 
  last_time = now; 
  last_opcode = *pc; 
  insn_count[last_opcode] += 1;

#endif
  opcode = *pc++;

  /* we special-case the single opcode aload_0 -- it makes 
     up 10% of the time spent in the main loop. */

  switch (opcode)
    {
    case op_aload_0:		// 0x2a
      LOADA(0);
      goto next_insn;

    case op_iload:		// 0x15
      LOADI (get1u (pc++));
      goto next_insn;

    case op_getfield_4:		// 0xd8
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHI (*(jint*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_iload_1:		// 0x1b
      LOADI (1);
      goto next_insn;

    case op_getfield_a:		// 0xda
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHA(*(jobject*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_invokevirtual:	// 0xb6
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

	TIME_SUSPEND;
	ffi_raw_call (cif, fun, (void*)&rvalue, raw);
	TIME_RESUME;

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
      goto next_insn;


    case op_nop:
      goto next_insn;

    case op_aconst_null:
      PUSHA (NULL);
      goto next_insn;

    case op_iconst_m1:
    case op_iconst_0:
    case op_iconst_1:
    case op_iconst_2:
    case op_iconst_3:
    case op_iconst_4:
    case op_iconst_5:
      PUSHI (opcode-op_iconst_0);
      goto next_insn;

    case op_lconst_0:
    case op_lconst_1:
      PUSHL ((jlong) (opcode-op_lconst_0));
      goto next_insn;
      
    case op_fconst_0:
    case op_fconst_1:
    case op_fconst_2:
      PUSHF ((jfloat) (opcode-op_fconst_0));
      goto next_insn;

    case op_dconst_0:
    case op_dconst_1:
      PUSHD ((jdouble) (opcode-op_dconst_0));
      goto next_insn;

    case op_bipush:
      PUSHI (get1s(pc++));
      goto next_insn;
      
    case op_sipush:
      PUSHI (get2s(pc)); pc += 2;
      goto next_insn;

    case op_ldc:
      {
	int index = get1u (pc++);
	PUSHA(pool_data[index].o);
      }
      goto next_insn;

    case op_ldc_w:
      {
	int index = get2u (pc); pc += 2;
	PUSHA(pool_data[index].o);
      }
      goto next_insn;

    case op_ldc2_w:
      {
	int index = get2u (pc); pc += 2;
	memcpy (sp, &pool_data[index], 2*sizeof (_Jv_word));
	sp += 2;
      }
      goto next_insn;

    case op_lload:
      LOADL (get1u (pc++));
      goto next_insn;

    case op_fload:
      LOADF (get1u (pc++));
      goto next_insn;

    case op_dload:
      LOADD (get1u (pc++));
      goto next_insn;

    case op_aload:
      LOADA (get1u (pc++));
      goto next_insn;

    case op_iload_0:
      LOADI (0);
      goto next_insn;

    case op_iload_2:
      LOADI (2);
      goto next_insn;

    case op_iload_3:
      LOADI (3);
      goto next_insn;

    case op_lload_0:
    case op_lload_1:
    case op_lload_2:
    case op_lload_3:
      LOADL (opcode-op_lload_0);
      goto next_insn;

    case op_fload_0:
    case op_fload_1:
    case op_fload_2:
    case op_fload_3:
      LOADF (opcode-op_fload_0);
      goto next_insn;

    case op_dload_0:
    case op_dload_1:
    case op_dload_2:
    case op_dload_3:
      LOADD (opcode-op_dload_0);
      goto next_insn;

    case op_aload_1:
      LOADA(1);
      goto next_insn;

    case op_aload_2:
      LOADA(2);
      goto next_insn;

    case op_aload_3:
      LOADA(3);
      goto next_insn;

    case op_iaload:
      {
	jint index = POPI();
	jintArray arr = (jintArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      goto next_insn;

    case op_laload:
      {
	jint index = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHL( elements(arr)[index] );
      }
      goto next_insn;

    case op_faload:
      {
	jint index = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHF( elements(arr)[index] );
      }
      goto next_insn;

    case op_daload:
      {
	jint index = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHD( elements(arr)[index] );
      }
      goto next_insn;

    case op_aaload:
      {
	jint index = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHA( elements(arr)[index] );
      }
      goto next_insn;

    case op_baload:
      {
	jint index = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      goto next_insn;

    case op_caload:
      {
	jint index = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      goto next_insn;

    case op_saload:
      {
	jint index = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	PUSHI( elements(arr)[index] );
      }
      goto next_insn;

    case op_istore:
      STOREI (get1u (pc++));
      goto next_insn;

    case op_lstore:
      STOREL (get1u (pc++));
      goto next_insn;

    case op_fstore:
      STOREF (get1u (pc++));
      goto next_insn;

    case op_dstore:
      STORED (get1u (pc++));
      goto next_insn;

    case op_astore:
      STOREI (get1u (pc++));
      goto next_insn;

    case op_istore_0:
    case op_istore_1:
    case op_istore_2:
    case op_istore_3:
      STOREI (opcode-op_istore_0);
      goto next_insn;

    case op_lstore_0:
    case op_lstore_1:
    case op_lstore_2:
    case op_lstore_3:
      STOREL (opcode-op_lstore_0);
      goto next_insn;

    case op_fstore_0:
    case op_fstore_1:
    case op_fstore_2:
    case op_fstore_3:
      STOREF (opcode-op_fstore_0);
      goto next_insn;

    case op_dstore_0:
    case op_dstore_1:
    case op_dstore_2:
    case op_dstore_3:
      STORED (opcode-op_dstore_0);
      goto next_insn;

    case op_astore_0:
    case op_astore_1:
    case op_astore_2:
    case op_astore_3:
      STOREA (opcode-op_astore_0);
      goto next_insn;

    case op_iastore:
      {
	jint value = POPI();
	jint index  = POPI();
	jintArray arr = (jintArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_lastore:
      {
	jlong value = POPL();
	jint index  = POPI();
	jlongArray arr = (jlongArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_fastore:
      {
	jfloat value = POPF();
	jint index  = POPI();
	jfloatArray arr = (jfloatArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_dastore:
      {
	jdouble value = POPD();
	jint index  = POPI();
	jdoubleArray arr = (jdoubleArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_aastore:
      {
	jobject value = POPA();
	jint index  = POPI();
	jobjectArray arr = (jobjectArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	_Jv_CheckArrayStore (arr, value);
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_bastore:
      {
	jbyte value = (jbyte) POPI();
	jint index  = POPI();
	jbyteArray arr = (jbyteArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_castore:
      {
	jchar value = (jchar) POPI();
	jint index  = POPI();
	jcharArray arr = (jcharArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_sastore:
      {
	jshort value = (jshort) POPI();
	jint index  = POPI();
	jshortArray arr = (jshortArray) POPA();
	NULLCHECK (arr);
	if (index < 0 || index >= arr->length)
 	  {
	    TIME_SUSPEND;
	    _Jv_ThrowBadArrayIndex (index);
	  }
	elements(arr)[index] = value;
      }
      goto next_insn;

    case op_pop:
      sp -= 1;
      goto next_insn;

    case op_pop2:
      sp -= 2;
      goto next_insn;

    case op_dup:
      sp[0] = sp[-1];
      sp += 1;
      goto next_insn;

    case op_dup_x1:
      dupx (sp, 1, 1);
      goto next_insn;

    case op_dup_x2:
      dupx (sp, 1, 2);
      goto next_insn;

    case op_dup2:
      sp[0] = sp[-2];
      sp[1] = sp[-1];
      sp += 2;
      goto next_insn;

    case op_dup2_x1:
      dupx (sp, 2, 1);
      goto next_insn;

    case op_dup2_x2:
      dupx (sp, 2, 2);
      goto next_insn;

    case op_swap:
      {
	jobject tmp1 = POPA();
	jobject tmp2 = POPA();
	PUSHA (tmp1);
	PUSHA (tmp2);
      }
      goto next_insn;

    case op_iadd:
      BINOPI(+);
      goto next_insn;

    case op_ladd:
      BINOPL(+);
      goto next_insn;

    case op_fadd:
      BINOPF(+);
      goto next_insn;

    case op_dadd:
      BINOPD(+);
      goto next_insn;

    case op_isub:
      BINOPI(-);
      goto next_insn;

    case op_lsub:
      BINOPL(-);
      goto next_insn;

    case op_fsub:
      BINOPF(-);
      goto next_insn;

    case op_dsub:
      BINOPD(-);
      goto next_insn;

    case op_imul:
      BINOPI(*);
      goto next_insn;

    case op_lmul:
      BINOPL(*);
      goto next_insn;

    case op_fmul:
      BINOPF(*);
      goto next_insn;

    case op_dmul:
      BINOPD(*);
      goto next_insn;

    case op_idiv:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	ZEROCHECK (value2);
	jint res = value1 / value2;
	PUSHI (res);
      }
      goto next_insn;

    case op_ldiv:
       {
	jlong value2 = POPL();
	jlong value1 = POPL();
	ZEROCHECK (value2);
	jlong res = value1 / value2;
	PUSHL (res);
      }
      goto next_insn;

    case op_fdiv:
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	ZEROCHECK (value2);
	jfloat res = value1 / value2;
	PUSHF (res);
      }
      goto next_insn;

    case op_ddiv:
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	ZEROCHECK (value2);
	jdouble res = value1 / value2;
	PUSHD (res);
      }
      goto next_insn;

    case op_irem:
      {
	jint value2 = POPI();
	jint value1 = POPI();
	ZEROCHECK (value2);	
	jint res = value1 % value2;
	PUSHI (res);
      }
      goto next_insn;

    case op_lrem:
       {
	jlong value2 = POPL();
	jlong value1 = POPL();
	ZEROCHECK (value2);
	jlong res = value1 % value2;
	PUSHL (res);
      }
      goto next_insn;

    case op_frem:
      {
	jfloat value2 = POPF();
	jfloat value1 = POPF();
	ZEROCHECK (value2);
	jfloat res    = __ieee754_fmod (value1, value2);
	PUSHF (res);
      }
      goto next_insn;

    case op_drem:
      {
	jdouble value2 = POPD();
	jdouble value1 = POPD();
	ZEROCHECK (value2);
	jdouble res    = __ieee754_fmod (value1, value2);
	PUSHD (res);
      }
      goto next_insn;

    case op_ineg:
      *(jint*) (sp-1) *= -1;
      goto next_insn;

    case op_lneg:
      *(jlong*) (sp-1) *= -1;
      goto next_insn;

    case op_fneg:
      *(jfloat*) (sp-1) *= -1;
      goto next_insn;

    case op_dneg:
      *(jdouble*) (sp-1) *= -1;
      goto next_insn;

    case op_ishl:
      {
	jint shift = (POPI() & 0x1f);
	jint value = POPI();
	PUSHI (value << shift);
      }
      goto next_insn;

    case op_lshl:
      {
	jint shift = (POPI() & 0x3f);
	jlong value = POPL();
	PUSHL (value << shift);
      }
      goto next_insn;

    case op_ishr:
      {
	jint shift = (POPI() & 0x1f);
	jint value = POPI();
	PUSHI (value >> shift);
      }
      goto next_insn;

    case op_lshr:
      {
	jint shift = (POPI() & 0x3f);
	jlong value = POPL();
	PUSHL (value >> shift);
      }
      goto next_insn;

    case op_iushr:
      {
	jint shift = (POPI() & 0x1f);
	unsigned long value = POPI();
	PUSHI ((jint) (value >> shift));
      }
      goto next_insn;

    case op_lushr:
      {
	jint shift = (POPI() & 0x3f);
	UINT64 value = (UINT64) POPL();
	PUSHL ((value >> shift));
      }
      goto next_insn;

    case op_iand:
      BINOPI (&);
      goto next_insn;

    case op_land:
      BINOPL (&);
      goto next_insn;

    case op_ior:
      BINOPI (|);
      goto next_insn;

    case op_lor:
      BINOPL (|);
      goto next_insn;

    case op_ixor:
      BINOPI (^);
      goto next_insn;

    case op_lxor:
      BINOPL (^);
      goto next_insn;

    case op_iinc:
      {
	jint index  = get1u (pc++);
	jint amount = get1s (pc++);
	*(jint*) (locals + index) += amount;
      }
      goto next_insn;

    case op_i2l:
      PUSHL ((jlong)POPI ());
      goto next_insn;

    case op_i2f:
      PUSHF ((jfloat)POPI ());
      goto next_insn;

    case op_i2d:
      PUSHD ((jdouble)POPI ());
      goto next_insn;

    case op_l2i:
      PUSHI ((jint)POPL ());
      goto next_insn;

    case op_l2f:
      PUSHF ((jfloat)POPL ());
      goto next_insn;

    case op_l2d:
      PUSHD ((jdouble)POPL ());
      goto next_insn;

    case op_f2i:
      PUSHI ((jint)POPF ());
      goto next_insn;

    case op_f2l:
      PUSHL ((jlong)POPF ());
      goto next_insn;

    case op_f2d:
      PUSHD ((jdouble)POPF ());
      goto next_insn;

    case op_d2i:
      PUSHI ((jint)POPD ());
      goto next_insn;

    case op_d2l:
      PUSHL ((jlong)POPD ());
      goto next_insn;

    case op_d2f:
      PUSHF ((jfloat)POPD ());
      goto next_insn;

    case op_i2b:
      PUSHI ((jbyte)POPI ());
      goto next_insn;

    case op_i2c:
      PUSHI ((jchar)POPI ());
      goto next_insn;

    case op_i2s:
      PUSHI ((jshort)POPI ());
      goto next_insn;

    case op_lcmp:
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
      goto next_insn;

    case op_fcmpl:
    case op_fcmpg:
      {
	jfloat value2 = POPF ();
	jfloat value1 = POPF ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else if (opcode == op_fcmpg)
	  PUSHI (1);
	else
	  PUSHI (-1);
      }
      goto next_insn;

    case op_dcmpl:
    case op_dcmpg:
      {
	jdouble value2 = POPD ();
	jdouble value1 = POPD ();
	if (value1 > value2)
	  PUSHI (1);
	else if (value1 == value2)
	  PUSHI (0);
	else if (value1 < value2)
	  PUSHI (-1);
	else if (opcode == op_dcmpg)
	  PUSHI (1);
	else
	  PUSHI (-1);
      }
      goto next_insn;

    case op_ifeq:
      {
	jint offset = get2s (pc); 
	if (POPI() == 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_ifne:
      {
	jint offset = get2s (pc); 
	if (POPI() != 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_iflt:
      {
	jint offset = get2s (pc); 
	if (POPI() < 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_ifge:
      {
	jint offset = get2s (pc); 
	if (POPI() >= 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_ifgt:
      {
	jint offset = get2s (pc); 
	if (POPI() > 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_ifle:
      {
	jint offset = get2s (pc); 
	if (POPI() <= 0)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmpeq:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 == value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmpne:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 != value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmplt:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 < value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmpge:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 >= value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmpgt:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 > value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_icmple:
      {
	jint offset = get2s (pc); 
	jint value2 = POPI();
	jint value1 = POPI();
	if (value1 <= value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_acmpeq:
      {
	jint offset = get2s (pc); 
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 == value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_if_acmpne:
      {
	jint offset = get2s (pc); 
	jobject value2 = POPA();
	jobject value1 = POPA();
	if (value1 != value2)
	  pc = pc-1+offset;
	else
	  pc = pc+2;
      }
      goto next_insn;

    case op_goto: 
      {
	jint offset = get2s (pc);
	pc = pc-1+offset;
      }
      goto next_insn;

    case op_jsr:
      {
	unsigned char *base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	PUSHA ((jobject)pc);
	pc = base_pc+offset;
      }
      goto next_insn;

    case op_ret:
      {
	jint index = get1u (pc);
	pc = (unsigned char*) PEEKA (index);
      }
      goto next_insn;

    case op_tableswitch:
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
      goto next_insn;

    case op_lookupswitch:
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
      goto next_insn;

      /* on return, just save the sp and return to caller */
    case op_ireturn:
    case op_lreturn:
    case op_freturn:
    case op_dreturn:
    case op_areturn:
    case op_return:
      inv->sp = sp;
      TIME_SUSPEND;
      return;

    case op_getstatic:
      {
	unsigned char *base_pc = pc-1;
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	jclass type = field->type;

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		*base_pc = op_getstatic_1;
		break;
	    
	      case 2:
		if (type == JvPrimClass (char))
		  *base_pc = op_getstatic_2u;
		else
		  *base_pc = op_getstatic_2s;
		break;

	      case 4:
		*base_pc = op_getstatic_4;
		break;

	      case 8:
		*base_pc = op_getstatic_8;
		break;
	      }
	  }
	else
	  {
	    *base_pc = op_getstatic_a;
	  }
	
	pc = base_pc;
      }
      goto next_insn;

    case op_getfield:
      {
	unsigned char *base_pc = pc-1;
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jclass type = field->type;

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
		*base_pc = op_getfield_1;
		break;
	    
	      case 2:
		if (type == JvPrimClass (char))
		  *base_pc = op_getfield_2u;
		else
		  *base_pc = op_getfield_2s;
		break;

	      case 4:
		*base_pc = op_getfield_4;
		break;

	      case 8:
		*base_pc = op_getfield_8;
		break;
	      }
	  }
	else
	  {
	    *base_pc = op_getfield_a;
	  }
	
	if (field->u.boffset > 0xffff)
	  JvThrow (new java::lang::VirtualMachineError);

	base_pc[1] = (field->u.boffset>>8) & 0xff;
	base_pc[2] = field->u.boffset & 0xff;

	pc = base_pc;
      }
      goto next_insn;

    case op_putstatic:
      {
	unsigned char* base_pc = pc-1;
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	// ResolvePoolEntry cannot check this
	if ((field->flags & STATIC) == 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field no longer static"));

	/* if this is patented, then maybe we could install
	   a function in the constant pool, to do the right thing */

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes) 
	      {
	      case 1:
		*base_pc = op_putstatic_1;
		break;

	      case 2:
		*base_pc = op_putstatic_2;
		break;
		
	      case 4:
		*base_pc = op_putstatic_4;
		break;
		
	      case 8:
		*base_pc = op_putstatic_8;
		break;
	      }
	  }
	else
	  {
	    *base_pc = op_putstatic_a;
	  }

	// do the instruction again!
	pc = base_pc;
      }
      goto next_insn;


    case op_putfield:
      {
	unsigned char* base_pc = pc-1;
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_ResolvePoolEntry (defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	if ((field->flags & STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes) 
	      {
	      case 1:
		*base_pc = op_putfield_1;
		break;

	      case 2:
		*base_pc = op_putfield_2;
		break;

	      case 4:
		*base_pc = op_putfield_4;
		break;

	      case 8:
		*base_pc = op_putfield_8;
		break;
	      }
	  }
	else
	  {
	    *base_pc = op_putfield_a;
	  }

	if (field->u.boffset > 0xffff)
	  JvThrow (new java::lang::VirtualMachineError);

	base_pc[1] = (field->u.boffset>>8) & 0xff;
	base_pc[2] = field->u.boffset & 0xff;

	// do the instruction again!
	pc = base_pc;
      }
      goto next_insn;


    case op_getfield_1:
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHI (*(jbyte*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_getfield_2s:
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHI (*(jshort*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_getfield_2u:
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHI (*(jchar*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_getfield_8:
      {
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	PUSHL(*(jlong*) ((char*)obj + field_offset));
      }
      goto next_insn;

    case op_getstatic_1:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHI (*(jbyte*) (field->u.addr));
      }
      goto next_insn;

    case op_getstatic_2s:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHI(*(jshort*) (field->u.addr));
      }
      goto next_insn;

    case op_getstatic_2u:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHI(*(jchar*) (field->u.addr));
      }
      goto next_insn;

    case op_getstatic_4:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHI(*(jint*) (field->u.addr));
      }
      goto next_insn;

    case op_getstatic_8:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHL(*(jlong*) (field->u.addr));
      }
      goto next_insn;

    case op_getstatic_a:
      {
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	PUSHA(*(jobject*) (field->u.addr));
      }
      goto next_insn;

    case op_putfield_1:
      {
	jint    value = POPI();
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	*(jbyte*) ((char*)obj + field_offset) = value;
      }
      goto next_insn;

    case op_putfield_2:
      {
	jint    value = POPI();
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	*(jchar*) ((char*)obj + field_offset) = value;
      }
      goto next_insn;

    case op_putfield_4:
      {
	jint    value = POPI();
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	*(jint*) ((char*)obj + field_offset) = value;
      }
      goto next_insn;

    case op_putfield_8:
      {
	jlong   value = POPL();
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	*(jlong*) ((char*)obj + field_offset) = value;
      }
      goto next_insn;

    case op_putfield_a:
      {
	jobject value = POPA();
	jobject obj   = POPA();
	NULLCHECK(obj);
	jint field_offset = get2u (pc); pc += 2;
	*(jobject*) ((char*)obj + field_offset) = value;
      }
      goto next_insn;

    case op_putstatic_1:
      {
	jint    value = POPI();
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	*(jbyte*) (field->u.addr) = value;
      }
      goto next_insn;

    case op_putstatic_2:
      {
	jint    value = POPI();
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	*(jchar*) (field->u.addr) = value;
      }
      goto next_insn;

    case op_putstatic_4:
      {
	jint    value = POPI();
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	*(jint*) (field->u.addr) = value;
      }
      goto next_insn;

    case op_putstatic_8:
      {
	jlong    value = POPL();
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	*(jlong*) (field->u.addr) = value;
      }
      goto next_insn;

    case op_putstatic_a:
      {
	jobject value = POPA();
	jint fieldref_index = get2u (pc); pc += 2;
	_Jv_Field *field = pool_data[fieldref_index].field;
	*(jobject*) (field->u.addr) = value;
      }
      goto next_insn;

    case op_invokespecial:
      {
	int index = get2u (pc); pc += 2;

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;
	
	NULLCHECK(sp[0]);

	fun = (void (*) (...))rmeth->method->ncode;
      }
      goto perform_invoke;

    case op_invokestatic:
      {
	int index = get2u (pc); pc += 2;

	rmeth = (_Jv_ResolvePoolEntry (defining_class, index)).rmethod;

	sp -= rmeth->stack_item_count;
	
	_Jv_InitClass (rmeth->klass);
	fun = (void (*) (...))rmeth->method->ncode;
      }
      goto perform_invoke;

    case op_invokeinterface:
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


    case op_new:
      {
	int index = get2u (pc); pc += 2;
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	_Jv_InitClass (klass);
	jobject res = _Jv_AllocObject (klass, klass->size_in_bytes);
	PUSHA (res);
      }
      goto next_insn;

    case op_newarray:
      {
	int atype = get1u (pc++);
	int size  = POPI();
	jobject result = _Jv_NewArray (atype, size);
	PUSHA (result);
      }
      goto next_insn;
      
    case op_anewarray:
      {
	int index = get2u (pc); pc += 2;
	jclass klass = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	int size  = POPI();
	_Jv_InitClass (klass);
	jobject result = _Jv_NewObjectArray (size, klass, 0);
	PUSHA (result);
      }
      goto next_insn;

    case op_arraylength:
      {
	__JArray *arr = (__JArray*)POPA();
	PUSHI (arr->length);
      }
      goto next_insn;

    case op_athrow:
      {
	jobject value = POPA();
	TIME_SUSPEND;
	JvThrow (value);
      }
      goto next_insn;

    case op_checkcast:
      {
	jobject value = POPA();
	jint index = get2u (pc); pc += 2;
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;

	if (value != NULL && ! to->isInstance (value))
	  {
	    TIME_SUSPEND;
	    JvThrow (new java::lang::ClassCastException
		     (to->getName()));
	  }

	PUSHA (value);
      }
      goto next_insn;

    case op_instanceof:
      {
	jobject value = POPA();
	jint index = get2u (pc); pc += 2;
	jclass to = (_Jv_ResolvePoolEntry (defining_class, index)).clazz;
	PUSHI (to->isInstance (value));
      }
      goto next_insn;

    case op_monitorenter:
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorEnter (value);
      }
      goto next_insn;

    case op_monitorexit:
      {
	jobject value = POPA();
	NULLCHECK(value);
	_Jv_MonitorExit (value);
      }
      goto next_insn;

    case op_ifnull:
      {
	unsigned char* base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	jobject val = POPA();
	if (val == NULL)
	  pc = base_pc+offset;
      }
      goto next_insn;

    case op_ifnonnull:
      {
	unsigned char* base_pc = pc-1;
	jint offset = get2s (pc); pc += 2;
	jobject val = POPA();
	if (val != NULL)
	  pc = base_pc+offset;
      }
      goto next_insn;

    case op_wide:
      {
	jint the_mod_op = get1u (pc++);
	jint wide       = get2u (pc); pc += 2;

	switch (the_mod_op)
	  {
	  case op_istore:
	    STOREI (wide);
	    goto next_insn;

	  case op_fstore:
	    STOREF (wide);
	    goto next_insn;

	  case op_astore:
	    STOREA (wide);
	    goto next_insn;

	  case op_lload:
	    LOADL (wide);
	    goto next_insn;

	  case op_dload:
	    LOADD (wide);
	    goto next_insn;

	  case op_iload:
	    LOADI (wide);
	    goto next_insn;

	  case op_aload:
	    LOADA (wide);
	    goto next_insn;

	  case op_lstore:
	    STOREL (wide);
	    goto next_insn;

	  case op_dstore:
	    STORED (wide);
	    goto next_insn;

	  case op_ret:
	    pc = (unsigned char*) PEEKA (wide);
	    goto next_insn;

	  case op_iinc:
	    {
	      jint amount = get2s (pc); pc += 2;
	      jint value = PEEKI (wide);
	      POKEI (wide, value+amount);
	    }
	    goto next_insn;

	  default:
	    throw_internal_error ("illegal bytecode modified by wide");
	  }

      }

    case op_multianewarray:
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
      goto next_insn;

    case op_goto_w:
      {
	unsigned char* base_pc = pc-1;
	int offset = get4 (pc); pc += 4;
	pc = base_pc+offset;
      }
      goto next_insn;

    case op_jsr_w:
      {
	unsigned char* base_pc = pc-1;
	int offset = get4 (pc); pc += 4;
	PUSHA((jobject)pc);
	pc = base_pc+offset;
      }
      goto next_insn;

    default:
      throw_internal_error ("opcode not implemented");

    }
  goto next_insn;
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

#if !HANDLE_SEGV
static java::lang::NullPointerException *null_pointer_exc;
static void 
throw_null_pointer_exception ()
{
  if (null_pointer_exc == NULL)
    null_pointer_exc = new java::lang::NullPointerException;

  JvThrow (null_pointer_exc);
}
#endif

#if !HANDLE_FPE
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

void
jvdump(jobject o)
{
  _Jv_InterpMethod::dump_object(o);
}

/* FIXME: This is not finished! */
void
_Jv_InterpMethod::dump_object(jobject o)
{
  java::io::PrintStream *out = java::lang::System::out;

  if (o == NULL)
    {
      out->println (JvNewStringLatin1 ("<null>"));
      return;
    }

  jclass klass = o->getClass ();

  out->print (klass->getName ());
  out->print (JvNewStringLatin1 ("@0x"));
  out->print (java::lang::Integer::toHexString ((jint)o));
  out->print (JvNewStringLatin1 ("{"));
#if 0
  while (klass && klass != &ObjectClass)
    {
      _Jv_Field *fields = klass->fields;
      int max           = klass->field_count;
      
      for (int i = 0; i < max; ++i)
	{
	  out->print (_Jv_NewStringUTF (field->name->data));
	  out->print (JvNewStringLatin1 ("="));

	  if (JvFieldIsRef (field))
	    {
	      if (field->flags & STATIC)
		out->print (JvGetSt)
	    }
	  field = field->getNextInstanceField ();

	  if (i+1 < max && klass->getSuperclass () != null)
	    out->print (JvNewStringLatin1 ("; "));
	}

      klass = klass->getSuperclass();
    }
#endif
  out->print (JvNewStringLatin1 ("}\n"));

}

#endif // INTERPRETER

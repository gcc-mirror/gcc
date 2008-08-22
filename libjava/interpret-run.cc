// interpret-run.cc - Code to interpret bytecode

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* This file is meant only to be included in interpret.cc, it should not be
 * compiled directly.	*/

  using namespace java::lang::reflect;
  
  pc_t pc = NULL;

  // FRAME_DESC registers this particular invocation as the top-most
  // interpreter frame.  This lets the stack tracing code (for
  // Throwable) print information about the method being interpreted
  // rather than about the interpreter itself.  FRAME_DESC has a
  // destructor so it cleans up automatically when the interpreter
  // returns.
  java::lang::Thread *thread = java::lang::Thread::currentThread();
  
#ifdef DEBUG
  _Jv_InterpFrame frame_desc (meth, thread, NULL, &pc);
#else
  _Jv_InterpFrame frame_desc (meth, thread);
#endif

  _Jv_word stack[meth->max_stack];
  _Jv_word *sp = stack;

  _Jv_word locals[meth->max_locals];

#ifdef DEBUG
  // This is the information needed to get and set local variables with
  // proper type checking.
  frame_desc.locals = locals;
  char locals_type[meth->max_locals];
  frame_desc.locals_type = locals_type;
  
  // Set all slots as invalid until they are written to.
  memset (locals_type, 'x', meth->max_locals);
  
  // We need to set the local variable types for the method arguments since
  // they are valid at invocation.
  
  _Jv_Method *method = meth->get_method ();
  int type_ctr = 0;
  
  // If the method is non-static, we need to set the type for the "this" pointer.
  if ((method->accflags & java::lang::reflect::Modifier::STATIC) == 0)
    {
      if (args)
        {
          // Set the "this" pointer for this frame.
          _Jv_word *this_ptr = reinterpret_cast<_Jv_word *> (args);
          frame_desc.obj_ptr = this_ptr[0].o;
        }

      frame_desc.locals_type[0] = 'o';
      type_ctr++;
    }
  
  // Now parse the method signature to set the types of the other arguments.  
  int sig_len = method->signature->len ();
  char *signature = method->signature->chars ();
  for (int i = 1; signature[i] != ')' && i <= sig_len; i++)
    {
      if (signature[i] == 'Z' || signature[i] == 'B' || signature[i] == 'C' 
          || signature[i] == 'S' || signature[i] == 'I')
        {
          frame_desc.locals_type[type_ctr] = 'i';
          type_ctr++;
          continue;
        }
      else if (signature[i] == 'F')
        {
          frame_desc.locals_type[type_ctr] = 'f';
          type_ctr++;
          continue;
        }
      else if (signature[i] == 'J')
        {
          frame_desc.locals_type[type_ctr] = 'l';
          frame_desc.locals_type[type_ctr+1] = 'x';
          type_ctr += 2;
          continue;
        }
      else if (signature[i] == 'D')
        {
          frame_desc.locals_type[type_ctr] = 'd';
          frame_desc.locals_type[type_ctr+1] = 'x';
          type_ctr += 2;
          continue;
        }
      else if (signature[i] == 'L')
        {
          frame_desc.locals_type[type_ctr] = 'o';
          type_ctr++;
          while (signature[i] != ';')
            i++;
          continue;
        }
      else if (signature[i] == '[')
        {
          frame_desc.locals_type[type_ctr] = 'o';
          type_ctr++;
          
          // Ignore multi-dimensional arrays.
          while (signature[i] == '[')
            i++;
          
          // Check for an object array
          if (signature[i] == 'L')
            {
              while (signature[i] != ';')
                i++;
            }
          continue;
        }
    }
#endif /* DEBUG */

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
    INSN_LABEL(breakpoint),
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
#ifdef DIRECT_THREADED
    INSN_LABEL (ldc_class)
#else
    0
#endif
  };

#ifdef DIRECT_THREADED

#ifdef DEBUG
#undef NEXT_INSN
#define NEXT_INSN							\
  do									\
    {									\
      pc_t insn = pc++;							\
      if (JVMTI_REQUESTED_EVENT (SingleStep))				\
	{								\
	  JNIEnv *env = _Jv_GetCurrentJNIEnv ();			\
	  jmethodID method = meth->self;				\
	  jlocation loc = meth->insn_index (insn);			\
	  _Jv_JVMTI_PostEvent (JVMTI_EVENT_SINGLE_STEP, thread,		\
			       env, method, loc);			\
	}								\
      goto *(insn->insn);						\
    }									\
  while (0)

#undef REWRITE_INSN
#define REWRITE_INSN(INSN,SLOT,VALUE)					\
  do {									\
    if (pc[-2].insn == breakpoint_insn->insn)				\
      {									\
	using namespace ::gnu::gcj::jvmti;				\
	jlocation location = meth->insn_index (pc - 2);			\
	_Jv_RewriteBreakpointInsn (meth->self, location, (pc_t) INSN);	\
      }									\
    else								\
      pc[-2].insn = INSN;						\
									\
    pc[-1].SLOT = VALUE;						\
  }									\
  while (0)

#undef INTERP_REPORT_EXCEPTION
#define INTERP_REPORT_EXCEPTION(Jthrowable) REPORT_EXCEPTION (Jthrowable)
#else // !DEBUG
#undef NEXT_INSN
#define NEXT_INSN goto *((pc++)->insn)

// REWRITE_INSN does nothing.
//
// Rewriting a multi-word instruction in the presence of multiple
// threads leads to a data race if a thread reads part of an
// instruction while some other thread is rewriting that instruction.
// For example, an invokespecial instruction may be rewritten to
// invokespecial_resolved and its operand changed from an index to a
// pointer while another thread is executing invokespecial.  This
// other thread then reads the pointer that is now the operand of
// invokespecial_resolved and tries to use it as an index.
//
// Fixing this requires either spinlocks, a more elaborate data
// structure, or even per-thread allocated pages.  It's clear from the
// locking in meth->compile below that the presence of multiple
// threads was contemplated when this code was written, but the full
// consequences were not fully appreciated.
#define REWRITE_INSN(INSN,SLOT,VALUE)

#undef INTERP_REPORT_EXCEPTION
#define INTERP_REPORT_EXCEPTION(Jthrowable) /* not needed when not debugging */
#endif // !DEBUG

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
  if (meth->prepared == NULL)
    {
      _Jv_MutexLock (&compile_mutex);
      if (meth->prepared == NULL)
	meth->compile (insn_target);
      _Jv_MutexUnlock (&compile_mutex);
    }

  // If we're only compiling, stop here
  if (args == NULL)
    return;

  pc = (insn_slot *) meth->prepared;

#else

#ifdef DEBUG
#define NEXT_INSN							\
  do									\
    {									\
      if (JVMTI_REQUESTED_EVENT (SingleStep))				\
	{								\
	  JNIEnv *env = _Jv_GetCurrentJNIEnv ();			\
	  jmethodID method = meth->self;				\
	  jlocation loc = meth->insn_index (pc);			\
	  _Jv_JVMTI_PostEvent (JVMTI_EVENT_SINGLE_STEP, thread,		\
			       env, method, loc);			\
	}								\
      goto *(insn_target[*pc++])
#else
#define NEXT_INSN goto *(insn_target[*pc++])
#endif

#define GET1S() get1s (pc++)
#define GET2S() (pc += 2, get2s (pc- 2))
#define GET1U() get1u (pc++)
#define GET2U() (pc += 2, get2u (pc - 2))
  // Note that these could be more efficient when not handling 'ldc
  // class'.
#define AVAL1U()						\
  ({ int index = get1u (pc++);					\
    _Jv_Linker::resolve_pool_entry (meth->defining_class, index).o; })
#define AVAL2U()						\
  ({ int index = get2u (pc); pc += 2;				\
    _Jv_Linker::resolve_pool_entry (meth->defining_class, index).o; })
  // Note that we don't need to resolve the pool entry here as class
  // constants are never wide.
#define AVAL2UP() ({ int index = get2u (pc); pc += 2; &pool_data[index]; })
#define SKIP_GOTO pc += 2
#define GOTO_VAL() pc - 1 + get2s (pc)
#define PCVAL(unionval) unionval.i
#define AMPAMP(label) NULL

  pc = meth->bytecode ();

#endif /* DIRECT_THREADED */

#define TAKE_GOTO pc = GOTO_VAL ()

  /* Go straight at it!  the ffi raw format matches the internal
     stack representation exactly.  At least, that's the idea.
  */
  memcpy ((void*) locals, (void*) args, meth->args_raw_size);

  _Jv_word *pool_data = meth->defining_class->constants.data;

  /* These three are temporaries for common code used by several
     instructions.  */
  void (*fun)();
  _Jv_ResolvedMethod* rmeth;
  int tmpval;

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
	SAVE_PC();
	int index = GET2U ();

	/* _Jv_Linker::resolve_pool_entry returns immediately if the
	 * value already is resolved.  If we want to clutter up the
	 * code here to gain a little performance, then we can check
	 * the corresponding bit JV_CONSTANT_ResolvedFlag in the tag
	 * directly.  For now, I don't think it is worth it.  */

	rmeth = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						   index)).rmethod;

	sp -= rmeth->stack_item_count;

	if (rmeth->method->accflags & Modifier::FINAL)
	  {
	    // We can't rely on NULLCHECK working if the method is final.
	    if (! sp[0].o)
	      throw_null_pointer_exception ();

	    // Final methods might not appear in the vtable.
	    fun = (void (*)()) rmeth->method->ncode;
	  }
	else
	  {
	    NULLCHECK (sp[0].o);
	    jobject rcv = sp[0].o;
	    _Jv_VTable *table = *(_Jv_VTable**) rcv;
	    fun = (void (*)()) table->get_method (rmeth->method->index);
	  }

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	REWRITE_INSN (&&invokevirtual_resolved, datum, rmeth);
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokevirtual_resolved:
      {
	SAVE_PC();
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;

	if (rmeth->method->accflags & Modifier::FINAL)
	  {
	    // We can't rely on NULLCHECK working if the method is final.
	    if (! sp[0].o)
	      throw_null_pointer_exception ();

	    // Final methods might not appear in the vtable.
	    fun = (void (*)()) rmeth->method->ncode;
	  }
	else
	  {
	    NULLCHECK (sp[0].o);
	    jobject rcv = sp[0].o;
	    _Jv_VTable *table = *(_Jv_VTable**) rcv;
	    fun = (void (*)()) table->get_method (rmeth->method->index);
	  }
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    perform_invoke:
      {
	/* here goes the magic again... */
	ffi_cif *cif = &rmeth->cif;
	INTERP_FFI_RAW_TYPE *raw = (INTERP_FFI_RAW_TYPE *) sp;

	_Jv_value rvalue;

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
	    PUSHA (rvalue.object_value);
	  }
	else if (rtype == FFI_TYPE_SINT32)
	  {
	    PUSHI (rvalue.int_value);
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
		PUSHI ((jbyte)(rvalue.int_value & 0xff));
		break;

	      case FFI_TYPE_SINT16:
		PUSHI ((jshort)(rvalue.int_value & 0xffff));
		break;

	      case FFI_TYPE_UINT16:
		PUSHI (rvalue.int_value & 0xffff);
		break;

	      case FFI_TYPE_FLOAT:
	        PUSHF (rvalue.float_value);
		break;

	      case FFI_TYPE_DOUBLE:
	        PUSHD (rvalue.double_value);
		break;

	      case FFI_TYPE_SINT64:
	        PUSHL (rvalue.long_value);
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

#ifdef DIRECT_THREADED
      // For direct threaded we have a separate 'ldc class' operation.
    insn_ldc_class:
      {
	SAVE_PC();
	// We could rewrite the instruction at this point.
	int index = INTVAL ();
	jobject k = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						     index)).o;
	PUSHA (k);
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

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
	SAVE_PC();
	jint value2 = POPI();
	jint value1 = POPI();
	jint res = _Jv_divI (value1, value2);
	PUSHI (res);
      }
      NEXT_INSN;

    insn_ldiv:
      {
	SAVE_PC();
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
	SAVE_PC();
	jint value2 = POPI();
	jint value1 =  POPI();
	jint res = _Jv_remI (value1, value2);
	PUSHI (res);
      }
      NEXT_INSN;

    insn_lrem:
      {
	SAVE_PC();
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
      tmpval = -1;
      goto dcmp;

    insn_dcmpg:
      tmpval = 1;

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

	pc_t base = (pc_t) meth->bytecode ();
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

	unsigned char* base = meth->bytecode ();
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
        SAVE_PC(); // Constant pool resolution could throw.
	_Jv_Linker::resolve_pool_entry (meth->defining_class, fieldref_index);
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
		PUSHI (*field->u.byte_addr);
		newinsn = AMPAMP (getstatic_resolved_1);
		break;

	      case 2:
		if (type == JvPrimClass (char))
		  {
		    PUSHI (*field->u.char_addr);
		    newinsn = AMPAMP (getstatic_resolved_char);
		  }
		else
		  {
		    PUSHI (*field->u.short_addr);
		    newinsn = AMPAMP (getstatic_resolved_short);
		  }
		break;

	      case 4:
	        PUSHI(*field->u.int_addr);
		newinsn = AMPAMP (getstatic_resolved_4);
		break;

	      case 8:
	        PUSHL(*field->u.long_addr);
		newinsn = AMPAMP (getstatic_resolved_8);
		break;
	      }
	  }
	else
	  {
	    PUSHA(*field->u.object_addr);
	    newinsn = AMPAMP (getstatic_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	REWRITE_INSN (newinsn, datum, field->u.addr);
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
	SAVE_PC();
	jint fieldref_index = GET2U ();
	_Jv_Linker::resolve_pool_entry (meth->defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jclass type = field->type;
	jint field_offset = field->u.boffset;

	jobject obj   = POPA();
	NULLCHECK(obj);

	void *newinsn = NULL;
	_Jv_value *val = (_Jv_value *) ((char *)obj + field_offset);
	if (type->isPrimitive ())
	  {
	    switch (type->size_in_bytes)
	      {
	      case 1:
	        PUSHI (val->byte_value);
		newinsn = AMPAMP (getfield_resolved_1);
		break;

	      case 2:
		if (type == JvPrimClass (char))
		  {
		    PUSHI (val->char_value);
		    newinsn = AMPAMP (getfield_resolved_char);
		  }
		else
		  {
		    PUSHI (val->short_value);
		    newinsn = AMPAMP (getfield_resolved_short);
		  }
		break;

	      case 4:
		PUSHI (val->int_value);
		newinsn = AMPAMP (getfield_resolved_4);
		break;

	      case 8:
	        PUSHL (val->long_value);
		newinsn = AMPAMP (getfield_resolved_8);
		break;
	      }
	  }
	else
	  {
	    PUSHA (val->object_value);
	    newinsn = AMPAMP (getfield_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	REWRITE_INSN (newinsn, int_val, field_offset);
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
	SAVE_PC();
	jint fieldref_index = GET2U ();
	_Jv_Linker::resolve_pool_entry (meth->defining_class, fieldref_index);
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
		  *field->u.byte_addr = value;
		  newinsn = AMPAMP (putstatic_resolved_1);
		  break;
		}

	      case 2:
		{
		  jint value = POPI();
		  *field->u.char_addr = value;
		  newinsn = AMPAMP (putstatic_resolved_2);
		  break;
		}

	      case 4:
		{
		  jint value = POPI();
		  *field->u.int_addr = value;
		  newinsn = AMPAMP (putstatic_resolved_4);
		  break;
		}

	      case 8:
		{
		  jlong value = POPL();
		  *field->u.long_addr = value;
		  newinsn = AMPAMP (putstatic_resolved_8);
		  break;
		}
	      }
	  }
	else
	  {
	    jobject value = POPA();
	    *field->u.object_addr = value;
	    newinsn = AMPAMP (putstatic_resolved_obj);
	  }

#ifdef DIRECT_THREADED
	REWRITE_INSN (newinsn, datum, field->u.addr);
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
	SAVE_PC();
	jint fieldref_index = GET2U ();
	_Jv_Linker::resolve_pool_entry (meth->defining_class, fieldref_index);
	_Jv_Field *field = pool_data[fieldref_index].field;

	jclass type = field->type;

	if ((field->flags & Modifier::STATIC) != 0)
	  throw_incompatible_class_change_error 
	    (JvNewStringLatin1 ("field is static"));

	jint field_offset = field->u.boffset;

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
	REWRITE_INSN (newinsn, int_val, field_offset);
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
	SAVE_PC();
	int index = GET2U ();

	rmeth = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						   index)).rmethod;

	sp -= rmeth->stack_item_count;

	// We don't use NULLCHECK here because we can't rely on that
	// working for <init>.  So instead we do an explicit test.
	if (! sp[0].o)
	  {
	    SAVE_PC();
	    throw_null_pointer_exception ();
	  }

	fun = (void (*)()) rmeth->method->ncode;

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	REWRITE_INSN (&&invokespecial_resolved, datum, rmeth);
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokespecial_resolved:
      {
	SAVE_PC();
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	// We don't use NULLCHECK here because we can't rely on that
	// working for <init>.  So instead we do an explicit test.
	if (! sp[0].o)
	  {
	    throw_null_pointer_exception ();
	  }
	fun = (void (*)()) rmeth->method->ncode;
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    insn_invokestatic:
      {
	SAVE_PC();
	int index = GET2U ();

	rmeth = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						   index)).rmethod;

	sp -= rmeth->stack_item_count;

	fun = (void (*)()) rmeth->method->ncode;

#ifdef DIRECT_THREADED
	// Rewrite instruction so that we use a faster pre-resolved
	// method.
	REWRITE_INSN (&&invokestatic_resolved, datum, rmeth);
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokestatic_resolved:
      {
	SAVE_PC();
	rmeth = (_Jv_ResolvedMethod *) AVAL ();
	sp -= rmeth->stack_item_count;
	fun = (void (*)()) rmeth->method->ncode;
      }
      goto perform_invoke;
#endif /* DIRECT_THREADED */

    insn_invokeinterface:
      {
	SAVE_PC();
	int index = GET2U ();

	rmeth = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						   index)).rmethod;

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
	REWRITE_INSN (&&invokeinterface_resolved, datum, rmeth);
#else
	// Skip dummy bytes.
	pc += 2;
#endif /* DIRECT_THREADED */
      }
      goto perform_invoke;

#ifdef DIRECT_THREADED
    invokeinterface_resolved:
      {
	SAVE_PC();
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
	SAVE_PC();
	int index = GET2U ();
	jclass klass = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
							  index)).clazz;
	/* VM spec, section 3.11.5 */
	if ((klass->getModifiers() & Modifier::ABSTRACT)
	    || klass->isInterface())
	  {
	    jthrowable t = new java::lang::InstantiationException;
	    INTERP_REPORT_EXCEPTION (t);
	    throw t;
	  }
	jobject res = _Jv_AllocObject (klass);
	PUSHA (res);

#ifdef DIRECT_THREADED
	REWRITE_INSN (&&new_resolved, datum, klass);
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    new_resolved:
      {
	jclass klass = (jclass) AVAL ();
	jobject res = _Jv_AllocObject (klass);
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
	SAVE_PC();
	int index = GET2U ();
	jclass klass = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
							  index)).clazz;
	int size  = POPI();
	jobject result = _Jv_NewObjectArray (size, klass, 0);
	PUSHA (result);

#ifdef DIRECT_THREADED
	REWRITE_INSN (&&anewarray_resolved, datum, klass);
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
	jthrowable t = static_cast<jthrowable> (value);
	INTERP_REPORT_EXCEPTION (t);
	throw t;
      }
      NEXT_INSN;

    insn_checkcast:
      {
        SAVE_PC();
	jobject value = POPA();
	jint index = GET2U ();
	jclass to = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						       index)).clazz;

	value = (jobject) _Jv_CheckCast (to, value);

	PUSHA (value);

#ifdef DIRECT_THREADED
	REWRITE_INSN (&&checkcast_resolved, datum, to);
#endif /* DIRECT_THREADED */
      }
      NEXT_INSN;

#ifdef DIRECT_THREADED
    checkcast_resolved:
      {
        SAVE_PC();
	jobject value = POPA ();
	jclass to = (jclass) AVAL ();
	value = (jobject) _Jv_CheckCast (to, value);
	PUSHA (value);
      }
      NEXT_INSN;
#endif /* DIRECT_THREADED */

    insn_instanceof:
      {
        SAVE_PC();
	jobject value = POPA();
	jint index = GET2U ();
	jclass to = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
						       index)).clazz;
	PUSHI (to->isInstance (value));

#ifdef DIRECT_THREADED
	REWRITE_INSN (&&instanceof_resolved, datum, to);
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
	SAVE_PC();
	int kind_index = GET2U ();
	int dim        = GET1U ();

	jclass type    
	  = (_Jv_Linker::resolve_pool_entry (meth->defining_class,
					       kind_index)).clazz;
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

	  case op_fload:
	    LOADF (wide);
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

    insn_breakpoint:
      {
	using namespace ::java::lang;
	jmethodID method = meth->self;
	jlocation location = meth->insn_index (pc - 1);

	using namespace gnu::gcj::jvmti;
	Breakpoint *bp
	  = BreakpointManager::getBreakpoint (reinterpret_cast<jlong> (method),
					      location);
	JvAssert (bp != NULL);

	// Save the insn here since the breakpoint could be removed
	// before the JVMTI notification returns.
	pc_t opc = reinterpret_cast<pc_t> (bp->getInsn ());

	bp->execute ();

	// Continue execution
#ifdef DIRECT_THREADED
	goto *(opc->insn);
#else
	goto *(insn_target[*opc]);
#endif
      }
    }
  catch (java::lang::Throwable *ex)
    {
      // Check if the exception is handled and, if so, set the pc to the start
      // of the appropriate catch block.
      if (meth->check_handler (&pc, meth, ex))
        {
          sp = stack;
          sp++->o = ex; // Push exception.
#ifdef DEBUG
          if (JVMTI_REQUESTED_EVENT (ExceptionCatch))
            {
              using namespace gnu::gcj::jvmti;
              jlong catch_meth = reinterpret_cast<jlong> (meth->get_method ());
              jlong catch_loc = meth->insn_index (pc);
	      _Jv_JVMTI_PostEvent (JVMTI_EVENT_EXCEPTION_CATCH, thread,
				   _Jv_GetCurrentJNIEnv (), catch_meth,
				   catch_loc, ex);
            }
#endif
          NEXT_INSN;
        }

      // No handler, so re-throw.
      throw ex;
    }

// natBreakpoint.cc - C++ side of Breakpoint

/* Copyright (C) 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <gcj/method.h>
#include <java-interp.h>
#include <java-insns.h>
#include <java-assert.h>
#include <jvmti.h>

#include <gnu/gcj/jvmti/Breakpoint.h>
#include <gnu/gcj/jvmti/BreakpointManager.h>

static _Jv_InterpMethod *
get_interp_method (jlong method)
{
  jmethodID id = reinterpret_cast<jmethodID> (method);
  jclass klass = _Jv_GetMethodDeclaringClass (id);
  JvAssert (_Jv_IsInterpretedClass (klass));
  _Jv_MethodBase *base
    = _Jv_FindInterpreterMethod (klass, id);
  JvAssert (base != NULL);
  return reinterpret_cast<_Jv_InterpMethod *> (base);
}

void
gnu::gcj::jvmti::Breakpoint::_save_insn ()
{
  _Jv_InterpMethod *imeth = get_interp_method (method);

  // copy contents of insn at location into data
  pc_t code = imeth->get_insn (location);
  data = (RawDataManaged *) JvAllocBytes (sizeof (*code));
  memcpy (data, code, sizeof (*code));
}

void
gnu::gcj::jvmti::Breakpoint::install ()
{
  _save_insn ();
  _Jv_InterpMethod *imeth = get_interp_method (method);
  imeth->install_break (location);
}

void
gnu::gcj::jvmti::Breakpoint::remove ()
{
  _Jv_InterpMethod *imeth = get_interp_method (method);
  imeth->set_insn (location, reinterpret_cast<pc_t> (data));
}

#ifdef DIRECT_THREADED
void
_Jv_RewriteBreakpointInsn (jmethodID mid, jlocation loc, pc_t new_insn)
{
  using namespace ::gnu::gcj::jvmti;
  Breakpoint *bp
    = BreakpointManager::getBreakpoint (reinterpret_cast<jlong> (mid), loc);
  if (bp != NULL)
    {
      pc_t old_insn = (pc_t) bp->data;
      old_insn->insn = new_insn;
    }
}
#endif // DIRECT_THREADED

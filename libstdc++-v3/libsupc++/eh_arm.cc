// -*- C++ -*- ARM specific Exception handling support routines.
// Copyright (C) 2004-2019 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.
//
// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cxxabi.h>
#include "unwind-cxx.h"

#ifdef __ARM_EABI_UNWINDER__

using namespace __cxxabiv1;


// Given the thrown type THROW_TYPE, exception object UE_HEADER and a
// type CATCH_TYPE to compare against, return whether or not there is
// a match and if so, update *THROWN_PTR_P to point to either the
// type-matched object, or in the case of a pointer type, the object
// pointed to by the pointer.

extern "C" __cxa_type_match_result
__cxa_type_match(_Unwind_Exception* ue_header,
		 const std::type_info* catch_type,
		 bool is_reference __attribute__((__unused__)),
		 void** thrown_ptr_p)
{
  bool forced_unwind
    = __is_gxx_forced_unwind_class(ue_header->exception_class);
  bool foreign_exception
    = !forced_unwind && !__is_gxx_exception_class(ue_header->exception_class);
  bool dependent_exception
    = __is_dependent_exception(ue_header->exception_class);
  __cxa_exception* xh = __get_exception_header_from_ue(ue_header);
  __cxa_dependent_exception *dx = __get_dependent_exception_from_ue(ue_header);
  const std::type_info* throw_type;
  void *thrown_ptr = 0;

  if (forced_unwind)
    throw_type = &typeid(abi::__forced_unwind);
  else if (foreign_exception)
    throw_type = &typeid(abi::__foreign_exception);
  else
    {
      if (dependent_exception)
	xh = __get_exception_header_from_obj (dx->primaryException);
      throw_type = xh->exceptionType;
      // We used to require the caller set the target of thrown_ptr_p,
      // but that's incorrect -- the EHABI makes no such requirement
      // -- and not all callers will set it.  Fortunately callers that
      // do initialize will always pass us the value we calculate
      // here, so there's no backwards compatibility problem.
      thrown_ptr = __get_object_from_ue (ue_header);
    }
  
  __cxa_type_match_result result = ctm_succeeded;

  // Pointer types need to adjust the actual pointer, not
  // the pointer to pointer that is the exception object.
  // This also has the effect of passing pointer types
  // "by value" through the __cxa_begin_catch return value.
  if (throw_type->__is_pointer_p())
    {
      thrown_ptr = *(void**) thrown_ptr;
      // We need to indicate the indirection to our caller.
      result = ctm_succeeded_with_ptr_to_base;
    }

  if (catch_type->__do_catch(throw_type, &thrown_ptr, 1))
    {
      *thrown_ptr_p = thrown_ptr;
      return result;
    }

  return ctm_failed;
}

// ABI defined routine called at the start of a cleanup handler.
extern "C" bool
__cxa_begin_cleanup(_Unwind_Exception* ue_header)
{
  __cxa_eh_globals *globals = __cxa_get_globals();
  __cxa_exception *header = __get_exception_header_from_ue(ue_header);
  bool native = __is_gxx_exception_class(header->unwindHeader.exception_class);


  if (native)
    {
      header->propagationCount++;
      // Add it to the chain if this is the first time we've seen this
      // exception.
      if (header->propagationCount == 1)
	{
	  header->nextPropagatingException = globals->propagatingExceptions;
	  globals->propagatingExceptions = header;
	}
    }
  else
    {
      // Remember the exception object, so end_cleanup can return it.
      // These cannot be stacked, so we must abort if we already have
      // a propagating exception.
      if (globals->propagatingExceptions)
	std::terminate ();
      globals->propagatingExceptions = header;
    }

  return true;
}

// Do the work for __cxa_end_cleanup.  Returns the currently propagating
// exception object.
extern "C" _Unwind_Exception *
__gnu_end_cleanup(void)
{
  __cxa_exception *header;
  __cxa_eh_globals *globals = __cxa_get_globals();

  header = globals->propagatingExceptions;

  // Check something hasn't gone horribly wrong.
  if (!header)
    std::terminate();

  if (__is_gxx_exception_class(header->unwindHeader.exception_class))
    {
      header->propagationCount--;
      if (header->propagationCount == 0)
	{
	  // Remove exception from chain.
	  globals->propagatingExceptions = header->nextPropagatingException;
	  header->nextPropagatingException = NULL;
	}
    }
  else
    globals->propagatingExceptions = NULL;

  return &header->unwindHeader;
}

#ifdef __TMS320C6X__
// Assembly wrapper to call __gnu_end_cleanup without clobbering
// function arguments to _Unwind_Resume.
asm (".global __cxa_end_cleanup\n"
"	.type __cxa_end_cleanup, \"function\"\n"
"__cxa_end_cleanup:\n"
"	stw	.d2t2	B9, *B15--[10]\n"
"	stw	.d2t2	B8, *+B15[9]\n"
"	stw	.d2t2	B7, *+B15[8]\n"
"	stw	.d2t2	B6, *+B15[7]\n"
"	stw	.d2t2	B5, *+B15[6]\n"
"	stw	.d2t2	B4, *+B15[5]\n"
"	stw	.d2t1	A9, *+B15[4]\n"
"	stw	.d2t1	A8, *+B15[3]\n"
"	stw	.d2t1	A7, *+B15[2]\n"
"	stw	.d2t1	A6, *+B15[1]\n"
#ifdef _TMS320C6400_PLUS
"	callp	.s2	(__gnu_end_cleanup), B3\n"
#elif defined(_TMS320C6400)
"	call	.s2	(__gnu_end_cleanup)\n"
"	addkpc	.s2	1f, B3, 0\n"
"	nop		4\n"
"1:\n"
#else
"	call	.s2	(__gnu_end_cleanup)\n"
"	mvkl	.s2	1f, B3\n"
"	mvkh	.s2	1f, B3\n"
"	nop		3\n"
"1:\n"
#endif
"	ldw	.d2t1	*+B15[1], A6\n"
"	ldw	.d2t1	*+B15[2], A7\n"
"	ldw	.d2t1	*+B15[3], A8\n"
"	ldw	.d2t1	*+B15[4], A9\n"
"	ldw	.d2t2	*+B15[5], B4\n"
"	ldw	.d2t2	*+B15[6], B5\n"
"	ldw	.d2t2	*+B15[7], B6\n"
"	ldw	.d2t2	*+B15[8], B7\n"
"	ldw	.d2t2	*+B15[9], B8\n"
"	ldw	.d2t2	*++B15[10], B9\n"
"	b	.s2	_Unwind_Resume\n"
"	nop		5\n");
#else
// Assembly wrapper to call __gnu_end_cleanup without clobbering r1-r3.
// Also push r4 to preserve stack alignment.
#ifdef __thumb__
asm ("  .pushsection .text.__cxa_end_cleanup\n"
"	.global __cxa_end_cleanup\n"
"	.type __cxa_end_cleanup, \"function\"\n"
"	.thumb_func\n"
"__cxa_end_cleanup:\n"
"	push\t{r1, r2, r3, r4}\n"
"	bl\t__gnu_end_cleanup\n"
"	pop\t{r1, r2, r3, r4}\n"
"	bl\t_Unwind_Resume @ Never returns\n"
"	.popsection\n");
#else
asm ("  .pushsection .text.__cxa_end_cleanup\n"
"	.global __cxa_end_cleanup\n"
"	.type __cxa_end_cleanup, \"function\"\n"
"__cxa_end_cleanup:\n"
"	stmfd\tsp!, {r1, r2, r3, r4}\n"
"	bl\t__gnu_end_cleanup\n"
"	ldmfd\tsp!, {r1, r2, r3, r4}\n"
"	bl\t_Unwind_Resume @ Never returns\n"
"	.popsection\n");
#endif
#endif

#endif

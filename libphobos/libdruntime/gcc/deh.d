// GNU D Compiler exception personality routines.
// Copyright (C) 2011-2020 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// This code is based on the libstdc++ exception handling routines.

module gcc.deh;

import gcc.unwind;
import gcc.unwind.pe;
import gcc.builtins;
import gcc.config;
import gcc.attribute;

extern(C)
{
    int _d_isbaseof(ClassInfo, ClassInfo);
    void _d_createTrace(Object, void*);

    // Not used in GDC but declaration required by rt/sections.d
    struct FuncTable
    {
    }
}

/**
 * Declare all known and handled exception classes.
 * D exceptions -- "GNUCD\0\0\0".
 * C++ exceptions -- "GNUCC++\0"
 * C++ dependent exceptions -- "GNUCC++\x01"
 */
static if (GNU_ARM_EABI_Unwinder)
{
    enum _Unwind_Exception_Class gdcExceptionClass = "GNUCD\0\0\0";
    enum _Unwind_Exception_Class gxxExceptionClass = "GNUCC++\0";
    enum _Unwind_Exception_Class gxxDependentExceptionClass = "GNUCC++\x01";
}
else
{
    enum _Unwind_Exception_Class gdcExceptionClass =
        (cast(_Unwind_Exception_Class)'G' << 56) |
        (cast(_Unwind_Exception_Class)'N' << 48) |
        (cast(_Unwind_Exception_Class)'U' << 40) |
        (cast(_Unwind_Exception_Class)'C' << 32) |
        (cast(_Unwind_Exception_Class)'D' << 24);

    enum _Unwind_Exception_Class gxxExceptionClass =
        (cast(_Unwind_Exception_Class)'G' << 56) |
        (cast(_Unwind_Exception_Class)'N' << 48) |
        (cast(_Unwind_Exception_Class)'U' << 40) |
        (cast(_Unwind_Exception_Class)'C' << 32) |
        (cast(_Unwind_Exception_Class)'C' << 24) |
        (cast(_Unwind_Exception_Class)'+' << 16) |
        (cast(_Unwind_Exception_Class)'+' <<  8) |
        (cast(_Unwind_Exception_Class)0 <<  0);

    enum _Unwind_Exception_Class gxxDependentExceptionClass =
        gxxExceptionClass + 1;
}

/**
 * Checks for GDC exception class.
 */
bool isGdcExceptionClass(_Unwind_Exception_Class c) @nogc
{
    static if (GNU_ARM_EABI_Unwinder)
    {
        return c[0] == gdcExceptionClass[0]
            && c[1] == gdcExceptionClass[1]
            && c[2] == gdcExceptionClass[2]
            && c[3] == gdcExceptionClass[3]
            && c[4] == gdcExceptionClass[4]
            && c[5] == gdcExceptionClass[5]
            && c[6] == gdcExceptionClass[6]
            && c[7] == gdcExceptionClass[7];
    }
    else
    {
        return c == gdcExceptionClass;
    }
}

/**
 * Checks for any C++ exception class.
 */
bool isGxxExceptionClass(_Unwind_Exception_Class c) @nogc
{
    static if (GNU_ARM_EABI_Unwinder)
    {
        return c[0] == gxxExceptionClass[0]
            && c[1] == gxxExceptionClass[1]
            && c[2] == gxxExceptionClass[2]
            && c[3] == gxxExceptionClass[3]
            && c[4] == gxxExceptionClass[4]
            && c[5] == gxxExceptionClass[5]
            && c[6] == gxxExceptionClass[6]
            && (c[7] == gxxExceptionClass[7]
                || c[7] == gxxDependentExceptionClass[7]);
    }
    else
    {
        return c == gxxExceptionClass
            || c == gxxDependentExceptionClass;
    }
}

/**
 * Checks for primary or dependent, but not that it is a C++ exception.
 */
bool isDependentException(_Unwind_Exception_Class c) @nogc
{
    static if (GNU_ARM_EABI_Unwinder)
        return (c[7] == '\x01');
    else
        return (c & 1);
}

/**
 * A D exception object consists of a header, which is a wrapper
 * around an unwind object header with additional D specific
 * information, prefixed by the exception object itself.
 */
struct ExceptionHeader
{
    // Because of a lack of __aligned__ style attribute, our object
    // and the unwind object are the first two fields.
    static if (Throwable.alignof < _Unwind_Exception.alignof)
        ubyte[_Unwind_Exception.alignof - Throwable.alignof] pad;

    // The object being thrown.  The compiled code expects this to
    // be immediately before the generic exception header.
    Throwable object;

    // The generic exception header.
    _Unwind_Exception unwindHeader;

    static assert(unwindHeader.offsetof - object.offsetof == object.sizeof);

    // Cache handler details between Phase 1 and Phase 2.
    static if (GNU_ARM_EABI_Unwinder)
    {
        // Nothing here yet.
    }
    else
    {
        // Which catch was found.
        int handler;

        // Language Specific Data Area for function enclosing the handler.
        const(ubyte)* languageSpecificData;

        // Pointer to catch code.
        _Unwind_Ptr landingPad;

        // Canonical Frame Address (CFA) for the enclosing handler.
        _Unwind_Word canonicalFrameAddress;
    }

    // Stack other thrown exceptions in current thread through here.
    ExceptionHeader* next;

    // Thread local stack of chained exceptions.
    static ExceptionHeader* stack;

    // Pre-allocate storage for 1 instance per thread.
    // Use calloc/free for multiple exceptions in flight.
    static ExceptionHeader ehstorage;

    /**
     * Allocate and initialize an ExceptionHeader.
     */
    static ExceptionHeader* create(Throwable o) @nogc
    {
        auto eh = &ehstorage;

        // Check exception object in use.
        if (eh.object)
        {
            eh = cast(ExceptionHeader*) __builtin_calloc(ExceptionHeader.sizeof, 1);
            // Out of memory while throwing - not much else can be done.
            if (!eh)
                terminate("out of memory", __LINE__);
        }
        eh.object = o;

        eh.unwindHeader.exception_class = gdcExceptionClass;

        return eh;
    }

    /**
     * Free ExceptionHeader that was created by create().
     */
    static void free(ExceptionHeader* eh) @nogc
    {
        *eh = ExceptionHeader.init;
        if (eh != &ehstorage)
            __builtin_free(eh);
    }

    /**
     * Push this onto stack of chained exceptions.
     */
    void push() @nogc
    {
        next = stack;
        stack = &this;
    }

    /**
     * Pop and return top of chained exception stack.
     */
    static ExceptionHeader* pop() @nogc
    {
        auto eh = stack;
        stack = eh.next;
        return eh;
    }

    /**
     * Save stage1 handler information in the exception object.
     */
    static void save(_Unwind_Exception* unwindHeader,
                     _Unwind_Word cfa, int handler,
                     const(ubyte)* lsda, _Unwind_Ptr landingPad) @nogc
    {
        static if (GNU_ARM_EABI_Unwinder)
        {
            unwindHeader.barrier_cache.sp = cfa;
            unwindHeader.barrier_cache.bitpattern[1] = cast(_uw)handler;
            unwindHeader.barrier_cache.bitpattern[2] = cast(_uw)lsda;
            unwindHeader.barrier_cache.bitpattern[3] = cast(_uw)landingPad;
        }
        else
        {
            ExceptionHeader* eh = toExceptionHeader(unwindHeader);
            eh.canonicalFrameAddress = cfa;
            eh.handler = handler;
            eh.languageSpecificData = lsda;
            eh.landingPad = landingPad;
        }
    }

    /**
     * Restore the catch handler data saved during phase1.
     */
    static void restore(_Unwind_Exception* unwindHeader, out int handler,
                        out const(ubyte)* lsda, out _Unwind_Ptr landingPad,
                        out _Unwind_Word cfa) @nogc
    {
        static if (GNU_ARM_EABI_Unwinder)
        {
            cfa = unwindHeader.barrier_cache.sp;
            handler = cast(int)unwindHeader.barrier_cache.bitpattern[1];
            lsda = cast(ubyte*)unwindHeader.barrier_cache.bitpattern[2];
            landingPad = cast(_Unwind_Ptr)unwindHeader.barrier_cache.bitpattern[3];
        }
        else
        {
            ExceptionHeader* eh = toExceptionHeader(unwindHeader);
            cfa = eh.canonicalFrameAddress;
            handler = eh.handler;
            lsda = eh.languageSpecificData;
            landingPad = cast(_Unwind_Ptr)eh.landingPad;
        }
    }

    /**
     * Look at the chain of inflight exceptions and pick the class type that'll
     * be looked for in catch clauses.
     */
    static ClassInfo getClassInfo(_Unwind_Exception* unwindHeader) @nogc
    {
        ExceptionHeader* eh = toExceptionHeader(unwindHeader);
        // The first thrown Exception at the top of the stack takes precedence
        // over others that are inflight, unless an Error was thrown, in which
        // case, we search for error handlers instead.
        Throwable ehobject = eh.object;
        for (ExceptionHeader* ehn = eh.next; ehn; ehn = ehn.next)
        {
            Error e = cast(Error)ehobject;
            if (e is null || (cast(Error)ehn.object) !is null)
                ehobject = ehn.object;
        }
        return ehobject.classinfo;
    }

    /**
     * Convert from pointer to unwindHeader to pointer to ExceptionHeader
     * that it is embedded inside of.
     */
    static ExceptionHeader* toExceptionHeader(_Unwind_Exception* exc) @nogc
    {
        return cast(ExceptionHeader*)(cast(void*)exc - ExceptionHeader.unwindHeader.offsetof);
    }
}

/**
 * Map to C++ std::type_info's virtual functions from D,
 * being careful to not require linking with libstdc++.
 * So it is given a different name.
 */
extern(C++) interface CxxTypeInfo
{
    void dtor1();
    void dtor2();
    bool __is_pointer_p() const;
    bool __is_function_p() const;
    bool __do_catch(const CxxTypeInfo, void**, uint) const;
    bool __do_upcast(const void*, void**) const;
}

/**
 * Structure of a C++ exception, represented as a C structure.
 * See unwind-cxx.h for the full definition.
 */
struct CxaExceptionHeader
{
    union
    {
        CxxTypeInfo exceptionType;
        void* primaryException;
    }
    void function(void*) exceptionDestructor;
    void function() unexpectedHandler;
    void function() terminateHandler;
    CxaExceptionHeader* nextException;
    int handlerCount;

    static if (GNU_ARM_EABI_Unwinder)
    {
        CxaExceptionHeader* nextPropagatingException;
        int propagationCount;
    }
    else
    {
        int handlerSwitchValue;
        const(ubyte)* actionRecord;
        const(ubyte)* languageSpecificData;
        _Unwind_Ptr catchTemp;
        void* adjustedPtr;
    }

    _Unwind_Exception unwindHeader;

    /**
     * There's no saving between phases, so only cache pointer.
     * __cxa_begin_catch expects this to be set.
     */
    static void save(_Unwind_Exception* unwindHeader, void* thrownPtr) @nogc
    {
        static if (GNU_ARM_EABI_Unwinder)
            unwindHeader.barrier_cache.bitpattern[0] = cast(_uw) thrownPtr;
        else
        {
            auto eh = toExceptionHeader(unwindHeader);
            eh.adjustedPtr = thrownPtr;
        }
    }

    /**
     * Get pointer to the thrown object if the thrown object type behind the
     * exception is implicitly convertible to the catch type.
     */
    static void* getAdjustedPtr(_Unwind_Exception* exc, CxxTypeInfo catchType)
    {
        void* thrownPtr;

        // A dependent C++ exceptions is just a wrapper around the unwind header.
        // A primary C++ exception has the thrown object located immediately after it.
        if (isDependentException(exc.exception_class))
            thrownPtr = toExceptionHeader(exc).primaryException;
        else
            thrownPtr = cast(void*)(exc + 1);

        // Pointer types need to adjust the actual pointer, not the pointer that is
        // the exception object.  This also has the effect of passing pointer types
        // "by value" through the __cxa_begin_catch return value.
        const throw_type = (cast(CxaExceptionHeader*)thrownPtr - 1).exceptionType;

        if (throw_type.__is_pointer_p())
            thrownPtr = *cast(void**)thrownPtr;

        // Pointer adjustment may be necessary due to multiple inheritance
        if (catchType is throw_type
            || catchType.__do_catch(throw_type, &thrownPtr, 1))
            return thrownPtr;

        return null;
    }

    /**
     * Convert from pointer to unwindHeader to pointer to CxaExceptionHeader
     * that it is embedded inside of.
     */
    static CxaExceptionHeader* toExceptionHeader(_Unwind_Exception* exc) @nogc
    {
        return cast(CxaExceptionHeader*)(exc + 1) - 1;
    }
}

/**
 * Called if exception handling must be abandoned for any reason.
 */
private void terminate(string msg, uint line) @nogc
{
    import core.stdc.stdio;
    import core.stdc.stdlib;

    static bool terminating;
    if (terminating)
    {
        fputs("terminate called recursively\n", stderr);
        abort();
    }
    terminating = true;

    fprintf(stderr, "gcc.deh(%u): %.*s\n", line, cast(int)msg.length, msg.ptr);

    abort();
}

/**
 * Called when fibers switch contexts.
 */
extern(C) void* _d_eh_swapContext(void* newContext) nothrow @nogc
{
    auto old = ExceptionHeader.stack;
    ExceptionHeader.stack = cast(ExceptionHeader*)newContext;
    return old;
}

/**
 * Called before starting a catch.  Returns the exception object.
 */
extern(C) void* __gdc_begin_catch(_Unwind_Exception* unwindHeader)
{
    ExceptionHeader* header = ExceptionHeader.toExceptionHeader(unwindHeader);

    void* objectp = cast(void*)header.object;

    // Something went wrong when stacking up chained headers...
    if (header != ExceptionHeader.pop())
        terminate("catch error", __LINE__);

    // Handling for this exception is complete.
    _Unwind_DeleteException(&header.unwindHeader);

    return objectp;
}

/**
 * Perform a throw, D style. Throw will unwind through this call,
 * so there better not be any handlers or exception thrown here.
 */
extern(C) void _d_throw(Throwable object)
{
    // If possible, avoid always allocating new memory for exception headers.
    ExceptionHeader *eh = ExceptionHeader.create(object);

    // Add to thrown exception stack.
    eh.push();

    // Called by unwinder when exception object needs destruction by other than our code.
    extern(C) void exception_cleanup(_Unwind_Reason_Code code, _Unwind_Exception* exc)
    {
        // If we haven't been caught by a foreign handler, then this is
        // some sort of unwind error.  In that case just die immediately.
        // _Unwind_DeleteException in the HP-UX IA64 libunwind library
        //  returns _URC_NO_REASON and not _URC_FOREIGN_EXCEPTION_CAUGHT
        // like the GCC _Unwind_DeleteException function does.
        if (code != _URC_FOREIGN_EXCEPTION_CAUGHT && code != _URC_NO_REASON)
            terminate("uncaught exception", __LINE__);

        auto eh = ExceptionHeader.toExceptionHeader(exc);
        ExceptionHeader.free(eh);
    }

    eh.unwindHeader.exception_cleanup = &exception_cleanup;

    // Runtime now expects us to do this first before unwinding.
    _d_createTrace(eh.object, null);

    // We're happy with setjmp/longjmp exceptions or region-based
    // exception handlers: entry points are provided here for both.
    _Unwind_Reason_Code r = void;

    version (GNU_SjLj_Exceptions)
        r = _Unwind_SjLj_RaiseException(&eh.unwindHeader);
    else
        r = _Unwind_RaiseException(&eh.unwindHeader);

    // If code == _URC_END_OF_STACK, then we reached top of stack without finding
    // a handler for the exception.  Since each thread is run in a try/catch,
    // this oughtn't happen.  If code is something else, we encountered some sort
    // of heinous lossage from which we could not recover.  As is the way of such
    // things, almost certainly we will have crashed before now, rather than
    // actually being able to diagnose the problem.
    if (r == _URC_END_OF_STACK)
        terminate("uncaught exception", __LINE__);

    terminate("unwind error", __LINE__);
}

static if (GNU_ARM_EABI_Unwinder)
{
    enum personality_fn_attributes = attribute("target", ("general-regs-only"));
}
else
{
    enum personality_fn_attributes = "";
}

/**
 * Read and extract information from the LSDA (.gcc_except_table section).
 */
@personality_fn_attributes
_Unwind_Reason_Code scanLSDA(const(ubyte)* lsda, _Unwind_Exception_Class exceptionClass,
                             _Unwind_Action actions, _Unwind_Exception* unwindHeader,
                             _Unwind_Context* context, _Unwind_Word cfa,
                             out _Unwind_Ptr landingPad, out int handler)
{
    // If no LSDA, then there are no handlers or cleanups.
    if (lsda is null)
        return CONTINUE_UNWINDING(unwindHeader, context);

    // Parse the LSDA header
    auto p = lsda;

    auto Start = (context ? _Unwind_GetRegionStart(context) : 0);

    // Find @LPStart, the base to which landing pad offsets are relative.
    ubyte LPStartEncoding = *p++;
    _Unwind_Ptr LPStart = 0;

    if (LPStartEncoding != DW_EH_PE_omit)
        LPStart = read_encoded_value(context, LPStartEncoding, &p);
    else
        LPStart = Start;

    // Find @TType, the base of the handler and exception spec type data.
    ubyte TTypeEncoding = *p++;
    const(ubyte)* TType = null;

    if (TTypeEncoding != DW_EH_PE_omit)
    {
        static if (__traits(compiles, _TTYPE_ENCODING))
        {
            // Older ARM EABI toolchains set this value incorrectly, so use a
            // hardcoded OS-specific format.
            TTypeEncoding = _TTYPE_ENCODING;
        }
        auto TTbase = read_uleb128(&p);
        TType = p + TTbase;
    }

    // The encoding and length of the call-site table; the action table
    // immediately follows.
    ubyte CSEncoding = *p++;
    auto CSTableSize = read_uleb128(&p);
    const(ubyte)* actionTable = p + CSTableSize;

    auto TTypeBase = base_of_encoded_value(TTypeEncoding, context);

    // Get instruction pointer (ip) at start of instruction that threw.
    version (CRuntime_Glibc)
    {
        int ip_before_insn;
        auto ip = _Unwind_GetIPInfo(context, &ip_before_insn);
        if (!ip_before_insn)
            --ip;
    }
    else
    {
        auto ip = _Unwind_GetIP(context);
        --ip;
    }

    bool saw_cleanup = false;
    bool saw_handler = false;
    const(ubyte)* actionRecord = null;

    version (GNU_SjLj_Exceptions)
    {
        // The given "IP" is an index into the call-site table, with two
        // exceptions -- -1 means no-action, and 0 means terminate.
        // But since we're using uleb128 values, we've not got random
        // access to the array.
        if (cast(int) ip <= 0)
        {
            return _URC_CONTINUE_UNWIND;
        }
        else
        {
            _uleb128_t CSLandingPad, CSAction;
            do
            {
                CSLandingPad = read_uleb128(&p);
                CSAction = read_uleb128(&p);
            }
            while (--ip);

            // Can never have null landing pad for sjlj -- that would have
            // been indicated by a -1 call site index.
            landingPad = CSLandingPad + 1;
            if (CSAction)
                actionRecord = actionTable + CSAction - 1;
        }
    }
    else
    {
        // Search the call-site table for the action associated with this IP.
        while (p < actionTable)
        {
            // Note that all call-site encodings are "absolute" displacements.
            auto CSStart = read_encoded_value(null, CSEncoding, &p);
            auto CSLen = read_encoded_value(null, CSEncoding, &p);
            auto CSLandingPad = read_encoded_value(null, CSEncoding, &p);
            auto CSAction = read_uleb128(&p);

            // The table is sorted, so if we've passed the ip, stop.
            if (ip < Start + CSStart)
                p = actionTable;
            else if (ip < Start + CSStart + CSLen)
            {
                if (CSLandingPad)
                    landingPad = LPStart + CSLandingPad;
                if (CSAction)
                    actionRecord = actionTable + CSAction - 1;
                break;
            }
        }
    }

    if (landingPad == 0)
    {
        // IP is present, but has a null landing pad.
        // No cleanups or handlers to be run.
    }
    else if (actionRecord is null)
    {
        // If ip is present, has a non-null landing pad, and a null
        // action table offset, then there are only cleanups present.
        // Cleanups use a zero switch value, as set above.
        saw_cleanup = true;
    }
    else
    {
        // Otherwise we have a catch handler or exception specification.
        handler = actionTableLookup(actions, unwindHeader, actionRecord,
                                    exceptionClass, TTypeBase,
                                    TType, TTypeEncoding,
                                    saw_handler, saw_cleanup);
    }

    // IP is not in table.  No associated cleanups.
    if (!saw_handler && !saw_cleanup)
        return CONTINUE_UNWINDING(unwindHeader, context);

    if (actions & _UA_SEARCH_PHASE)
    {
        if (!saw_handler)
            return CONTINUE_UNWINDING(unwindHeader, context);

        // For domestic exceptions, we cache data from phase 1 for phase 2.
        if (isGdcExceptionClass(exceptionClass))
            ExceptionHeader.save(unwindHeader, cfa, handler, lsda, landingPad);

        return _URC_HANDLER_FOUND;
    }

    return 0;
}

/**
 * Look up and return the handler index of the classType in Action Table.
 */
int actionTableLookup(_Unwind_Action actions, _Unwind_Exception* unwindHeader,
                      const(ubyte)* actionRecord, _Unwind_Exception_Class exceptionClass,
                      _Unwind_Ptr TTypeBase, const(ubyte)* TType,
                      ubyte TTypeEncoding,
                      out bool saw_handler, out bool saw_cleanup)
{
    ClassInfo thrownType;
    if (isGdcExceptionClass(exceptionClass))
    {
        thrownType = ExceptionHeader.getClassInfo(unwindHeader);
    }

    while (1)
    {
        auto ap = actionRecord;
        auto ARFilter = read_sleb128(&ap);
        auto apn = ap;
        auto ARDisp = read_sleb128(&ap);

        if (ARFilter == 0)
        {
            // Zero filter values are cleanups.
            saw_cleanup = true;
        }
        else if (actions & _UA_FORCE_UNWIND)
        {
            // During forced unwinding, we only run cleanups.
        }
        else if (ARFilter > 0)
        {
            // Positive filter values are handlers.
            auto encodedSize = size_of_encoded_value(TTypeEncoding);

            // ARFilter is the negative index from TType, which is where
            // the ClassInfo is stored.
            const(ubyte)* tp = TType - ARFilter * encodedSize;

            auto entry = read_encoded_value_with_base(TTypeEncoding, TTypeBase, &tp);
            ClassInfo ci = cast(ClassInfo)cast(void*)(entry);

            // D does not have catch-all handlers, and so the following
            // assumes that we will never handle a null value.
            assert(ci !is null);

            if (ci.classinfo is __cpp_type_info_ptr.classinfo
                && isGxxExceptionClass(exceptionClass))
            {
                // catchType is the catch clause type_info.
                auto catchType = cast(CxxTypeInfo)((cast(__cpp_type_info_ptr)cast(void*)ci).ptr);
                auto thrownPtr = CxaExceptionHeader.getAdjustedPtr(unwindHeader, catchType);

                if (thrownPtr !is null)
                {
                    if (actions & _UA_SEARCH_PHASE)
                        CxaExceptionHeader.save(unwindHeader, thrownPtr);
                    saw_handler = true;
                    return cast(int)ARFilter;
                }
            }
            else if (isGdcExceptionClass(exceptionClass)
                     && _d_isbaseof(thrownType, ci))
            {
                saw_handler = true;
                return cast(int)ARFilter;
            }
            else
            {
                // ??? What to do about other GNU language exceptions.
            }
        }
        else
        {
            // Negative filter values are exception specifications,
            // which D does not use.
            break;
        }

        if (ARDisp == 0)
            break;
        actionRecord = apn + ARDisp;
    }

    return 0;
}

/**
 * Called when the personality function has found neither a cleanup or handler.
 * To support ARM EABI personality routines, that must also unwind the stack.
 */
@personality_fn_attributes
_Unwind_Reason_Code CONTINUE_UNWINDING(_Unwind_Exception* unwindHeader, _Unwind_Context* context)
{
    static if (GNU_ARM_EABI_Unwinder)
    {
        if (__gnu_unwind_frame(unwindHeader, context) != _URC_OK)
            return _URC_FAILURE;
    }
    return _URC_CONTINUE_UNWIND;
}

/**
 * Using a different personality function name causes link failures
 * when trying to mix code using different exception handling models.
 */
version (GNU_SEH_Exceptions)
{
    enum PERSONALITY_FUNCTION = "__gdc_personality_imp";

    extern(C) EXCEPTION_DISPOSITION __gdc_personality_seh0(void* ms_exc, void* this_frame,
                                                           void* ms_orig_context, void* ms_disp)
    {
        return _GCC_specific_handler(ms_exc, this_frame, ms_orig_context,
                                     ms_disp, &__gdc_personality_imp);
    }
}
else version (GNU_SjLj_Exceptions)
{
    enum PERSONALITY_FUNCTION = "__gdc_personality_sj0";

    private int __builtin_eh_return_data_regno(int x) { return x; }
}
else
{
    enum PERSONALITY_FUNCTION = "__gdc_personality_v0";
}

/**
 * The "personality" function, specific to each language.
 */
static if (GNU_ARM_EABI_Unwinder)
{
    pragma(mangle, PERSONALITY_FUNCTION)
    @personality_fn_attributes
    extern(C) _Unwind_Reason_Code gdc_personality(_Unwind_State state,
                                                  _Unwind_Exception* unwindHeader,
                                                  _Unwind_Context* context)
    {
        _Unwind_Action actions;

        switch (state & _US_ACTION_MASK)
        {
            case _US_VIRTUAL_UNWIND_FRAME:
                // If the unwind state pattern is (_US_VIRTUAL_UNWIND_FRAME | _US_FORCE_UNWIND)
                // then we don't need to search for any handler as it is not a real exception.
                // Just unwind the stack.
                if (state & _US_FORCE_UNWIND)
                    return CONTINUE_UNWINDING(unwindHeader, context);
                actions = _UA_SEARCH_PHASE;
                break;

            case _US_UNWIND_FRAME_STARTING:
                actions = _UA_CLEANUP_PHASE;
                if (!(state & _US_FORCE_UNWIND)
                    && unwindHeader.barrier_cache.sp == _Unwind_GetGR(context, UNWIND_STACK_REG))
                    actions |= _UA_HANDLER_FRAME;
                break;

            case _US_UNWIND_FRAME_RESUME:
                return CONTINUE_UNWINDING(unwindHeader, context);

            default:
                terminate("unwind error", __LINE__);
        }
        actions |= state & _US_FORCE_UNWIND;

        // The dwarf unwinder assumes the context structure holds things like
        // the function and LSDA pointers.  The ARM implementation caches these
        // in the exception header (UCB).  To avoid rewriting everything we make
        // the virtual IP register point at the UCB.
        _Unwind_SetGR(context, UNWIND_POINTER_REG, cast(_Unwind_Ptr)unwindHeader);

        return __gdc_personality(actions, unwindHeader.exception_class,
                                 unwindHeader, context);
    }
}
else
{
    pragma(mangle, PERSONALITY_FUNCTION)
    extern(C) _Unwind_Reason_Code gdc_personality(int iversion,
                                                  _Unwind_Action actions,
                                                  _Unwind_Exception_Class exceptionClass,
                                                  _Unwind_Exception* unwindHeader,
                                                  _Unwind_Context* context)
    {
        // Interface version check.
        if (iversion != 1)
            return _URC_FATAL_PHASE1_ERROR;

        return __gdc_personality(actions, exceptionClass, unwindHeader, context);
    }
}

@personality_fn_attributes
private _Unwind_Reason_Code __gdc_personality(_Unwind_Action actions,
                                              _Unwind_Exception_Class exceptionClass,
                                              _Unwind_Exception* unwindHeader,
                                              _Unwind_Context* context)
{
    const(ubyte)* lsda;
    _Unwind_Ptr landingPad;
    _Unwind_Word cfa;
    int handler;

    // Shortcut for phase 2 found handler for domestic exception.
    if (actions == (_UA_CLEANUP_PHASE | _UA_HANDLER_FRAME)
        && isGdcExceptionClass(exceptionClass))
    {
        ExceptionHeader.restore(unwindHeader, handler, lsda, landingPad, cfa);
        // Shouldn't have cached a null landing pad in phase 1.
        if (landingPad == 0)
            terminate("unwind error", __LINE__);
    }
    else
    {
        lsda = cast(ubyte*)_Unwind_GetLanguageSpecificData(context);

        static if (GNU_ARM_EABI_Unwinder)
            cfa = _Unwind_GetGR(context, UNWIND_STACK_REG);
        else
            cfa = _Unwind_GetCFA(context);

        auto result = scanLSDA(lsda, exceptionClass, actions, unwindHeader,
                               context, cfa, landingPad, handler);

        // Positive on handler found in phase 1, continue unwinding, or failure.
        if (result)
            return result;
    }

    // Unexpected negative handler, call terminate directly.
    if (handler < 0)
        terminate("unwind error", __LINE__);

    // We can't use any of the deh routines with foreign exceptions,
    // because they all expect unwindHeader to be an ExceptionHeader.
    if (isGdcExceptionClass(exceptionClass))
    {
        // If there are any in-flight exceptions being thrown, chain our
        // current object onto the end of the prevous object.
        ExceptionHeader* eh = ExceptionHeader.toExceptionHeader(unwindHeader);
        auto currentLsd = lsda;
        auto currentCfa = cfa;
        bool bypassed = false;

        while (eh.next)
        {
            ExceptionHeader* ehn = eh.next;
            const(ubyte)* nextLsd;
            _Unwind_Ptr nextLandingPad;
            _Unwind_Word nextCfa;
            int nextHandler;

            ExceptionHeader.restore(&ehn.unwindHeader, nextHandler, nextLsd, nextLandingPad, nextCfa);

            Error e = cast(Error)eh.object;
            if (e !is null && !cast(Error)ehn.object)
            {
                // We found an Error, bypass the exception chain.
                currentLsd = nextLsd;
                currentCfa = nextCfa;
                eh = ehn;
                bypassed = true;
                continue;
            }

            // Don't combine when the exceptions are from different functions.
            if (currentLsd != nextLsd && currentCfa != nextCfa)
                break;

            // Add our object onto the end of the existing chain.
            Throwable n = ehn.object;
            while (n.next)
                n = n.next;
            n.next = eh.object;

            // Replace our exception object with in-flight one
            eh.object = ehn.object;
            if (nextHandler != handler && !bypassed)
            {
                handler = nextHandler;
                ExceptionHeader.save(unwindHeader, cfa, handler, lsda, landingPad);
            }

            // Exceptions chained, can now throw away the previous header.
            eh.next = ehn.next;
            _Unwind_DeleteException(&ehn.unwindHeader);
        }

        if (bypassed)
        {
            eh = ExceptionHeader.toExceptionHeader(unwindHeader);
            Error e = cast(Error)eh.object;
            auto ehn = eh.next;
            e.bypassedException = ehn.object;
            eh.next = ehn.next;
            _Unwind_DeleteException(&ehn.unwindHeader);
        }
    }

    // Set up registers and jump to cleanup or handler.
    // For targets with pointers smaller than the word size, we must extend the
    // pointer, and this extension is target dependent.
    _Unwind_SetGR(context, __builtin_eh_return_data_regno(0),
                  cast(_Unwind_Ptr)unwindHeader);
    _Unwind_SetGR(context, __builtin_eh_return_data_regno(1), handler);
    _Unwind_SetIP(context, landingPad);

    return _URC_INSTALL_CONTEXT;
}

/**
 * The exception module defines all system-level exceptions and provides a
 * mechanism to alter system-level error handling.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2013.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly and Jonathan M Davis
 * Source:    $(DRUNTIMESRC core/_exception.d)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
module core.exception;

/**
 * Thrown on a range error.
 */
class RangeError : Error
{
    @safe pure nothrow this( string file = __FILE__, size_t line = __LINE__, Throwable next = null )
    {
        super( "Range violation", file, line, next );
    }
}

unittest
{
    {
        auto re = new RangeError();
        assert(re.file == __FILE__);
        assert(re.line == __LINE__ - 2);
        assert(re.next is null);
        assert(re.msg == "Range violation");
    }

    {
        auto re = new RangeError("hello", 42, new Exception("It's an Exception!"));
        assert(re.file == "hello");
        assert(re.line == 42);
        assert(re.next !is null);
        assert(re.msg == "Range violation");
    }
}


/**
 * Thrown on an assert error.
 */
class AssertError : Error
{
    @safe pure nothrow this( string file, size_t line )
    {
        this(cast(Throwable)null, file, line);
    }

    @safe pure nothrow this( Throwable next, string file = __FILE__, size_t line = __LINE__ )
    {
        this( "Assertion failure", file, line, next);
    }

    @safe pure nothrow this( string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null )
    {
        super( msg, file, line, next );
    }
}

unittest
{
    {
        auto ae = new AssertError("hello", 42);
        assert(ae.file == "hello");
        assert(ae.line == 42);
        assert(ae.next is null);
        assert(ae.msg == "Assertion failure");
    }

    {
        auto ae = new AssertError(new Exception("It's an Exception!"));
        assert(ae.file == __FILE__);
        assert(ae.line == __LINE__ - 2);
        assert(ae.next !is null);
        assert(ae.msg == "Assertion failure");
    }

    {
        auto ae = new AssertError(new Exception("It's an Exception!"), "hello", 42);
        assert(ae.file == "hello");
        assert(ae.line == 42);
        assert(ae.next !is null);
        assert(ae.msg == "Assertion failure");
    }

    {
        auto ae = new AssertError("msg");
        assert(ae.file == __FILE__);
        assert(ae.line == __LINE__ - 2);
        assert(ae.next is null);
        assert(ae.msg == "msg");
    }

    {
        auto ae = new AssertError("msg", "hello", 42);
        assert(ae.file == "hello");
        assert(ae.line == 42);
        assert(ae.next is null);
        assert(ae.msg == "msg");
    }

    {
        auto ae = new AssertError("msg", "hello", 42, new Exception("It's an Exception!"));
        assert(ae.file == "hello");
        assert(ae.line == 42);
        assert(ae.next !is null);
        assert(ae.msg == "msg");
    }
}


/**
 * Thrown on finalize error.
 */
class FinalizeError : Error
{
    TypeInfo   info;

    this( TypeInfo ci, Throwable next, string file = __FILE__, size_t line = __LINE__ ) @safe pure nothrow @nogc
    {
        this(ci, file, line, next);
    }

    this( TypeInfo ci, string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow @nogc
    {
        super( "Finalization error", file, line, next );
        super.info = SuppressTraceInfo.instance;
        info = ci;
    }

    override string toString() const @safe
    {
        return "An exception was thrown while finalizing an instance of " ~ info.toString();
    }
}

unittest
{
    ClassInfo info = new ClassInfo;
    info.name = "testInfo";

    {
        auto fe = new FinalizeError(info);
        assert(fe.file == __FILE__);
        assert(fe.line == __LINE__ - 2);
        assert(fe.next is null);
        assert(fe.msg == "Finalization error");
        assert(fe.info == info);
    }

    {
        auto fe = new FinalizeError(info, new Exception("It's an Exception!"));
        assert(fe.file == __FILE__);
        assert(fe.line == __LINE__ - 2);
        assert(fe.next !is null);
        assert(fe.msg == "Finalization error");
        assert(fe.info == info);
    }

    {
        auto fe = new FinalizeError(info, "hello", 42);
        assert(fe.file == "hello");
        assert(fe.line == 42);
        assert(fe.next is null);
        assert(fe.msg == "Finalization error");
        assert(fe.info == info);
    }

    {
        auto fe = new FinalizeError(info, "hello", 42, new Exception("It's an Exception!"));
        assert(fe.file == "hello");
        assert(fe.line == 42);
        assert(fe.next !is null);
        assert(fe.msg == "Finalization error");
        assert(fe.info == info);
    }
}

/**
 * Thrown on hidden function error.
 * $(RED Deprecated.
 *   This feature is not longer part of the language.)
 */
deprecated class HiddenFuncError : Error
{
    @safe pure nothrow this( ClassInfo ci )
    {
        super( "Hidden method called for " ~ ci.name );
    }
}

deprecated unittest
{
    ClassInfo info = new ClassInfo;
    info.name = "testInfo";

    {
        auto hfe = new HiddenFuncError(info);
        assert(hfe.next is null);
        assert(hfe.msg == "Hidden method called for testInfo");
    }
}


/**
 * Thrown on an out of memory error.
 */
class OutOfMemoryError : Error
{
    this(string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow @nogc
    {
        this(true, file, line, next);
    }

    this(bool trace, string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow @nogc
    {
        super("Memory allocation failed", file, line, next);
        if (!trace)
            this.info = SuppressTraceInfo.instance;
    }

    override string toString() const @trusted
    {
        return msg.length ? (cast()this).superToString() : "Memory allocation failed";
    }

    // kludge to call non-const super.toString
    private string superToString() @trusted
    {
        return super.toString();
    }
}

unittest
{
    {
        auto oome = new OutOfMemoryError();
        assert(oome.file == __FILE__);
        assert(oome.line == __LINE__ - 2);
        assert(oome.next is null);
        assert(oome.msg == "Memory allocation failed");
        assert(oome.toString.length);
    }

    {
        auto oome = new OutOfMemoryError("hello", 42, new Exception("It's an Exception!"));
        assert(oome.file == "hello");
        assert(oome.line == 42);
        assert(oome.next !is null);
        assert(oome.msg == "Memory allocation failed");
    }
}


/**
 * Thrown on an invalid memory operation.
 *
 * An invalid memory operation error occurs in circumstances when the garbage
 * collector has detected an operation it cannot reliably handle. The default
 * D GC is not re-entrant, so this can happen due to allocations done from
 * within finalizers called during a garbage collection cycle.
 */
class InvalidMemoryOperationError : Error
{
    this(string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow @nogc
    {
        super( "Invalid memory operation", file, line, next );
        this.info = SuppressTraceInfo.instance;
    }

    override string toString() const @trusted
    {
        return msg.length ? (cast()this).superToString() : "Invalid memory operation";
    }

    // kludge to call non-const super.toString
    private string superToString() @trusted
    {
        return super.toString();
    }
}

unittest
{
    {
        auto oome = new InvalidMemoryOperationError();
        assert(oome.file == __FILE__);
        assert(oome.line == __LINE__ - 2);
        assert(oome.next is null);
        assert(oome.msg == "Invalid memory operation");
        assert(oome.toString.length);
    }

    {
        auto oome = new InvalidMemoryOperationError("hello", 42, new Exception("It's an Exception!"));
        assert(oome.file == "hello");
        assert(oome.line == 42);
        assert(oome.next !is null);
        assert(oome.msg == "Invalid memory operation");
    }
}


/**
 * Thrown on a switch error.
 */
class SwitchError : Error
{
    @safe pure nothrow this( string file = __FILE__, size_t line = __LINE__, Throwable next = null )
    {
        super( "No appropriate switch clause found", file, line, next );
    }
}

unittest
{
    {
        auto se = new SwitchError();
        assert(se.file == __FILE__);
        assert(se.line == __LINE__ - 2);
        assert(se.next is null);
        assert(se.msg == "No appropriate switch clause found");
    }

    {
        auto se = new SwitchError("hello", 42, new Exception("It's an Exception!"));
        assert(se.file == "hello");
        assert(se.line == 42);
        assert(se.next !is null);
        assert(se.msg == "No appropriate switch clause found");
    }
}


/**
 * Thrown on a unicode conversion error.
 */
class UnicodeException : Exception
{
    size_t idx;

    this( string msg, size_t idx, string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow
    {
        super( msg, file, line, next );
        this.idx = idx;
    }
}

unittest
{
    {
        auto ue = new UnicodeException("msg", 2);
        assert(ue.file == __FILE__);
        assert(ue.line == __LINE__ - 2);
        assert(ue.next is null);
        assert(ue.msg == "msg");
        assert(ue.idx == 2);
    }

    {
        auto ue = new UnicodeException("msg", 2, "hello", 42, new Exception("It's an Exception!"));
        assert(ue.file == "hello");
        assert(ue.line == 42);
        assert(ue.next !is null);
        assert(ue.msg == "msg");
        assert(ue.idx == 2);
    }
}


///////////////////////////////////////////////////////////////////////////////
// Overrides
///////////////////////////////////////////////////////////////////////////////


// NOTE: One assert handler is used for all threads.  Thread-local
//       behavior should occur within the handler itself.  This delegate
//       is __gshared for now based on the assumption that it will only
//       set by the main thread during program initialization.
private __gshared AssertHandler _assertHandler = null;


/**
Gets/sets assert hander. null means the default handler is used.
*/
alias AssertHandler = void function(string file, size_t line, string msg) nothrow;

/// ditto
@property AssertHandler assertHandler() @trusted nothrow @nogc
{
    return _assertHandler;
}

/// ditto
@property void assertHandler(AssertHandler handler) @trusted nothrow @nogc
{
    _assertHandler = handler;
}

/**
 * Overrides the default assert hander with a user-supplied version.
 * $(RED Deprecated.
 *   Please use $(LREF assertHandler) instead.)
 *
 * Params:
 *  h = The new assert handler.  Set to null to use the default handler.
 */
deprecated void setAssertHandler( AssertHandler h ) @trusted nothrow @nogc
{
    assertHandler = h;
}


///////////////////////////////////////////////////////////////////////////////
// Overridable Callbacks
///////////////////////////////////////////////////////////////////////////////


/**
 * A callback for assert errors in D.  The user-supplied assert handler will
 * be called if one has been supplied, otherwise an $(LREF AssertError) will be
 * thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 */
extern (C) void onAssertError( string file = __FILE__, size_t line = __LINE__ ) nothrow
{
    if ( _assertHandler is null )
        throw new AssertError( file, line );
    _assertHandler( file, line, null);
}


/**
 * A callback for assert errors in D.  The user-supplied assert handler will
 * be called if one has been supplied, otherwise an $(LREF AssertError) will be
 * thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *  msg  = An error message supplied by the user.
 */
extern (C) void onAssertErrorMsg( string file, size_t line, string msg ) nothrow
{
    if ( _assertHandler is null )
        throw new AssertError( msg, file, line );
    _assertHandler( file, line, msg );
}


/**
 * A callback for unittest errors in D.  The user-supplied unittest handler
 * will be called if one has been supplied, otherwise the error will be
 * written to stderr.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *  msg  = An error message supplied by the user.
 */
extern (C) void onUnittestErrorMsg( string file, size_t line, string msg ) nothrow
{
    onAssertErrorMsg( file, line, msg );
}


///////////////////////////////////////////////////////////////////////////////
// Internal Error Callbacks
///////////////////////////////////////////////////////////////////////////////

/**
 * A callback for array bounds errors in D.  A $(LREF RangeError) will be thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF RangeError).
 */
extern (C) void onRangeError( string file = __FILE__, size_t line = __LINE__ ) @safe pure nothrow
{
    throw new RangeError( file, line, null );
}


/**
 * A callback for finalize errors in D.  A $(LREF FinalizeError) will be thrown.
 *
 * Params:
 *  info = The TypeInfo instance for the object that failed finalization.
 *  e = The exception thrown during finalization.
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF FinalizeError).
 */
extern (C) void onFinalizeError( TypeInfo info, Throwable e, string file = __FILE__, size_t line = __LINE__ ) @trusted nothrow
{
    // This error is thrown during a garbage collection, so no allocation must occur while
    //  generating this object. So we use a preallocated instance
    throw staticError!FinalizeError(info, e, file, line);
}


/**
 * A callback for hidden function errors in D.  A $(LREF HiddenFuncError) will be
 * thrown.
 * $(RED Deprecated.
 *   This feature is not longer part of the language.)
 *
 * Throws:
 *  $(LREF HiddenFuncError).
 */
deprecated extern (C) void onHiddenFuncError( Object o ) @safe pure nothrow
{
    throw new HiddenFuncError( typeid(o) );
}


/**
 * A callback for out of memory errors in D.  An $(LREF OutOfMemoryError) will be
 * thrown.
 *
 * Throws:
 *  $(LREF OutOfMemoryError).
 */
extern (C) void onOutOfMemoryError(void* pretend_sideffect = null) @trusted pure nothrow @nogc /* dmd @@@BUG11461@@@ */
{
    // NOTE: Since an out of memory condition exists, no allocation must occur
    //       while generating this object.
    throw staticError!OutOfMemoryError();
}

extern (C) void onOutOfMemoryErrorNoGC() @trusted nothrow @nogc
{
    // suppress stacktrace until they are @nogc
    throw staticError!OutOfMemoryError(false);
}


/**
 * A callback for invalid memory operations in D.  An
 * $(LREF InvalidMemoryOperationError) will be thrown.
 *
 * Throws:
 *  $(LREF InvalidMemoryOperationError).
 */
extern (C) void onInvalidMemoryOperationError(void* pretend_sideffect = null) @trusted pure nothrow @nogc /* dmd @@@BUG11461@@@ */
{
    // The same restriction applies as for onOutOfMemoryError. The GC is in an
    // undefined state, thus no allocation must occur while generating this object.
    throw staticError!InvalidMemoryOperationError();
}


/**
 * A callback for switch errors in D.  A $(LREF SwitchError) will be thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF SwitchError).
 */
extern (C) void onSwitchError( string file = __FILE__, size_t line = __LINE__ ) @safe pure nothrow
{
    throw new SwitchError( file, line, null );
}


/**
 * A callback for unicode errors in D.  A $(LREF UnicodeException) will be thrown.
 *
 * Params:
 *  msg = Information about the error.
 *  idx = String index where this error was detected.
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF UnicodeException).
 */
extern (C) void onUnicodeError( string msg, size_t idx, string file = __FILE__, size_t line = __LINE__ ) @safe pure
{
    throw new UnicodeException( msg, idx, file, line );
}

/***********************************
 * These functions must be defined for any D program linked
 * against this library.
 */
/+
extern (C) void onAssertError(string file, size_t line);
extern (C) void onAssertErrorMsg(string file, size_t line, string msg);
extern (C) void onUnittestErrorMsg(string file, size_t line, string msg);
extern (C) void onRangeError(string file, size_t line);
extern (C) void onHiddenFuncError(Object o);
extern (C) void onSwitchError(string file, size_t line);
+/

/***********************************
 * Function calls to these are generated by the compiler and inserted into
 * the object code.
 */

extern (C)
{
    // Use ModuleInfo to get file name for "m" versions

    /* One of these three is called upon an assert() fail.
     */
    void _d_assertp(immutable(char)* file, uint line)
    {
        import core.stdc.string : strlen;
        onAssertError(file[0 .. strlen(file)], line);
    }

    void _d_assert_msg(string msg, string file, uint line)
    {
        onAssertErrorMsg(file, line, msg);
    }

    void _d_assert(string file, uint line)
    {
        onAssertError(file, line);
    }

    /* One of these three is called upon an assert() fail inside of a unittest block
     */
    void _d_unittestp(immutable(char)* file, uint line)
    {
        import core.stdc.string : strlen;
        _d_unittest(file[0 .. strlen(file)], line);
    }

    void _d_unittest_msg(string msg, string file, uint line)
    {
        onUnittestErrorMsg(file, line, msg);
    }

    void _d_unittest(string file, uint line)
    {
        _d_unittest_msg("unittest failure", file, line);
    }

    /* Called when an array index is out of bounds
     */
    void _d_arrayboundsp(immutable(char*) file, uint line)
    {
        import core.stdc.string : strlen;
        onRangeError(file[0 .. strlen(file)], line);
    }

    void _d_arraybounds(string file, uint line)
    {
        onRangeError(file, line);
    }

    /* Called when a switch statement has no DefaultStatement, yet none of the cases match
     */
    void _d_switch_errorm(immutable(ModuleInfo)* m, uint line)
    {
        onSwitchError(m.name, line);
    }

    void _d_switch_error(string file, uint line)
    {
        onSwitchError(file, line);
    }
}

// TLS storage shared for all errors, chaining might create circular reference
private void[128] _store;

// only Errors for now as those are rarely chained
private T staticError(T, Args...)(auto ref Args args)
    if (is(T : Error))
{
    // pure hack, what we actually need is @noreturn and allow to call that in pure functions
    static T get()
    {
        static assert(__traits(classInstanceSize, T) <= _store.length,
                      T.stringof ~ " is too large for staticError()");

        _store[0 .. __traits(classInstanceSize, T)] = typeid(T).initializer[];
        return cast(T) _store.ptr;
    }
    auto res = (cast(T function() @trusted pure nothrow @nogc) &get)();
    res.__ctor(args);
    return res;
}

// Suppress traceinfo generation when the GC cannot be used.  Workaround for
// Bugzilla 14993. We should make stack traces @nogc instead.
package class SuppressTraceInfo : Throwable.TraceInfo
{
    override int opApply(scope int delegate(ref const(char[]))) const { return 0; }
    override int opApply(scope int delegate(ref size_t, ref const(char[]))) const { return 0; }
    override string toString() const { return null; }
    static SuppressTraceInfo instance() @trusted @nogc pure nothrow
    {
        static immutable SuppressTraceInfo it = new SuppressTraceInfo;
        return cast(SuppressTraceInfo)it;
    }
}

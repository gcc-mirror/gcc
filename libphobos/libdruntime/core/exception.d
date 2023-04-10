/**
    The exception module defines all system-level exceptions and provides a
    mechanism to alter system-level error handling.

    Copyright: Copyright Sean Kelly 2005 - 2013.
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Sean Kelly and $(HTTP jmdavisprog.com, Jonathan M Davis)
    Source:    $(DRUNTIMESRC core/_exception.d)
 */
module core.exception;

// Compiler lowers final switch default case to this (which is a runtime error)
void __switch_errorT()(string file = __FILE__, size_t line = __LINE__) @trusted
{
    // Consider making this a compile time check.
    version (D_Exceptions)
        throw staticError!SwitchError("No appropriate switch clause found", file, line, null);
    else
        assert(0, "No appropriate switch clause found");
}

/**
 * Thrown on a range error.
 */
class RangeError : Error
{
    this( string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @nogc nothrow pure @safe
    {
        super( "Range violation", file, line, next );
    }

    protected this( string msg, string file, size_t line, Throwable next = null ) @nogc nothrow pure @safe
    {
        super( msg, file, line, next );
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
 * Thrown when an out of bounds array index is accessed.
 */
class ArrayIndexError : RangeError
{
    /// Index into array
    const size_t index;
    /// Length of indexed array
    const size_t length;

    // Buffer to avoid GC allocations
    private immutable char[100] msgBuf = '\0';

    this(size_t index, size_t length, string file = __FILE__,
         size_t line = __LINE__, Throwable next = null) @nogc nothrow pure @safe
    {
        this.index  = index;
        this.length = length;

        // Constructing the message is a bit clumsy:
        // It's essentially `printf("index [%zu] is out of bounds for array of length [%zu]", index, length)`,
        // but even `snprintf` isn't `pure`.
        // Also string concatenation isn't `@nogc`, and casting to/from immutable isn't `@safe`
        import core.internal.string : unsignedToTempString;
        char[msgBuf.length] buf = void;
        char[20] tmpBuf = void;
        char[] sink = buf[];
        sink.rangeMsgPut("index [");
        sink.rangeMsgPut(unsignedToTempString!10(index, tmpBuf));
        sink.rangeMsgPut("] is out of bounds for array of length ");
        sink.rangeMsgPut(unsignedToTempString!10(length, tmpBuf));
        this.msgBuf = buf;
        super(msgBuf[0..$-sink.length], file, line, next);
    }
}

@safe pure unittest
{
    assert(new ArrayIndexError(900, 700).msg == "index [900] is out of bounds for array of length 700");
    // Ensure msg buffer doesn't overflow on large numbers
    assert(new ArrayIndexError(size_t.max, size_t.max-1).msg);
}

unittest
{
    try
    {
        _d_arraybounds_indexp("test", 400, 9, 3);
        assert(0, "no ArrayIndexError thrown");
    }
    catch (ArrayIndexError re)
    {
        assert(re.file   == "test");
        assert(re.line   == 400);
        assert(re.index  == 9);
        assert(re.length == 3);
    }
}

/**
 * Thrown when an out of bounds array slice is created
 */
class ArraySliceError : RangeError
{
    /// Lower/upper bound passed to slice: `array[lower .. upper]`
    const size_t lower, upper;
    /// Length of sliced array
    const size_t length;

    private immutable char[120] msgBuf = '\0';

    this(size_t lower, size_t upper, size_t length, string file = __FILE__,
         size_t line = __LINE__, Throwable next = null) @nogc nothrow pure @safe
    {
        this.lower  = lower;
        this.upper  = upper;
        this.length = length;

        // Constructing the message is a bit clumsy for the same reasons as ArrayIndexError
        import core.internal.string : unsignedToTempString;
        char[msgBuf.length] buf = void;
        char[20] tmpBuf = void;
        char[] sink = buf;
        sink.rangeMsgPut("slice [");
        sink.rangeMsgPut(unsignedToTempString!10(lower, tmpBuf));
        sink.rangeMsgPut(" .. ");
        sink.rangeMsgPut(unsignedToTempString!10(upper, tmpBuf));
        sink.rangeMsgPut("] ");
        if (lower > upper)
        {
            sink.rangeMsgPut("has a larger lower index than upper index");
        }
        else
        {
            sink.rangeMsgPut("extends past source array of length ");
            sink.rangeMsgPut(unsignedToTempString!10(length, tmpBuf));
        }

        this.msgBuf = buf;
        super(msgBuf[0..$-sink.length], file, line, next);
    }
}

@safe pure unittest
{
    assert(new ArraySliceError(40, 80, 20).msg == "slice [40 .. 80] extends past source array of length 20");
    assert(new ArraySliceError(90, 70, 20).msg == "slice [90 .. 70] has a larger lower index than upper index");
    // Ensure msg buffer doesn't overflow on large numbers
    assert(new ArraySliceError(size_t.max, size_t.max, size_t.max-1).msg);
}

unittest
{
    try
    {
        _d_arraybounds_slicep("test", 400, 1, 7, 3);
        assert(0, "no ArraySliceError thrown");
    }
    catch (ArraySliceError re)
    {
        assert(re.file   == "test");
        assert(re.line   == 400);
        assert(re.lower  == 1);
        assert(re.upper  == 7);
        assert(re.length == 3);
    }
}

/// Mini `std.range.primitives: put` for constructor of ArraySliceError / ArrayIndexError
private void rangeMsgPut(ref char[] r, scope const(char)[] e) @nogc nothrow pure @safe
{
    assert(r.length >= e.length); // don't throw ArraySliceError inside ArrayIndexError ctor
    r[0 .. e.length] = e[];
    r = r[e.length .. $];
}

/**
 * Thrown on an assert error.
 */
class AssertError : Error
{
    @safe pure nothrow @nogc this( string file, size_t line )
    {
        this(cast(Throwable)null, file, line);
    }

    @safe pure nothrow @nogc this( Throwable next, string file = __FILE__, size_t line = __LINE__ )
    {
        this( "Assertion failure", file, line, next);
    }

    @safe pure nothrow @nogc this( string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null )
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
* Thrown on a configuration error.
*/
class ForkError : Error
{
    this( string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @nogc nothrow pure @safe
    {
        super( "fork() failed", file, line, next );
    }
}


/**
 * Thrown on a switch error.
 */
class SwitchError : Error
{
    @safe pure nothrow @nogc this( string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null )
    {
        super( msg, file, line, next );
    }
}

unittest
{
    {
        auto se = new SwitchError("No appropriate switch clause found");
        assert(se.file == __FILE__);
        assert(se.line == __LINE__ - 2);
        assert(se.next is null);
        assert(se.msg == "No appropriate switch clause found");
    }

    {
        auto se = new SwitchError("No appropriate switch clause found", "hello", 42, new Exception("It's an Exception!"));
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

    this( string msg, size_t idx, string file = __FILE__, size_t line = __LINE__, Throwable next = null ) @safe pure nothrow @nogc
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
        throw staticError!AssertError(file, line);
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
        throw staticError!AssertError(msg, file, line);
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
 * A callback for general array bounds errors in D. A $(LREF RangeError) will be thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF RangeError).
 */
extern (C) noreturn onRangeError( string file = __FILE__, size_t line = __LINE__ ) @trusted pure nothrow @nogc
{
    throw staticError!RangeError(file, line, null);
}

/**
 * A callback for array slice out of bounds errors in D.
 *
 * Params:
 *  lower  = the lower bound of the index passed of a slice
 *  upper  = the upper bound of the index passed of a slice or the index if not a slice
 *  length = length of the array
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF ArraySliceError).
 */
extern (C) noreturn onArraySliceError( size_t lower = 0, size_t upper = 0, size_t length = 0,
                              string file = __FILE__, size_t line = __LINE__ ) @trusted pure nothrow @nogc
{
    throw staticError!ArraySliceError(lower, upper, length, file, line, null);
}

/**
 * A callback for array index out of bounds errors in D.
 *
 * Params:
 *  index  = index in the array
 *  length = length of the array
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF ArrayIndexError).
 */
extern (C) noreturn onArrayIndexError( size_t index = 0, size_t length = 0,
                              string file = __FILE__, size_t line = __LINE__ ) @trusted pure nothrow @nogc
{
    throw staticError!ArrayIndexError(index, length, file, line, null);
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
extern (C) noreturn onFinalizeError( TypeInfo info, Throwable e, string file = __FILE__, size_t line = __LINE__ ) @trusted nothrow
{
    // This error is thrown during a garbage collection, so no allocation must occur while
    //  generating this object. So we use a preallocated instance
    throw staticError!FinalizeError(info, e, file, line);
}

version (D_BetterC)
{
    // When compiling with -betterC we use template functions so if they are
    // used the bodies are copied into the user's program so there is no need
    // for the D runtime during linking.

    // In the future we might want to convert all functions in this module to
    // templates even for ordinary builds instead of providing them as an
    // extern(C) library.

    noreturn onOutOfMemoryError()(void* pretend_sideffect = null) @nogc nothrow pure @trusted
    {
        assert(0, "Memory allocation failed");
    }
    alias onOutOfMemoryErrorNoGC = onOutOfMemoryError;

    noreturn onInvalidMemoryOperationError()(void* pretend_sideffect = null) @nogc nothrow pure @trusted
    {
        assert(0, "Invalid memory operation");
    }
}
else
{
    /**
     * A callback for out of memory errors in D.  An $(LREF OutOfMemoryError) will be
     * thrown.
     *
     * Throws:
     *  $(LREF OutOfMemoryError).
     */
    extern (C) noreturn onOutOfMemoryError(void* pretend_sideffect = null) @trusted pure nothrow @nogc /* dmd @@@BUG11461@@@ */
    {
        // NOTE: Since an out of memory condition exists, no allocation must occur
        //       while generating this object.
        throw staticError!OutOfMemoryError();
    }

    extern (C) noreturn onOutOfMemoryErrorNoGC() @trusted nothrow @nogc
    {
        // suppress stacktrace until they are @nogc
        throw staticError!OutOfMemoryError(false);
    }
}

/**
 * A callback for invalid memory operations in D.  An
 * $(LREF InvalidMemoryOperationError) will be thrown.
 *
 * Throws:
 *  $(LREF InvalidMemoryOperationError).
 */
extern (C) noreturn onInvalidMemoryOperationError(void* pretend_sideffect = null) @trusted pure nothrow @nogc /* dmd @@@BUG11461@@@ */
{
    // The same restriction applies as for onOutOfMemoryError. The GC is in an
    // undefined state, thus no allocation must occur while generating this object.
    throw staticError!InvalidMemoryOperationError();
}


/**
 * A callback for errors in the case of a failed fork in D.  A $(LREF ForkError) will be thrown.
 *
 * Params:
 *  file = The name of the file that signaled this error.
 *  line = The line number on which this error occurred.
 *
 * Throws:
 *  $(LREF ConfigurationError).
 */
extern (C) noreturn onForkError( string file = __FILE__, size_t line = __LINE__ ) @trusted pure nothrow @nogc
{
    throw staticError!ForkError( file, line, null );
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
extern (C) noreturn onUnicodeError( string msg, size_t idx, string file = __FILE__, size_t line = __LINE__ ) @safe pure
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
+/

/***********************************
 * Function calls to these are generated by the compiler and inserted into
 * the object code.
 */

extern (C)
{
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

    /// Called when an invalid array index/slice or associative array key is accessed
    void _d_arrayboundsp(immutable(char*) file, uint line)
    {
        import core.stdc.string : strlen;
        onRangeError(file[0 .. strlen(file)], line);
    }

    /// ditto
    void _d_arraybounds(string file, uint line)
    {
        onRangeError(file, line);
    }

    /// Called when an out of range slice of an array is created
    void _d_arraybounds_slicep(immutable(char*) file, uint line, size_t lower, size_t upper, size_t length)
    {
        import core.stdc.string : strlen;
        onArraySliceError(lower, upper, length, file[0 .. strlen(file)], line);
    }

    /// ditto
    void _d_arraybounds_slice(string file, uint line, size_t lower, size_t upper, size_t length)
    {
        onArraySliceError(lower, upper, length, file, line);
    }

    /// Called when an out of range array index is accessed
    void _d_arraybounds_indexp(immutable(char*) file, uint line, size_t index, size_t length)
    {
        import core.stdc.string : strlen;
        onArrayIndexError(index, length, file[0 .. strlen(file)], line);
    }

    /// ditto
    void _d_arraybounds_index(string file, uint line, size_t index, size_t length)
    {
        onArrayIndexError(index, length, file, line);
    }
}

// TLS storage shared for all errors, chaining might create circular reference
private align(2 * size_t.sizeof) void[256] _store;

// only Errors for now as those are rarely chained
package T staticError(T, Args...)(auto ref Args args)
    if (is(T : Error))
{
    // pure hack, what we actually need is @noreturn and allow to call that in pure functions
    static T get()
    {
        static assert(__traits(classInstanceSize, T) <= _store.length,
                      T.stringof ~ " is too large for staticError()");

        return cast(T) _store.ptr;
    }
    auto res = (cast(T function() @trusted pure nothrow @nogc) &get)();
    import core.lifetime : emplace;
    emplace(res, args);
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

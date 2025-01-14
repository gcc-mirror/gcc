/**
 * The demangle module converts mangled D symbols to a representation similar
 * to what would have existed in code.
 *
 * Copyright: Copyright Sean Kelly 2010 - 2014.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly
 * Source:    $(DRUNTIMESRC core/_demangle.d)
 */

module core.demangle;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

debug (trace) debug = needPrintf;
debug (info) debug = needPrintf;

debug (needPrintf)
private int printf(Args...)(scope const char* fmt, scope const Args args)
    => __ctfe ? 0 : imported!"core.stdc.stdio".printf(fmt, args);

extern (C) alias CXX_DEMANGLER = char* function (const char* mangled_name,
                                                char* output_buffer,
                                                size_t* length,
                                                int* status) nothrow pure @trusted;

private struct NoHooks
{
    // supported hooks
    // static bool parseLName(ref Demangle);
    // static char[] parseType(ref Demangle, char[])
}

private struct Demangle(Hooks = NoHooks)
{
    // NOTE: This implementation currently only works with mangled function
    //       names as they exist in an object file.  Type names mangled via
    //       the .mangleof property are effectively incomplete as far as the
    //       ABI is concerned and so are not considered to be mangled symbol
    //       names.

    // NOTE: This implementation builds the demangled buffer in place by
    //       writing data as it is decoded and then rearranging it later as
    //       needed.  In practice this results in very little data movement,
    //       and the performance cost is more than offset by the gain from
    //       not allocating dynamic memory to assemble the name piecemeal.
    //
    //       If the destination buffer is too small, parsing will restart
    //       with a larger buffer.  Since this generally means only one
    //       allocation during the course of a parsing run, this is still
    //       faster than assembling the result piecemeal.

pure @safe:
    enum AddType { no, yes }


    this( return scope const(char)[] buf_, return scope char[] dst_ = null )
    {
        this( buf_, AddType.yes, dst_ );
    }


    this( return scope const(char)[] buf_, AddType addType_, return scope char[] dst_ = null )
    {
        buf     = buf_;
        addType = addType_;
        dst.dst = dst_;
    }

    const(char)[]   buf     = null;
    Buffer          dst;
    size_t          pos     = 0;
    size_t          brp     = 0; // current back reference pos
    AddType         addType = AddType.yes;
    bool            mute    = false;
    Hooks           hooks;

    //////////////////////////////////////////////////////////////////////////
    // Type Testing and Conversion
    //////////////////////////////////////////////////////////////////////////


    static bool isAlpha( char val )
    {
        return ('a' <= val && 'z' >= val) ||
               ('A' <= val && 'Z' >= val) ||
               (0x80 & val); // treat all unicode as alphabetic
    }


    static bool isDigit( char val ) nothrow
    {
        return '0' <= val && '9' >= val;
    }


    static bool isHexDigit( char val )
    {
        return ('0' <= val && '9' >= val) ||
               ('a' <= val && 'f' >= val) ||
               ('A' <= val && 'F' >= val);
    }


    static ubyte ascii2hex( out bool errStatus, char val ) nothrow
    {
        if (val >= 'a' && val <= 'f')
            return cast(ubyte)(val - 'a' + 10);
        if (val >= 'A' && val <= 'F')
            return cast(ubyte)(val - 'A' + 10);
        if (val >= '0' && val <= '9')
            return cast(ubyte)(val - '0');

        errStatus = true;
        return 0;
    }

    BufSlice shift(scope const BufSlice val) return scope
    {
        if (mute)
            return dst.bslice_empty;
        return dst.shift(val);
    }

    void putComma(size_t n)
    {
        version (DigitalMars) pragma(inline, false);
        if (n)
            put(", ");
    }

    void put(char c) return scope
    {
        char[1] val = c;
        put(val[]);
    }

    void put(scope BufSlice val) return scope
    {
        put(val.getSlice);
    }

    void put(scope const(char)[] val) return scope nothrow
    {
        if (mute)
            return;
        dst.append(val);
    }


    void putAsHex( size_t val, int width = 0 )
    {
        import core.internal.string;

        UnsignedStringBuf buf = void;

        auto s = unsignedToTempString!16(val, buf);
        int slen = cast(int)s.length;
        if (slen < width)
        {
            foreach (i; slen .. width)
                put('0');
        }
        put(s);
    }


    void pad( const(char)[] val )
    {
        if ( val.length )
        {
            put(" ");
            put( val );
        }
    }


    void silent( out bool err_status, void delegate(out bool err_status) pure @safe nothrow dg ) nothrow
    {
        debug(trace) printf( "silent+\n" );
        debug(trace) scope(success) printf( "silent-\n" );
        auto n = dst.length;
        dg(err_status);
        if(!err_status)
            dst.len = n;
    }


    //////////////////////////////////////////////////////////////////////////
    // Parsing Utility
    //////////////////////////////////////////////////////////////////////////

    @property bool empty()
    {
        return pos >= buf.length;
    }

    @property char front()
    {
        if ( pos < buf.length )
            return buf[pos];
        return char.init;
    }

    char peek( size_t n )
    {
        if ( pos + n < buf.length )
            return buf[pos + n];
        return char.init;
    }


    bool test( char val ) nothrow
    {
        return val == front;
    }

    void popFront() nothrow
    {
        if ( pos++ >= buf.length )
            assert(false);
    }


    void popFront(int i) nothrow
    {
        while (i--)
            popFront();
    }


    bool match( char val ) nothrow
    {
        if (!test(val))
            return false;
        else
        {
            popFront();
            return true;
        }
    }

    bool match( const(char)[] val ) nothrow
    {
        foreach (char e; val )
            if (!match( e ))
                return false;

        return true;
    }


    void eat( char val )
    {
        if ( val == front )
            popFront();
    }

    bool isSymbolNameFront(out bool errStatus) nothrow
    {
        char val = front;
        if ( isDigit( val ) || val == '_' )
            return true;
        if ( val != 'Q' )
            return false;

        // check the back reference encoding after 'Q'
        val = peekBackref();
        if (val == 0)
        {
            // invalid back reference
            errStatus = true;
            return false;
        }

        return isDigit( val ); // identifier ref
    }

    // return the first character at the back reference
    char peekBackref() nothrow
    {
        assert( front == 'Q' );
        auto n = decodeBackref!1();
        if (!n || n > pos)
            return 0; // invalid back reference

        return buf[pos - n];
    }

    size_t decodeBackref(size_t peekAt = 0)() nothrow
    {
        enum base = 26;
        size_t n = 0;
        for (size_t p; ; p++)
        {
            char t;
            static if (peekAt > 0)
            {
                t = peek(peekAt + p);
            }
            else
            {
                t = front;
                popFront();
            }
            if (t < 'A' || t > 'Z')
            {
                if (t < 'a' || t > 'z')
                    return 0; // invalid back reference

                n = base * n + t - 'a';
                return n;
            }
            n = base * n + t - 'A';
        }
    }

    //////////////////////////////////////////////////////////////////////////
    // Parsing Implementation
    //////////////////////////////////////////////////////////////////////////


    /*
    Number:
        Digit
        Digit Number
    */
    const(char)[] sliceNumber() return scope
    {
        debug(trace) printf( "sliceNumber+\n" );
        debug(trace) scope(success) printf( "sliceNumber-\n" );

        auto beg = pos;

        while ( true )
        {
            auto t = front;
            if (t >= '0' && t <= '9')
                popFront();
            else
                return buf[beg .. pos];
        }
    }


    size_t decodeNumber(out bool errStatus) scope nothrow
    {
        debug(trace) printf( "decodeNumber+\n" );
        debug(trace) scope(success) printf( "decodeNumber-\n" );

        return decodeNumber( errStatus, sliceNumber() );
    }

    size_t decodeNumber( out bool errStatus, scope const(char)[] num ) scope nothrow
    {
        debug(trace) printf( "decodeNumber+\n" );
        debug(trace) scope(success) printf( "decodeNumber-\n" );

        size_t val = 0;

        foreach ( c; num )
        {
            import core.checkedint : mulu, addu;

            bool overflow = false;
            val = mulu(val, 10, overflow);
            val = addu(val, c - '0',  overflow);
            if (overflow)
            {
                errStatus = true;
                return 0;
            }
        }
        return val;
    }

    void parseReal(out bool errStatus) scope nothrow
    {
        debug(trace) printf( "parseReal+\n" );
        debug(trace) scope(success) printf( "parseReal-\n" );

        char[64] tbuf = void;
        size_t   tlen = 0;
        real     val  = void;

        void onError()
        {
            errStatus = true;
        }

        if ( 'I' == front )
        {
            if (!match("INF"))
                return onError();
            put( "real.infinity" );
            return;
        }
        if ( 'N' == front )
        {
            popFront();
            if ( 'I' == front )
            {
                if (!match("INF"))
                    return onError();
                put( "-real.infinity" );
                return;
            }
            if ( 'A' == front )
            {
                if (!match("AN"))
                    return onError();
                put( "real.nan" );
                return;
            }
            tbuf[tlen++] = '-';
        }

        tbuf[tlen++] = '0';
        tbuf[tlen++] = 'X';
        errStatus = !isHexDigit( front );
        if (errStatus)
            return; // Expected hex digit

        tbuf[tlen++] = front;
        tbuf[tlen++] = '.';
        popFront();

        while ( isHexDigit( front ) )
        {
            if (tlen >= tbuf.length)
                return onError(); // Too many hex float digits
            tbuf[tlen++] = front;
            popFront();
        }
        if (!match('P'))
            return onError();
        tbuf[tlen++] = 'p';
        if ( 'N' == front )
        {
            tbuf[tlen++] = '-';
            popFront();
        }
        else
        {
            tbuf[tlen++] = '+';
        }
        while ( isDigit( front ) )
        {
            tbuf[tlen++] = front;
            popFront();
        }

        tbuf[tlen] = 0;
        debug(info) printf( "got (%s)\n", tbuf.ptr );
        pureReprintReal( tbuf[] );
        debug(info) printf( "converted (%.*s)\n", cast(int) tlen, tbuf.ptr );
        put( tbuf[0 .. tlen] );
    }


    /*
    LName:
        Number Name

    Name:
        Namestart
        Namestart Namechars

    Namestart:
        _
        Alpha

    Namechar:
        Namestart
        Digit

    Namechars:
        Namechar
        Namechar Namechars
    */
    void parseLName(out string errMsg) scope nothrow
    {
        debug(trace) printf( "parseLName+\n" );
        debug(trace) scope(success) printf( "parseLName-\n" );

        static if (__traits(hasMember, Hooks, "parseLName"))
        {
            auto r = hooks.parseLName(errMsg, this);
            if (errMsg !is null)
                return;
            if (r) return;
        }

        void error(string msg)
        {
            errMsg = msg;
        }

        if ( front == 'Q' )
        {
            // back reference to LName
            auto refPos = pos;
            popFront();
            size_t n = decodeBackref();
            if (!n || n > refPos)
                return error("Invalid LName back reference");

            if ( !mute )
            {
                auto savePos = pos;
                scope(exit) pos = savePos;
                pos = refPos - n;
                parseLName(errMsg);
            }
            return;
        }

        bool err_flag;
        auto n = decodeNumber(err_flag);
        if (err_flag)
            return error("Number overflow");

        if ( n == 0 )
        {
            put( "__anonymous" );
            return;
        }
        if ( n > buf.length || n > buf.length - pos )
            return error("LName must be at least 1 character");

        if ( '_' != front && !isAlpha( front ) )
            return error("Invalid character in LName");

        foreach (char e; buf[pos + 1 .. pos + n] )
        {
            if ( '_' != e && !isAlpha( e ) && !isDigit( e ) )
                return error("Invalid character in LName");
        }

        put( buf[pos .. pos + n] );
        pos += n;
    }


    /*
    Type:
        Shared
        Const
        Immutable
        Wild
        TypeArray
        TypeVector
        TypeStaticArray
        TypeAssocArray
        TypePointer
        TypeFunction
        TypeIdent
        TypeClass
        TypeStruct
        TypeEnum
        TypeTypedef
        TypeDelegate
        TypeNone
        TypeVoid
        TypeNoreturn
        TypeByte
        TypeUbyte
        TypeShort
        TypeUshort
        TypeInt
        TypeUint
        TypeLong
        TypeUlong
        TypeCent
        TypeUcent
        TypeFloat
        TypeDouble
        TypeReal
        TypeIfloat
        TypeIdouble
        TypeIreal
        TypeCfloat
        TypeCdouble
        TypeCreal
        TypeBool
        TypeChar
        TypeWchar
        TypeDchar
        TypeTuple

    Shared:
        O Type

    Const:
        x Type

    Immutable:
        y Type

    Wild:
        Ng Type

    TypeArray:
        A Type

    TypeVector:
        Nh Type

    TypeStaticArray:
        G Number Type

    TypeAssocArray:
        H Type Type

    TypePointer:
        P Type

    TypeFunction:
        CallConvention FuncAttrs Arguments ArgClose Type

    TypeIdent:
        I LName

    TypeClass:
        C LName

    TypeStruct:
        S LName

    TypeEnum:
        E LName

    TypeTypedef:
        T LName

    TypeDelegate:
        D TypeFunction

    TypeNone:
        n

    TypeVoid:
        v

    TypeNoreturn
        Nn

    TypeByte:
        g

    TypeUbyte:
        h

    TypeShort:
        s

    TypeUshort:
        t

    TypeInt:
        i

    TypeUint:
        k

    TypeLong:
        l

    TypeUlong:
        m

    TypeCent
        zi

    TypeUcent
        zk

    TypeFloat:
        f

    TypeDouble:
        d

    TypeReal:
        e

    TypeIfloat:
        o

    TypeIdouble:
        p

    TypeIreal:
        j

    TypeCfloat:
        q

    TypeCdouble:
        r

    TypeCreal:
        c

    TypeBool:
        b

    TypeChar:
        a

    TypeWchar:
        u

    TypeDchar:
        w

    TypeTuple:
        B Number Arguments
    */
    BufSlice parseType(out bool errStatus) return scope nothrow
    {
        static immutable string[23] primitives = [
            "char", // a
            "bool", // b
            "creal", // c
            "double", // d
            "real", // e
            "float", // f
            "byte", // g
            "ubyte", // h
            "int", // i
            "ireal", // j
            "uint", // k
            "long", // l
            "ulong", // m
            null, // n
            "ifloat", // o
            "idouble", // p
            "cfloat", // q
            "cdouble", // r
            "short", // s
            "ushort", // t
            "wchar", // u
            "void", // v
            "dchar", // w
        ];

        static if (__traits(hasMember, Hooks, "parseType"))
        {
            auto n = hooks.parseType(errStatus, this, null);
            if (errStatus)
                return dst.bslice_empty;
            else
                if (n !is null)
                    return BufSlice(n, 0, n.length);
        }

        debug(trace) printf( "parseType+\n" );
        debug(trace) scope(success) printf( "parseType-\n" );
        auto beg = dst.length;
        auto t = front;

        BufSlice parseBackrefType(out string errStatus, scope BufSlice delegate(bool err_flag) pure @safe nothrow parseDg) pure @safe nothrow
        {
            if (pos == brp)
            {
                errStatus = "recursive back reference";
                return dst.bslice_empty;
            }

            auto refPos = pos;
            popFront();
            auto n = decodeBackref();
            if (n == 0 || n > pos)
            {
                errStatus = "invalid back reference";
                return dst.bslice_empty;
            }

            if ( mute )
                return dst.bslice_empty;
            auto savePos = pos;
            auto saveBrp = brp;
            scope(success) { pos = savePos; brp = saveBrp; }
            pos = refPos - n;
            brp = refPos;

            bool err_flag;
            auto ret = parseDg(err_flag);
            if (err_flag)
            {
                errStatus = "parseDg error";
                return dst.bslice_empty;
            }

            return ret;
        }

        // call parseType() and return error if occured
        enum parseTypeOrF = "parseType(errStatus); if (errStatus) return dst.bslice_empty;";

        switch ( t )
        {
        case 'Q': // Type back reference
            string errMsg;
            auto r = parseBackrefType(errMsg, (e_flag) => parseType(e_flag));
            if (errMsg !is null)
                return dst.bslice_empty;
            return r;
        case 'O': // Shared (O Type)
            popFront();
            put( "shared(" );
            mixin(parseTypeOrF);
            put( ')' );
            return dst[beg .. $];
        case 'x': // Const (x Type)
            popFront();
            put( "const(" );
            mixin(parseTypeOrF);
            put( ')' );
            return dst[beg .. $];
        case 'y': // Immutable (y Type)
            popFront();
            put( "immutable(" );
            mixin(parseTypeOrF);
            put( ')' );
            return dst[beg .. $];
        case 'N':
            popFront();
            switch ( front )
            {
            case 'n': // Noreturn
                popFront();
                put("noreturn");
                return dst[beg .. $];
            case 'g': // Wild (Ng Type)
                popFront();
                // TODO: Anything needed here?
                put( "inout(" );
                mixin(parseTypeOrF);
                put( ')' );
                return dst[beg .. $];
            case 'h': // TypeVector (Nh Type)
                popFront();
                put( "__vector(" );
                mixin(parseTypeOrF);
                put( ')' );
                return dst[beg .. $];
            default:
                errStatus = true;
                return dst.bslice_empty;
            }
        case 'A': // TypeArray (A Type)
            popFront();
            mixin(parseTypeOrF);
            put( "[]" );
            return dst[beg .. $];
        case 'G': // TypeStaticArray (G Number Type)
            popFront();
            auto num = sliceNumber();
            mixin(parseTypeOrF);
            put( '[' );
            put( num );
            put( ']' );
            return dst[beg .. $];
        case 'H': // TypeAssocArray (H Type Type)
            popFront();
            // skip t1
            auto tx = parseType(errStatus);
            if (errStatus)
                return dst.bslice_empty;
            mixin(parseTypeOrF);
            put( '[' );
            shift(tx);
            put( ']' );
            return dst[beg .. $];
        case 'P': // TypePointer (P Type)
            popFront();
            mixin(parseTypeOrF);
            put( '*' );
            return dst[beg .. $];
        case 'F': case 'U': case 'W': case 'V': case 'R': // TypeFunction
            auto r = parseTypeFunction(errStatus);
            if (errStatus)
                return dst.bslice_empty;
            return r;
        case 'C': // TypeClass (C LName)
        case 'S': // TypeStruct (S LName)
        case 'E': // TypeEnum (E LName)
        case 'T': // TypeTypedef (T LName)
            popFront();
            parseQualifiedName(errStatus);
            if (errStatus)
                return dst.bslice_empty;
            return dst[beg .. $];
        case 'D': // TypeDelegate (D TypeFunction)
            popFront();
            auto modifiers = parseModifier();
            if ( front == 'Q' )
            {
                string errMsg;
                auto r = parseBackrefType(errMsg, (e_flag) => parseTypeFunction(e_flag, IsDelegate.yes));
                if (errMsg !is null)
                    return dst.bslice_empty;
                return r;
            }
            else
            {
                parseTypeFunction(errStatus, IsDelegate.yes);
                if (errStatus)
                    return dst.bslice_empty;
            }

            if (modifiers)
            {
                // write modifiers behind the function arguments
                while (auto str = typeCtors.toStringConsume(modifiers))
                {
                    put(' ');
                    put(str);
                }
            }
            return dst[beg .. $];
        case 'n': // TypeNone (n)
            popFront();
            // TODO: Anything needed here?
            return dst[beg .. $];
        case 'B': // TypeTuple (B Number Arguments)
            popFront();
            // TODO: Handle this.
            return dst[beg .. $];
        case 'Z': // Internal symbol
            // This 'type' is used for untyped internal symbols, i.e.:
            // __array
            // __init
            // __vtbl
            // __Class
            // __Interface
            // __ModuleInfo
            popFront();
            return dst[beg .. $];
        default:
            if (t >= 'a' && t <= 'w')
            {
                popFront();
                put( primitives[cast(size_t)(t - 'a')] );
                return dst[beg .. $];
            }
            else if (t == 'z')
            {
                popFront();
                switch ( front )
                {
                case 'i':
                    popFront();
                    put( "cent" );
                    return dst[beg .. $];
                case 'k':
                    popFront();
                    put( "ucent" );
                    return dst[beg .. $];
                default:
                    errStatus = true;
                    return dst.bslice_empty;
                }
            }
            errStatus = true;
            return dst.bslice_empty;
        }
    }


    /*
    TypeFunction:
        CallConvention FuncAttrs Arguments ArgClose Type

    CallConvention:
        F       // D
        U       // C
        W       // Windows
        R       // C++

    FuncAttrs:
        FuncAttr
        FuncAttr FuncAttrs

    FuncAttr:
        empty
        FuncAttrPure
        FuncAttrNothrow
        FuncAttrProperty
        FuncAttrRef
        FuncAttrReturn
        FuncAttrScope
        FuncAttrTrusted
        FuncAttrSafe

    FuncAttrPure:
        Na

    FuncAttrNothrow:
        Nb

    FuncAttrRef:
        Nc

    FuncAttrProperty:
        Nd

    FuncAttrTrusted:
        Ne

    FuncAttrSafe:
        Nf

    FuncAttrNogc:
        Ni

    FuncAttrReturn:
        Nj

    FuncAttrScope:
        Nl

    Arguments:
        Argument
        Argument Arguments

    Argument:
        Argument2
        M Argument2     // scope

    Argument2:
        Type
        J Type     // out
        K Type     // ref
        L Type     // lazy

    ArgClose
        X     // variadic T t,...) style
        Y     // variadic T t...) style
        Z     // not variadic
    */
    void parseCallConvention(out bool errStatus) nothrow
    {
        // CallConvention
        switch ( front )
        {
        case 'F': // D
            popFront();
            break;
        case 'U': // C
            popFront();
            put( "extern (C) " );
            break;
        case 'W': // Windows
            popFront();
            put( "extern (Windows) " );
            break;
        case 'R': // C++
            popFront();
            put( "extern (C++) " );
            break;
        default:
            errStatus = true;
        }
    }

    /// Returns: Flags of `TypeCtor`
    ushort parseModifier()
    {
        TypeCtor res = TypeCtor.None;
        switch ( front )
        {
        case 'y':
            popFront();
            return TypeCtor.Immutable;
        case 'O':
            popFront();
            res |= TypeCtor.Shared;
            if (front == 'x')
                goto case 'x';
            if (front == 'N')
                goto case 'N';
            return TypeCtor.Shared;
        case 'N':
            if (peek( 1 ) != 'g')
                return res;
            popFront();
            popFront();
            res |= TypeCtor.InOut;
            if ( front == 'x' )
                goto case 'x';
            return res;
        case 'x':
            popFront();
            res |= TypeCtor.Const;
            return res;
        default: return TypeCtor.None;
        }
    }

    ushort parseFuncAttr(out bool errStatus) nothrow
    {
        // FuncAttrs
        ushort result;
        while ('N' == front)
        {
            popFront();
            switch ( front )
            {
            case 'a': // FuncAttrPure
                popFront();
                result |= FuncAttributes.Pure;
                continue;
            case 'b': // FuncAttrNoThrow
                popFront();
                result |= FuncAttributes.Nothrow;
                continue;
            case 'c': // FuncAttrRef
                popFront();
                result |= FuncAttributes.Ref;
                continue;
            case 'd': // FuncAttrProperty
                popFront();
                result |= FuncAttributes.Property;
                continue;
            case 'e': // FuncAttrTrusted
                popFront();
                result |= FuncAttributes.Trusted;
                continue;
            case 'f': // FuncAttrSafe
                popFront();
                result |= FuncAttributes.Safe;
                continue;
            case 'g':
            case 'h':
            case 'k':
            case 'n':
                // NOTE: The inout parameter type is represented as "Ng".
                //       The vector parameter type is represented as "Nh".
                //       The return parameter type is represented as "Nk".
                //       The noreturn parameter type is represented as "Nn".
                //       These make it look like a FuncAttr, but infact
                //       if we see these, then we know we're really in
                //       the parameter list.  Rewind and break.
                pos--;
                return result;
            case 'i': // FuncAttrNogc
                popFront();
                result |= FuncAttributes.NoGC;
                continue;
            case 'j': // FuncAttrReturn
                popFront();
                if (this.peek(0) == 'N' && this.peek(1) == 'l')
                {
                    result |= FuncAttributes.ReturnScope;
                    popFront();
                    popFront();
                } else {
                    result |= FuncAttributes.Return;
                }
                continue;
            case 'l': // FuncAttrScope
                popFront();
                if (this.peek(0) == 'N' && this.peek(1) == 'j')
                {
                    result |= FuncAttributes.ScopeReturn;
                    popFront();
                    popFront();
                } else {
                    result |= FuncAttributes.Scope;
                }
                continue;
            case 'm': // FuncAttrLive
                popFront();
                result |= FuncAttributes.Live;
                continue;
            default:
                errStatus = true;
                return 0;
            }
        }
        return result;
    }

    void parseFuncArguments(out bool errStatus) scope nothrow
    {
        // Arguments
        for ( size_t n = 0; true; n++ )
        {
            debug(info) printf( "tok (%c)\n", front );
            switch ( front )
            {
            case 'X': // ArgClose (variadic T t...) style)
                popFront();
                put( "..." );
                return;
            case 'Y': // ArgClose (variadic T t,...) style)
                popFront();
                put( ", ..." );
                return;
            case 'Z': // ArgClose (not variadic)
                popFront();
                return;
            default:
                break;
            }
            putComma(n);

            /* Do special return, scope, ref, out combinations
             */
            int npops;
            if ( 'M' == front && peek(1) == 'N' && peek(2) == 'k')
            {
                const c3 = peek(3);
                if (c3 == 'J')
                {
                    put("scope return out ");   // MNkJ
                    npops = 4;
                }
                else if (c3 == 'K')
                {
                    put("scope return ref ");   // MNkK
                    npops = 4;
                }
            }
            else if ('N' == front && peek(1) == 'k')
            {
                const c2 = peek(2);
                if (c2 == 'J')
                {
                    put("return out ");         // NkJ
                    npops = 3;
                }
                else if (c2 == 'K')
                {
                    put("return ref ");         // NkK
                    npops = 3;
                }
                else if (c2 == 'M')
                {
                    const c3 = peek(3);
                    if (c3 == 'J')
                    {
                        put("return scope out ");       // NkMJ
                        npops = 4;
                    }
                    else if (c3 == 'K')
                    {
                        put("return scope ref ");       // NkMK
                        npops = 4;
                    }
                    else
                    {
                        put("return scope ");           // NkM
                        npops = 3;
                    }
                }
            }
            popFront(npops);

            if ( 'M' == front )
            {
                popFront();
                put( "scope " );
            }
            if ( 'N' == front )
            {
                popFront();
                if ( 'k' == front ) // Return (Nk Parameter2)
                {
                    popFront();
                    put( "return " );
                }
                else
                    pos--;
            }

            // call parseType() and return error if occured
            enum parseTypeOrF = "parseType(errStatus); if (errStatus) return;";

            switch ( front )
            {
            case 'I': // in  (I Type)
                popFront();
                put("in ");
                if (front == 'K')
                    goto case;
                mixin(parseTypeOrF);
                continue;
            case 'K': // ref (K Type)
                popFront();
                put( "ref " );
                mixin(parseTypeOrF);
                continue;
            case 'J': // out (J Type)
                popFront();
                put( "out " );
                mixin(parseTypeOrF);
                continue;
            case 'L': // lazy (L Type)
                popFront();
                put( "lazy " );
                mixin(parseTypeOrF);
                continue;
            default:
                mixin(parseTypeOrF);
            }
        }
    }

    enum IsDelegate { no, yes }

    /*
        TypeFunction:
            CallConvention FuncAttrs Arguments ArgClose Type
    */
    BufSlice parseTypeFunction(out bool errStatus, IsDelegate isdg = IsDelegate.no) return scope nothrow
    {
        debug(trace) printf( "parseTypeFunction+\n" );
        debug(trace) scope(success) printf( "parseTypeFunction-\n" );
        auto beg = dst.length;

        parseCallConvention(errStatus);
        if (errStatus)
            return dst.bslice_empty;

        auto attributes = parseFuncAttr(errStatus);
        if (errStatus)
            return dst.bslice_empty;

        auto argbeg = dst.length;
        put(IsDelegate.yes == isdg ? "delegate" : "function");
        put( '(' );
        parseFuncArguments(errStatus);
        if (errStatus)
            return dst.bslice_empty;
        put( ')' );
        if (attributes)
        {
            // write function attributes behind arguments
            while (auto str = funcAttrs.toStringConsume(attributes))
            {
                put(' ');
                put(str);
            }
        }

        // A function / delegate return type is located at the end of its mangling
        // Write it in order, then shift it back to 'code order'
        // e.g. `delegate(int) @safedouble ' => 'double delegate(int) @safe'
        {
            auto retbeg = dst.length;
            parseType(errStatus);
            if (errStatus)
                return dst.bslice_empty;
            put(' ');
            shift(dst[argbeg .. retbeg]);
        }

        return dst[beg .. $];
    }

    static bool isCallConvention( char ch )
    {
        switch ( ch )
        {
            case 'F', 'U', 'V', 'W', 'R':
                return true;
            default:
                return false;
        }
    }

    /*
    Value:
        n
        Number
        i Number
        N Number
        e HexFloat
        c HexFloat c HexFloat
        A Number Value...

    HexFloat:
        NAN
        INF
        NINF
        N HexDigits P Exponent
        HexDigits P Exponent

    Exponent:
        N Number
        Number

    HexDigits:
        HexDigit
        HexDigit HexDigits

    HexDigit:
        Digit
        A
        B
        C
        D
        E
        F
    */

    void parseValue(out bool errStatus) scope nothrow
    {
        parseValue(errStatus, dst.bslice_empty);
    }

    void parseValue(out bool errStatus, scope BufSlice name, char type = '\0' ) scope nothrow
    {
        debug(trace) printf( "parseValue+\n" );
        debug(trace) scope(success) printf( "parseValue-\n" );

        void onError()
        {
            errStatus = true;
        }

//        printf( "*** %c\n", front );
        switch ( front )
        {
        case 'n':
            popFront();
            put( "null" );
            return;
        case 'i':
            popFront();
            if ('0' > front || '9' < front)
                return onError(); // Number expected
            goto case;
        case '0': .. case '9':
            parseIntegerValue( errStatus, name, type );
            return;
        case 'N':
            popFront();
            put( '-' );
            parseIntegerValue( errStatus, name, type );
            return;
        case 'e':
            popFront();
            parseReal(errStatus);
            return;
        case 'c':
            popFront();
            parseReal(errStatus);
            if (errStatus)
                return;
            put( '+' );
            if (!match('c'))
                return onError();
            parseReal(errStatus);
            if (errStatus)
                return;
            put( 'i' );
            return;
        case 'a': case 'w': case 'd':
            char t = front;
            popFront();
            auto n = decodeNumber(errStatus);
            if (errStatus)
                return;
            if (!match('_'))
                return onError();
            put( '"' );
            foreach (i; 0..n)
            {
                auto a = ascii2hex( errStatus, front );
                if (errStatus)
                    return;
                popFront();

                auto b = ascii2hex( errStatus, front );
                if (errStatus)
                    return;
                popFront();

                auto v = cast(char)((a << 4) | b);
                if (' ' <= v && v <= '~')   // ASCII printable
                {
                    put(v);
                }
                else
                {
                    put("\\x");
                    putAsHex(v, 2);
                }
            }
            put( '"' );
            if ( 'a' != t )
                put(t);
            return;
        case 'A':
            // NOTE: This is kind of a hack.  An associative array literal
            //       [1:2, 3:4] is represented as HiiA2i1i2i3i4, so the type
            //       is "Hii" and the value is "A2i1i2i3i4".  Thus the only
            //       way to determine that this is an AA value rather than an
            //       array value is for the caller to supply the type char.
            //       Hopefully, this will change so that the value is
            //       "H2i1i2i3i4", rendering this unnecesary.
            if ( 'H' == type )
                goto LassocArray;
            // A Number Value...
            // An array literal. Value is repeated Number times.
            popFront();
            put( '[' );
            auto n = decodeNumber(errStatus);
            if (errStatus)
                return;
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue(errStatus);
                if (errStatus)
                    return;
            }
            put( ']' );
            return;
        case 'H':
        LassocArray:
            // H Number Value...
            // An associative array literal. Value is repeated 2*Number times.
            popFront();
            put( '[' );
            auto n = decodeNumber(errStatus);
            if (errStatus)
                return;
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue(errStatus);
                if (errStatus)
                    return;
                put(':');
                parseValue(errStatus);
                if (errStatus)
                    return;
            }
            put( ']' );
            return;
        case 'S':
            // S Number Value...
            // A struct literal. Value is repeated Number times.
            popFront();
            if ( name.length )
                put( name );
            put( '(' );
            auto n = decodeNumber(errStatus);
            if (errStatus)
                return;
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue(errStatus);
                if (errStatus)
                    return;
            }
            put( ')' );
            return;
        case 'f':
            // f MangledName
            // A function literal symbol
            popFront();
            parseMangledName(errStatus, false, 1);
            return;
        default:
            errStatus = true;
        }
    }

    void parseIntegerValue( out bool errStatus, scope BufSlice name, char type = '\0' ) scope nothrow
    {
        debug(trace) printf( "parseIntegerValue+\n" );
        debug(trace) scope(success) printf( "parseIntegerValue-\n" );

        switch ( type )
        {
        case 'a': // char
        case 'u': // wchar
        case 'w': // dchar
        {
            auto val = sliceNumber();
            auto num = decodeNumber( errStatus, val );
            if (errStatus)
                return;

            switch ( num )
            {
            case '\'':
                put( "'\\''" );
                return;
            // \", \?
            case '\\':
                put( "'\\\\'" );
                return;
            case '\a':
                put( "'\\a'" );
                return;
            case '\b':
                put( "'\\b'" );
                return;
            case '\f':
                put( "'\\f'" );
                return;
            case '\n':
                put( "'\\n'" );
                return;
            case '\r':
                put( "'\\r'" );
                return;
            case '\t':
                put( "'\\t'" );
                return;
            case '\v':
                put( "'\\v'" );
                return;
            default:
                switch ( type )
                {
                case 'a':
                    if ( num >= 0x20 && num < 0x7F )
                    {
                        put( '\'' );
                        put( cast(char)num );
                        put( '\'' );
                        return;
                    }
                    put( "\\x" );
                    putAsHex( num, 2 );
                    return;
                case 'u':
                    put( "'\\u" );
                    putAsHex( num, 4 );
                    put( '\'' );
                    return;
                case 'w':
                    put( "'\\U" );
                    putAsHex( num, 8 );
                    put( '\'' );
                    return;
                default:
                    assert( 0 );
                }
            }
        }
        case 'b': // bool
            auto d = decodeNumber(errStatus);
            if (errStatus)
                return;
            put( d ? "true" : "false" );
            return;
        case 'h', 't', 'k': // ubyte, ushort, uint
            put( sliceNumber() );
            put( 'u' );
            return;
        case 'l': // long
            put( sliceNumber() );
            put( 'L' );
            return;
        case 'm': // ulong
            put( sliceNumber() );
            put( "uL" );
            return;
        default:
            put( sliceNumber() );
            return;
        }
    }


    /*
    TemplateArgs:
        TemplateArg
        TemplateArg TemplateArgs

    TemplateArg:
        TemplateArgX
        H TemplateArgX

    TemplateArgX:
        T Type
        V Type Value
        S Number_opt QualifiedName
        X ExternallyMangledName
    */
    void parseTemplateArgs(out bool errStatus) scope nothrow
    {
        debug(trace) printf( "parseTemplateArgs+\n" );
        debug(trace) scope(success) printf( "parseTemplateArgs-\n" );

    L_nextArg:
        for ( size_t n = 0; true; n++ )
        {
            if ( front == 'H' )
                popFront();

            switch ( front )
            {
            case 'T':
                popFront();
                putComma(n);
                parseType(errStatus);
                if (errStatus)
                    return;
                continue;
            case 'V':
                popFront();
                putComma(n);
                // NOTE: In the few instances where the type is actually
                //       desired in the output it should precede the value
                //       generated by parseValue, so it is safe to simply
                //       decrement len and let put/append do its thing.
                char t = front; // peek at type for parseValue
                if ( t == 'Q' )
                {
                    t = peekBackref();
                    if (t == 0)
                    {
                        // invalid back reference
                        errStatus = true;
                        return;
                    }
                }
                BufSlice name = dst.bslice_empty;
                silent( errStatus, delegate void(out bool e_flg) nothrow { name = parseType(e_flg); } );
                if (errStatus)
                    return;
                parseValue( errStatus, name, t );
                if (errStatus)
                    return;
                continue;
            case 'S':
                popFront();
                putComma(n);

                if ( mayBeMangledNameArg() )
                {
                    auto l = dst.length;
                    auto p = pos;
                    auto b = brp;

                    debug(trace) printf( "may be mangled name arg\n" );

                    if (parseMangledNameArg())
                        continue;
                    dst.len = l;
                    pos = p;
                    brp = b;
                    debug(trace) printf( "not a mangled name arg\n" );
                }
                if ( isDigit( front ) && isDigit( peek( 1 ) ) )
                {
                    // ambiguity: length followed by qualified name (starting with number)
                    // try all possible pairs of numbers
                    auto qlen = decodeNumber(errStatus);
                    if (errStatus)
                        return;

                    qlen /= 10; // last digit needed for QualifiedName
                    pos--;
                    auto l = dst.length;
                    auto p = pos;
                    auto b = brp;
                    while ( qlen > 0 )
                    {
                        errStatus = false;
                        parseQualifiedName(errStatus);

                        if (!errStatus)
                        {
                            if ( pos == p + qlen )
                                continue L_nextArg;
                        }

                        qlen /= 10; // retry with one digit less
                        pos = --p;
                        dst.len = l;
                        brp = b;
                    }
                }

                parseQualifiedName(errStatus);
                if (errStatus)
                    return;
                continue;
            case 'X':
                popFront();
                putComma(n);
                {
                    string errMsg;
                    parseLName(errMsg);
                    if (errMsg)
                        return;
                }
                continue;
            default:
                return;
            }
        }
    }


    bool mayBeMangledNameArg() nothrow
    {
        debug(trace) printf( "mayBeMangledNameArg+\n" );
        debug(trace) scope(success) printf( "mayBeMangledNameArg-\n" );

        bool errStatus;
        auto p = pos;
        scope(exit) pos = p;

        if ( isDigit( buf[pos] ) )
        {
            auto n = decodeNumber(errStatus);

            return !errStatus && n >= 4 &&
                pos < buf.length && '_' == buf[pos++] &&
                pos < buf.length && 'D' == buf[pos++] &&
                isDigit( buf[pos] );
        }
        else
        {
            const isSNF = isSymbolNameFront(errStatus);

            return !errStatus &&
                   pos < buf.length && '_' == buf[pos++] &&
                   pos < buf.length && 'D' == buf[pos++] &&
                   isSNF;
        }
    }

    bool parseMangledNameArg() nothrow
    {
        debug(trace) printf( "parseMangledNameArg+\n" );
        debug(trace) scope(success) printf( "parseMangledNameArg-\n" );

        bool errStatus;

        size_t n = 0;
        if ( isDigit( front ) )
        {
            n = decodeNumber(errStatus);

            if (errStatus)
                return false;
        }

        parseMangledName(errStatus, false, n );

        return !errStatus;
    }

    /*
    TemplateInstanceName:
        Number __T LName TemplateArgs Z
    */
    void parseTemplateInstanceName(out bool errStatus, bool hasNumber) scope nothrow
    {
        debug(trace) printf( "parseTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "parseTemplateInstanceName-\n" );

        auto sav = pos;
        auto saveBrp = brp;

        void onError()
        {
            errStatus = true;
            pos = sav;
            brp = saveBrp;
        }

        size_t n = 0;
        if (hasNumber)
        {
            n = decodeNumber(errStatus);
            if (errStatus)
                return onError();
        }

        auto beg = pos;
        errStatus = !match( "__T" );
        if (errStatus)
            return onError();

        {
            string errMsg;
            parseLName(errMsg);
            if (errMsg !is null)
                return onError();
        }

        put( "!(" );

        parseTemplateArgs(errStatus);
        if (errStatus)
            return onError();

        if (!match('Z'))
            return onError();

        if ( hasNumber && pos - beg != n )
        {
            // Template name length mismatch
            return onError();
        }

        put( ')' );
    }


    bool mayBeTemplateInstanceName() scope nothrow
    {
        debug(trace) printf( "mayBeTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "mayBeTemplateInstanceName-\n" );

        auto p = pos;
        scope(exit) pos = p;

        bool errStatus;
        auto n = decodeNumber(errStatus);
        if (errStatus)
            return false;

        return n >= 5 &&
               pos < buf.length && '_' == buf[pos++] &&
               pos < buf.length && '_' == buf[pos++] &&
               pos < buf.length && 'T' == buf[pos++];
    }


    /*
    SymbolName:
        LName
        TemplateInstanceName
    */
    void parseSymbolName(out bool errStatus) scope nothrow
    {
        debug(trace) printf( "parseSymbolName+\n" );
        debug(trace) scope(success) printf( "parseSymbolName-\n" );

        // LName -> Number
        // TemplateInstanceName -> Number "__T"
        switch ( front )
        {
        case '_':
            // no length encoding for templates for new mangling
            parseTemplateInstanceName(errStatus, false);
            return;

        case '0': .. case '9':
            if ( mayBeTemplateInstanceName() )
            {
                auto t = dst.length;

                debug(trace) printf( "may be template instance name\n" );
                parseTemplateInstanceName(errStatus, true);
                if (!errStatus)
                    return;
                else
                {
                    debug(trace) printf( "not a template instance name\n" );
                    dst.len = t;
                }
            }
            goto case;
        case 'Q':
            string errMsg;
            parseLName(errMsg);
            errStatus = errMsg !is null;
            return;
        default:
            errStatus = true;
        }
    }

    // parse optional function arguments as part of a symbol name, i.e without return type
    // if keepAttr, the calling convention and function attributes are not discarded, but returned
    BufSlice parseFunctionTypeNoReturn( bool keepAttr = false ) return scope nothrow
    {
        // try to demangle a function, in case we are pointing to some function local
        auto prevpos = pos;
        auto prevlen = dst.length;
        auto prevbrp = brp;

        if ( 'M' == front )
        {
            // do not emit "needs this"
            popFront();
            auto modifiers = parseModifier();
            while (auto str = typeCtors.toStringConsume(modifiers))
            {
                put(str);
                put(' ');
            }
        }
        if ( isCallConvention( front ) )
        {
            BufSlice attr = dst.bslice_empty;
            // we don't want calling convention and attributes in the qualified name
            bool errStatus;
            parseCallConvention(errStatus);
            if (!errStatus)
            {
                auto attributes = parseFuncAttr(errStatus);
                if (!errStatus)
                {
                    if (keepAttr) {
                        while (auto str = funcAttrs.toStringConsume(attributes))
                        {
                            put(str);
                            put(' ');
                        }
                        attr = dst[prevlen .. $];
                    }

                    put( '(' );
                    parseFuncArguments(errStatus);
                    if (errStatus)
                        return dst.bslice_empty;
                    put( ')' );
                    return attr;
                }
            }

            // not part of a qualified name, so back up
            pos = prevpos;
            dst.len = prevlen;
            brp = prevbrp;
        }

        return dst.bslice_empty;
    }

    /*
    QualifiedName:
        SymbolName
        SymbolName QualifiedName
    */
    void parseQualifiedName(out bool errStatus) return scope nothrow
    {
        debug(trace) printf( "parseQualifiedName+\n" );
        debug(trace) scope(success) printf( "parseQualifiedName-\n" );

        size_t  n   = 0;
        bool is_sym_name_front;

        do
        {
            if ( n++ )
                put( '.' );

            parseSymbolName(errStatus);
            if (errStatus)
                return;

            parseFunctionTypeNoReturn();

            is_sym_name_front = isSymbolNameFront(errStatus);
            if (errStatus)
                return;

        } while ( is_sym_name_front );
    }


    /*
    MangledName:
        _D QualifiedName Type
        _D QualifiedName M Type
    */
    void parseMangledName( out bool errStatus, bool displayType, size_t n = 0 ) scope nothrow
    {
        debug(trace) printf( "parseMangledName+\n" );
        debug(trace) scope(success) printf( "parseMangledName-\n" );
        BufSlice name = dst.bslice_empty;

        auto end = pos + n;

        eat( '_' );
        errStatus = !match( 'D' );
        if (errStatus)
            return;

        do
        {
            size_t  beg = dst.length;
            size_t  nameEnd = dst.length;
            BufSlice attr = dst.bslice_empty;
            bool is_sym_name_front;

            do
            {
                if ( attr.length )
                    dst.remove(attr); // dump attributes of parent symbols
                if (beg != dst.length)
                    put( '.' );

                parseSymbolName(errStatus);
                if (errStatus)
                    return;

                nameEnd = dst.length;
                attr = parseFunctionTypeNoReturn( displayType );

                is_sym_name_front = isSymbolNameFront(errStatus);
                if (errStatus)
                    return;
            } while (is_sym_name_front);

            if ( displayType )
            {
                attr = shift( attr );
                nameEnd = dst.length - attr.length;  // name includes function arguments
            }
            name = dst[beg .. nameEnd];

            debug(info) printf( "name (%.*s)\n", cast(int) name.length, name.getSlice.ptr );
            if ( 'M' == front )
                popFront(); // has 'this' pointer

            auto lastlen = dst.length;
            auto type = parseType(errStatus);
            if (errStatus)
                return;

            if ( displayType )
            {
                if ( type.length )
                    put( ' ' );
                // sort (name,attr,type) -> (attr,type,name)
                shift( name );
            }
            else
            {
                // remove type
                assert( attr.length == 0 );
                dst.len = lastlen;
            }
            if ( pos >= buf.length || (n != 0 && pos >= end) )
                return;

            switch ( front )
            {
            case 'T': // terminators when used as template alias parameter
            case 'V':
            case 'S':
            case 'Z':
                return;
            default:
            }
            put( '.' );

        } while ( true );
    }

    void parseMangledName(out bool errStatus) nothrow
    {
        parseMangledName(errStatus, AddType.yes == addType);
    }

    char[] doDemangle(alias FUNC)() return scope nothrow
    {
        while ( true )
        {
            debug(info) printf( "demangle(%.*s)\n", cast(int) buf.length, buf.ptr );

            bool errStatus;
            FUNC(errStatus);
            if (!errStatus)
            {
                return dst[0 .. $].getSlice;
            }
            else
            {
                debug(info) printf( "error" );

                return dst.copyInput(buf);
            }
        }
    }

    char[] demangleName() nothrow
    {
        return doDemangle!parseMangledName();
    }

    char[] demangleType() nothrow
    {
        return doDemangle!parseType();
    }
}


/**
 * Demangles D/C++ mangled names.  If it is not a D/C++ mangled name, it
 * returns its argument name.
 *
 * Params:
 *  buf = The string to demangle.
 *  dst = An optional destination buffer.
 *  __cxa_demangle = optional C++ demangler
 *
 * Returns:
 *  The demangled name or the original string if the name is not a mangled
 *  D/C++ name.
 */
char[] demangle(return scope const(char)[] buf, return scope char[] dst = null, CXX_DEMANGLER __cxa_demangle = null) nothrow pure @safe
{
    if (__cxa_demangle && buf.length > 2 && buf[0..2] == "_Z")
        return demangleCXX(buf, __cxa_demangle, dst);
    auto d = Demangle!()(buf, dst);
    // fast path (avoiding throwing & catching exception) for obvious
    // non-D mangled names
    if (buf.length < 2 || !(buf[0] == 'D' || buf[0..2] == "_D"))
        return d.dst.copyInput(buf);
    return d.demangleName();
}


/**
 * Demangles a D mangled type.
 *
 * Params:
 *  buf = The string to demangle.
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The demangled type name or the original string if the name is not a
 *  mangled D type.
*/
char[] demangleType( const(char)[] buf, char[] dst = null ) nothrow pure @safe
{
    auto d = Demangle!()(buf, dst);
    return d.demangleType();
}

/**
* reencode a mangled symbol name that might include duplicate occurrences
* of the same identifier by replacing all but the first occurence with
* a back reference.
*
* Params:
*  mangled = The mangled string representing the type
*
* Returns:
*  The mangled name with deduplicated identifiers
*/
char[] reencodeMangled(return scope const(char)[] mangled) nothrow pure @safe
{
    static struct PrependHooks
    {
        size_t lastpos;
        char[] result;
        size_t[const(char)[]] idpos; // identifier positions

        static struct Replacement
        {
            size_t pos;    // postion in original mangled string
            size_t respos; // postion in result string
        }
        Replacement [] replacements;

    pure @safe:
        size_t positionInResult(size_t pos) scope
        {
            foreach_reverse (r; replacements)
                if (pos >= r.pos)
                    return r.respos + pos - r.pos;
            return pos;
        }

        alias Remangle = Demangle!(PrependHooks);

        void flushPosition(ref Remangle d) scope
        {
            if (lastpos < d.pos)
            {
                result ~= d.buf[lastpos .. d.pos];
            }
            else if (lastpos > d.pos)
            {
                // roll back to earlier position
                while (replacements.length > 0 && replacements[$-1].pos > d.pos)
                    replacements = replacements[0 .. $-1];

                if (replacements.length > 0)
                    result.length = replacements[$-1].respos + d.pos - replacements[$-1].pos;
                else
                    result.length = d.pos;
            }
        }

        bool parseLName(out string errMsg, scope ref Remangle d) scope @trusted nothrow
        {
            bool error(string msg)
            {
                errMsg = msg;
                return false;
            }
            flushPosition(d);

            auto reslen = result.length;
            auto refpos = d.pos;
            if (d.front == 'Q')
            {
                size_t npos;
                {
                    scope(exit) result.length = reslen; // remove all intermediate additions
                    // only support identifier back references
                    d.popFront();
                    size_t n = d.decodeBackref();
                    if (!n || n > refpos)
                        return error("invalid back reference");

                    auto savepos = d.pos;
                    scope(exit) d.pos = savepos;
                    size_t srcpos = refpos - n;

                    bool errStatus;
                    auto idlen = d.decodeNumber(errStatus);
                    if (errStatus)
                        return error("invalid number");

                    if (d.pos + idlen > d.buf.length)
                        return error("invalid back reference");

                    auto id = d.buf[d.pos .. d.pos + idlen];
                    auto pid = id in idpos;
                    if (!pid)
                        return error("invalid back reference");

                    npos = positionInResult(*pid);
                }
                encodeBackref(reslen - npos);
                const pos = d.pos; // work around issues.dlang.org/show_bug.cgi?id=20675
                replacements ~= Replacement(pos, result.length);
            }
            else
            {
                bool errStatus;
                auto n = d.decodeNumber(errStatus);
                if (errStatus)
                    return error("invalid number");

                if (!n || n > d.buf.length || n > d.buf.length - d.pos)
                    return error("LName too short or too long");

                auto id = d.buf[d.pos .. d.pos + n];
                d.pos += n;
                if (auto pid = id in idpos)
                {
                    size_t npos = positionInResult(*pid);
                    result.length = reslen;
                    encodeBackref(reslen - npos);
                    const pos = d.pos; // work around issues.dlang.org/show_bug.cgi?id=20675
                    replacements ~= Replacement(pos, result.length);
                }
                else
                {
                    idpos[id] = refpos; //! scope variable id used as AA key, makes this function @trusted
                    result ~= d.buf[refpos .. d.pos];
                }
            }
            lastpos = d.pos;
            return true;
        }

        char[] parseType( out bool errStatus, ref Remangle d, char[] name ) return scope nothrow
        {
            if (d.front != 'Q')
                return null;

            flushPosition(d);

            auto refPos = d.pos;
            d.popFront();
            auto n = d.decodeBackref();
            if (n == 0 || n > refPos)
            {
                // invalid back reference
                errStatus = true;
                return null;
            }

            size_t npos = positionInResult(refPos - n);
            size_t reslen = result.length;
            encodeBackref(reslen - npos);

            lastpos = d.pos;
            return result[reslen .. $]; // anything but null
        }

        void encodeBackref(size_t relpos) scope
        {
            result ~= 'Q';
            enum base = 26;
            size_t div = 1;
            while (relpos >= div * base)
                div *= base;
            while (div >= base)
            {
                auto dig = (relpos / div);
                result ~= cast(char)('A' + dig);
                relpos -= dig * div;
                div /= base;
            }
            result ~= cast(char)('a' + relpos);
        }
    }

    auto d = Demangle!(PrependHooks)(mangled, null);
    d.hooks = PrependHooks();
    d.mute = true; // no demangled output

    bool errStatus;
    d.parseMangledName(errStatus);
    if (errStatus)
    {
        // error cannot occur
        return mangled.dup;
    }

    if (d.hooks.lastpos < d.pos)
        d.hooks.result ~= d.buf[d.hooks.lastpos .. d.pos];
    return d.hooks.result;
}

/**
 * Mangles a D symbol.
 *
 * Params:
 *  T = The type of the symbol.
 *  fqn = The fully qualified name of the symbol.
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The mangled name for a symbols of type T and the given fully
 *  qualified name.
 */
char[] mangle(T)(return scope const(char)[] fqn, return scope char[] dst = null) @safe pure nothrow
{
    import core.internal.string : numDigits, unsignedToTempString;

    static struct DotSplitter
    {
    @safe pure nothrow:
        const(char)[] s;

        @property bool empty() const { return !s.length; }

        @property const(char)[] front() const return scope
        {
            immutable i = indexOfDot();
            return i == -1 ? s[0 .. $] : s[0 .. i];
        }

        void popFront() scope
        {
            immutable i = indexOfDot();
            s = i == -1 ? s[$ .. $] : s[i+1 .. $];
        }

        private ptrdiff_t indexOfDot() const scope
        {
            foreach (i, c; s) if (c == '.') return i;
            return -1;
        }
    }

    size_t len = "_D".length;
    foreach (comp; DotSplitter(fqn))
        len += numDigits(comp.length) + comp.length;
    len += T.mangleof.length;
    if (dst.length < len) dst.length = len;

    size_t i = "_D".length;
    dst[0 .. i] = "_D";
    foreach (comp; DotSplitter(fqn))
    {
        const ndigits = numDigits(comp.length);
        unsignedToTempString(comp.length, dst[i .. i + ndigits]);
        i += ndigits;
        dst[i .. i + comp.length] = comp[];
        i += comp.length;
    }
    dst[i .. i + T.mangleof.length] = T.mangleof[];
    i += T.mangleof.length;

    static if (hasTypeBackRef)
        return reencodeMangled(dst[0 .. i]);
    else
        return dst[0 .. i];
}


///
@safe pure nothrow unittest
{
    assert(mangle!int("a.b") == "_D1a1bi");
    assert(mangle!(char[])("test.foo") == "_D4test3fooAa");
    assert(mangle!(int function(int))("a.b") == "_D1a1bPFiZi");
}

@safe pure nothrow unittest
{
    static assert(mangle!int("a.b") == "_D1a1bi");

    auto buf = new char[](10);
    buf = mangle!int("a.b", buf);
    assert(buf == "_D1a1bi");
    buf = mangle!(char[])("test.foo", buf);
    assert(buf == "_D4test3fooAa");
    buf = mangle!(real delegate(int))("modµ.dg");
    assert(buf == "_D5modµ2dgDFiZe", buf);
}


/**
 * Mangles a D function.
 *
 * Params:
 *  T = function pointer type.
 *  fqn = The fully qualified name of the symbol.
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The mangled name for a function with function pointer type T and
 *  the given fully qualified name.
 */
char[] mangleFunc(T:FT*, FT)(return scope const(char)[] fqn, return scope char[] dst = null) @safe pure nothrow if (is(FT == function))
{
    static if (isExternD!FT)
    {
        return mangle!FT(fqn, dst);
    }
    else static if (hasPlainMangling!FT)
    {
        dst.length = fqn.length;
        dst[] = fqn[];
        return dst;
    }
    else static if (isExternCPP!FT)
    {
        static assert(0, "Can't mangle extern(C++) functions.");
    }
    else
    {
        static assert(0, "Can't mangle function with unknown linkage ("~FT.stringof~").");
    }
}

private enum hasTypeBackRef = (int function(void**,void**)).mangleof[$-4 .. $] == "QdZi";

@safe pure nothrow unittest
{
    assert(mangleFunc!(int function(int))("a.b") == "_D1a1bFiZi");
    static if (hasTypeBackRef)
    {
        assert(mangleFunc!(int function(Object))("object.Object.opEquals") == "_D6object6Object8opEqualsFCQsZi");
        assert(mangleFunc!(int function(Object, Object))("object.Object.opEquals") == "_D6object6Object8opEqualsFCQsQdZi");
    }
    else
    {
        auto mngl = mangleFunc!(int function(Object))("object.Object.opEquals");
        assert(mngl == "_D6object6Object8opEqualsFC6ObjectZi");
        auto remngl = reencodeMangled(mngl);
        assert(remngl == "_D6object6Object8opEqualsFCQsZi");
    }
    // trigger back tracking with ambiguity on '__T', template or identifier
    assert(reencodeMangled("_D3std4conv4conv7__T3std4convi") == "_D3std4convQf7__T3stdQpi");
}

@safe pure nothrow unittest
{
    int function(lazy int[], ...) fp;
    assert(mangle!(typeof(fp))("demangle.test") == "_D8demangle4testPFLAiYi");
    assert(mangle!(typeof(*fp))("demangle.test") == "_D8demangle4testFLAiYi");
}

private template isExternD(FT) if (is(FT == function))
{
    enum isExternD = __traits(getLinkage, FT) == "D";
}

private template isExternCPP(FT) if (is(FT == function))
{
    enum isExternCPP = __traits(getLinkage, FT) == "C++";
}

private template hasPlainMangling(FT) if (is(FT == function))
{
    enum lnk = __traits(getLinkage, FT);
    // C || Windows
    enum hasPlainMangling = lnk == "C" || lnk == "Windows" || lnk == "System";
}

@safe pure nothrow unittest
{
    static extern(D) void fooD();
    static extern(C) void fooC();
    static extern(Windows) void fooW();
    static extern(C++) void fooCPP();

    bool check(FT)(bool isD, bool isCPP, bool isPlain)
    {
        return isExternD!FT == isD && isExternCPP!FT == isCPP &&
            hasPlainMangling!FT == isPlain;
    }
    static assert(check!(typeof(fooD))(true, false, false));
    static assert(check!(typeof(fooC))(false, false, true));
    static assert(check!(typeof(fooW))(false, false, true));
    static assert(check!(typeof(fooCPP))(false, true, false));

    static assert(__traits(compiles, mangleFunc!(typeof(&fooD))("")));
    static assert(__traits(compiles, mangleFunc!(typeof(&fooC))("")));
    static assert(__traits(compiles, mangleFunc!(typeof(&fooW))("")));
    static assert(!__traits(compiles, mangleFunc!(typeof(&fooCPP))("")));
}

/***
 * C name mangling is done by adding a prefix on some platforms.
 */
version (Win32)
    enum string cPrefix = "_";
else version (Darwin)
    enum string cPrefix = "_";
else
    enum string cPrefix = "";

@safe pure nothrow unittest
{
    immutable string[2][] table =
    [
        ["printf", "printf"],
        ["_foo", "_foo"],
        ["_D88", "_D88"],
        ["_D3fooQeFIAyaZv", "void foo.foo(in immutable(char)[])" ],
        ["_D3barQeFIKAyaZv", "void bar.bar(in ref immutable(char)[])" ],
        ["_D4test3fooAa", "char[] test.foo"],
        ["_D8demangle8demangleFAaZAa", "char[] demangle.demangle(char[])"],
        ["_D6object6Object8opEqualsFC6ObjectZi", "int object.Object.opEquals(Object)"],
        ["_D4test2dgDFiYd", "double delegate(int, ...) test.dg"],
        ["_D4test2dgDxFNfiYd", "double delegate(int, ...) @safe const test.dg"],
        //["_D4test58__T9factorialVde67666666666666860140VG5aa5_68656c6c6fVPvnZ9factorialf", ""],
        //["_D4test101__T9factorialVde67666666666666860140Vrc9a999999999999d9014000000000000000c00040VG5aa5_68656c6c6fVPvnZ9factorialf", ""],
        ["_D4test34__T3barVG3uw3_616263VG3wd3_646566Z1xi", "int test.bar!(\"abc\"w, \"def\"d).x"],
        ["_D8demangle4testFLC6ObjectLDFLiZiZi", "int demangle.test(lazy Object, lazy int delegate(lazy int))"],
        ["_D8demangle4testFAiXi", "int demangle.test(int[]...)"],
        ["_D8demangle4testFAiYi", "int demangle.test(int[], ...)"],
        ["_D8demangle4testFLAiXi", "int demangle.test(lazy int[]...)"],
        ["_D8demangle4testFLAiYi", "int demangle.test(lazy int[], ...)"],
        ["_D6plugin8generateFiiZAya", "immutable(char)[] plugin.generate(int, int)"],
        ["_D6plugin8generateFiiZAxa", "const(char)[] plugin.generate(int, int)"],
        ["_D6plugin8generateFiiZAOa", "shared(char)[] plugin.generate(int, int)"],
        ["_D8demangle3fnAFZ3fnBMFZv", "void demangle.fnA().fnB()"],
        ["_D8demangle4mainFZ1S3fnCMFZv", "void demangle.main().S.fnC()"],
        ["_D8demangle4mainFZ1S3fnDMFZv", "void demangle.main().S.fnD()"],
        ["_D8demangle20__T2fnVAiA4i1i2i3i4Z2fnFZv", "void demangle.fn!([1, 2, 3, 4]).fn()"],
        ["_D8demangle10__T2fnVi1Z2fnFZv", "void demangle.fn!(1).fn()"],
        ["_D8demangle26__T2fnVS8demangle1SS2i1i2Z2fnFZv", "void demangle.fn!(demangle.S(1, 2)).fn()"],
        ["_D8demangle13__T2fnVeeNANZ2fnFZv", "void demangle.fn!(real.nan).fn()"],
        ["_D8demangle14__T2fnVeeNINFZ2fnFZv", "void demangle.fn!(-real.infinity).fn()"],
        ["_D8demangle13__T2fnVeeINFZ2fnFZv", "void demangle.fn!(real.infinity).fn()"],
        ["_D8demangle21__T2fnVHiiA2i1i2i3i4Z2fnFZv", "void demangle.fn!([1:2, 3:4]).fn()"],
        ["_D8demangle2fnFNgiZNgi", "inout(int) demangle.fn(inout(int))"],
        ["_D8demangle29__T2fnVa97Va9Va0Vu257Vw65537Z2fnFZv", "void demangle.fn!('a', '\\t', \\x00, '\\u0101', '\\U00010001').fn()"],
        ["_D2gc11gctemplates56__T8mkBitmapTS3std5range13__T4iotaTiTiZ4iotaFiiZ6ResultZ8mkBitmapFNbNiNfPmmZv",
         "nothrow @nogc @safe void gc.gctemplates.mkBitmap!(std.range.iota!(int, int).iota(int, int).Result).mkBitmap(ulong*, ulong)"],
        ["_D8serenity9persister6Sqlite69__T15SqlitePersisterTS8serenity9persister6Sqlite11__unittest6FZ4TestZ15SqlitePersister12__T7opIndexZ7opIndexMFmZS8serenity9persister6Sqlite11__unittest6FZ4Test",
         "serenity.persister.Sqlite.__unittest6().Test serenity.persister.Sqlite.SqlitePersister!(serenity.persister.Sqlite.__unittest6().Test).SqlitePersister.opIndex!().opIndex(ulong)"],
        ["_D8bug100274mainFZ5localMFZi","int bug10027.main().local()"],
        ["_D8demangle4testFNhG16gZv", "void demangle.test(__vector(byte[16]))"],
        ["_D8demangle4testFNhG8sZv", "void demangle.test(__vector(short[8]))"],
        ["_D8demangle4testFNhG4iZv", "void demangle.test(__vector(int[4]))"],
        ["_D8demangle4testFNhG2lZv", "void demangle.test(__vector(long[2]))"],
        ["_D8demangle4testFNhG4fZv", "void demangle.test(__vector(float[4]))"],
        ["_D8demangle4testFNhG2dZv", "void demangle.test(__vector(double[2]))"],
        ["_D8demangle4testFNhG4fNhG4fZv", "void demangle.test(__vector(float[4]), __vector(float[4]))"],
        ["_D8bug1119234__T3fooS23_D8bug111924mainFZ3bariZ3fooMFZv","void bug11192.foo!(bug11192.main().bar).foo()"],
        ["_D13libd_demangle12__ModuleInfoZ", "libd_demangle.__ModuleInfo"],
        ["_D15TypeInfo_Struct6__vtblZ", "TypeInfo_Struct.__vtbl"],
        ["_D3std5stdio12__ModuleInfoZ", "std.stdio.__ModuleInfo"],
        ["_D3std6traits15__T8DemangleTkZ8Demangle6__initZ", "std.traits.Demangle!(uint).Demangle.__init"],
        ["_D3foo3Bar7__ClassZ", "foo.Bar.__Class"],
        ["_D3foo3Bar6__vtblZ", "foo.Bar.__vtbl"],
        ["_D3foo3Bar11__interfaceZ", "foo.Bar.__interface"],
        ["_D3foo7__arrayZ", "foo.__array"],
        ["_D8link657428__T3fooVE8link65746Methodi0Z3fooFZi", "int link6574.foo!(0).foo()"],
        ["_D8link657429__T3fooHVE8link65746Methodi0Z3fooFZi", "int link6574.foo!(0).foo()"],
        ["_D4test22__T4funcVAyaa3_610a62Z4funcFNaNbNiNmNfZAya", `pure nothrow @nogc @live @safe immutable(char)[] test.func!("a\x0ab").func()`],
        ["_D3foo3barFzkZzi", "cent foo.bar(ucent)"],
        ["_D5bug145Class3fooMFNlZPv", "scope void* bug14.Class.foo()"],
        ["_D5bug145Class3barMFNjZPv", "return void* bug14.Class.bar()"],
        ["_D5bug143fooFMPvZPv", "void* bug14.foo(scope void*)"],
        ["_D5bug143barFMNkPvZPv", "void* bug14.bar(scope return void*)"],
        ["_D3std5range15__T4iotaTtTtTtZ4iotaFtttZ6Result7opIndexMNgFNaNbNiNfmZNgt",
         "inout pure nothrow @nogc @safe inout(ushort) std.range.iota!(ushort, ushort, ushort).iota(ushort, ushort, ushort).Result.opIndex(ulong)"],
        ["_D3std6format77__T6getNthVAyaa13_696e7465676572207769647468S233std6traits10isIntegralTiTkTkZ6getNthFNaNfkkkZi",
         "pure @safe int std.format.getNth!(\"integer width\", std.traits.isIntegral, int, uint, uint).getNth(uint, uint, uint)"],
        ["_D3std11parallelism42__T16RoundRobinBufferTDFKAaZvTDxFNaNdNeZbZ16RoundRobinBuffer5primeMFZv",
         "void std.parallelism.RoundRobinBuffer!(void delegate(ref char[]), bool delegate() pure @property @trusted const).RoundRobinBuffer.prime()"],
        ["_D6mangle__T8fun21753VSQv6S21753S1f_DQBj10__lambda71MFNaNbNiNfZvZQCbQp",
        "void function() pure nothrow @nogc @safe mangle.fun21753!(mangle.S21753(mangle.__lambda71())).fun21753"],
        // Lname '0'
        ["_D3std9algorithm9iteration__T9MapResultSQBmQBlQBe005stripTAAyaZQBi7opSliceMFNaNbNiNfmmZSQDiQDhQDa__TQCtSQDyQDxQDq00QCmTQCjZQDq",
         "pure nothrow @nogc @safe std.algorithm.iteration.MapResult!(std.algorithm.iteration.__anonymous.strip, "
        ~"immutable(char)[][]).MapResult std.algorithm.iteration.MapResult!(std.algorithm.iteration.strip, immutable(char)[][]).MapResult.opSlice(ulong, ulong)"],

        // back references
        ["_D4core4stdc5errnoQgFZi", "int core.stdc.errno.errno()"], // identifier back reference
        ["_D4testFS10structnameQnZb", "bool test(structname, structname)"], // type back reference
        ["_D3std11parallelism__T4TaskS8unittest3cmpTAyaTQeZQBb6__dtorMFNfZv",
        "@safe void std.parallelism.Task!(unittest.cmp, immutable(char)[], immutable(char)[]).Task.__dtor()"],
        // 1.s.s.foo from https://issues.dlang.org/show_bug.cgi?id=15831
        ["_D13testexpansion44__T1sTS13testexpansion8__T1sTiZ1sFiZ6ResultZ1sFS13testexpansion8__T1sTiZ1sFiZ6ResultZ6Result3fooMFNaNfZv",
         "pure @safe void testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result.foo()"],
        ["_D13testexpansion__T1sTSQw__TQjTiZQoFiZ6ResultZQBbFQBcZQq3fooMFNaNfZv",
         "pure @safe void testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result.foo()"],
        // formerly ambiguous on 'V', template value argument or pascal function
        // pascal functions have now been removed (in v2.095.0)
        ["_D3std4conv__T7enumRepTyAaTEQBa12experimental9allocator15building_blocks15stats_collector7OptionsVQCti64ZQDnyQDh",
         "immutable(char[]) std.conv.enumRep!(immutable(char[]), std.experimental.allocator.building_blocks.stats_collector.Options, 64).enumRep"],
        // symbol back reference to location with symbol back reference
        ["_D3std12experimental9allocator6common__T10reallocateTSQCaQBzQBo15building_blocks17kernighan_ritchie__T8KRRegionTSQEhQEgQDvQCh14null_allocator13NullAllocatorZQCdZQErFNaNbNiKQEpKAvmZb",
         "pure nothrow @nogc bool std.experimental.allocator.common.reallocate!(std.experimental.allocator.building_blocks.kernighan_ritchie.KRRegion!("
        ~"std.experimental.allocator.building_blocks.null_allocator.NullAllocator).KRRegion).reallocate(ref "
        ~"std.experimental.allocator.building_blocks.kernighan_ritchie.KRRegion!(std.experimental.allocator.building_blocks.null_allocator.NullAllocator).KRRegion, ref void[], ulong)"],
        ["_D3std9exception__T11doesPointToTASQBh5regex8internal2ir10NamedGroupTQBkTvZQCeFNaNbNiNeKxASQDlQCeQCbQBvQBvKxQtZb",
         "pure nothrow @nogc @trusted bool std.exception.doesPointTo!(std.regex.internal.ir.NamedGroup[], "
        ~"std.regex.internal.ir.NamedGroup[], void).doesPointTo(ref const(std.regex.internal.ir.NamedGroup[]), ref const(std.regex.internal.ir.NamedGroup[]))"],
        ["_D3std9algorithm9iteration__T14SplitterResultS_DQBu3uni7isWhiteFNaNbNiNfwZbTAyaZQBz9__xtoHashFNbNeKxSQDvQDuQDn__TQDgS_DQEnQCtQCsQCnTQCeZQEdZm",
         "nothrow @trusted ulong std.algorithm.iteration.SplitterResult!(std.uni.isWhite(dchar), immutable(char)[]).SplitterResult."
        ~"__xtoHash(ref const(std.algorithm.iteration.SplitterResult!(std.uni.isWhite, immutable(char)[]).SplitterResult))"],
        ["_D3std8typecons__T7TypedefTCQBaQz19__unittestL6513_208FNfZ7MyClassVQBonVAyanZQCh6__ctorMFNaNbNcNiNfQCuZSQDyQDx__TQDrTQDmVQDqnVQCcnZQEj",
         "pure nothrow ref @nogc @safe std.typecons.Typedef!(std.typecons.__unittestL6513_208().MyClass, null, null).Typedef "
        ~"std.typecons.Typedef!(std.typecons.__unittestL6513_208().MyClass, null, null).Typedef.__ctor(std.typecons.__unittestL6513_208().MyClass)"],
        ["_D3std6getopt__TQkTAyaTDFNaNbNiNfQoZvTQtTDQsZQBnFNfKAQBiQBlQBkQBrQyZSQCpQCo12GetoptResult",
         "@safe std.getopt.GetoptResult std.getopt.getopt!(immutable(char)[], void delegate(immutable(char)[]) pure nothrow @nogc @safe, "
        ~"immutable(char)[], void delegate(immutable(char)[]) pure nothrow @nogc @safe)."
        ~"getopt(ref immutable(char)[][], immutable(char)[], void delegate(immutable(char)[]) pure nothrow @nogc @safe, "
        ~"immutable(char)[], void delegate(immutable(char)[]) pure nothrow @nogc @safe)"],
        ["_D3std5regex8internal9kickstart__T7ShiftOrTaZQl11ShiftThread__T3setS_DQCqQCpQCmQCg__TQBzTaZQCfQBv10setInvMaskMFNaNbNiNfkkZvZQCjMFNaNfwZv",
         "pure @safe void std.regex.internal.kickstart.ShiftOr!(char).ShiftOr.ShiftThread.set!(std.regex.internal.kickstart.ShiftOr!(char).ShiftOr.ShiftThread.setInvMask(uint, uint)).set(dchar)"],
        ["_D3std5stdio4File__T8lockImplX10LockFileExTykZQBaMFmmykZi", // C function as template alias parameter
         "int std.stdio.File.lockImpl!(LockFileEx, immutable(uint)).lockImpl(ulong, ulong, immutable(uint))"],
        // back reference for type in template AA parameter value
        ["_D3std9algorithm9iteration__T12FilterResultSQBq8typecons__T5TupleTiVAyaa1_61TiVQla1_62TiVQva1_63ZQBm__T6renameVHiQBtA2i0a1_63i2a1_61ZQBeMFNcZ9__lambda1TAiZQEw9__xtoHashFNbNeKxSQGsQGrQGk__TQGdSQHiQFs__TQFmTiVQFja1_61TiVQFua1_62TiVQGfa1_63ZQGx__TQFlVQFhA2i0a1_63i2a1_61ZQGjMFNcZQFfTQEyZQJvZm",
         `nothrow @trusted ulong std.algorithm.iteration.FilterResult!(std.typecons.Tuple!(int, "a", int, "b", int, "c").`
        ~`Tuple.rename!([0:"c", 2:"a"]).rename().__lambda1, int[]).FilterResult.__xtoHash(ref const(std.algorithm.iteration.`
        ~`FilterResult!(std.typecons.Tuple!(int, "a", int, "b", int, "c").Tuple.rename!([0:"c", 2:"a"]).rename().__lambda1, int[]).FilterResult))`],

        ["_D4test4rrs1FKPiZv",    "void test.rrs1(ref int*)"],
        ["_D4test4rrs1FMNkJPiZv", "void test.rrs1(scope return out int*)"],
        ["_D4test4rrs1FMNkKPiZv", "void test.rrs1(scope return ref int*)"],
        ["_D4test4rrs1FNkJPiZv",  "void test.rrs1(return out int*)"],
        ["_D4test4rrs1FNkKPiZv",  "void test.rrs1(return ref int*)"],
        ["_D4test4rrs1FNkMJPiZv", "void test.rrs1(return scope out int*)"],
        ["_D4test4rrs1FNkMKPiZv", "void test.rrs1(return scope ref int*)"],
        ["_D4test4rrs1FNkMPiZv",  "void test.rrs1(return scope int*)"],

        // `scope` and `return` combinations
        ["_D3foo3Foo3barMNgFNjNlNfZNgPv", "inout return scope @safe inout(void*) foo.Foo.bar()"],
        ["_D3foo3FooQiMNgFNlNfZv",        "inout scope @safe void foo.Foo.foo()"],
        ["_D3foo3Foo4foorMNgFNjNfZv",     "inout return @safe void foo.Foo.foor()"],
        ["_D3foo3Foo3rabMNgFNlNjNfZv",    "inout scope return @safe void foo.Foo.rab()"],

        // Hex float digit overflow
        ["_D3foo__T1fVdeFA3D0FBFB72A3C33FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", "_D3foo__T1fVdeFA3D0FBFB72A3C33FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"],
    ];


    template staticIota(int x)
    {
        template Seq(T...){ alias Seq = T; }

        static if (x == 0)
            alias staticIota = Seq!();
        else
            alias staticIota = Seq!(staticIota!(x - 1), x - 1);
    }
    foreach ( i, name; table )
    {
        auto r = demangle( name[0] );
        assert( r == name[1],
                "demangled `" ~ name[0] ~ "` as `" ~ r ~ "` but expected `" ~ name[1] ~ "`");
    }
    foreach ( i; staticIota!(table.length) )
    {
        enum r = demangle( table[i][0] );
        static assert( r == table[i][1],
                "demangled `" ~ table[i][0] ~ "` as `" ~ r ~ "` but expected `" ~ table[i][1] ~ "`");
    }

    {
        // https://issues.dlang.org/show_bug.cgi?id=18531
        auto symbol = `_D3std3uni__T6toCaseS_DQvQt12toLowerIndexFNaNbNiNewZtVii1043S_DQCjQCi10toLowerTabFNaNbNiNemZwSQDo5ascii7toLowerTAyaZQDzFNaNeQmZ14__foreachbody2MFNaNeKmKwZ14__foreachbody3MFNaNeKwZi`;
        auto demangled = `pure @trusted int std.uni.toCase!(std.uni.toLowerIndex(dchar), 1043, std.uni.toLowerTab(ulong), std.ascii.toLower, immutable(char)[]).toCase(immutable(char)[]).__foreachbody2(ref ulong, ref dchar).__foreachbody3(ref dchar)`;
        auto dst = new char[200];
        auto ret = demangle( symbol, dst);
        assert( ret == demangled );
    }
}

unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=18300
    string s = demangle.mangleof;
    foreach (i; 1..77)
    {
        char[] buf = new char[i];
        auto ds = demangle(s, buf);
        assert(ds == "pure nothrow @safe char[] core.demangle.demangle(scope return const(char)[], scope return char[], extern (C) char* function(const(char*), char*, ulong*, int*) pure nothrow @trusted*)" ||
               ds == "pure nothrow @safe char[] core.demangle.demangle(return scope const(char)[], return scope char[], extern (C) char* function(const(char*), char*, ulong*, int*) pure nothrow @trusted*)" ||
               ds == "pure nothrow @safe char[] core.demangle.demangle(scope return const(char)[], scope return char[], extern (C) char* function(const(char*), char*, uint*, int*) pure nothrow @trusted*)" ||
               ds == "pure nothrow @safe char[] core.demangle.demangle(return scope const(char)[], return scope char[], extern (C) char* function(const(char*), char*, uint*, int*) pure nothrow @trusted*)", ds);
    }
}

unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=18300
    string s = "_D1";
    string expected = "int ";
    foreach (_; 0..10_000)
    {
        s ~= "a1";
        expected ~= "a.";
    }
    s ~= "FiZi";
    expected ~= "F";
    assert(s.demangle == expected);

    // https://issues.dlang.org/show_bug.cgi?id=23562
    assert(demangle("_Zv") == "_Zv");
}

// https://issues.dlang.org/show_bug.cgi?id=22235
unittest
{
    enum parent = __MODULE__ ~ '.' ~ __traits(identifier, __traits(parent, {}));

    static noreturn abort() { assert(false); }
    assert(demangle(abort.mangleof) == "pure nothrow @nogc @safe noreturn " ~ parent ~ "().abort()");

    static void accept(noreturn) {}
    assert(demangle(accept.mangleof) == "pure nothrow @nogc @safe void " ~ parent ~ "().accept(noreturn)");

    static void templ(T)(T, T) {}
    assert(demangle(templ!noreturn.mangleof) == "pure nothrow @nogc @safe void " ~ parent ~ "().templ!(noreturn).templ(noreturn, noreturn)");

    static struct S(T) {}
    static void aggr(S!noreturn) { assert(0); }
    assert(demangle(aggr.mangleof) == "pure nothrow @nogc @safe void " ~ parent ~ "().aggr(" ~ parent ~ "().S!(noreturn).S)");
}

// locally purified for internal use here only
extern (C) private
{
    pure @trusted @nogc nothrow pragma(mangle, "fakePureReprintReal") void pureReprintReal(char[] nptr);

    void fakePureReprintReal(char[] nptr)
    {
        import core.stdc.stdlib : strtold;
        import core.stdc.stdio : snprintf;
        import core.stdc.errno : errno;

        const err = errno;
        real val = strtold(nptr.ptr, null);
        snprintf(nptr.ptr, nptr.length, "%#Lg", val);
        errno = err;
    }
}

private struct ManglingFlagInfo
{
    /// The flag value to use
    ushort flag;

    /// Human-readable representation
    string value;
}

private enum TypeCtor : ushort {
    None      = 0,
    //// 'x'
    Const     = (1 << 1),
    /// 'y'
    Immutable = (1 << 2),
    /// 'O'
    Shared    = (1 << 3),
    ///
    InOut     = (1 << 4),
}

private immutable ManglingFlagInfo[] typeCtors = [
    ManglingFlagInfo(TypeCtor.Immutable, "immutable"),
    ManglingFlagInfo(TypeCtor.Shared,    "shared"),
    ManglingFlagInfo(TypeCtor.InOut,     "inout"),
    ManglingFlagInfo(TypeCtor.Const,     "const"),
];

private enum FuncAttributes : ushort {
    None      = 0,
    //// 'a'
    Pure     = (1 << 1),
    //// 'b'
    Nothrow  = (1 << 2),
    //// 'c'
    Ref      = (1 << 3),
    //// 'd'
    Property = (1 << 4),
    //// 'e'
    Trusted  = (1 << 5),
    //// 'f'
    Safe     = (1 << 6),
    //// 'i'
    NoGC     = (1 << 7),
    //// 'j'
    Return   = (1 << 8),
    //// 'l'
    Scope    = (1 << 9),
    //// 'm'
    Live     = (1 << 10),

    /// Their order matter
    ReturnScope   = (1 << 11),
    ScopeReturn   = (1 << 12),
}

// The order in which we process is the same as in compiler/dmd/src/dmangle.d
private immutable ManglingFlagInfo[] funcAttrs = [
    ManglingFlagInfo(FuncAttributes.Pure,     "pure"),
    ManglingFlagInfo(FuncAttributes.Nothrow,  "nothrow"),
    ManglingFlagInfo(FuncAttributes.Ref,      "ref"),
    ManglingFlagInfo(FuncAttributes.Property, "@property"),
    ManglingFlagInfo(FuncAttributes.NoGC,     "@nogc"),

    ManglingFlagInfo(FuncAttributes.ReturnScope, "return scope"),
    ManglingFlagInfo(FuncAttributes.ScopeReturn, "scope return"),

    ManglingFlagInfo(FuncAttributes.Return,   "return"),
    ManglingFlagInfo(FuncAttributes.Scope,    "scope"),

    ManglingFlagInfo(FuncAttributes.Live,     "@live"),
    ManglingFlagInfo(FuncAttributes.Trusted,  "@trusted"),
    ManglingFlagInfo(FuncAttributes.Safe,     "@safe"),
];

private string toStringConsume (immutable ManglingFlagInfo[] infos, ref ushort base)
    @safe pure nothrow @nogc
{
    foreach (const ref info; infos)
    {
        if ((base & info.flag) == info.flag)
        {
            base &= ~info.flag;
            return info.value;
        }
    }
    return null;
}

private shared CXX_DEMANGLER __cxa_demangle;

/**
 * Returns:
 *  a CXX_DEMANGLER if a C++ stdlib is loaded
 */

CXX_DEMANGLER getCXXDemangler() nothrow @trusted
{
    import core.atomic : atomicLoad, atomicStore;
    if (__cxa_demangle is null)
    version (Posix)
    {
        import core.sys.posix.dlfcn : dlsym;
        version (DragonFlyBSD) import core.sys.dragonflybsd.dlfcn : RTLD_DEFAULT;
        version (FreeBSD) import core.sys.freebsd.dlfcn : RTLD_DEFAULT;
        version (linux) import core.sys.linux.dlfcn : RTLD_DEFAULT;
        version (NetBSD) import core.sys.netbsd.dlfcn : RTLD_DEFAULT;
        version (OpenBSD) import core.sys.openbsd.dlfcn : RTLD_DEFAULT;
        version (Darwin) import core.sys.darwin.dlfcn : RTLD_DEFAULT;
        version (Solaris) import core.sys.solaris.dlfcn : RTLD_DEFAULT;

        if (auto found = cast(CXX_DEMANGLER) dlsym(RTLD_DEFAULT, "__cxa_demangle"))
            atomicStore(__cxa_demangle, found);
    }

    if (__cxa_demangle is null)
    {
        static extern(C) char* _(const char* mangled_name, char* output_buffer,
             size_t* length, int* status) nothrow pure @trusted
        {
            *status = -1;
            return null;
        }
        atomicStore(__cxa_demangle, &_);
    }

    return atomicLoad(__cxa_demangle);
}

/**
 * Demangles C++ mangled names.  If it is not a C++ mangled name, it
 * returns its argument name.
 *
 * Params:
 *  buf = The string to demangle.
 *  __cxa_demangle = C++ demangler
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The demangled name or the original string if the name is not a mangled
 *  C++ name.
 */
private char[] demangleCXX(return scope const(char)[] buf, CXX_DEMANGLER __cxa_demangle, return scope char[] dst = null,) nothrow pure @trusted
{
    char[] c_string = dst; // temporarily use dst buffer if possible
    c_string.length = buf.length + 1;
    c_string[0 .. buf.length] = buf[0 .. buf.length];
    c_string[buf.length] = '\0';

    int status;
    size_t demangled_length;
    auto demangled = __cxa_demangle(&c_string[0], null, &demangled_length, &status);
    scope (exit) {
        import core.memory;
        pureFree(cast(void*) demangled);
    }
    if (status == 0)
    {
        dst.length = demangled_length;
        dst[] = demangled[0 .. demangled_length];
        return dst;
    }

    dst.length = buf.length;
    dst[] = buf[];
    return dst;
}

private struct Buffer
{
    enum size_t minSize = 4000;

    @safe pure:

    private char[] dst;
    private size_t len;

    public alias opDollar = len;

    public size_t length () const scope @safe pure nothrow @nogc
    {
        return this.len;
    }

    public BufSlice opSlice (size_t from, size_t to)
        return scope @safe pure nothrow @nogc
    {
        return bslice(from, to);
    }

    static bool contains(scope const(char)[] a, scope const BufSlice b) @safe nothrow
    {
        return
            b.from < a.length &&
            b.to <= a.length;
    }

    char[] copyInput(scope const(char)[] buf)
        return scope nothrow
    {
        if (dst.length < buf.length)
            dst.length = buf.length;
        char[] r = dst[0 .. buf.length];
        r[] = buf[];
        return r;
    }

    private void checkAndStretchBuf(size_t len_to_add) scope nothrow
    {
        const required = len + len_to_add;

        if (required > dst.length)
            dst.length = dst.length + len_to_add;
    }

    // move val to the end of the dst buffer
    BufSlice shift(scope const BufSlice val) return scope nothrow
    {
        version (DigitalMars) pragma(inline, false); // tame dmd inliner

        if (val.length)
        {
            const ptrdiff_t s = val.from;
            const size_t f = len;

            assert(contains( dst[0 .. len], val ),
                "\ndst=\""~dst[0 .. len]~"\"\n"~
                "val=\""~val.getSlice~"\"\n"
            );

            checkAndStretchBuf(val.length);

            // store value temporary over len index
            dst[len .. len + val.length] = val.getSlice();

            // shift all chars including temporary saved above
            // if buf was allocated above it will be leave for further usage
            for (size_t p = s; p < f; p++)
                dst[p] = dst[p + val.length];

            return bslice(len - val.length, len);
        }

        return bslice_empty;
    }

    // remove val from dst buffer
    void remove(scope BufSlice val) scope nothrow
    {
        version (DigitalMars) pragma(inline, false); // tame dmd inliner

        if ( val.length )
        {
            assert( contains( dst[0 .. len], val ) );

            assert( len >= val.length && len <= dst.length );
            len -= val.length;
            for (size_t p = val.from; p < len; p++)
                dst[p] = dst[p + val.length];
        }
    }

    void append(scope const(char)[] val) scope nothrow
    {
        version (DigitalMars) pragma(inline, false); // tame dmd inliner

        if (val.length)
        {
            if ( !dst.length )
                dst.length = minSize;

            debug(info) printf( "appending (%.*s)\n", cast(int) val.length, val.ptr );

            checkAndStretchBuf(val.length);

            // data is already not in place?
            if ( &dst[len] != &val[0] )
                dst[len .. len + val.length] = val[];

            len += val.length;
        }
    }

    @nogc:

    private scope bslice(size_t from, size_t to) nothrow
    {
        return BufSlice(dst, from, to);
    }

    private static scope bslice_empty() nothrow
    {
        return BufSlice.init;
    }
}

private struct BufSlice
{
    char[] buf;
    size_t from;
    size_t to;

    @safe pure nothrow:

    @disable this();

    this(return scope char[] buf) scope nothrow @nogc
    {
        this(buf, 0, 0);
    }

    this(return scope char[] buf, size_t from, size_t to, bool lastArgIsLen = false) scope nothrow @nogc
    {
        this.buf = buf;
        this.from = from;

        if (lastArgIsLen)
            this.to = from + to;
        else
            this.to = to;
    }

    invariant
    {
        if (buf is null)
        {
            assert(from == 0);
            assert(to == 0);
        }

        assert(from <= to);
    }

    auto getSlice() inout nothrow scope { return buf[from .. to]; }
    size_t length() const scope { return to - from; }
}

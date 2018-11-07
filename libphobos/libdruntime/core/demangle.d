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

debug(trace) import core.stdc.stdio : printf;
debug(info) import core.stdc.stdio : printf;

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


    this( const(char)[] buf_, char[] dst_ = null )
    {
        this( buf_, AddType.yes, dst_ );
    }


    this( const(char)[] buf_, AddType addType_, char[] dst_ = null )
    {
        buf     = buf_;
        addType = addType_;
        dst     = dst_;
    }


    enum size_t minBufSize = 4000;


    const(char)[]   buf     = null;
    char[]          dst     = null;
    size_t          pos     = 0;
    size_t          len     = 0;
    size_t          brp     = 0; // current back reference pos
    AddType         addType = AddType.yes;
    bool            mute    = false;
    Hooks           hooks;

    static class ParseException : Exception
    {
        @safe pure nothrow this( string msg )
        {
            super( msg );
        }
    }


    static class OverflowException : Exception
    {
        @safe pure nothrow this( string msg )
        {
            super( msg );
        }
    }


    static void error( string msg = "Invalid symbol" ) @trusted /* exception only used in module */
    {
        pragma(inline, false); // tame dmd inliner

        //throw new ParseException( msg );
        debug(info) printf( "error: %.*s\n", cast(int) msg.length, msg.ptr );
        throw __ctfe ? new ParseException(msg)
                     : cast(ParseException) cast(void*) typeid(ParseException).initializer;

    }


    static void overflow( string msg = "Buffer overflow" ) @trusted /* exception only used in module */
    {
        pragma(inline, false); // tame dmd inliner

        //throw new OverflowException( msg );
        debug(info) printf( "overflow: %.*s\n", cast(int) msg.length, msg.ptr );
        throw cast(OverflowException) cast(void*) typeid(OverflowException).initializer;
    }


    //////////////////////////////////////////////////////////////////////////
    // Type Testing and Conversion
    //////////////////////////////////////////////////////////////////////////


    static bool isAlpha( char val )
    {
        return ('a' <= val && 'z' >= val) ||
               ('A' <= val && 'Z' >= val) ||
               (0x80 & val); // treat all unicode as alphabetic
    }


    static bool isDigit( char val )
    {
        return '0' <= val && '9' >= val;
    }


    static bool isHexDigit( char val )
    {
        return ('0' <= val && '9' >= val) ||
               ('a' <= val && 'f' >= val) ||
               ('A' <= val && 'F' >= val);
    }


    static ubyte ascii2hex( char val )
    {
        if (val >= 'a' && val <= 'f')
            return cast(ubyte)(val - 'a' + 10);
        if (val >= 'A' && val <= 'F')
            return cast(ubyte)(val - 'A' + 10);
        if (val >= '0' && val <= '9')
            return cast(ubyte)(val - '0');
        error();
        return 0;
    }


    //////////////////////////////////////////////////////////////////////////
    // Data Output
    //////////////////////////////////////////////////////////////////////////


    static bool contains( const(char)[] a, const(char)[] b ) @trusted
    {
        if (a.length && b.length)
        {
            auto bend = b.ptr + b.length;
            auto aend = a.ptr + a.length;
            return a.ptr <= b.ptr && bend <= aend;
        }
        return false;
    }


    // move val to the end of the dst buffer
    char[] shift( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length && !mute )
        {
            assert( contains( dst[0 .. len], val ) );
            debug(info) printf( "shifting (%.*s)\n", cast(int) val.length, val.ptr );

            if (len + val.length > dst.length)
                overflow();
            size_t v = &val[0] - &dst[0];
            dst[len .. len + val.length] = val[];
            for (size_t p = v; p < len; p++)
                dst[p] = dst[p + val.length];

            return dst[len - val.length .. len];
        }
        return null;
    }

    // remove val from dst buffer
    void remove( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length )
        {
            assert( contains( dst[0 .. len], val ) );
            debug(info) printf( "removing (%.*s)\n", cast(int) val.length, val.ptr );

            size_t v = &val[0] - &dst[0];
            for (size_t p = v; p < len; p++)
                dst[p] = dst[p + val.length];
            len -= val.length;
        }
    }

    char[] append( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length && !mute )
        {
            if ( !dst.length )
                dst.length = minBufSize;
            assert( !contains( dst[0 .. len], val ) );
            debug(info) printf( "appending (%.*s)\n", cast(int) val.length, val.ptr );

            if ( &dst[len] == &val[0] &&
                dst.length - len >= val.length )
            {
                // data is already in place
                auto t = dst[len .. len + val.length];
                len += val.length;
                return t;
            }
            if ( dst.length - len >= val.length )
            {
                dst[len .. len + val.length] = val[];
                auto t = dst[len .. len + val.length];
                len += val.length;
                return t;
            }
            overflow();
        }
        return null;
    }

    void putComma(size_t n)
    {
        pragma(inline, false);
        if (n)
            put(", ");
    }

    char[] put(char c)
    {
        char[1] val = c;
        return put(val[]);
    }

    char[] put( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length )
        {
            if ( !contains( dst[0 .. len], val ) )
                return append( val );
            return shift( val );
        }
        return null;
    }


    void putAsHex( size_t val, int width = 0 )
    {
        import core.internal.string;

        UnsignedStringBuf buf;

        auto s = unsignedToTempString(val, buf, 16);
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
            append( " " );
            put( val );
        }
    }


    void silent( lazy void dg )
    {
        debug(trace) printf( "silent+\n" );
        debug(trace) scope(success) printf( "silent-\n" );
        auto n = len; dg(); len = n;
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


    void test( char val )
    {
        if ( val != front )
            error();
    }


    void popFront()
    {
        if ( pos++ >= buf.length )
            error();
    }


    void match( char val )
    {
        test( val );
        popFront();
    }


    void match( const(char)[] val )
    {
        foreach (char e; val )
        {
            test( e );
            popFront();
        }
    }


    void eat( char val )
    {
        if ( val == front )
            popFront();
    }

    bool isSymbolNameFront()
    {
        char val = front;
        if ( isDigit( val ) || val == '_' )
            return true;
        if ( val != 'Q' )
            return false;

        // check the back reference encoding after 'Q'
        val = peekBackref();
        return isDigit( val ); // identifier ref
    }

    // return the first character at the back reference
    char peekBackref()
    {
        assert( front == 'Q' );
        auto n = decodeBackref!1();
        if (!n || n > pos)
            error("invalid back reference");

        return buf[pos - n];
    }

    size_t decodeBackref(size_t peekAt = 0)()
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
                    error("invalid back reference");
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
    const(char)[] sliceNumber()
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


    size_t decodeNumber()
    {
        debug(trace) printf( "decodeNumber+\n" );
        debug(trace) scope(success) printf( "decodeNumber-\n" );

        return decodeNumber( sliceNumber() );
    }


    size_t decodeNumber( const(char)[] num )
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
                error();
        }
        return val;
    }


    void parseReal()
    {
        debug(trace) printf( "parseReal+\n" );
        debug(trace) scope(success) printf( "parseReal-\n" );

        char[64] tbuf = void;
        size_t   tlen = 0;
        real     val  = void;

        if ( 'I' == front )
        {
            match( "INF" );
            put( "real.infinity" );
            return;
        }
        if ( 'N' == front )
        {
            popFront();
            if ( 'I' == front )
            {
                match( "INF" );
                put( "-real.infinity" );
                return;
            }
            if ( 'A' == front )
            {
                match( "AN" );
                put( "real.nan" );
                return;
            }
            tbuf[tlen++] = '-';
        }

        tbuf[tlen++] = '0';
        tbuf[tlen++] = 'X';
        if ( !isHexDigit( front ) )
            error( "Expected hex digit" );
        tbuf[tlen++] = front;
        tbuf[tlen++] = '.';
        popFront();

        while ( isHexDigit( front ) )
        {
            tbuf[tlen++] = front;
            popFront();
        }
        match( 'P' );
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
    void parseLName()
    {
        debug(trace) printf( "parseLName+\n" );
        debug(trace) scope(success) printf( "parseLName-\n" );

        static if (__traits(hasMember, Hooks, "parseLName"))
            if (hooks.parseLName(this))
                return;

        if ( front == 'Q' )
        {
            // back reference to LName
            auto refPos = pos;
            popFront();
            size_t n = decodeBackref();
            if ( !n || n > refPos )
                error( "Invalid LName back reference" );
            if ( !mute )
            {
                auto savePos = pos;
                scope(exit) pos = savePos;
                pos = refPos - n;
                parseLName();
            }
            return;
        }
        auto n = decodeNumber();
        if ( n == 0 )
        {
            put( "__anonymous" );
            return;
        }
        if ( n > buf.length || n > buf.length - pos )
            error( "LName must be at least 1 character" );
        if ( '_' != front && !isAlpha( front ) )
            error( "Invalid character in LName" );
        foreach (char e; buf[pos + 1 .. pos + n] )
        {
            if ( '_' != e && !isAlpha( e ) && !isDigit( e ) )
                error( "Invalid character in LName" );
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
    char[] parseType( char[] name = null )
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
            if (auto n = hooks.parseType(this, name))
                return n;

        debug(trace) printf( "parseType+\n" );
        debug(trace) scope(success) printf( "parseType-\n" );
        auto beg = len;
        auto t = front;

        char[] parseBackrefType(scope char[] delegate() pure @safe parseDg) pure @safe
        {
            if (pos == brp)
                error("recursive back reference");
            auto refPos = pos;
            popFront();
            auto n = decodeBackref();
            if (n == 0 || n > pos)
                error("invalid back reference");
            if ( mute )
                return null;
            auto savePos = pos;
            auto saveBrp = brp;
            scope(success) { pos = savePos; brp = saveBrp; }
            pos = refPos - n;
            brp = refPos;
            auto ret = parseDg();
            return ret;
        }

        switch ( t )
        {
        case 'Q': // Type back reference
            return parseBackrefType( () => parseType( name ) );
        case 'O': // Shared (O Type)
            popFront();
            put( "shared(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'x': // Const (x Type)
            popFront();
            put( "const(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'y': // Immutable (y Type)
            popFront();
            put( "immutable(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'N':
            popFront();
            switch ( front )
            {
            case 'g': // Wild (Ng Type)
                popFront();
                // TODO: Anything needed here?
                put( "inout(" );
                parseType();
                put( ')' );
                return dst[beg .. len];
            case 'h': // TypeVector (Nh Type)
                popFront();
                put( "__vector(" );
                parseType();
                put( ')' );
                return dst[beg .. len];
            default:
                error();
                assert( 0 );
            }
        case 'A': // TypeArray (A Type)
            popFront();
            parseType();
            put( "[]" );
            pad( name );
            return dst[beg .. len];
        case 'G': // TypeStaticArray (G Number Type)
            popFront();
            auto num = sliceNumber();
            parseType();
            put( '[' );
            put( num );
            put( ']' );
            pad( name );
            return dst[beg .. len];
        case 'H': // TypeAssocArray (H Type Type)
            popFront();
            // skip t1
            auto tx = parseType();
            parseType();
            put( '[' );
            put( tx );
            put( ']' );
            pad( name );
            return dst[beg .. len];
        case 'P': // TypePointer (P Type)
            popFront();
            parseType();
            put( '*' );
            pad( name );
            return dst[beg .. len];
        case 'F': case 'U': case 'W': case 'V': case 'R': // TypeFunction
            return parseTypeFunction( name );
        case 'I': // TypeIdent (I LName)
        case 'C': // TypeClass (C LName)
        case 'S': // TypeStruct (S LName)
        case 'E': // TypeEnum (E LName)
        case 'T': // TypeTypedef (T LName)
            popFront();
            parseQualifiedName();
            pad( name );
            return dst[beg .. len];
        case 'D': // TypeDelegate (D TypeFunction)
            popFront();
            auto modbeg = len;
            parseModifier();
            auto modend = len;
            if ( front == 'Q' )
                parseBackrefType( () => parseTypeFunction( name, IsDelegate.yes ) );
            else
                parseTypeFunction( name, IsDelegate.yes );
            if (modend > modbeg)
            {
                // move modifiers behind the function arguments
                shift(dst[modend-1 .. modend]); // trailing space
                shift(dst[modbeg .. modend-1]);
            }
            return dst[beg .. len];
        case 'n': // TypeNone (n)
            popFront();
            // TODO: Anything needed here?
            return dst[beg .. len];
        case 'B': // TypeTuple (B Number Arguments)
            popFront();
            // TODO: Handle this.
            return dst[beg .. len];
        case 'Z': // Internal symbol
            // This 'type' is used for untyped internal symbols, i.e.:
            // __array
            // __init
            // __vtbl
            // __Class
            // __Interface
            // __ModuleInfo
            popFront();
            return dst[beg .. len];
        default:
            if (t >= 'a' && t <= 'w')
            {
                popFront();
                put( primitives[cast(size_t)(t - 'a')] );
                pad( name );
                return dst[beg .. len];
            }
            else if (t == 'z')
            {
                popFront();
                switch ( front )
                {
                case 'i':
                    popFront();
                    put( "cent" );
                    pad( name );
                    return dst[beg .. len];
                case 'k':
                    popFront();
                    put( "ucent" );
                    pad( name );
                    return dst[beg .. len];
                default:
                    error();
                    assert( 0 );
                }
            }
            error();
            return null;
        }
    }


    /*
    TypeFunction:
        CallConvention FuncAttrs Arguments ArgClose Type

    CallConvention:
        F       // D
        U       // C
        W       // Windows
        V       // Pascal
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
    void parseCallConvention()
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
        case 'V': // Pascal
            popFront();
            put( "extern (Pascal) " );
            break;
        case 'R': // C++
            popFront();
            put( "extern (C++) " );
            break;
        default:
            error();
        }
    }

    void parseModifier()
    {
        switch ( front )
        {
        case 'y':
            popFront();
            put( "immutable " );
            break;
        case 'O':
            popFront();
            put( "shared " );
            if ( front == 'x' )
                goto case 'x';
            if ( front == 'N' )
                goto case 'N';
            break;
        case 'N':
            if ( peek( 1 ) != 'g' )
                break;
            popFront();
            popFront();
            put( "inout " );
            if ( front == 'x' )
                goto case 'x';
            break;
        case 'x':
            popFront();
            put( "const " );
            break;
        default: break;
        }
    }

    void parseFuncAttr()
    {
        // FuncAttrs
        breakFuncAttrs:
        while ('N' == front)
        {
            popFront();
            switch ( front )
            {
            case 'a': // FuncAttrPure
                popFront();
                put( "pure " );
                continue;
            case 'b': // FuncAttrNoThrow
                popFront();
                put( "nothrow " );
                continue;
            case 'c': // FuncAttrRef
                popFront();
                put( "ref " );
                continue;
            case 'd': // FuncAttrProperty
                popFront();
                put( "@property " );
                continue;
            case 'e': // FuncAttrTrusted
                popFront();
                put( "@trusted " );
                continue;
            case 'f': // FuncAttrSafe
                popFront();
                put( "@safe " );
                continue;
            case 'g':
            case 'h':
            case 'k':
                // NOTE: The inout parameter type is represented as "Ng".
                //       The vector parameter type is represented as "Nh".
                //       The return parameter type is represented as "Nk".
                //       These make it look like a FuncAttr, but infact
                //       if we see these, then we know we're really in
                //       the parameter list.  Rewind and break.
                pos--;
                break breakFuncAttrs;
            case 'i': // FuncAttrNogc
                popFront();
                put( "@nogc " );
                continue;
            case 'j': // FuncAttrReturn
                popFront();
                put( "return " );
                continue;
            case 'l': // FuncAttrScope
                popFront();
                put( "scope " );
                continue;
            default:
                error();
            }
        }
    }

    void parseFuncArguments()
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
            switch ( front )
            {
            case 'J': // out (J Type)
                popFront();
                put( "out " );
                parseType();
                continue;
            case 'K': // ref (K Type)
                popFront();
                put( "ref " );
                parseType();
                continue;
            case 'L': // lazy (L Type)
                popFront();
                put( "lazy " );
                parseType();
                continue;
            default:
                parseType();
            }
        }
    }

    enum IsDelegate { no, yes }

    /*
        TypeFunction:
            CallConvention FuncAttrs Arguments ArgClose Type
    */
    char[] parseTypeFunction( char[] name = null, IsDelegate isdg = IsDelegate.no )
    {
        debug(trace) printf( "parseTypeFunction+\n" );
        debug(trace) scope(success) printf( "parseTypeFunction-\n" );
        auto beg = len;

        parseCallConvention();
        auto attrbeg = len;
        parseFuncAttr();

        auto argbeg = len;
        put( '(' );
        parseFuncArguments();
        put( ')' );
        if (attrbeg < argbeg)
        {
            // move function attributes behind arguments
            shift( dst[argbeg - 1 .. argbeg] ); // trailing space
            shift( dst[attrbeg .. argbeg - 1] ); // attributes
            argbeg = attrbeg;
        }
        auto retbeg = len;
        parseType();
        put( ' ' );
        // append name/delegate/function
        if ( name.length )
        {
            if ( !contains( dst[0 .. len], name ) )
                put( name );
            else if ( shift( name ).ptr != name.ptr )
            {
                argbeg -= name.length;
                retbeg -= name.length;
            }
        }
        else if ( IsDelegate.yes == isdg )
            put( "delegate" );
        else
            put( "function" );
        // move arguments and attributes behind name
        shift( dst[argbeg .. retbeg] );
        return dst[beg..len];
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
    void parseValue( char[] name = null, char type = '\0' )
    {
        debug(trace) printf( "parseValue+\n" );
        debug(trace) scope(success) printf( "parseValue-\n" );

//        printf( "*** %c\n", front );
        switch ( front )
        {
        case 'n':
            popFront();
            put( "null" );
            return;
        case 'i':
            popFront();
            if ( '0' > front || '9' < front )
                error( "Number expected" );
            goto case;
        case '0': .. case '9':
            parseIntegerValue( name, type );
            return;
        case 'N':
            popFront();
            put( '-' );
            parseIntegerValue( name, type );
            return;
        case 'e':
            popFront();
            parseReal();
            return;
        case 'c':
            popFront();
            parseReal();
            put( '+' );
            match( 'c' );
            parseReal();
            put( 'i' );
            return;
        case 'a': case 'w': case 'd':
            char t = front;
            popFront();
            auto n = decodeNumber();
            match( '_' );
            put( '"' );
            foreach (i; 0..n)
            {
                auto a = ascii2hex( front ); popFront();
                auto b = ascii2hex( front ); popFront();
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
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
            }
            put( ']' );
            return;
        case 'H':
        LassocArray:
            // H Number Value...
            // An associative array literal. Value is repeated 2*Number times.
            popFront();
            put( '[' );
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
                put(':');
                parseValue();
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
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
            }
            put( ')' );
            return;
        default:
            error();
        }
    }


    void parseIntegerValue( char[] name = null, char type = '\0' )
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
            auto num = decodeNumber( val );

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
            put( decodeNumber() ? "true" : "false" );
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
    void parseTemplateArgs()
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
                parseType();
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
                    t = peekBackref();
                char[] name; silent( name = parseType() );
                parseValue( name, t );
                continue;
            case 'S':
                popFront();
                putComma(n);

                if ( mayBeMangledNameArg() )
                {
                    auto l = len;
                    auto p = pos;
                    auto b = brp;
                    try
                    {
                        debug(trace) printf( "may be mangled name arg\n" );
                        parseMangledNameArg();
                        continue;
                    }
                    catch ( ParseException e )
                    {
                        len = l;
                        pos = p;
                        brp = b;
                        debug(trace) printf( "not a mangled name arg\n" );
                    }
                }
                if ( isDigit( front ) && isDigit( peek( 1 ) ) )
                {
                    // ambiguity: length followed by qualified name (starting with number)
                    // try all possible pairs of numbers
                    auto qlen = decodeNumber() / 10; // last digit needed for QualifiedName
                    pos--;
                    auto l = len;
                    auto p = pos;
                    auto b = brp;
                    while ( qlen > 0 )
                    {
                        try
                        {
                            parseQualifiedName();
                            if ( pos == p + qlen )
                                continue L_nextArg;
                        }
                        catch ( ParseException e )
                        {
                        }
                        qlen /= 10; // retry with one digit less
                        pos = --p;
                        len = l;
                        brp = b;
                    }
                }
                parseQualifiedName();
                continue;
            case 'X':
                popFront();
                putComma(n);
                parseLName();
                continue;
            default:
                return;
            }
        }
    }


    bool mayBeMangledNameArg()
    {
        debug(trace) printf( "mayBeMangledNameArg+\n" );
        debug(trace) scope(success) printf( "mayBeMangledNameArg-\n" );

        auto p = pos;
        scope(exit) pos = p;
        if ( isDigit( buf[pos] ) )
        {
            auto n = decodeNumber();
            return n >= 4 &&
                pos < buf.length && '_' == buf[pos++] &&
                pos < buf.length && 'D' == buf[pos++] &&
                isDigit( buf[pos] );
        }
        else
        {
            return pos < buf.length && '_' == buf[pos++] &&
                   pos < buf.length && 'D' == buf[pos++] &&
                   isSymbolNameFront();
        }
    }


    void parseMangledNameArg()
    {
        debug(trace) printf( "parseMangledNameArg+\n" );
        debug(trace) scope(success) printf( "parseMangledNameArg-\n" );

        size_t n = 0;
        if ( isDigit( front ) )
            n = decodeNumber();
        parseMangledName( false, n );
    }


    /*
    TemplateInstanceName:
        Number __T LName TemplateArgs Z
    */
    void parseTemplateInstanceName(bool hasNumber)
    {
        debug(trace) printf( "parseTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "parseTemplateInstanceName-\n" );

        auto sav = pos;
        auto saveBrp = brp;
        scope(failure)
        {
            pos = sav;
            brp = saveBrp;
        }
        auto n = hasNumber ? decodeNumber() : 0;
        auto beg = pos;
        match( "__T" );
        parseLName();
        put( "!(" );
        parseTemplateArgs();
        match( 'Z' );
        if ( hasNumber && pos - beg != n )
            error( "Template name length mismatch" );
        put( ')' );
    }


    bool mayBeTemplateInstanceName()
    {
        debug(trace) printf( "mayBeTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "mayBeTemplateInstanceName-\n" );

        auto p = pos;
        scope(exit) pos = p;
        auto n = decodeNumber();
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
    void parseSymbolName()
    {
        debug(trace) printf( "parseSymbolName+\n" );
        debug(trace) scope(success) printf( "parseSymbolName-\n" );

        // LName -> Number
        // TemplateInstanceName -> Number "__T"
        switch ( front )
        {
        case '_':
            // no length encoding for templates for new mangling
            parseTemplateInstanceName(false);
            return;

        case '0': .. case '9':
            if ( mayBeTemplateInstanceName() )
            {
                auto t = len;

                try
                {
                    debug(trace) printf( "may be template instance name\n" );
                    parseTemplateInstanceName(true);
                    return;
                }
                catch ( ParseException e )
                {
                    debug(trace) printf( "not a template instance name\n" );
                    len = t;
                }
            }
            goto case;
        case 'Q':
            parseLName();
            return;
        default:
            error();
        }
    }

    // parse optional function arguments as part of a symbol name, i.e without return type
    // if keepAttr, the calling convention and function attributes are not discarded, but returned
    char[] parseFunctionTypeNoReturn( bool keepAttr = false )
    {
        // try to demangle a function, in case we are pointing to some function local
        auto prevpos = pos;
        auto prevlen = len;
        auto prevbrp = brp;

        char[] attr;
        try
        {
            if ( 'M' == front )
            {
                // do not emit "needs this"
                popFront();
                parseModifier();
            }
            if ( isCallConvention( front ) )
            {
                // we don't want calling convention and attributes in the qualified name
                parseCallConvention();
                parseFuncAttr();
                if ( keepAttr )
                {
                    attr = dst[prevlen .. len];
                }
                else
                {
                    len = prevlen;
                }

                put( '(' );
                parseFuncArguments();
                put( ')' );
            }
        }
        catch ( ParseException )
        {
            // not part of a qualified name, so back up
            pos = prevpos;
            len = prevlen;
            brp = prevbrp;
            attr = null;
        }
        return attr;
    }

    /*
    QualifiedName:
        SymbolName
        SymbolName QualifiedName
    */
    char[] parseQualifiedName()
    {
        debug(trace) printf( "parseQualifiedName+\n" );
        debug(trace) scope(success) printf( "parseQualifiedName-\n" );
        size_t  beg = len;
        size_t  n   = 0;

        do
        {
            if ( n++ )
                put( '.' );
            parseSymbolName();
            parseFunctionTypeNoReturn();

        } while ( isSymbolNameFront() );
        return dst[beg .. len];
    }


    /*
    MangledName:
        _D QualifiedName Type
        _D QualifiedName M Type
    */
    void parseMangledName( bool displayType, size_t n = 0 )
    {
        debug(trace) printf( "parseMangledName+\n" );
        debug(trace) scope(success) printf( "parseMangledName-\n" );
        char[] name = null;

        auto end = pos + n;

        eat( '_' );
        match( 'D' );
        do
        {
            size_t  beg = len;
            size_t  nameEnd = len;
            char[] attr;
            do
            {
                if ( attr )
                    remove( attr ); // dump attributes of parent symbols
                if ( beg != len )
                    put( '.' );
                parseSymbolName();
                nameEnd = len;
                attr = parseFunctionTypeNoReturn( displayType );

            } while ( isSymbolNameFront() );

            if ( displayType )
            {
                attr = shift( attr );
                nameEnd = len - attr.length;  // name includes function arguments
            }
            name = dst[beg .. nameEnd];

            debug(info) printf( "name (%.*s)\n", cast(int) name.length, name.ptr );
            if ( 'M' == front )
                popFront(); // has 'this' pointer

            auto lastlen = len;
            auto type = parseType();
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
                len = lastlen;
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

    void parseMangledName()
    {
        parseMangledName( AddType.yes == addType );
    }

    char[] doDemangle(alias FUNC)()
    {
        while ( true )
        {
            try
            {
                debug(info) printf( "demangle(%.*s)\n", cast(int) buf.length, buf.ptr );
                FUNC();
                return dst[0 .. len];
            }
            catch ( OverflowException e )
            {
                debug(trace) printf( "overflow... restarting\n" );
                auto a = minBufSize;
                auto b = 2 * dst.length;
                auto newsz = a < b ? b : a;
                debug(info) printf( "growing dst to %lu bytes\n", newsz );
                dst.length = newsz;
                pos = len = brp = 0;
                continue;
            }
            catch ( ParseException e )
            {
                debug(info)
                {
                    auto msg = e.toString();
                    printf( "error: %.*s\n", cast(int) msg.length, msg.ptr );
                }
                if ( dst.length < buf.length )
                    dst.length = buf.length;
                dst[0 .. buf.length] = buf[];
                return dst[0 .. buf.length];
            }
            catch ( Exception e )
            {
                assert( false ); // no other exceptions thrown
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
 * Demangles D mangled names.  If it is not a D mangled name, it returns its
 * argument name.
 *
 * Params:
 *  buf = The string to demangle.
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The demangled name or the original string if the name is not a mangled D
 *  name.
 */
char[] demangle( const(char)[] buf, char[] dst = null ) nothrow pure @safe
{
    //return Demangle(buf, dst)();
    auto d = Demangle!()(buf, dst);
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
char[] reencodeMangled(const(char)[] mangled) nothrow pure @safe
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
        size_t positionInResult(size_t pos)
        {
            foreach_reverse (r; replacements)
                if (pos >= r.pos)
                    return r.respos + pos - r.pos;
            return pos;
        }

        alias Remangle = Demangle!(PrependHooks);

        void flushPosition(ref Remangle d)
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

        bool parseLName(ref Remangle d)
        {
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
                        d.error("invalid back reference");

                    auto savepos = d.pos;
                    scope(exit) d.pos = savepos;
                    size_t srcpos = refpos - n;

                    auto idlen = d.decodeNumber();
                    if (d.pos + idlen > d.buf.length)
                        d.error("invalid back reference");
                    auto id = d.buf[d.pos .. d.pos + idlen];
                    auto pid = id in idpos;
                    if (!pid)
                        d.error("invalid back reference");
                    npos = positionInResult(*pid);
                }
                encodeBackref(reslen - npos);
                replacements ~= Replacement(d.pos, result.length);
            }
            else
            {
                auto n = d.decodeNumber();
                if (!n || n > d.buf.length || n > d.buf.length - d.pos)
                    d.error("LName too shot or too long");
                auto id = d.buf[d.pos .. d.pos + n];
                d.pos += n;
                if (auto pid = id in idpos)
                {
                    size_t npos = positionInResult(*pid);
                    result.length = reslen;
                    encodeBackref(reslen - npos);
                    replacements ~= Replacement(d.pos, result.length);
                }
                else
                {
                    idpos[id] = refpos;
                    result ~= d.buf[refpos .. d.pos];
                }
            }
            lastpos = d.pos;
            return true;
        }

        char[] parseType( ref Remangle d, char[] name = null )
        {
            if (d.front != 'Q')
                return null;

            flushPosition(d);

            auto refPos = d.pos;
            d.popFront();
            auto n = d.decodeBackref();
            if (n == 0 || n > refPos)
                d.error("invalid back reference");

            size_t npos = positionInResult(refPos - n);
            size_t reslen = result.length;
            encodeBackref(reslen - npos);

            lastpos = d.pos;
            return result[reslen .. $]; // anything but null
        }

        void encodeBackref(size_t relpos)
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
    try
    {
        d.parseMangledName();
        if (d.hooks.lastpos < d.pos)
            d.hooks.result ~= d.buf[d.hooks.lastpos .. d.pos];
        return d.hooks.result;
    }
    catch (Exception)
    {
        // overflow exception cannot occur
        return mangled.dup;
    }
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
char[] mangle(T)(const(char)[] fqn, char[] dst = null) @safe pure nothrow
{
    import core.internal.string : numDigits, unsignedToTempString;

    static struct DotSplitter
    {
    @safe pure nothrow:
        const(char)[] s;

        @property bool empty() const { return !s.length; }

        @property const(char)[] front() const
        {
            immutable i = indexOfDot();
            return i == -1 ? s[0 .. $] : s[0 .. i];
        }

        void popFront()
        {
            immutable i = indexOfDot();
            s = i == -1 ? s[$ .. $] : s[i+1 .. $];
        }

        private ptrdiff_t indexOfDot() const
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
char[] mangleFunc(T:FT*, FT)(const(char)[] fqn, char[] dst = null) @safe pure nothrow if (is(FT == function))
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

///
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
    // C || Pascal || Windows
    enum hasPlainMangling = lnk == "C" || lnk == "Pascal" || lnk == "Windows";
}

@safe pure nothrow unittest
{
    static extern(D) void fooD();
    static extern(C) void fooC();
    static extern(Pascal) void fooP();
    static extern(Windows) void fooW();
    static extern(C++) void fooCPP();

    bool check(FT)(bool isD, bool isCPP, bool isPlain)
    {
        return isExternD!FT == isD && isExternCPP!FT == isCPP &&
            hasPlainMangling!FT == isPlain;
    }
    static assert(check!(typeof(fooD))(true, false, false));
    static assert(check!(typeof(fooC))(false, false, true));
    static assert(check!(typeof(fooP))(false, false, true));
    static assert(check!(typeof(fooW))(false, false, true));
    static assert(check!(typeof(fooCPP))(false, true, false));

    static assert(__traits(compiles, mangleFunc!(typeof(&fooD))("")));
    static assert(__traits(compiles, mangleFunc!(typeof(&fooC))("")));
    static assert(__traits(compiles, mangleFunc!(typeof(&fooP))("")));
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

version (unittest)
{
    immutable string[2][] table =
    [
        ["printf", "printf"],
        ["_foo", "_foo"],
        ["_D88", "_D88"],
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
        ["_D4test22__T4funcVAyaa3_610a62Z4funcFNaNbNiNfZAya", `pure nothrow @nogc @safe immutable(char)[] test.func!("a\x0ab").func()`],
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
        // ambiguity on 'V', template value argument or pascal function
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
    ];


    template staticIota(int x)
    {
        template Seq(T...){ alias Seq = T; }

        static if (x == 0)
            alias staticIota = Seq!();
        else
            alias staticIota = Seq!(staticIota!(x - 1), x - 1);
    }
}
@safe pure nothrow unittest
{
    foreach ( i, name; table )
    {
        auto r = demangle( name[0] );
        assert( r == name[1],
                "demangled \"" ~ name[0] ~ "\" as \"" ~ r ~ "\" but expected \"" ~ name[1] ~ "\"");
    }
    foreach ( i; staticIota!(table.length) )
    {
        enum r = demangle( table[i][0] );
        static assert( r == table[i][1],
                "demangled \"" ~ table[i][0] ~ "\" as \"" ~ r ~ "\" but expected \"" ~ table[i][1] ~ "\"");
    }
}


/*
 *
 */
string decodeDmdString( const(char)[] ln, ref size_t p ) nothrow pure @safe
{
    string s;
    uint zlen, zpos;

    // decompress symbol
    while ( p < ln.length )
    {
        int ch = cast(ubyte) ln[p++];
        if ( (ch & 0xc0) == 0xc0 )
        {
            zlen = (ch & 0x7) + 1;
            zpos = ((ch >> 3) & 7) + 1; // + zlen;
            if ( zpos > s.length )
                break;
            s ~= s[$ - zpos .. $ - zpos + zlen];
        }
        else if ( ch >= 0x80 )
        {
            if ( p >= ln.length )
                break;
            int ch2 = cast(ubyte) ln[p++];
            zlen = (ch2 & 0x7f) | ((ch & 0x38) << 4);
            if ( p >= ln.length )
                break;
            int ch3 = cast(ubyte) ln[p++];
            zpos = (ch3 & 0x7f) | ((ch & 7) << 7);
            if ( zpos > s.length )
                break;
            s ~= s[$ - zpos .. $ - zpos + zlen];
        }
        else if ( Demangle!().isAlpha(cast(char)ch) || Demangle!().isDigit(cast(char)ch) || ch == '_' )
            s ~= cast(char) ch;
        else
        {
            p--;
            break;
        }
    }
    return s;
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

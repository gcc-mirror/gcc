// Written in the D programming language.

/**
JavaScript Object Notation

Copyright: Copyright Jeremie Pelletier 2008 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Jeremie Pelletier, David Herberth
References: $(LINK http://json.org/)
Source:    $(PHOBOSSRC std/_json.d)
*/
/*
         Copyright Jeremie Pelletier 2008 - 2009.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.json;

import std.array;
import std.conv;
import std.range.primitives;
import std.traits;

///
@system unittest
{
    import std.conv : to;

    // parse a file or string of json into a usable structure
    string s = `{ "language": "D", "rating": 3.5, "code": "42" }`;
    JSONValue j = parseJSON(s);
    // j and j["language"] return JSONValue,
    // j["language"].str returns a string
    assert(j["language"].str == "D");
    assert(j["rating"].floating == 3.5);

    // check a type
    long x;
    if (const(JSONValue)* code = "code" in j)
    {
        if (code.type() == JSON_TYPE.INTEGER)
            x = code.integer;
        else
            x = to!int(code.str);
    }

    // create a json struct
    JSONValue jj = [ "language": "D" ];
    // rating doesnt exist yet, so use .object to assign
    jj.object["rating"] = JSONValue(3.5);
    // create an array to assign to list
    jj.object["list"] = JSONValue( ["a", "b", "c"] );
    // list already exists, so .object optional
    jj["list"].array ~= JSONValue("D");

    string jjStr = `{"language":"D","list":["a","b","c","D"],"rating":3.5}`;
    assert(jj.toString == jjStr);
}

/**
String literals used to represent special float values within JSON strings.
*/
enum JSONFloatLiteral : string
{
    nan         = "NaN",       /// string representation of floating-point NaN
    inf         = "Infinite",  /// string representation of floating-point Infinity
    negativeInf = "-Infinite", /// string representation of floating-point negative Infinity
}

/**
Flags that control how json is encoded and parsed.
*/
enum JSONOptions
{
    none,                       /// standard parsing
    specialFloatLiterals = 0x1, /// encode NaN and Inf float values as strings
    escapeNonAsciiChars = 0x2,  /// encode non ascii characters with an unicode escape sequence
    doNotEscapeSlashes = 0x4,   /// do not escape slashes ('/')
}

/**
JSON type enumeration
*/
enum JSON_TYPE : byte
{
    /// Indicates the type of a $(D JSONValue).
    NULL,
    STRING,  /// ditto
    INTEGER, /// ditto
    UINTEGER,/// ditto
    FLOAT,   /// ditto
    OBJECT,  /// ditto
    ARRAY,   /// ditto
    TRUE,    /// ditto
    FALSE    /// ditto
}

/**
JSON value node
*/
struct JSONValue
{
    import std.exception : enforceEx, enforce;

    union Store
    {
        string                          str;
        long                            integer;
        ulong                           uinteger;
        double                          floating;
        JSONValue[string]               object;
        JSONValue[]                     array;
    }
    private Store store;
    private JSON_TYPE type_tag;

    /**
      Returns the JSON_TYPE of the value stored in this structure.
    */
    @property JSON_TYPE type() const pure nothrow @safe @nogc
    {
        return type_tag;
    }
    ///
    @safe unittest
    {
          string s = "{ \"language\": \"D\" }";
          JSONValue j = parseJSON(s);
          assert(j.type == JSON_TYPE.OBJECT);
          assert(j["language"].type == JSON_TYPE.STRING);
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.STRING).
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.STRING).
     */
    @property string str() const pure @trusted
    {
        enforce!JSONException(type == JSON_TYPE.STRING,
                                "JSONValue is not a string");
        return store.str;
    }
    /// ditto
    @property string str(string v) pure nothrow @nogc @safe
    {
        assign(v);
        return v;
    }
    ///
    @safe unittest
    {
        JSONValue j = [ "language": "D" ];

        // get value
        assert(j["language"].str == "D");

        // change existing key to new string
        j["language"].str = "Perl";
        assert(j["language"].str == "Perl");
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.INTEGER).
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.INTEGER).
     */
    @property inout(long) integer() inout pure @safe
    {
        enforce!JSONException(type == JSON_TYPE.INTEGER,
                                "JSONValue is not an integer");
        return store.integer;
    }
    /// ditto
    @property long integer(long v) pure nothrow @safe @nogc
    {
        assign(v);
        return store.integer;
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.UINTEGER).
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.UINTEGER).
     */
    @property inout(ulong) uinteger() inout pure @safe
    {
        enforce!JSONException(type == JSON_TYPE.UINTEGER,
                                "JSONValue is not an unsigned integer");
        return store.uinteger;
    }
    /// ditto
    @property ulong uinteger(ulong v) pure nothrow @safe @nogc
    {
        assign(v);
        return store.uinteger;
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.FLOAT). Note that despite
     * the name, this is a $(B 64)-bit `double`, not a 32-bit `float`.
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.FLOAT).
     */
    @property inout(double) floating() inout pure @safe
    {
        enforce!JSONException(type == JSON_TYPE.FLOAT,
                                "JSONValue is not a floating type");
        return store.floating;
    }
    /// ditto
    @property double floating(double v) pure nothrow @safe @nogc
    {
        assign(v);
        return store.floating;
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.OBJECT).
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.OBJECT).
     * Note: this is @system because of the following pattern:
       ---
       auto a = &(json.object());
       json.uinteger = 0;        // overwrite AA pointer
       (*a)["hello"] = "world";  // segmentation fault
       ---
     */
    @property ref inout(JSONValue[string]) object() inout pure @system
    {
        enforce!JSONException(type == JSON_TYPE.OBJECT,
                                "JSONValue is not an object");
        return store.object;
    }
    /// ditto
    @property JSONValue[string] object(JSONValue[string] v) pure nothrow @nogc @safe
    {
        assign(v);
        return v;
    }

    /***
     * Value getter for $(D JSON_TYPE.OBJECT).
     * Unlike $(D object), this retrieves the object by value and can be used in @safe code.
     *
     * A caveat is that, if the returned value is null, modifications will not be visible:
     * ---
     * JSONValue json;
     * json.object = null;
     * json.objectNoRef["hello"] = JSONValue("world");
     * assert("hello" !in json.object);
     * ---
     *
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.OBJECT).
     */
    @property inout(JSONValue[string]) objectNoRef() inout pure @trusted
    {
        enforce!JSONException(type == JSON_TYPE.OBJECT,
                                "JSONValue is not an object");
        return store.object;
    }

    /***
     * Value getter/setter for $(D JSON_TYPE.ARRAY).
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.ARRAY).
     * Note: this is @system because of the following pattern:
       ---
       auto a = &(json.array());
       json.uinteger = 0;  // overwrite array pointer
       (*a)[0] = "world";  // segmentation fault
       ---
     */
    @property ref inout(JSONValue[]) array() inout pure @system
    {
        enforce!JSONException(type == JSON_TYPE.ARRAY,
                                "JSONValue is not an array");
        return store.array;
    }
    /// ditto
    @property JSONValue[] array(JSONValue[] v) pure nothrow @nogc @safe
    {
        assign(v);
        return v;
    }

    /***
     * Value getter for $(D JSON_TYPE.ARRAY).
     * Unlike $(D array), this retrieves the array by value and can be used in @safe code.
     *
     * A caveat is that, if you append to the returned array, the new values aren't visible in the
     * JSONValue:
     * ---
     * JSONValue json;
     * json.array = [JSONValue("hello")];
     * json.arrayNoRef ~= JSONValue("world");
     * assert(json.array.length == 1);
     * ---
     *
     * Throws: $(D JSONException) for read access if $(D type) is not
     * $(D JSON_TYPE.ARRAY).
     */
    @property inout(JSONValue[]) arrayNoRef() inout pure @trusted
    {
        enforce!JSONException(type == JSON_TYPE.ARRAY,
                                "JSONValue is not an array");
        return store.array;
    }

    /// Test whether the type is $(D JSON_TYPE.NULL)
    @property bool isNull() const pure nothrow @safe @nogc
    {
        return type == JSON_TYPE.NULL;
    }

    private void assign(T)(T arg) @safe
    {
        static if (is(T : typeof(null)))
        {
            type_tag = JSON_TYPE.NULL;
        }
        else static if (is(T : string))
        {
            type_tag = JSON_TYPE.STRING;
            string t = arg;
            () @trusted { store.str = t; }();
        }
        else static if (isSomeString!T) // issue 15884
        {
            type_tag = JSON_TYPE.STRING;
            // FIXME: std.array.array(Range) is not deduced as 'pure'
            () @trusted {
                import std.utf : byUTF;
                store.str = cast(immutable)(arg.byUTF!char.array);
            }();
        }
        else static if (is(T : bool))
        {
            type_tag = arg ? JSON_TYPE.TRUE : JSON_TYPE.FALSE;
        }
        else static if (is(T : ulong) && isUnsigned!T)
        {
            type_tag = JSON_TYPE.UINTEGER;
            store.uinteger = arg;
        }
        else static if (is(T : long))
        {
            type_tag = JSON_TYPE.INTEGER;
            store.integer = arg;
        }
        else static if (isFloatingPoint!T)
        {
            type_tag = JSON_TYPE.FLOAT;
            store.floating = arg;
        }
        else static if (is(T : Value[Key], Key, Value))
        {
            static assert(is(Key : string), "AA key must be string");
            type_tag = JSON_TYPE.OBJECT;
            static if (is(Value : JSONValue))
            {
                JSONValue[string] t = arg;
                () @trusted { store.object = t; }();
            }
            else
            {
                JSONValue[string] aa;
                foreach (key, value; arg)
                    aa[key] = JSONValue(value);
                () @trusted { store.object = aa; }();
            }
        }
        else static if (isArray!T)
        {
            type_tag = JSON_TYPE.ARRAY;
            static if (is(ElementEncodingType!T : JSONValue))
            {
                JSONValue[] t = arg;
                () @trusted { store.array = t; }();
            }
            else
            {
                JSONValue[] new_arg = new JSONValue[arg.length];
                foreach (i, e; arg)
                    new_arg[i] = JSONValue(e);
                () @trusted { store.array = new_arg; }();
            }
        }
        else static if (is(T : JSONValue))
        {
            type_tag = arg.type;
            store = arg.store;
        }
        else
        {
            static assert(false, text(`unable to convert type "`, T.stringof, `" to json`));
        }
    }

    private void assignRef(T)(ref T arg) if (isStaticArray!T)
    {
        type_tag = JSON_TYPE.ARRAY;
        static if (is(ElementEncodingType!T : JSONValue))
        {
            store.array = arg;
        }
        else
        {
            JSONValue[] new_arg = new JSONValue[arg.length];
            foreach (i, e; arg)
                new_arg[i] = JSONValue(e);
            store.array = new_arg;
        }
    }

    /**
     * Constructor for $(D JSONValue). If $(D arg) is a $(D JSONValue)
     * its value and type will be copied to the new $(D JSONValue).
     * Note that this is a shallow copy: if type is $(D JSON_TYPE.OBJECT)
     * or $(D JSON_TYPE.ARRAY) then only the reference to the data will
     * be copied.
     * Otherwise, $(D arg) must be implicitly convertible to one of the
     * following types: $(D typeof(null)), $(D string), $(D ulong),
     * $(D long), $(D double), an associative array $(D V[K]) for any $(D V)
     * and $(D K) i.e. a JSON object, any array or $(D bool). The type will
     * be set accordingly.
     */
    this(T)(T arg) if (!isStaticArray!T)
    {
        assign(arg);
    }
    /// Ditto
    this(T)(ref T arg) if (isStaticArray!T)
    {
        assignRef(arg);
    }
    /// Ditto
    this(T : JSONValue)(inout T arg) inout
    {
        store = arg.store;
        type_tag = arg.type;
    }
    ///
    @safe unittest
    {
        JSONValue j = JSONValue( "a string" );
        j = JSONValue(42);

        j = JSONValue( [1, 2, 3] );
        assert(j.type == JSON_TYPE.ARRAY);

        j = JSONValue( ["language": "D"] );
        assert(j.type == JSON_TYPE.OBJECT);
    }

    void opAssign(T)(T arg) if (!isStaticArray!T && !is(T : JSONValue))
    {
        assign(arg);
    }

    void opAssign(T)(ref T arg) if (isStaticArray!T)
    {
        assignRef(arg);
    }

    /***
     * Array syntax for json arrays.
     * Throws: $(D JSONException) if $(D type) is not $(D JSON_TYPE.ARRAY).
     */
    ref inout(JSONValue) opIndex(size_t i) inout pure @safe
    {
        auto a = this.arrayNoRef;
        enforceEx!JSONException(i < a.length,
                                "JSONValue array index is out of range");
        return a[i];
    }
    ///
    @safe unittest
    {
        JSONValue j = JSONValue( [42, 43, 44] );
        assert( j[0].integer == 42 );
        assert( j[1].integer == 43 );
    }

    /***
     * Hash syntax for json objects.
     * Throws: $(D JSONException) if $(D type) is not $(D JSON_TYPE.OBJECT).
     */
    ref inout(JSONValue) opIndex(string k) inout pure @safe
    {
        auto o = this.objectNoRef;
        return *enforce!JSONException(k in o,
                                        "Key not found: " ~ k);
    }
    ///
    @safe unittest
    {
        JSONValue j = JSONValue( ["language": "D"] );
        assert( j["language"].str == "D" );
    }

    /***
     * Operator sets $(D value) for element of JSON object by $(D key).
     *
     * If JSON value is null, then operator initializes it with object and then
     * sets $(D value) for it.
     *
     * Throws: $(D JSONException) if $(D type) is not $(D JSON_TYPE.OBJECT)
     * or $(D JSON_TYPE.NULL).
     */
    void opIndexAssign(T)(auto ref T value, string key) pure
    {
        enforceEx!JSONException(type == JSON_TYPE.OBJECT || type == JSON_TYPE.NULL,
                                "JSONValue must be object or null");
        JSONValue[string] aa = null;
        if (type == JSON_TYPE.OBJECT)
        {
            aa = this.objectNoRef;
        }

        aa[key] = value;
        this.object = aa;
    }
    ///
    @safe unittest
    {
            JSONValue j = JSONValue( ["language": "D"] );
            j["language"].str = "Perl";
            assert( j["language"].str == "Perl" );
    }

    void opIndexAssign(T)(T arg, size_t i) pure
    {
        auto a = this.arrayNoRef;
        enforceEx!JSONException(i < a.length,
                                "JSONValue array index is out of range");
        a[i] = arg;
        this.array = a;
    }
    ///
    @safe unittest
    {
            JSONValue j = JSONValue( ["Perl", "C"] );
            j[1].str = "D";
            assert( j[1].str == "D" );
    }

    JSONValue opBinary(string op : "~", T)(T arg) @safe
    {
        auto a = this.arrayNoRef;
        static if (isArray!T)
        {
            return JSONValue(a ~ JSONValue(arg).arrayNoRef);
        }
        else static if (is(T : JSONValue))
        {
            return JSONValue(a ~ arg.arrayNoRef);
        }
        else
        {
            static assert(false, "argument is not an array or a JSONValue array");
        }
    }

    void opOpAssign(string op : "~", T)(T arg) @safe
    {
        auto a = this.arrayNoRef;
        static if (isArray!T)
        {
            a ~= JSONValue(arg).arrayNoRef;
        }
        else static if (is(T : JSONValue))
        {
            a ~= arg.arrayNoRef;
        }
        else
        {
            static assert(false, "argument is not an array or a JSONValue array");
        }
        this.array = a;
    }

    /**
     * Support for the $(D in) operator.
     *
     * Tests wether a key can be found in an object.
     *
     * Returns:
     *      when found, the $(D const(JSONValue)*) that matches to the key,
     *      otherwise $(D null).
     *
     * Throws: $(D JSONException) if the right hand side argument $(D JSON_TYPE)
     * is not $(D OBJECT).
     */
    auto opBinaryRight(string op : "in")(string k) const @safe
    {
        return k in this.objectNoRef;
    }
    ///
    @safe unittest
    {
        JSONValue j = [ "language": "D", "author": "walter" ];
        string a = ("author" in j).str;
    }

    bool opEquals(const JSONValue rhs) const @nogc nothrow pure @safe
    {
        return opEquals(rhs);
    }

    bool opEquals(ref const JSONValue rhs) const @nogc nothrow pure @trusted
    {
        // Default doesn't work well since store is a union.  Compare only
        // what should be in store.
        // This is @trusted to remain nogc, nothrow, fast, and usable from @safe code.
        if (type_tag != rhs.type_tag) return false;

        final switch (type_tag)
        {
        case JSON_TYPE.STRING:
            return store.str == rhs.store.str;
        case JSON_TYPE.INTEGER:
            return store.integer == rhs.store.integer;
        case JSON_TYPE.UINTEGER:
            return store.uinteger == rhs.store.uinteger;
        case JSON_TYPE.FLOAT:
            return store.floating == rhs.store.floating;
        case JSON_TYPE.OBJECT:
            return store.object == rhs.store.object;
        case JSON_TYPE.ARRAY:
            return store.array == rhs.store.array;
        case JSON_TYPE.TRUE:
        case JSON_TYPE.FALSE:
        case JSON_TYPE.NULL:
            return true;
        }
    }

    /// Implements the foreach $(D opApply) interface for json arrays.
    int opApply(scope int delegate(size_t index, ref JSONValue) dg) @system
    {
        int result;

        foreach (size_t index, ref value; array)
        {
            result = dg(index, value);
            if (result)
                break;
        }

        return result;
    }

    /// Implements the foreach $(D opApply) interface for json objects.
    int opApply(scope int delegate(string key, ref JSONValue) dg) @system
    {
        enforce!JSONException(type == JSON_TYPE.OBJECT,
                                "JSONValue is not an object");
        int result;

        foreach (string key, ref value; object)
        {
            result = dg(key, value);
            if (result)
                break;
        }

        return result;
    }

    /***
     * Implicitly calls $(D toJSON) on this JSONValue.
     *
     * $(I options) can be used to tweak the conversion behavior.
     */
    string toString(in JSONOptions options = JSONOptions.none) const @safe
    {
        return toJSON(this, false, options);
    }

    /***
     * Implicitly calls $(D toJSON) on this JSONValue, like $(D toString), but
     * also passes $(I true) as $(I pretty) argument.
     *
     * $(I options) can be used to tweak the conversion behavior
     */
    string toPrettyString(in JSONOptions options = JSONOptions.none) const @safe
    {
        return toJSON(this, true, options);
    }
}

/**
Parses a serialized string and returns a tree of JSON values.
Throws: $(LREF JSONException) if the depth exceeds the max depth.
Params:
    json = json-formatted string to parse
    maxDepth = maximum depth of nesting allowed, -1 disables depth checking
    options = enable decoding string representations of NaN/Inf as float values
*/
JSONValue parseJSON(T)(T json, int maxDepth = -1, JSONOptions options = JSONOptions.none)
if (isInputRange!T && !isInfinite!T && isSomeChar!(ElementEncodingType!T))
{
    import std.ascii : isWhite, isDigit, isHexDigit, toUpper, toLower;
    import std.typecons : Yes;
    JSONValue root;
    root.type_tag = JSON_TYPE.NULL;

    // Avoid UTF decoding when possible, as it is unnecessary when
    // processing JSON.
    static if (is(T : const(char)[]))
        alias Char = char;
    else
        alias Char = Unqual!(ElementType!T);

    if (json.empty) return root;

    int depth = -1;
    Char next = 0;
    int line = 1, pos = 0;

    void error(string msg)
    {
        throw new JSONException(msg, line, pos);
    }

    Char popChar()
    {
        if (json.empty) error("Unexpected end of data.");
        static if (is(T : const(char)[]))
        {
            Char c = json[0];
            json = json[1..$];
        }
        else
        {
            Char c = json.front;
            json.popFront();
        }

        if (c == '\n')
        {
            line++;
            pos = 0;
        }
        else
        {
            pos++;
        }

        return c;
    }

    Char peekChar()
    {
        if (!next)
        {
            if (json.empty) return '\0';
            next = popChar();
        }
        return next;
    }

    void skipWhitespace()
    {
        while (isWhite(peekChar())) next = 0;
    }

    Char getChar(bool SkipWhitespace = false)()
    {
        static if (SkipWhitespace) skipWhitespace();

        Char c;
        if (next)
        {
            c = next;
            next = 0;
        }
        else
            c = popChar();

        return c;
    }

    void checkChar(bool SkipWhitespace = true, bool CaseSensitive = true)(char c)
    {
        static if (SkipWhitespace) skipWhitespace();
        auto c2 = getChar();
        static if (!CaseSensitive) c2 = toLower(c2);

        if (c2 != c) error(text("Found '", c2, "' when expecting '", c, "'."));
    }

    bool testChar(bool SkipWhitespace = true, bool CaseSensitive = true)(char c)
    {
        static if (SkipWhitespace) skipWhitespace();
        auto c2 = peekChar();
        static if (!CaseSensitive) c2 = toLower(c2);

        if (c2 != c) return false;

        getChar();
        return true;
    }

    wchar parseWChar()
    {
        wchar val = 0;
        foreach_reverse (i; 0 .. 4)
        {
            auto hex = toUpper(getChar());
            if (!isHexDigit(hex)) error("Expecting hex character");
            val += (isDigit(hex) ? hex - '0' : hex - ('A' - 10)) << (4 * i);
        }
        return val;
    }

    string parseString()
    {
        import std.ascii : isControl;
        import std.uni : isSurrogateHi, isSurrogateLo;
        import std.utf : encode, decode;

        auto str = appender!string();

    Next:
        switch (peekChar())
        {
            case '"':
                getChar();
                break;

            case '\\':
                getChar();
                auto c = getChar();
                switch (c)
                {
                    case '"':       str.put('"');   break;
                    case '\\':      str.put('\\');  break;
                    case '/':       str.put('/');   break;
                    case 'b':       str.put('\b');  break;
                    case 'f':       str.put('\f');  break;
                    case 'n':       str.put('\n');  break;
                    case 'r':       str.put('\r');  break;
                    case 't':       str.put('\t');  break;
                    case 'u':
                        wchar wc = parseWChar();
                        dchar val;
                        // Non-BMP characters are escaped as a pair of
                        // UTF-16 surrogate characters (see RFC 4627).
                        if (isSurrogateHi(wc))
                        {
                            wchar[2] pair;
                            pair[0] = wc;
                            if (getChar() != '\\') error("Expected escaped low surrogate after escaped high surrogate");
                            if (getChar() != 'u') error("Expected escaped low surrogate after escaped high surrogate");
                            pair[1] = parseWChar();
                            size_t index = 0;
                            val = decode(pair[], index);
                            if (index != 2) error("Invalid escaped surrogate pair");
                        }
                        else
                        if (isSurrogateLo(wc))
                            error(text("Unexpected low surrogate"));
                        else
                            val = wc;

                        char[4] buf;
                        immutable len = encode!(Yes.useReplacementDchar)(buf, val);
                        str.put(buf[0 .. len]);
                        break;

                    default:
                        error(text("Invalid escape sequence '\\", c, "'."));
                }
                goto Next;

            default:
                // RFC 7159 states that control characters U+0000 through
                // U+001F must not appear unescaped in a JSON string.
                auto c = getChar();
                if (isControl(c))
                    error("Illegal control character.");
                str.put(c);
                goto Next;
        }

        return str.data.length ? str.data : "";
    }

    bool tryGetSpecialFloat(string str, out double val) {
        switch (str)
        {
            case JSONFloatLiteral.nan:
                val = double.nan;
                return true;
            case JSONFloatLiteral.inf:
                val = double.infinity;
                return true;
            case JSONFloatLiteral.negativeInf:
                val = -double.infinity;
                return true;
            default:
                return false;
        }
    }

    void parseValue(ref JSONValue value)
    {
        depth++;

        if (maxDepth != -1 && depth > maxDepth) error("Nesting too deep.");

        auto c = getChar!true();

        switch (c)
        {
            case '{':
                if (testChar('}'))
                {
                    value.object = null;
                    break;
                }

                JSONValue[string] obj;
                do
                {
                    checkChar('"');
                    string name = parseString();
                    checkChar(':');
                    JSONValue member;
                    parseValue(member);
                    obj[name] = member;
                }
                while (testChar(','));
                value.object = obj;

                checkChar('}');
                break;

            case '[':
                if (testChar(']'))
                {
                    value.type_tag = JSON_TYPE.ARRAY;
                    break;
                }

                JSONValue[] arr;
                do
                {
                    JSONValue element;
                    parseValue(element);
                    arr ~= element;
                }
                while (testChar(','));

                checkChar(']');
                value.array = arr;
                break;

            case '"':
                auto str = parseString();

                // if special float parsing is enabled, check if string represents NaN/Inf
                if ((options & JSONOptions.specialFloatLiterals) &&
                    tryGetSpecialFloat(str, value.store.floating))
                {
                    // found a special float, its value was placed in value.store.floating
                    value.type_tag = JSON_TYPE.FLOAT;
                    break;
                }

                value.type_tag = JSON_TYPE.STRING;
                value.store.str = str;
                break;

            case '0': .. case '9':
            case '-':
                auto number = appender!string();
                bool isFloat, isNegative;

                void readInteger()
                {
                    if (!isDigit(c)) error("Digit expected");

                Next: number.put(c);

                    if (isDigit(peekChar()))
                    {
                        c = getChar();
                        goto Next;
                    }
                }

                if (c == '-')
                {
                    number.put('-');
                    c = getChar();
                    isNegative = true;
                }

                readInteger();

                if (testChar('.'))
                {
                    isFloat = true;
                    number.put('.');
                    c = getChar();
                    readInteger();
                }
                if (testChar!(false, false)('e'))
                {
                    isFloat = true;
                    number.put('e');
                    if (testChar('+')) number.put('+');
                    else if (testChar('-')) number.put('-');
                    c = getChar();
                    readInteger();
                }

                string data = number.data;
                if (isFloat)
                {
                    value.type_tag = JSON_TYPE.FLOAT;
                    value.store.floating = parse!double(data);
                }
                else
                {
                    if (isNegative)
                        value.store.integer = parse!long(data);
                    else
                        value.store.uinteger = parse!ulong(data);

                    value.type_tag = !isNegative && value.store.uinteger & (1UL << 63) ?
                        JSON_TYPE.UINTEGER : JSON_TYPE.INTEGER;
                }
                break;

            case 't':
            case 'T':
                value.type_tag = JSON_TYPE.TRUE;
                checkChar!(false, false)('r');
                checkChar!(false, false)('u');
                checkChar!(false, false)('e');
                break;

            case 'f':
            case 'F':
                value.type_tag = JSON_TYPE.FALSE;
                checkChar!(false, false)('a');
                checkChar!(false, false)('l');
                checkChar!(false, false)('s');
                checkChar!(false, false)('e');
                break;

            case 'n':
            case 'N':
                value.type_tag = JSON_TYPE.NULL;
                checkChar!(false, false)('u');
                checkChar!(false, false)('l');
                checkChar!(false, false)('l');
                break;

            default:
                error(text("Unexpected character '", c, "'."));
        }

        depth--;
    }

    parseValue(root);
    return root;
}

@safe unittest
{
    enum issue15742objectOfObject = `{ "key1": { "key2": 1 }}`;
    static assert(parseJSON(issue15742objectOfObject).type == JSON_TYPE.OBJECT);

    enum issue15742arrayOfArray = `[[1]]`;
    static assert(parseJSON(issue15742arrayOfArray).type == JSON_TYPE.ARRAY);
}

@safe unittest
{
    // Ensure we can parse and use JSON from @safe code
    auto a = `{ "key1": { "key2": 1 }}`.parseJSON;
    assert(a["key1"]["key2"].integer == 1);
    assert(a.toString == `{"key1":{"key2":1}}`);
}

@system unittest
{
    // Ensure we can parse JSON from a @system range.
    struct Range
    {
        string s;
        size_t index;
        @system
        {
            bool empty() { return index >= s.length; }
            void popFront() { index++; }
            char front() { return s[index]; }
        }
    }
    auto s = Range(`{ "key1": { "key2": 1 }}`);
    auto json = parseJSON(s);
    assert(json["key1"]["key2"].integer == 1);
}

/**
Parses a serialized string and returns a tree of JSON values.
Throws: $(REF JSONException, std,json) if the depth exceeds the max depth.
Params:
    json = json-formatted string to parse
    options = enable decoding string representations of NaN/Inf as float values
*/
JSONValue parseJSON(T)(T json, JSONOptions options)
if (isInputRange!T && !isInfinite!T && isSomeChar!(ElementEncodingType!T))
{
    return parseJSON!T(json, -1, options);
}

/**
Takes a tree of JSON values and returns the serialized string.

Any Object types will be serialized in a key-sorted order.

If $(D pretty) is false no whitespaces are generated.
If $(D pretty) is true serialized string is formatted to be human-readable.
Set the $(LREF JSONOptions.specialFloatLiterals) flag is set in $(D options) to encode NaN/Infinity as strings.
*/
string toJSON(const ref JSONValue root, in bool pretty = false, in JSONOptions options = JSONOptions.none) @safe
{
    auto json = appender!string();

    void toStringImpl(Char)(string str) @safe
    {
        json.put('"');

        foreach (Char c; str)
        {
            switch (c)
            {
                case '"':       json.put("\\\"");       break;
                case '\\':      json.put("\\\\");       break;

                case '/':
                    if (!(options & JSONOptions.doNotEscapeSlashes))
                        json.put('\\');
                    json.put('/');
                    break;

                case '\b':      json.put("\\b");        break;
                case '\f':      json.put("\\f");        break;
                case '\n':      json.put("\\n");        break;
                case '\r':      json.put("\\r");        break;
                case '\t':      json.put("\\t");        break;
                default:
                {
                    import std.ascii : isControl;
                    import std.utf : encode;

                    // Make sure we do UTF decoding iff we want to
                    // escape Unicode characters.
                    assert(((options & JSONOptions.escapeNonAsciiChars) != 0)
                        == is(Char == dchar));

                    with (JSONOptions) if (isControl(c) ||
                        ((options & escapeNonAsciiChars) >= escapeNonAsciiChars && c >= 0x80))
                    {
                        // Ensure non-BMP characters are encoded as a pair
                        // of UTF-16 surrogate characters, as per RFC 4627.
                        wchar[2] wchars; // 1 or 2 UTF-16 code units
                        size_t wNum = encode(wchars, c); // number of UTF-16 code units
                        foreach (wc; wchars[0 .. wNum])
                        {
                            json.put("\\u");
                            foreach_reverse (i; 0 .. 4)
                            {
                                char ch = (wc >>> (4 * i)) & 0x0f;
                                ch += ch < 10 ? '0' : 'A' - 10;
                                json.put(ch);
                            }
                        }
                    }
                    else
                    {
                        json.put(c);
                    }
                }
            }
        }

        json.put('"');
    }

    void toString(string str) @safe
    {
        // Avoid UTF decoding when possible, as it is unnecessary when
        // processing JSON.
        if (options & JSONOptions.escapeNonAsciiChars)
            toStringImpl!dchar(str);
        else
            toStringImpl!char(str);
    }

    void toValue(ref in JSONValue value, ulong indentLevel) @safe
    {
        void putTabs(ulong additionalIndent = 0)
        {
            if (pretty)
                foreach (i; 0 .. indentLevel + additionalIndent)
                    json.put("    ");
        }
        void putEOL()
        {
            if (pretty)
                json.put('\n');
        }
        void putCharAndEOL(char ch)
        {
            json.put(ch);
            putEOL();
        }

        final switch (value.type)
        {
            case JSON_TYPE.OBJECT:
                auto obj = value.objectNoRef;
                if (!obj.length)
                {
                    json.put("{}");
                }
                else
                {
                    putCharAndEOL('{');
                    bool first = true;

                    void emit(R)(R names)
                    {
                        foreach (name; names)
                        {
                            auto member = obj[name];
                            if (!first)
                                putCharAndEOL(',');
                            first = false;
                            putTabs(1);
                            toString(name);
                            json.put(':');
                            if (pretty)
                                json.put(' ');
                            toValue(member, indentLevel + 1);
                        }
                    }

                    import std.algorithm.sorting : sort;
                    // @@@BUG@@@ 14439
                    // auto names = obj.keys;  // aa.keys can't be called in @safe code
                    auto names = new string[obj.length];
                    size_t i = 0;
                    foreach (k, v; obj)
                    {
                        names[i] = k;
                        i++;
                    }
                    sort(names);
                    emit(names);

                    putEOL();
                    putTabs();
                    json.put('}');
                }
                break;

            case JSON_TYPE.ARRAY:
                auto arr = value.arrayNoRef;
                if (arr.empty)
                {
                    json.put("[]");
                }
                else
                {
                    putCharAndEOL('[');
                    foreach (i, el; arr)
                    {
                        if (i)
                            putCharAndEOL(',');
                        putTabs(1);
                        toValue(el, indentLevel + 1);
                    }
                    putEOL();
                    putTabs();
                    json.put(']');
                }
                break;

            case JSON_TYPE.STRING:
                toString(value.str);
                break;

            case JSON_TYPE.INTEGER:
                json.put(to!string(value.store.integer));
                break;

            case JSON_TYPE.UINTEGER:
                json.put(to!string(value.store.uinteger));
                break;

            case JSON_TYPE.FLOAT:
                import std.math : isNaN, isInfinity;

                auto val = value.store.floating;

                if (val.isNaN)
                {
                    if (options & JSONOptions.specialFloatLiterals)
                    {
                        toString(JSONFloatLiteral.nan);
                    }
                    else
                    {
                        throw new JSONException(
                            "Cannot encode NaN. Consider passing the specialFloatLiterals flag.");
                    }
                }
                else if (val.isInfinity)
                {
                    if (options & JSONOptions.specialFloatLiterals)
                    {
                        toString((val > 0) ?  JSONFloatLiteral.inf : JSONFloatLiteral.negativeInf);
                    }
                    else
                    {
                        throw new JSONException(
                            "Cannot encode Infinity. Consider passing the specialFloatLiterals flag.");
                    }
                }
                else
                {
                    import std.format : format;
                    // The correct formula for the number of decimal digits needed for lossless round
                    // trips is actually:
                    //     ceil(log(pow(2.0, double.mant_dig - 1)) / log(10.0) + 1) == (double.dig + 2)
                    // Anything less will round off (1 + double.epsilon)
                    json.put("%.18g".format(val));
                }
                break;

            case JSON_TYPE.TRUE:
                json.put("true");
                break;

            case JSON_TYPE.FALSE:
                json.put("false");
                break;

            case JSON_TYPE.NULL:
                json.put("null");
                break;
        }
    }

    toValue(root, 0);
    return json.data;
}

@safe unittest // bugzilla 12897
{
    JSONValue jv0 = JSONValue("test测试");
    assert(toJSON(jv0, false, JSONOptions.escapeNonAsciiChars) == `"test\u6D4B\u8BD5"`);
    JSONValue jv00 = JSONValue("test\u6D4B\u8BD5");
    assert(toJSON(jv00, false, JSONOptions.none) == `"test测试"`);
    assert(toJSON(jv0, false, JSONOptions.none) == `"test测试"`);
    JSONValue jv1 = JSONValue("été");
    assert(toJSON(jv1, false, JSONOptions.escapeNonAsciiChars) == `"\u00E9t\u00E9"`);
    JSONValue jv11 = JSONValue("\u00E9t\u00E9");
    assert(toJSON(jv11, false, JSONOptions.none) == `"été"`);
    assert(toJSON(jv1, false, JSONOptions.none) == `"été"`);
}

/**
Exception thrown on JSON errors
*/
class JSONException : Exception
{
    this(string msg, int line = 0, int pos = 0) pure nothrow @safe
    {
        if (line)
            super(text(msg, " (Line ", line, ":", pos, ")"));
        else
            super(msg);
    }

    this(string msg, string file, size_t line) pure nothrow @safe
    {
        super(msg, file, line);
    }
}


@system unittest
{
    import std.exception;
    JSONValue jv = "123";
    assert(jv.type == JSON_TYPE.STRING);
    assertNotThrown(jv.str);
    assertThrown!JSONException(jv.integer);
    assertThrown!JSONException(jv.uinteger);
    assertThrown!JSONException(jv.floating);
    assertThrown!JSONException(jv.object);
    assertThrown!JSONException(jv.array);
    assertThrown!JSONException(jv["aa"]);
    assertThrown!JSONException(jv[2]);

    jv = -3;
    assert(jv.type == JSON_TYPE.INTEGER);
    assertNotThrown(jv.integer);

    jv = cast(uint) 3;
    assert(jv.type == JSON_TYPE.UINTEGER);
    assertNotThrown(jv.uinteger);

    jv = 3.0;
    assert(jv.type == JSON_TYPE.FLOAT);
    assertNotThrown(jv.floating);

    jv = ["key" : "value"];
    assert(jv.type == JSON_TYPE.OBJECT);
    assertNotThrown(jv.object);
    assertNotThrown(jv["key"]);
    assert("key" in jv);
    assert("notAnElement" !in jv);
    assertThrown!JSONException(jv["notAnElement"]);
    const cjv = jv;
    assert("key" in cjv);
    assertThrown!JSONException(cjv["notAnElement"]);

    foreach (string key, value; jv)
    {
        static assert(is(typeof(value) == JSONValue));
        assert(key == "key");
        assert(value.type == JSON_TYPE.STRING);
        assertNotThrown(value.str);
        assert(value.str == "value");
    }

    jv = [3, 4, 5];
    assert(jv.type == JSON_TYPE.ARRAY);
    assertNotThrown(jv.array);
    assertNotThrown(jv[2]);
    foreach (size_t index, value; jv)
    {
        static assert(is(typeof(value) == JSONValue));
        assert(value.type == JSON_TYPE.INTEGER);
        assertNotThrown(value.integer);
        assert(index == (value.integer-3));
    }

    jv = null;
    assert(jv.type == JSON_TYPE.NULL);
    assert(jv.isNull);
    jv = "foo";
    assert(!jv.isNull);

    jv = JSONValue("value");
    assert(jv.type == JSON_TYPE.STRING);
    assert(jv.str == "value");

    JSONValue jv2 = JSONValue("value");
    assert(jv2.type == JSON_TYPE.STRING);
    assert(jv2.str == "value");

    JSONValue jv3 = JSONValue("\u001c");
    assert(jv3.type == JSON_TYPE.STRING);
    assert(jv3.str == "\u001C");
}

@system unittest
{
    // Bugzilla 11504

    JSONValue jv = 1;
    assert(jv.type == JSON_TYPE.INTEGER);

    jv.str = "123";
    assert(jv.type == JSON_TYPE.STRING);
    assert(jv.str == "123");

    jv.integer = 1;
    assert(jv.type == JSON_TYPE.INTEGER);
    assert(jv.integer == 1);

    jv.uinteger = 2u;
    assert(jv.type == JSON_TYPE.UINTEGER);
    assert(jv.uinteger == 2u);

    jv.floating = 1.5;
    assert(jv.type == JSON_TYPE.FLOAT);
    assert(jv.floating == 1.5);

    jv.object = ["key" : JSONValue("value")];
    assert(jv.type == JSON_TYPE.OBJECT);
    assert(jv.object == ["key" : JSONValue("value")]);

    jv.array = [JSONValue(1), JSONValue(2), JSONValue(3)];
    assert(jv.type == JSON_TYPE.ARRAY);
    assert(jv.array == [JSONValue(1), JSONValue(2), JSONValue(3)]);

    jv = true;
    assert(jv.type == JSON_TYPE.TRUE);

    jv = false;
    assert(jv.type == JSON_TYPE.FALSE);

    enum E{True = true}
    jv = E.True;
    assert(jv.type == JSON_TYPE.TRUE);
}

@system pure unittest
{
    // Adding new json element via array() / object() directly

    JSONValue jarr = JSONValue([10]);
    foreach (i; 0 .. 9)
        jarr.array ~= JSONValue(i);
    assert(jarr.array.length == 10);

    JSONValue jobj = JSONValue(["key" : JSONValue("value")]);
    foreach (i; 0 .. 9)
        jobj.object[text("key", i)] = JSONValue(text("value", i));
    assert(jobj.object.length == 10);
}

@system pure unittest
{
    // Adding new json element without array() / object() access

    JSONValue jarr = JSONValue([10]);
    foreach (i; 0 .. 9)
        jarr ~= [JSONValue(i)];
    assert(jarr.array.length == 10);

    JSONValue jobj = JSONValue(["key" : JSONValue("value")]);
    foreach (i; 0 .. 9)
        jobj[text("key", i)] = JSONValue(text("value", i));
    assert(jobj.object.length == 10);

    // No array alias
    auto jarr2 = jarr ~ [1,2,3];
    jarr2[0] = 999;
    assert(jarr[0] == JSONValue(10));
}

@system unittest
{
    // @system because JSONValue.array is @system
    import std.exception;

    // An overly simple test suite, if it can parse a serializated string and
    // then use the resulting values tree to generate an identical
    // serialization, both the decoder and encoder works.

    auto jsons = [
        `null`,
        `true`,
        `false`,
        `0`,
        `123`,
        `-4321`,
        `0.25`,
        `-0.25`,
        `""`,
        `"hello\nworld"`,
        `"\"\\\/\b\f\n\r\t"`,
        `[]`,
        `[12,"foo",true,false]`,
        `{}`,
        `{"a":1,"b":null}`,
        `{"goodbye":[true,"or",false,["test",42,{"nested":{"a":23.5,"b":0.140625}}]],`
        ~`"hello":{"array":[12,null,{}],"json":"is great"}}`,
    ];

    enum dbl1_844 = `1.8446744073709568`;
    version (MinGW)
        jsons ~= dbl1_844 ~ `e+019`;
    else
        jsons ~= dbl1_844 ~ `e+19`;

    JSONValue val;
    string result;
    foreach (json; jsons)
    {
        try
        {
            val = parseJSON(json);
            enum pretty = false;
            result = toJSON(val, pretty);
            assert(result == json, text(result, " should be ", json));
        }
        catch (JSONException e)
        {
            import std.stdio : writefln;
            writefln(text(json, "\n", e.toString()));
        }
    }

    // Should be able to correctly interpret unicode entities
    val = parseJSON(`"\u003C\u003E"`);
    assert(toJSON(val) == "\"\&lt;\&gt;\"");
    assert(val.to!string() == "\"\&lt;\&gt;\"");
    val = parseJSON(`"\u0391\u0392\u0393"`);
    assert(toJSON(val) == "\"\&Alpha;\&Beta;\&Gamma;\"");
    assert(val.to!string() == "\"\&Alpha;\&Beta;\&Gamma;\"");
    val = parseJSON(`"\u2660\u2666"`);
    assert(toJSON(val) == "\"\&spades;\&diams;\"");
    assert(val.to!string() == "\"\&spades;\&diams;\"");

    //0x7F is a control character (see Unicode spec)
    val = parseJSON(`"\u007F"`);
    assert(toJSON(val) == "\"\\u007F\"");
    assert(val.to!string() == "\"\\u007F\"");

    with(parseJSON(`""`))
        assert(str == "" && str !is null);
    with(parseJSON(`[]`))
        assert(!array.length);

    // Formatting
    val = parseJSON(`{"a":[null,{"x":1},{},[]]}`);
    assert(toJSON(val, true) == `{
    "a": [
        null,
        {
            "x": 1
        },
        {},
        []
    ]
}`);
}

@safe unittest
{
  auto json = `"hello\nworld"`;
  const jv = parseJSON(json);
  assert(jv.toString == json);
  assert(jv.toPrettyString == json);
}

@system pure unittest
{
    // Bugzilla 12969

    JSONValue jv;
    jv["int"] = 123;

    assert(jv.type == JSON_TYPE.OBJECT);
    assert("int" in jv);
    assert(jv["int"].integer == 123);

    jv["array"] = [1, 2, 3, 4, 5];

    assert(jv["array"].type == JSON_TYPE.ARRAY);
    assert(jv["array"][2].integer == 3);

    jv["str"] = "D language";
    assert(jv["str"].type == JSON_TYPE.STRING);
    assert(jv["str"].str == "D language");

    jv["bool"] = false;
    assert(jv["bool"].type == JSON_TYPE.FALSE);

    assert(jv.object.length == 4);

    jv = [5, 4, 3, 2, 1];
    assert( jv.type == JSON_TYPE.ARRAY );
    assert( jv[3].integer == 2 );
}

@safe unittest
{
    auto s = q"EOF
[
  1,
  2,
  3,
  potato
]
EOF";

    import std.exception;

    auto e = collectException!JSONException(parseJSON(s));
    assert(e.msg == "Unexpected character 'p'. (Line 5:3)", e.msg);
}

// handling of special float values (NaN, Inf, -Inf)
@safe unittest
{
    import std.exception : assertThrown;
    import std.math : isNaN, isInfinity;

    // expected representations of NaN and Inf
    enum {
        nanString         = '"' ~ JSONFloatLiteral.nan         ~ '"',
        infString         = '"' ~ JSONFloatLiteral.inf         ~ '"',
        negativeInfString = '"' ~ JSONFloatLiteral.negativeInf ~ '"',
    }

    // with the specialFloatLiterals option, encode NaN/Inf as strings
    assert(JSONValue(float.nan).toString(JSONOptions.specialFloatLiterals)       == nanString);
    assert(JSONValue(double.infinity).toString(JSONOptions.specialFloatLiterals) == infString);
    assert(JSONValue(-real.infinity).toString(JSONOptions.specialFloatLiterals)  == negativeInfString);

    // without the specialFloatLiterals option, throw on encoding NaN/Inf
    assertThrown!JSONException(JSONValue(float.nan).toString);
    assertThrown!JSONException(JSONValue(double.infinity).toString);
    assertThrown!JSONException(JSONValue(-real.infinity).toString);

    // when parsing json with specialFloatLiterals option, decode special strings as floats
    JSONValue jvNan    = parseJSON(nanString, JSONOptions.specialFloatLiterals);
    JSONValue jvInf    = parseJSON(infString, JSONOptions.specialFloatLiterals);
    JSONValue jvNegInf = parseJSON(negativeInfString, JSONOptions.specialFloatLiterals);

    assert(jvNan.floating.isNaN);
    assert(jvInf.floating.isInfinity    && jvInf.floating > 0);
    assert(jvNegInf.floating.isInfinity && jvNegInf.floating < 0);

    // when parsing json without the specialFloatLiterals option, decode special strings as strings
    jvNan    = parseJSON(nanString);
    jvInf    = parseJSON(infString);
    jvNegInf = parseJSON(negativeInfString);

    assert(jvNan.str    == JSONFloatLiteral.nan);
    assert(jvInf.str    == JSONFloatLiteral.inf);
    assert(jvNegInf.str == JSONFloatLiteral.negativeInf);
}

pure nothrow @safe @nogc unittest
{
    JSONValue testVal;
    testVal = "test";
    testVal = 10;
    testVal = 10u;
    testVal = 1.0;
    testVal = (JSONValue[string]).init;
    testVal = JSONValue[].init;
    testVal = null;
    assert(testVal.isNull);
}

pure nothrow @safe unittest // issue 15884
{
    import std.typecons;
    void Test(C)() {
        C[] a = ['x'];
        JSONValue testVal = a;
        assert(testVal.type == JSON_TYPE.STRING);
        testVal = a.idup;
        assert(testVal.type == JSON_TYPE.STRING);
    }
    Test!char();
    Test!wchar();
    Test!dchar();
}

@safe unittest // issue 15885
{
    enum bool realInDoublePrecision = real.mant_dig == double.mant_dig;

    static bool test(const double num0)
    {
        import std.math : feqrel;
        const json0 = JSONValue(num0);
        const num1 = to!double(toJSON(json0));
        static if (realInDoublePrecision)
            return feqrel(num1, num0) >= (double.mant_dig - 1);
        else
            return num1 == num0;
    }

    assert(test( 0.23));
    assert(test(-0.23));
    assert(test(1.223e+24));
    assert(test(23.4));
    assert(test(0.0012));
    assert(test(30738.22));

    assert(test(1 + double.epsilon));
    assert(test(double.min_normal));
    static if (realInDoublePrecision)
        assert(test(-double.max / 2));
    else
        assert(test(-double.max));

    const minSub = double.min_normal * double.epsilon;
    assert(test(minSub));
    assert(test(3*minSub));
}

@safe unittest // issue 17555
{
    import std.exception : assertThrown;

    assertThrown!JSONException(parseJSON("\"a\nb\""));
}

@safe unittest // issue 17556
{
    auto v = JSONValue("\U0001D11E");
    auto j = toJSON(v, false, JSONOptions.escapeNonAsciiChars);
    assert(j == `"\uD834\uDD1E"`);
}

@safe unittest // issue 5904
{
    string s = `"\uD834\uDD1E"`;
    auto j = parseJSON(s);
    assert(j.str == "\U0001D11E");
}

@safe unittest // issue 17557
{
    assert(parseJSON("\"\xFF\"").str == "\xFF");
    assert(parseJSON("\"\U0001D11E\"").str == "\U0001D11E");
}

@safe unittest // issue 17553
{
    auto v = JSONValue("\xFF");
    assert(toJSON(v) == "\"\xFF\"");
}

@safe unittest
{
    import std.utf;
    assert(parseJSON("\"\xFF\"".byChar).str == "\xFF");
    assert(parseJSON("\"\U0001D11E\"".byChar).str == "\U0001D11E");
}

@safe unittest // JSONOptions.doNotEscapeSlashes (issue 17587)
{
    assert(parseJSON(`"/"`).toString == `"\/"`);
    assert(parseJSON(`"\/"`).toString == `"\/"`);
    assert(parseJSON(`"/"`).toString(JSONOptions.doNotEscapeSlashes) == `"/"`);
    assert(parseJSON(`"\/"`).toString(JSONOptions.doNotEscapeSlashes) == `"/"`);
}

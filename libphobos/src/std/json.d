// Written in the D programming language.

/**
Implements functionality to read and write JavaScript Object Notation values.

JavaScript Object Notation is a lightweight data interchange format commonly used in web services and configuration files.
It's easy for humans to read and write, and it's easy for machines to parse and generate.

$(RED Warning: While $(LREF JSONValue) is fine for small-scale use, at the range of hundreds of megabytes it is
known to cause and exacerbate GC problems. If you encounter problems, try replacing it with a stream parser. See
also $(LINK https://forum.dlang.org/post/dzfyaxypmkdrpakmycjv@forum.dlang.org).)

Copyright: Copyright Jeremie Pelletier 2008 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Jeremie Pelletier, David Herberth
References: $(LINK http://json.org/), $(LINK http://seriot.ch/parsing_json.html)
Source:    $(PHOBOSSRC std/json.d)
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
import std.range;
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
        if (code.type() == JSONType.integer)
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
    strictParsing = 0x8,        /// Strictly follow RFC-8259 grammar when parsing
}

/**
JSON type enumeration
*/
enum JSONType : byte
{
    /// Indicates the type of a `JSONValue`.
    null_,
    string,   /// ditto
    integer,  /// ditto
    uinteger, /// ditto
    float_,   /// ditto
    array,    /// ditto
    object,   /// ditto
    true_,    /// ditto
    false_,   /// ditto
    // FIXME: Find some way to deprecate the enum members below, which does NOT
    // create lots of spam-like deprecation warnings, which can't be fixed
    // by the user. See discussion on this issue at
    // https://forum.dlang.org/post/feudrhtxkaxxscwhhhff@forum.dlang.org
    /* deprecated("Use .null_")    */ NULL = null_,
    /* deprecated("Use .string")   */ STRING = string,
    /* deprecated("Use .integer")  */ INTEGER = integer,
    /* deprecated("Use .uinteger") */ UINTEGER = uinteger,
    /* deprecated("Use .float_")   */ FLOAT = float_,
    /* deprecated("Use .array")    */ ARRAY = array,
    /* deprecated("Use .object")   */ OBJECT = object,
    /* deprecated("Use .true_")    */ TRUE = true_,
    /* deprecated("Use .false_")   */ FALSE = false_,
}

deprecated("Use JSONType and the new enum member names") alias JSON_TYPE = JSONType;

/**
JSON value node
*/
struct JSONValue
{
    import std.exception : enforce;

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
    private JSONType type_tag;

    /**
      Returns the JSONType of the value stored in this structure.
    */
    @property JSONType type() const pure nothrow @safe @nogc
    {
        return type_tag;
    }
    ///
    @safe unittest
    {
          string s = "{ \"language\": \"D\" }";
          JSONValue j = parseJSON(s);
          assert(j.type == JSONType.object);
          assert(j["language"].type == JSONType.string);
    }

    /***
     * Value getter/setter for `JSONType.string`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.string`.
     */
    @property string str() const pure @trusted return scope
    {
        enforce!JSONException(type == JSONType.string,
                                "JSONValue is not a string");
        return store.str;
    }
    /// ditto
    @property string str(return scope string v) pure nothrow @nogc @trusted return // TODO make @safe
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
     * Value getter/setter for `JSONType.integer`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.integer`.
     */
    @property long integer() const pure @safe
    {
        enforce!JSONException(type == JSONType.integer,
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
     * Value getter/setter for `JSONType.uinteger`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.uinteger`.
     */
    @property ulong uinteger() const pure @safe
    {
        enforce!JSONException(type == JSONType.uinteger,
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
     * Value getter/setter for `JSONType.float_`. Note that despite
     * the name, this is a $(B 64)-bit `double`, not a 32-bit `float`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.float_`.
     */
    @property double floating() const pure @safe
    {
        enforce!JSONException(type == JSONType.float_,
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
     * Value getter/setter for boolean stored in JSON.
     * Throws: `JSONException` for read access if `this.type` is not
     * `JSONType.true_` or `JSONType.false_`.
     */
    @property bool boolean() const pure @safe
    {
        if (type == JSONType.true_) return true;
        if (type == JSONType.false_) return false;

        throw new JSONException("JSONValue is not a boolean type");
    }
    /// ditto
    @property bool boolean(bool v) pure nothrow @safe @nogc
    {
        assign(v);
        return v;
    }
    ///
    @safe unittest
    {
        JSONValue j = true;
        assert(j.boolean == true);

        j.boolean = false;
        assert(j.boolean == false);

        j.integer = 12;
        import std.exception : assertThrown;
        assertThrown!JSONException(j.boolean);
    }

    /***
     * Value getter/setter for `JSONType.object`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.object`.
     * Note: this is @system because of the following pattern:
       ---
       auto a = &(json.object());
       json.uinteger = 0;        // overwrite AA pointer
       (*a)["hello"] = "world";  // segmentation fault
       ---
     */
    @property ref inout(JSONValue[string]) object() inout pure @system return
    {
        enforce!JSONException(type == JSONType.object,
                                "JSONValue is not an object");
        return store.object;
    }
    /// ditto
    @property JSONValue[string] object(return scope JSONValue[string] v) pure nothrow @nogc @trusted // TODO make @safe
    {
        assign(v);
        return v;
    }

    /***
     * Value getter for `JSONType.object`.
     * Unlike `object`, this retrieves the object by value and can be used in @safe code.
     *
     * A caveat is that, if the returned value is null, modifications will not be visible:
     * ---
     * JSONValue json;
     * json.object = null;
     * json.objectNoRef["hello"] = JSONValue("world");
     * assert("hello" !in json.object);
     * ---
     *
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.object`.
     */
    @property inout(JSONValue[string]) objectNoRef() inout pure @trusted
    {
        enforce!JSONException(type == JSONType.object,
                                "JSONValue is not an object");
        return store.object;
    }

    /***
     * Value getter/setter for `JSONType.array`.
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.array`.
     * Note: this is @system because of the following pattern:
       ---
       auto a = &(json.array());
       json.uinteger = 0;  // overwrite array pointer
       (*a)[0] = "world";  // segmentation fault
       ---
     */
    @property ref inout(JSONValue[]) array() scope return inout pure @system
    {
        enforce!JSONException(type == JSONType.array,
                                "JSONValue is not an array");
        return store.array;
    }
    /// ditto
    @property JSONValue[] array(return scope JSONValue[] v) pure nothrow @nogc @trusted scope // TODO make @safe
    {
        assign(v);
        return v;
    }

    /***
     * Value getter for `JSONType.array`.
     * Unlike `array`, this retrieves the array by value and can be used in @safe code.
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
     * Throws: `JSONException` for read access if `type` is not
     * `JSONType.array`.
     */
    @property inout(JSONValue[]) arrayNoRef() inout pure @trusted
    {
        enforce!JSONException(type == JSONType.array,
                                "JSONValue is not an array");
        return store.array;
    }

    /// Test whether the type is `JSONType.null_`
    @property bool isNull() const pure nothrow @safe @nogc
    {
        return type == JSONType.null_;
    }

    /***
     * Generic type value getter
     * A convenience getter that returns this `JSONValue` as the specified D type.
     * Note: only numeric, `bool`, `string`, `JSONValue[string]` and `JSONValue[]` types are accepted
     * Throws: `JSONException` if `T` cannot hold the contents of this `JSONValue`
     *         `ConvException` in case of integer overflow when converting to `T`
     */
    @property inout(T) get(T)() inout const pure @safe
    {
        static if (is(immutable T == immutable string))
        {
            return str;
        }
        else static if (is(immutable T == immutable bool))
        {
            return boolean;
        }
        else static if (isFloatingPoint!T)
        {
            switch (type)
            {
            case JSONType.float_:
                return cast(T) floating;
            case JSONType.uinteger:
                return cast(T) uinteger;
            case JSONType.integer:
                return cast(T) integer;
            default:
                throw new JSONException("JSONValue is not a number type");
            }
        }
        else static if (isIntegral!T)
        {
            switch (type)
            {
            case JSONType.uinteger:
                return uinteger.to!T;
            case JSONType.integer:
                return integer.to!T;
            default:
                throw new JSONException("JSONValue is not a an integral type");
            }
        }
        else
        {
            static assert(false, "Unsupported type");
        }
    }
    // This specialization is needed because arrayNoRef requires inout
    @property inout(T) get(T : JSONValue[])() inout pure @trusted /// ditto
    {
        return arrayNoRef;
    }
    /// ditto
    @property inout(T) get(T : JSONValue[string])() inout pure @trusted
    {
        return object;
    }
    ///
    @safe unittest
    {
        import std.exception;
        import std.conv;
        string s =
        `{
            "a": 123,
            "b": 3.1415,
            "c": "text",
            "d": true,
            "e": [1, 2, 3],
            "f": { "a": 1 },
            "g": -45,
            "h": ` ~ ulong.max.to!string ~ `,
         }`;

        struct a { }

        immutable json = parseJSON(s);
        assert(json["a"].get!double == 123.0);
        assert(json["a"].get!int == 123);
        assert(json["a"].get!uint == 123);
        assert(json["b"].get!double == 3.1415);
        assertThrown!JSONException(json["b"].get!int);
        assert(json["c"].get!string == "text");
        assert(json["d"].get!bool == true);
        assertNotThrown(json["e"].get!(JSONValue[]));
        assertNotThrown(json["f"].get!(JSONValue[string]));
        static assert(!__traits(compiles, json["a"].get!a));
        assertThrown!JSONException(json["e"].get!float);
        assertThrown!JSONException(json["d"].get!(JSONValue[string]));
        assertThrown!JSONException(json["f"].get!(JSONValue[]));
        assert(json["g"].get!int == -45);
        assertThrown!ConvException(json["g"].get!uint);
        assert(json["h"].get!ulong == ulong.max);
        assertThrown!ConvException(json["h"].get!uint);
        assertNotThrown(json["h"].get!float);
    }

    private void assign(T)(T arg)
    {
        static if (is(T : typeof(null)))
        {
            type_tag = JSONType.null_;
        }
        else static if (is(T : string))
        {
            type_tag = JSONType.string;
            string t = arg;
            () @trusted { store.str = t; }();
        }
        // https://issues.dlang.org/show_bug.cgi?id=15884
        else static if (isSomeString!T)
        {
            type_tag = JSONType.string;
            // FIXME: std.Array.Array(Range) is not deduced as 'pure'
            () @trusted {
                import std.utf : byUTF;
                store.str = cast(immutable)(arg.byUTF!char.array);
            }();
        }
        else static if (is(T : bool))
        {
            type_tag = arg ? JSONType.true_ : JSONType.false_;
        }
        else static if (is(T : ulong) && isUnsigned!T)
        {
            type_tag = JSONType.uinteger;
            store.uinteger = arg;
        }
        else static if (is(T : long))
        {
            type_tag = JSONType.integer;
            store.integer = arg;
        }
        else static if (isFloatingPoint!T)
        {
            type_tag = JSONType.float_;
            store.floating = arg;
        }
        else static if (is(T : Value[Key], Key, Value))
        {
            static assert(is(Key : string), "AA key must be string");
            type_tag = JSONType.object;
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
            type_tag = JSONType.array;
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
        type_tag = JSONType.array;
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
     * Constructor for `JSONValue`. If `arg` is a `JSONValue`
     * its value and type will be copied to the new `JSONValue`.
     * Note that this is a shallow copy: if type is `JSONType.object`
     * or `JSONType.array` then only the reference to the data will
     * be copied.
     * Otherwise, `arg` must be implicitly convertible to one of the
     * following types: `typeof(null)`, `string`, `ulong`,
     * `long`, `double`, an associative array `V[K]` for any `V`
     * and `K` i.e. a JSON object, any array or `bool`. The type will
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
        assert(j.type == JSONType.array);

        j = JSONValue( ["language": "D"] );
        assert(j.type == JSONType.object);
    }

    /**
     * An enum value that can be used to obtain a `JSONValue` representing
     * an empty JSON object.
     */
    enum emptyObject = JSONValue(string[string].init);
    ///
    @system unittest
    {
        JSONValue obj1 = JSONValue.emptyObject;
        assert(obj1.type == JSONType.object);
        obj1.object["a"] = JSONValue(1);
        assert(obj1.object["a"] == JSONValue(1));

        JSONValue obj2 = JSONValue.emptyObject;
        assert("a" !in obj2.object);
        obj2.object["b"] = JSONValue(5);
        assert(obj1 != obj2);
    }

    /**
     * An enum value that can be used to obtain a `JSONValue` representing
     * an empty JSON array.
     */
    enum emptyArray = JSONValue(JSONValue[].init);
    ///
    @system unittest
    {
        JSONValue arr1 = JSONValue.emptyArray;
        assert(arr1.type == JSONType.array);
        assert(arr1.array.length == 0);
        arr1.array ~= JSONValue("Hello");
        assert(arr1.array.length == 1);
        assert(arr1.array[0] == JSONValue("Hello"));

        JSONValue arr2 = JSONValue.emptyArray;
        assert(arr2.array.length == 0);
        assert(arr1 != arr2);
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
     * Throws: `JSONException` if `type` is not `JSONType.array`.
     */
    ref inout(JSONValue) opIndex(size_t i) inout pure @safe
    {
        auto a = this.arrayNoRef;
        enforce!JSONException(i < a.length,
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
     * Throws: `JSONException` if `type` is not `JSONType.object`.
     */
    ref inout(JSONValue) opIndex(return scope string k) inout pure @safe
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
     * Operator sets `value` for element of JSON object by `key`.
     *
     * If JSON value is null, then operator initializes it with object and then
     * sets `value` for it.
     *
     * Throws: `JSONException` if `type` is not `JSONType.object`
     * or `JSONType.null_`.
     */
    void opIndexAssign(T)(auto ref T value, string key)
    {
        enforce!JSONException(type == JSONType.object || type == JSONType.null_,
                                "JSONValue must be object or null");
        JSONValue[string] aa = null;
        if (type == JSONType.object)
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

    /// ditto
    void opIndexAssign(T)(T arg, size_t i)
    {
        auto a = this.arrayNoRef;
        enforce!JSONException(i < a.length,
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

    JSONValue opBinary(string op : "~", T)(T arg)
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

    void opOpAssign(string op : "~", T)(T arg)
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
     * Support for the `in` operator.
     *
     * Tests wether a key can be found in an object.
     *
     * Returns:
     *      when found, the `inout(JSONValue)*` that matches to the key,
     *      otherwise `null`.
     *
     * Throws: `JSONException` if the right hand side argument `JSONType`
     * is not `object`.
     */
    inout(JSONValue)* opBinaryRight(string op : "in")(string k) inout @safe
    {
        return k in this.objectNoRef;
    }
    ///
    @safe unittest
    {
        JSONValue j = [ "language": "D", "author": "walter" ];
        string a = ("author" in j).str;
        *("author" in j) = "Walter";
        assert(j["author"].str == "Walter");
    }

    ///
    bool opEquals(const JSONValue rhs) const @nogc nothrow pure @safe
    {
        return opEquals(rhs);
    }

    /// ditto
    bool opEquals(ref const JSONValue rhs) const @nogc nothrow pure @trusted
    {
        // Default doesn't work well since store is a union.  Compare only
        // what should be in store.
        // This is @trusted to remain nogc, nothrow, fast, and usable from @safe code.

        final switch (type_tag)
        {
        case JSONType.integer:
            switch (rhs.type_tag)
            {
                case JSONType.integer:
                    return store.integer == rhs.store.integer;
                case JSONType.uinteger:
                    return store.integer == rhs.store.uinteger;
                case JSONType.float_:
                    return store.integer == rhs.store.floating;
                default:
                    return false;
            }
        case JSONType.uinteger:
            switch (rhs.type_tag)
            {
                case JSONType.integer:
                    return store.uinteger == rhs.store.integer;
                case JSONType.uinteger:
                    return store.uinteger == rhs.store.uinteger;
                case JSONType.float_:
                    return store.uinteger == rhs.store.floating;
                default:
                    return false;
            }
        case JSONType.float_:
            switch (rhs.type_tag)
            {
                case JSONType.integer:
                    return store.floating == rhs.store.integer;
                case JSONType.uinteger:
                    return store.floating == rhs.store.uinteger;
                case JSONType.float_:
                    return store.floating == rhs.store.floating;
                default:
                    return false;
            }
        case JSONType.string:
            return type_tag == rhs.type_tag && store.str == rhs.store.str;
        case JSONType.object:
            return type_tag == rhs.type_tag && store.object == rhs.store.object;
        case JSONType.array:
            return type_tag == rhs.type_tag && store.array == rhs.store.array;
        case JSONType.true_:
        case JSONType.false_:
        case JSONType.null_:
            return type_tag == rhs.type_tag;
        }
    }

    ///
    @safe unittest
    {
        assert(JSONValue(0u) == JSONValue(0));
        assert(JSONValue(0u) == JSONValue(0.0));
        assert(JSONValue(0) == JSONValue(0.0));
    }

    /// Implements the foreach `opApply` interface for json arrays.
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

    /// Implements the foreach `opApply` interface for json objects.
    int opApply(scope int delegate(string key, ref JSONValue) dg) @system
    {
        enforce!JSONException(type == JSONType.object,
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
     * Implicitly calls `toJSON` on this JSONValue.
     *
     * $(I options) can be used to tweak the conversion behavior.
     */
    string toString(in JSONOptions options = JSONOptions.none) const @safe
    {
        return toJSON(this, false, options);
    }

    ///
    void toString(Out)(Out sink, in JSONOptions options = JSONOptions.none) const
    {
        toJSON(sink, this, false, options);
    }

    /***
     * Implicitly calls `toJSON` on this JSONValue, like `toString`, but
     * also passes $(I true) as $(I pretty) argument.
     *
     * $(I options) can be used to tweak the conversion behavior
     */
    string toPrettyString(in JSONOptions options = JSONOptions.none) const @safe
    {
        return toJSON(this, true, options);
    }

    ///
    void toPrettyString(Out)(Out sink, in JSONOptions options = JSONOptions.none) const
    {
        toJSON(sink, this, true, options);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=20874
@system unittest
{
    static struct MyCustomType
    {
        public string toString () const @system { return null; }
        alias toString this;
    }

    static struct B
    {
        public JSONValue asJSON() const @system { return JSONValue.init; }
        alias asJSON this;
    }

    if (false) // Just checking attributes
    {
        JSONValue json;
        MyCustomType ilovedlang;
        json = ilovedlang;
        json["foo"] = ilovedlang;
        auto s = ilovedlang in json;

        B b;
        json ~= b;
        json ~ b;
    }
}

/**
Parses a serialized string and returns a tree of JSON values.
Throws: $(LREF JSONException) if string does not follow the JSON grammar or the depth exceeds the max depth,
        $(LREF ConvException) if a number in the input cannot be represented by a native D type.
Params:
    json = json-formatted string to parse
    maxDepth = maximum depth of nesting allowed, -1 disables depth checking
    options = enable decoding string representations of NaN/Inf as float values
*/
JSONValue parseJSON(T)(T json, int maxDepth = -1, JSONOptions options = JSONOptions.none)
if (isSomeFiniteCharInputRange!T)
{
    import std.ascii : isDigit, isHexDigit, toUpper, toLower;
    import std.typecons : Nullable, Yes;
    JSONValue root;
    root.type_tag = JSONType.null_;

    // Avoid UTF decoding when possible, as it is unnecessary when
    // processing JSON.
    static if (is(T : const(char)[]))
        alias Char = char;
    else
        alias Char = Unqual!(ElementType!T);

    int depth = -1;
    Nullable!Char next;
    int line = 1, pos = 0;
    immutable bool strict = (options & JSONOptions.strictParsing) != 0;

    void error(string msg)
    {
        throw new JSONException(msg, line, pos);
    }

    if (json.empty)
    {
        if (strict)
        {
            error("Empty JSON body");
        }
        return root;
    }

    bool isWhite(dchar c)
    {
        if (strict)
        {
            // RFC 7159 has a stricter definition of whitespace than general ASCII.
            return c == ' ' || c == '\t' || c == '\n' || c == '\r';
        }
        import std.ascii : isWhite;
        // Accept ASCII NUL as whitespace in non-strict mode.
        return c == 0 || isWhite(c);
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
        if (next.isNull)
        {
            if (json.empty) return '\0';
            next = popChar();
        }
        return next.get;
    }

    Nullable!Char peekCharNullable()
    {
        if (next.isNull && !json.empty)
        {
            next = popChar();
        }
        return next;
    }

    void skipWhitespace()
    {
        while (true)
        {
            auto c = peekCharNullable();
            if (c.isNull ||
                !isWhite(c.get))
            {
                return;
            }
            next.nullify();
        }
    }

    Char getChar(bool SkipWhitespace = false)()
    {
        static if (SkipWhitespace) skipWhitespace();

        Char c;
        if (!next.isNull)
        {
            c = next.get;
            next.nullify();
        }
        else
            c = popChar();

        return c;
    }

    void checkChar(bool SkipWhitespace = true)(char c, bool caseSensitive = true)
    {
        static if (SkipWhitespace) skipWhitespace();
        auto c2 = getChar();
        if (!caseSensitive) c2 = toLower(c2);

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
                // Note: std.ascii.isControl can't be used for this test
                // because it considers ASCII DEL (0x7f) to be a control
                // character but RFC 7159 does not.
                // Accept unescaped ASCII NULs in non-strict mode.
                auto c = getChar();
                if (c < 0x20 && (strict || c != 0))
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
                    skipWhitespace();
                    if (!strict && peekChar() == '}')
                    {
                        break;
                    }
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
                    value.type_tag = JSONType.array;
                    break;
                }

                JSONValue[] arr;
                do
                {
                    skipWhitespace();
                    if (!strict && peekChar() == ']')
                    {
                        break;
                    }
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
                    value.type_tag = JSONType.float_;
                    break;
                }

                value.assign(str);
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

                if (strict && c == '0')
                {
                    number.put('0');
                    if (isDigit(peekChar()))
                    {
                        error("Additional digits not allowed after initial zero digit");
                    }
                }
                else
                {
                    readInteger();
                }

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
                    value.type_tag = JSONType.float_;
                    value.store.floating = parse!double(data);
                }
                else
                {
                    if (isNegative)
                    {
                        value.store.integer = parse!long(data);
                        value.type_tag = JSONType.integer;
                    }
                    else
                    {
                        // only set the correct union member to not confuse CTFE
                        ulong u = parse!ulong(data);
                        if (u & (1UL << 63))
                        {
                            value.store.uinteger = u;
                            value.type_tag = JSONType.uinteger;
                        }
                        else
                        {
                            value.store.integer = u;
                            value.type_tag = JSONType.integer;
                        }
                    }
                }
                break;

            case 'T':
                if (strict) goto default;
                goto case;
            case 't':
                value.type_tag = JSONType.true_;
                checkChar!false('r', strict);
                checkChar!false('u', strict);
                checkChar!false('e', strict);
                break;

            case 'F':
                if (strict) goto default;
                goto case;
            case 'f':
                value.type_tag = JSONType.false_;
                checkChar!false('a', strict);
                checkChar!false('l', strict);
                checkChar!false('s', strict);
                checkChar!false('e', strict);
                break;

            case 'N':
                if (strict) goto default;
                goto case;
            case 'n':
                value.type_tag = JSONType.null_;
                checkChar!false('u', strict);
                checkChar!false('l', strict);
                checkChar!false('l', strict);
                break;

            default:
                error(text("Unexpected character '", c, "'."));
        }

        depth--;
    }

    parseValue(root);
    if (strict)
    {
        skipWhitespace();
        if (!peekCharNullable().isNull) error("Trailing non-whitespace characters");
    }
    return root;
}

@safe unittest
{
    enum issue15742objectOfObject = `{ "key1": { "key2": 1 }}`;
    static assert(parseJSON(issue15742objectOfObject).type == JSONType.object);

    enum issue15742arrayOfArray = `[[1]]`;
    static assert(parseJSON(issue15742arrayOfArray).type == JSONType.array);
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

// https://issues.dlang.org/show_bug.cgi?id=20527
@safe unittest
{
    static assert(parseJSON(`{"a" : 2}`)["a"].integer == 2);
}

/**
Parses a serialized string and returns a tree of JSON values.
Throws: $(LREF JSONException) if the depth exceeds the max depth.
Params:
    json = json-formatted string to parse
    options = enable decoding string representations of NaN/Inf as float values
*/
JSONValue parseJSON(T)(T json, JSONOptions options)
if (isSomeFiniteCharInputRange!T)
{
    return parseJSON!T(json, -1, options);
}

/**
Takes a tree of JSON values and returns the serialized string.

Any Object types will be serialized in a key-sorted order.

If `pretty` is false no whitespaces are generated.
If `pretty` is true serialized string is formatted to be human-readable.
Set the $(LREF JSONOptions.specialFloatLiterals) flag is set in `options` to encode NaN/Infinity as strings.
*/
string toJSON(const ref JSONValue root, in bool pretty = false, in JSONOptions options = JSONOptions.none) @safe
{
    auto json = appender!string();
    toJSON(json, root, pretty, options);
    return json.data;
}

///
void toJSON(Out)(
    auto ref Out json,
    const ref JSONValue root,
    in bool pretty = false,
    in JSONOptions options = JSONOptions.none)
if (isOutputRange!(Out,char))
{
    void toStringImpl(Char)(string str)
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
                        == is(Char == dchar), "JSONOptions.escapeNonAsciiChars needs dchar strings");

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

    void toString(string str)
    {
        // Avoid UTF decoding when possible, as it is unnecessary when
        // processing JSON.
        if (options & JSONOptions.escapeNonAsciiChars)
            toStringImpl!dchar(str);
        else
            toStringImpl!char(str);
    }

    /* make the function infer @system when json.put() is @system
     */
    if (0)
        json.put(' ');

    /* Mark as @trusted because json.put() may be @system. This has difficulty
     * inferring @safe because it is recursive.
     */
    void toValueImpl(ref const JSONValue value, ulong indentLevel) @trusted
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
            case JSONType.object:
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
                            toValueImpl(member, indentLevel + 1);
                        }
                    }

                    import std.algorithm.sorting : sort;
                    // https://issues.dlang.org/show_bug.cgi?id=14439
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

            case JSONType.array:
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
                        toValueImpl(el, indentLevel + 1);
                    }
                    putEOL();
                    putTabs();
                    json.put(']');
                }
                break;

            case JSONType.string:
                toString(value.str);
                break;

            case JSONType.integer:
                json.put(to!string(value.store.integer));
                break;

            case JSONType.uinteger:
                json.put(to!string(value.store.uinteger));
                break;

            case JSONType.float_:
                import std.math.traits : isNaN, isInfinity;

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
                    import std.algorithm.searching : canFind;
                    import std.format : sformat;
                    // The correct formula for the number of decimal digits needed for lossless round
                    // trips is actually:
                    //     ceil(log(pow(2.0, double.mant_dig - 1)) / log(10.0) + 1) == (double.dig + 2)
                    // Anything less will round off (1 + double.epsilon)
                    char[25] buf;
                    auto result = buf[].sformat!"%.18g"(val);
                    json.put(result);
                    if (!result.canFind('e') && !result.canFind('.'))
                        json.put(".0");
                }
                break;

            case JSONType.true_:
                json.put("true");
                break;

            case JSONType.false_:
                json.put("false");
                break;

            case JSONType.null_:
                json.put("null");
                break;
        }
    }

    toValueImpl(root, 0);
}

 // https://issues.dlang.org/show_bug.cgi?id=12897
@safe unittest
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

// https://issues.dlang.org/show_bug.cgi?id=20511
@system unittest
{
    import std.format.write : formattedWrite;
    import std.range : nullSink, outputRangeObject;

    outputRangeObject!(const(char)[])(nullSink)
        .formattedWrite!"%s"(JSONValue.init);
}

// Issue 16432 - JSON incorrectly parses to string
@safe unittest
{
    // Floating points numbers are rounded to the nearest integer and thus get
    // incorrectly parsed

    import std.math.operations : isClose;

    string s = "{\"rating\": 3.0 }";
    JSONValue j = parseJSON(s);
    assert(j["rating"].type == JSONType.float_);
    j = j.toString.parseJSON;
    assert(j["rating"].type == JSONType.float_);
    assert(isClose(j["rating"].floating, 3.0));

    s = "{\"rating\": -3.0 }";
    j = parseJSON(s);
    assert(j["rating"].type == JSONType.float_);
    j = j.toString.parseJSON;
    assert(j["rating"].type == JSONType.float_);
    assert(isClose(j["rating"].floating, -3.0));

    // https://issues.dlang.org/show_bug.cgi?id=13660
    auto jv1 = JSONValue(4.0);
    auto textual = jv1.toString();
    auto jv2 = parseJSON(textual);
    assert(jv1.type == JSONType.float_);
    assert(textual == "4.0");
    assert(jv2.type == JSONType.float_);
}

@safe unittest
{
    // Adapted from https://github.com/dlang/phobos/pull/5005
    // Result from toString is not checked here, because this
    // might differ (%e-like or %f-like output) depending
    // on OS and compiler optimization.
    import std.math.operations : isClose;

    // test positive extreme values
    JSONValue j;
    j["rating"] = 1e18 - 65;
    assert(isClose(j.toString.parseJSON["rating"].floating, 1e18 - 65));

    j["rating"] = 1e18 - 64;
    assert(isClose(j.toString.parseJSON["rating"].floating, 1e18 - 64));

    // negative extreme values
    j["rating"] = -1e18 + 65;
    assert(isClose(j.toString.parseJSON["rating"].floating, -1e18 + 65));

    j["rating"] = -1e18 + 64;
    assert(isClose(j.toString.parseJSON["rating"].floating, -1e18 + 64));
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
    assert(jv.type == JSONType.string);
    assertNotThrown(jv.str);
    assertThrown!JSONException(jv.integer);
    assertThrown!JSONException(jv.uinteger);
    assertThrown!JSONException(jv.floating);
    assertThrown!JSONException(jv.object);
    assertThrown!JSONException(jv.array);
    assertThrown!JSONException(jv["aa"]);
    assertThrown!JSONException(jv[2]);

    jv = -3;
    assert(jv.type == JSONType.integer);
    assertNotThrown(jv.integer);

    jv = cast(uint) 3;
    assert(jv.type == JSONType.uinteger);
    assertNotThrown(jv.uinteger);

    jv = 3.0;
    assert(jv.type == JSONType.float_);
    assertNotThrown(jv.floating);

    jv = ["key" : "value"];
    assert(jv.type == JSONType.object);
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
        assert(value.type == JSONType.string);
        assertNotThrown(value.str);
        assert(value.str == "value");
    }

    jv = [3, 4, 5];
    assert(jv.type == JSONType.array);
    assertNotThrown(jv.array);
    assertNotThrown(jv[2]);
    foreach (size_t index, value; jv)
    {
        static assert(is(typeof(value) == JSONValue));
        assert(value.type == JSONType.integer);
        assertNotThrown(value.integer);
        assert(index == (value.integer-3));
    }

    jv = null;
    assert(jv.type == JSONType.null_);
    assert(jv.isNull);
    jv = "foo";
    assert(!jv.isNull);

    jv = JSONValue("value");
    assert(jv.type == JSONType.string);
    assert(jv.str == "value");

    JSONValue jv2 = JSONValue("value");
    assert(jv2.type == JSONType.string);
    assert(jv2.str == "value");

    JSONValue jv3 = JSONValue("\u001c");
    assert(jv3.type == JSONType.string);
    assert(jv3.str == "\u001C");
}

// https://issues.dlang.org/show_bug.cgi?id=11504
@system unittest
{
    JSONValue jv = 1;
    assert(jv.type == JSONType.integer);

    jv.str = "123";
    assert(jv.type == JSONType.string);
    assert(jv.str == "123");

    jv.integer = 1;
    assert(jv.type == JSONType.integer);
    assert(jv.integer == 1);

    jv.uinteger = 2u;
    assert(jv.type == JSONType.uinteger);
    assert(jv.uinteger == 2u);

    jv.floating = 1.5;
    assert(jv.type == JSONType.float_);
    assert(jv.floating == 1.5);

    jv.object = ["key" : JSONValue("value")];
    assert(jv.type == JSONType.object);
    assert(jv.object == ["key" : JSONValue("value")]);

    jv.array = [JSONValue(1), JSONValue(2), JSONValue(3)];
    assert(jv.type == JSONType.array);
    assert(jv.array == [JSONValue(1), JSONValue(2), JSONValue(3)]);

    jv = true;
    assert(jv.type == JSONType.true_);

    jv = false;
    assert(jv.type == JSONType.false_);

    enum E{True = true}
    jv = E.True;
    assert(jv.type == JSONType.true_);
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
    // https://issues.dlang.org/show_bug.cgi?id=12969

    JSONValue jv;
    jv["int"] = 123;

    assert(jv.type == JSONType.object);
    assert("int" in jv);
    assert(jv["int"].integer == 123);

    jv["array"] = [1, 2, 3, 4, 5];

    assert(jv["array"].type == JSONType.array);
    assert(jv["array"][2].integer == 3);

    jv["str"] = "D language";
    assert(jv["str"].type == JSONType.string);
    assert(jv["str"].str == "D language");

    jv["bool"] = false;
    assert(jv["bool"].type == JSONType.false_);

    assert(jv.object.length == 4);

    jv = [5, 4, 3, 2, 1];
    assert(jv.type == JSONType.array);
    assert(jv[3].integer == 2);
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
    import std.math.traits : isNaN, isInfinity;

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

// https://issues.dlang.org/show_bug.cgi?id=15884
pure nothrow @safe unittest
{
    import std.typecons;
    void Test(C)() {
        C[] a = ['x'];
        JSONValue testVal = a;
        assert(testVal.type == JSONType.string);
        testVal = a.idup;
        assert(testVal.type == JSONType.string);
    }
    Test!char();
    Test!wchar();
    Test!dchar();
}

// https://issues.dlang.org/show_bug.cgi?id=15885
@safe unittest
{
    enum bool realInDoublePrecision = real.mant_dig == double.mant_dig;

    static bool test(const double num0)
    {
        import std.math.operations : feqrel;
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

// https://issues.dlang.org/show_bug.cgi?id=17555
@safe unittest
{
    import std.exception : assertThrown;

    assertThrown!JSONException(parseJSON("\"a\nb\""));
}

// https://issues.dlang.org/show_bug.cgi?id=17556
@safe unittest
{
    auto v = JSONValue("\U0001D11E");
    auto j = toJSON(v, false, JSONOptions.escapeNonAsciiChars);
    assert(j == `"\uD834\uDD1E"`);
}

// https://issues.dlang.org/show_bug.cgi?id=5904
@safe unittest
{
    string s = `"\uD834\uDD1E"`;
    auto j = parseJSON(s);
    assert(j.str == "\U0001D11E");
}

// https://issues.dlang.org/show_bug.cgi?id=17557
@safe unittest
{
    assert(parseJSON("\"\xFF\"").str == "\xFF");
    assert(parseJSON("\"\U0001D11E\"").str == "\U0001D11E");
}

// https://issues.dlang.org/show_bug.cgi?id=17553
@safe unittest
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

// JSONOptions.doNotEscapeSlashes (https://issues.dlang.org/show_bug.cgi?id=17587)
@safe unittest
{
    assert(parseJSON(`"/"`).toString == `"\/"`);
    assert(parseJSON(`"\/"`).toString == `"\/"`);
    assert(parseJSON(`"/"`).toString(JSONOptions.doNotEscapeSlashes) == `"/"`);
    assert(parseJSON(`"\/"`).toString(JSONOptions.doNotEscapeSlashes) == `"/"`);
}

// JSONOptions.strictParsing (https://issues.dlang.org/show_bug.cgi?id=16639)
@safe unittest
{
    import std.exception : assertThrown;

    // Unescaped ASCII NULs
    assert(parseJSON("[\0]").type == JSONType.array);
    assertThrown!JSONException(parseJSON("[\0]", JSONOptions.strictParsing));
    assert(parseJSON("\"\0\"").str == "\0");
    assertThrown!JSONException(parseJSON("\"\0\"", JSONOptions.strictParsing));

    // Unescaped ASCII DEL (0x7f) in strings
    assert(parseJSON("\"\x7f\"").str == "\x7f");
    assert(parseJSON("\"\x7f\"", JSONOptions.strictParsing).str == "\x7f");

    // "true", "false", "null" case sensitivity
    assert(parseJSON("true").type == JSONType.true_);
    assert(parseJSON("true", JSONOptions.strictParsing).type == JSONType.true_);
    assert(parseJSON("True").type == JSONType.true_);
    assertThrown!JSONException(parseJSON("True", JSONOptions.strictParsing));
    assert(parseJSON("tRUE").type == JSONType.true_);
    assertThrown!JSONException(parseJSON("tRUE", JSONOptions.strictParsing));

    assert(parseJSON("false").type == JSONType.false_);
    assert(parseJSON("false", JSONOptions.strictParsing).type == JSONType.false_);
    assert(parseJSON("False").type == JSONType.false_);
    assertThrown!JSONException(parseJSON("False", JSONOptions.strictParsing));
    assert(parseJSON("fALSE").type == JSONType.false_);
    assertThrown!JSONException(parseJSON("fALSE", JSONOptions.strictParsing));

    assert(parseJSON("null").type == JSONType.null_);
    assert(parseJSON("null", JSONOptions.strictParsing).type == JSONType.null_);
    assert(parseJSON("Null").type == JSONType.null_);
    assertThrown!JSONException(parseJSON("Null", JSONOptions.strictParsing));
    assert(parseJSON("nULL").type == JSONType.null_);
    assertThrown!JSONException(parseJSON("nULL", JSONOptions.strictParsing));

    // Whitespace characters
    assert(parseJSON("[\f\v]").type == JSONType.array);
    assertThrown!JSONException(parseJSON("[\f\v]", JSONOptions.strictParsing));
    assert(parseJSON("[ \t\r\n]").type == JSONType.array);
    assert(parseJSON("[ \t\r\n]", JSONOptions.strictParsing).type == JSONType.array);

    // Empty input
    assert(parseJSON("").type == JSONType.null_);
    assertThrown!JSONException(parseJSON("", JSONOptions.strictParsing));

    // Numbers with leading '0's
    assert(parseJSON("01").integer == 1);
    assertThrown!JSONException(parseJSON("01", JSONOptions.strictParsing));
    assert(parseJSON("-01").integer == -1);
    assertThrown!JSONException(parseJSON("-01", JSONOptions.strictParsing));
    assert(parseJSON("0.01").floating == 0.01);
    assert(parseJSON("0.01", JSONOptions.strictParsing).floating == 0.01);
    assert(parseJSON("0e1").floating == 0);
    assert(parseJSON("0e1", JSONOptions.strictParsing).floating == 0);

    // Trailing characters after JSON value
    assert(parseJSON(`""asdf`).str == "");
    assertThrown!JSONException(parseJSON(`""asdf`, JSONOptions.strictParsing));
    assert(parseJSON("987\0").integer == 987);
    assertThrown!JSONException(parseJSON("987\0", JSONOptions.strictParsing));
    assert(parseJSON("987\0\0").integer == 987);
    assertThrown!JSONException(parseJSON("987\0\0", JSONOptions.strictParsing));
    assert(parseJSON("[]]").type == JSONType.array);
    assertThrown!JSONException(parseJSON("[]]", JSONOptions.strictParsing));
    assert(parseJSON("123 \t\r\n").integer == 123); // Trailing whitespace is OK
    assert(parseJSON("123 \t\r\n", JSONOptions.strictParsing).integer == 123);
}

@system unittest
{
    import std.algorithm.iteration : map;
    import std.array : array;
    import std.exception : assertThrown;

    string s = `{ "a" : [1,2,3,], }`;
    JSONValue j = parseJSON(s);
    assert(j["a"].array().map!(i => i.integer()).array == [1,2,3]);

    assertThrown(parseJSON(s, -1, JSONOptions.strictParsing));
}

@system unittest
{
    import std.algorithm.iteration : map;
    import std.array : array;
    import std.exception : assertThrown;

    string s = `{ "a" : { }  , }`;
    JSONValue j = parseJSON(s);
    assert("a" in j);
    auto t = j["a"].object();
    assert(t.empty);

    assertThrown(parseJSON(s, -1, JSONOptions.strictParsing));
}

// https://issues.dlang.org/show_bug.cgi?id=20330
@safe unittest
{
    import std.array : appender;

    string s = `{"a":[1,2,3]}`;
    JSONValue j = parseJSON(s);

    auto app = appender!string();
    j.toString(app);

    assert(app.data == s, app.data);
}

// https://issues.dlang.org/show_bug.cgi?id=20330
@safe unittest
{
    import std.array : appender;
    import std.format.write : formattedWrite;

    string s =
`{
    "a": [
        1,
        2,
        3
    ]
}`;
    JSONValue j = parseJSON(s);

    auto app = appender!string();
    j.toPrettyString(app);

    assert(app.data == s, app.data);
}

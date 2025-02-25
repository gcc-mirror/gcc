@system unittest
{
    import std.json;

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

@safe unittest
{
    import std.json;

          string s = "{ \"language\": \"D\" }";
          JSONValue j = parseJSON(s);
          assert(j.type == JSONType.object);
          assert(j["language"].type == JSONType.string);
    
}

@safe unittest
{
    import std.json;

        JSONValue j = [ "language": "D" ];

        // get value
        assert(j["language"].str == "D");

        // change existing key to new string
        j["language"].str = "Perl";
        assert(j["language"].str == "Perl");
    
}

@safe unittest
{
    import std.json;

        JSONValue j = true;
        assert(j.boolean == true);

        j.boolean = false;
        assert(j.boolean == false);

        j.integer = 12;
        import std.exception : assertThrown;
        assertThrown!JSONException(j.boolean);
    
}

@safe unittest
{
    import std.json;

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

@safe unittest
{
    import std.json;

        JSONValue j = JSONValue( "a string" );
        j = JSONValue(42);

        j = JSONValue( [1, 2, 3] );
        assert(j.type == JSONType.array);

        j = JSONValue( ["language": "D"] );
        assert(j.type == JSONType.object);
    
}

@system unittest
{
    import std.json;

        JSONValue obj1 = JSONValue.emptyObject;
        assert(obj1.type == JSONType.object);
        obj1.object["a"] = JSONValue(1);
        assert(obj1.object["a"] == JSONValue(1));

        JSONValue obj2 = JSONValue.emptyObject;
        assert("a" !in obj2.object);
        obj2.object["b"] = JSONValue(5);
        assert(obj1 != obj2);
    
}

@system unittest
{
    import std.json;

        JSONValue obj = JSONValue.emptyOrderedObject;
        assert(obj.type == JSONType.object);
        assert(obj.isOrdered);
        obj["b"] = JSONValue(2);
        obj["a"] = JSONValue(1);
        assert(obj["a"] == JSONValue(1));
        assert(obj["b"] == JSONValue(2));

        string[] keys;
        foreach (string k, JSONValue v; obj)
            keys ~= k;
        assert(keys == ["b", "a"]);
    
}

@system unittest
{
    import std.json;

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

@safe unittest
{
    import std.json;

        JSONValue j = JSONValue( [42, 43, 44] );
        assert( j[0].integer == 42 );
        assert( j[1].integer == 43 );
    
}

@safe unittest
{
    import std.json;

        JSONValue j = JSONValue( ["language": "D"] );
        assert( j["language"].str == "D" );
    
}

@safe unittest
{
    import std.json;

            JSONValue j = JSONValue( ["language": "D"] );
            j["language"].str = "Perl";
            assert( j["language"].str == "Perl" );
    
}

@safe unittest
{
    import std.json;

            JSONValue j = JSONValue( ["Perl", "C"] );
            j[1].str = "D";
            assert( j[1].str == "D" );
    
}

@safe unittest
{
    import std.json;

        JSONValue j = [ "language": "D", "author": "walter" ];
        string a = ("author" in j).str;
        *("author" in j) = "Walter";
        assert(j["author"].str == "Walter");
    
}

@safe unittest
{
    import std.json;

        assert(JSONValue(10).opEquals(JSONValue(10.0)));
        assert(JSONValue(10) != (JSONValue(10.5)));

        assert(JSONValue(1) != JSONValue(true));
        assert(JSONValue.emptyArray != JSONValue.emptyObject);

        assert(parseJSON(`{"a": 1, "b": 2}`).opEquals(parseJSON(`{"b": 2, "a": 1}`)));
    
}


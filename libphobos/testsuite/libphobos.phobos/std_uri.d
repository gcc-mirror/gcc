@safe unittest
{
    import std.uri;

    import std.exception : assertThrown;
    assertThrown!URIException("%ab".decode);
}

@safe unittest
{
    import std.uri;

    assert("foo%20bar".decode == "foo bar");
    assert("%3C%3E.@.%E2%84%A2".decode == "<>.@.™");
    assert("foo&/".decode == "foo&/");
    assert("!@#$&*(".decode == "!@#$&*(");
}

@safe unittest
{
    import std.uri;

    assert("foo%2F%26".decodeComponent == "foo/&");
    assert("dl%C3%A4ng%20r%C3%B6cks".decodeComponent == "dläng röcks");
    assert("!%40%23%24%25%5E%26*(".decodeComponent == "!@#$%^&*(");
}

@safe unittest
{
    import std.uri;

    assert("foo bar".encode == "foo%20bar");
    assert("<>.@.™".encode == "%3C%3E.@.%E2%84%A2");
    assert("foo/#?a=1&b=2".encode == "foo/#?a=1&b=2");
    assert("dlang+rocks!".encode == "dlang+rocks!");
    assert("!@#$%^&*(".encode == "!@#$%25%5E&*(");
}

@safe unittest
{
    import std.uri;

    assert("!@#$%^&*(".encodeComponent == "!%40%23%24%25%5E%26*(");
    assert("<>.@.™".encodeComponent == "%3C%3E.%40.%E2%84%A2");
    assert("foo/&".encodeComponent == "foo%2F%26");
    assert("dläng röcks".encodeComponent == "dl%C3%A4ng%20r%C3%B6cks");
    assert("dlang+rocks!".encodeComponent == "dlang%2Brocks!");
}

@safe pure unittest
{
    import std.uri;

    string s1 = "http://www.digitalmars.com/~fred/fredsRX.html#foo end!";
    assert(uriLength(s1) == 49);
    string s2 = "no uri here";
    assert(uriLength(s2) == -1);
    assert(uriLength("issue 14924") < 0);
}

@safe pure unittest
{
    import std.uri;

    string s1 = "my.e-mail@www.example-domain.com with garbage added";
    assert(emailLength(s1) == 32);
    string s2 = "no email address here";
    assert(emailLength(s2) == -1);
    assert(emailLength("issue 14924") < 0);
}


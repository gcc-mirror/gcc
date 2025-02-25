@safe unittest
{
    import std.ascii;

    import std.conv : to;

    assert(42.to!string(16, LetterCase.upper) == "2A");
    assert(42.to!string(16, LetterCase.lower) == "2a");
}

@safe unittest
{
    import std.ascii;

    import std.digest.hmac : hmac;
    import std.digest : toHexString;
    import std.digest.sha : SHA1;
    import std.string : representation;

    const sha1HMAC = "A very long phrase".representation
        .hmac!SHA1("secret".representation)
        .toHexString!(LetterCase.lower);
    assert(sha1HMAC == "49f2073c7bf58577e8c9ae59fe8cfd37c9ab94e5");
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    import std.algorithm.comparison, std.algorithm.searching, std.range, std.traits;

    // Because all ASCII characters fit in char, so do these
    static assert(ControlChar.ack.sizeof == 1);

    // All control characters except del are in row starting from 0
    static assert(EnumMembers!ControlChar.only.until(ControlChar.del).equal(iota(32)));

    static assert(ControlChar.nul == '\0');
    static assert(ControlChar.bel == '\a');
    static assert(ControlChar.bs  == '\b');
    static assert(ControlChar.ff  == '\f');
    static assert(ControlChar.lf  == '\n');
    static assert(ControlChar.cr  == '\r');
    static assert(ControlChar.tab == '\t');
    static assert(ControlChar.vt  == '\v');
}

@safe pure nothrow unittest
{
    import std.ascii;

    import std.conv;
    //Control character table can be used in place of hexcodes.
    with (ControlChar) assert(text("Phobos", us, "Deimos", us, "Tango", rs) == "Phobos\x1FDeimos\x1FTango\x1E");
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isAlphaNum('A'));
    assert( isAlphaNum('1'));
    assert(!isAlphaNum('#'));

    // N.B.: does not return true for non-ASCII Unicode alphanumerics:
    assert(!isAlphaNum('á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isAlpha('A'));
    assert(!isAlpha('1'));
    assert(!isAlpha('#'));

    // N.B.: does not return true for non-ASCII Unicode alphabetic characters:
    assert(!isAlpha('á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isLower('a'));
    assert(!isLower('A'));
    assert(!isLower('#'));

    // N.B.: does not return true for non-ASCII Unicode lowercase letters
    assert(!isLower('á'));
    assert(!isLower('Á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isUpper('A'));
    assert(!isUpper('a'));
    assert(!isUpper('#'));

    // N.B.: does not return true for non-ASCII Unicode uppercase letters
    assert(!isUpper('á'));
    assert(!isUpper('Á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isDigit('3'));
    assert( isDigit('8'));
    assert(!isDigit('B'));
    assert(!isDigit('#'));

    // N.B.: does not return true for non-ASCII Unicode numbers
    assert(!isDigit('０')); // full-width digit zero (U+FF10)
    assert(!isDigit('４')); // full-width digit four (U+FF14)
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isOctalDigit('0'));
    assert( isOctalDigit('7'));
    assert(!isOctalDigit('8'));
    assert(!isOctalDigit('A'));
    assert(!isOctalDigit('#'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isHexDigit('0'));
    assert( isHexDigit('A'));
    assert( isHexDigit('f')); // lowercase hex digits are accepted
    assert(!isHexDigit('g'));
    assert(!isHexDigit('G'));
    assert(!isHexDigit('#'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isWhite(' '));
    assert( isWhite('\t'));
    assert( isWhite('\n'));
    assert(!isWhite('1'));
    assert(!isWhite('a'));
    assert(!isWhite('#'));

    // N.B.: Does not return true for non-ASCII Unicode whitespace characters.
    static import std.uni;
    assert(std.uni.isWhite('\u00A0'));
    assert(!isWhite('\u00A0')); // std.ascii.isWhite
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isControl('\0'));
    assert( isControl('\022'));
    assert( isControl('\n')); // newline is both whitespace and control
    assert(!isControl(' '));
    assert(!isControl('1'));
    assert(!isControl('a'));
    assert(!isControl('#'));

    // N.B.: non-ASCII Unicode control characters are not recognized:
    assert(!isControl('\u0080'));
    assert(!isControl('\u2028'));
    assert(!isControl('\u2029'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isPunctuation('.'));
    assert( isPunctuation(','));
    assert( isPunctuation(':'));
    assert( isPunctuation('!'));
    assert( isPunctuation('#'));
    assert( isPunctuation('~'));
    assert( isPunctuation('+'));
    assert( isPunctuation('_'));

    assert(!isPunctuation('1'));
    assert(!isPunctuation('a'));
    assert(!isPunctuation(' '));
    assert(!isPunctuation('\n'));
    assert(!isPunctuation('\0'));

    // N.B.: Non-ASCII Unicode punctuation characters are not recognized.
    assert(!isPunctuation('\u2012')); // (U+2012 = en-dash)
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isGraphical('1'));
    assert( isGraphical('a'));
    assert( isGraphical('#'));
    assert(!isGraphical(' ')); // whitespace is not graphical
    assert(!isGraphical('\n'));
    assert(!isGraphical('\0'));

    // N.B.: Unicode graphical characters are not regarded as such.
    assert(!isGraphical('á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isPrintable(' '));  // whitespace is printable
    assert( isPrintable('1'));
    assert( isPrintable('a'));
    assert( isPrintable('#'));
    assert(!isPrintable('\0')); // control characters are not printable

    // N.B.: Printable non-ASCII Unicode characters are not recognized.
    assert(!isPrintable('á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert( isASCII('a'));
    assert(!isASCII('á'));
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert(toLower('a') == 'a');
    assert(toLower('A') == 'a');
    assert(toLower('#') == '#');

    // N.B.: Non-ASCII Unicode uppercase letters are not converted.
    assert(toLower('Á') == 'Á');
}

@safe pure nothrow @nogc unittest
{
    import std.ascii;

    assert(toUpper('a') == 'A');
    assert(toUpper('A') == 'A');
    assert(toUpper('#') == '#');

    // N.B.: Non-ASCII Unicode lowercase letters are not converted.
    assert(toUpper('á') == 'á');
}


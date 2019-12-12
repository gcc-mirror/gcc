// Written in the D programming language.

/++
    Functions which operate on ASCII characters.

    All of the functions in std._ascii accept Unicode characters but
    effectively ignore them if they're not ASCII. All $(D isX) functions return
    $(D false) for non-ASCII characters, and all $(D toX) functions do nothing
    to non-ASCII characters.

    For functions which operate on Unicode characters, see
    $(MREF std, uni).

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Validation) $(TD
        $(LREF isAlpha)
        $(LREF isAlphaNum)
        $(LREF isASCII)
        $(LREF isControl)
        $(LREF isDigit)
        $(LREF isGraphical)
        $(LREF isHexDigit)
        $(LREF isOctalDigit)
        $(LREF isPrintable)
        $(LREF isPunctuation)
        $(LREF isUpper)
        $(LREF isWhite)
))
$(TR $(TD Conversions) $(TD
        $(LREF toLower)
        $(LREF toUpper)
))
$(TR $(TD Constants) $(TD
        $(LREF digits)
        $(LREF fullHexDigits)
        $(LREF hexDigits)
        $(LREF letters)
        $(LREF lowercase)
        $(LREF lowerHexDigits)
        $(LREF newline)
        $(LREF octalDigits)
        $(LREF uppercase)
        $(LREF whitespace)
))
$(TR $(TD Enums) $(TD
        $(LREF LetterCase)
))
))
    References:
        $(LINK2 http://www.digitalmars.com/d/ascii-table.html, ASCII Table),
        $(HTTP en.wikipedia.org/wiki/Ascii, Wikipedia)

    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP digitalmars.com, Walter Bright) and Jonathan M Davis
    Source:    $(PHOBOSSRC std/_ascii.d)
  +/
module std.ascii;

version (unittest)
{
    // FIXME: When dmd bug #314 is fixed, make these selective.
    import std.meta; // : AliasSeq;
    import std.range; // : chain;
    import std.traits; // : functionAttributes, FunctionAttribute, isSafe;
}


immutable fullHexDigits  = "0123456789ABCDEFabcdef";     /// 0 .. 9A .. Fa .. f
immutable hexDigits      = fullHexDigits[0 .. 16];         /// 0 .. 9A .. F
immutable lowerHexDigits = "0123456789abcdef";           /// 0 .. 9a .. f
immutable digits         = hexDigits[0 .. 10];             /// 0 .. 9
immutable octalDigits    = digits[0 .. 8];                 /// 0 .. 7
immutable letters        = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"; /// A .. Za .. z
immutable uppercase      = letters[0 .. 26];               /// A .. Z
immutable lowercase      = letters[26 .. 52];              /// a .. z
immutable whitespace     = " \t\v\r\n\f";                /// ASCII _whitespace

/++
    Letter case specifier.
  +/
enum LetterCase : bool
{
    upper, /// Upper case letters
    lower  /// Lower case letters
}

///
@safe unittest
{
    import std.conv : to;

    assert(42.to!string(16, LetterCase.upper) == "2A");
    assert(42.to!string(16, LetterCase.lower) == "2a");
}

///
@system unittest
{
    import std.digest.hmac : hmac;
    import std.digest.digest : toHexString;
    import std.digest.sha : SHA1;
    import std.string : representation;

    const sha1HMAC = "A very long phrase".representation
        .hmac!SHA1("secret".representation)
        .toHexString!(LetterCase.lower);
    assert(sha1HMAC == "49f2073c7bf58577e8c9ae59fe8cfd37c9ab94e5");
}

/// Newline sequence for this system.
version (Windows)
    immutable newline = "\r\n";
else version (Posix)
    immutable newline = "\n";
else
    static assert(0, "Unsupported OS");


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a letter or a number (0 .. 9, a .. z, A .. Z).
  +/
bool isAlphaNum(dchar c) @safe pure nothrow @nogc
{
    return c <= 'z' && c >= '0' && (c <= '9' || c >= 'a' || (c >= 'A' && c <= 'Z'));
}

///
@safe pure nothrow @nogc unittest
{
    assert( isAlphaNum('A'));
    assert( isAlphaNum('1'));
    assert(!isAlphaNum('#'));

    // N.B.: does not return true for non-ASCII Unicode alphanumerics:
    assert(!isAlphaNum('á'));
}

@safe unittest
{
    foreach (c; chain(digits, octalDigits, fullHexDigits, letters, lowercase, uppercase))
        assert(isAlphaNum(c));

    foreach (c; whitespace)
        assert(!isAlphaNum(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is an ASCII letter (A .. Z, a .. z).
  +/
bool isAlpha(dchar c) @safe pure nothrow @nogc
{
    // Optimizer can turn this into a bitmask operation on 64 bit code
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

///
@safe pure nothrow @nogc unittest
{
    assert( isAlpha('A'));
    assert(!isAlpha('1'));
    assert(!isAlpha('#'));

    // N.B.: does not return true for non-ASCII Unicode alphabetic characters:
    assert(!isAlpha('á'));
}

@safe unittest
{
    foreach (c; chain(letters, lowercase, uppercase))
        assert(isAlpha(c));

    foreach (c; chain(digits, octalDigits, whitespace))
        assert(!isAlpha(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a lowercase ASCII letter (a .. z).
  +/
bool isLower(dchar c) @safe pure nothrow @nogc
{
    return c >= 'a' && c <= 'z';
}

///
@safe pure nothrow @nogc unittest
{
    assert( isLower('a'));
    assert(!isLower('A'));
    assert(!isLower('#'));

    // N.B.: does not return true for non-ASCII Unicode lowercase letters
    assert(!isLower('á'));
    assert(!isLower('Á'));
}

@safe unittest
{
    foreach (c; lowercase)
        assert(isLower(c));

    foreach (c; chain(digits, uppercase, whitespace))
        assert(!isLower(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is an uppercase ASCII letter (A .. Z).
  +/
bool isUpper(dchar c) @safe pure nothrow @nogc
{
    return c <= 'Z' && 'A' <= c;
}

///
@safe pure nothrow @nogc unittest
{
    assert( isUpper('A'));
    assert(!isUpper('a'));
    assert(!isUpper('#'));

    // N.B.: does not return true for non-ASCII Unicode uppercase letters
    assert(!isUpper('á'));
    assert(!isUpper('Á'));
}

@safe unittest
{
    foreach (c; uppercase)
        assert(isUpper(c));

    foreach (c; chain(digits, lowercase, whitespace))
        assert(!isUpper(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a digit (0 .. 9).
  +/
bool isDigit(dchar c) @safe pure nothrow @nogc
{
    return '0' <= c && c <= '9';
}

///
@safe pure nothrow @nogc unittest
{
    assert( isDigit('3'));
    assert( isDigit('8'));
    assert(!isDigit('B'));
    assert(!isDigit('#'));

    // N.B.: does not return true for non-ASCII Unicode numbers
    assert(!isDigit('０')); // full-width digit zero (U+FF10)
    assert(!isDigit('４')); // full-width digit four (U+FF14)
}

@safe unittest
{
    foreach (c; digits)
        assert(isDigit(c));

    foreach (c; chain(letters, whitespace))
        assert(!isDigit(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a digit in base 8 (0 .. 7).
  +/
bool isOctalDigit(dchar c) @safe pure nothrow @nogc
{
    return c >= '0' && c <= '7';
}

///
@safe pure nothrow @nogc unittest
{
    assert( isOctalDigit('0'));
    assert( isOctalDigit('7'));
    assert(!isOctalDigit('8'));
    assert(!isOctalDigit('A'));
    assert(!isOctalDigit('#'));
}

@safe unittest
{
    foreach (c; octalDigits)
        assert(isOctalDigit(c));

    foreach (c; chain(letters, ['8', '9'], whitespace))
        assert(!isOctalDigit(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a digit in base 16 (0 .. 9, A .. F, a .. f).
  +/
bool isHexDigit(dchar c) @safe pure nothrow @nogc
{
    return c <= 'f' && c >= '0' && (c <= '9' || c >= 'a' || (c >= 'A' && c <= 'F'));
}

///
@safe pure nothrow @nogc unittest
{
    assert( isHexDigit('0'));
    assert( isHexDigit('A'));
    assert( isHexDigit('f')); // lowercase hex digits are accepted
    assert(!isHexDigit('g'));
    assert(!isHexDigit('G'));
    assert(!isHexDigit('#'));
}

@safe unittest
{
    foreach (c; fullHexDigits)
        assert(isHexDigit(c));

    foreach (c; chain(lowercase[6 .. $], uppercase[6 .. $], whitespace))
        assert(!isHexDigit(c));
}


/++
    Params: c = The character to test.
    Returns: Whether or not $(D c) is a whitespace character. That includes the
    space, tab, vertical tab, form feed, carriage return, and linefeed
    characters.
  +/
bool isWhite(dchar c) @safe pure nothrow @nogc
{
    return c == ' ' || (c >= 0x09 && c <= 0x0D);
}

///
@safe pure nothrow @nogc unittest
{
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

@safe unittest
{
    foreach (c; whitespace)
        assert(isWhite(c));

    foreach (c; chain(digits, letters))
        assert(!isWhite(c));
}


/++
    Params: c = The character to test.
    Returns: Whether $(D c) is a control character.
  +/
bool isControl(dchar c) @safe pure nothrow @nogc
{
    return c < 0x20 || c == 0x7F;
}

///
@safe pure nothrow @nogc unittest
{
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

@safe unittest
{
    foreach (dchar c; 0 .. 32)
        assert(isControl(c));
    assert(isControl(127));

    foreach (c; chain(digits, letters, [' ']))
        assert(!isControl(c));
}


/++
    Params: c = The character to test.
    Returns: Whether or not $(D c) is a punctuation character. That includes
    all ASCII characters which are not control characters, letters, digits, or
    whitespace.
  +/
bool isPunctuation(dchar c) @safe pure nothrow @nogc
{
    return c <= '~' && c >= '!' && !isAlphaNum(c);
}

///
@safe pure nothrow @nogc unittest
{
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

@safe unittest
{
    foreach (dchar c; 0 .. 128)
    {
        if (isControl(c) || isAlphaNum(c) || c == ' ')
            assert(!isPunctuation(c));
        else
            assert(isPunctuation(c));
    }
}


/++
    Params: c = The character to test.
    Returns: Whether or not $(D c) is a printable character other than the
    space character.
  +/
bool isGraphical(dchar c) @safe pure nothrow @nogc
{
    return '!' <= c && c <= '~';
}

///
@safe pure nothrow @nogc unittest
{
    assert( isGraphical('1'));
    assert( isGraphical('a'));
    assert( isGraphical('#'));
    assert(!isGraphical(' ')); // whitespace is not graphical
    assert(!isGraphical('\n'));
    assert(!isGraphical('\0'));

    // N.B.: Unicode graphical characters are not regarded as such.
    assert(!isGraphical('á'));
}

@safe unittest
{
    foreach (dchar c; 0 .. 128)
    {
        if (isControl(c) || c == ' ')
            assert(!isGraphical(c));
        else
            assert(isGraphical(c));
    }
}


/++
    Params: c = The character to test.
    Returns: Whether or not $(D c) is a printable character - including the
    space character.
  +/
bool isPrintable(dchar c) @safe pure nothrow @nogc
{
    return c >= ' ' && c <= '~';
}

///
@safe pure nothrow @nogc unittest
{
    assert( isPrintable(' '));  // whitespace is printable
    assert( isPrintable('1'));
    assert( isPrintable('a'));
    assert( isPrintable('#'));
    assert(!isPrintable('\0')); // control characters are not printable

    // N.B.: Printable non-ASCII Unicode characters are not recognized.
    assert(!isPrintable('á'));
}

@safe unittest
{
    foreach (dchar c; 0 .. 128)
    {
        if (isControl(c))
            assert(!isPrintable(c));
        else
            assert(isPrintable(c));
    }
}


/++
    Params: c = The character to test.
    Returns: Whether or not $(D c) is in the ASCII character set - i.e. in the
    range 0 .. 0x7F.
  +/
pragma(inline, true)
bool isASCII(dchar c) @safe pure nothrow @nogc
{
    return c <= 0x7F;
}

///
@safe pure nothrow @nogc unittest
{
    assert( isASCII('a'));
    assert(!isASCII('á'));
}

@safe unittest
{
    foreach (dchar c; 0 .. 128)
        assert(isASCII(c));

    assert(!isASCII(128));
}


/++
    Converts an ASCII letter to lowercase.

    Params: c = A character of any type that implicitly converts to $(D dchar).
    In the case where it's a built-in type, or an enum of a built-in type,
    $(D Unqual!(OriginalType!C)) is returned, whereas if it's a user-defined
    type, $(D dchar) is returned.

    Returns: The corresponding lowercase letter, if $(D c) is an uppercase
    ASCII character, otherwise $(D c) itself.
  +/
auto toLower(C)(C c)
if (is(C : dchar))
{
    import std.traits : isAggregateType, OriginalType, Unqual;

    alias OC = OriginalType!C;
    static if (isAggregateType!OC)
        alias R = dchar;
    else
        alias R = Unqual!OC;

    return isUpper(c) ? cast(R)(cast(R) c + 'a' - 'A') : cast(R) c;
}

///
@safe pure nothrow @nogc unittest
{
    assert(toLower('a') == 'a');
    assert(toLower('A') == 'a');
    assert(toLower('#') == '#');

    // N.B.: Non-ASCII Unicode uppercase letters are not converted.
    assert(toLower('Á') == 'Á');
}

@safe pure nothrow unittest
{

    foreach (C; AliasSeq!(char, wchar, dchar, immutable char, ubyte))
    {
        foreach (i, c; uppercase)
            assert(toLower(cast(C) c) == lowercase[i]);

        foreach (C c; 0 .. 128)
        {
            if (c < 'A' || c > 'Z')
                assert(toLower(c) == c);
            else
                assert(toLower(c) != c);
        }

        foreach (C c; 128 .. C.max)
            assert(toLower(c) == c);

        //CTFE
        static assert(toLower(cast(C)'a') == 'a');
        static assert(toLower(cast(C)'A') == 'a');
    }
}


/++
    Converts an ASCII letter to uppercase.

    Params: c = Any type which implicitly converts to $(D dchar). In the case
    where it's a built-in type, or an enum of a built-in type,
    $(D Unqual!(OriginalType!C)) is returned, whereas if it's a user-defined
    type, $(D dchar) is returned.

    Returns: The corresponding uppercase letter, if $(D c) is a lowercase ASCII
    character, otherwise $(D c) itself.
  +/
auto toUpper(C)(C c)
if (is(C : dchar))
{
    import std.traits : isAggregateType, OriginalType, Unqual;

    alias OC = OriginalType!C;
    static if (isAggregateType!OC)
        alias R = dchar;
    else
        alias R = Unqual!OC;

    return isLower(c) ? cast(R)(cast(R) c - ('a' - 'A')) : cast(R) c;
}

///
@safe pure nothrow @nogc unittest
{
    assert(toUpper('a') == 'A');
    assert(toUpper('A') == 'A');
    assert(toUpper('#') == '#');

    // N.B.: Non-ASCII Unicode lowercase letters are not converted.
    assert(toUpper('á') == 'á');
}

@safe pure nothrow unittest
{
    foreach (C; AliasSeq!(char, wchar, dchar, immutable char, ubyte))
    {
        foreach (i, c; lowercase)
            assert(toUpper(cast(C) c) == uppercase[i]);

        foreach (C c; 0 .. 128)
        {
            if (c < 'a' || c > 'z')
                assert(toUpper(c) == c);
            else
                assert(toUpper(c) != c);
        }

        foreach (C c; 128 .. C.max)
            assert(toUpper(c) == c);

        //CTFE
        static assert(toUpper(cast(C)'a') == 'A');
        static assert(toUpper(cast(C)'A') == 'A');
    }
}


@safe unittest //Test both toUpper and toLower with non-builtin
{
    //User Defined [Char|Wchar|Dchar]
    static struct UDC {  char c; alias c this; }
    static struct UDW { wchar c; alias c this; }
    static struct UDD { dchar c; alias c this; }
    //[Char|Wchar|Dchar] Enum
    enum CE :  char {a = 'a', A = 'A'}
    enum WE : wchar {a = 'a', A = 'A'}
    enum DE : dchar {a = 'a', A = 'A'}
    //User Defined [Char|Wchar|Dchar] Enum
    enum UDCE : UDC {a = UDC('a'), A = UDC('A')}
    enum UDWE : UDW {a = UDW('a'), A = UDW('A')}
    enum UDDE : UDD {a = UDD('a'), A = UDD('A')}

    //User defined types with implicit cast to dchar test.
    foreach (Char; AliasSeq!(UDC, UDW, UDD))
    {
        assert(toLower(Char('a')) == 'a');
        assert(toLower(Char('A')) == 'a');
        static assert(toLower(Char('a')) == 'a');
        static assert(toLower(Char('A')) == 'a');
        static assert(toUpper(Char('a')) == 'A');
        static assert(toUpper(Char('A')) == 'A');
    }

    //Various enum tests.
    foreach (Enum; AliasSeq!(CE, WE, DE, UDCE, UDWE, UDDE))
    {
        assert(toLower(Enum.a) == 'a');
        assert(toLower(Enum.A) == 'a');
        assert(toUpper(Enum.a) == 'A');
        assert(toUpper(Enum.A) == 'A');
        static assert(toLower(Enum.a) == 'a');
        static assert(toLower(Enum.A) == 'a');
        static assert(toUpper(Enum.a) == 'A');
        static assert(toUpper(Enum.A) == 'A');
    }

    //Return value type tests for enum of non-UDT. These should be the original type.
    foreach (T; AliasSeq!(CE, WE, DE))
    {
        alias C = OriginalType!T;
        static assert(is(typeof(toLower(T.init)) == C));
        static assert(is(typeof(toUpper(T.init)) == C));
    }

    //Return value tests for UDT and enum of UDT. These should be dchar
    foreach (T; AliasSeq!(UDC, UDW, UDD, UDCE, UDWE, UDDE))
    {
        static assert(is(typeof(toLower(T.init)) == dchar));
        static assert(is(typeof(toUpper(T.init)) == dchar));
    }
}

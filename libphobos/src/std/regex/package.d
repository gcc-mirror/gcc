/++
  $(LINK2 https://en.wikipedia.org/wiki/Regular_expression, Regular expressions)
  are a commonly used method of pattern matching
  on strings, with $(I regex) being a catchy word for a pattern in this domain
  specific language. Typical problems usually solved by regular expressions
  include validation of user input and the ubiquitous find $(AMP) replace
  in text processing utilities.

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Matching) $(TD
        $(LREF bmatch)
        $(LREF match)
        $(LREF matchAll)
        $(LREF matchFirst)
))
$(TR $(TD Building) $(TD
        $(LREF ctRegex)
        $(LREF escaper)
        $(LREF regex)
))
$(TR $(TD Replace) $(TD
        $(LREF replace)
        $(LREF replaceAll)
        $(LREF replaceAllInto)
        $(LREF replaceFirst)
        $(LREF replaceFirstInto)
))
$(TR $(TD Split) $(TD
        $(LREF split)
        $(LREF splitter)
))
$(TR $(TD Objects) $(TD
        $(LREF Captures)
        $(LREF Regex)
        $(LREF RegexException)
        $(LREF RegexMatch)
        $(LREF Splitter)
        $(LREF StaticRegex)
))
))

  $(SECTION Synopsis)

  Create a regex at runtime:
  $(RUNNABLE_EXAMPLE
  $(RUNNABLE_EXAMPLE_STDIN
They met on 24/01/1970.
7/8/99 wasn't as hot as 7/8/2022.
)
      ---
      import std.regex;
      import std.stdio;
      // Print out all possible dd/mm/yy(yy) dates found in user input.
      auto r = regex(r"\b[0-9][0-9]?/[0-9][0-9]?/[0-9][0-9](?:[0-9][0-9])?\b");
      foreach (line; stdin.byLine)
      {
        // matchAll() returns a range that can be iterated
        // to get all subsequent matches.
        foreach (c; matchAll(line, r))
            writeln(c.hit);
      }
      ---
  )
  Create a static regex at compile-time, which contains fast native code:
  $(RUNNABLE_EXAMPLE
  ---
  import std.regex;
  auto ctr = ctRegex!(`^.*/([^/]+)/?$`);

  // It works just like a normal regex:
  auto c2 = matchFirst("foo/bar", ctr);   // First match found here, if any
  assert(!c2.empty);   // Be sure to check if there is a match before examining contents!
  assert(c2[1] == "bar");   // Captures is a range of submatches: 0 = full match.
  ---
  )
  Multi-pattern regex:
  $(RUNNABLE_EXAMPLE
  ---
  import std.regex;
  auto multi = regex([`\d+,\d+`, `([a-z]+):(\d+)`]);
  auto m = "abc:43 12,34".matchAll(multi);
  assert(m.front.whichPattern == 2);
  assert(m.front[1] == "abc");
  assert(m.front[2] == "43");
  m.popFront();
  assert(m.front.whichPattern == 1);
  assert(m.front[0] == "12,34");
  ---
  )
  $(LREF Captures) and `opCast!bool`:
  $(RUNNABLE_EXAMPLE
  ---
  import std.regex;
  // The result of `matchAll/matchFirst` is directly testable with `if/assert/while`,
  // e.g. test if a string consists of letters only:
  assert(matchFirst("LettersOnly", `^\p{L}+$`));

  // And we can take advantage of the ability to define a variable in the IfCondition:
  if (const captures = matchFirst("At l34st one digit, but maybe more...", `((\d)(\d*))`))
  {
      assert(captures[2] == "3");
      assert(captures[3] == "4");
      assert(captures[1] == "34");
  }
  ---
  )
  See_Also: $(LINK2 https://dlang.org/spec/statement.html#IfCondition, `IfCondition`).

  $(SECTION Syntax and general information)
  The general usage guideline is to keep regex complexity on the side of simplicity,
  as its capabilities reside in purely character-level manipulation.
  As such it's ill-suited for tasks involving higher level invariants
  like matching an integer number $(U bounded) in an [a,b] interval.
  Checks of this sort of are better addressed by additional post-processing.

  The basic syntax shouldn't surprise experienced users of regular expressions.
  For an introduction to `std.regex` see a
  $(HTTP dlang.org/regular-expression.html, short tour) of the module API
  and its abilities.

  There are other web resources on regular expressions to help newcomers,
  and a good $(HTTP www.regular-expressions.info, reference with tutorial)
  can easily be found.

  This library uses a remarkably common ECMAScript syntax flavor
  with the following extensions:
  $(UL
    $(LI Named subexpressions, with Python syntax. )
    $(LI Unicode properties such as Scripts, Blocks and common binary properties e.g Alphabetic, White_Space, Hex_Digit etc.)
    $(LI Arbitrary length and complexity lookbehind, including lookahead in lookbehind and vise-versa.)
  )

  $(REG_START Pattern syntax )
  $(I std.regex operates on codepoint level,
    'character' in this table denotes a single Unicode codepoint.)
  $(REG_TABLE
    $(REG_TITLE Pattern element, Semantics )
    $(REG_TITLE Atoms, Match single characters )
    $(REG_ROW any character except [{|*+?()^$, Matches the character itself. )
    $(REG_ROW ., In single line mode matches any character.
      Otherwise it matches any character except '\n' and '\r'. )
    $(REG_ROW [class], Matches a single character
      that belongs to this character class. )
    $(REG_ROW [^class], Matches a single character that
      does $(U not) belong to this character class.)
    $(REG_ROW \cC, Matches the control character corresponding to letter C)
    $(REG_ROW \xXX, Matches a character with hexadecimal value of XX. )
    $(REG_ROW \uXXXX, Matches a character  with hexadecimal value of XXXX. )
    $(REG_ROW \U00YYYYYY, Matches a character with hexadecimal value of YYYYYY. )
    $(REG_ROW \f, Matches a formfeed character. )
    $(REG_ROW \n, Matches a linefeed character. )
    $(REG_ROW \r, Matches a carriage return character. )
    $(REG_ROW \t, Matches a tab character. )
    $(REG_ROW \v, Matches a vertical tab character. )
    $(REG_ROW \d, Matches any Unicode digit. )
    $(REG_ROW \D, Matches any character except Unicode digits. )
    $(REG_ROW \w, Matches any word character (note: this includes numbers).)
    $(REG_ROW \W, Matches any non-word character.)
    $(REG_ROW \s, Matches whitespace, same as \p{White_Space}.)
    $(REG_ROW \S, Matches any character except those recognized as $(I \s ). )
    $(REG_ROW \\\\, Matches \ character. )
    $(REG_ROW \c where c is one of [|*+?(), Matches the character c itself. )
    $(REG_ROW \p{PropertyName}, Matches a character that belongs
        to the Unicode PropertyName set.
      Single letter abbreviations can be used without surrounding {,}. )
    $(REG_ROW  \P{PropertyName}, Matches a character that does not belong
        to the Unicode PropertyName set.
      Single letter abbreviations can be used without surrounding {,}. )
    $(REG_ROW \p{InBasicLatin}, Matches any character that is part of
          the BasicLatin Unicode $(U block).)
    $(REG_ROW \P{InBasicLatin}, Matches any character except ones in
          the BasicLatin Unicode $(U block).)
    $(REG_ROW \p{Cyrillic}, Matches any character that is part of
        Cyrillic $(U script).)
    $(REG_ROW \P{Cyrillic}, Matches any character except ones in
        Cyrillic $(U script).)
    $(REG_TITLE Quantifiers, Specify repetition of other elements)
    $(REG_ROW *, Matches previous character/subexpression 0 or more times.
      Greedy version - tries as many times as possible.)
    $(REG_ROW *?, Matches previous character/subexpression 0 or more times.
      Lazy version  - stops as early as possible.)
    $(REG_ROW +, Matches previous character/subexpression 1 or more times.
      Greedy version - tries as many times as possible.)
    $(REG_ROW +?, Matches previous character/subexpression 1 or more times.
      Lazy version  - stops as early as possible.)
    $(REG_ROW ?, Matches previous character/subexpression 0 or 1 time.
      Greedy version - tries as many times as possible.)
    $(REG_ROW ??, Matches previous character/subexpression 0 or 1 time.
      Lazy version  - stops as early as possible.)
    $(REG_ROW {n}, Matches previous character/subexpression exactly n times. )
    $(REG_ROW {n$(COMMA)}, Matches previous character/subexpression n times or more.
      Greedy version - tries as many times as possible. )
    $(REG_ROW {n$(COMMA)}?, Matches previous character/subexpression n times or more.
      Lazy version - stops as early as possible.)
    $(REG_ROW {n$(COMMA)m}, Matches previous character/subexpression n to m times.
      Greedy version - tries as many times as possible, but no more than m times. )
    $(REG_ROW {n$(COMMA)m}?, Matches previous character/subexpression n to m times.
      Lazy version - stops as early as possible, but no less then n times.)
    $(REG_TITLE Other, Subexpressions $(AMP) alternations )
    $(REG_ROW (regex),  Matches subexpression regex,
      saving matched portion of text for later retrieval. )
    $(REG_ROW (?#comment), An inline comment that is ignored while matching.)
    $(REG_ROW (?:regex), Matches subexpression regex,
      $(U not) saving matched portion of text. Useful to speed up matching. )
    $(REG_ROW A|B, Matches subexpression A, or failing that, matches B. )
    $(REG_ROW (?P$(LT)name$(GT)regex), Matches named subexpression
        regex labeling it with name 'name'.
        When referring to a matched portion of text,
        names work like aliases in addition to direct numbers.
     )
    $(REG_TITLE Assertions, Match position rather than character )
    $(REG_ROW ^, Matches at the beginning of input or line (in multiline mode).)
    $(REG_ROW $, Matches at the end of input or line (in multiline mode). )
    $(REG_ROW \b, Matches at word boundary. )
    $(REG_ROW \B, Matches when $(U not) at word boundary. )
    $(REG_ROW (?=regex), Zero-width lookahead assertion.
        Matches at a point where the subexpression
        regex could be matched starting from the current position.
      )
    $(REG_ROW (?!regex), Zero-width negative lookahead assertion.
        Matches at a point where the subexpression
        regex could $(U not) be matched starting from the current position.
      )
    $(REG_ROW (?<=regex), Zero-width lookbehind assertion. Matches at a point
        where the subexpression regex could be matched ending
        at the current position (matching goes backwards).
      )
    $(REG_ROW  (?<!regex), Zero-width negative lookbehind assertion.
      Matches at a point where the subexpression regex could $(U not)
      be matched ending at the current position (matching goes backwards).
     )
  )

  $(REG_START Character classes )
  $(REG_TABLE
    $(REG_TITLE Pattern element, Semantics )
    $(REG_ROW Any atom, Has the same meaning as outside of a character class,
      except for ] which must be written as \\])
    $(REG_ROW a-z, Includes characters a, b, c, ..., z. )
    $(REG_ROW [a||b]$(COMMA) [a--b]$(COMMA) [a~~b]$(COMMA) [a$(AMP)$(AMP)b],
     Where a, b are arbitrary classes, means union, set difference,
     symmetric set difference, and intersection respectively.
     $(I Any sequence of character class elements implicitly forms a union.) )
  )

  $(REG_START Regex flags )
  $(REG_TABLE
    $(REG_TITLE Flag, Semantics )
    $(REG_ROW g, Global regex, repeat over the whole input. )
    $(REG_ROW i, Case insensitive matching. )
    $(REG_ROW m, Multi-line mode, match ^, $ on start and end line separators
       as well as start and end of input.)
    $(REG_ROW s, Single-line mode, makes . match '\n' and '\r' as well. )
    $(REG_ROW x, Free-form syntax, ignores whitespace in pattern,
      useful for formatting complex regular expressions. )
  )

  $(SECTION Unicode support)

  This library provides full Level 1 support* according to
    $(HTTP unicode.org/reports/tr18/, UTS 18). Specifically:
  $(UL
    $(LI 1.1 Hex notation via any of \uxxxx, \U00YYYYYY, \xZZ.)
    $(LI 1.2 Unicode properties.)
    $(LI 1.3 Character classes with set operations.)
    $(LI 1.4 Word boundaries use the full set of "word" characters.)
    $(LI 1.5 Using simple casefolding to match case
        insensitively across the full range of codepoints.)
    $(LI 1.6 Respecting line breaks as any of
        \u000A | \u000B | \u000C | \u000D | \u0085 | \u2028 | \u2029 | \u000D\u000A.)
    $(LI 1.7 Operating on codepoint level.)
  )
  *With exception of point 1.1.1, as of yet, normalization of input
    is expected to be enforced by user.

    $(SECTION Replace format string)

    A set of functions in this module that do the substitution rely
    on a simple format to guide the process. In particular the table below
    applies to the `format` argument of
    $(LREF replaceFirst) and $(LREF replaceAll).

    The format string can reference parts of match using the following notation.
    $(REG_TABLE
        $(REG_TITLE Format specifier, Replaced by )
        $(REG_ROW $(DOLLAR)$(AMP), the whole match. )
        $(REG_ROW $(DOLLAR)$(BACKTICK), part of input $(I preceding) the match. )
        $(REG_ROW $', part of input $(I following) the match. )
        $(REG_ROW $$, '$' character. )
        $(REG_ROW \c $(COMMA) where c is any character, the character c itself. )
        $(REG_ROW \\\\, '\\' character. )
        $(REG_ROW $(DOLLAR)1 .. $(DOLLAR)99, submatch number 1 to 99 respectively. )
    )

  $(SECTION Slicing and zero memory allocations orientation)

  All matches returned by pattern matching functionality in this library
    are slices of the original input. The notable exception is the `replace`
    family of functions  that generate a new string from the input.

    In cases where producing the replacement is the ultimate goal
    $(LREF replaceFirstInto) and $(LREF replaceAllInto) could come in handy
    as functions that  avoid allocations even for replacement.

    Copyright: Copyright Dmitry Olshansky, 2011-

  License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

  Authors: Dmitry Olshansky,

    API and utility constructs are modeled after the original `std.regex`
  by Walter Bright and Andrei Alexandrescu.

  Source: $(PHOBOSSRC std/regex/package.d)

Macros:
    REG_ROW = $(TR $(TD $(I $1 )) $(TD $+) )
    REG_TITLE = $(TR $(TD $(B $1)) $(TD $(B $2)) )
    REG_TABLE = <table border="1" cellspacing="0" cellpadding="5" > $0 </table>
    REG_START = <h3><div align="center"> $0 </div></h3>
    SECTION = <h3><a id="$1" href="#$1" class="anchor">$0</a></h3>
    S_LINK = <a href="#$1">$+</a>
 +/
module std.regex;

import std.range.primitives, std.traits;
import std.regex.internal.ir;
import std.typecons : Flag, Yes, No;

/++
    `Regex` object holds regular expression pattern in compiled form.

    Instances of this object are constructed via calls to `regex`.
    This is an intended form for caching and storage of frequently
    used regular expressions.

    Example:

    Test if this object doesn't contain any compiled pattern.
    ---
    Regex!char r;
    assert(r.empty);
    r = regex(""); // Note: "" is a valid regex pattern.
    assert(!r.empty);
    ---

    Getting a range of all the named captures in the regex.
    ----
    import std.range;
    import std.algorithm;

    auto re = regex(`(?P<name>\w+) = (?P<var>\d+)`);
    auto nc = re.namedCaptures;
    static assert(isRandomAccessRange!(typeof(nc)));
    assert(!nc.empty);
    assert(nc.length == 2);
    assert(nc.equal(["name", "var"]));
    assert(nc[0] == "name");
    assert(nc[1..$].equal(["var"]));
    ----
+/
public alias Regex(Char) = std.regex.internal.ir.Regex!(Char);

/++
    A `StaticRegex` is `Regex` object that contains D code specially
    generated at compile-time to speed up matching.

    No longer used, kept as alias to Regex for backwards compatibility.
+/
public alias StaticRegex = Regex;

/++
    Compile regular expression pattern for the later execution.
    Returns: `Regex` object that works on inputs having
    the same character width as `pattern`.

    Params:
    pattern = A single regular expression to match.
    patterns = An array of regular expression strings.
        The resulting `Regex` object will match any expression;
        use $(LREF whichPattern) to know which.
    flags = The _attributes (g, i, m, s and x accepted)

    Throws: `RegexException` if there were any errors during compilation.
+/
@trusted public auto regex(S : C[], C)(const S[] patterns, const(char)[] flags="")
if (isSomeString!(S))
{
    import std.array : appender;
    import std.functional : memoize;
    enum cacheSize = 8; //TODO: invent nice interface to control regex caching
    const(C)[] pat;
    if (patterns.length > 1)
    {
        auto app = appender!S();
        foreach (i, p; patterns)
        {
            if (i != 0)
                app.put("|");
            app.put("(?:");
            app.put(patterns[i]);
            // terminator for the pattern
            // to detect if the pattern unexpectedly ends
            app.put("\\");
            app.put(cast(dchar)(privateUseStart+i));
            app.put(")");
            // another one to return correct whichPattern
            // for all of potential alternatives in the patterns[i]
            app.put("\\");
            app.put(cast(dchar)(privateUseStart+i));
        }
        pat = app.data;
    }
    else
        pat = patterns[0];

    if (__ctfe)
        return regexImpl(pat, flags);
    return memoize!(regexImpl!S, cacheSize)(pat, flags);
}

///ditto
@trusted public auto regex(S)(S pattern, const(char)[] flags="")
if (isSomeString!(S))
{
    return regex([pattern], flags);
}

///
@system unittest
{
    void test(S)()
    {
        // multi-pattern regex example
        S[] arr = [`([a-z]+):(\d+)`, `(\d+),\d+`];
        auto multi = regex(arr); // multi regex
        S str = "abc:43 12,34";
        auto m = str.matchAll(multi);
        assert(m.front.whichPattern == 1);
        assert(m.front[1] == "abc");
        assert(m.front[2] == "43");
        m.popFront();
        assert(m.front.whichPattern == 2);
        assert(m.front[1] == "12");
    }

    import std.meta : AliasSeq;
    static foreach (C; AliasSeq!(string, wstring, dstring))
        // Test with const array of patterns - see https://issues.dlang.org/show_bug.cgi?id=20301
        static foreach (S; AliasSeq!(C, const C, immutable C))
            test!S();
}

@system unittest
{
    import std.conv : to;
    import std.string : indexOf;

    immutable pattern = "s+";
    auto regexString = to!string(regex(pattern, "U"));
    assert(regexString.length <= pattern.length + 100, "String representation shouldn't be unreasonably bloated.");
    assert(indexOf(regexString, "s+") >= 0, "String representation should include pattern.");
    assert(indexOf(regexString, 'U') >= 0, "String representation should include flags.");
}

public auto regexImpl(S)(const S pattern, const(char)[] flags="")
if (isSomeString!(typeof(pattern)))
{
    import std.regex.internal.parser : Parser, CodeGen;
    auto parser = Parser!(Unqual!(typeof(pattern)), CodeGen)(pattern, flags);
    auto r = parser.program;
    return r;
}


private struct CTRegexWrapper(Char)
{
    private immutable(Regex!Char)* re;

    // allow code that expects mutable Regex to still work
    // we stay "logically const"
    @property @trusted ref getRe() const { return *cast(Regex!Char*) re; }
    alias getRe this;
}

template ctRegexImpl(alias pattern, string flags="")
{
    import std.regex.internal.backtracking, std.regex.internal.parser;
    static immutable r = cast(immutable) regex(pattern, flags);
    alias Char = BasicElementOf!(typeof(pattern));
    enum source = ctGenRegExCode(r);
    @trusted pure bool func(BacktrackingMatcher!Char matcher)
    {
        debug(std_regex_ctr) pragma(msg, source);
        cast(void) matcher;
        mixin(source);
    }
    static immutable staticRe =
        cast(immutable) r.withFactory(new CtfeFactory!(BacktrackingMatcher, Char, func));
    enum wrapper = CTRegexWrapper!Char(&staticRe);
}

@safe pure unittest
{
    // test compat for logical const workaround
    static void test(StaticRegex!char)
    {
    }
    enum re = ctRegex!``;
    test(re);
}

@safe pure unittest
{
    auto re = ctRegex!`foo`;
    assert(matchFirst("foo", re));

    // test reassignment
    re = ctRegex!`bar`;
    assert(matchFirst("bar", re));
    assert(!matchFirst("bar", ctRegex!`foo`));
}

/++
    Compile regular expression using CTFE
    and generate optimized native machine code for matching it.

    Returns: StaticRegex object for faster matching.

    Params:
    pattern = Regular expression
    flags = The _attributes (g, i, m, s and x accepted)
+/
public enum ctRegex(alias pattern, string flags="") = ctRegexImpl!(pattern, flags).wrapper;

enum isRegexFor(RegEx, R) = is(immutable RegEx == immutable Regex!(BasicElementOf!R))
     || is(RegEx : const(Regex!(BasicElementOf!R)))
     || is(immutable RegEx == immutable StaticRegex!(BasicElementOf!R));


/++
    `Captures` object contains submatches captured during a call
    to `match` or iteration over `RegexMatch` range.

    First element of range is the whole match.
+/
@trusted public struct Captures(R)
if (isSomeString!R)
{//@trusted because of union inside
    alias DataIndex = size_t;
    alias String = R;
    alias Store = SmallFixedArray!(Group!DataIndex, 3);
private:
    import std.conv : text;
    Store matches;
    const(NamedGroup)[] _names;
    R _input;
    int _nMatch;
    uint _f, _b;

    this(R input, uint n, const(NamedGroup)[] named)
    {
        _input = input;
        _names = named;
        matches = Store(n);
        _b = n;
        _f = 0;
    }

    this(ref RegexMatch!R rmatch)
    {
        _input = rmatch._input;
        _names = rmatch._engine.pattern.dict;
        immutable n = rmatch._engine.pattern.ngroup;
        matches = Store(n);
        _b = n;
        _f = 0;
    }

    inout(R) getMatch(size_t index) inout
    {
        auto m = &matches[index];
        return *m ? _input[m.begin .. m.end] : null;
    }

public:
    ///Slice of input prior to the match.
    @property R pre()
    {
        return _nMatch == 0 ? _input[] : _input[0 .. matches[0].begin];
    }

    ///Slice of input immediately after the match.
    @property R post()
    {
        return _nMatch == 0 ? _input[] : _input[matches[0].end .. $];
    }

    ///Slice of matched portion of input.
    @property R hit()
    {
        assert(_nMatch, "attempted to get hit of an empty match");
        return _input[matches[0].begin .. matches[0].end];
    }

    ///Range interface.
    @property R front()
    {
        assert(_nMatch, "attempted to get front of an empty match");
        return getMatch(_f);
    }

    ///ditto
    @property R back()
    {
        assert(_nMatch, "attempted to get back of an empty match");
        return getMatch(_b - 1);
    }

    ///ditto
    void popFront()
    {
        assert(!empty);
        ++_f;
    }

    ///ditto
    void popBack()
    {
        assert(!empty);
        --_b;
    }

    ///ditto
    @property bool empty() const { return _nMatch == 0 || _f >= _b; }

    ///ditto
    inout(R) opIndex()(size_t i) inout
    {
        assert(_f + i < _b,text("requested submatch number ", i," is out of range"));
        return getMatch(_f + i);
    }

    /++
        Explicit cast to bool.
        Useful as a shorthand for !(x.empty) in if and assert statements.

        ---
        import std.regex;

        assert(!matchFirst("nothing", "something"));
        ---
    +/

    @safe bool opCast(T:bool)() const nothrow { return _nMatch != 0; }

    /++
        Number of pattern matched counting, where 1 - the first pattern.
        Returns 0 on no match.
    +/

    @safe @property int whichPattern() const nothrow { return _nMatch; }

    ///
    @system unittest
    {
        import std.regex;
        assert(matchFirst("abc", "[0-9]+", "[a-z]+").whichPattern == 2);
    }

    /++
        Lookup named submatch.

        ---
        import std.regex;
        import std.range;

        auto c = matchFirst("a = 42;", regex(`(?P<var>\w+)\s*=\s*(?P<value>\d+);`));
        assert(c["var"] == "a");
        assert(c["value"] == "42");
        popFrontN(c, 2);
        //named groups are unaffected by range primitives
        assert(c["var"] =="a");
        assert(c.front == "42");
        ----
    +/
    R opIndex(String)(String i) /*const*/ //@@@BUG@@@
        if (isSomeString!String)
    {
        size_t index = lookupNamedGroup(_names, i);
        return getMatch(index);
    }

    ///Number of matches in this object.
    @property size_t length() const { return _nMatch == 0 ? 0 : _b - _f;  }

    ///A hook for compatibility with original std.regex.
    @property ref captures(){ return this; }
}

///
@system unittest
{
    import std.range.primitives : popFrontN;

    auto c = matchFirst("@abc#", regex(`(\w)(\w)(\w)`));
    assert(c.pre == "@"); // Part of input preceding match
    assert(c.post == "#"); // Immediately after match
    assert(c.hit == c[0] && c.hit == "abc"); // The whole match
    assert(c[2] == "b");
    assert(c.front == "abc");
    c.popFront();
    assert(c.front == "a");
    assert(c.back == "c");
    c.popBack();
    assert(c.back == "b");
    popFrontN(c, 2);
    assert(c.empty);

    assert(!matchFirst("nothing", "something"));

    // Captures that are not matched will be null.
    c = matchFirst("ac", regex(`a(b)?c`));
    assert(c);
    assert(!c[1]);
}

@system unittest
{
    Captures!string c;
    string s = "abc";
    assert(cast(bool)(c = matchFirst(s, regex("d")))
        || cast(bool)(c = matchFirst(s, regex("a"))));
}

// https://issues.dlang.org/show_bug.cgi?id=19979
@system unittest
{
    auto c = matchFirst("bad", regex(`(^)(not )?bad($)`));
    assert(c[0] && c[0].length == "bad".length);
    assert(c[1] && !c[1].length);
    assert(!c[2]);
    assert(c[3] && !c[3].length);
}

/++
    A regex engine state, as returned by `match` family of functions.

    Effectively it's a forward range of Captures!R, produced
    by lazily searching for matches in a given input.
+/
@trusted public struct RegexMatch(R)
if (isSomeString!R)
{
    import std.typecons : Rebindable;
private:
    alias Char = BasicElementOf!R;
    Matcher!Char _engine;
    Rebindable!(const MatcherFactory!Char) _factory;
    R _input;
    Captures!R _captures;

    this(RegEx)(R input, RegEx prog)
    {
        import std.exception : enforce;
        _input = input;
        if (prog.factory is null) _factory = defaultFactory!Char(prog);
        else _factory = prog.factory;
        _engine = _factory.create(prog, input);
        assert(_engine.refCount == 1);
        _captures = Captures!R(this);
        _captures.matches.mutate((slice) pure { _captures._nMatch = _engine.match(slice); });
    }

public:
    this(this)
    {
        if (_engine) _factory.incRef(_engine);
    }

    ~this()
    {
        if (_engine) _factory.decRef(_engine);
    }

    ///Shorthands for front.pre, front.post, front.hit.
    @property R pre()
    {
        return _captures.pre;
    }

    ///ditto
    @property R post()
    {
        return _captures.post;
    }

    ///ditto
    @property R hit()
    {
        return _captures.hit;
    }

    /++
        Functionality for processing subsequent matches of global regexes via range interface:
        ---
        import std.regex;
        auto m = matchAll("Hello, world!", regex(`\w+`));
        assert(m.front.hit == "Hello");
        m.popFront();
        assert(m.front.hit == "world");
        m.popFront();
        assert(m.empty);
        ---
    +/
    @property inout(Captures!R) front() inout
    {
        return _captures;
    }

    ///ditto
    void popFront()
    {
        import std.exception : enforce;
        // CoW - if refCount is not 1, we are aliased by somebody else
        if (_engine.refCount != 1)
        {
            // we create a new engine & abandon this reference
            auto old = _engine;
            _engine = _factory.dup(old, _input);
            _factory.decRef(old);
        }
        _captures.matches.mutate((slice) { _captures._nMatch = _engine.match(slice); });
    }

    ///ditto
    auto save(){ return this; }

    ///Test if this match object is empty.
    @property bool empty() const { return _captures._nMatch == 0; }

    ///Same as !(x.empty), provided for its convenience  in conditional statements.
    T opCast(T:bool)(){ return !empty; }

    /// Same as .front, provided for compatibility with original std.regex.
    @property inout(Captures!R) captures() inout { return _captures; }
}

private auto matchOnceImpl(RegEx, R)(R input, const auto ref RegEx prog) @trusted
{
    alias Char = BasicElementOf!R;
    static struct Key
    {
        immutable(Char)[] pattern;
        uint flags;
    }
    static Key cacheKey = Key("", -1);
    static Matcher!Char cache;
    auto factory = prog.factory is null ? defaultFactory!Char(prog) : prog.factory;
    auto key = Key(prog.pattern, prog.flags);
    Matcher!Char engine;
    if (cacheKey == key)
    {
        engine = cache;
        engine.rearm(input);
    }
    else
    {
        engine = factory.create(prog, input);
        if (cache) factory.decRef(cache); // destroy cached engine *after* building a new one
        cache = engine;
        cacheKey = key;
    }
    auto captures = Captures!R(input, prog.ngroup, prog.dict);
    captures.matches.mutate((slice) pure { captures._nMatch = engine.match(slice); });
    return captures;
}

// matchOnce is constructed as a safe, pure wrapper over matchOnceImpl. It can be
// faked as pure because the static mutable variables are used to cache the key and
// character matcher. The technique used avoids delegates and GC.
private @safe auto matchOnce(RegEx, R)(R input, const auto ref RegEx prog) pure
{
    static auto impl(R input, const ref RegEx prog)
    {
        return matchOnceImpl(input, prog);
    }

    static @trusted auto pureImpl(R input, const ref RegEx prog)
    {
        auto p = assumePureFunction(&impl);
        return p(input, prog);
    }

    return pureImpl(input, prog);
}

private auto matchMany(RegEx, R)(R input, auto ref RegEx re) @safe
{
    return RegexMatch!R(input, re.withFlags(re.flags | RegexOption.global));
}

@system unittest
{
    //sanity checks for new API
    auto re = regex("abc");
    assert(!"abc".matchOnce(re).empty);
    assert("abc".matchOnce(re)[0] == "abc");
}

// https://issues.dlang.org/show_bug.cgi?id=18135
@system unittest
{
    static struct MapResult { RegexMatch!string m; }
    MapResult m;
    m = MapResult();
    assert(m == m);
}

private enum isReplaceFunctor(alias fun, R) =
    __traits(compiles, (Captures!R c) { fun(c); });

// the lowest level - just stuff replacements into the sink
private @trusted void replaceCapturesInto(alias output, Sink, R, T)
        (ref Sink sink, R input, T captures)
if (isOutputRange!(Sink, dchar) && isSomeString!R)
{
    if (captures.empty)
    {
        sink.put(input);
        return;
    }
    sink.put(captures.pre);
    // a hack to get around bogus errors, should be simply output(captures, sink)
    // "is a nested function and cannot be accessed from"
    static if (isReplaceFunctor!(output, R))
        sink.put(output(captures)); //"mutator" type of function
    else
        output(captures, sink); //"output" type of function
    sink.put(captures.post);
}

// ditto for a range of captures
private void replaceMatchesInto(alias output, Sink, R, T)
        (ref Sink sink, R input, T matches)
if (isOutputRange!(Sink, dchar) && isSomeString!R)
{
    size_t offset = 0;
    foreach (cap; matches)
    {
        sink.put(cap.pre[offset .. $]);
        // same hack, see replaceCapturesInto
        static if (isReplaceFunctor!(output, R))
            sink.put(output(cap)); //"mutator" type of function
        else
            output(cap, sink); //"output" type of function
        offset = cap.pre.length + cap.hit.length;
    }
    sink.put(input[offset .. $]);
}

//  a general skeleton of replaceFirst
private R replaceFirstWith(alias output, R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    import std.array : appender;
    auto data = matchFirst(input, re);
    if (data.empty)
        return input;
    auto app = appender!(R)();
    replaceCapturesInto!output(app, input, data);
    return app.data;
}

// ditto for replaceAll
// the method parameter allows old API to ride on the back of the new one
private R replaceAllWith(alias output,
        alias method=matchAll, R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    import std.array : appender;
    auto matches = method(input, re); //inout(C)[] fails
    if (matches.empty)
        return input;
    auto app = appender!(R)();
    replaceMatchesInto!output(app, input, matches);
    return app.data;
}


/++
    Start matching `input` to regex pattern `re`,
    using Thompson NFA matching scheme.

    The use of this function is $(RED discouraged) - use either of
    $(LREF matchAll) or $(LREF matchFirst).

    Delegating  the kind of operation
    to "g" flag is soon to be phased out along with the
    ability to choose the exact matching scheme. The choice of
    matching scheme to use depends highly on the pattern kind and
    can done automatically on case by case basis.

    Returns: a `RegexMatch` object holding engine state after first match.
+/

public auto match(R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx,R))
{
    return RegexMatch!(Unqual!(typeof(input)))(input, re);
}

///ditto
public auto match(R, String)(R input, String re)
if (isSomeString!R && isSomeString!String)
{
    return RegexMatch!(Unqual!(typeof(input)))(input, regex(re));
}

/++
    Find the first (leftmost) slice of the `input` that
    matches the pattern `re`. This function picks the most suitable
    regular expression engine depending on the pattern properties.

    `re` parameter can be one of three types:
    $(UL
      $(LI Plain string(s), in which case it's compiled to bytecode before matching. )
      $(LI Regex!char (wchar/dchar) that contains a pattern in the form of
        compiled  bytecode. )
      $(LI StaticRegex!char (wchar/dchar) that contains a pattern in the form of
        compiled native machine code. )
    )

    Returns:
    $(LREF Captures) containing the extent of a match together with all submatches
    if there was a match, otherwise an empty $(LREF Captures) object.
+/
public auto matchFirst(R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return matchOnce(input, re);
}

///ditto
public auto matchFirst(R, String)(R input, String re)
if (isSomeString!R && isSomeString!String)
{
    return matchOnce(input, regex(re));
}

///ditto
public auto matchFirst(R, String)(R input, String[] re...)
if (isSomeString!R && isSomeString!String)
{
    return matchOnce(input, regex(re));
}

/++
    Initiate a search for all non-overlapping matches to the pattern `re`
    in the given `input`. The result is a lazy range of matches generated
    as they are encountered in the input going left to right.

    This function picks the most suitable regular expression engine
    depending on the pattern properties.

    `re` parameter can be one of three types:
    $(UL
      $(LI Plain string(s), in which case it's compiled to bytecode before matching. )
      $(LI Regex!char (wchar/dchar) that contains a pattern in the form of
        compiled  bytecode. )
      $(LI StaticRegex!char (wchar/dchar) that contains a pattern in the form of
        compiled native machine code. )
    )

    Returns:
    $(LREF RegexMatch) object that represents matcher state
    after the first match was found or an empty one if not present.
+/
public auto matchAll(R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return matchMany(input, re);
}

///ditto
public auto matchAll(R, String)(R input, String re)
if (isSomeString!R && isSomeString!String)
{
    return matchMany(input, regex(re));
}

///ditto
public auto matchAll(R, String)(R input, String[] re...)
if (isSomeString!R && isSomeString!String)
{
    return matchMany(input, regex(re));
}

// another set of tests just to cover the new API
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.conv : to;

    static foreach (String; AliasSeq!(string, wstring, const(dchar)[]))
    {{
        auto str1 = "blah-bleh".to!String();
        auto pat1 = "bl[ae]h".to!String();
        auto mf = matchFirst(str1, pat1);
        assert(mf.equal(["blah".to!String()]));
        auto mAll = matchAll(str1, pat1);
        assert(mAll.equal!((a,b) => a.equal(b))
            ([["blah".to!String()], ["bleh".to!String()]]));

        auto str2 = "1/03/12 - 3/03/12".to!String();
        auto pat2 = regex([r"(\d+)/(\d+)/(\d+)".to!String(), "abc".to!String]);
        auto mf2 = matchFirst(str2, pat2);
        assert(mf2.equal(["1/03/12", "1", "03", "12"].map!(to!String)()));
        auto mAll2 = matchAll(str2, pat2);
        assert(mAll2.front.equal(mf2));
        mAll2.popFront();
        assert(mAll2.front.equal(["3/03/12", "3", "03", "12"].map!(to!String)()));
        mf2.popFrontN(3);
        assert(mf2.equal(["12".to!String()]));

        auto ctPat = ctRegex!(`(?P<Quot>\d+)/(?P<Denom>\d+)`.to!String());
        auto str = "2 + 34/56 - 6/1".to!String();
        auto cmf = matchFirst(str, ctPat);
        assert(cmf.equal(["34/56", "34", "56"].map!(to!String)()));
        assert(cmf["Quot"] == "34".to!String());
        assert(cmf["Denom"] == "56".to!String());

        auto cmAll = matchAll(str, ctPat);
        assert(cmAll.front.equal(cmf));
        cmAll.popFront();
        assert(cmAll.front.equal(["6/1", "6", "1"].map!(to!String)()));
    }}
}

/++
    Start matching of `input` to regex pattern `re`,
    using traditional $(LINK2 https://en.wikipedia.org/wiki/Backtracking,
    backtracking) matching scheme.

    The use of this function is $(RED discouraged) - use either of
    $(LREF matchAll) or $(LREF matchFirst).

    Delegating  the kind of operation
    to "g" flag is soon to be phased out along with the
    ability to choose the exact matching scheme. The choice of
    matching scheme to use depends highly on the pattern kind and
    can done automatically on case by case basis.

    Returns: a `RegexMatch` object holding engine
    state after first match.

+/
public auto bmatch(R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return RegexMatch!(Unqual!(typeof(input)))(input, re);
}

///ditto
public auto bmatch(R, String)(R input, String re)
if (isSomeString!R && isSomeString!String)
{
    return RegexMatch!(Unqual!(typeof(input)))(input, regex(re));
}

// produces replacement string from format using captures for substitution
package void replaceFmt(R, Capt, OutR)
    (R format, Capt captures, OutR sink, bool ignoreBadSubs = false)
if (isOutputRange!(OutR, ElementEncodingType!R[]) &&
    isOutputRange!(OutR, ElementEncodingType!(Capt.String)[]))
{
    import std.algorithm.searching : find;
    import std.ascii : isDigit, isAlpha;
    import std.conv : text, parse;
    import std.exception : enforce;
    enum State { Normal, Dollar }
    auto state = State.Normal;
    size_t offset;
L_Replace_Loop:
    while (!format.empty)
        final switch (state)
        {
        case State.Normal:
            for (offset = 0; offset < format.length; offset++)//no decoding
            {
                if (format[offset] == '$')
                {
                    state = State.Dollar;
                    sink.put(format[0 .. offset]);
                    format = format[offset+1 .. $];//ditto
                    continue L_Replace_Loop;
                }
            }
            sink.put(format[0 .. offset]);
            format = format[offset .. $];
            break;
        case State.Dollar:
            if (isDigit(format[0]))
            {
                uint digit = parse!uint(format);
                enforce(ignoreBadSubs || digit < captures.length, text("invalid submatch number ", digit));
                if (digit < captures.length)
                    sink.put(captures[digit]);
            }
            else if (format[0] == '{')
            {
                auto x = find!(a => !isAlpha(a))(format[1..$]);
                enforce(!x.empty && x[0] == '}', "no matching '}' in replacement format");
                auto name = format[1 .. $ - x.length];
                format = x[1..$];
                enforce(!name.empty, "invalid name in ${...} replacement format");
                sink.put(captures[name]);
            }
            else if (format[0] == '&')
            {
                sink.put(captures[0]);
                format = format[1 .. $];
            }
            else if (format[0] == '`')
            {
                sink.put(captures.pre);
                format = format[1 .. $];
            }
            else if (format[0] == '\'')
            {
                sink.put(captures.post);
                format = format[1 .. $];
            }
            else if (format[0] == '$')
            {
                sink.put(format[0 .. 1]);
                format = format[1 .. $];
            }
            state = State.Normal;
            break;
        }
    enforce(state == State.Normal, "invalid format string in regex replace");
}

/++
    Construct a new string from `input` by replacing the first match with
    a string generated from it according to the `format` specifier.

    To replace all matches use $(LREF replaceAll).

    Params:
    input = string to search
    re = compiled regular expression to use
    format = _format string to generate replacements from,
    see $(S_LINK Replace _format string, the _format string).

    Returns:
    A string of the same type with the first match (if any) replaced.
    If no match is found returns the input string itself.
+/
public R replaceFirst(R, C, RegEx)(R input, RegEx re, const(C)[] format)
if (isSomeString!R && is(C : dchar) && isRegexFor!(RegEx, R))
{
    return replaceFirstWith!((m, sink) => replaceFmt(format, m, sink))(input, re);
}

///
@system unittest
{
    assert(replaceFirst("noon", regex("n"), "[$&]") == "[n]oon");
}

/++
    This is a general replacement tool that construct a new string by replacing
    matches of pattern `re` in the `input`. Unlike the other overload
    there is no format string instead captures are passed to
    to a user-defined functor `fun` that returns a new string
    to use as replacement.

    This version replaces the first match in `input`,
    see $(LREF replaceAll) to replace the all of the matches.

    Returns:
    A new string of the same type as `input` with all matches
    replaced by return values of `fun`. If no matches found
    returns the `input` itself.
+/
public R replaceFirst(alias fun, R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return replaceFirstWith!((m, sink) => sink.put(fun(m)))(input, re);
}

///
@system unittest
{
    import std.conv : to;
    string list = "#21 out of 46";
    string newList = replaceFirst!(cap => to!string(to!int(cap.hit)+1))
        (list, regex(`[0-9]+`));
    assert(newList == "#22 out of 46");
}

/++
    A variation on $(LREF replaceFirst) that instead of allocating a new string
    on each call outputs the result piece-wise to the `sink`. In particular
    this enables efficient construction of a final output incrementally.

    Like in $(LREF replaceFirst) family of functions there is an overload
    for the substitution guided by the `format` string
    and the one with the user defined callback.
+/
public @trusted void replaceFirstInto(Sink, R, C, RegEx)
        (ref Sink sink, R input, RegEx re, const(C)[] format)
if (isOutputRange!(Sink, dchar) && isSomeString!R
    && is(C : dchar) && isRegexFor!(RegEx, R))
    {
    replaceCapturesInto!((m, sink) => replaceFmt(format, m, sink))
        (sink, input, matchFirst(input, re));
    }

///ditto
public @trusted void replaceFirstInto(alias fun, Sink, R, RegEx)
    (Sink sink, R input, RegEx re)
if (isOutputRange!(Sink, dchar) && isSomeString!R && isRegexFor!(RegEx, R))
{
    replaceCapturesInto!fun(sink, input, matchFirst(input, re));
}

///
@system unittest
{
    import std.array;
    string m1 = "first message\n";
    string m2 = "second message\n";
    auto result = appender!string();
    replaceFirstInto(result, m1, regex(`([a-z]+) message`), "$1");
    //equivalent of the above with user-defined callback
    replaceFirstInto!(cap=>cap[1])(result, m2, regex(`([a-z]+) message`));
    assert(result.data == "first\nsecond\n");
}

//examples for replaceFirst
@system unittest
{
    import std.conv;
    string list = "#21 out of 46";
    string newList = replaceFirst!(cap => to!string(to!int(cap.hit)+1))
        (list, regex(`[0-9]+`));
    assert(newList == "#22 out of 46");
    import std.array;
    string m1 = "first message\n";
    string m2 = "second message\n";
    auto result = appender!string();
    replaceFirstInto(result, m1, regex(`([a-z]+) message`), "$1");
    //equivalent of the above with user-defined callback
    replaceFirstInto!(cap=>cap[1])(result, m2, regex(`([a-z]+) message`));
    assert(result.data == "first\nsecond\n");
}

/++
    Construct a new string from `input` by replacing all of the
    fragments that match a pattern `re` with a string generated
    from the match according to the `format` specifier.

    To replace only the first match use $(LREF replaceFirst).

    Params:
    input = string to search
    re = compiled regular expression to use
    format = _format string to generate replacements from,
    see $(S_LINK Replace _format string, the _format string).

    Returns:
    A string of the same type as `input` with the all
    of the matches (if any) replaced.
    If no match is found returns the input string itself.
+/
public @trusted R replaceAll(R, C, RegEx)(R input, RegEx re, const(C)[] format)
if (isSomeString!R && is(C : dchar) && isRegexFor!(RegEx, R))
{
    return replaceAllWith!((m, sink) => replaceFmt(format, m, sink))(input, re);
}

///
@system unittest
{
    // insert comma as thousands delimiter
    auto re = regex(r"(?<=\d)(?=(\d\d\d)+\b)","g");
    assert(replaceAll("12000 + 42100 = 54100", re, ",") == "12,000 + 42,100 = 54,100");
}

/++
    This is a general replacement tool that construct a new string by replacing
    matches of pattern `re` in the `input`. Unlike the other overload
    there is no format string instead captures are passed to
    to a user-defined functor `fun` that returns a new string
    to use as replacement.

    This version replaces all of the matches found in `input`,
    see $(LREF replaceFirst) to replace the first match only.

    Returns:
    A new string of the same type as `input` with all matches
    replaced by return values of `fun`. If no matches found
    returns the `input` itself.

    Params:
    input = string to search
    re = compiled regular expression
    fun = delegate to use
+/
public @trusted R replaceAll(alias fun, R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return replaceAllWith!((m, sink) => sink.put(fun(m)))(input, re);
}

///
@system unittest
{
    string baz(Captures!(string) m)
    {
        import std.string : toUpper;
        return toUpper(m.hit);
    }
    // Capitalize the letters 'a' and 'r':
    auto s = replaceAll!(baz)("Strap a rocket engine on a chicken.",
            regex("[ar]"));
    assert(s == "StRAp A Rocket engine on A chicken.");
}

/++
    A variation on $(LREF replaceAll) that instead of allocating a new string
    on each call outputs the result piece-wise to the `sink`. In particular
    this enables efficient construction of a final output incrementally.

    As with $(LREF replaceAll) there are 2 overloads - one with a format string,
    the other one with a user defined functor.
+/
public @trusted void replaceAllInto(Sink, R, C, RegEx)
        (Sink sink, R input, RegEx re, const(C)[] format)
if (isOutputRange!(Sink, dchar) && isSomeString!R
    && is(C : dchar) && isRegexFor!(RegEx, R))
    {
    replaceMatchesInto!((m, sink) => replaceFmt(format, m, sink))
        (sink, input, matchAll(input, re));
    }

///ditto
public @trusted void replaceAllInto(alias fun, Sink, R, RegEx)
        (Sink sink, R input, RegEx re)
if (isOutputRange!(Sink, dchar) && isSomeString!R && isRegexFor!(RegEx, R))
{
    replaceMatchesInto!fun(sink, input, matchAll(input, re));
}

///
@system unittest
{
    // insert comma as thousands delimiter in fifty randomly produced big numbers
    import std.array, std.conv, std.random, std.range;
    static re = regex(`(?<=\d)(?=(\d\d\d)+\b)`, "g");
    auto sink = appender!(char [])();
    enum ulong min = 10UL ^^ 10, max = 10UL ^^ 19;
    foreach (i; 0 .. 50)
    {
        sink.clear();
        replaceAllInto(sink, text(uniform(min, max)), re, ",");
        foreach (pos; iota(sink.data.length - 4, 0, -4))
            assert(sink.data[pos] == ',');
    }
}

// exercise all of the replace APIs
@system unittest
{
    import std.array : appender;
    import std.conv;
    // try and check first/all simple substitution
    static foreach (S; AliasSeq!(string, wstring, dstring, char[], wchar[], dchar[]))
    {{
        S s1 = "curt trial".to!S();
        S s2 = "round dome".to!S();
        S t1F = "court trial".to!S();
        S t2F = "hound dome".to!S();
        S t1A = "court trial".to!S();
        S t2A = "hound home".to!S();
        auto re1 = regex("curt".to!S());
        auto re2 = regex("[dr]o".to!S());

        assert(replaceFirst(s1, re1, "court") == t1F);
        assert(replaceFirst(s2, re2, "ho") == t2F);
        assert(replaceAll(s1, re1, "court") == t1A);
        assert(replaceAll(s2, re2, "ho") == t2A);

        auto rep1 = replaceFirst!(cap => cap[0][0]~"o".to!S()~cap[0][1..$])(s1, re1);
        assert(rep1 == t1F);
        assert(replaceFirst!(cap => "ho".to!S())(s2, re2) == t2F);
        auto rep1A = replaceAll!(cap => cap[0][0]~"o".to!S()~cap[0][1..$])(s1, re1);
        assert(rep1A == t1A);
        assert(replaceAll!(cap => "ho".to!S())(s2, re2) == t2A);

        auto sink = appender!S();
        replaceFirstInto(sink, s1, re1, "court");
        assert(sink.data == t1F);
        replaceFirstInto(sink, s2, re2, "ho");
        assert(sink.data == t1F~t2F);
        replaceAllInto(sink, s1, re1, "court");
        assert(sink.data == t1F~t2F~t1A);
        replaceAllInto(sink, s2, re2, "ho");
        assert(sink.data == t1F~t2F~t1A~t2A);
    }}
}

/++
    Old API for replacement, operation depends on flags of pattern `re`.
    With "g" flag it performs the equivalent of $(LREF replaceAll) otherwise it
    works the same as $(LREF replaceFirst).

    The use of this function is $(RED discouraged), please use $(LREF replaceAll)
    or $(LREF replaceFirst) explicitly.
+/
public R replace(alias scheme = match, R, C, RegEx)(R input, RegEx re, const(C)[] format)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return replaceAllWith!((m, sink) => replaceFmt(format, m, sink), match)(input, re);
}

///ditto
public R replace(alias fun, R, RegEx)(R input, RegEx re)
if (isSomeString!R && isRegexFor!(RegEx, R))
{
    return replaceAllWith!(fun, match)(input, re);
}

/**
Splits a string `r` using a regular expression `pat` as a separator.

Params:
    keepSeparators = flag to specify if the matches should be in the resulting range
    r = the string to split
    pat = the pattern to split on
Returns:
    A lazy range of strings
*/
public struct Splitter(Flag!"keepSeparators" keepSeparators = No.keepSeparators, Range, alias RegEx = Regex)
if (isSomeString!Range && isRegexFor!(RegEx, Range))
{
private:
    Range _input;
    size_t _offset;
    alias Rx = typeof(match(Range.init,RegEx.init));
    Rx _match;

    static if (keepSeparators) bool onMatch = false;

    @trusted this(Range input, RegEx separator)
    {//@@@BUG@@@ generated opAssign of RegexMatch is not @trusted
        _input = input;
        const re = separator.withFlags(separator.flags | RegexOption.global);
        if (_input.empty)
        {
            //there is nothing to match at all, make _offset > 0
            _offset = 1;
        }
        else
        {
            _match = Rx(_input, re);

            static if (keepSeparators)
                if (_match.pre.empty)
                    popFront();
        }
    }

public:
    auto ref opSlice()
    {
        return this.save;
    }

    ///Forward range primitives.
    @property Range front()
    {
        import std.algorithm.comparison : min;

        assert(!empty && _offset <= _match.pre.length
                && _match.pre.length <= _input.length);

        static if (keepSeparators)
        {
            if (!onMatch)
                return _input[_offset .. min($, _match.pre.length)];
            else
                return _match.hit();
        }
        else
        {
            return _input[_offset .. min($, _match.pre.length)];
        }
    }

    ///ditto
    @property bool empty()
    {
        static if (keepSeparators)
            return _offset >= _input.length;
        else
            return _offset > _input.length;
    }

    ///ditto
    void popFront()
    {
        assert(!empty);
        if (_match.empty)
        {
            //No more separators, work is done here
            _offset = _input.length + 1;
        }
        else
        {
            static if (keepSeparators)
            {
                if (!onMatch)
                {
                    //skip past the separator
                    _offset = _match.pre.length;
                }
                else
                {
                    _offset += _match.hit.length;
                    _match.popFront();
                }

                onMatch = !onMatch;
            }
            else
            {
                //skip past the separator
                _offset = _match.pre.length + _match.hit.length;
                _match.popFront();
            }
        }
    }

    ///ditto
    @property auto save()
    {
        return this;
    }
}

/// ditto
public Splitter!(keepSeparators, Range, RegEx) splitter(
    Flag!"keepSeparators" keepSeparators = No.keepSeparators, Range, RegEx)(Range r, RegEx pat)
if (
    is(BasicElementOf!Range : dchar) && isRegexFor!(RegEx, Range))
{
    return Splitter!(keepSeparators, Range, RegEx)(r, pat);
}

///
@system unittest
{
    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,  fg, hi, ";
    assert(equal(splitter(s1, regex(", *")),
        ["", "abc", "de", "fg", "hi", ""]));
}

/// Split on a pattern, but keep the matches in the resulting range
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    auto pattern = regex(`([\.,])`);

    assert("2003.04.05"
        .splitter!(Yes.keepSeparators)(pattern)
        .equal(["2003", ".", "04", ".", "05"]));

    assert(",1,2,3"
        .splitter!(Yes.keepSeparators)(pattern)
        .equal([",", "1", ",", "2", ",", "3"]));
}

///An eager version of `splitter` that creates an array with splitted slices of `input`.
public @trusted String[] split(String, RegEx)(String input, RegEx rx)
if (isSomeString!String  && isRegexFor!(RegEx, String))
{
    import std.array : appender;
    auto a = appender!(String[])();
    foreach (e; splitter(input, rx))
        a.put(e);
    return a.data;
}

///Exception object thrown in case of errors during regex compilation.
public alias RegexException = std.regex.internal.ir.RegexException;

/++
  A range that lazily produces a string output escaped
  to be used inside of a regular expression.
+/
auto escaper(Range)(Range r)
{
    import std.algorithm.searching : find;
    static immutable escapables = [Escapables];
    static struct Escaper // template to deduce attributes
    {
        Range r;
        bool escaped;

        @property ElementType!Range front(){
          if (escaped)
              return '\\';
          else
              return r.front;
        }

        @property bool empty(){ return r.empty; }

        void popFront(){
          if (escaped) escaped = false;
          else
          {
              r.popFront();
              if (!r.empty && !escapables.find(r.front).empty)
                  escaped = true;
          }
        }

        @property auto save(){ return Escaper(r.save, escaped); }
    }

    bool escaped = !r.empty && !escapables.find(r.front).empty;
    return Escaper(r, escaped);
}

///
@system unittest
{
    import std.algorithm.comparison;
    import std.regex;
    string s = `This is {unfriendly} to *regex*`;
    assert(s.escaper.equal(`This is \{unfriendly\} to \*regex\*`));
}

@system unittest
{
    import std.algorithm.comparison;
    import std.conv;
    static foreach (S; AliasSeq!(string, wstring, dstring))
    {{
      auto s = "^".to!S;
      assert(s.escaper.equal(`\^`));
      auto s2 = "";
      assert(s2.escaper.equal(""));
    }}
}

@system unittest
{
    assert("ab".matchFirst(regex(`a?b?`)).hit == "ab");
    assert("ab".matchFirst(regex(`a??b?`)).hit == "");
}

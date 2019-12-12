/*
    Regualar expressions package test suite.
*/
module std.regex.internal.tests;

package(std.regex):

import std.conv, std.exception, std.meta, std.range,
    std.typecons, std.regex;

import std.regex.internal.parser : Escapables; // characters that need escaping

alias Sequence(int B, int E) = staticIota!(B, E);

@safe unittest
{//sanity checks
    regex("(a|b)*");
    regex(`(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(.*)\s*#`);
    regex("abc|edf|ighrg");
    auto r1 = regex("abc");
    auto r2 = regex("(gylba)");
    assert(match("abcdef", r1).hit == "abc");
    assert(!match("wida",r2));
    assert(bmatch("abcdef", r1).hit == "abc");
    assert(!bmatch("wida", r2));
    assert(match("abc", "abc".dup));
    assert(bmatch("abc", "abc".dup));
    Regex!char rc;
    assert(rc.empty);
    rc = regex("test");
    assert(!rc.empty);
}

/* The test vectors in this file are altered from Henry Spencer's regexp
   test code. His copyright notice is:

        Copyright (c) 1986 by University of Toronto.
        Written by Henry Spencer.  Not derived from licensed software.

        Permission is granted to anyone to use this software for any
        purpose on any computer system, and to redistribute it freely,
        subject to the following restrictions:

        1. The author is not responsible for the consequences of use of
                this software, no matter how awful, even if they arise
                from defects in it.

        2. The origin of this software must not be misrepresented, either
                by explicit claim or by omission.

        3. Altered versions must be plainly marked as such, and must not
                be misrepresented as being the original software.


 */

@safe unittest
{
    struct TestVectors
    {
        string pattern;
        string input;
        string result;
        string format;
        string replace;
        string flags;
    }

    static immutable TestVectors[] tv = [
        TestVectors(  "a\\b",       "a",  "y",    "$&",    "a" ),
        TestVectors(  "(a)b\\1",   "abaab","y",    "$&",    "aba" ),
        TestVectors(  "()b\\1",     "aaab", "y",    "$&",    "b" ),
        TestVectors(  "abc",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "abc",       "xbc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "axc",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "abx",  "n",    "-",    "-" ),
        TestVectors(  "abc",       "xabcy","y",    "$&",    "abc" ),
        TestVectors(  "abc",       "ababc","y",    "$&",    "abc" ),
        TestVectors(  "ab*c",      "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab*bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab*bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab+bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab+bc",     "abc",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abq",  "n",    "-",    "-" ),
        TestVectors(  "ab+bc",     "abbbbc","y",   "$&",    "abbbbc" ),
        TestVectors(  "ab?bc",     "abbc", "y",    "$&",    "abbc" ),
        TestVectors(  "ab?bc",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "ab?bc",     "abbbbc","n",   "-",    "-" ),
        TestVectors(  "ab?c",      "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "abcc", "n",    "-",    "-" ),
        TestVectors(  "^abc",      "abcc", "y",    "$&",    "abc" ),
        TestVectors(  "^abc$",     "aabc", "n",    "-",    "-" ),
        TestVectors(  "abc$",      "aabc", "y",    "$&",    "abc" ),
        TestVectors(  "^",         "abc",  "y",    "$&",    "" ),
        TestVectors(  "$",         "abc",  "y",    "$&",    "" ),
        TestVectors(  "a.c",       "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a.c",       "axc",  "y",    "$&",    "axc" ),
        TestVectors(  "a.*c",      "axyzc","y",    "$&",    "axyzc" ),
        TestVectors(  "a.*c",      "axyzd","n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abc",  "n",    "-",    "-" ),
        TestVectors(  "a[bc]d",    "abd",  "y",    "$&",    "abd" ),
        TestVectors(  "a[b-d]e",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[b-d]e",   "ace",  "y",    "$&",    "ace" ),
        TestVectors(  "a[b-d]",    "aac",  "y",    "$&",    "ac" ),
        TestVectors(  "a[-b]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-]",     "a-",   "y",    "$&",    "a-" ),
        TestVectors(  "a[b-a]",    "-",    "c",    "-",    "-" ),
        TestVectors(  "a[]b",      "-",    "c",    "-",    "-" ),
        TestVectors(  "a[",        "-",    "c",    "-",    "-" ),
        TestVectors(  "a]",        "a]",   "y",    "$&",    "a]" ),
        TestVectors(  "a[\\]]b",     "a]b",  "y",  "$&",    "a]b" ),
        TestVectors(  "a[^bc]d",   "aed",  "y",    "$&",    "aed" ),
        TestVectors(  "a[^bc]d",   "abd",  "n",    "-",    "-" ),
        TestVectors(  "a[^-b]c",   "adc",  "y",    "$&",    "adc" ),
        TestVectors(  "a[^-b]c",   "a-c",  "n",    "-",    "-" ),
        TestVectors(  "a[^\\]b]c",   "adc",  "y",  "$&",    "adc" ),
        TestVectors(  "ab|cd",     "abc",  "y",    "$&",    "ab" ),
        TestVectors(  "ab|cd",     "abcd", "y",    "$&",    "ab" ),
        TestVectors(  "()ef",      "def",  "y",    "$&-$1",        "ef-" ),
        TestVectors(  "()*",       "-",    "y",    "-",    "-" ),
        TestVectors(  "*a",        "-",    "c",    "-",    "-" ),
        TestVectors(  "^*",        "-",    "y",    "-",    "-" ),
        TestVectors(  "$*",        "-",    "y",    "-",    "-" ),
        TestVectors(  "(*)b",      "-",    "c",    "-",    "-" ),
        TestVectors(  "$b",        "b",    "n",    "-",    "-" ),
        TestVectors(  "a\\",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a\\(b",     "a(b",  "y",    "$&-$1",        "a(b-" ),
        TestVectors(  "a\\(*b",    "ab",   "y",    "$&",    "ab" ),
        TestVectors(  "a\\(*b",    "a((b", "y",    "$&",    "a((b" ),
        TestVectors(  "a\\\\b",    "a\\b", "y",    "$&",    "a\\b" ),
        TestVectors(  "abc)",      "-",    "c",    "-",    "-" ),
        TestVectors(  "(abc",      "-",    "c",    "-",    "-" ),
        TestVectors(  "((a))",     "abc",  "y",    "$&-$1-$2",    "a-a-a" ),
        TestVectors(  "(a)b(c)",   "abc",  "y",    "$&-$1-$2",    "abc-a-c" ),
        TestVectors(  "a+b+c",     "aabbabc","y",  "$&",    "abc" ),
        TestVectors(  "a**",       "-",    "c",    "-",    "-" ),
        TestVectors(  "a*?a",      "aa",   "y",    "$&",    "a" ),
        TestVectors(  "(a*)*",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a*)+",     "aaa",  "y",    "-",    "-" ),
        TestVectors(  "(a|)*",     "-",    "y",    "-",    "-" ),
        TestVectors(  "(a*|b)*",   "aabb", "y",    "-",    "-" ),
        TestVectors(  "(a|b)*",    "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)*",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)+",   "ab",   "y",    "$&-$1",        "ab-b" ),
        TestVectors(  "(a+|b)?",   "ab",   "y",    "$&-$1",        "a-a" ),
        TestVectors(  "[^ab]*",    "cde",  "y",    "$&",    "cde" ),
        TestVectors(  "(^)*",      "-",    "y",    "-",    "-" ),
        TestVectors(  "(ab|)*",    "-",    "y",    "-",    "-" ),
        TestVectors(  ")(",        "-",    "c",    "-",    "-" ),
        TestVectors(  "",  "abc",  "y",    "$&",    "" ),
        TestVectors(  "abc",       "",     "n",    "-",    "-" ),
        TestVectors(  "a*",        "",     "y",    "$&",    "" ),
        TestVectors(  "([abc])*d", "abbbcd",       "y",    "$&-$1",        "abbbcd-c" ),
        TestVectors(  "([abc])*bcd", "abcd",       "y",    "$&-$1",        "abcd-a" ),
        TestVectors(  "a|b|c|d|e", "e",    "y",    "$&",    "e" ),
        TestVectors(  "(a|b|c|d|e)f", "ef",        "y",    "$&-$1",        "ef-e" ),
        TestVectors(  "((a*|b))*", "aabb", "y",    "-",    "-" ),
        TestVectors(  "abcd*efg",  "abcdefg",      "y",    "$&",    "abcdefg" ),
        TestVectors(  "ab*",       "xabyabbbz",    "y",    "$&",    "ab" ),
        TestVectors(  "ab*",       "xayabbbz",     "y",    "$&",    "a" ),
        TestVectors(  "(ab|cd)e",  "abcde",        "y",    "$&-$1",        "cde-cd" ),
        TestVectors(  "[abhgefdc]ij",      "hij",  "y",    "$&",    "hij" ),
        TestVectors(  "^(ab|cd)e", "abcde",        "n",    "x$1y",        "xy" ),
        TestVectors(  "(abc|)ef",  "abcdef",       "y",    "$&-$1",        "ef-" ),
        TestVectors(  "(a|b)c*d",  "abcd",         "y",    "$&-$1",        "bcd-b" ),
        TestVectors(  "(ab|ab*)bc",        "abc",  "y",    "$&-$1",        "abc-a" ),
        TestVectors(  "a([bc]*)c*",        "abc",  "y",    "$&-$1",        "abc-bc" ),
        TestVectors(  "a([bc]*)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]+)(c*d)",     "abcd", "y",    "$&-$1-$2",    "abcd-bc-d" ),
        TestVectors(  "a([bc]*)(c+d)",     "abcd", "y",    "$&-$1-$2",    "abcd-b-cd" ),
        TestVectors(  "a[bcd]*dcdcde",     "adcdcde",      "y",    "$&",    "adcdcde" ),
        TestVectors(  "a[bcd]+dcdcde",     "adcdcde",      "n",    "-",    "-" ),
        TestVectors(  "(ab|a)b*c", "abc",           "y",    "$&-$1",        "abc-ab" ),
        TestVectors(  "((a)(b)c)(d)",      "abcd",  "y",    "$1-$2-$3-$4",      "abc-a-b-d" ),
        TestVectors(  "[a-zA-Z_][a-zA-Z0-9_]*",    "alpha",        "y",    "$&",    "alpha" ),
        TestVectors(  "^a(bc+|b[eh])g|.h$",        "abh",  "y",    "$&-$1",        "bh-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effgz",        "y",    "$&-$1-$2",    "effgz-effgz-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "ij",   "y",    "$&-$1-$2",    "ij-ij-j" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "effg", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "bcdd", "n",    "-",    "-" ),
        TestVectors(  "(bc+d$|ef*g.|h?i(j|k))",    "reffgz",       "y",    "$&-$1-$2",    "effgz-effgz-" ),
        TestVectors(  "(((((((((a)))))))))",       "a",    "y",    "$&",    "a" ),
        TestVectors(  "multiple words of text",    "uh-uh",        "n",    "-",    "-" ),
        TestVectors(  "multiple words",    "multiple words, yeah", "y",    "$&",    "multiple words" ),
        TestVectors(  "(.*)c(.*)", "abcde",                "y",    "$&-$1-$2",    "abcde-ab-de" ),
        TestVectors(  "\\((.*), (.*)\\)",  "(a, b)",       "y",    "($2, $1)",   "(b, a)" ),
        TestVectors(  "abcd",      "abcd",                   "y",    "$&-&-$$$&",  "abcd-&-$abcd" ),
        TestVectors(  "a(bc)d",    "abcd",                 "y",    "$1-$$1-$$$1",    "bc-$1-$bc" ),
        TestVectors(  "[k]",                       "ab",   "n",    "-",    "-" ),
        TestVectors(  "[ -~]*",                    "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~]*",                 "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~]*",              "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~]*",           "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~]*",        "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~]*",     "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "[ -~ -~ -~ -~ -~ -~ -~]*",  "abc",  "y",    "$&",    "abc" ),
        TestVectors(  "a{2}",      "candy",                "n",    "",     "" ),
        TestVectors(  "a{2}",      "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2}",      "caaandy",              "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "candy",                "n",    "",     "" ),
        TestVectors(  "a{2,}",     "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{2,}",     "caaaaaandy",           "y",    "$&",    "aaaaaa" ),
        TestVectors(  "a{1,3}",    "cndy",                 "n",    "",     "" ),
        TestVectors(  "a{1,3}",    "candy",                "y",    "$&",    "a" ),
        TestVectors(  "a{1,3}",    "caandy",               "y",    "$&",    "aa" ),
        TestVectors(  "a{1,3}",    "caaaaaandy",           "y",    "$&",    "aaa" ),
        TestVectors(  "e?le?",     "angel",                "y",    "$&",    "el" ),
        TestVectors(  "e?le?",     "angle",                "y",    "$&",    "le" ),
        TestVectors(  "\\bn\\w",   "noonday",              "y",    "$&",    "no" ),
        TestVectors(  "\\wy\\b",   "possibly yesterday",   "y",    "$&",    "ly" ),
        TestVectors(  "\\w\\Bn",   "noonday",              "y",    "$&",    "on" ),
        TestVectors(  "y\\B\\w",   "possibly yesterday",   "y",    "$&",    "ye" ),
        TestVectors(  "\\cJ",      "abc\ndef",             "y",    "$&",    "\n" ),
        TestVectors(  "\\d",       "B2 is",                "y",    "$&",    "2" ),
        TestVectors(  "\\D",       "B2 is",                "y",    "$&",    "B" ),
        TestVectors(  "\\s\\w*",   "foo bar",              "y",    "$&",    " bar" ),
        TestVectors(  "\\S\\w*",   "foo bar",              "y",    "$&",    "foo" ),
        TestVectors(  "abc",       "ababc",                "y",    "$&",    "abc" ),
        TestVectors(  "apple(,)\\sorange\\1",      "apple, orange, cherry, peach", "y", "$&", "apple, orange," ),
        TestVectors(  "(\\w+)\\s(\\w+)",           "John Smith", "y", "$2, $1", "Smith, John" ),
        TestVectors(  "\\n\\f\\r\\t\\v",           "abc\n\f\r\t\vdef", "y", "$&", "\n\f\r\t\v" ),
        TestVectors(  ".*c",       "abcde",                        "y",    "$&",    "abc" ),
        TestVectors(  "^\\w+((;|=)\\w+)+$", "some=host=tld",    "y", "$&-$1-$2", "some=host=tld-=tld-=" ),
        TestVectors(  "^\\w+((\\.|-)\\w+)+$", "some.host.tld",    "y", "$&-$1-$2", "some.host.tld-.tld-." ),
        TestVectors(  "q(a|b)*q",  "xxqababqyy",                "y",    "$&-$1",        "qababq-b" ),
        TestVectors(  "^(a)(b){0,1}(c*)",   "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b)?(c*)",       "abcc", "y", "$1 $2 $3", "a b cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "abcc", "y", "$1 $2 $3", "a b b" ),
        TestVectors(  "^(a)(b){0,1}(c*)",   "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b){0,1})(c*)", "acc",  "y", "$1 $2 $3", "a  " ),
        TestVectors(  "^(a)(b)?(c*)",       "acc",  "y", "$1 $2 $3", "a  cc" ),
        TestVectors(  "^(a)((b)?)(c*)",     "acc",  "y", "$1 $2 $3", "a  " ),
        TestVectors(  "(?:ab){3}",       "_abababc","y", "$&-$1",    "ababab-" ),
        TestVectors(  "(?:a(?:x)?)+",    "aaxaxx",  "y", "$&-$1-$2", "aaxax--" ),
        TestVectors(  `\W\w\W`,         "aa b!ca",  "y", "$&",       " b!"),
//more repetitions:
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}",  "aaabaaaabbb", "y", "$&", "aaabaaaabbb" ),
        TestVectors(  "(?:a{2,4}b{1,3}){1,2}?", "aaabaaaabbb", "y", "$&", "aaab" ),
//groups:
        TestVectors(  "(abc)|(edf)|(xyz)",     "xyz",             "y",   "$1-$2-$3","--xyz"),
        TestVectors(  "(?P<q>\\d+)/(?P<d>\\d+)",     "2/3",       "y",     "${d}/${q}",    "3/2"),
//set operations:
        TestVectors(  "[a-z--d-f]",                  " dfa",      "y",   "$&",     "a"),
        TestVectors(  "[abc[pq--acq]]{2}",           "bqpaca",    "y",   "$&",     "pa"),
        TestVectors(  "[a-z9&&abc0-9]{3}",           "z90a0abc",  "y",   "$&",     "abc"),
        TestVectors(  "[0-9a-f~~0-5a-z]{2}",         "g0a58x",    "y",   "$&",     "8x"),
        TestVectors(  "[abc[pq]xyz[rs]]{4}",         "cqxr",      "y",   "$&",     "cqxr"),
        TestVectors(  "[abcdf--[ab&&[bcd]][acd]]",   "abcdefgh",  "y",   "$&",     "f"),
        TestVectors(  "[a-c||d-f]+",    "abcdef", "y", "$&", "abcdef"),
        TestVectors(  "[a-f--a-c]+",    "abcdef", "y", "$&", "def"),
        TestVectors(  "[a-c&&b-f]+",    "abcdef", "y", "$&", "bc"),
        TestVectors(  "[a-c~~b-f]+",    "abcdef", "y", "$&", "a"),
//unicode blocks & properties:
        TestVectors(  `\P{Inlatin1suppl ement}`, "\u00c2!", "y", "$&", "!"),
        TestVectors(  `\p{InLatin-1 Supplement}\p{in-mathematical-operators}\P{Inlatin1suppl ement}`,
            "\u00c2\u2200\u00c3\u2203.", "y", "$&", "\u00c3\u2203."),
        TestVectors(  `[-+*/\p{in-mathematical-operators}]{2}`,    "a+\u2212",    "y",    "$&",    "+\u2212"),
        TestVectors(  `\p{Ll}+`,                      "XabcD",    "y",  "$&",      "abc"),
        TestVectors(  `\p{Lu}+`,                      "абвГДЕ",   "y",  "$&",      "ГДЕ"),
        TestVectors(  `^\p{Currency Symbol}\p{Sc}`,   "$₤",       "y",  "$&",      "$₤"),
        TestVectors(  `\p{Common}\p{Thai}`,           "!ฆ",       "y",  "$&",      "!ฆ"),
        TestVectors(  `[\d\s]*\D`,  "12 \t3\U00001680\u0F20_2",   "y",  "$&", "12 \t3\U00001680\u0F20_"),
        TestVectors(  `[c-wф]фф`, "ффф", "y", "$&", "ффф"),
//case insensitive:
        TestVectors(   `^abcdEf$`,           "AbCdEF",              "y",   "$&", "AbCdEF",      "i"),
        TestVectors(   `Русский язык`, "рУсскИй ЯзЫк", "y", "$&", "рУсскИй ЯзЫк", "i"),
        TestVectors(   `ⒶⒷⓒ` ,        "ⓐⓑⒸ",                   "y",   "$&", "ⓐⓑⒸ",      "i"),
        TestVectors(   "\U00010400{2}",  "\U00010428\U00010400 ",   "y",   "$&", "\U00010428\U00010400", "i"),
        TestVectors(   `[adzУ-Я]{4}`,    "DzюЯ",                   "y",   "$&", "DzюЯ", "i"),
        TestVectors(   `\p{L}\p{Lu}{10}`, "абвгдеЖЗИКЛ", "y",   "$&", "абвгдеЖЗИКЛ", "i"),
        TestVectors(   `(?:Dåb){3}`,  "DåbDÅBdÅb",                  "y",   "$&", "DåbDÅBdÅb", "i"),
//escapes:
        TestVectors(    `\u0041\u005a\U00000065\u0001`,         "AZe\u0001",       "y",   "$&", "AZe\u0001"),
        TestVectors(    `\u`,               "",   "c",   "-",  "-"),
        TestVectors(    `\U`,               "",   "c",   "-",  "-"),
        TestVectors(    `\u003`,            "",   "c",   "-",  "-"),
        TestVectors(    `[\x00-\x7f]{4}`,        "\x00\x09ab",   "y", "$&", "\x00\x09ab"),
        TestVectors(    `[\cJ\cK\cA-\cD]{3}\cQ`, "\x01\x0B\x0A\x11", "y", "$&", "\x01\x0B\x0A\x11"),
        TestVectors(    `\r\n\v\t\f\\`,     "\r\n\v\t\f\\",   "y",   "$&", "\r\n\v\t\f\\"),
        TestVectors(    `[\u0003\u0001]{2}`,  "\u0001\u0003",         "y",   "$&", "\u0001\u0003"),
        TestVectors(    `^[\u0020-\u0080\u0001\n-\r]{8}`,  "abc\u0001\v\f\r\n",  "y",   "$&", "abc\u0001\v\f\r\n"),
        TestVectors(    `\w+\S\w+`, "ab7!44c",  "y", "$&", "ab7!44c"),
        TestVectors(    `\b\w+\b`,  " abde4 ",  "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  " abde4",   "y", "$&", "abde4"),
        TestVectors(    `\b\w+\b`,  "abde4 ",   "y", "$&", "abde4"),
        TestVectors(    `\pL\pS`,   "a\u02DA",  "y", "$&", "a\u02DA"),
        TestVectors(    `\pX`,      "",         "c", "-",  "-"),
// ^, $, \b, \B, multiline :
        TestVectors(    `\r.*?$`,    "abc\r\nxy", "y", "$&", "\r\nxy", "sm"),
        TestVectors(    `^a$^b$`,    "a\r\nb\n",  "n", "$&", "-", "m"),
        TestVectors(    `^a$\r\n^b$`,"a\r\nb\n",  "y", "$&", "a\r\nb", "m"),
        TestVectors(    `^$`,        "\r\n",      "y", "$&", "", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u2028","y", "$&", "a\nx", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u2029","y", "$&", "a\nx", "m"),
        TestVectors(    `^a$\nx$`,   "a\nx\u0085","y", "$&", "a\nx","m"),
        TestVectors(    `^x$`,       "\u2028x",   "y", "$&", "x", "m"),
        TestVectors(    `^x$`,       "\u2029x",   "y", "$&", "x", "m"),
        TestVectors(    `^x$`,       "\u0085x",   "y", "$&", "x", "m"),
        TestVectors(    `\b^.`,      "ab",        "y", "$&", "a"),
        TestVectors(    `\B^.`,      "ab",        "n", "-",  "-"),
        TestVectors(    `^ab\Bc\B`,  "\r\nabcd",  "y", "$&", "abc", "m"),
        TestVectors(    `^.*$`,      "12345678",  "y", "$&", "12345678"),

// luckily obtained regression on incremental matching in backtracker
        TestVectors(  `^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([^ ]*)\s*#|# (?:\w|_)+=((?:\w|_)+))`,
            "0020  ; White_Space # ", "y", "$1-$2-$3", "--0020"),
//lookahead
        TestVectors(    "(foo.)(?=(bar))",     "foobar foodbar", "y", "$&-$1-$2", "food-food-bar" ),
        TestVectors(    `\b(\d+)[a-z](?=\1)`,  "123a123",        "y", "$&-$1", "123a-123" ),
        TestVectors(    `\$(?!\d{3})\w+`,      "$123 $abc",      "y", "$&", "$abc"),
        TestVectors(    `(abc)(?=(ed(f))\3)`,    "abcedff",      "y", "-", "-"),
        TestVectors(    `\b[A-Za-z0-9.]+(?=(@(?!gmail)))`, "a@gmail,x@com",  "y", "$&-$1", "x-@"),
        TestVectors(    `x()(abc)(?=(d)(e)(f)\2)`,   "xabcdefabc", "y", "$&", "xabc"),
        TestVectors(    `x()(abc)(?=(d)(e)(f)()\3\4\5)`,   "xabcdefdef", "y", "$&", "xabc"),
//lookback
        TestVectors(    `(?<=(ab))\d`,    "12ba3ab4",    "y",   "$&-$1", "4-ab",  "i"),
        TestVectors(    `\w(?<!\d)\w`,   "123ab24",  "y",   "$&", "ab"),
        TestVectors(    `(?<=Dåb)x\w`,  "DåbDÅBxdÅb",  "y",   "$&", "xd", "i"),
        TestVectors(    `(?<=(ab*c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(    `(?<=(ab*?c))x`,   "abbbbcxac",  "y",   "$&-$1", "x-abbbbc"),
        TestVectors(    `(?<=(a.*?c))x`,   "ababbcxac",  "y",   "$&-$1", "x-abbc"),
        TestVectors(    `(?<=(a{2,4}b{1,3}))x`,   "yyaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(    `(?<=((?:a{2,4}b{1,3}){1,2}))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aabbbaaaab"),
        TestVectors(    `(?<=((?:a{2,4}b{1,3}){1,2}?))x`,   "aabbbaaaabx",  "y",   "$&-$1", "x-aaaab"),
        TestVectors(    `(?<=(abc|def|aef))x`,    "abcx", "y",        "$&-$1",  "x-abc"),
        TestVectors(    `(?<=(abc|def|aef))x`,    "aefx", "y",        "$&-$1",  "x-aef"),
        TestVectors(    `(?<=(abc|dabc))(x)`,    "dabcx", "y",        "$&-$1-$2",  "x-abc-x"),
        TestVectors(    `(?<=(|abc))x`,        "dabcx", "y",        "$&-$1",  "x-"),
        TestVectors(    `(?<=((ab|da)*))x`,    "abdaabx", "y",        "$&-$2-$1",  "x-ab-abdaab"),
        TestVectors(    `a(?<=(ba(?<=(aba)(?<=aaba))))`, "aabaa", "y", "$&-$1-$2", "a-ba-aba"),
        TestVectors(    `.(?<!b).`,   "bax",  "y", "$&", "ax"),
        TestVectors(    `(?<=b(?<!ab)).`,   "abbx",  "y",  "$&", "x"),
        TestVectors(    `(?<=\.|[!?]+)X`,   "Hey?!X", "y", "$&", "X"),
        TestVectors(    `(?<=\.|[!?]+)a{3}`,   ".Nope.aaaX", "y", "$&", "aaa"),
//mixed lookaround
        TestVectors(   `a(?<=a(?=b))b`,    "ab", "y",      "$&", "ab"),
        TestVectors(   `a(?<=a(?!b))c`,    "ac", "y",      "$&", "ac"),
        TestVectors(   `a(?i)bc`,         "aBc", "y",      "$&", "aBc"),
        TestVectors(   `a(?i)bc`,         "Abc", "n",      "$&", "-"),
        TestVectors(   `(?i)a(?-i)bc`, "aBcAbc", "y",      "$&", "Abc"),
        TestVectors(   `(?s).(?-s).`, "\n\n\na", "y",      "$&", "\na"),
        TestVectors(   `(?m)^a(?-m)$`,  "\na",   "y",      "$&", "a")
        ];
    string produceExpected(M,String)(auto ref M m, String fmt)
    {
        auto app = appender!(String)();
        replaceFmt(fmt, m.captures, app, true);
        return app.data;
    }
    void run_tests(alias matchFn)()
    {
        int i;
        foreach (Char; AliasSeq!( char, wchar, dchar))
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            alias String = immutable(Char)[];
            String produceExpected(M,Range)(auto ref M m, Range fmt)
            {
                auto app = appender!(String)();
                replaceFmt(fmt, m.captures, app, true);
                return app.data;
            }
            Regex!(Char) r;
            foreach (a, tvd; tv)
            {
                uint c = tvd.result[0];
                debug(std_regex_test) writeln(" Test #", a, " pattern: ", tvd.pattern, " with Char = ", Char.stringof);
                try
                {
                    i = 1;
                    r = regex(to!(String)(tvd.pattern), tvd.flags);
                }
                catch (RegexException e)
                {
                    i = 0;
                    debug(std_regex_test) writeln(e.msg);
                }

                assert((c == 'c') ? !i : i, "failed to compile pattern "~tvd.pattern);

                if (c != 'c')
                {
                    auto m = matchFn(to!(String)(tvd.input), r);
                    i = !m.empty;
                    assert(
                        (c == 'y') ? i : !i,
                        text(matchFn.stringof ~": failed to match pattern #", a ,": ", tvd.pattern)
                    );
                    if (c == 'y')
                    {
                        auto result = produceExpected(m, to!(String)(tvd.format));
                        assert(result == to!String(tvd.replace),
                            text(matchFn.stringof ~": mismatch pattern #", a, ": ", tvd.pattern," expected: ",
                                    tvd.replace, " vs ", result));
                    }
                }
            }
        }();
        debug(std_regex_test) writeln("!!! FReD bulk test done "~matchFn.stringof~" !!!");
    }


    void ct_tests()
    {
        import std.algorithm.comparison : equal;
        version (std_regex_ct1)
        {
            pragma(msg, "Testing 1st part of ctRegex");
            alias Tests = Sequence!(0, 155);
        }
        else version (std_regex_ct2)
        {
            pragma(msg, "Testing 2nd part of ctRegex");
            alias Tests = Sequence!(155, 174);
        }
        //FIXME: #174-178 contains CTFE parser bug
        else version (std_regex_ct3)
        {
            pragma(msg, "Testing 3rd part of ctRegex");
            alias Tests = Sequence!(178, 220);
        }
        else version (std_regex_ct4)
        {
            pragma(msg, "Testing 4th part of ctRegex");
            alias Tests = Sequence!(220, tv.length);
        }
        else
            alias Tests = AliasSeq!(Sequence!(0, 30), Sequence!(235, tv.length-5));
        foreach (a, v; Tests)
        (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
            enum tvd = tv[v];
            static if (tvd.result == "c")
            {
                static assert(!__traits(compiles, (){
                    enum r = regex(tvd.pattern, tvd.flags);
                }), "errornously compiles regex pattern: " ~ tvd.pattern);
            }
            else
            {
                //BUG: tv[v] is fine but tvd is not known at compile time?!
                auto r = ctRegex!(tv[v].pattern, tv[v].flags);
                auto nr = regex(tvd.pattern, tvd.flags);
                assert(equal(r.ir, nr.ir),
                    text("!C-T regex! failed to compile pattern #", a ,": ", tvd.pattern));
                auto m = match(tvd.input, r);
                auto c = tvd.result[0];
                bool ok = (c == 'y') ^ m.empty;
                assert(ok, text("ctRegex: failed to match pattern #",
                    a ,": ", tvd.pattern));
                if (c == 'y')
                {
                    import std.stdio;
                    auto result = produceExpected(m, tvd.format);
                    if (result != tvd.replace)
                        writeln("ctRegex mismatch pattern #", a, ": ", tvd.pattern," expected: ",
                                tvd.replace, " vs ", result);
                }
            }
        }();
        debug(std_regex_test) writeln("!!! FReD C-T test done !!!");
    }

    ct_tests();
    run_tests!bmatch(); //backtracker
    run_tests!match(); //thompson VM
}

@safe unittest
{
    auto cr = ctRegex!("abc");
    assert(bmatch("abc",cr).hit == "abc");
    auto cr2 = ctRegex!("ab*c");
    assert(bmatch("abbbbc",cr2).hit == "abbbbc");
}
@safe unittest
{
    auto cr3 = ctRegex!("^abc$");
    assert(bmatch("abc",cr3).hit == "abc");
    auto cr4 = ctRegex!(`\b(a\B[a-z]b)\b`);
    assert(array(match("azb",cr4).captures) == ["azb", "azb"]);
}

@safe unittest
{
    auto cr5 = ctRegex!("(?:a{2,4}b{1,3}){1,2}");
    assert(bmatch("aaabaaaabbb", cr5).hit == "aaabaaaabbb");
    auto cr6 = ctRegex!("(?:a{2,4}b{1,3}){1,2}?"w);
    assert(bmatch("aaabaaaabbb"w,  cr6).hit == "aaab"w);
}

@safe unittest
{
    auto cr7 = ctRegex!(`\r.*?$`,"sm");
    assert(bmatch("abc\r\nxy",  cr7).hit == "\r\nxy");
    auto greed =  ctRegex!("<packet.*?/packet>");
    assert(bmatch("<packet>text</packet><packet>text</packet>", greed).hit
            == "<packet>text</packet>");
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto cr8 = ctRegex!("^(a)(b)?(c*)");
    auto m8 = bmatch("abcc",cr8);
    assert(m8);
    assert(m8.captures[1] == "a");
    assert(m8.captures[2] == "b");
    assert(m8.captures[3] == "cc");
    auto cr9 = ctRegex!("q(a|b)*q");
    auto m9 = match("xxqababqyy",cr9);
    assert(m9);
    assert(equal(bmatch("xxqababqyy",cr9).captures, ["qababq", "b"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto rtr = regex("a|b|c");
    enum ctr = regex("a|b|c");
    assert(equal(rtr.ir,ctr.ir));
    //CTFE parser BUG is triggered by group
    //in the middle of alternation (at least not first and not last)
    enum testCT = regex(`abc|(edf)|xyz`);
    auto testRT = regex(`abc|(edf)|xyz`);
    assert(equal(testCT.ir,testRT.ir));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    enum cx = ctRegex!"(A|B|C)";
    auto mx = match("B",cx);
    assert(mx);
    assert(equal(mx.captures, [ "B", "B"]));
    enum cx2 = ctRegex!"(A|B)*";
    assert(match("BAAA",cx2));

    enum cx3 = ctRegex!("a{3,4}","i");
    auto mx3 = match("AaA",cx3);
    assert(mx3);
    assert(mx3.captures[0] == "AaA");
    enum cx4 = ctRegex!(`^a{3,4}?[a-zA-Z0-9~]{1,2}`,"i");
    auto mx4 = match("aaaabc", cx4);
    assert(mx4);
    assert(mx4.captures[0] == "aaaab");
    auto cr8 = ctRegex!("(a)(b)?(c*)");
    auto m8 = bmatch("abcc",cr8);
    assert(m8);
    assert(m8.captures[1] == "a");
    assert(m8.captures[2] == "b");
    assert(m8.captures[3] == "cc");
    auto cr9 = ctRegex!(".*$", "gm");
    auto m9 = match("First\rSecond", cr9);
    assert(m9);
    assert(equal(map!"a.hit"(m9), ["First", "", "Second"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
//global matching
    void test_body(alias matchFn)()
    {
        string s = "a quick brown fox jumps over a lazy dog";
        auto r1 = regex("\\b[a-z]+\\b","g");
        string[] test;
        foreach (m; matchFn(s, r1))
            test ~= m.hit;
        assert(equal(test, [ "a", "quick", "brown", "fox", "jumps", "over", "a", "lazy", "dog"]));
        auto free_reg = regex(`

            abc
            \s+
            "
            (
                    [^"]+
                |   \\ "
            )+
            "
            z
        `, "x");
        auto m = match(`abc  "quoted string with \" inside"z`,free_reg);
        assert(m);
        string mails = " hey@you.com no@spam.net ";
        auto rm = regex(`@(?<=\S+@)\S+`,"g");
        assert(equal(map!"a[0]"(matchFn(mails, rm)), ["@you.com", "@spam.net"]));
        auto m2 = matchFn("First line\nSecond line",regex(".*$","gm"));
        assert(equal(map!"a[0]"(m2), ["First line", "", "Second line"]));
        auto m2a = matchFn("First line\nSecond line",regex(".+$","gm"));
        assert(equal(map!"a[0]"(m2a), ["First line", "Second line"]));
        auto m2b = matchFn("First line\nSecond line",regex(".+?$","gm"));
        assert(equal(map!"a[0]"(m2b), ["First line", "Second line"]));
        debug(std_regex_test) writeln("!!! FReD FLAGS test done "~matchFn.stringof~" !!!");
    }
    test_body!bmatch();
    test_body!match();
}

//tests for accumulated std.regex issues and other regressions
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    void test_body(alias matchFn)()
    {
        //issue 5857
        //matching goes out of control if ... in (...){x} has .*/.+
        auto c = matchFn("axxxzayyyyyzd",regex("(a.*z){2}d")).captures;
        assert(c[0] == "axxxzayyyyyzd");
        assert(c[1] == "ayyyyyz");
        auto c2 = matchFn("axxxayyyyyd",regex("(a.*){2}d")).captures;
        assert(c2[0] == "axxxayyyyyd");
        assert(c2[1] == "ayyyyy");
        //issue 2108
        //greedy vs non-greedy
        auto nogreed = regex("<packet.*?/packet>");
        assert(matchFn("<packet>text</packet><packet>text</packet>", nogreed).hit
               == "<packet>text</packet>");
        auto greed =  regex("<packet.*/packet>");
        assert(matchFn("<packet>text</packet><packet>text</packet>", greed).hit
               == "<packet>text</packet><packet>text</packet>");
        //issue 4574
        //empty successful match still advances the input
        string[] pres, posts, hits;
        foreach (m; matchFn("abcabc", regex("","g")))
        {
            pres ~= m.pre;
            posts ~= m.post;
            assert(m.hit.empty);

        }
        auto heads = [
            "abcabc",
            "abcab",
            "abca",
            "abc",
            "ab",
            "a",
            ""
        ];
        auto tails = [
            "abcabc",
             "bcabc",
              "cabc",
               "abc",
                "bc",
                 "c",
                  ""
        ];
        assert(pres == array(retro(heads)));
        assert(posts == tails);
        //issue 6076
        //regression on .*
        auto re = regex("c.*|d");
        auto m = matchFn("mm", re);
        assert(!m);
        debug(std_regex_test) writeln("!!! FReD REGRESSION test done "~matchFn.stringof~" !!!");
        auto rprealloc = regex(`((.){5}.{1,10}){5}`);
        auto arr = array(repeat('0',100));
        auto m2 = matchFn(arr, rprealloc);
        assert(m2);
        assert(collectException(
                regex(r"^(import|file|binary|config)\s+([^\(]+)\(?([^\)]*)\)?\s*$")
                ) is null);
        foreach (ch; [Escapables])
        {
            assert(match(to!string(ch),regex(`[\`~ch~`]`)));
            assert(!match(to!string(ch),regex(`[^\`~ch~`]`)));
            assert(match(to!string(ch),regex(`[\`~ch~`-\`~ch~`]`)));
        }
        //bugzilla 7718
        string strcmd = "./myApp.rb -os OSX -path \"/GIT/Ruby Apps/sec\" -conf 'notimer'";
        auto reStrCmd = regex (`(".*")|('.*')`, "g");
        assert(equal(map!"a[0]"(matchFn(strcmd, reStrCmd)),
                     [`"/GIT/Ruby Apps/sec"`, `'notimer'`]));
    }
    test_body!bmatch();
    test_body!match();
}

// tests for replace
@safe unittest
{
    void test(alias matchFn)()
    {
        import std.uni : toUpper;

        foreach (i, v; AliasSeq!(string, wstring, dstring))
        {
            auto baz(Cap)(Cap m)
            if (is(Cap == Captures!(Cap.String)))
            {
                return toUpper(m.hit);
            }
            alias String = v;
            assert(std.regex.replace!(matchFn)(to!String("ark rapacity"), regex(to!String("r")), to!String("c"))
                   == to!String("ack rapacity"));
            assert(std.regex.replace!(matchFn)(to!String("ark rapacity"), regex(to!String("r"), "g"), to!String("c"))
                   == to!String("ack capacity"));
            assert(std.regex.replace!(matchFn)(to!String("noon"), regex(to!String("^n")), to!String("[$&]"))
                   == to!String("[n]oon"));
            assert(std.regex.replace!(matchFn)(
                to!String("test1 test2"), regex(to!String(`\w+`),"g"), to!String("$`:$'")
            ) == to!String(": test2 test1 :"));
            auto s = std.regex.replace!(baz!(Captures!(String)))(to!String("Strap a rocket engine on a chicken."),
                    regex(to!String("[ar]"), "g"));
            assert(s == "StRAp A Rocket engine on A chicken.");
        }
        debug(std_regex_test) writeln("!!! Replace test done "~matchFn.stringof~"  !!!");
    }
    test!(bmatch)();
    test!(match)();
}

// tests for splitter
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,     fg, hi, ";
    auto sp1 = splitter(s1, regex(", *"));
    auto w1 = ["", "abc", "de", "fg", "hi", ""];
    assert(equal(sp1, w1));

    auto s2 = ", abc, de,  fg, hi";
    auto sp2 = splitter(s2, regex(", *"));
    auto w2 = ["", "abc", "de", "fg", "hi"];

    uint cnt;
    foreach (e; sp2)
    {
        assert(w2[cnt++] == e);
    }
    assert(equal(sp2, w2));
}

@safe unittest
{
    char[] s1 = ", abc, de,  fg, hi, ".dup;
    auto sp2 = splitter(s1, regex(", *"));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,  fg, hi, ";
    auto w1 = ["", "abc", "de", "fg", "hi", ""];
    assert(equal(split(s1, regex(", *")), w1[]));
}

@safe unittest
{ // bugzilla 7141
    string pattern = `[a\--b]`;
    assert(match("-", pattern));
    assert(match("b", pattern));
    string pattern2 = `[&-z]`;
    assert(match("b", pattern2));
}
@safe unittest
{//bugzilla 7111
    assert(match("", regex("^")));
}
@safe unittest
{//bugzilla 7300
    assert(!match("a"d, "aa"d));
}

// bugzilla 7551
@safe unittest
{
    auto r = regex("[]abc]*");
    assert("]ab".matchFirst(r).hit == "]ab");
    assertThrown(regex("[]"));
    auto r2 = regex("[]abc--ab]*");
    assert("]ac".matchFirst(r2).hit == "]");
}

@safe unittest
{//bugzilla 7674
    assert("1234".replace(regex("^"), "$$") == "$1234");
    assert("hello?".replace(regex(r"\?", "g"), r"\?") == r"hello\?");
    assert("hello?".replace(regex(r"\?", "g"), r"\\?") != r"hello\?");
}
@safe unittest
{// bugzilla 7679
    import std.algorithm.comparison : equal;
    foreach (S; AliasSeq!(string, wstring, dstring))
    (){ // avoid slow optimizations for large functions @@@BUG@@@ 2396
        enum re = ctRegex!(to!S(r"\."));
        auto str = to!S("a.b");
        assert(equal(std.regex.splitter(str, re), [to!S("a"), to!S("b")]));
        assert(split(str, re) == [to!S("a"), to!S("b")]);
    }();
}
@safe unittest
{//bugzilla 8203
    string data = "
    NAME   = XPAW01_STA:STATION
    NAME   = XPAW01_STA
    ";
    auto uniFileOld = data;
    auto r = regex(
       r"^NAME   = (?P<comp>[a-zA-Z0-9_]+):*(?P<blk>[a-zA-Z0-9_]*)","gm");
    auto uniCapturesNew = match(uniFileOld, r);
    for (int i = 0; i < 20; i++)
        foreach (matchNew; uniCapturesNew) {}
    //a second issue with same symptoms
    auto r2 = regex(`([а-яА-Я\-_]+\s*)+(?<=[\s\.,\^])`);
    match("аллея Театральная", r2);
}
@safe unittest
{// bugzilla 8637 purity of enforce
    auto m = match("hello world", regex("world"));
    enforce(m);
}

// bugzilla 8725
@safe unittest
{
  static italic = regex( r"\*
                (?!\s+)
                (.*?)
                (?!\s+)
                \*", "gx" );
  string input = "this * is* interesting, *very* interesting";
  assert(replace(input, italic, "<i>$1</i>") ==
      "this * is* interesting, <i>very</i> interesting");
}

// bugzilla 8349
@safe unittest
{
    enum peakRegexStr = r"\>(wgEncode.*Tfbs.*\.(?:narrow)|(?:broad)Peak.gz)</a>";
    enum peakRegex = ctRegex!(peakRegexStr);
    //note that the regex pattern itself is probably bogus
    assert(match(r"\>wgEncode-blah-Tfbs.narrow</a>", peakRegex));
}

// bugzilla 9211
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto rx_1 =  regex(r"^(\w)*(\d)");
    auto m = match("1234", rx_1);
    assert(equal(m.front, ["1234", "3", "4"]));
    auto rx_2 = regex(r"^([0-9])*(\d)");
    auto m2 = match("1234", rx_2);
    assert(equal(m2.front, ["1234", "3", "4"]));
}

// bugzilla 9280
@safe unittest
{
    string tomatch = "a!b@c";
    static r = regex(r"^(?P<nick>.*?)!(?P<ident>.*?)@(?P<host>.*?)$");
    auto nm = match(tomatch, r);
    assert(nm);
    auto c = nm.captures;
    assert(c[1] == "a");
    assert(c["nick"] == "a");
}


// bugzilla 9579
@safe unittest
{
    char[] input = ['a', 'b', 'c'];
    string format = "($1)";
    // used to give a compile error:
    auto re = regex(`(a)`, "g");
    auto r = replace(input, re, format);
    assert(r == "(a)bc");
}

// bugzilla 9634
@safe unittest
{
    auto re = ctRegex!"(?:a+)";
    assert(match("aaaa", re).hit == "aaaa");
}

//bugzilla 10798
@safe unittest
{
    auto cr = ctRegex!("[abcd--c]*");
    auto m  = "abc".match(cr);
    assert(m);
    assert(m.hit == "ab");
}

// bugzilla 10913
@system unittest
{
    @system static string foo(const(char)[] s)
    {
        return s.dup;
    }
    @safe static string bar(const(char)[] s)
    {
        return s.dup;
    }
    () @system {
        replace!((a) => foo(a.hit))("blah", regex(`a`));
    }();
    () @safe {
        replace!((a) => bar(a.hit))("blah", regex(`a`));
    }();
}

// bugzilla 11262
@safe unittest
{
    enum reg = ctRegex!(r",", "g");
    auto str = "This,List";
    str = str.replace(reg, "-");
    assert(str == "This-List");
}

// bugzilla 11775
@safe unittest
{
    assert(collectException(regex("a{1,0}")));
}

// bugzilla 11839
@safe unittest
{
    import std.algorithm.comparison : equal;
    assert(regex(`(?P<var1>\w+)`).namedCaptures.equal(["var1"]));
    assert(collectException(regex(`(?P<1>\w+)`)));
    assert(regex(`(?P<v1>\w+)`).namedCaptures.equal(["v1"]));
    assert(regex(`(?P<__>\w+)`).namedCaptures.equal(["__"]));
    assert(regex(`(?P<я>\w+)`).namedCaptures.equal(["я"]));
}

// bugzilla 12076
@safe unittest
{
    auto RE = ctRegex!(r"(?<!x[a-z]+)\s([a-z]+)");
    string s = "one two";
    auto m = match(s, RE);
}

// bugzilla 12105
@safe unittest
{
    auto r = ctRegex!`.*?(?!a)`;
    assert("aaab".matchFirst(r).hit == "aaa");
    auto r2 = ctRegex!`.*(?!a)`;
    assert("aaab".matchFirst(r2).hit == "aaab");
}

//bugzilla 11784
@safe unittest
{
    assert("abcdefghijklmnopqrstuvwxyz"
        .matchFirst("[a-z&&[^aeiuo]]").hit == "b");
}

//bugzilla 12366
@safe unittest
{
     auto re = ctRegex!(`^((?=(xx+?)\2+$)((?=\2+$)(?=(x+)(\4+$))\5){2})*x?$`);
     assert("xxxxxxxx".match(re).empty);
     assert(!"xxxx".match(re).empty);
}

// bugzilla 12582
@safe unittest
{
    auto r = regex(`(?P<a>abc)`);
    assert(collectException("abc".matchFirst(r)["b"]));
}

// bugzilla 12691
@safe unittest
{
    assert(bmatch("e@", "^([a-z]|)*$").empty);
    assert(bmatch("e@", ctRegex!`^([a-z]|)*$`).empty);
}

//bugzilla  12713
@safe unittest
{
    assertThrown(regex("[[a-z]([a-z]|(([[a-z])))"));
}

//bugzilla 12747
@safe unittest
{
    assertThrown(regex(`^x(\1)`));
    assertThrown(regex(`^(x(\1))`));
    assertThrown(regex(`^((x)(?=\1))`));
}

// bugzilla 14504
@safe unittest
{
    auto p = ctRegex!("a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?" ~
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
}

// bugzilla 14529
@safe unittest
{
    auto ctPat2 = regex(r"^[CDF]$", "i");
    foreach (v; ["C", "c", "D", "d", "F", "f"])
        assert(matchAll(v, ctPat2).front.hit == v);
}

// bugzilla 14615
@safe unittest
{
    import std.array : appender;
    import std.regex : replaceFirst, replaceFirstInto, regex;
    import std.stdio : writeln;

    auto example = "Hello, world!";
    auto pattern = regex("^Hello, (bug)");  // won't find this one
    auto result = replaceFirst(example, pattern, "$1 Sponge Bob");
    assert(result == "Hello, world!");  // Ok.

    auto sink = appender!string;
    replaceFirstInto(sink, example, pattern, "$1 Sponge Bob");
    assert(sink.data == "Hello, world!");
    replaceAllInto(sink, example, pattern, "$1 Sponge Bob");
    assert(sink.data == "Hello, world!Hello, world!");
}

// bugzilla 15573
@safe unittest
{
    auto rx = regex("[c d]", "x");
    assert("a b".matchFirst(rx));
}

// bugzilla 15864
@safe unittest
{
    regex(`(<a (?:(?:\w+=\"[^"]*\")?\s*)*href="\.\.?)"`);
}

@safe unittest
{
    auto r = regex("(?# comment)abc(?# comment2)");
    assert("abc".matchFirst(r));
    assertThrown(regex("(?#..."));
}

// bugzilla 17075
@safe unittest
{
    enum titlePattern = `<title>(.+)</title>`;
    static titleRegex = ctRegex!titlePattern;
    string input = "<title>" ~ "<".repeat(100_000).join;
    assert(input.matchFirst(titleRegex).empty);
}

// bugzilla 17212
@safe unittest
{
    auto r = regex(" [a] ", "x");
    assert("a".matchFirst(r));
}

// bugzilla 17157
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto ctr = ctRegex!"(a)|(b)|(c)|(d)";
    auto r = regex("(a)|(b)|(c)|(d)", "g");
    auto s = "--a--b--c--d--";
    auto outcomes = [
        ["a", "a", "", "", ""],
        ["b", "", "b", "", ""],
        ["c", "", "", "c", ""],
        ["d", "", "", "", "d"]
    ];
    assert(equal!equal(s.matchAll(ctr), outcomes));
    assert(equal!equal(s.bmatch(r), outcomes));
}

// bugzilla 17667
@safe unittest
{
    import std.algorithm.searching : canFind;
    void willThrow(T, size_t line = __LINE__)(T arg, string msg)
    {
        auto e = collectException(regex(arg));
        assert(e.msg.canFind(msg), to!string(line) ~ ": " ~ e.msg);
    }
    willThrow([r".", r"[\(\{[\]\}\)]"], "no matching ']' found while parsing character class");
    willThrow([r"[\", r"123"], "no matching ']' found while parsing character class");
    willThrow([r"[a-", r"123"], "no matching ']' found while parsing character class");
    willThrow([r"[a-\", r"123"], "invalid escape sequence");
    willThrow([r"\", r"123"], "invalid escape sequence");
}

// bugzilla 17668
@safe unittest
{
    import std.algorithm.searching;
    auto e = collectException!RegexException(regex(q"<[^]>"));
    assert(e.msg.canFind("no operand for '^'"));
}

// bugzilla 17673
@safe unittest
{
    string str = `<">`;
    string[] regexps = ["abc", "\"|x"];
    auto regexp = regex(regexps);
    auto c = matchFirst(str, regexp);
    assert(c);
    assert(c.whichPattern == 2);
}


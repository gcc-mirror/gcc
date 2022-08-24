/**
* parse configuration options
*
* Copyright: Copyright Digital Mars 2017
* License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
*
* Source: $(DRUNTIMESRC core/internal/parseoptions.d)
*/

module core.internal.parseoptions;

import core.stdc.stdlib;
import core.stdc.stdio;
import core.stdc.ctype;
import core.stdc.string;
import core.vararg;
import core.internal.traits : externDFunc, hasUDA;


@nogc nothrow:
extern extern(C) string[] rt_args() @system;

extern extern(C) __gshared bool rt_envvars_enabled;
extern extern(C) __gshared bool rt_cmdline_enabled;
extern extern(C) __gshared string[] rt_options;

alias rt_configCallBack = string delegate(string) @nogc nothrow;
alias fn_configOption = string function(string opt, scope rt_configCallBack dg, bool reverse) @nogc nothrow;
alias rt_configOption = externDFunc!("rt.config.rt_configOption", fn_configOption);

/// UDA for field treated as memory value
struct MemVal {}

/**
* initialize members of struct CFG from rt_config options
*
* options will be read from the environment, the command line or embedded
* into the executable as configured (see rt.config)
*
* fields of the struct are populated by parseOptions().
*/
bool initConfigOptions(CFG)(ref CFG cfg, string cfgname)
{
    string parse(string opt) @nogc nothrow
    {
        if (!parseOptions(cfg, opt))
            return "err";
        return null; // continue processing
    }
    string s = rt_configOption(cfgname, &parse, true);
    return s is null;
}

/**
* initialize members of struct CFG from a string of sub-options.
*
* fields of the struct are populated by listing them as space separated
* sub-options <field-name>:value, e.g. "precise:1 profile:1"
*
* supported field value types:
*  - strings (without spaces)
*  - integer types (positive values only)
*  - bool
*  - float
*
* If the struct has a member "help" it is called if it is found as a sub-option.
* If the struct has a member "errorName", is used as the name reported in error
* messages. Otherwise the struct name is used.
*/
bool parseOptions(CFG)(ref CFG cfg, string opt)
{
    static if (is(typeof(__traits(getMember, CFG, "errorName"))))
        string errName = cfg.errorName;
    else
        string errName = CFG.stringof;
    opt = skip!isspace(opt);
    while (opt.length)
    {
        auto tail = find!(c => c == ':' || c == '=' || c == ' ')(opt);
        auto name = opt[0 .. $ - tail.length];
        static if (is(typeof(__traits(getMember, CFG, "help"))))
            if (name == "help")
            {
                version (CoreUnittest) {} else
                cfg.help();
                opt = skip!isspace(tail);
                continue;
            }
        if (tail.length <= 1 || tail[0] == ' ')
            return optError("Missing argument for", name, errName);
        tail = tail[1 .. $];

        NAMES_SWITCH:
        switch (name)
        {
            static foreach (field; __traits(allMembers, CFG))
            {
                static if (!is(typeof(__traits(getMember, cfg, field)) == function))
                {
                    case field:
                        bool r;

                        static if (hasUDA!(__traits(getMember, cfg, field), MemVal))
                            r = parse(name, tail, __traits(getMember, cfg, field), errName, true);
                        else
                            r = parse(name, tail, __traits(getMember, cfg, field), errName);

                        if (!r)
                            return false;

                        break NAMES_SWITCH;
                }
            }

            default:
                return optError("Unknown", name, errName);
        }
        opt = skip!isspace(tail);
    }
    return true;
}

/**
Parses an individual option `optname` value from a provided string `str`.
The option type is given by the type `T` of the field `res` to which the parsed
value will be written too.
In case of an error, `errName` will be used to display an error message and
the failure of the parsing will be indicated by a falsy return value.

For boolean values, '0/n/N' (false) or '1/y/Y' (true) are accepted.

Params:
    optname = name of the option to parse
    str = raw string to parse the option value from
    res = reference to the resulting data field that the option should be parsed too
    errName = full-text name of the option which should be displayed in case of errors

Returns: `false` if a parsing error happened.
*/
bool rt_parseOption(T)(const(char)[] optname, ref inout(char)[] str, ref T res, const(char)[] errName)
{
    return parse(optname, str, res, errName);
}

private:

bool optError(const scope char[] msg, const scope char[] name, const(char)[] errName)
{
    version (CoreUnittest) if (inUnittest) return false;

    fprintf(stderr, "%.*s %.*s option '%.*s'.\n",
            cast(int)msg.length, msg.ptr,
            cast(int)errName.length, errName.ptr,
            cast(int)name.length, name.ptr);
    return false;
}

inout(char)[] skip(alias pred)(inout(char)[] str)
{
    return find!(c => !pred(c))(str);
}

inout(char)[] find(alias pred)(inout(char)[] str)
{
    foreach (i; 0 .. str.length)
        if (pred(str[i])) return str[i .. $];
    return null;
}

bool parse(T : size_t)(const(char)[] optname, ref inout(char)[] str, ref T res, const(char)[] errName, bool mayHaveSuffix = false)
if (is(T == size_t))
in { assert(str.length); }
do
{
    size_t i, v;

    auto tail = find!(c => c == ' ')(str);
    size_t len = str.length - tail.length;

    import core.checkedint : mulu;

    bool overflowed;

    for (; i < len; i++)
    {
        char c = str[i];

        if (isdigit(c))
            v = 10 * v + c - '0';
        else // non-digit
        {
            if (mayHaveSuffix && i == len-1) // suffix
            {
                switch (c)
                {

                    case 'G':
                        v = mulu(v, 1024 * 1024 * 1024, overflowed);
                        break;

                    case 'M':
                        v = mulu(v, 1024 * 1024, overflowed);
                        break;

                    case 'K':
                        v = mulu(v, 1024, overflowed);
                        break;

                    case 'B':
                        break;

                    default:
                        return parseError("value with unit type M, K or B", optname, str, "with suffix");
                }

                if (overflowed)
                    return overflowedError(optname, str);

                i++;
                break;
            }
            else // unexpected non-digit character
            {
                i = 0;
                break;
            }
        }
    }

    if (!i)
        return parseError("a number", optname, str, errName);

    if (mayHaveSuffix && isdigit(str[len-1]))
    {
        // No suffix found, default to megabytes

        v = mulu(v, 1024 * 1024, overflowed);

        if (overflowed)
            return overflowedError(optname, str);
    }

    if (v > res.max)
        return parseError("a number " ~ T.max.stringof ~ " or below", optname, str[0 .. i], errName);
    str = str[i .. $];
    res = v;
    return true;
}

bool parse(T : size_t)(const(char)[] optname, ref inout(char)[] str, ref T res, const(char)[] errName, bool mayHaveSuffix = false)
if (!is(T == size_t))
in { assert(str.length); }
do
{
    const oldStr = str;
    size_t v;
    if (!parse!size_t(optname, str, v, errName, mayHaveSuffix))
        return false;

    if (v > res.max)
        return parseError("a number " ~ T.max.stringof ~ " or below", optname, oldStr[0 .. $-str.length], errName);
    res = cast(T) v;
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref bool res, const(char)[] errName)
in { assert(str.length); }
do
{
    if (str[0] == '1' || str[0] == 'y' || str[0] == 'Y')
        res = true;
    else if (str[0] == '0' || str[0] == 'n' || str[0] == 'N')
        res = false;
    else
        return parseError("'0/n/N' or '1/y/Y'", optname, str, errName);
    str = str[1 .. $];
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref float res, const(char)[] errName)
in { assert(str.length); }
do
{
    // % uint f %n \0
    char[1 + 10 + 1 + 2 + 1] fmt=void;
    // specify max-width
    immutable n = snprintf(fmt.ptr, fmt.length, "%%%uf%%n", cast(uint)str.length);
    assert(n > 4 && n < fmt.length);

    int nscanned;
    version (CRuntime_DigitalMars)
    {
        /* Older sscanf's in snn.lib can write to its first argument, causing a crash
        * if the string is in readonly memory. Recent updates to DMD
        * https://github.com/dlang/dmd/pull/6546
        * put string literals in readonly memory.
        * Although sscanf has been fixed,
        * http://ftp.digitalmars.com/snn.lib
        * this workaround is here so it still works with the older snn.lib.
        */
        // Create mutable copy of str
        const length = str.length;
        char* mptr = cast(char*)malloc(length + 1);
        assert(mptr);
        memcpy(mptr, str.ptr, length);
        mptr[length] = 0;
        const result = sscanf(mptr, fmt.ptr, &res, &nscanned);
        free(mptr);
        if (result < 1)
            return parseError("a float", optname, str, errName);
    }
    else
    {
        if (sscanf(str.ptr, fmt.ptr, &res, &nscanned) < 1)
            return parseError("a float", optname, str, errName);
    }
    str = str[nscanned .. $];
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref inout(char)[] res, const(char)[] errName)
in { assert(str.length); }
do
{
    auto tail = str.find!(c => c == ' ');
    res = str[0 .. $ - tail.length];
    if (!res.length)
        return parseError("an identifier", optname, str, errName);
    str = tail;
    return true;
}

bool parseError(const scope char[] exp, const scope char[] opt, const scope char[] got, const(char)[] errName)
{
    version (CoreUnittest) if (inUnittest) return false;

    fprintf(stderr, "Expecting %.*s as argument for %.*s option '%.*s', got '%.*s' instead.\n",
            cast(int)exp.length, exp.ptr,
            cast(int)errName.length, errName.ptr,
            cast(int)opt.length, opt.ptr,
            cast(int)got.length, got.ptr);
    return false;
}

bool overflowedError(const scope char[] opt, const scope char[] got)
{
    version (CoreUnittest) if (inUnittest) return false;

    fprintf(stderr, "Argument for %.*s option '%.*s' is too big.\n",
            cast(int)opt.length, opt.ptr,
            cast(int)got.length, got.ptr);
    return false;
}

size_t min(size_t a, size_t b) { return a <= b ? a : b; }

version (CoreUnittest) __gshared bool inUnittest;

unittest
{
    inUnittest = true;
    scope (exit) inUnittest = false;

    static struct Config
    {
        bool disable;            // start disabled
        ubyte profile;           // enable profiling with summary when terminating program
        string gc = "conservative"; // select gc implementation conservative|manual

        @MemVal size_t initReserve;      // initial reserve (bytes)
        @MemVal size_t minPoolSize = 1 << 20;  // initial and minimum pool size (bytes)
        float heapSizeFactor = 2.0; // heap size to used memory ratio

        @nogc nothrow:
        void help();
        string errorName() @nogc nothrow { return "GC"; }
    }
    Config conf;

    assert(!conf.parseOptions("disable"));
    assert(!conf.parseOptions("disable:"));
    assert(!conf.parseOptions("disable:5"));
    assert(conf.parseOptions("disable:y") && conf.disable);
    assert(conf.parseOptions("disable:n") && !conf.disable);
    assert(conf.parseOptions("disable:Y") && conf.disable);
    assert(conf.parseOptions("disable:N") && !conf.disable);
    assert(conf.parseOptions("disable:1") && conf.disable);
    assert(conf.parseOptions("disable:0") && !conf.disable);

    assert(conf.parseOptions("disable=y") && conf.disable);
    assert(conf.parseOptions("disable=n") && !conf.disable);

    assert(conf.parseOptions("profile=0") && conf.profile == 0);
    assert(conf.parseOptions("profile=1") && conf.profile == 1);
    assert(conf.parseOptions("profile=2") && conf.profile == 2);
    assert(!conf.parseOptions("profile=256"));

    assert(conf.parseOptions("disable:1 minPoolSize:16"));
    assert(conf.disable);
    assert(conf.minPoolSize == 1024 * 1024 * 16);

    assert(conf.parseOptions("disable:1 minPoolSize:4096B"));
    assert(conf.disable);
    assert(conf.minPoolSize == 4096);

    assert(conf.parseOptions("disable:1 minPoolSize:2K help"));
    assert(conf.disable);
    assert(conf.minPoolSize == 2048);

    assert(conf.parseOptions("minPoolSize:3G help"));
    assert(conf.disable);
    assert(conf.minPoolSize == 1024UL * 1024 * 1024 * 3);

    assert(!conf.parseOptions("minPoolSize:922337203685477G"), "size_t overflow");

    assert(conf.parseOptions("heapSizeFactor:3.1"));
    assert(conf.heapSizeFactor == 3.1f);
    assert(conf.parseOptions("heapSizeFactor:3.1234567890 disable:0"));
    assert(conf.heapSizeFactor > 3.123f);
    assert(!conf.disable);
    assert(!conf.parseOptions("heapSizeFactor:3.0.2.5"));
    assert(conf.parseOptions("heapSizeFactor:2"));
    assert(conf.heapSizeFactor == 2.0f);

    assert(!conf.parseOptions("initReserve:foo"));
    assert(!conf.parseOptions("initReserve:y"));
    assert(!conf.parseOptions("initReserve:20.5"));

    assert(conf.parseOptions("help"));
    assert(conf.parseOptions("help profile:1"));
    assert(conf.parseOptions("help profile:1 help"));

    assert(conf.parseOptions("gc:manual") && conf.gc == "manual");
    assert(conf.parseOptions("gc:my-gc~modified") && conf.gc == "my-gc~modified");
    assert(conf.parseOptions("gc:conservative help profile:1") && conf.gc == "conservative" && conf.profile == 1);

    // the config parse doesn't know all available GC names, so should accept unknown ones
    assert(conf.parseOptions("gc:whatever"));
}

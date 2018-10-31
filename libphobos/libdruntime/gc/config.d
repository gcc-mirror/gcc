/**
* Contains the garbage collector configuration.
*
* Copyright: Copyright Digital Mars 2016
* License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
*/

module gc.config;

import core.stdc.stdlib;
import core.stdc.stdio;
import core.stdc.ctype;
import core.stdc.string;
import core.vararg;

nothrow @nogc:
extern extern(C) string[] rt_args();

extern extern(C) __gshared bool rt_envvars_enabled;
extern extern(C) __gshared bool rt_cmdline_enabled;
extern extern(C) __gshared string[] rt_options;

__gshared Config config;

struct Config
{
    bool disable;            // start disabled
    ubyte profile;           // enable profiling with summary when terminating program
    string gc = "conservative"; // select gc implementation conservative|manual

    size_t initReserve;      // initial reserve (MB)
    size_t minPoolSize = 1;  // initial and minimum pool size (MB)
    size_t maxPoolSize = 64; // maximum pool size (MB)
    size_t incPoolSize = 3;  // pool size increment (MB)
    float heapSizeFactor = 2.0; // heap size to used memory ratio

@nogc nothrow:

    bool initialize()
    {
        import core.internal.traits : externDFunc;

        alias rt_configCallBack = string delegate(string) @nogc nothrow;
        alias fn_configOption = string function(string opt, scope rt_configCallBack dg, bool reverse) @nogc nothrow;

        alias rt_configOption = externDFunc!("rt.config.rt_configOption", fn_configOption);

        string parse(string opt) @nogc nothrow
        {
            if (!parseOptions(opt))
                return "err";
            return null; // continue processing
        }
        string s = rt_configOption("gcopt", &parse, true);
        return s is null;
    }

    void help()
    {
        version (unittest) if (inUnittest) return;

        string s = "GC options are specified as white space separated assignments:
    disable:0|1    - start disabled (%d)
    profile:0|1|2  - enable profiling with summary when terminating program (%d)
    gc:conservative|manual - select gc implementation (default = conservative)

    initReserve:N  - initial memory to reserve in MB (%lld)
    minPoolSize:N  - initial and minimum pool size in MB (%lld)
    maxPoolSize:N  - maximum pool size in MB (%lld)
    incPoolSize:N  - pool size increment MB (%lld)
    heapSizeFactor:N - targeted heap size to used memory ratio (%g)
";
        printf(s.ptr, disable, profile, cast(long)initReserve, cast(long)minPoolSize,
               cast(long)maxPoolSize, cast(long)incPoolSize, heapSizeFactor);
    }

    bool parseOptions(string opt)
    {
        opt = skip!isspace(opt);
        while (opt.length)
        {
            auto tail = find!(c => c == ':' || c == '=' || c == ' ')(opt);
            auto name = opt[0 .. $ - tail.length];
            if (name == "help")
            {
                help();
                opt = skip!isspace(tail);
                continue;
            }
            if (tail.length <= 1 || tail[0] == ' ')
                return optError("Missing argument for", name);
            tail = tail[1 .. $];

            switch (name)
            {
            foreach (field; __traits(allMembers, Config))
            {
                static if (!is(typeof(__traits(getMember, this, field)) == function))
                {
                case field:
                    if (!parse(name, tail, __traits(getMember, this, field)))
                        return false;
                    break;
                }
            }
            break;

            default:
                return optError("Unknown", name);
            }
            opt = skip!isspace(tail);
        }
        return true;
    }
}

private:

bool optError(in char[] msg, in char[] name)
{
    version (unittest) if (inUnittest) return false;

    fprintf(stderr, "%.*s GC option '%.*s'.\n",
            cast(int)msg.length, msg.ptr,
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

bool parse(T:size_t)(const(char)[] optname, ref inout(char)[] str, ref T res)
in { assert(str.length); }
body
{
    size_t i, v;
    for (; i < str.length && isdigit(str[i]); ++i)
        v = 10 * v + str[i] - '0';

    if (!i)
        return parseError("a number", optname, str);
    if (v > res.max)
        return parseError("a number " ~ T.max.stringof ~ " or below", optname, str[0 .. i]);
    str = str[i .. $];
    res = cast(T) v;
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref bool res)
in { assert(str.length); }
body
{
    if (str[0] == '1' || str[0] == 'y' || str[0] == 'Y')
        res = true;
    else if (str[0] == '0' || str[0] == 'n' || str[0] == 'N')
        res = false;
    else
        return parseError("'0/n/N' or '1/y/Y'", optname, str);
    str = str[1 .. $];
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref float res)
in { assert(str.length); }
body
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
            return parseError("a float", optname, str);
    }
    else
    {
        if (sscanf(str.ptr, fmt.ptr, &res, &nscanned) < 1)
            return parseError("a float", optname, str);
    }
    str = str[nscanned .. $];
    return true;
}

bool parse(const(char)[] optname, ref inout(char)[] str, ref inout(char)[] res)
in { assert(str.length); }
body
{
    auto tail = str.find!(c => c == ':' || c == '=' || c == ' ');
    res = str[0 .. $ - tail.length];
    if (!res.length)
        return parseError("an identifier", optname, str);
    str = tail;
    return true;
}

bool parseError(in char[] exp, in char[] opt, in char[] got)
{
    version (unittest) if (inUnittest) return false;

    fprintf(stderr, "Expecting %.*s as argument for GC option '%.*s', got '%.*s' instead.\n",
            cast(int)exp.length, exp.ptr,
            cast(int)opt.length, opt.ptr,
            cast(int)got.length, got.ptr);
    return false;
}

size_t min(size_t a, size_t b) { return a <= b ? a : b; }

version (unittest) __gshared bool inUnittest;

unittest
{
    inUnittest = true;
    scope (exit) inUnittest = false;

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
    assert(conf.minPoolSize == 16);

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

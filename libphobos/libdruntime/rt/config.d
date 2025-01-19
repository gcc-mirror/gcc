/**
Configuration options for druntime.

The default way to configure the runtime is by passing command line arguments
starting with `--DRT-` and followed by the option name, e.g. `--DRT-gcopt` to
configure the GC.
When command line parsing is enabled, command line options starting
with `--DRT-` are filtered out before calling main, so the program
will not see them. They are still available via `rt_args()`.

Configuration via the command line can be disabled by declaring a variable for the
linker to pick up before using it's default from the runtime:

---
extern(C) __gshared bool rt_cmdline_enabled = false;
---

Likewise, declare a boolean rt_envvars_enabled to enable configuration via the
environment variable `DRT_` followed by the option name, e.g. `DRT_GCOPT`:

---
extern(C) __gshared bool rt_envvars_enabled = true;
---

Setting default configuration properties in the executable can be done by specifying an
array of options named `rt_options`:

---
extern(C) __gshared string[] rt_options = [ "gcopt=precise:1 profile:1"];
---

Evaluation order of options is `rt_options`, then environment variables, then command
line arguments, i.e. if command line arguments are not disabled, they can override
options specified through the environment or embedded in the executable.

Copyright: Copyright Digital Mars 2014.
License: Distributed under the
     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
   (See accompanying file LICENSE)
Authors:   Rainer Schuetze
Source: $(DRUNTIMESRC rt/_config.d)
*/

module rt.config;

// put each variable in its own COMDAT by making them template instances
template rt_envvars_enabled()
{
    extern(C) pragma(mangle, "rt_envvars_enabled") __gshared bool rt_envvars_enabled = false;
}
template rt_cmdline_enabled()
{
    extern(C) pragma(mangle, "rt_cmdline_enabled") __gshared bool rt_cmdline_enabled = true;
}
template rt_options()
{
    extern(C) pragma(mangle, "rt_options") __gshared string[] rt_options = [];
}

import core.stdc.ctype : toupper;
import core.stdc.stdlib : getenv;
import core.stdc.string : strlen;

extern extern(C) string[] rt_args() @nogc nothrow @system;

alias rt_configCallBack = string delegate(string) @nogc nothrow;

/**
* get a druntime config option using standard configuration options
*      opt             name of the option to retrieve
*      dg              if non-null, passes the option through this
*                      delegate and only returns its return value if non-null
*      reverse         reverse the default processing order cmdline/envvar/rt_options
*                      to allow overwriting settings in the delegate with values
*                      from higher priority
*
* returns the options' value if
*  - set on the command line as "--DRT-<opt>=value" (rt_cmdline_enabled enabled)
*  - the environment variable "DRT_<OPT>" is set (rt_envvars_enabled enabled)
*  - rt_options[] contains an entry "<opt>=value"
*  - null otherwise
*/
string rt_configOption(string opt, scope rt_configCallBack dg = null, bool reverse = false) @nogc nothrow
{
    if (!dg)
        dg = (string s) => s;

    string s = (reverse ? rt_linkOption(opt, dg) : rt_cmdlineOption(opt, dg));
    if (s != null)
        return s;
    s = rt_envvarsOption(opt, dg);
    if (s != null)
        return s;
    s = (reverse ? rt_cmdlineOption(opt, dg) : rt_linkOption(opt, dg));
    return s;
}

string rt_cmdlineOption(string opt, scope rt_configCallBack dg) @nogc nothrow
{
    if (rt_cmdline_enabled!())
    {
        foreach (a; rt_args)
        {
            if (a == "--")
                break;

            if (a.length >= opt.length + 7 && a[0..6] == "--DRT-" &&
                a[6 .. 6 + opt.length] == opt && a[6 + opt.length] == '=')
            {
                string s = dg(a[7 + opt.length .. $]);
                if (s != null)
                    return s;
            }
        }
    }
    return null;
}

string rt_envvarsOption(string opt, scope rt_configCallBack dg) @nogc nothrow
{
    if (rt_envvars_enabled!())
    {
        if (opt.length >= 32)
            assert(0);

        char[40] var = void;
        var[0 .. 4] = "DRT_";
        foreach (i, c; opt)
            var[4 + i] = cast(char) toupper(c);
        var[4 + opt.length] = 0;

        if (auto p = getenv(var.ptr))
        {
            string s = dg(cast(string) p[0 .. strlen(p)]);
            if (s != null)
                return s;
        }
    }
    return null;
}

string rt_linkOption(string opt, scope rt_configCallBack dg) @nogc nothrow
{
    foreach (a; rt_options!())
    {
        if (a.length > opt.length && a[0..opt.length] == opt && a[opt.length] == '=')
        {
            string s = dg(a[opt.length + 1 .. $]);
            if (s != null)
                return s;
        }
    }
    return null;
}

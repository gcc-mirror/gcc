// Written in the D programming language.

/**
Processing of command line options.

The getopt module implements a $(D getopt) function, which adheres to
the POSIX syntax for command line options. GNU extensions are
supported in the form of long options introduced by a double dash
("--"). Support for bundling of command line options, as was the case
with the more traditional single-letter approach, is provided but not
enabled by default.

Copyright: Copyright Andrei Alexandrescu 2008 - 2015.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   $(HTTP erdani.org, Andrei Alexandrescu)
Credits:   This module and its documentation are inspired by Perl's $(HTTP
           perldoc.perl.org/Getopt/Long.html, Getopt::Long) module. The syntax of
           D's $(D getopt) is simpler than its Perl counterpart because $(D
           getopt) infers the expected parameter types from the static types of
           the passed-in pointers.
Source:    $(PHOBOSSRC std/_getopt.d)
*/
/*
         Copyright Andrei Alexandrescu 2008 - 2015.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.getopt;

import std.exception;  // basicExceptionCtors
import std.traits;

/**
Thrown on one of the following conditions:
$(UL
  $(LI An unrecognized command-line argument is passed, and
       $(D std.getopt.config.passThrough) was not present.)
  $(LI A command-line option was not found, and
       $(D std.getopt.config.required) was present.)
)
*/
class GetOptException : Exception
{
    mixin basicExceptionCtors;
}

static assert(is(typeof(new GetOptException("message"))));
static assert(is(typeof(new GetOptException("message", Exception.init))));

/**
   Parse and remove command line options from a string array.

   Synopsis:

---------
import std.getopt;

string data = "file.dat";
int length = 24;
bool verbose;
enum Color { no, yes };
Color color;

void main(string[] args)
{
  auto helpInformation = getopt(
    args,
    "length",  &length,    // numeric
    "file",    &data,      // string
    "verbose", &verbose,   // flag
    "color", "Information about this color", &color);    // enum
  ...

  if (helpInformation.helpWanted)
  {
    defaultGetoptPrinter("Some information about the program.",
      helpInformation.options);
  }
}
---------

 The $(D getopt) function takes a reference to the command line
 (as received by $(D main)) as its first argument, and an
 unbounded number of pairs of strings and pointers. Each string is an
 option meant to "fill" the value referenced by the pointer to its
 right (the "bound" pointer). The option string in the call to
 $(D getopt) should not start with a dash.

 In all cases, the command-line options that were parsed and used by
 $(D getopt) are removed from $(D args). Whatever in the
 arguments did not look like an option is left in $(D args) for
 further processing by the program. Values that were unaffected by the
 options are not touched, so a common idiom is to initialize options
 to their defaults and then invoke $(D getopt). If a
 command-line argument is recognized as an option with a parameter and
 the parameter cannot be parsed properly (e.g., a number is expected
 but not present), a $(D ConvException) exception is thrown.
 If $(D std.getopt.config.passThrough) was not passed to $(D getopt)
 and an unrecognized command-line argument is found, a $(D GetOptException)
 is thrown.

 Depending on the type of the pointer being bound, $(D getopt)
 recognizes the following kinds of options:

 $(OL
    $(LI $(I Boolean options). A lone argument sets the option to $(D true).
    Additionally $(B true) or $(B false) can be set within the option separated
    with an "=" sign:

---------
  bool verbose = false, debugging = true;
  getopt(args, "verbose", &verbose, "debug", &debugging);
---------

    To set $(D verbose) to $(D true), invoke the program with either
    $(D --verbose) or $(D --verbose=true).

    To set $(D debugging) to $(D false), invoke the program with
    $(D --debugging=false).
    )

    $(LI $(I Numeric options.) If an option is bound to a numeric type, a
    number is expected as the next option, or right within the option separated
    with an "=" sign:

---------
  uint timeout;
  getopt(args, "timeout", &timeout);
---------

    To set $(D timeout) to $(D 5), invoke the program with either
    $(D --timeout=5) or $(D --timeout 5).
    )

    $(LI $(I Incremental options.) If an option name has a "+" suffix and is
    bound to a numeric type, then the option's value tracks the number of times
    the option occurred on the command line:

---------
  uint paranoid;
  getopt(args, "paranoid+", &paranoid);
---------

    Invoking the program with "--paranoid --paranoid --paranoid" will set $(D
    paranoid) to 3. Note that an incremental option never expects a parameter,
    e.g., in the command line "--paranoid 42 --paranoid", the "42" does not set
    $(D paranoid) to 42; instead, $(D paranoid) is set to 2 and "42" is not
    considered as part of the normal program arguments.
    )

    $(LI $(I Enum options.) If an option is bound to an enum, an enum symbol as
    a string is expected as the next option, or right within the option
    separated with an "=" sign:

---------
  enum Color { no, yes };
  Color color; // default initialized to Color.no
  getopt(args, "color", &color);
---------

    To set $(D color) to $(D Color.yes), invoke the program with either
    $(D --color=yes) or $(D --color yes).
    )

    $(LI $(I String options.) If an option is bound to a string, a string is
    expected as the next option, or right within the option separated with an
    "=" sign:

---------
string outputFile;
getopt(args, "output", &outputFile);
---------

    Invoking the program with "--output=myfile.txt" or "--output myfile.txt"
    will set $(D outputFile) to "myfile.txt". If you want to pass a string
    containing spaces, you need to use the quoting that is appropriate to your
    shell, e.g. --output='my file.txt'.
    )

    $(LI $(I Array options.) If an option is bound to an array, a new element
    is appended to the array each time the option occurs:

---------
string[] outputFiles;
getopt(args, "output", &outputFiles);
---------

    Invoking the program with "--output=myfile.txt --output=yourfile.txt" or
    "--output myfile.txt --output yourfile.txt" will set $(D outputFiles) to
    $(D [ "myfile.txt", "yourfile.txt" ]).

    Alternatively you can set $(LREF arraySep) as the element separator:

---------
string[] outputFiles;
arraySep = ",";  // defaults to "", separation by whitespace
getopt(args, "output", &outputFiles);
---------

    With the above code you can invoke the program with
    "--output=myfile.txt,yourfile.txt", or "--output myfile.txt,yourfile.txt".)

    $(LI $(I Hash options.) If an option is bound to an associative array, a
    string of the form "name=value" is expected as the next option, or right
    within the option separated with an "=" sign:

---------
double[string] tuningParms;
getopt(args, "tune", &tuningParms);
---------

    Invoking the program with e.g. "--tune=alpha=0.5 --tune beta=0.6" will set
    $(D tuningParms) to [ "alpha" : 0.5, "beta" : 0.6 ].

    Alternatively you can set $(LREF arraySep) as the element separator:

---------
double[string] tuningParms;
arraySep = ",";  // defaults to "", separation by whitespace
getopt(args, "tune", &tuningParms);
---------

    With the above code you can invoke the program with
    "--tune=alpha=0.5,beta=0.6", or "--tune alpha=0.5,beta=0.6".

    In general, the keys and values can be of any parsable types.
    )

    $(LI $(I Callback options.) An option can be bound to a function or
    delegate with the signature $(D void function()), $(D void function(string
    option)), $(D void function(string option, string value)), or their
    delegate equivalents.

    $(UL
        $(LI If the callback doesn't take any arguments, the callback is
        invoked whenever the option is seen.
        )

        $(LI If the callback takes one string argument, the option string
        (without the leading dash(es)) is passed to the callback.  After that,
        the option string is considered handled and removed from the options
        array.

---------
void main(string[] args)
{
  uint verbosityLevel = 1;
  void myHandler(string option)
  {
    if (option == "quiet")
    {
      verbosityLevel = 0;
    }
    else
    {
      assert(option == "verbose");
      verbosityLevel = 2;
    }
  }
  getopt(args, "verbose", &myHandler, "quiet", &myHandler);
}
---------

        )

        $(LI If the callback takes two string arguments, the option string is
        handled as an option with one argument, and parsed accordingly. The
        option and its value are passed to the callback. After that, whatever
        was passed to the callback is considered handled and removed from the
        list.

---------
int main(string[] args)
{
  uint verbosityLevel = 1;
  bool handlerFailed = false;
  void myHandler(string option, string value)
  {
    switch (value)
    {
      case "quiet": verbosityLevel = 0; break;
      case "verbose": verbosityLevel = 2; break;
      case "shouting": verbosityLevel = verbosityLevel.max; break;
      default :
        stderr.writeln("Unknown verbosity level ", value);
        handlerFailed = true;
        break;
    }
  }
  getopt(args, "verbosity", &myHandler);
  return handlerFailed ? 1 : 0;
}
---------
        )
    ))
)

Options_with_multiple_names:
Sometimes option synonyms are desirable, e.g. "--verbose",
"--loquacious", and "--garrulous" should have the same effect. Such
alternate option names can be included in the option specification,
using "|" as a separator:

---------
bool verbose;
getopt(args, "verbose|loquacious|garrulous", &verbose);
---------

Case:
By default options are case-insensitive. You can change that behavior
by passing $(D getopt) the $(D caseSensitive) directive like this:

---------
bool foo, bar;
getopt(args,
    std.getopt.config.caseSensitive,
    "foo", &foo,
    "bar", &bar);
---------

In the example above, "--foo" and "--bar" are recognized, but "--Foo", "--Bar",
"--FOo", "--bAr", etc. are rejected.
The directive is active until the end of $(D getopt), or until the
converse directive $(D caseInsensitive) is encountered:

---------
bool foo, bar;
getopt(args,
    std.getopt.config.caseSensitive,
    "foo", &foo,
    std.getopt.config.caseInsensitive,
    "bar", &bar);
---------

The option "--Foo" is rejected due to $(D
std.getopt.config.caseSensitive), but not "--Bar", "--bAr"
etc. because the directive $(D
std.getopt.config.caseInsensitive) turned sensitivity off before
option "bar" was parsed.

Short_versus_long_options:
Traditionally, programs accepted single-letter options preceded by
only one dash (e.g. $(D -t)). $(D getopt) accepts such parameters
seamlessly. When used with a double-dash (e.g. $(D --t)), a
single-letter option behaves the same as a multi-letter option. When
used with a single dash, a single-letter option is accepted. If the
option has a parameter, that must be "stuck" to the option without
any intervening space or "=":

---------
uint timeout;
getopt(args, "timeout|t", &timeout);
---------

To set $(D timeout) to $(D 5), use either of the following: $(D --timeout=5),
$(D --timeout 5), $(D --t=5), $(D --t 5), or $(D -t5). Forms such as $(D -t 5)
and $(D -timeout=5) will be not accepted.

For more details about short options, refer also to the next section.

Bundling:
Single-letter options can be bundled together, i.e. "-abc" is the same as
$(D "-a -b -c"). By default, this option is turned off. You can turn it on
with the $(D std.getopt.config.bundling) directive:

---------
bool foo, bar;
getopt(args,
    std.getopt.config.bundling,
    "foo|f", &foo,
    "bar|b", &bar);
---------

In case you want to only enable bundling for some of the parameters,
bundling can be turned off with $(D std.getopt.config.noBundling).

Required:
An option can be marked as required. If that option is not present in the
arguments an exception will be thrown.

---------
bool foo, bar;
getopt(args,
    std.getopt.config.required,
    "foo|f", &foo,
    "bar|b", &bar);
---------

Only the option directly following $(D std.getopt.config.required) is
required.

Passing_unrecognized_options_through:
If an application needs to do its own processing of whichever arguments
$(D getopt) did not understand, it can pass the
$(D std.getopt.config.passThrough) directive to $(D getopt):

---------
bool foo, bar;
getopt(args,
    std.getopt.config.passThrough,
    "foo", &foo,
    "bar", &bar);
---------

An unrecognized option such as "--baz" will be found untouched in
$(D args) after $(D getopt) returns.

Help_Information_Generation:
If an option string is followed by another string, this string serves as a
description for this option. The $(D getopt) function returns a struct of type
$(D GetoptResult). This return value contains information about all passed options
as well a $(D bool GetoptResult.helpWanted) flag indicating whether information
about these options was requested. The $(D getopt) function always adds an option for
`--help|-h` to set the flag if the option is seen on the command line.

Options_Terminator:
A lone double-dash terminates $(D getopt) gathering. It is used to
separate program options from other parameters (e.g., options to be passed
to another program). Invoking the example above with $(D "--foo -- --bar")
parses foo but leaves "--bar" in $(D args). The double-dash itself is
removed from the argument array unless the $(D std.getopt.config.keepEndOfOptions)
directive is given.
*/
GetoptResult getopt(T...)(ref string[] args, T opts)
{
    import std.exception : enforce;
    enforce(args.length,
            "Invalid arguments string passed: program name missing");
    configuration cfg;
    GetoptResult rslt;

    GetOptException excep;
    void[][string] visitedLongOpts, visitedShortOpts;
    getoptImpl(args, cfg, rslt, excep, visitedLongOpts, visitedShortOpts, opts);

    if (!rslt.helpWanted && excep !is null)
    {
        throw excep;
    }

    return rslt;
}

///
@system unittest
{
    auto args = ["prog", "--foo", "-b"];

    bool foo;
    bool bar;
    auto rslt = getopt(args, "foo|f", "Some information about foo.", &foo, "bar|b",
        "Some help message about bar.", &bar);

    if (rslt.helpWanted)
    {
        defaultGetoptPrinter("Some information about the program.",
            rslt.options);
    }
}

/**
   Configuration options for $(D getopt).

   You can pass them to $(D getopt) in any position, except in between an option
   string and its bound pointer.
*/
enum config {
    /// Turn case sensitivity on
    caseSensitive,
    /// Turn case sensitivity off (default)
    caseInsensitive,
    /// Turn bundling on
    bundling,
    /// Turn bundling off (default)
    noBundling,
    /// Pass unrecognized arguments through
    passThrough,
    /// Signal unrecognized arguments as errors (default)
    noPassThrough,
    /// Stop at first argument that does not look like an option
    stopOnFirstNonOption,
    /// Do not erase the endOfOptions separator from args
    keepEndOfOptions,
    /// Make the next option a required option
    required
}

/** The result of the $(D getopt) function.

$(D helpWanted) is set if the option `--help` or `-h` was passed to the option parser.
*/
struct GetoptResult {
    bool helpWanted; /// Flag indicating if help was requested
    Option[] options; /// All possible options
}

/** Information about an option.
*/
struct Option {
    string optShort; /// The short symbol for this option
    string optLong; /// The long symbol for this option
    string help; /// The description of this option
    bool required; /// If a option is required, not passing it will result in an error
}

private pure Option splitAndGet(string opt) @trusted nothrow
{
    import std.array : split;
    auto sp = split(opt, "|");
    Option ret;
    if (sp.length > 1)
    {
        ret.optShort = "-" ~ (sp[0].length < sp[1].length ?
            sp[0] : sp[1]);
        ret.optLong = "--" ~ (sp[0].length > sp[1].length ?
            sp[0] : sp[1]);
    }
    else if (sp[0].length > 1)
    {
        ret.optLong = "--" ~ sp[0];
    }
    else
    {
        ret.optShort = "-" ~ sp[0];
    }

    return ret;
}

@safe unittest
{
    auto oshort = splitAndGet("f");
    assert(oshort.optShort == "-f");
    assert(oshort.optLong == "");

    auto olong = splitAndGet("foo");
    assert(olong.optShort == "");
    assert(olong.optLong == "--foo");

    auto oshortlong = splitAndGet("f|foo");
    assert(oshortlong.optShort == "-f");
    assert(oshortlong.optLong == "--foo");

    auto olongshort = splitAndGet("foo|f");
    assert(olongshort.optShort == "-f");
    assert(olongshort.optLong == "--foo");
}

/*
This function verifies that the variadic parameters passed in getOpt
follow this pattern:

  [config override], option, [description], receiver,

 - config override: a config value, optional
 - option:          a string or a char
 - description:     a string, optional
 - receiver:        a pointer or a callable
*/
private template optionValidator(A...)
{
    import std.format : format;
    import std.typecons : staticIota;

    enum fmt = "getopt validator: %s (at position %d)";
    enum isReceiver(T) = isPointer!T || (is(T == function)) || (is(T == delegate));
    enum isOptionStr(T) = isSomeString!T || isSomeChar!T;

    auto validator()
    {
        string msg;
        static if (A.length > 0)
        {
            static if (isReceiver!(A[0]))
            {
                msg = format(fmt, "first argument must be a string or a config", 0);
            }
            else static if (!isOptionStr!(A[0]) && !is(A[0] == config))
            {
                msg = format(fmt, "invalid argument type: " ~ A[0].stringof, 0);
            }
            else foreach (i; staticIota!(1, A.length))
            {
                static if (!isReceiver!(A[i]) && !isOptionStr!(A[i]) &&
                    !(is(A[i] == config)))
                {
                    msg = format(fmt, "invalid argument type: " ~ A[i].stringof, i);
                    break;
                }
                else static if (isReceiver!(A[i]) && !isOptionStr!(A[i-1]))
                {
                    msg = format(fmt, "a receiver can not be preceeded by a receiver", i);
                    break;
                }
                else static if (i > 1 && isOptionStr!(A[i]) && isOptionStr!(A[i-1])
                    && isSomeString!(A[i-2]))
                {
                    msg = format(fmt, "a string can not be preceeded by two strings", i);
                    break;
                }
            }
            static if (!isReceiver!(A[$-1]) && !is(A[$-1] == config))
            {
                msg = format(fmt, "last argument must be a receiver or a config",
                    A.length -1);
            }
        }
        return msg;
    }
    enum message = validator;
    alias optionValidator = message;
}

@safe pure unittest
{
    alias P = void*;
    alias S = string;
    alias A = char;
    alias C = config;
    alias F = void function();

    static assert(optionValidator!(S,P) == "");
    static assert(optionValidator!(S,F) == "");
    static assert(optionValidator!(A,P) == "");
    static assert(optionValidator!(A,F) == "");

    static assert(optionValidator!(C,S,P) == "");
    static assert(optionValidator!(C,S,F) == "");
    static assert(optionValidator!(C,A,P) == "");
    static assert(optionValidator!(C,A,F) == "");

    static assert(optionValidator!(C,S,S,P) == "");
    static assert(optionValidator!(C,S,S,F) == "");
    static assert(optionValidator!(C,A,S,P) == "");
    static assert(optionValidator!(C,A,S,F) == "");

    static assert(optionValidator!(C,S,S,P) == "");
    static assert(optionValidator!(C,S,S,P,C,S,F) == "");
    static assert(optionValidator!(C,S,P,C,S,S,F) == "");

    static assert(optionValidator!(C,A,P,A,S,F) == "");
    static assert(optionValidator!(C,A,P,C,A,S,F) == "");

    static assert(optionValidator!(P,S,S) != "");
    static assert(optionValidator!(P,P,S) != "");
    static assert(optionValidator!(P,F,S,P) != "");
    static assert(optionValidator!(C,C,S) != "");
    static assert(optionValidator!(S,S,P,S,S,P,S) != "");
    static assert(optionValidator!(S,S,P,P) != "");
    static assert(optionValidator!(S,S,S,P) != "");

    static assert(optionValidator!(C,A,S,P,C,A,F) == "");
    static assert(optionValidator!(C,A,P,C,A,S,F) == "");
}

@system unittest // bugzilla 15914
{
    bool opt;
    string[] args = ["program", "-a"];
    getopt(args, config.passThrough, 'a', &opt);
    assert(opt);
    opt = false;
    args = ["program", "-a"];
    getopt(args, 'a', &opt);
    assert(opt);
    opt = false;
    args = ["program", "-a"];
    getopt(args, 'a', "help string", &opt);
    assert(opt);
    opt = false;
    args = ["program", "-a"];
    getopt(args, config.caseSensitive, 'a', "help string", &opt);
    assert(opt);

    assertThrown(getopt(args, "", "forgot to put a string", &opt));
}

private void getoptImpl(T...)(ref string[] args, ref configuration cfg,
    ref GetoptResult rslt, ref GetOptException excep,
    void[][string] visitedLongOpts, void[][string] visitedShortOpts, T opts)
{
    enum validationMessage = optionValidator!T;
    static assert(validationMessage == "", validationMessage);

    import std.algorithm.mutation : remove;
    import std.conv : to;
    static if (opts.length)
    {
        static if (is(typeof(opts[0]) : config))
        {
            // it's a configuration flag, act on it
            setConfig(cfg, opts[0]);
            return getoptImpl(args, cfg, rslt, excep, visitedLongOpts,
                visitedShortOpts, opts[1 .. $]);
        }
        else
        {
            // it's an option string
            auto option = to!string(opts[0]);
            if (option.length == 0)
            {
                excep = new GetOptException("An option name may not be an empty string", excep);
                return;
            }
            Option optionHelp = splitAndGet(option);
            optionHelp.required = cfg.required;

            if (optionHelp.optLong.length)
            {
                assert(optionHelp.optLong !in visitedLongOpts,
                    "Long option " ~ optionHelp.optLong ~ " is multiply defined");

                visitedLongOpts[optionHelp.optLong] = [];
            }

            if (optionHelp.optShort.length)
            {
                assert(optionHelp.optShort !in visitedShortOpts,
                    "Short option " ~ optionHelp.optShort
                    ~ " is multiply defined");

                visitedShortOpts[optionHelp.optShort] = [];
            }

            static if (is(typeof(opts[1]) : string))
            {
                auto receiver = opts[2];
                optionHelp.help = opts[1];
                immutable lowSliceIdx = 3;
            }
            else
            {
                auto receiver = opts[1];
                immutable lowSliceIdx = 2;
            }

            rslt.options ~= optionHelp;

            bool incremental;
            // Handle options of the form --blah+
            if (option.length && option[$ - 1] == autoIncrementChar)
            {
                option = option[0 .. $ - 1];
                incremental = true;
            }

            bool optWasHandled = handleOption(option, receiver, args, cfg, incremental);

            if (cfg.required && !optWasHandled)
            {
                excep = new GetOptException("Required option "
                    ~ option ~ " was not supplied", excep);
            }
            cfg.required = false;

            getoptImpl(args, cfg, rslt, excep, visitedLongOpts,
                visitedShortOpts, opts[lowSliceIdx .. $]);
        }
    }
    else
    {
        // no more options to look for, potentially some arguments left
        for (size_t i = 1; i < args.length;)
        {
            auto a = args[i];
            if (endOfOptions.length && a == endOfOptions)
            {
                // Consume the "--" if keepEndOfOptions is not specified
                if (!cfg.keepEndOfOptions)
                    args = args.remove(i);
                break;
            }
            if (!a.length || a[0] != optionChar)
            {
                // not an option
                if (cfg.stopOnFirstNonOption) break;
                ++i;
                continue;
            }
            if (a == "--help" || a == "-h")
            {
                rslt.helpWanted = true;
                args = args.remove(i);
                continue;
            }
            if (!cfg.passThrough)
            {
                throw new GetOptException("Unrecognized option "~a, excep);
            }
            ++i;
        }

        Option helpOpt;
        helpOpt.optShort = "-h";
        helpOpt.optLong = "--help";
        helpOpt.help = "This help information.";
        rslt.options ~= helpOpt;
    }
}

private bool handleOption(R)(string option, R receiver, ref string[] args,
    ref configuration cfg, bool incremental)
{
    import std.algorithm.iteration : map, splitter;
    import std.ascii : isAlpha;
    import std.conv : text, to;
    // Scan arguments looking for a match for this option
    bool ret = false;
    for (size_t i = 1; i < args.length; )
    {
        auto a = args[i];
        if (endOfOptions.length && a == endOfOptions) break;
        if (cfg.stopOnFirstNonOption && (!a.length || a[0] != optionChar))
        {
            // first non-option is end of options
            break;
        }
        // Unbundle bundled arguments if necessary
        if (cfg.bundling && a.length > 2 && a[0] == optionChar &&
                a[1] != optionChar)
        {
            string[] expanded;
            foreach (j, dchar c; a[1 .. $])
            {
                // If the character is not alpha, stop right there. This allows
                // e.g. -j100 to work as "pass argument 100 to option -j".
                if (!isAlpha(c))
                {
                    if (c == '=')
                        j++;
                    expanded ~= a[j + 1 .. $];
                    break;
                }
                expanded ~= text(optionChar, c);
            }
            args = args[0 .. i] ~ expanded ~ args[i + 1 .. $];
            continue;
        }

        string val;
        if (!optMatch(a, option, val, cfg))
        {
            ++i;
            continue;
        }

        ret = true;

        // found it
        // from here on, commit to eat args[i]
        // (and potentially args[i + 1] too, but that comes later)
        args = args[0 .. i] ~ args[i + 1 .. $];

        static if (is(typeof(*receiver) == bool))
        {
            if (val.length)
            {
                // parse '--b=true/false'
                *receiver = to!(typeof(*receiver))(val);
            }
            else
            {
                // no argument means set it to true
                *receiver = true;
            }
        }
        else
        {
            import std.exception : enforce;
            // non-boolean option, which might include an argument
            //enum isCallbackWithOneParameter = is(typeof(receiver("")) : void);
            enum isCallbackWithLessThanTwoParameters =
                (is(typeof(receiver) == delegate) || is(typeof(*receiver) == function)) &&
                !is(typeof(receiver("", "")));
            if (!isCallbackWithLessThanTwoParameters && !(val.length) && !incremental)
            {
                // Eat the next argument too.  Check to make sure there's one
                // to be eaten first, though.
                enforce(i < args.length,
                    "Missing value for argument " ~ a ~ ".");
                val = args[i];
                args = args[0 .. i] ~ args[i + 1 .. $];
            }
            static if (is(typeof(*receiver) == enum))
            {
                *receiver = to!(typeof(*receiver))(val);
            }
            else static if (is(typeof(*receiver) : real))
            {
                // numeric receiver
                if (incremental) ++*receiver;
                else *receiver = to!(typeof(*receiver))(val);
            }
            else static if (is(typeof(*receiver) == string))
            {
                // string receiver
                *receiver = to!(typeof(*receiver))(val);
            }
            else static if (is(typeof(receiver) == delegate) ||
                            is(typeof(*receiver) == function))
            {
                static if (is(typeof(receiver("", "")) : void))
                {
                    // option with argument
                    receiver(option, val);
                }
                else static if (is(typeof(receiver("")) : void))
                {
                    static assert(is(typeof(receiver("")) : void));
                    // boolean-style receiver
                    receiver(option);
                }
                else
                {
                    static assert(is(typeof(receiver()) : void));
                    // boolean-style receiver without argument
                    receiver();
                }
            }
            else static if (isArray!(typeof(*receiver)))
            {
                // array receiver
                import std.range : ElementEncodingType;
                alias E = ElementEncodingType!(typeof(*receiver));

                if (arraySep == "")
                {
                    *receiver ~= to!E(val);
                }
                else
                {
                    foreach (elem; val.splitter(arraySep).map!(a => to!E(a))())
                        *receiver ~= elem;
                }
            }
            else static if (isAssociativeArray!(typeof(*receiver)))
            {
                // hash receiver
                alias K = typeof(receiver.keys[0]);
                alias V = typeof(receiver.values[0]);

                import std.range : only;
                import std.string : indexOf;
                import std.typecons : Tuple, tuple;

                static Tuple!(K, V) getter(string input)
                {
                    auto j = indexOf(input, assignChar);
                    enforce!GetOptException(j != -1, "Could not find '"
                        ~ to!string(assignChar) ~ "' in argument '" ~ input ~ "'.");
                    auto key = input[0 .. j];
                    auto value = input[j + 1 .. $];
                    return tuple(to!K(key), to!V(value));
                }

                static void setHash(Range)(R receiver, Range range)
                {
                    foreach (k, v; range.map!getter)
                        (*receiver)[k] = v;
                }

                if (arraySep == "")
                    setHash(receiver, val.only);
                else
                    setHash(receiver, val.splitter(arraySep));
            }
            else
                static assert(false, "getopt does not know how to handle the type " ~ typeof(receiver).stringof);
        }
    }

    return ret;
}

// 17574
@system unittest
{
    import std.algorithm.searching : startsWith;

    try
    {
        string[string] mapping;
        immutable as = arraySep;
        arraySep = ",";
        scope (exit)
            arraySep = as;
        string[] args = ["testProgram", "-m", "a=b,c=\"d,e,f\""];
        args.getopt("m", &mapping);
        assert(false, "Exception not thrown");
    }
    catch (GetOptException goe)
        assert(goe.msg.startsWith("Could not find"));
}

// 5316 - arrays with arraySep
@system unittest
{
    import std.conv;

    arraySep = ",";
    scope (exit) arraySep = "";

    string[] names;
    auto args = ["program.name", "-nfoo,bar,baz"];
    getopt(args, "name|n", &names);
    assert(names == ["foo", "bar", "baz"], to!string(names));

    names = names.init;
    args = ["program.name", "-n", "foo,bar,baz"];
    getopt(args, "name|n", &names);
    assert(names == ["foo", "bar", "baz"], to!string(names));

    names = names.init;
    args = ["program.name", "--name=foo,bar,baz"];
    getopt(args, "name|n", &names);
    assert(names == ["foo", "bar", "baz"], to!string(names));

    names = names.init;
    args = ["program.name", "--name", "foo,bar,baz"];
    getopt(args, "name|n", &names);
    assert(names == ["foo", "bar", "baz"], to!string(names));
}

// 5316 - associative arrays with arraySep
@system unittest
{
    import std.conv;

    arraySep = ",";
    scope (exit) arraySep = "";

    int[string] values;
    values = values.init;
    auto args = ["program.name", "-vfoo=0,bar=1,baz=2"];
    getopt(args, "values|v", &values);
    assert(values == ["foo":0, "bar":1, "baz":2], to!string(values));

    values = values.init;
    args = ["program.name", "-v", "foo=0,bar=1,baz=2"];
    getopt(args, "values|v", &values);
    assert(values == ["foo":0, "bar":1, "baz":2], to!string(values));

    values = values.init;
    args = ["program.name", "--values=foo=0,bar=1,baz=2"];
    getopt(args, "values|t", &values);
    assert(values == ["foo":0, "bar":1, "baz":2], to!string(values));

    values = values.init;
    args = ["program.name", "--values", "foo=0,bar=1,baz=2"];
    getopt(args, "values|v", &values);
    assert(values == ["foo":0, "bar":1, "baz":2], to!string(values));
}

/**
   The option character (default '-').

   Defaults to '-' but it can be assigned to prior to calling $(D getopt).
 */
dchar optionChar = '-';

/**
   The string that conventionally marks the end of all options (default '--').

   Defaults to "--" but can be assigned to prior to calling $(D getopt). Assigning an
   empty string to $(D endOfOptions) effectively disables it.
 */
string endOfOptions = "--";

/**
   The assignment character used in options with parameters (default '=').

   Defaults to '=' but can be assigned to prior to calling $(D getopt).
 */
dchar assignChar = '=';

/**
   The string used to separate the elements of an array or associative array
   (default is "" which means the elements are separated by whitespace).

   Defaults to "" but can be assigned to prior to calling $(D getopt).
 */
string arraySep = "";

private enum autoIncrementChar = '+';

private struct configuration
{
    import std.bitmanip : bitfields;
    mixin(bitfields!(
                bool, "caseSensitive",  1,
                bool, "bundling", 1,
                bool, "passThrough", 1,
                bool, "stopOnFirstNonOption", 1,
                bool, "keepEndOfOptions", 1,
                bool, "required", 1,
                ubyte, "", 2));
}

private bool optMatch(string arg, string optPattern, ref string value,
    configuration cfg) @safe
{
    import std.array : split;
    import std.string : indexOf;
    import std.uni : toUpper;
    //writeln("optMatch:\n  ", arg, "\n  ", optPattern, "\n  ", value);
    //scope(success) writeln("optMatch result: ", value);
    if (arg.length < 2 || arg[0] != optionChar) return false;
    // yank the leading '-'
    arg = arg[1 .. $];
    immutable isLong = arg.length > 1 && arg[0] == optionChar;
    //writeln("isLong: ", isLong);
    // yank the second '-' if present
    if (isLong) arg = arg[1 .. $];
    immutable eqPos = indexOf(arg, assignChar);
    if (isLong && eqPos >= 0)
    {
        // argument looks like --opt=value
        value = arg[eqPos + 1 .. $];
        arg = arg[0 .. eqPos];
    }
    else
    {
        if (!isLong && eqPos == 1)
        {
            // argument looks like -o=value
            value = arg[2 .. $];
            arg = arg[0 .. 1];
        }
        else
        if (!isLong && !cfg.bundling)
        {
            // argument looks like -ovalue and there's no bundling
            value = arg[1 .. $];
            arg = arg[0 .. 1];
        }
        else
        {
            // argument looks like --opt, or -oxyz with bundling
            value = null;
        }
    }
    //writeln("Arg: ", arg, " pattern: ", optPattern, " value: ", value);
    // Split the option
    const variants = split(optPattern, "|");
    foreach (v ; variants)
    {
        //writeln("Trying variant: ", v, " against ", arg);
        if (arg == v || !cfg.caseSensitive && toUpper(arg) == toUpper(v))
            return true;
        if (cfg.bundling && !isLong && v.length == 1
                && indexOf(arg, v) >= 0)
        {
            //writeln("success");
            return true;
        }
    }
    return false;
}

private void setConfig(ref configuration cfg, config option) @safe pure nothrow @nogc
{
    final switch (option)
    {
    case config.caseSensitive: cfg.caseSensitive = true; break;
    case config.caseInsensitive: cfg.caseSensitive = false; break;
    case config.bundling: cfg.bundling = true; break;
    case config.noBundling: cfg.bundling = false; break;
    case config.passThrough: cfg.passThrough = true; break;
    case config.noPassThrough: cfg.passThrough = false; break;
    case config.required: cfg.required = true; break;
    case config.stopOnFirstNonOption:
        cfg.stopOnFirstNonOption = true; break;
    case config.keepEndOfOptions:
        cfg.keepEndOfOptions = true; break;
    }
}

@system unittest
{
    import std.conv;
    import std.math;

    uint paranoid = 2;
    string[] args = ["program.name", "--paranoid", "--paranoid", "--paranoid"];
    getopt(args, "paranoid+", &paranoid);
    assert(paranoid == 5, to!(string)(paranoid));

    enum Color { no, yes }
    Color color;
    args = ["program.name", "--color=yes",];
    getopt(args, "color", &color);
    assert(color, to!(string)(color));

    color = Color.no;
    args = ["program.name", "--color", "yes",];
    getopt(args, "color", &color);
    assert(color, to!(string)(color));

    string data = "file.dat";
    int length = 24;
    bool verbose = false;
    args = ["program.name", "--length=5", "--file", "dat.file", "--verbose"];
    getopt(
        args,
        "length",  &length,
        "file",    &data,
        "verbose", &verbose);
    assert(args.length == 1);
    assert(data == "dat.file");
    assert(length == 5);
    assert(verbose);

    //
    string[] outputFiles;
    args = ["program.name", "--output=myfile.txt", "--output", "yourfile.txt"];
    getopt(args, "output", &outputFiles);
    assert(outputFiles.length == 2
           && outputFiles[0] == "myfile.txt" && outputFiles[1] == "yourfile.txt");

    outputFiles = [];
    arraySep = ",";
    args = ["program.name", "--output", "myfile.txt,yourfile.txt"];
    getopt(args, "output", &outputFiles);
    assert(outputFiles.length == 2
           && outputFiles[0] == "myfile.txt" && outputFiles[1] == "yourfile.txt");
    arraySep = "";

    foreach (testArgs;
        [["program.name", "--tune=alpha=0.5", "--tune", "beta=0.6"],
         ["program.name", "--tune=alpha=0.5,beta=0.6"],
         ["program.name", "--tune", "alpha=0.5,beta=0.6"]])
    {
        arraySep = ",";
        double[string] tuningParms;
        getopt(testArgs, "tune", &tuningParms);
        assert(testArgs.length == 1);
        assert(tuningParms.length == 2);
        assert(approxEqual(tuningParms["alpha"], 0.5));
        assert(approxEqual(tuningParms["beta"], 0.6));
        arraySep = "";
    }

    uint verbosityLevel = 1;
    void myHandler(string option)
    {
        if (option == "quiet")
        {
            verbosityLevel = 0;
        }
        else
        {
            assert(option == "verbose");
            verbosityLevel = 2;
        }
    }
    args = ["program.name", "--quiet"];
    getopt(args, "verbose", &myHandler, "quiet", &myHandler);
    assert(verbosityLevel == 0);
    args = ["program.name", "--verbose"];
    getopt(args, "verbose", &myHandler, "quiet", &myHandler);
    assert(verbosityLevel == 2);

    verbosityLevel = 1;
    void myHandler2(string option, string value)
    {
        assert(option == "verbose");
        verbosityLevel = 2;
    }
    args = ["program.name", "--verbose", "2"];
    getopt(args, "verbose", &myHandler2);
    assert(verbosityLevel == 2);

    verbosityLevel = 1;
    void myHandler3()
    {
        verbosityLevel = 2;
    }
    args = ["program.name", "--verbose"];
    getopt(args, "verbose", &myHandler3);
    assert(verbosityLevel == 2);

    bool foo, bar;
    args = ["program.name", "--foo", "--bAr"];
    getopt(args,
        std.getopt.config.caseSensitive,
        std.getopt.config.passThrough,
        "foo", &foo,
        "bar", &bar);
    assert(args[1] == "--bAr");

    // test stopOnFirstNonOption

    args = ["program.name", "--foo", "nonoption", "--bar"];
    foo = bar = false;
    getopt(args,
        std.getopt.config.stopOnFirstNonOption,
        "foo", &foo,
        "bar", &bar);
    assert(foo && !bar && args[1] == "nonoption" && args[2] == "--bar");

    args = ["program.name", "--foo", "nonoption", "--zab"];
    foo = bar = false;
    getopt(args,
        std.getopt.config.stopOnFirstNonOption,
        "foo", &foo,
        "bar", &bar);
    assert(foo && !bar && args[1] == "nonoption" && args[2] == "--zab");

    args = ["program.name", "--fb1", "--fb2=true", "--tb1=false"];
    bool fb1, fb2;
    bool tb1 = true;
    getopt(args, "fb1", &fb1, "fb2", &fb2, "tb1", &tb1);
    assert(fb1 && fb2 && !tb1);

    // test keepEndOfOptions

    args = ["program.name", "--foo", "nonoption", "--bar", "--", "--baz"];
    getopt(args,
        std.getopt.config.keepEndOfOptions,
        "foo", &foo,
        "bar", &bar);
    assert(args == ["program.name", "nonoption", "--", "--baz"]);

    // Ensure old behavior without the keepEndOfOptions

    args = ["program.name", "--foo", "nonoption", "--bar", "--", "--baz"];
    getopt(args,
        "foo", &foo,
        "bar", &bar);
    assert(args == ["program.name", "nonoption", "--baz"]);

    // test function callbacks

    static class MyEx : Exception
    {
        this() { super(""); }
        this(string option) { this(); this.option = option; }
        this(string option, string value) { this(option); this.value = value; }

        string option;
        string value;
    }

    static void myStaticHandler1() { throw new MyEx(); }
    args = ["program.name", "--verbose"];
    try { getopt(args, "verbose", &myStaticHandler1); assert(0); }
    catch (MyEx ex) { assert(ex.option is null && ex.value is null); }

    static void myStaticHandler2(string option) { throw new MyEx(option); }
    args = ["program.name", "--verbose"];
    try { getopt(args, "verbose", &myStaticHandler2); assert(0); }
    catch (MyEx ex) { assert(ex.option == "verbose" && ex.value is null); }

    static void myStaticHandler3(string option, string value) { throw new MyEx(option, value); }
    args = ["program.name", "--verbose", "2"];
    try { getopt(args, "verbose", &myStaticHandler3); assert(0); }
    catch (MyEx ex) { assert(ex.option == "verbose" && ex.value == "2"); }
}

@safe unittest // @safe std.getopt.config option use
{
    long x = 0;
    string[] args = ["program", "--inc-x", "--inc-x"];
    getopt(args,
           std.getopt.config.caseSensitive,
           "inc-x", "Add one to x", delegate void() { x++; });
    assert(x == 2);
}

@system unittest
{
    // From bugzilla 2142
    bool f_linenum, f_filename;
    string[] args = [ "", "-nl" ];
    getopt
        (
            args,
            std.getopt.config.bundling,
            //std.getopt.config.caseSensitive,
            "linenum|l", &f_linenum,
            "filename|n", &f_filename
        );
    assert(f_linenum);
    assert(f_filename);
}

@system unittest
{
    // From bugzilla 6887
    string[] p;
    string[] args = ["", "-pa"];
    getopt(args, "p", &p);
    assert(p.length == 1);
    assert(p[0] == "a");
}

@system unittest
{
    // From bugzilla 6888
    int[string] foo;
    auto args = ["", "-t", "a=1"];
    getopt(args, "t", &foo);
    assert(foo == ["a":1]);
}

@system unittest
{
    // From bugzilla 9583
    int opt;
    auto args = ["prog", "--opt=123", "--", "--a", "--b", "--c"];
    getopt(args, "opt", &opt);
    assert(args == ["prog", "--a", "--b", "--c"]);
}

@system unittest
{
    string foo, bar;
    auto args = ["prog", "-thello", "-dbar=baz"];
    getopt(args, "t", &foo, "d", &bar);
    assert(foo == "hello");
    assert(bar == "bar=baz");

    // From bugzilla 5762
    string a;
    args = ["prog", "-a-0x12"];
    getopt(args, config.bundling, "a|addr", &a);
    assert(a == "-0x12", a);
    args = ["prog", "--addr=-0x12"];
    getopt(args, config.bundling, "a|addr", &a);
    assert(a == "-0x12");

    // From https://d.puremagic.com/issues/show_bug.cgi?id=11764
    args = ["main", "-test"];
    bool opt;
    args.getopt(config.passThrough, "opt", &opt);
    assert(args == ["main", "-test"]);

    // From https://issues.dlang.org/show_bug.cgi?id=15220
    args = ["main", "-o=str"];
    string o;
    args.getopt("o", &o);
    assert(o == "str");

    args = ["main", "-o=str"];
    o = null;
    args.getopt(config.bundling, "o", &o);
    assert(o == "str");
}

@system unittest // 5228
{
    import std.conv;
    import std.exception;

    auto args = ["prog", "--foo=bar"];
    int abc;
    assertThrown!GetOptException(getopt(args, "abc", &abc));

    args = ["prog", "--abc=string"];
    assertThrown!ConvException(getopt(args, "abc", &abc));
}

@system unittest // From bugzilla 7693
{
    import std.exception;

    enum Foo {
        bar,
        baz
    }

    auto args = ["prog", "--foo=barZZZ"];
    Foo foo;
    assertThrown(getopt(args, "foo", &foo));
    args = ["prog", "--foo=bar"];
    assertNotThrown(getopt(args, "foo", &foo));
    args = ["prog", "--foo", "barZZZ"];
    assertThrown(getopt(args, "foo", &foo));
    args = ["prog", "--foo", "baz"];
    assertNotThrown(getopt(args, "foo", &foo));
}

@system unittest // same bug as 7693 only for bool
{
    import std.exception;

    auto args = ["prog", "--foo=truefoobar"];
    bool foo;
    assertThrown(getopt(args, "foo", &foo));
    args = ["prog", "--foo"];
    getopt(args, "foo", &foo);
    assert(foo);
}

@system unittest
{
    bool foo;
    auto args = ["prog", "--foo"];
    getopt(args, "foo", &foo);
    assert(foo);
}

@system unittest
{
    bool foo;
    bool bar;
    auto args = ["prog", "--foo", "-b"];
    getopt(args, config.caseInsensitive,"foo|f", "Some foo", &foo,
        config.caseSensitive, "bar|b", "Some bar", &bar);
    assert(foo);
    assert(bar);
}

@system unittest
{
    bool foo;
    bool bar;
    auto args = ["prog", "-b", "--foo", "-z"];
    getopt(args, config.caseInsensitive, config.required, "foo|f", "Some foo",
        &foo, config.caseSensitive, "bar|b", "Some bar", &bar,
        config.passThrough);
    assert(foo);
    assert(bar);
}

@system unittest
{
    import std.exception;

    bool foo;
    bool bar;
    auto args = ["prog", "-b", "-z"];
    assertThrown(getopt(args, config.caseInsensitive, config.required, "foo|f",
        "Some foo", &foo, config.caseSensitive, "bar|b", "Some bar", &bar,
        config.passThrough));
}

@system unittest
{
    import std.exception;

    bool foo;
    bool bar;
    auto args = ["prog", "--foo", "-z"];
    assertNotThrown(getopt(args, config.caseInsensitive, config.required,
        "foo|f", "Some foo", &foo, config.caseSensitive, "bar|b", "Some bar",
        &bar, config.passThrough));
    assert(foo);
    assert(!bar);
}

@system unittest
{
    bool foo;
    auto args = ["prog", "-f"];
    auto r = getopt(args, config.caseInsensitive, "help|f", "Some foo", &foo);
    assert(foo);
    assert(!r.helpWanted);
}

@safe unittest // implicit help option without config.passThrough
{
    string[] args = ["program", "--help"];
    auto r = getopt(args);
    assert(r.helpWanted);
}

// Issue 13316 - std.getopt: implicit help option breaks the next argument
@system unittest
{
    string[] args = ["program", "--help", "--", "something"];
    getopt(args);
    assert(args == ["program", "something"]);

    args = ["program", "--help", "--"];
    getopt(args);
    assert(args == ["program"]);

    bool b;
    args = ["program", "--help", "nonoption", "--option"];
    getopt(args, config.stopOnFirstNonOption, "option", &b);
    assert(args == ["program", "nonoption", "--option"]);
}

// Issue 13317 - std.getopt: endOfOptions broken when it doesn't look like an option
@system unittest
{
    auto endOfOptionsBackup = endOfOptions;
    scope(exit) endOfOptions = endOfOptionsBackup;
    endOfOptions = "endofoptions";
    string[] args = ["program", "endofoptions", "--option"];
    bool b = false;
    getopt(args, "option", &b);
    assert(!b);
    assert(args == ["program", "--option"]);
}

/** This function prints the passed $(D Option)s and text in an aligned manner on $(D stdout).

The passed text will be printed first, followed by a newline, then the short
and long version of every option will be printed. The short and long version
will be aligned to the longest option of every $(D Option) passed. If the option
is required, then "Required:" will be printed after the long version of the
$(D Option). If a help message is present it will be printed next. The format is
illustrated by this code:

------------
foreach (it; opt)
{
    writefln("%*s %*s%s%s", lengthOfLongestShortOption, it.optShort,
        lengthOfLongestLongOption, it.optLong,
        it.required ? " Required: " : " ", it.help);
}
------------

Params:
    text = The text to printed at the beginning of the help output.
    opt = The $(D Option) extracted from the $(D getopt) parameter.
*/
void defaultGetoptPrinter(string text, Option[] opt)
{
    import std.stdio : stdout;

    defaultGetoptFormatter(stdout.lockingTextWriter(), text, opt);
}

/** This function writes the passed text and $(D Option) into an output range
in the manner described in the documentation of function
$(D defaultGetoptPrinter).

Params:
    output = The output range used to write the help information.
    text = The text to print at the beginning of the help output.
    opt = The $(D Option) extracted from the $(D getopt) parameter.
*/
void defaultGetoptFormatter(Output)(Output output, string text, Option[] opt)
{
    import std.algorithm.comparison : min, max;
    import std.format : formattedWrite;

    output.formattedWrite("%s\n", text);

    size_t ls, ll;
    bool hasRequired = false;
    foreach (it; opt)
    {
        ls = max(ls, it.optShort.length);
        ll = max(ll, it.optLong.length);

        hasRequired = hasRequired || it.required;
    }

    string re = " Required: ";

    foreach (it; opt)
    {
        output.formattedWrite("%*s %*s%*s%s\n", ls, it.optShort, ll, it.optLong,
            hasRequired ? re.length : 1, it.required ? re : " ", it.help);
    }
}

@system unittest
{
    import std.conv;

    import std.array;
    import std.string;
    bool a;
    auto args = ["prog", "--foo"];
    auto t = getopt(args, "foo|f", "Help", &a);
    string s;
    auto app = appender!string();
    defaultGetoptFormatter(app, "Some Text", t.options);

    string helpMsg = app.data;
    //writeln(helpMsg);
    assert(helpMsg.length);
    assert(helpMsg.count("\n") == 3, to!string(helpMsg.count("\n")) ~ " "
        ~ helpMsg);
    assert(helpMsg.indexOf("--foo") != -1);
    assert(helpMsg.indexOf("-f") != -1);
    assert(helpMsg.indexOf("-h") != -1);
    assert(helpMsg.indexOf("--help") != -1);
    assert(helpMsg.indexOf("Help") != -1);

    string wanted = "Some Text\n-f  --foo Help\n-h --help This help "
        ~ "information.\n";
    assert(wanted == helpMsg);
}

@system unittest
{
    import std.array ;
    import std.conv;
    import std.string;
    bool a;
    auto args = ["prog", "--foo"];
    auto t = getopt(args, config.required, "foo|f", "Help", &a);
    string s;
    auto app = appender!string();
    defaultGetoptFormatter(app, "Some Text", t.options);

    string helpMsg = app.data;
    //writeln(helpMsg);
    assert(helpMsg.length);
    assert(helpMsg.count("\n") == 3, to!string(helpMsg.count("\n")) ~ " "
        ~ helpMsg);
    assert(helpMsg.indexOf("Required:") != -1);
    assert(helpMsg.indexOf("--foo") != -1);
    assert(helpMsg.indexOf("-f") != -1);
    assert(helpMsg.indexOf("-h") != -1);
    assert(helpMsg.indexOf("--help") != -1);
    assert(helpMsg.indexOf("Help") != -1);

    string wanted = "Some Text\n-f  --foo Required: Help\n-h --help "
        ~ "          This help information.\n";
    assert(wanted == helpMsg, helpMsg ~ wanted);
}

@system unittest // Issue 14724
{
    bool a;
    auto args = ["prog", "--help"];
    GetoptResult rslt;
    try
    {
        rslt = getopt(args, config.required, "foo|f", "bool a", &a);
    }
    catch (Exception e)
    {
        enum errorMsg = "If the request for help was passed required options" ~
                "must not be set.";
        assert(false, errorMsg);
    }

    assert(rslt.helpWanted);
}

// throw on duplicate options
@system unittest
{
    import core.exception;
    auto args = ["prog", "--abc", "1"];
    int abc, def;
    assertThrown!AssertError(getopt(args, "abc", &abc, "abc", &abc));
    assertThrown!AssertError(getopt(args, "abc|a", &abc, "def|a", &def));
    assertNotThrown!AssertError(getopt(args, "abc", &abc, "def", &def));
}

@system unittest // Issue 17327 repeated option use
{
    long num = 0;

    string[] args = ["program", "--num", "3"];
    getopt(args, "n|num", &num);
    assert(num == 3);

    args = ["program", "--num", "3", "--num", "5"];
    getopt(args, "n|num", &num);
    assert(num == 5);

    args = ["program", "--n", "3", "--num", "5", "-n", "-7"];
    getopt(args, "n|num", &num);
    assert(num == -7);

    void add1() { num++; }
    void add2(string option) { num += 2; }
    void addN(string option, string value)
    {
        import std.conv : to;
        num += value.to!long;
    }

    num = 0;
    args = ["program", "--add1", "--add2", "--add1", "--add", "5", "--add2", "--add", "10"];
    getopt(args,
           "add1", "Add 1 to num", &add1,
           "add2", "Add 2 to num", &add2,
           "add", "Add N to num", &addN,);
    assert(num == 21);

    bool flag = false;
    args = ["program", "--flag"];
    getopt(args, "f|flag", "Boolean", &flag);
    assert(flag);

    flag = false;
    args = ["program", "-f", "-f"];
    getopt(args, "f|flag", "Boolean", &flag);
    assert(flag);

    flag = false;
    args = ["program", "--flag=true", "--flag=false"];
    getopt(args, "f|flag", "Boolean", &flag);
    assert(!flag);

    flag = false;
    args = ["program", "--flag=true", "--flag=false", "-f"];
    getopt(args, "f|flag", "Boolean", &flag);
    assert(flag);
}

@safe unittest  // Delegates as callbacks
{
    alias TwoArgOptionHandler = void delegate(string option, string value) @safe;

    TwoArgOptionHandler makeAddNHandler(ref long dest)
    {
        void addN(ref long dest, string n)
        {
            import std.conv : to;
            dest += n.to!long;
        }

        return (option, value) => addN(dest, value);
    }

    long x = 0;
    long y = 0;

    string[] args =
        ["program", "--x-plus-1", "--x-plus-1", "--x-plus-5", "--x-plus-n", "10",
         "--y-plus-n", "25", "--y-plus-7", "--y-plus-n", "15", "--y-plus-3"];

    getopt(args,
           "x-plus-1", "Add one to x", delegate void() { x += 1; },
           "x-plus-5", "Add five to x", delegate void(string option) { x += 5; },
           "x-plus-n", "Add NUM to x", makeAddNHandler(x),
           "y-plus-7", "Add seven to y", delegate void() { y += 7; },
           "y-plus-3", "Add three to y", delegate void(string option) { y += 3; },
           "y-plus-n", "Add NUM to x", makeAddNHandler(y),);

    assert(x == 17);
    assert(y == 50);
}

@system unittest // Hyphens at the start of option values; Issue 17650
{
    auto args = ["program", "-m", "-5", "-n", "-50", "-c", "-", "-f", "-"];

    int m;
    int n;
    char c;
    string f;

    getopt(args,
           "m|mm", "integer", &m,
           "n|nn", "integer", &n,
           "c|cc", "character", &c,
           "f|file", "filename or hyphen for stdin", &f);

    assert(m == -5);
    assert(n == -50);
    assert(c == '-');
    assert(f == "-");
}

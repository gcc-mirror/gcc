/++
Convenience file that allows to import entire Phobos in one import.
+/
module std;

///
@safe unittest
{
    import std;

    int len;
    const r = 6.iota
              .filter!(a => a % 2) // 1 3 5
              .map!(a => a * 2) // 2 6 10
              .tee!(_ => len++)
              .substitute(6, -6) // 2 -6 10
              .sum
              .reverseArgs!format("Sum: %d");

    assert(len == 3);
    assert(r == "Sum: 6");
}

///
@safe unittest
{
    import std;
    assert(10.iota.map!(a => pow(2, a)).sum == 1023);
}

public import
 std.algorithm,
 std.array,
 std.ascii,
 std.base64,
 std.bigint,
 std.bitmanip,
 std.checkedint,
 std.compiler,
 std.complex,
 std.concurrency,
 std.container,
 std.conv,
 std.csv,
 std.datetime,
 std.demangle,
 std.digest,
 std.encoding,
 std.exception,
 std.file,
 std.format,
 std.functional,
 std.getopt,
 std.int128,
 std.json,
 std.logger,
 std.math,
 std.mathspecial,
 std.meta,
 std.mmfile,
 std.net.curl,
 std.net.isemail,
 std.numeric,
 std.parallelism,
 std.path,
 std.process,
 std.random,
 std.range,
 std.regex,
 std.signals,
 std.socket,
 std.stdint,
 std.stdio,
 std.string,
 std.sumtype,
 std.system,
 std.traits,
 std.typecons,
 std.uni,
 std.uri,
 std.utf,
 std.uuid,
 std.variant,
 std.zip,
 std.zlib;

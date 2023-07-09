/*
 * Data collection and report generation for
 *   -profile=gc
 * switch
 *
 * Copyright: Copyright Digital Mars 2015 - 2015.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Andrei Alexandrescu and Walter Bright
 * Source: $(DRUNTIMESRC rt/_profilegc.d)
 */

module rt.profilegc;

private:

import core.stdc.errno;
import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;

import core.exception : onOutOfMemoryError;
import core.internal.container.hashtab;

struct Entry { ulong count, size; }

char[] buffer;
HashTab!(const(char)[], Entry) newCounts;

__gshared
{
    HashTab!(const(char)[], Entry) globalNewCounts;
    string logfilename = "profilegc.log";
}

/****
 * Set file name for output.
 * A file name of "" means write results to stdout.
 * Params:
 *      name = file name
 */

extern (C) void profilegc_setlogfilename(string name)
{
    logfilename = name ~ "\0";
}

public void accumulate(string file, uint line, string funcname, string type, ulong sz) @nogc nothrow
{
    if (sz == 0)
        return;

    char[3 * line.sizeof + 1] buf = void;
    auto buflen = snprintf(buf.ptr, buf.length, "%u", line);

    auto length = type.length + 1 + funcname.length + 1 + file.length + 1 + buflen;
    if (length > buffer.length)
    {
        // Enlarge buffer[] so it is big enough
        assert(buffer.length > 0 || buffer.ptr is null);
        auto p = cast(char*)realloc(buffer.ptr, length);
        if (!p)
            onOutOfMemoryError();
        buffer = p[0 .. length];
    }

    // "type funcname file:line"
    buffer[0 .. type.length] = type[];
    buffer[type.length] = ' ';
    buffer[type.length + 1 ..
           type.length + 1 + funcname.length] = funcname[];
    buffer[type.length + 1 + funcname.length] = ' ';
    buffer[type.length + 1 + funcname.length + 1 ..
           type.length + 1 + funcname.length + 1 + file.length] = file[];
    buffer[type.length + 1 + funcname.length + 1 + file.length] = ':';
    buffer[type.length + 1 + funcname.length + 1 + file.length + 1 ..
           type.length + 1 + funcname.length + 1 + file.length + 1 + buflen] = buf[0 .. buflen];

    if (auto pcount = cast(string)buffer[0 .. length] in newCounts)
    { // existing entry
        pcount.count++;
        pcount.size += sz;
    }
    else
    {
        auto key = (cast(char*) malloc(char.sizeof * length))[0 .. length];
        key[] = buffer[0..length];
        newCounts[key] = Entry(1, sz); // new entry
    }
}

// Merge thread local newCounts into globalNewCounts
static ~this()
{
    if (newCounts.length)
    {
        synchronized
        {
            foreach (name, entry; newCounts)
            {
                if (!(name in globalNewCounts))
                    globalNewCounts[name] = Entry.init;

                globalNewCounts[name].count += entry.count;
                globalNewCounts[name].size += entry.size;
            }
        }
        newCounts.reset();
    }
    free(buffer.ptr);
    buffer = null;
}

// Write report to stderr
shared static ~this()
{
    static struct Result
    {
        const(char)[] name;
        Entry entry;

        // qsort() comparator to sort by count field
        extern (C) static int qsort_cmp(scope const void *r1, scope const void *r2) @nogc nothrow
        {
            auto result1 = cast(Result*)r1;
            auto result2 = cast(Result*)r2;
            long cmp = result2.entry.size - result1.entry.size;
            if (cmp) return cmp < 0 ? -1 : 1;
            cmp = result2.entry.count - result1.entry.count;
            if (cmp) return cmp < 0 ? -1 : 1;
            if (result2.name == result1.name) return 0;
            // ascending order for names reads better
            return result2.name > result1.name ? -1 : 1;
        }
    }

    size_t size = globalNewCounts.length;
    Result[] counts = (cast(Result*) malloc(size * Result.sizeof))[0 .. size];
    scope(exit)
        free(counts.ptr);

    size_t i;
    foreach (name, entry; globalNewCounts)
    {
        counts[i].name = name;
        counts[i].entry = entry;
        ++i;
    }

    if (counts.length)
    {
        qsort(counts.ptr, counts.length, Result.sizeof, &Result.qsort_cmp);

        FILE* fp = logfilename == "\0" ? stdout : fopen((logfilename).ptr, "w");
        if (fp)
        {
            fprintf(fp, "bytes allocated, allocations, type, function, file:line\n");
            foreach (ref c; counts)
            {
                fprintf(fp, "%15llu\t%15llu\t%8.*s\n",
                    cast(ulong)c.entry.size, cast(ulong)c.entry.count,
                    cast(int) c.name.length, c.name.ptr);
            }
            if (logfilename.length)
                fclose(fp);
        }
        else
        {
            const err = errno;
            fprintf(stderr, "cannot write profilegc log file '%.*s' (errno=%d)",
                cast(int) logfilename.length,
                logfilename.ptr,
                cast(int) err);
        }
    }
}

#!/usr/bin/env dub
/++dub.sdl:
name "gen_phobos_sources"
+/
// Written in the D programming language.
import std.stdio;
import std.file;
import std.path;
import std.range;
import std.string;
import std.algorithm;

string[] filterList = [
    "./Makefile.in", "./Makefile.am",
    "./index.dd",
    "./libgphobos.spec.in", "./drtstuff.spec",
    "./LICENSE_1_0.txt", "./MERGE",
    "./std/experimental/note.md"
];

struct Files
{
    string[] baseList;
    string[][string] sysList;
}

void main(string[] args)
{
    Files[string] fileMap;

    foreach (entry; "."
                    .dirEntries(SpanMode.depth)
                    .filter!(a => !filterList.canFind(a)))
    {
        if (entry.isFile)
        {
            auto ext = entry.extension.empty ? "" : entry.extension[1 .. $];
            if (!(ext in fileMap))
                fileMap[ext] = Files.init;

            string sentry = entry[2 .. $];

            if (entry.name.startsWith("./std/c/"))
            {
                if (entry.dirName == "./std/c")
                {
                    fileMap[ext].sysList["stdc"] ~= sentry;
                }
                else
                {
                    auto components = entry.pathSplitter;
                    components.popFrontN(3);
                    fileMap[ext].sysList[components.front] ~= sentry;
                }
            }
            else
                fileMap[ext].baseList ~= sentry;
        }
    }

    writeln("if ENABLE_LIBDRUNTIME_ONLY");
    foreach (extEntry; fileMap.byKeyValue.array.sort!"a.key < b.key")
    {
        auto ext = extEntry.key;
        auto value = extEntry.value;
        writeList("PHOBOS_" ~ ext.toUpper() ~ "SOURCES", [],
                  !value.baseList.empty);
        foreach (entry; value.sysList.byKeyValue.array.sort!"a.key < b.key")
        {
            string name = "PHOBOS_" ~ ext.toUpper() ~ "SOURCES_"
                ~ entry.key.toUpper();
            writeList(name, [], !entry.value.empty);
        }
    }
    writeln();
    writeln("else");
    foreach (extEntry; fileMap.byKeyValue.array.sort!"a.key < b.key")
    {
        auto ext = extEntry.key;
        auto value = extEntry.value;
        writeList("PHOBOS_" ~ ext.toUpper() ~ "SOURCES", value.baseList);
        foreach (entry; value.sysList.byKeyValue.array.sort!"a.key < b.key")
        {
            string name = "PHOBOS_" ~ ext.toUpper() ~ "SOURCES_"
                ~ entry.key.toUpper();
            writeList(name, entry.value);
        }
    }
    writeln();
    writeln("endif");
}

void writeList(string name, string[] values, bool force = false)
{
    if (!force && values.empty)
        return;

    values = sort(values).array();
    writeln();
    writef("%s =", name);
    size_t line = name.length + 3;
    foreach (entry; values)
    {
        if (line + entry.length > 70)
        {
            line = 0;
            writeln(` \`);
            write('\t');
        }
        else
            write(" ");
        write(entry);
        line += entry.length + 1;
    }
    writeln();
}

#!/usr/bin/env dub
/++dub.sdl:
name "gen_druntime_sources"
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
    "./gcc/config.d.in", "./gcc/libbacktrace.d.in", "./gcc/drtstuff.c",
    "./LICENSE.txt", "./MERGE",
    "./rt/dylib_fixes.c"
];

struct Files
{
    string[] baseList, cppList;
    string[][string] sysList;
}

void main(string[] args)
{
    Files[string] fileMap;

    foreach(entry; ".".dirEntries(SpanMode.depth).filter!(a => !filterList.canFind(a)))
    {
        if (entry.name.startsWith("./config/"))
            continue;

        if(entry.isFile)
        {
            auto ext = entry.extension.empty ? "" : entry.extension[1 .. $];
            if(!(ext in fileMap))
                fileMap[ext] = Files.init;

            string sentry = entry[2 .. $];

            if(entry.name.startsWith("./core/stdcpp/"))
                fileMap[ext].cppList ~= sentry;
            else if(entry.name.startsWith("./core/sys/"))
            {
                auto components = entry.pathSplitter;
                components.popFrontN(3);
                fileMap[ext].sysList[components.front] ~= sentry;
            }
            else
                fileMap[ext].baseList ~= sentry;
        }
    }

    foreach(extEntry; fileMap.byKeyValue.array.sort!"a.key < b.key")
    {
        auto ext = extEntry.key;
        auto value = extEntry.value;
        writeList("DRUNTIME_" ~ ext.toUpper() ~ "SOURCES", value.baseList);
        writeList("DRUNTIME_" ~ ext.toUpper() ~ "SOURCES_STDCXX", value.cppList);
        foreach(entry; value.sysList.byKeyValue.array.sort!"a.key < b.key")
        {
            writeList("DRUNTIME_" ~ ext.toUpper() ~ "SOURCES_" ~ entry.key.toUpper(), entry.value);
        }
    }
}

void writeList(string name, string[] values, bool force = false)
{
    if (!force && values.empty)
        return;

    values = sort(values).array();
    writeln();
    writef("%s =", name);
    size_t line = name.length + 3;
    foreach(entry; values)
    {
        if(line + entry.length > 70)
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

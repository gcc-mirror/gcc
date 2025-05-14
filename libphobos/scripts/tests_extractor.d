#!/usr/bin/env dub
/++dub.sdl:
name "tests_extractor"
dependency "libdparse" version="~>0.24.0"
dflags "-fall-instantiations" platform="gdc"
+/
// Written in the D programming language.

import dparse.ast;
import std.algorithm;
import std.conv;
import std.exception;
import std.experimental.logger;
import std.file;
import std.path;
import std.range;
import std.stdio;

class TestVisitor : ASTVisitor
{
    File outFile;
    ubyte[] sourceCode;
    string moduleName;

    this(File outFile, ubyte[] sourceCode)
    {
        this.outFile = outFile;
        this.sourceCode = sourceCode;
    }

    alias visit = ASTVisitor.visit;

    override void visit(const Module m)
    {
        if (m.moduleDeclaration !is null)
        {
            moduleName = m.moduleDeclaration.moduleName.identifiers.map!(i => i.text).join(".");
        }
        else
        {
            // Fallback: convert the file path to its module path, e.g. std/uni.d -> std.uni
            moduleName = outFile.name.replace(".d", "").replace(dirSeparator, ".").replace(".package", "");
        }
        m.accept(this);
    }

    override void visit(const Declaration decl)
    {
        if (decl.unittest_ !is null && decl.unittest_.comment !is null)
            print(decl.unittest_, decl.attributes);

        decl.accept(this);
    }

    override void visit(const ConditionalDeclaration decl)
    {
        bool skipTrue;

        // Check if it's a version that should be skipped
        if (auto vcd = decl.compileCondition.versionCondition)
        {
            if (vcd.token.text == "StdDdoc")
                skipTrue = true;
        }

        // Search if/version block
        if (!skipTrue)
        {
            foreach (d; decl.trueDeclarations)
                visit(d);
        }

        // Search else block
        foreach (d; decl.falseDeclarations)
            visit(d);
    }

private:

    void print(const Unittest u, const Attribute[] attributes)
    {
        static immutable predefinedAttributes = ["nogc", "system", "nothrow", "safe", "trusted", "pure"];

        // Write system attributes
        foreach (attr; attributes)
        {
            // pure and nothrow
            if (attr.attribute.type != 0)
            {
                import dparse.lexer : str;
                const attrText = attr.attribute.type.str;
                outFile.write(text(attrText, " "));
            }

            const atAttribute = attr.atAttribute;
            if (atAttribute is null)
                continue;

            const atText = atAttribute.identifier.text;

            // Ignore custom attributes (@myArg)
            if (!predefinedAttributes.canFind(atText))
                continue;

            outFile.write(text("@", atText, " "));
        }

        // Write the unittest block
        outFile.write("unittest\n{\n");
        scope(exit) outFile.writeln("}\n");

        // Add an import to the current module
        outFile.writefln("    import %s;", moduleName);

        // Write the content of the unittest block (but skip the first brace)
        auto k = cast(immutable(char)[]) sourceCode[u.blockStatement.startLocation .. u.blockStatement.endLocation];
        k.findSkip("{");
        outFile.write(k);

        // If the last line contains characters, we want to add an extra line
        // for increased visual beauty
        if (k[$ - 1] != '\n')
            outFile.writeln;
    }
}

bool parseFile(File inFile, File outFile)
{
    import dparse.lexer;
    import dparse.parser : parseModule;
    import dparse.rollback_allocator : RollbackAllocator;
    import std.array : uninitializedArray;

    if (inFile.size == 0)
        return false;

    ubyte[] sourceCode = uninitializedArray!(ubyte[])(to!size_t(inFile.size));
    inFile.rawRead(sourceCode);
    LexerConfig config;
    auto cache = StringCache(StringCache.defaultBucketCount);
    auto tokens = getTokensForParser(sourceCode, config, &cache);

    RollbackAllocator rba;
    auto m = parseModule(tokens.array, inFile.name, &rba);
    auto visitor = new TestVisitor(outFile, sourceCode);
    visitor.visit(m);
    return visitor.outFile.size != 0;
}

void parseFileDir(string inputDir, string fileName, string outputDir)
{
    import std.path : buildPath, dirSeparator, buildNormalizedPath;

    // File name without its parent directory, e.g. std/uni.d
    string fileNameNormalized = (inputDir == "." ? fileName : fileName.replace(inputDir, ""));

    // Remove leading dots or slashes
    while (!fileNameNormalized.empty && fileNameNormalized[0] == '.')
        fileNameNormalized = fileNameNormalized[1 .. $];
    if (fileNameNormalized.length >= dirSeparator.length &&
            fileNameNormalized[0 .. dirSeparator.length] == dirSeparator)
        fileNameNormalized = fileNameNormalized[dirSeparator.length .. $];

    // Convert the file path to a nice output file, e.g. std/uni.d -> std_uni.d
    string outName = fileNameNormalized.replace(dirSeparator, "_");
    auto outFile = buildPath(outputDir, outName);

    // Removes the output file if nothing was written
    if (!parseFile(File(fileName), File(outFile, "w")))
        remove(outFile);
}

void main(string[] args)
{
    import std.getopt;

    string inputDir;
    string outputDir = "./out";
    string modulePrefix;

    auto helpInfo = getopt(args, config.required,
            "inputdir|i", "Folder to start the recursive search for unittest blocks (can be a single file)", &inputDir,
            "outputdir|o", "Folder to which the extracted test files should be saved (stdout for a single file)", &outputDir,
    );

    if (helpInfo.helpWanted)
    {
        return defaultGetoptPrinter(`phobos_tests_extractor
Searches the input directory recursively for public unittest blocks, i.e.
unittest blocks that are annotated with three slashes (///).
The tests will be extracted as one file for each source file
to the output directory.
`, helpInfo.options);
    }

    inputDir = inputDir.asNormalizedPath.array;
    outputDir= outputDir.asNormalizedPath.array;

    if (!exists(outputDir))
        mkdir(outputDir);

    // If the module prefix is std -> add a dot for the next modules to follow
    if (!modulePrefix.empty)
        modulePrefix ~= '.';

    DirEntry[] files;

    if (inputDir.isFile)
    {
        stderr.writeln("ignoring ", inputDir);
        return;
    }
    else
    {
        files = dirEntries(inputDir, SpanMode.depth).filter!(
                a => a.name.endsWith(".d") && !a.name.canFind(".git")).array;
    }

    foreach (file; files)
    {
        stderr.writeln("parsing ", file);
        parseFileDir(inputDir, file, outputDir);
    }
}

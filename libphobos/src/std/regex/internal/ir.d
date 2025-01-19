/*
    Implementation of std.regex IR, an intermediate representation
    of a regular expression pattern.

    This is a common ground between frontend regex component (parser)
    and backend components - generators, matchers and other "filters".
*/
module std.regex.internal.ir;

package(std.regex):

import std.exception, std.meta, std.range.primitives, std.traits, std.uni;

debug(std_regex_parser) import std.stdio;
// just a common trait, may be moved elsewhere
alias BasicElementOf(Range) = Unqual!(ElementEncodingType!Range);

enum privateUseStart = '\U000F0000', privateUseEnd ='\U000FFFFD';

// heuristic value determines maximum CodepointSet length suitable for linear search
enum maxCharsetUsed = 6;

// another variable to tweak behavior of caching generated Tries for character classes
enum maxCachedMatchers = 8;

alias Trie = CodepointSetTrie!(13, 8);
alias makeTrie = codepointSetTrie!(13, 8);

CharMatcher[CodepointSet] matcherCache;

//accessor with caching
@trusted CharMatcher getMatcher(CodepointSet set)
{
    // almost all properties of AA are not @safe
    // https://issues.dlang.org/show_bug.cgi?id=6357
    if (__ctfe || maxCachedMatchers == 0)
        return CharMatcher(set);
    else
    {
        auto p = set in matcherCache;
        if (p)
            return *p;
        if (matcherCache.length == maxCachedMatchers)
        {
            // flush enmatchers in trieCache
            matcherCache = null;
        }
        return (matcherCache[set] = CharMatcher(set));
    }
}

// Force pure because that is needed
// Templated so that we don't pull in std.uni wordCharacter unnecessarily.
@property ref wordMatcher()() pure
{
    static auto actual()
    {
        static CharMatcher matcher;
        static bool haveMatcher;

        if (!haveMatcher)
        {
            matcher = CharMatcher(wordCharacter);
            haveMatcher = true;
        }

        return &matcher;
    }

    // WORKAROUND: if the compiler won't memoize the output of the function for us,
    //  we'll do it with pure and there will be casts and it'll be happy about it.
    // This is unfortunately needed to make std.regex as a whole faster to import & use
    //  in build times ~500ms.
    return *(cast(immutable(CharMatcher)* function() @safe nothrow @nogc pure)&actual)();
}

// some special Unicode white space characters
private enum NEL = '\u0085', LS = '\u2028', PS = '\u2029';

//Regular expression engine/parser options:
// global - search  all nonoverlapping matches in input
// casefold - case insensitive matching, do casefolding on match in unicode mode
// freeform - ignore whitespace in pattern, to match space use [ ] or \s
// multiline - switch  ^, $ detect start and end of linesinstead of just start and end of input
enum RegexOption: uint {
    global = 0x1,
    casefold = 0x2,
    freeform = 0x4,
    nonunicode = 0x8,
    multiline = 0x10,
    singleline = 0x20
}
//do not reorder this list
alias RegexOptionNames = AliasSeq!('g', 'i', 'x', 'U', 'm', 's');
static assert( RegexOption.max < 0x80);

package(std) string regexOptionsToString()(uint flags) nothrow pure @safe
{
    flags &= (RegexOption.max << 1) - 1;
    if (!flags)
        return "";
    char[RegexOptionNames.length] buffer = void;
    size_t pos = 0;
    foreach (i, flag; __traits(allMembers, RegexOption))
        if (flags & __traits(getMember, RegexOption, flag))
            buffer[pos++] = RegexOptionNames[i];
    return buffer[0 .. pos].idup;
}

// flags that allow guide execution of engine
enum RegexInfo : uint { oneShot = 0x80 }

// IR bit pattern: 0b1_xxxxx_yy
// where yy indicates class of instruction, xxxxx for actual operation code
//     00: atom, a normal instruction
//     01: open, opening of a group, has length of contained IR in the low bits
//     10: close, closing of a group, has length of contained IR in the low bits
//     11 unused
//
// Loops with Q (non-greedy, with ? mark) must have the same size / other properties as non Q version
// Possible changes:
//* merge group, option, infinite/repeat start (to never copy during parsing of (a|b){1,2})
//* reorganize groups to make n args easier to find, or simplify the check for groups of similar ops
//  (like lookaround), or make it easier to identify hotspots.

enum IR:uint {
    Char              = 0b1_00000_00, //a character
    Any               = 0b1_00001_00, //any character
    CodepointSet      = 0b1_00010_00, //a most generic CodepointSet [...]
    Trie              = 0b1_00011_00, //CodepointSet implemented as Trie
    //match with any of a consecutive OrChar's in this sequence
    //(used for case insensitive match)
    //OrChar holds in upper two bits of data total number of OrChars in this _sequence_
    //the drawback of this representation is that it is difficult
    // to detect a jump in the middle of it
    OrChar             = 0b1_00100_00,
    Nop                = 0b1_00101_00, //no operation (padding)
    End                = 0b1_00110_00, //end of program
    Bol                = 0b1_00111_00, //beginning of a line ^
    Eol                = 0b1_01000_00, //end of a line $
    Wordboundary       = 0b1_01001_00, //boundary of a word
    Notwordboundary    = 0b1_01010_00, //not a word boundary
    Backref            = 0b1_01011_00, //backreference to a group (that has to be pinned, i.e. locally unique) (group index)
    GroupStart         = 0b1_01100_00, //start of a group (x) (groupIndex+groupPinning(1bit))
    GroupEnd           = 0b1_01101_00, //end of a group (x) (groupIndex+groupPinning(1bit))
    Option             = 0b1_01110_00, //start of an option within an alternation x | y (length)
    GotoEndOr          = 0b1_01111_00, //end of an option (length of the rest)
    Bof                = 0b1_10000_00, //begining of "file" (string) ^
    Eof                = 0b1_10001_00, //end of "file" (string) $
    //... any additional atoms here

    OrStart            = 0b1_00000_01, //start of alternation group  (length)
    OrEnd              = 0b1_00000_10, //end of the or group (length,mergeIndex)
    //with this instruction order
    //bit mask 0b1_00001_00 could be used to test/set greediness
    InfiniteStart      = 0b1_00001_01, //start of an infinite repetition x* (length)
    InfiniteEnd        = 0b1_00001_10, //end of infinite repetition x* (length,mergeIndex)
    InfiniteQStart     = 0b1_00010_01, //start of a non eager infinite repetition x*? (length)
    InfiniteQEnd       = 0b1_00010_10, //end of non eager infinite repetition x*? (length,mergeIndex)
    InfiniteBloomStart = 0b1_00011_01, //start of an filtered infinite repetition x* (length)
    InfiniteBloomEnd   = 0b1_00011_10, //end of filtered infinite repetition x* (length,mergeIndex)
    RepeatStart        = 0b1_00100_01, //start of a {n,m} repetition (length)
    RepeatEnd          = 0b1_00100_10, //end of x{n,m} repetition (length,step,minRep,maxRep)
    RepeatQStart       = 0b1_00101_01, //start of a non eager x{n,m}? repetition (length)
    RepeatQEnd         = 0b1_00101_10, //end of non eager x{n,m}? repetition (length,step,minRep,maxRep)

    //
    LookaheadStart     = 0b1_00110_01, //begin of the lookahead group (length)
    LookaheadEnd       = 0b1_00110_10, //end of a lookahead group (length)
    NeglookaheadStart  = 0b1_00111_01, //start of a negative lookahead (length)
    NeglookaheadEnd    = 0b1_00111_10, //end of a negative lookahead (length)
    LookbehindStart    = 0b1_01000_01, //start of a lookbehind (length)
    LookbehindEnd      = 0b1_01000_10, //end of a lookbehind (length)
    NeglookbehindStart = 0b1_01001_01, //start of a negative lookbehind (length)
    NeglookbehindEnd   = 0b1_01001_10, //end of negative lookbehind (length)
}

//a shorthand for IR length - full length of specific opcode evaluated at compile time
template IRL(IR code)
{
    enum uint IRL =  lengthOfIR(code);
}
static assert(IRL!(IR.LookaheadStart) == 3);

//how many parameters follow the IR, should be optimized fixing some IR bits
int immediateParamsIR(IR i) @safe pure nothrow @nogc
{
    switch (i)
    {
    case IR.OrEnd,IR.InfiniteEnd,IR.InfiniteQEnd:
        return 1;  // merge table index
    case IR.InfiniteBloomEnd:
        return 2;  // bloom filter index + merge table index
    case IR.RepeatEnd, IR.RepeatQEnd:
        return 4;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        return 2;  // start-end of captures used
    default:
        return 0;
    }
}

//full length of IR instruction inlcuding all parameters that might follow it
int lengthOfIR(IR i) @safe pure nothrow @nogc
{
    return 1 + immediateParamsIR(i);
}

//full length of the paired IR instruction inlcuding all parameters that might follow it
int lengthOfPairedIR(IR i) @safe pure nothrow @nogc
{
    return 1 + immediateParamsIR(pairedIR(i));
}

//if the operation has a merge point (this relies on the order of the ops)
bool hasMerge(IR i) @safe pure nothrow @nogc
{
    return (i&0b11)==0b10 && i <= IR.RepeatQEnd;
}

//is an IR that opens a "group"
bool isStartIR(IR i) @safe pure nothrow @nogc
{
    return (i&0b11)==0b01;
}

//is an IR that ends a "group"
bool isEndIR(IR i) @safe pure nothrow @nogc
{
    return (i&0b11)==0b10;
}

//is a standalone IR
bool isAtomIR(IR i) @safe pure nothrow @nogc
{
    return (i&0b11)==0b00;
}

//makes respective pair out of IR i, swapping start/end bits of instruction
IR pairedIR(IR i) @safe pure nothrow @nogc
{
    assert(isStartIR(i) || isEndIR(i));
    return cast(IR) (i ^ 0b11);
}

//encoded IR instruction
@safe pure
struct Bytecode
{
    uint raw;
    //natural constraints
    enum maxSequence = 2+4;
    enum maxData = 1 << 22;
    enum maxRaw = 1 << 31;

@safe pure:
    this(IR code, uint data)
    {
        assert(data < (1 << 22) && code < 256);
        raw = code << 24 | data;
    }

    this(IR code, uint data, uint seq)
    {
        assert(data < (1 << 22) && code < 256 );
        assert(seq >= 2 && seq < maxSequence);
        raw = code << 24 | (seq - 2)<<22 | data;
    }

    //store raw data
    static Bytecode fromRaw(uint data)
    {
        Bytecode t;
        t.raw = data;
        return t;
    }

    // bit twiddling helpers
    // 0-arg template due to https://issues.dlang.org/show_bug.cgi?id=10985
    @property uint data()() const { return raw & 0x003f_ffff; }

    @property void data()(uint val)
    {
        raw = (raw & ~0x003f_ffff) | (val & 0x003f_ffff);
    }

    // ditto
    // 0-arg template due to https://issues.dlang.org/show_bug.cgi?id=10985
    @property uint sequence()() const { return 2 + (raw >> 22 & 0x3); }

    // ditto
    // 0-arg template due to https://issues.dlang.org/show_bug.cgi?id=10985
    @property IR code()() const { return cast(IR)(raw >> 24); }

    //ditto
    @property bool hotspot() const { return hasMerge(code); }

    //test the class of this instruction
    @property bool isAtom() const { return isAtomIR(code); }

    //ditto
    @property bool isStart() const { return isStartIR(code); }

    //ditto
    @property bool isEnd() const { return isEndIR(code); }

    //number of arguments for this instruction
    @property int args() const { return immediateParamsIR(code); }

    //mark this GroupStart or GroupEnd as referenced in backreference
    void setBackrefence()
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        raw = raw | 1 << 23;
    }

    //is referenced
    @property bool backreference() const
    {
        assert(code == IR.GroupStart || code == IR.GroupEnd);
        return (raw & 1 << 23) != 0;
    }

    //mark as local reference (for backrefs in lookarounds)
    void setLocalRef()
    {
        assert(code == IR.Backref);
        raw = raw | 1 << 23;
    }

    //is a local ref
    @property bool localRef() const
    {
        assert(code == IR.Backref);
        return (raw & 1 << 23) != 0;
    }

    //human readable name of instruction
    @trusted @property string mnemonic()() const
    {//@@@BUG@@@ to is @system
        import std.conv : to;
        return to!string(code);
    }

    //full length of instruction
    @property uint length() const
    {
        return lengthOfIR(code);
    }

    //full length of respective start/end of this instruction
    @property uint pairedLength() const
    {
        return lengthOfPairedIR(code);
    }

    //returns bytecode of paired instruction (assuming this one is start or end)
    @property Bytecode paired() const
    {//depends on bit and struct layout order
        assert(isStart || isEnd);
        return Bytecode.fromRaw(raw ^ 0b11 << 24);
    }

    //gets an index into IR block of the respective pair
    uint indexOfPair(uint pc) const
    {
        assert(isStart || isEnd);
        return isStart ? pc + data + length  : pc - data - lengthOfPairedIR(code);
    }
}

static assert(Bytecode.sizeof == 4);


//index entry structure for name --> number of submatch
struct NamedGroup
{
    string name;
    uint group;
}

//holds pair of start-end markers for a submatch
struct Group(DataIndex)
{
    DataIndex begin = DataIndex.max;
    DataIndex end   = DataIndex.min;

    bool opCast(T : bool)() const
    {
        return begin <= end;
    }

    @trusted string toString()() const
    {
        if (begin < end)
            return "(unmatched)";
        import std.array : appender;
        import std.format.write : formattedWrite;
        auto a = appender!string();
        formattedWrite(a, "%s..%s", begin, end);
        return a.data;
    }
}

//debugging tool, prints out instruction along with opcodes
debug(std_regex_parser) @trusted string disassemble(in Bytecode[] irb, uint pc, in NamedGroup[] dict=[])
{
    import std.array : appender;
    import std.format.write : formattedWrite;
    auto output = appender!string();
    formattedWrite(output,"%s", irb[pc].mnemonic);
    switch (irb[pc].code)
    {
    case IR.Char:
        formattedWrite(output, " %s (0x%x)",cast(dchar) irb[pc].data, irb[pc].data);
        break;
    case IR.OrChar:
        formattedWrite(output, " %s (0x%x) seq=%d", cast(dchar) irb[pc].data, irb[pc].data, irb[pc].sequence);
        break;
    case IR.RepeatStart, IR.InfiniteStart, IR.InfiniteBloomStart,
    IR.Option, IR.GotoEndOr, IR.OrStart:
        //forward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc+len+IRL!(IR.RepeatStart));
        break;
    case IR.RepeatEnd, IR.RepeatQEnd: //backward-jump instructions
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u min=%u max=%u step=%u",
            pc - len, irb[pc + 3].raw, irb[pc + 4].raw, irb[pc + 2].raw);
        break;
    case IR.InfiniteEnd, IR.InfiniteQEnd, IR.InfiniteBloomEnd, IR.OrEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
        break;
    case  IR.LookaheadEnd, IR.NeglookaheadEnd: //ditto
        uint len = irb[pc].data;
        formattedWrite(output, " pc=>%u", pc-len);
        break;
    case IR.GroupStart, IR.GroupEnd:
        uint n = irb[pc].data;
        string name;
        foreach (v;dict)
            if (v.group == n)
            {
                name = "'"~v.name~"'";
                break;
            }
        formattedWrite(output, " %s #%u " ~ (irb[pc].backreference ? "referenced" : ""),
                name, n);
        break;
    case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
        uint len = irb[pc].data;
        uint start = irb[pc+1].raw, end = irb[pc+2].raw;
        formattedWrite(output, " pc=>%u [%u..%u]", pc + len + IRL!(IR.LookaheadStart), start, end);
        break;
    case IR.Backref: case IR.CodepointSet: case IR.Trie:
        uint n = irb[pc].data;
        formattedWrite(output, " %u",  n);
        if (irb[pc].code == IR.Backref)
            formattedWrite(output, " %s", irb[pc].localRef ? "local" : "global");
        break;
    default://all data-free instructions
    }
    if (irb[pc].hotspot)
        formattedWrite(output, " Hotspot %u", irb[pc+1].raw);
    return output.data;
}

//disassemble the whole chunk
debug(std_regex_parser) @trusted void printBytecode()(in Bytecode[] slice, in NamedGroup[] dict=[])
{
    import std.stdio : writeln;
    for (uint pc=0; pc<slice.length; pc += slice[pc].length)
        writeln("\t", disassemble(slice, pc, dict));
}

// Encapsulates memory management, explicit ref counting
// and the exact type of engine created
// there is a single instance per engine combination type x Char
// In future may also maintain a (TLS?) cache of memory
interface MatcherFactory(Char)
{
@safe:
    Matcher!Char create(const ref Regex!Char, in Char[] input) const;
    Matcher!Char dup(Matcher!Char m, in Char[] input) const;
    size_t incRef(Matcher!Char m) const;
    size_t decRef(Matcher!Char m) const;
}

// Only memory management, no compile-time vs run-time specialities
abstract class GenericFactory(alias EngineType, Char) : MatcherFactory!Char
{
    import core.memory : pureFree;
    import std.internal.memory : enforceMalloc;
    import core.memory : GC;
    // round up to next multiple of size_t for alignment purposes
    enum classSize = (__traits(classInstanceSize, EngineType!Char) + size_t.sizeof - 1) & ~(size_t.sizeof - 1);

    EngineType!Char construct(const ref Regex!Char re, in Char[] input, void[] memory) const;

    override EngineType!Char create(const ref Regex!Char re, in Char[] input) const @trusted
    {
        immutable size = EngineType!Char.initialMemory(re) + classSize;
        auto memory = enforceMalloc(size)[0 .. size];
        scope(failure) pureFree(memory.ptr);
        GC.addRange(memory.ptr, classSize);
        auto engine = construct(re, input, memory);
        assert(engine.refCount == 1);
        assert(cast(void*) engine == memory.ptr);
        return engine;
    }

    override EngineType!Char dup(Matcher!Char engine, in Char[] input) const @trusted
    {
        immutable size = EngineType!Char.initialMemory(engine.pattern) + classSize;
        auto memory = enforceMalloc(size)[0 .. size];
        scope(failure) pureFree(memory.ptr);
        auto copy = construct(engine.pattern, input, memory);
        GC.addRange(memory.ptr, classSize);
        engine.dupTo(copy, memory[classSize .. size]);
        assert(copy.refCount == 1);
        return copy;
    }

    override size_t incRef(Matcher!Char m) const
    {
        return ++m.refCount;
    }

    override size_t decRef(Matcher!Char m) const  @trusted
    {
        assert(m.refCount != 0);
        auto cnt = --m.refCount;
        if (cnt == 0)
        {
            void* ptr = cast(void*) m;
            GC.removeRange(ptr);
            pureFree(ptr);
        }
        return cnt;
    }
}

// A factory for run-time engines
class RuntimeFactory(alias EngineType, Char) : GenericFactory!(EngineType, Char)
{
    override EngineType!Char construct(const ref Regex!Char re, in Char[] input, void[] memory) const
    {
        import core.lifetime : emplace;
        return emplace!(EngineType!Char)(memory[0 .. classSize],
            re, Input!Char(input), memory[classSize .. $]);
    }
}

// A factory for compile-time engine
class CtfeFactory(alias EngineType, Char, alias func) : GenericFactory!(EngineType, Char)
{
    override EngineType!Char construct(const ref Regex!Char re, in Char[] input, void[] memory) const
    {
        import core.lifetime : emplace;
        return emplace!(EngineType!Char)(memory[0 .. classSize],
            re, &func, Input!Char(input), memory[classSize .. $]);
    }
}

private auto defaultFactoryImpl(Char)(const ref Regex!Char re)
{
    import std.regex.internal.backtracking : BacktrackingMatcher;
    import std.regex.internal.thompson : ThompsonMatcher;
    import std.algorithm.searching : canFind;
    static MatcherFactory!Char backtrackingFactory;
    static MatcherFactory!Char thompsonFactory;
    if (re.backrefed.canFind!"a != 0")
    {
        if (backtrackingFactory is null)
            backtrackingFactory = new RuntimeFactory!(BacktrackingMatcher, Char);
        return backtrackingFactory;
    }
    else
    {
        if (thompsonFactory is null)
            thompsonFactory = new RuntimeFactory!(ThompsonMatcher, Char);
        return thompsonFactory;
    }
}

// Used to generate a pure wrapper for defaultFactoryImpl. Based on the example in
// the std.traits.SetFunctionAttributes documentation.
auto assumePureFunction(T)(T t)
if (isFunctionPointer!T)
{
    enum attrs = functionAttributes!T | FunctionAttribute.pure_;
    return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) t;
}

// A workaround for R-T enum re = regex(...)
template defaultFactory(Char)
{
    // defaultFactory is constructed as a safe, pure wrapper over defaultFactoryImpl.
    // It can be faked as pure because the static mutable variables are used to cache
    // the key and character matcher. The technique used avoids delegates and GC.
    @property MatcherFactory!Char defaultFactory(const ref Regex!Char re) @safe pure
    {
        static auto impl(const ref Regex!Char re)
        {
            return defaultFactoryImpl(re);
        }

        static auto pureImpl(const ref Regex!Char re) @trusted
        {
            auto p = assumePureFunction(&impl);
            return p(re);
        }

        return pureImpl(re);
    }
}

// Defining it as an interface has the undesired side-effect:
// casting any class to an interface silently adjusts pointer to point to a nested vtbl
abstract class Matcher(Char)
{
abstract:
    // Get a (next) match
    int match(Group!size_t[] matches) pure;
    // This only maintains internal ref-count,
    // deallocation happens inside MatcherFactory
    @property ref size_t refCount() @safe;
    // Copy internal state to another engine, using memory arena 'memory'
    void dupTo(Matcher!Char m, void[] memory);
    // The pattern loaded
    @property ref const(Regex!Char) pattern() @safe;
    // Re-arm the engine with new Input
    Matcher rearm(in Char[] stream);
}

/++
    `Regex` object holds regular expression pattern in compiled form.
    Instances of this object are constructed via calls to `regex`.
    This is an intended form for caching and storage of frequently
    used regular expressions.
+/
struct Regex(Char)
{
    //temporary workaround for identifier lookup
    CodepointSet[] charsets; //
    Bytecode[] ir;      //compiled bytecode of pattern


    @safe @property bool empty() const nothrow {  return ir is null; }
    /++
    `namedCaptures` returns a range of all named captures in a given regular expression.
    +/
    @safe @property auto namedCaptures()
    {
        static struct NamedGroupRange
        {
        private:
            const(NamedGroup)[] groups;
            size_t start;
            size_t end;
        public:
            this(const(NamedGroup)[] g, size_t s, size_t e)
            {
                assert(s <= e);
                assert(e <= g.length);
                groups = g;
                start = s;
                end = e;
            }

            @property string front() { return groups[start].name; }
            @property string back() { return groups[end-1].name; }
            @property bool empty() { return start >= end; }
            @property size_t length() { return end - start; }
            alias opDollar = length;
            @property NamedGroupRange save()
            {
                return NamedGroupRange(groups, start, end);
            }
            void popFront() { assert(!empty); start++; }
            void popBack() { assert(!empty); end--; }
            string opIndex()(size_t i)
            {
                assert(start + i < end,
                       "Requested named group is out of range.");
                return groups[start+i].name;
            }
            NamedGroupRange opSlice(size_t low, size_t high) {
                assert(low <= high);
                assert(start + high <= end);
                return NamedGroupRange(groups, start + low, start + high);
            }
            NamedGroupRange opSlice() { return this.save; }
        }
        return NamedGroupRange(dict, 0, dict.length);
    }

package(std.regex):
    import std.regex.internal.kickstart : Kickstart; //TODO: get rid of this dependency
    const(NamedGroup)[] dict;              // maps name -> user group number
    uint ngroup;                           // number of internal groups
    uint maxCounterDepth;                  // max depth of nested {n,m} repetitions
    uint hotspotTableSize;                 // number of entries in merge table
    uint threadCount;                      // upper bound on number of Thompson VM threads
    uint flags;                            // global regex flags
    public const(CharMatcher)[]  matchers; // tables that represent character sets
    public const(BitTable)[] filters;      // bloom filters for conditional loops
    uint[] backrefed;                      // bit array of backreferenced submatches
    Kickstart!Char kickstart;
    MatcherFactory!Char factory;           // produces optimal matcher for this pattern
    immutable(Char)[] pattern;             // copy of pattern to serve as cache key

    const(Regex) withFactory(MatcherFactory!Char factory) pure const @trusted
    {
        auto r = cast() this;
        r.factory = factory;
        return r;
    }

    const(Regex) withFlags(uint newFlags) pure const @trusted
    {
        auto r = cast() this;
        r.flags = newFlags;
        return r;
    }

    const(Regex) withCode(const(Bytecode)[] code) pure const @trusted
    {
        auto r = cast() this;
        r.ir = code.dup; // TODO: sidestep const instead?
        return r;
    }

    const(Regex) withNGroup(uint nGroup) pure const @trusted
    {
        auto r = cast() this;
        r.ngroup = nGroup;
        return r;
    }

    //bit access helper
    uint isBackref(uint n)
    {
        if (n/32 >= backrefed.length)
            return 0;
        return backrefed[n / 32] & (1 << (n & 31));
    }

    //check if searching is not needed
    void checkIfOneShot()
    {
    L_CheckLoop:
        for (uint i = 0; i < ir.length; i += ir[i].length)
        {
            switch (ir[i].code)
            {
                case IR.Bof:
                    flags |= RegexInfo.oneShot;
                    break L_CheckLoop;
                case IR.GroupStart, IR.GroupEnd, IR.Bol, IR.Eol, IR.Eof,
                IR.Wordboundary, IR.Notwordboundary:
                    break;
                default:
                    break L_CheckLoop;
            }
        }
    }

    //print out disassembly a program's IR
    @trusted debug(std_regex_parser) void print() const
    {//@@@BUG@@@ write is system
        for (uint i = 0; i < ir.length; i += ir[i].length)
        {
            writefln("%d\t%s ", i, disassemble(ir, i, dict));
        }
        writeln("Total merge table size: ", hotspotTableSize);
        writeln("Max counter nesting depth: ", maxCounterDepth);
    }

    public string toString()() const
    {
        import std.format : format;
        static if (is(typeof(pattern) : string))
            alias patternString = pattern;
        else
        {
            import std.conv : to;
            auto patternString = conv.to!string(pattern);
        }
        auto quotedEscapedPattern = format("%(%s %)", [patternString]);
        auto flagString = regexOptionsToString(flags);
        return "Regex!" ~ Char.stringof ~ "(" ~ quotedEscapedPattern ~ ", \"" ~ flagString ~ "\")";
    }
}

// The stuff below this point is temporarrily part of IR module
// but may need better place in the future (all internals)
package(std.regex):

//Simple UTF-string abstraction compatible with stream interface
struct Input(Char)
if (is(Char :dchar))
{
    import std.utf : decode;
    alias DataIndex = size_t;
    enum bool isLoopback = false;
    alias String = const(Char)[];
    String _origin;
    size_t _index;

    //constructs Input object out of plain string
    this(String input, size_t idx = 0)
    {
        _origin = input;
        _index = idx;
    }

    //codepoint at current stream position
    pragma(inline, true) bool nextChar(ref dchar res, ref size_t pos)
    {
        pos = _index;
        // DMD's inliner hates multiple return functions
        // but can live with single statement if/else bodies
        bool n = !(_index == _origin.length);
        if (n)
            res = decode(_origin, _index);
        return n;
    }
    @property bool atEnd(){
        return _index == _origin.length;
    }
    bool search(Kickstart)(ref const Kickstart kick, ref dchar res, ref size_t pos)
    {
        size_t idx = kick.search(_origin, _index);
        _index = idx;
        return nextChar(res, pos);
    }

    //index of at End position
    @property size_t lastIndex(){   return _origin.length; }

    //support for backtracker engine, might not be present
    void reset(size_t index){   _index = index;  }

    String opSlice(size_t start, size_t end){   return _origin[start .. end]; }

    auto loopBack(size_t index){   return BackLooper!Input(this, index); }
}

struct BackLooperImpl(Input)
{
    import std.utf : strideBack;
    alias DataIndex = size_t;
    alias String = Input.String;
    enum bool isLoopback = true;
    String _origin;
    size_t _index;
    this(Input input, size_t index)
    {
        _origin = input._origin;
        _index = index;
    }
    this(String input)
    {
        _origin = input;
        _index = input.length;
    }
    @trusted bool nextChar(ref dchar res,ref size_t pos)
    {
        pos = _index;
        if (_index == 0)
            return false;

        res = _origin[0.._index].back;
        _index -= strideBack(_origin, _index);

        return true;
    }
    @property atEnd(){ return _index == 0 || _index == strideBack(_origin, _index); }
    auto loopBack(size_t index){   return Input(_origin, index); }

    //support for backtracker engine, might not be present
    //void reset(size_t index){   _index = index ? index-std.utf.strideBack(_origin, index) : 0;  }
    void reset(size_t index){   _index = index;  }

    String opSlice(size_t start, size_t end){   return _origin[end .. start]; }
    //index of at End position
    @property size_t lastIndex(){   return 0; }
}

template BackLooper(E)
{
    static if (is(E : BackLooperImpl!U, U))
    {
        alias BackLooper = U;
    }
    else
    {
        alias BackLooper = BackLooperImpl!E;
    }
}

//both helpers below are internal, on its own are quite "explosive"
//unsafe, no initialization of elements
@system pure T[] mallocArray(T)(size_t len)
{
    import core.memory : pureMalloc;
    return (cast(T*) pureMalloc(len * T.sizeof))[0 .. len];
}

//very unsafe, no initialization
@system T[] arrayInChunk(T)(size_t len, ref void[] chunk)
{
    auto ret = (cast(T*) chunk.ptr)[0 .. len];
    chunk = chunk[len * T.sizeof .. $];
    return ret;
}

//
@trusted uint lookupNamedGroup(String)(const(NamedGroup)[] dict, String name)
{//equal is @system?
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.conv : text;
    import std.range : assumeSorted;

    auto fnd = assumeSorted!"cmp(a,b) < 0"(map!"a.name"(dict)).lowerBound(name).length;
    enforce(fnd < dict.length && equal(dict[fnd].name, name),
        text("no submatch named ", name));
    return dict[fnd].group;
}

// whether ch is one of unicode newline sequences
// 0-arg template due to https://issues.dlang.org/show_bug.cgi?id=10985
bool endOfLine()(dchar front, bool seenCr)
{
    return ((front == '\n') ^ seenCr) || front == '\r'
    || front == NEL || front == LS || front == PS;
}

// 0-arg template due to https://issues.dlang.org/show_bug.cgi?id=10985
bool startOfLine()(dchar back, bool seenNl)
{
    return ((back == '\r') ^ seenNl) || back == '\n'
    || back == NEL || back == LS || back == PS;
}

///Exception object thrown in case of errors during regex compilation.
public class RegexException : Exception
{
    mixin basicExceptionCtors;
}

// simple 128-entry bit-table used with a hash function
struct BitTable {
    uint[4] filter;

    this(CodepointSet set){
        foreach (iv; set.byInterval)
        {
            foreach (v; iv.a .. iv.b)
                add(v);
        }
    }

    void add()(dchar ch){
        immutable i = index(ch);
        filter[i >> 5]  |=  1<<(i & 31);
    }
    // non-zero -> might be present, 0 -> absent
    bool opIndex()(dchar ch) const{
        immutable i = index(ch);
        return (filter[i >> 5]>>(i & 31)) & 1;
    }

    static uint index()(dchar ch){
        return ((ch >> 7) ^ ch) & 0x7F;
    }
}

struct CharMatcher {
    BitTable ascii; // fast path for ASCII
    Trie trie;      // slow path for Unicode

    this(CodepointSet set)
    {
        auto asciiSet = set & unicode.ASCII;
        ascii = BitTable(asciiSet);
        trie = makeTrie(set);
    }

    bool opIndex()(dchar ch) const
    {
        if (ch < 0x80)
            return ascii[ch];
        else
            return trie[ch];
    }
}

// Internal non-resizeble array, switches between inline storage and CoW
// POD-only
struct SmallFixedArray(T, uint SMALL=3)
if (!hasElaborateDestructor!T)
{
    import std.internal.memory : enforceMalloc;
    import core.memory : pureFree;
    static struct Payload
    {
        size_t refcount;
        T[0] placeholder;
        inout(T)* ptr() inout { return placeholder.ptr; }
    }
    static assert(Payload.sizeof == size_t.sizeof);
    union
    {
        Payload* big;
        T[SMALL] small;
    }
    size_t _sizeMask;
    enum BIG_MASK = size_t(1)<<(8*size_t.sizeof-1);
    enum SIZE_MASK = ~BIG_MASK;

    @property bool isBig() const { return (_sizeMask & BIG_MASK) != 0; }
    @property size_t length() const { return _sizeMask & SIZE_MASK; }

    this(size_t size)
    {
        if (size <= SMALL)
        {
            small[] = T.init;
            _sizeMask = size;
        }
        else
        {
            big = cast(Payload*) enforceMalloc(Payload.sizeof + T.sizeof*size);
            big.refcount = 1;
            _sizeMask = size | BIG_MASK;
        }
    }

    private @trusted @property inout(T)[] internalSlice() inout
    {
        return isBig ? big.ptr[0 .. length] : small[0 .. length];
    }

    this(this) @trusted
    {
        if (isBig)
        {
            big.refcount++;
        }
    }

    bool opEquals(SmallFixedArray a)
    {
        return internalSlice[] == a.internalSlice[];
    }

    size_t toHash() const
    {
        return hashOf(internalSlice[]);
    }

    ref inout(T) opIndex(size_t idx) inout
    {
        return internalSlice[idx];
    }

    // accesses big to test self-referencing so not @safe
    @trusted ref opAssign(SmallFixedArray arr)
    {
        if (isBig)
        {
            if (arr.isBig)
            {
                if (big is arr.big) return this; // self-assign
                else
                {
                    abandonRef();
                    _sizeMask = arr._sizeMask;
                    big = arr.big;
                    big.refcount++;
                }
            }
            else
            {
                abandonRef();
                _sizeMask = arr._sizeMask;
                small = arr.small;
            }
        }
        else
        {
            if (arr.isBig)
            {
                _sizeMask = arr._sizeMask;
                big = arr.big;
                big.refcount++;
            }
            else
            {
                _sizeMask = arr._sizeMask;
                small = arr.small;
            }
        }
        return this;
    }

    void mutate(scope void delegate(T[]) pure filler)
    {
        if (isBig && big.refcount != 1) // copy on write
        {
            auto oldSizeMask = _sizeMask;
            auto newbig = cast(Payload*) enforceMalloc(Payload.sizeof + T.sizeof*length);
            newbig.refcount = 1;
            abandonRef();
            big = newbig;
            _sizeMask = oldSizeMask;
        }
        filler(internalSlice);
    }

    ~this()
    {
        if (isBig)
        {
            abandonRef();
        }
    }

    @trusted private void abandonRef()
    {
        assert(isBig);
        if (--big.refcount == 0)
        {
            pureFree(big);
            _sizeMask = 0;
            assert(!isBig);
        }
    }
}

@system unittest
{
    alias SA = SmallFixedArray!(int, 2);
    SA create(int[] data)
    {
        SA a = SA(data.length);
        a.mutate((slice) { slice[] = data[]; });
        assert(a.internalSlice == data);
        return a;
    }

    {
        SA a;
        a = SA(1);
        assert(a.length == 1);
        a = SA.init;
        assert(a.length == 0);
    }

    {
        SA a, b, c, d;
        assert(a.length == 0);
        assert(a.internalSlice == b.internalSlice);
        a = create([1]);
        assert(a.internalSlice == [1]);
        b = create([2, 3]);
        assert(b.internalSlice == [2, 3]);
        c = create([3, 4, 5]);
        d = create([5, 6, 7, 8]);
        assert(c.isBig);
        a = c;
        assert(a.isBig);
        assert(a.big is c.big);
        assert(a.big.refcount == 2);
        assert(a.internalSlice == [3, 4, 5]);
        assert(c.internalSlice == [3, 4, 5]);
        a = b;
        assert(!a.isBig);
        assert(a.internalSlice == [2, 3]);
        assert(c.big.refcount == 1);
        a = c;
        assert(c.big.refcount == 2);

        // mutate copies on write if ref-count is not 1
        a.mutate((slice){ slice[] = 1; });
        assert(a.internalSlice == [1, 1, 1]);
        assert(c.internalSlice == [3, 4, 5]);
        assert(a.isBig && c.isBig);
        assert(a.big.refcount == 1);
        assert(c.big.refcount == 1);

        auto e = d;
        assert(e.big.refcount == 2);
        auto f = d;
        f = a;
        assert(f.isBig);
        assert(f.internalSlice == [1, 1, 1]);
        assert(f.big.refcount == 2); // a & f
        assert(e.big.refcount == 2); // d & e
        a = c;
        assert(f.big.refcount == 1); // f
        assert(e.big.refcount == 2); // d & e
        a = a;
        a = a;
        a = a;
        assert(a.big.refcount == 2); // a & c
    }
}

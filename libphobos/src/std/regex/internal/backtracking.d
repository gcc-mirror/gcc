/*
    Implementation of backtracking std.regex engine.
    Contains both compile-time and run-time versions.
*/
module std.regex.internal.backtracking;

package(std.regex):

import core.stdc.stdlib, std.range.primitives, std.traits, std.typecons;
import std.regex.internal.ir;

import core.memory : pureMalloc, pureFree;

/+
    BacktrackingMatcher implements backtracking scheme of matching
    regular expressions.
+/
@trusted class BacktrackingMatcher(Char, Stream = Input!Char) : Matcher!Char
if (is(Char : dchar))
{
    alias DataIndex = Stream.DataIndex;
    struct State
    {//top bit in pc is set if saved along with matches
        DataIndex index;
        uint pc, counter, infiniteNesting;
    }
    static assert(State.sizeof % size_t.sizeof == 0);
    enum stateSize = State.sizeof / size_t.sizeof;
    enum initialStack = 1 << 11; // items in a block of segmented stack
    alias String = const(Char)[];
    alias RegEx = Regex!Char;
    alias MatchFn = bool function(BacktrackingMatcher) pure;
    const RegEx re;         // regex program
    MatchFn nativeFn; // native code for that program
    // Stream state
    Stream s;
    DataIndex index;
    dchar front;
    bool exhausted;
    // Backtracking machine state
    uint pc, counter;
    DataIndex lastState = 0;    // Top of state stack
    uint infiniteNesting;
    size_t[] memory;
    Trace[]  merge;
    static struct Trace
    {
        ulong mask;
        size_t offset;

        bool mark(size_t idx)
        {
            immutable d = idx - offset;
            if (d < 64) // including overflow
            {
                immutable p = mask & (1UL << d);
                mask |= 1UL << d;
                return p != 0;
            }
            else
            {
                offset = idx;
                mask = 1;
                return false;
            }
        }
    }
    //local slice of matches, global for backref
    Group!DataIndex[] matches, backrefed;
    size_t _refCount;
final:

    override @property ref size_t refCount() { return _refCount; }
    override @property ref const(RegEx) pattern(){ return re; }

    enum kicked = __traits(hasMember, Stream, "search");

    static size_t initialMemory(const ref RegEx re)
    {
        return stackSize(re)*size_t.sizeof + re.hotspotTableSize*Trace.sizeof;
    }

    static size_t stackSize(const ref RegEx re)
    {
        size_t itemSize = stateSize
            + re.ngroup * (Group!DataIndex).sizeof / size_t.sizeof;
        return initialStack * itemSize + 2;
    }

    @property bool atStart(){ return index == 0; }

    @property bool atEnd(){ return index == s.lastIndex && s.atEnd; }

    void next()
    {
        if (!s.nextChar(front, index))
            index = s.lastIndex;
    }

    void search()
    {
        static if (kicked)
        {
            if (!s.search(re.kickstart, front, index))
            {
                index = s.lastIndex;
            }
        }
        else
            next();
    }

    //
    void newStack()
    {
        auto chunk = mallocArray!(size_t)(stackSize(re));
        chunk[0] = cast(size_t)(memory.ptr);
        chunk[1] = lastState;
        memory = chunk[2..$];
        lastState = 0;
    }

    bool prevStack()
    {
        // pointer to previous block
        size_t* prev = cast(size_t*) memory.ptr[-2];
        if (!prev)
        {
            // The last segment is freed in RegexMatch
            return false;
        }
        else
        {
            import core.memory : pureFree;
            // memory used in previous block
            size_t size = memory.ptr[-1];
            pureFree(memory.ptr-2);
            memory = prev[0 .. size];
            lastState = size;
            return true;
        }
    }

    void initExternalMemory(void[] memBlock)
    {
        merge = arrayInChunk!(Trace)(re.hotspotTableSize, memBlock);
        merge[] = Trace.init;
        memory = cast(size_t[]) memBlock;
        memory[0] = 0; // hidden pointer
        memory[1] = 0; // used size
        memory = memory[2..$];
    }

    void initialize(ref const RegEx program, Stream stream, void[] memBlock)
    {
        s = stream;
        exhausted = false;
        initExternalMemory(memBlock);
        backrefed = null;
    }

    override void dupTo(Matcher!Char m, void[] memBlock)
    {
        auto backtracking = cast(BacktrackingMatcher) m;
        backtracking.s = s;
        backtracking.front = front;
        backtracking.index = index;
        backtracking.exhausted = exhausted;
        backtracking.initExternalMemory(memBlock);
    }

    override Matcher!Char rearm(in Char[] data)
    {
        merge[] = Trace.init;
        exhausted = false;
        s = Stream(data);
        next();
        return this;
    }

    this(ref const RegEx program, Stream stream, void[] memBlock, dchar ch, DataIndex idx)
    {
        _refCount = 1;
        re = program;
        nativeFn = null;
        initialize(program, stream, memBlock);
        front = ch;
        index = idx;
    }

    this(ref const RegEx program, MatchFn func, Stream stream, void[] memBlock)
    {
        _refCount = 1;
        re = program;
        initialize(program, stream, memBlock);
        nativeFn = func;
        next();
    }

    this(ref const RegEx program, Stream stream, void[] memBlock)
    {
        _refCount = 1;
        re = program;
        nativeFn = null;
        initialize(program, stream, memBlock);
        next();
    }

    auto fwdMatcher(ref const RegEx re, void[] memBlock)
    {
        alias BackMatcher = BacktrackingMatcher!(Char, Stream);
        auto fwdMatcher = new BackMatcher(re, s, memBlock, front, index);
        return fwdMatcher;
    }

    auto bwdMatcher(ref const RegEx re, void[] memBlock)
    {
        alias BackMatcher = BacktrackingMatcher!(Char, typeof(s.loopBack(index)));
        auto fwdMatcher =
            new BackMatcher(re, s.loopBack(index), memBlock);
        return fwdMatcher;
    }

    //
    int matchFinalize()
    {
        immutable start = index;
        immutable val = matchImpl();
        if (val)
        {//stream is updated here
            matches[0].begin = start;
            matches[0].end = index;
            if (!(re.flags & RegexOption.global) || atEnd)
                exhausted = true;
            if (start == index)//empty match advances input
                next();
            return val;
        }
        else
            return 0;
    }

    //lookup next match, fill matches with indices into input
    override int match(Group!DataIndex[] matches)
    {
        debug(std_regex_matcher)
        {
            writeln("------------------------------------------");
        }
        if (exhausted) //all matches collected
            return false;
        this.matches = matches;
        if (re.flags & RegexInfo.oneShot)
        {
            exhausted = true;
            const DataIndex start = index;
            immutable m = matchImpl();
            if (m)
            {
                matches[0].begin = start;
                matches[0].end = index;
            }
            return m;
        }
        static if (kicked)
        {
            if (!re.kickstart.empty)
            {
                for (;;)
                {
                    immutable val = matchFinalize();
                    if (val)
                        return val;
                    else
                    {
                        if (atEnd)
                            break;
                        search();
                        if (atEnd)
                        {
                            exhausted = true;
                            return matchFinalize();
                        }
                    }
                }
                exhausted = true;
                return 0; //early return
            }
        }
        //no search available - skip a char at a time
        for (;;)
        {
            immutable val = matchFinalize();
            if (val)
                return val;
            else
            {
                if (atEnd)
                    break;
                next();
                if (atEnd)
                {
                    exhausted = true;
                    return matchFinalize();
                }
            }
        }
        exhausted = true;
        return 0;
    }

    /+
        match subexpression against input,
        results are stored in matches
    +/
    int matchImpl() pure
    {
        if (nativeFn)
        {
            debug(std_regex_ctr) writeln("using C-T matcher");
            return nativeFn(this);
        }
        else
        {
            pc = 0;
            counter = 0;
            lastState = 0;
            infiniteNesting = 0;
            matches[] = Group!DataIndex.init;
            auto start = s._index;
            debug(std_regex_matcher)
                writeln("Try match starting at ", s[index .. s.lastIndex]);
            for (;;)
            {
                debug(std_regex_matcher)
                    writefln("PC: %s\tCNT: %s\t%s \tfront: %s src: %s",
                        pc, counter, disassemble(re.ir, pc, re.dict),
                        front, s._index);
                switch (re.ir[pc].code)
                {
                case IR.OrChar://assumes IRL!(OrChar) == 1
                    if (atEnd)
                        goto L_backtrack;
                    uint len = re.ir[pc].sequence;
                    uint end = pc + len;
                    if (re.ir[pc].data != front && re.ir[pc+1].data != front)
                    {
                        for (pc = pc+2; pc < end; pc++)
                            if (re.ir[pc].data == front)
                                break;
                        if (pc == end)
                            goto L_backtrack;
                    }
                    pc = end;
                    next();
                    break;
                case IR.Char:
                    if (atEnd || front != re.ir[pc].data)
                        goto L_backtrack;
                    pc += IRL!(IR.Char);
                    next();
                break;
                case IR.Any:
                    if (atEnd)
                        goto L_backtrack;
                    pc += IRL!(IR.Any);
                    next();
                    break;
                case IR.CodepointSet:
                    if (atEnd || !re.charsets[re.ir[pc].data].scanFor(front))
                        goto L_backtrack;
                    next();
                    pc += IRL!(IR.CodepointSet);
                    break;
                case IR.Trie:
                    if (atEnd || !re.matchers[re.ir[pc].data][front])
                        goto L_backtrack;
                    next();
                    pc += IRL!(IR.Trie);
                    break;
                case IR.Wordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if (atStart && wordMatcher[front])
                    {
                        pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if (atEnd && s.loopBack(index).nextChar(back, bi)
                            && wordMatcher[back])
                    {
                        pc += IRL!(IR.Wordboundary);
                        break;
                    }
                    else if (s.loopBack(index).nextChar(back, bi))
                    {
                        immutable af = wordMatcher[front];
                        immutable ab = wordMatcher[back];
                        if (af ^ ab)
                        {
                            pc += IRL!(IR.Wordboundary);
                            break;
                        }
                    }
                    goto L_backtrack;
                case IR.Notwordboundary:
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if (atStart && wordMatcher[front])
                        goto L_backtrack;
                    else if (atEnd && s.loopBack(index).nextChar(back, bi)
                            && wordMatcher[back])
                        goto L_backtrack;
                    else if (s.loopBack(index).nextChar(back, bi))
                    {
                        immutable af = wordMatcher[front];
                        immutable ab = wordMatcher[back];
                        if (af ^ ab)
                            goto L_backtrack;
                    }
                    pc += IRL!(IR.Wordboundary);
                    break;
                case IR.Bof:
                    if (atStart)
                        pc += IRL!(IR.Bol);
                    else
                        goto L_backtrack;
                    break;
                case IR.Bol:
                    dchar back;
                    DataIndex bi;
                    if (atStart)
                        pc += IRL!(IR.Bol);
                    else if (s.loopBack(index).nextChar(back,bi)
                        && endOfLine(back, front == '\n'))
                    {
                        pc += IRL!(IR.Bol);
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.Eof:
                    if (atEnd)
                        pc += IRL!(IR.Eol);
                    else
                        goto L_backtrack;
                    break;
                case IR.Eol:
                    dchar back;
                    DataIndex bi;
                    debug(std_regex_matcher) writefln("EOL (front 0x%x) %s", front, s[index .. s.lastIndex]);
                    //no matching inside \r\n
                    if (atEnd || (endOfLine(front, s.loopBack(index).nextChar(back,bi)
                            && back == '\r')))
                    {
                        pc += IRL!(IR.Eol);
                    }
                    else
                        goto L_backtrack;
                    break;
                case IR.InfiniteStart, IR.InfiniteQStart:
                    pc += re.ir[pc].data + IRL!(IR.InfiniteStart);
                    //now pc is at end IR.Infinite(Q)End
                    uint len = re.ir[pc].data;
                    if (re.ir[pc].code == IR.InfiniteEnd)
                    {
                        pushState(pc+IRL!(IR.InfiniteEnd), counter);
                        pc -= len;
                    }
                    else
                    {
                        pushState(pc - len, counter);
                        pc += IRL!(IR.InfiniteEnd);
                    }
                    break;
                case IR.InfiniteBloomStart:
                    pc += re.ir[pc].data + IRL!(IR.InfiniteBloomStart);
                    //now pc is at end IR.InfiniteBloomEnd
                    immutable len = re.ir[pc].data;
                    immutable filterIdx = re.ir[pc+2].raw;
                    if (re.filters[filterIdx][front])
                        pushState(pc+IRL!(IR.InfiniteBloomEnd), counter);
                    pc -= len;
                    break;
                case IR.RepeatStart, IR.RepeatQStart:
                    pc += re.ir[pc].data + IRL!(IR.RepeatStart);
                    break;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    if (merge[re.ir[pc + 1].raw+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }
                    //len, step, min, max
                    immutable len = re.ir[pc].data;
                    immutable step =  re.ir[pc+2].raw;
                    immutable min = re.ir[pc+3].raw;
                    immutable max = re.ir[pc+4].raw;
                    if (counter < min)
                    {
                        counter += step;
                        pc -= len;
                    }
                    else if (counter < max)
                    {
                        if (re.ir[pc].code == IR.RepeatEnd)
                        {
                            pushState(pc + IRL!(IR.RepeatEnd), counter%step);
                            counter += step;
                            pc -= len;
                        }
                        else
                        {
                            pushState(pc - len, counter + step);
                            counter = counter%step;
                            pc += IRL!(IR.RepeatEnd);
                        }
                    }
                    else
                    {
                        counter = counter%step;
                        pc += IRL!(IR.RepeatEnd);
                    }
                    break;
                case IR.InfiniteEnd:
                case IR.InfiniteQEnd:
                    debug(std_regex_matcher) writeln("Infinited nesting:", infiniteNesting);
                    if (merge[re.ir[pc + 1].raw+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }
                    immutable len = re.ir[pc].data;
                    if (re.ir[pc].code == IR.InfiniteEnd)
                    {
                        pushState(pc + IRL!(IR.InfiniteEnd), counter);
                        pc -= len;
                    }
                    else
                    {
                        pushState(pc-len, counter);
                        pc += IRL!(IR.InfiniteEnd);
                    }
                    break;
                case IR.InfiniteBloomEnd:
                    debug(std_regex_matcher) writeln("Infinited nesting:", infiniteNesting);
                    if (merge[re.ir[pc + 1].raw+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }
                    immutable len = re.ir[pc].data;
                    immutable filterIdx = re.ir[pc+2].raw;
                    if (re.filters[filterIdx][front])
                    {
                        infiniteNesting--;
                        pushState(pc + IRL!(IR.InfiniteBloomEnd), counter);
                        infiniteNesting++;
                    }
                    pc -= len;
                    break;
                case IR.OrEnd:
                    if (merge[re.ir[pc + 1].raw+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }
                    pc += IRL!(IR.OrEnd);
                    break;
                case IR.OrStart:
                    pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    immutable len = re.ir[pc].data;
                    if (re.ir[pc+len].code == IR.GotoEndOr)//not a last one
                    {
                        pushState(pc + len + IRL!(IR.Option), counter); //remember 2nd branch
                    }
                    pc += IRL!(IR.Option);
                    break;
                case IR.GotoEndOr:
                    pc = pc + re.ir[pc].data + IRL!(IR.GotoEndOr);
                    break;
                case IR.GroupStart:
                    immutable n = re.ir[pc].data;
                    matches[n].begin = index;
                    debug(std_regex_matcher)  writefln("IR group #%u starts at %u", n, index);
                    pc += IRL!(IR.GroupStart);
                    break;
                case IR.GroupEnd:
                    immutable n = re.ir[pc].data;
                    matches[n].end = index;
                    debug(std_regex_matcher) writefln("IR group #%u ends at %u", n, index);
                    pc += IRL!(IR.GroupEnd);
                    break;
                case IR.LookaheadStart:
                case IR.NeglookaheadStart:
                    immutable len = re.ir[pc].data;
                    auto save = index;
                    immutable ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    auto mem = pureMalloc(initialMemory(re))[0 .. initialMemory(re)];
                    scope(exit) pureFree(mem.ptr);
                    auto slicedRe = re.withCode(re.ir[
                        pc+IRL!(IR.LookaheadStart) .. pc+IRL!(IR.LookaheadStart)+len+IRL!(IR.LookaheadEnd)
                    ]);
                    static if (Stream.isLoopback)
                    {
                        auto matcher = bwdMatcher(slicedRe, mem);
                    }
                    else
                    {
                        auto matcher = fwdMatcher(slicedRe, mem);
                    }
                    matcher.matches = matches[ms .. me];
                    matcher.backrefed = backrefed.empty ? matches : backrefed;
                    immutable match = (matcher.matchImpl() != 0) ^ (re.ir[pc].code == IR.NeglookaheadStart);
                    s.reset(save);
                    next();
                    if (!match)
                        goto L_backtrack;
                    else
                    {
                        pc += IRL!(IR.LookaheadStart)+len+IRL!(IR.LookaheadEnd);
                    }
                    break;
                case IR.LookbehindStart:
                case IR.NeglookbehindStart:
                    immutable len = re.ir[pc].data;
                    immutable ms = re.ir[pc+1].raw, me = re.ir[pc+2].raw;
                    auto mem = pureMalloc(initialMemory(re))[0 .. initialMemory(re)];
                    scope(exit) pureFree(mem.ptr);
                    auto slicedRe = re.withCode(re.ir[
                        pc + IRL!(IR.LookbehindStart) .. pc + IRL!(IR.LookbehindStart) + len + IRL!(IR.LookbehindEnd)
                    ]);
                    static if (Stream.isLoopback)
                    {
                        alias Matcher = BacktrackingMatcher!(Char, Stream);
                        auto matcher = new Matcher(slicedRe, s, mem, front, index);
                    }
                    else
                    {
                        alias Matcher = BacktrackingMatcher!(Char, typeof(s.loopBack(index)));
                        auto matcher = new Matcher(slicedRe, s.loopBack(index), mem);
                    }
                    matcher.matches = matches[ms .. me];
                    matcher.backrefed  = backrefed.empty ? matches : backrefed;
                    immutable match = (matcher.matchImpl() != 0) ^ (re.ir[pc].code == IR.NeglookbehindStart);
                    if (!match)
                        goto L_backtrack;
                    else
                    {
                        pc += IRL!(IR.LookbehindStart)+len+IRL!(IR.LookbehindEnd);
                    }
                    break;
                case IR.Backref:
                    immutable n = re.ir[pc].data;
                    auto referenced = re.ir[pc].localRef
                            ? s[matches[n].begin .. matches[n].end]
                            : s[backrefed[n].begin .. backrefed[n].end];
                    while (!atEnd && !referenced.empty && front == referenced.front)
                    {
                        next();
                        referenced.popFront();
                    }
                    if (referenced.empty)
                        pc++;
                    else
                        goto L_backtrack;
                    break;
                    case IR.Nop:
                    pc += IRL!(IR.Nop);
                    break;
                case IR.LookaheadEnd:
                case IR.NeglookaheadEnd:
                case IR.LookbehindEnd:
                case IR.NeglookbehindEnd:
                case IR.End:
                    // cleanup stale stack blocks if any
                    while (prevStack()) {}
                    return re.ir[pc].data;
                default:
                    debug(std_regex_debug) printBytecode(re.ir[0..$]);
                    assert(0);
                L_backtrack:
                    if (!popState())
                    {
                        s.reset(start);
                        return 0;
                    }
                }
            }
        }
        assert(0);
    }

    @property size_t stackAvail()
    {
        return memory.length - lastState;
    }

    void stackPush(T)(T val)
    if (!isDynamicArray!T)
    {
        *cast(T*)&memory[lastState] = val;
        enum delta = (T.sizeof+size_t.sizeof/2)/size_t.sizeof;
        lastState += delta;
        debug(std_regex_matcher) writeln("push element SP= ", lastState);
    }

    void stackPush(T)(T[] val)
    {
        static assert(T.sizeof % size_t.sizeof == 0);
        (cast(T*)&memory[lastState])[0 .. val.length]
            = val[0..$];
        lastState += val.length*(T.sizeof/size_t.sizeof);
        debug(std_regex_matcher) writeln("push array SP= ", lastState);
    }

    void stackPop(T)(ref T val)
    if (!isDynamicArray!T)
    {
        enum delta = (T.sizeof+size_t.sizeof/2)/size_t.sizeof;
        lastState -= delta;
        val = *cast(T*)&memory[lastState];
        debug(std_regex_matcher) writeln("pop element SP= ", lastState);
    }

    void stackPop(T)(T[] val)
    {
        stackPop(val);  // call ref version
    }
    void stackPop(T)(ref T[] val)
    {
        lastState -= val.length*(T.sizeof/size_t.sizeof);
        val[0..$] = (cast(T*)&memory[lastState])[0 .. val.length];
        debug(std_regex_matcher) writeln("pop array SP= ", lastState);
    }
    //helper function, saves engine state
    void pushState(uint pc, uint counter)
    {
        if (stateSize + 2 * matches.length > stackAvail)
        {
            newStack();
        }
        *cast(State*)&memory[lastState] =
            State(index, pc, counter, infiniteNesting);
        lastState += stateSize;
        memory[lastState .. lastState + 2 * matches.length] = (cast(size_t[]) matches)[];
        lastState += 2*matches.length;
        debug(std_regex_matcher)
            writefln("Saved(pc=%s) front: %s src: %s",
                pc, front, s[index .. s.lastIndex]);
    }

    //helper function, restores engine state
    bool popState()
    {
        if (!lastState && !prevStack())
            return false;
        lastState -= 2*matches.length;
        auto pm = cast(size_t[]) matches;
        pm[] = memory[lastState .. lastState + 2 * matches.length];
        lastState -= stateSize;
        State* state = cast(State*)&memory[lastState];
        index = state.index;
        pc = state.pc;
        counter = state.counter;
        infiniteNesting = state.infiniteNesting;
        debug(std_regex_matcher)
        {
            writefln("Restored matches", front, s[index .. s.lastIndex]);
            foreach (i, m; matches)
                writefln("Sub(%d) : %s..%s", i, m.begin, m.end);
        }
        s.reset(index);
        next();
        debug(std_regex_matcher)
            writefln("Backtracked (pc=%s) front: %s src: %s",
                pc, front, s[index .. s.lastIndex]);
        return true;
    }
}

//very shitty string formatter, $$ replaced with next argument converted to string
@trusted string ctSub( U...)(string format, U args)
{
    import std.conv : to;
    bool seenDollar;
    foreach (i, ch; format)
    {
        if (ch == '$')
        {
            if (seenDollar)
            {
                static if (args.length > 0)
                {
                    return  format[0 .. i - 1] ~ to!string(args[0])
                        ~ ctSub(format[i + 1 .. $], args[1 .. $]);
                }
                else
                    assert(0);
            }
            else
                seenDollar = true;
        }
        else
            seenDollar = false;

    }
    return format;
}

struct CtContext
{
    import std.conv : to, text;
    //dirty flags
    bool counter;
    //to mark the portion of matches to save
    int match, total_matches;
    int reserved;
    const(CodepointInterval)[][] charsets;


    //state of codegenerator
    static struct CtState
    {
        string code;
        int addr;
    }

    this(Char)(ref const Regex!Char re)
    {
        match = 1;
        reserved = 1; //first match is skipped
        total_matches = re.ngroup;
        foreach (ref set; re.charsets)
        {
            charsets ~= set.intervals;
        }
    }

    CtContext lookaround(uint s, uint e)
    {
        CtContext ct;
        ct.total_matches = e - s;
        ct.match = 1;
        return ct;
    }

    //restore state having current context
    string restoreCode()
    {
        string text;
        //stack is checked in L_backtrack
        text ~= counter
            ? "
                    stackPop(counter);"
            : "
                    counter = 0;";
        if (match < total_matches)
        {
            text ~= ctSub("
                    stackPop(matches[$$..$$]);", reserved, match);
            text ~= ctSub("
                    matches[$$..$] = typeof(matches[0]).init;", match);
        }
        else
            text ~= ctSub("
                    stackPop(matches[$$..$]);", reserved);
        return text;
    }

    //save state having current context
    string saveCode(uint pc, string count_expr="counter")
    {
        string text = ctSub("
                    if (stackAvail < $$*(Group!(DataIndex)).sizeof/size_t.sizeof + $$)
                    {
                        newStack();
                    }", match - reserved, cast(int) counter + 2);
        if (match < total_matches)
            text ~= ctSub("
                    stackPush(matches[$$..$$]);", reserved, match);
        else
            text ~= ctSub("
                    stackPush(matches[$$..$]);", reserved);
        text ~= counter ? ctSub("
                    stackPush($$);", count_expr) : "";
        text ~= ctSub("
                    stackPush(index); stackPush($$); \n", pc);
        return text;
    }

    //
    CtState ctGenBlock(const(Bytecode)[] ir, int addr)
    {
        CtState result;
        result.addr = addr;
        while (!ir.empty)
        {
            auto n = ctGenGroup(ir, result.addr);
            result.code ~= n.code;
            result.addr = n.addr;
        }
        return result;
    }

    //
    CtState ctGenGroup(ref const(Bytecode)[] ir, int addr)
    {
        import std.algorithm.comparison : max;
        auto bailOut = "goto L_backtrack;";
        auto nextInstr = ctSub("goto case $$;", addr+1);
        CtState r;
        assert(!ir.empty);
        switch (ir[0].code)
        {
        case IR.InfiniteStart,  IR.InfiniteBloomStart,IR.InfiniteQStart, IR.RepeatStart, IR.RepeatQStart:
            immutable infLoop =
                ir[0].code == IR.InfiniteStart || ir[0].code == IR.InfiniteQStart ||
                ir[0].code == IR.InfiniteBloomStart;

            counter = counter ||
                ir[0].code == IR.RepeatStart || ir[0].code == IR.RepeatQStart;
            immutable len = ir[0].data;
            auto nir = ir[ir[0].length .. ir[0].length+len];
            r = ctGenBlock(nir, addr+1);
            //start/end codegen
            //r.addr is at last test+ jump of loop, addr+1 is body of loop
            nir = ir[ir[0].length + len .. $];
            r.code = ctGenFixupCode(ir[0 .. ir[0].length], addr, r.addr) ~ r.code;
            r.code ~= ctGenFixupCode(nir, r.addr, addr+1);
            r.addr += 2;   //account end instruction + restore state
            ir = nir;
            break;
        case IR.OrStart:
            immutable len = ir[0].data;
            auto nir = ir[ir[0].length .. ir[0].length+len];
            r = ctGenAlternation(nir, addr);
            ir = ir[ir[0].length + len .. $];
            assert(ir[0].code == IR.OrEnd);
            ir = ir[ir[0].length..$];
            break;
        case IR.LookaheadStart:
        case IR.NeglookaheadStart:
        case IR.LookbehindStart:
        case IR.NeglookbehindStart:
            immutable len = ir[0].data;
            immutable behind = ir[0].code == IR.LookbehindStart || ir[0].code == IR.NeglookbehindStart;
            immutable negative = ir[0].code == IR.NeglookaheadStart || ir[0].code == IR.NeglookbehindStart;
            string fwdType = "typeof(fwdMatcher(re, []))";
            string bwdType = "typeof(bwdMatcher(re, []))";
            string fwdCreate = "fwdMatcher(re, mem)";
            string bwdCreate = "bwdMatcher(re, mem)";
            immutable start = IRL!(IR.LookbehindStart);
            immutable end = IRL!(IR.LookbehindStart)+len+IRL!(IR.LookaheadEnd);
            CtContext context = lookaround(ir[1].raw, ir[2].raw); //split off new context
            auto slice = ir[start .. end];
            r.code ~= ctSub(`
            case $$: //fake lookaround "atom"
                    static if (typeof(matcher.s).isLoopback)
                        alias Lookaround = $$;
                    else
                        alias Lookaround = $$;
                    static bool matcher_$$(Lookaround matcher) @trusted
                    {
                        //(neg)lookaround piece start
                        $$
                        //(neg)lookaround piece ends
                    }
                    auto save = index;
                    auto mem = pureMalloc(initialMemory(re))[0 .. initialMemory(re)];
                    scope(exit) pureFree(mem.ptr);
                    static if (typeof(matcher.s).isLoopback)
                        auto lookaround = $$;
                    else
                        auto lookaround = $$;
                    lookaround.matches = matches[$$..$$];
                    lookaround.backrefed = backrefed.empty ? matches : backrefed;
                    lookaround.nativeFn = &matcher_$$; //hookup closure's binary code
                    int match = $$;
                    s.reset(save);
                    next();
                    if (match)
                        $$
                    else
                        $$`, addr,
                        behind ? fwdType : bwdType, behind ? bwdType : fwdType,
                        addr, context.ctGenRegEx(slice),
                        behind ? fwdCreate : bwdCreate, behind ? bwdCreate : fwdCreate,
                        ir[1].raw, ir[2].raw, //start - end of matches slice
                        addr,
                        negative ? "!lookaround.matchImpl()" : "lookaround.matchImpl()",
                        nextInstr, bailOut);
            ir = ir[end .. $];
            r.addr = addr + 1;
            break;
        case IR.LookaheadEnd: case IR.NeglookaheadEnd:
        case IR.LookbehindEnd: case IR.NeglookbehindEnd:
            ir = ir[IRL!(IR.LookaheadEnd) .. $];
            r.addr = addr;
            break;
        default:
            assert(ir[0].isAtom,  text(ir[0].mnemonic));
            r = ctGenAtom(ir, addr);
        }
        return r;
    }

    //generate source for bytecode contained  in OrStart ... OrEnd
    CtState ctGenAlternation(const(Bytecode)[] ir, int addr)
    {
        CtState[] pieces;
        CtState r;
        enum optL = IRL!(IR.Option);
        for (;;)
        {
            assert(ir[0].code == IR.Option);
            auto len = ir[0].data;
            if (optL+len < ir.length  && ir[optL+len].code == IR.Option)//not a last option
            {
                auto nir = ir[optL .. optL+len-IRL!(IR.GotoEndOr)];
                r = ctGenBlock(nir, addr+2);//space for Option + restore state
                //r.addr+1 to account GotoEndOr  at end of branch
                r.code = ctGenFixupCode(ir[0 .. ir[0].length], addr, r.addr+1) ~ r.code;
                addr = r.addr+1;//leave space for GotoEndOr
                pieces ~= r;
                ir = ir[optL + len .. $];
            }
            else
            {
                pieces ~= ctGenBlock(ir[optL..$], addr);
                addr = pieces[$-1].addr;
                break;
            }
        }
        r = pieces[0];
        for (uint i = 1; i < pieces.length; i++)
        {
            r.code ~= ctSub(`
                case $$:
                    goto case $$; `, pieces[i-1].addr, addr);
            r.code ~= pieces[i].code;
        }
        r.addr = addr;
        return r;
    }

    // generate fixup code for instruction in ir,
    // fixup means it has an alternative way for control flow
    string ctGenFixupCode(const(Bytecode)[] ir, int addr, int fixup)
    {
        return ctGenFixupCode(ir, addr, fixup); // call ref Bytecode[] version
    }
    string ctGenFixupCode(ref const(Bytecode)[] ir, int addr, int fixup)
    {
        string r;
        string testCode;
        r = ctSub(`
                case $$: debug(std_regex_matcher) writeln("#$$");`,
                    addr, addr);
        switch (ir[0].code)
        {
        case IR.InfiniteStart, IR.InfiniteQStart, IR.InfiniteBloomStart:
            r ~= ctSub( `
                    goto case $$;`, fixup);
            ir = ir[ir[0].length..$];
            break;
        case IR.InfiniteEnd:
            testCode = ctQuickTest(ir[IRL!(IR.InfiniteEnd) .. $],addr + 1);
            r ~= ctSub( `
                    if (merge[$$+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }

                    $$
                    {
                        $$
                    }
                    goto case $$;
                case $$: //restore state and go out of loop
                    $$
                    goto case;`, ir[1].raw, testCode, saveCode(addr+1), fixup,
                    addr+1, restoreCode());
            ir = ir[ir[0].length..$];
            break;
        case IR.InfiniteBloomEnd:
            //TODO: check bloom filter and skip on failure
            testCode = ctQuickTest(ir[IRL!(IR.InfiniteBloomEnd) .. $],addr + 1);
            r ~= ctSub( `
                    if (merge[$$+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }

                    $$
                    {
                        $$
                    }
                    goto case $$;
                case $$: //restore state and go out of loop
                    $$
                    goto case;`, ir[1].raw, testCode, saveCode(addr+1), fixup,
                    addr+1, restoreCode());
            ir = ir[ir[0].length..$];
            break;
        case IR.InfiniteQEnd:
            testCode = ctQuickTest(ir[IRL!(IR.InfiniteEnd) .. $],addr + 1);
            auto altCode = testCode.length ? ctSub("else goto case $$;", fixup) : "";
            r ~= ctSub( `
                    if (merge[$$+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }

                    $$
                    {
                        $$
                        goto case $$;
                    }
                    $$
                case $$://restore state and go inside loop
                    $$
                    goto case $$;`, ir[1].raw,
                    testCode, saveCode(addr+1), addr+2, altCode,
                    addr+1, restoreCode(), fixup);
            ir = ir[ir[0].length..$];
            break;
        case IR.RepeatStart, IR.RepeatQStart:
            r ~= ctSub( `
                    goto case $$;`, fixup);
            ir = ir[ir[0].length..$];
            break;
         case IR.RepeatEnd, IR.RepeatQEnd:
            //len, step, min, max
            immutable len = ir[0].data;
            immutable step = ir[2].raw;
            immutable min = ir[3].raw;
            immutable max = ir[4].raw;
            r ~= ctSub(`
                    if (merge[$$+counter].mark(index))
                    {
                        // merged!
                        goto L_backtrack;
                    }
                    if (counter < $$)
                    {
                        debug(std_regex_matcher) writeln("RepeatEnd min case pc=", $$);
                        counter += $$;
                        goto case $$;
                    }`,  ir[1].raw, min, addr, step, fixup);
            if (ir[0].code == IR.RepeatEnd)
            {
                string counter_expr = ctSub("counter % $$", step);
                r ~= ctSub(`
                    else if (counter < $$)
                    {
                            $$
                            counter += $$;
                            goto case $$;
                    }`, max, saveCode(addr+1, counter_expr), step, fixup);
            }
            else
            {
                string counter_expr = ctSub("counter % $$", step);
                r ~= ctSub(`
                    else if (counter < $$)
                    {
                        $$
                        counter = counter % $$;
                        goto case $$;
                    }`, max, saveCode(addr+1,counter_expr), step, addr+2);
            }
            r ~= ctSub(`
                    else
                    {
                        counter = counter % $$;
                        goto case $$;
                    }
                case $$: //restore state
                    $$
                    goto case $$;`, step, addr+2, addr+1, restoreCode(),
                    ir[0].code == IR.RepeatEnd ? addr+2 : fixup );
            ir = ir[ir[0].length..$];
            break;
        case IR.Option:
            r ~= ctSub( `
                {
                    $$
                }
                goto case $$;
            case $$://restore thunk to go to the next group
                $$
                goto case $$;`, saveCode(addr+1), addr+2,
                    addr+1, restoreCode(), fixup);
                ir = ir[ir[0].length..$];
            break;
        default:
            assert(0, text(ir[0].mnemonic));
        }
        return r;
    }


    string ctQuickTest(const(Bytecode)[] ir, int id)
    {
        uint pc = 0;
        while (pc < ir.length && ir[pc].isAtom)
        {
            if (ir[pc].code == IR.GroupStart || ir[pc].code == IR.GroupEnd)
            {
                pc++;
            }
            else if (ir[pc].code == IR.Backref)
                break;
            else
            {
                auto code = ctAtomCode(ir[pc..$], -1);
                return ctSub(`
                    int test_$$()
                    {
                        $$ //$$
                    }
                    if (test_$$() >= 0)`, id, code.ptr ? code : "return 0;",
                        ir[pc].mnemonic, id);
            }
        }
        return "";
    }

    //process & generate source for simple bytecodes at front of ir using address addr
    CtState ctGenAtom(ref const(Bytecode)[] ir, int addr)
    {
        CtState result;
        result.code = ctAtomCode(ir, addr);
        ir.popFrontN(ir[0].code == IR.OrChar ? ir[0].sequence : ir[0].length);
        result.addr = addr + 1;
        return result;
    }

    //D code for atom at ir using address addr, addr < 0 means quickTest
    string ctAtomCode(const(Bytecode)[] ir, int addr)
    {
        string code;
        string bailOut, nextInstr;
        if (addr < 0)
        {
            bailOut = "return -1;";
            nextInstr = "return 0;";
        }
        else
        {
            bailOut = "goto L_backtrack;";
            nextInstr = ctSub("goto case $$;", addr+1);
            code ~=  ctSub( `
                 case $$: debug(std_regex_matcher) writeln("#$$");
                    `, addr, addr);
        }
        switch (ir[0].code)
        {
        case IR.OrChar://assumes IRL!(OrChar) == 1
            code ~=  ctSub(`
                    if (atEnd)
                        $$`, bailOut);
            immutable len = ir[0].sequence;
            for (uint i = 0; i < len; i++)
            {
                code ~= ctSub( `
                    if (front == $$)
                    {
                        $$
                        $$
                    }`,   ir[i].data, addr >= 0 ? "next();" :"", nextInstr);
            }
            code ~= ctSub( `
                $$`, bailOut);
            break;
        case IR.Char:
            code ~= ctSub( `
                    if (atEnd || front != $$)
                        $$
                    $$
                    $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
            break;
        case IR.Any:
            code ~= ctSub( `
                    if (atEnd || (!(re.flags & RegexOption.singleline)
                                && (front == '\r' || front == '\n')))
                        $$
                    $$
                    $$`, bailOut, addr >= 0 ? "next();" :"",nextInstr);
            break;
        case IR.CodepointSet:
            if (charsets.length)
            {
                string name = `func_`~to!string(addr+1);
                string funcCode = CodepointSet.toSourceCode(charsets[ir[0].data], name);
                code ~= ctSub( `
                    static $$
                    if (atEnd || !$$(front))
                        $$
                    $$
                $$`, funcCode, name, bailOut, addr >= 0 ? "next();" :"", nextInstr);
            }
            else
                code ~= ctSub( `
                    if (atEnd || !re.charsets[$$].scanFor(front))
                        $$
                    $$
                $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
            break;
        case IR.Trie:
            if (charsets.length && charsets[ir[0].data].length  <= 8)
                goto case IR.CodepointSet;
            code ~= ctSub( `
                    if (atEnd || !re.matchers[$$][front])
                        $$
                    $$
                $$`, ir[0].data, bailOut, addr >= 0 ? "next();" :"", nextInstr);
            break;
        case IR.Wordboundary:
            code ~= ctSub( `
                    dchar back;
                    DataIndex bi;
                    if (atStart && wordMatcher[front])
                    {
                        $$
                    }
                    else if (atEnd && s.loopBack(index).nextChar(back, bi)
                            && wordMatcher[back])
                    {
                        $$
                    }
                    else if (s.loopBack(index).nextChar(back, bi))
                    {
                        bool af = wordMatcher[front];
                        bool ab = wordMatcher[back];
                        if (af ^ ab)
                        {
                            $$
                        }
                    }
                    $$`, nextInstr, nextInstr, nextInstr, bailOut);
            break;
        case IR.Notwordboundary:
            code ~= ctSub( `
                    dchar back;
                    DataIndex bi;
                    //at start & end of input
                    if (atStart && wordMatcher[front])
                        $$
                    else if (atEnd && s.loopBack(index).nextChar(back, bi)
                            && wordMatcher[back])
                        $$
                    else if (s.loopBack(index).nextChar(back, bi))
                    {
                        bool af = wordMatcher[front];
                        bool ab = wordMatcher[back];
                        if (af ^ ab)
                            $$
                    }
                    $$`, bailOut, bailOut, bailOut, nextInstr);

            break;
        case IR.Bol:
            code ~= ctSub(`
                    dchar back;
                    DataIndex bi;
                    if (atStart || (s.loopBack(index).nextChar(back,bi)
                        && endOfLine(back, front == '\n')))
                    {
                        debug(std_regex_matcher) writeln("BOL matched");
                        $$
                    }
                    else
                        $$`, nextInstr, bailOut);

            break;
        case IR.Bof:
            code ~= ctSub(`
                    if (atStart)
                    {
                        debug(std_regex_matcher) writeln("BOF matched");
                        $$
                    }
                    else
                        $$`, nextInstr, bailOut);
            break;
        case IR.Eol:
            code ~= ctSub(`
                    dchar back;
                    DataIndex bi;
                    debug(std_regex_matcher) writefln("EOL (front 0x%x) %s", front, s[index .. s.lastIndex]);
                    //no matching inside \r\n
                    if (atEnd || (endOfLine(front, s.loopBack(index).nextChar(back,bi)
                             && back == '\r')))
                    {
                        debug(std_regex_matcher) writeln("EOL matched");
                        $$
                    }
                    else
                        $$`, nextInstr, bailOut);
            break;
        case IR.Eof:
            code ~= ctSub(`
                    if (atEnd)
                    {
                        debug(std_regex_matcher) writeln("BOF matched");
                        $$
                    }
                    else
                        $$`, nextInstr, bailOut);
            break;
        case IR.GroupStart:
            code ~= ctSub(`
                    matches[$$].begin = index;
                    $$`, ir[0].data, nextInstr);
            match = ir[0].data+1;
            break;
        case IR.GroupEnd:
            code ~= ctSub(`
                    matches[$$].end = index;
                    $$`, ir[0].data, nextInstr);
            break;
        case IR.Backref:
            string mStr = "auto referenced = ";
            mStr ~= ir[0].localRef
                ? ctSub("s[matches[$$].begin .. matches[$$].end];",
                    ir[0].data, ir[0].data)
                : ctSub("s[backrefed[$$].begin .. backrefed[$$].end];",
                    ir[0].data, ir[0].data);
            code ~= ctSub( `
                    $$
                    while (!atEnd && !referenced.empty && front == referenced.front)
                    {
                        next();
                        referenced.popFront();
                    }
                    if (referenced.empty)
                        $$
                    else
                        $$`, mStr, nextInstr, bailOut);
            break;
        case IR.Nop:
        case IR.End:
            break;
        default:
            assert(0, text(ir[0].mnemonic, " is not supported yet"));
        }
        return code;
    }

    //generate D code for the whole regex
    public string ctGenRegEx(const(Bytecode)[] ir)
    {
        auto bdy = ctGenBlock(ir, 0);
        auto r = `
            import core.memory : pureMalloc, pureFree;
            with(matcher)
            {
            pc = 0;
            counter = 0;
            lastState = 0;
            matches[] = Group!DataIndex.init;
            auto start = s._index;`;
        r ~= `
            goto StartLoop;
            debug(std_regex_matcher) writeln("Try CT matching  starting at ",s[index .. s.lastIndex]);
        L_backtrack:
            if (lastState || prevStack())
            {
                stackPop(pc);
                stackPop(index);
                s.reset(index);
                next();
            }
            else
            {
                s.reset(start);
                return false;
            }
        StartLoop:
            switch (pc)
            {
        `;
        r ~= bdy.code;
        r ~= ctSub(`
                case $$: break;`,bdy.addr);
        r ~= `
            default:
                assert(0);
            }
            // cleanup stale stack blocks
            while (prevStack()) {}
            return true;
            }
        `;
        return r;
    }

}

string ctGenRegExCode(Char)(const Regex!Char re)
{
    auto context = CtContext(re);
    return context.ctGenRegEx(re.ir);
}

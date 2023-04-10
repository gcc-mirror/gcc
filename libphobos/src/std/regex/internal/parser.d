//Written in the D programming language
/*
    Regular expression pattern parser.
*/
module std.regex.internal.parser;

import std.regex.internal.ir;
import std.range.primitives, std.uni, std.meta,
    std.traits, std.typecons, std.exception;
static import std.ascii;

// package relevant info from parser into a regex object
auto makeRegex(S, CG)(Parser!(S, CG) p)
{
    import std.regex.internal.backtracking : BacktrackingMatcher;
    import std.regex.internal.thompson : ThompsonMatcher;
    import std.algorithm.searching : canFind;
    alias Char = BasicElementOf!S;
    Regex!Char re;
    auto g = p.g;
    with(re)
    {
        ir = g.ir;
        dict = g.dict;
        ngroup = g.ngroup;
        maxCounterDepth = g.counterDepth;
        flags = p.re_flags;
        charsets = g.charsets;
        matchers = g.matchers;
        backrefed = g.backrefed;
        re.pattern = p.origin.idup;
        re.postprocess();
        // check if we have backreferences, if so - use backtracking
        if (__ctfe) factory = null; // allows us to use the awful enum re = regex(...);
        else if (re.backrefed.canFind!"a != 0")
            factory =  new RuntimeFactory!(BacktrackingMatcher, Char);
        else
            factory = new RuntimeFactory!(ThompsonMatcher, Char);
        debug(std_regex_parser)
        {
            __ctfe || print();
        }
        //@@@BUG@@@ (not reduced)
        //somehow just using validate _collides_ with std.utf.validate (!)
        version (assert) re.validateRe();
    }
    return re;
}

// helper for unittest
auto makeRegex(S)(S arg)
if (isSomeString!S)
{
    return makeRegex(Parser!(S, CodeGen)(arg, ""));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    auto re = makeRegex(`(?P<name>\w+) = (?P<var>\d+)`);
    auto nc = re.namedCaptures;
    static assert(isRandomAccessRange!(typeof(nc)));
    assert(!nc.empty);
    assert(nc.length == 2);
    assert(nc.equal(["name", "var"]));
    assert(nc[0] == "name");
    assert(nc[1..$].equal(["var"]));

    re = makeRegex(`(\w+) (?P<named>\w+) (\w+)`);
    nc = re.namedCaptures;
    assert(nc.length == 1);
    assert(nc[0] == "named");
    assert(nc.front == "named");
    assert(nc.back == "named");

    re = makeRegex(`(\w+) (\w+)`);
    nc = re.namedCaptures;
    assert(nc.empty);

    re = makeRegex(`(?P<year>\d{4})/(?P<month>\d{2})/(?P<day>\d{2})/`);
    nc = re.namedCaptures;
    auto cp = nc.save;
    assert(nc.equal(cp));
    nc.popFront();
    assert(nc.equal(cp[1..$]));
    nc.popBack();
    assert(nc.equal(cp[1 .. $ - 1]));
}


@trusted void reverseBytecode()(Bytecode[] code)
{
    Bytecode[] rev = new Bytecode[code.length];
    uint revPc = cast(uint) rev.length;
    Stack!(Tuple!(uint, uint, uint)) stack;
    uint start = 0;
    uint end = cast(uint) code.length;
    for (;;)
    {
        for (uint pc = start; pc < end; )
        {
            immutable len = code[pc].length;
            if (code[pc].code == IR.GotoEndOr)
                break; //pick next alternation branch
            if (code[pc].isAtom)
            {
                rev[revPc - len .. revPc] = code[pc .. pc + len];
                revPc -= len;
                pc += len;
            }
            else if (code[pc].isStart || code[pc].isEnd)
            {
                //skip over other embedded lookbehinds they are reversed
                if (code[pc].code == IR.LookbehindStart
                    || code[pc].code == IR.NeglookbehindStart)
                {
                    immutable blockLen = len + code[pc].data
                         + code[pc].pairedLength;
                    rev[revPc - blockLen .. revPc] = code[pc .. pc + blockLen];
                    pc += blockLen;
                    revPc -= blockLen;
                    continue;
                }
                immutable second = code[pc].indexOfPair(pc);
                immutable secLen = code[second].length;
                rev[revPc - secLen .. revPc] = code[second .. second + secLen];
                revPc -= secLen;
                if (code[pc].code == IR.OrStart)
                {
                    //we pass len bytes forward, but secLen in reverse
                    immutable revStart = revPc - (second + len - secLen - pc);
                    uint r = revStart;
                    uint i = pc + IRL!(IR.OrStart);
                    while (code[i].code == IR.Option)
                    {
                        if (code[i - 1].code != IR.OrStart)
                        {
                            assert(code[i - 1].code == IR.GotoEndOr);
                            rev[r - 1] = code[i - 1];
                        }
                        rev[r] = code[i];
                        auto newStart = i + IRL!(IR.Option);
                        auto newEnd = newStart + code[i].data;
                        auto newRpc = r + code[i].data + IRL!(IR.Option);
                        if (code[newEnd].code != IR.OrEnd)
                        {
                            newRpc--;
                        }
                        stack.push(tuple(newStart, newEnd, newRpc));
                        r += code[i].data + IRL!(IR.Option);
                        i += code[i].data + IRL!(IR.Option);
                    }
                    pc = i;
                    revPc = revStart;
                    assert(code[pc].code == IR.OrEnd);
                }
                else
                    pc += len;
            }
        }
        if (stack.empty)
            break;
        start = stack.top[0];
        end = stack.top[1];
        revPc = stack.top[2];
        stack.pop();
    }
    code[] = rev[];
}

struct CodeGen
{
    Bytecode[] ir;                 // resulting bytecode
    Stack!(uint) fixupStack;       // stack of opened start instructions
    NamedGroup[] dict;             // maps name -> user group number
    Stack!(uint) groupStack;       // stack of current number of group
    uint nesting = 0;              // group nesting level and repetitions step
    uint lookaroundNest = 0;       // nesting of lookaround
    uint counterDepth = 0;         // current depth of nested counted repetitions
    CodepointSet[] charsets;       // sets for char classes
    const(CharMatcher)[] matchers; // matchers for char classes
    uint[] backrefed;              // bitarray for groups refered by backref
    uint ngroup;                   // final number of groups (of all patterns)

    void start(uint length)
    {
        if (!__ctfe)
            ir.reserve((length*5+2)/4);
        fixupStack.push(0);
        groupStack.push(1);//0 - whole match
    }

    //mark referenced groups for latter processing
    void markBackref(uint n)
    {
        if (n/32 >= backrefed.length)
            backrefed.length = n/32 + 1;
        backrefed[n / 32] |= 1 << (n & 31);
    }

    bool isOpenGroup(uint n)
    {
        import std.algorithm.searching : canFind;
        // walk the fixup stack and see if there are groups labeled 'n'
        // fixup '0' is reserved for alternations
        return fixupStack.data[1..$].
            canFind!(fix => ir[fix].code == IR.GroupStart && ir[fix].data == n)();
    }

    void put(Bytecode code)
    {
        enforce(ir.length < maxCompiledLength,
            "maximum compiled pattern length is exceeded");
        ir ~= code;
    }

    void putRaw(uint number)
    {
        enforce(ir.length < maxCompiledLength,
            "maximum compiled pattern length is exceeded");
        ir ~= Bytecode.fromRaw(number);
    }

    //try to generate optimal IR code for this CodepointSet
    @trusted void charsetToIr(CodepointSet set)
    {//@@@BUG@@@ writeln is @system
        uint chars = cast(uint) set.length;
        if (chars < Bytecode.maxSequence)
        {
            switch (chars)
            {
                case 1:
                    put(Bytecode(IR.Char, set.byCodepoint.front));
                    break;
                case 0:
                    throw new RegexException("empty CodepointSet not allowed");
                default:
                    foreach (ch; set.byCodepoint)
                        put(Bytecode(IR.OrChar, ch, chars));
            }
        }
        else
        {
            import std.algorithm.searching : countUntil;
            const ivals = set.byInterval;
            immutable n = charsets.countUntil(set);
            if (n >= 0)
            {
                if (ivals.length*2 > maxCharsetUsed)
                    put(Bytecode(IR.Trie, cast(uint) n));
                else
                    put(Bytecode(IR.CodepointSet, cast(uint) n));
                return;
            }
            if (ivals.length*2 > maxCharsetUsed)
            {
                auto t  = getMatcher(set);
                put(Bytecode(IR.Trie, cast(uint) matchers.length));
                matchers ~= t;
                debug(std_regex_allocation) writeln("Trie generated");
            }
            else
            {
                put(Bytecode(IR.CodepointSet, cast(uint) charsets.length));
                matchers ~= CharMatcher.init;
            }
            charsets ~= set;
            assert(charsets.length == matchers.length);
        }
    }

    void genLogicGroup()
    {
        nesting++;
        pushFixup(length);
        put(Bytecode(IR.Nop, 0));
    }

    void genGroup()
    {
        nesting++;
        pushFixup(length);
        immutable nglob = groupStack.top++;
        enforce(groupStack.top <= maxGroupNumber, "limit on number of submatches is exceeded");
        put(Bytecode(IR.GroupStart, nglob));
    }

    void genNamedGroup(string name)
    {
        import std.array : insertInPlace;
        import std.range : assumeSorted;
        nesting++;
        pushFixup(length);
        immutable nglob = groupStack.top++;
        enforce(groupStack.top <= maxGroupNumber, "limit on submatches is exceeded");
        auto t = NamedGroup(name, nglob);
        auto d = assumeSorted!"a.name < b.name"(dict);
        immutable ind = d.lowerBound(t).length;
        insertInPlace(dict, ind, t);
        put(Bytecode(IR.GroupStart, nglob));
    }

        //generate code for start of lookaround: (?= (?! (?<= (?<!
    void genLookaround(IR opcode)
    {
        nesting++;
        pushFixup(length);
        put(Bytecode(opcode, 0));
        put(Bytecode.fromRaw(0));
        put(Bytecode.fromRaw(0));
        groupStack.push(0);
        lookaroundNest++;
        enforce(lookaroundNest <= maxLookaroundDepth,
            "maximum lookaround depth is exceeded");
    }

    void endPattern(uint num)
    {
        import std.algorithm.comparison : max;
        put(Bytecode(IR.End, num));
        ngroup = max(ngroup, groupStack.top);
        groupStack.top = 1; // reset group counter
    }

    //fixup lookaround with start at offset fix and append a proper *-End opcode
    void fixLookaround(uint fix)
    {
        lookaroundNest--;
        ir[fix] = Bytecode(ir[fix].code,
            cast(uint) ir.length - fix - IRL!(IR.LookaheadStart));
        auto g = groupStack.pop();
        assert(!groupStack.empty);
        ir[fix+1] = Bytecode.fromRaw(groupStack.top);
        //groups are cumulative across lookarounds
        ir[fix+2] = Bytecode.fromRaw(groupStack.top+g);
        groupStack.top += g;
        if (ir[fix].code == IR.LookbehindStart || ir[fix].code == IR.NeglookbehindStart)
        {
            reverseBytecode(ir[fix + IRL!(IR.LookbehindStart) .. $]);
        }
        put(ir[fix].paired);
    }

    // repetition of {1,1}
    void fixRepetition(uint offset)
    {
        import std.algorithm.mutation : copy;
        immutable replace = ir[offset].code == IR.Nop;
        if (replace)
        {
            copy(ir[offset + 1 .. $], ir[offset .. $ - 1]);
            ir.length -= 1;
        }
    }

    // repetition of {x,y}
    void fixRepetition(uint offset, uint min, uint max, bool greedy)
    {
        static import std.algorithm.comparison;
        import std.algorithm.mutation : copy;
        import std.array : insertInPlace;
        immutable replace = ir[offset].code == IR.Nop;
        immutable len = cast(uint) ir.length - offset - replace;
        if (max != infinite)
        {
            if (min != 1 || max != 1)
            {
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if (replace)
                    ir[offset] = op;
                else
                    insertInPlace(ir, offset, op);
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(max);
                counterDepth = std.algorithm.comparison.max(counterDepth, nesting+1);
            }
        }
        else if (min) //&& max is infinite
        {
            if (min != 1)
            {
                Bytecode op = Bytecode(greedy ? IR.RepeatStart : IR.RepeatQStart, len);
                if (replace)
                    ir[offset] = op;
                else
                    insertInPlace(ir, offset, op);
                offset += 1;//so it still points to the repeated block
                put(Bytecode(greedy ? IR.RepeatEnd : IR.RepeatQEnd, len));
                put(Bytecode.init); //hotspot
                putRaw(1);
                putRaw(min);
                putRaw(min);
                counterDepth = std.algorithm.comparison.max(counterDepth, nesting+1);
            }
            else if (replace)
            {
                copy(ir[offset+1 .. $], ir[offset .. $-1]);
                ir.length -= 1;
            }
            put(Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len));
            enforce(ir.length + len < maxCompiledLength,  "maximum compiled pattern length is exceeded");
            ir ~= ir[offset .. offset+len];
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
        else//vanila {0,inf}
        {
            Bytecode op = Bytecode(greedy ? IR.InfiniteStart : IR.InfiniteQStart, len);
            if (replace)
                ir[offset] = op;
            else
                insertInPlace(ir, offset, op);
            //IR.InfinteX is always a hotspot
            put(Bytecode(greedy ? IR.InfiniteEnd : IR.InfiniteQEnd, len));
            put(Bytecode.init); //merge index
        }
    }

    void fixAlternation()
    {
        import std.array : insertInPlace;
        uint fix = fixupStack.top;
        if (ir.length > fix && ir[fix].code == IR.Option)
        {
            ir[fix] = Bytecode(ir[fix].code, cast(uint) ir.length - fix);
            put(Bytecode(IR.GotoEndOr, 0));
            fixupStack.top = cast(uint) ir.length; //replace latest fixup for Option
            put(Bytecode(IR.Option, 0));
            return;
        }
        uint len, orStart;
        //start a new option
        if (fixupStack.length == 1)
        {//only root entry, effectively no fixup
            len = cast(uint) ir.length + IRL!(IR.GotoEndOr);
            orStart = 0;
        }
        else
        {//IR.lookahead, etc. fixups that have length > 1, thus check ir[x].length
            len = cast(uint) ir.length - fix - (ir[fix].length - 1);
            orStart = fix + ir[fix].length;
        }
        insertInPlace(ir, orStart, Bytecode(IR.OrStart, 0), Bytecode(IR.Option, len));
        assert(ir[orStart].code == IR.OrStart);
        put(Bytecode(IR.GotoEndOr, 0));
        fixupStack.push(orStart); //fixup for StartOR
        fixupStack.push(cast(uint) ir.length); //for second Option
        put(Bytecode(IR.Option, 0));
    }

    // finalizes IR.Option, fix points to the first option of sequence
    void finishAlternation(uint fix)
    {
        enforce(ir[fix].code == IR.Option, "no matching ')'");
        ir[fix] = Bytecode(ir[fix].code, cast(uint) ir.length - fix - IRL!(IR.OrStart));
        fix = fixupStack.pop();
        enforce(ir[fix].code == IR.OrStart, "no matching ')'");
        ir[fix] = Bytecode(IR.OrStart, cast(uint) ir.length - fix - IRL!(IR.OrStart));
        put(Bytecode(IR.OrEnd, cast(uint) ir.length - fix - IRL!(IR.OrStart)));
        uint pc = fix + IRL!(IR.OrStart);
        while (ir[pc].code == IR.Option)
        {
            pc = pc + ir[pc].data;
            if (ir[pc].code != IR.GotoEndOr)
                break;
            ir[pc] = Bytecode(IR.GotoEndOr, cast(uint)(ir.length - pc - IRL!(IR.OrEnd)));
            pc += IRL!(IR.GotoEndOr);
        }
        put(Bytecode.fromRaw(0));
    }

    // returns: (flag - repetition possible?, fixup of the start of this "group")
    Tuple!(bool, uint) onClose()
    {
        nesting--;
        uint fix = popFixup();
        switch (ir[fix].code)
        {
        case IR.GroupStart:
            put(Bytecode(IR.GroupEnd, ir[fix].data));
            return tuple(true, fix);
        case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
            assert(lookaroundNest);
            fixLookaround(fix);
            return tuple(false, 0u);
        case IR.Option: //| xxx )
            //two fixups: last option + full OR
            finishAlternation(fix);
            fix = topFixup;
            switch (ir[fix].code)
            {
            case IR.GroupStart:
                popFixup();
                put(Bytecode(IR.GroupEnd, ir[fix].data));
                return tuple(true, fix);
            case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                assert(lookaroundNest);
                fix = popFixup();
                fixLookaround(fix);
                return tuple(false, 0u);
            default://(?:xxx)
                popFixup();
                return tuple(true, fix);
            }
        default://(?:xxx)
            return tuple(true, fix);
        }
    }

    uint popFixup(){ return fixupStack.pop(); }

    void pushFixup(uint val){ return fixupStack.push(val); }

    @property uint topFixup(){ return fixupStack.top; }

    @property size_t fixupLength(){ return fixupStack.data.length; }

    @property uint length(){ return cast(uint) ir.length; }
}

// safety limits
enum maxGroupNumber = 2^^19;
enum maxLookaroundDepth = 16;
// *Bytecode.sizeof, i.e. 1Mb of bytecode alone
enum maxCompiledLength = 2^^18;
// amounts to up to 4 Mb of auxilary table for matching
enum maxCumulativeRepetitionLength = 2^^20;
// marker to indicate infinite repetition
enum infinite = ~0u;

struct Parser(R, Generator)
if (isForwardRange!R && is(ElementType!R : dchar))
{
    dchar front;
    bool empty;
    R pat, origin;       //keep full pattern for pretty printing error messages
    uint re_flags = 0;   //global flags e.g. multiline + internal ones
    Generator g;

    @trusted this(S)(R pattern, S flags)
        if (isSomeString!S)
    {
        pat = origin = pattern;
        //reserve slightly more then avg as sampled from unittests
        parseFlags(flags);
        front = ' ';//a safe default for freeform parsing
        popFront();
        g.start(cast(uint) pat.length);
        try
        {
            parseRegex();
        }
        catch (Exception e)
        {
            error(e.msg);//also adds pattern location
        }
        g.endPattern(1);
    }

    void _popFront()
    {
        if (pat.empty)
        {
            empty =  true;
        }
        else
        {
            front = pat.front;
            pat.popFront();
        }
    }

    void skipSpace()
    {
        while (!empty && isWhite(front)) _popFront();
    }

    void popFront()
    {
        _popFront();
        if (re_flags & RegexOption.freeform) skipSpace();
    }

    auto save(){ return this; }

    //parsing number with basic overflow check
    uint parseDecimal()
    {
        uint r = 0;
        while (std.ascii.isDigit(front))
        {
            if (r >= (uint.max/10))
                error("Overflow in decimal number");
            r = 10*r + cast(uint)(front-'0');
            popFront();
            if (empty) break;
        }
        return r;
    }

    //
    @trusted void parseFlags(S)(S flags)
    {//@@@BUG@@@ text is @system
        import std.conv : text;
        foreach (ch; flags)//flags are ASCII anyway
        {
        L_FlagSwitch:
            switch (ch)
            {

                foreach (i, op; __traits(allMembers, RegexOption))
                {
                    case RegexOptionNames[i]:
                            if (re_flags & mixin("RegexOption."~op))
                                throw new RegexException(text("redundant flag specified: ",ch));
                            re_flags |= mixin("RegexOption."~op);
                            break L_FlagSwitch;
                }
                default:
                    throw new RegexException(text("unknown regex flag '",ch,"'"));
            }
        }
    }

    //parse and store IR for regex pattern
    @trusted void parseRegex()
    {
        uint fix;//fixup pointer

        while (!empty)
        {
            debug(std_regex_parser)
                __ctfe || writeln("*LR*\nSource: ", pat, "\nStack: ",fixupStack.data);
            switch (front)
            {
            case '(':
                popFront();
                if (front == '?')
                {
                    popFront();
                    switch (front)
                    {
                    case '#':
                        for (;;)
                        {
                            popFront();
                            enforce(!empty, "Unexpected end of pattern");
                            if (front == ')')
                            {
                                popFront();
                                break;
                            }
                        }
                        break;
                    case ':':
                        g.genLogicGroup();
                        popFront();
                        break;
                    case '=':
                        g.genLookaround(IR.LookaheadStart);
                        popFront();
                        break;
                    case '!':
                        g.genLookaround(IR.NeglookaheadStart);
                        popFront();
                        break;
                    case 'P':
                        popFront();
                        enforce(front == '<', "Expected '<' in named group");
                        string name;
                        popFront();
                        if (empty || !(isAlpha(front) || front == '_'))
                            error("Expected alpha starting a named group");
                        name ~= front;
                        popFront();
                        while (!empty && (isAlpha(front) ||
                            front == '_' || std.ascii.isDigit(front)))
                        {
                            name ~= front;
                            popFront();
                        }
                        enforce(front == '>', "Expected '>' closing named group");
                        popFront();
                        g.genNamedGroup(name);
                        break;
                    case '<':
                        popFront();
                        if (front == '=')
                            g.genLookaround(IR.LookbehindStart);
                        else if (front == '!')
                            g.genLookaround(IR.NeglookbehindStart);
                        else
                            error("'!' or '=' expected after '<'");
                        popFront();
                        break;
                    default:
                        uint enableFlags, disableFlags;
                        bool enable = true;
                        do
                        {
                            switch (front)
                            {
                            case 's':
                                if (enable)
                                    enableFlags |= RegexOption.singleline;
                                else
                                    disableFlags |= RegexOption.singleline;
                                break;
                            case 'x':
                                if (enable)
                                    enableFlags |= RegexOption.freeform;
                                else
                                    disableFlags |= RegexOption.freeform;
                                break;
                            case 'i':
                                if (enable)
                                    enableFlags |= RegexOption.casefold;
                                else
                                    disableFlags |= RegexOption.casefold;
                                break;
                            case 'm':
                                if (enable)
                                    enableFlags |= RegexOption.multiline;
                                else
                                    disableFlags |= RegexOption.multiline;
                                break;
                            case '-':
                                if (!enable)
                                    error(" unexpected second '-' in flags");
                                enable = false;
                                break;
                            default:
                                error(" 's', 'x', 'i', 'm' or '-' expected after '(?' ");
                            }
                            popFront();
                        }while (front != ')');
                        popFront();
                        re_flags |= enableFlags;
                        re_flags &= ~disableFlags;
                    }
                }
                else
                {
                    g.genGroup();
                }
                break;
            case ')':
                enforce(g.nesting, "Unmatched ')'");
                popFront();
                auto pair = g.onClose();
                if (pair[0])
                    parseQuantifier(pair[1]);
                break;
            case '|':
                popFront();
                g.fixAlternation();
                break;
            default://no groups or whatever
                immutable start = g.length;
                parseAtom();
                parseQuantifier(start);
            }
        }

        if (g.fixupLength != 1)
        {
            fix = g.popFixup();
            g.finishAlternation(fix);
            enforce(g.fixupLength == 1, "no matching ')'");
        }
    }


    //parse and store IR for atom-quantifier pair
    @trusted void parseQuantifier(uint offset)
    {//copy is @system
        if (empty)
            return g.fixRepetition(offset);
        uint min, max;
        switch (front)
        {
        case '*':
            min = 0;
            max = infinite;
            break;
        case '?':
            min = 0;
            max = 1;
            break;
        case '+':
            min = 1;
            max = infinite;
            break;
        case '{':
            popFront();
            enforce(!empty, "Unexpected end of regex pattern");
            enforce(std.ascii.isDigit(front), "First number required in repetition");
            min = parseDecimal();
            if (front == '}')
                max = min;
            else if (front == ',')
            {
                popFront();
                if (std.ascii.isDigit(front))
                    max = parseDecimal();
                else if (front == '}')
                    max = infinite;
                else
                    error("Unexpected symbol in regex pattern");
                skipSpace();
                enforce(front == '}', "Unmatched '{' in regex pattern");
            }
            else
                error("Unexpected symbol in regex pattern");
            enforce(min <= max, "Illegal {n,m} quantifier");
            break;
        default:
            g.fixRepetition(offset);
            return;
        }
        bool greedy = true;
        //check only if we managed to get new symbol
        popFront();
        if (!empty && front == '?')
        {
            greedy = false;
            popFront();
        }
        g.fixRepetition(offset, min, max, greedy);
    }

    //parse and store IR for atom
    void parseAtom()
    {
        if (empty)
            return;
        switch (front)
        {
        case '*', '?', '+', '|', '{', '}':
            return error("'*', '+', '?', '{', '}' not allowed in atom");
        case '.':
            if (re_flags & RegexOption.singleline)
                g.put(Bytecode(IR.Any, 0));
            else
            {
                CodepointSet set;
                g.charsetToIr(set.add('\n','\n'+1).add('\r', '\r'+1).inverted);
            }
            popFront();
            break;
        case '[':
            parseCharset();
            break;
        case '\\':
            _popFront();
            enforce(!empty, "Unfinished escape sequence");
            parseEscape();
            break;
        case '^':
            if (re_flags & RegexOption.multiline)
                g.put(Bytecode(IR.Bol, 0));
            else
                g.put(Bytecode(IR.Bof, 0));
            popFront();
            break;
        case '$':
            if (re_flags & RegexOption.multiline)
                g.put(Bytecode(IR.Eol, 0));
            else
                g.put(Bytecode(IR.Eof, 0));
            popFront();
            break;
        default:
            if (re_flags & RegexOption.casefold)
            {
                auto range = simpleCaseFoldings(front);
                assert(range.length <= 5);
                if (range.length == 1)
                    g.put(Bytecode(IR.Char, range.front));
                else
                    foreach (v; range)
                        g.put(Bytecode(IR.OrChar, v, cast(uint) range.length));
            }
            else
                g.put(Bytecode(IR.Char, front));
            popFront();
        }
    }

    //parse and store IR for CodepointSet
    void parseCharset()
    {
        const save = re_flags;
        re_flags &= ~RegexOption.freeform; // stop ignoring whitespace if we did
        bool casefold = cast(bool)(re_flags & RegexOption.casefold);
        g.charsetToIr(unicode.parseSet(this, casefold));
        re_flags = save;
        // Last next() in parseCharset is executed w/o freeform flag
        if (re_flags & RegexOption.freeform) skipSpace();
    }

    //parse and generate IR for escape stand alone escape sequence
    @trusted void parseEscape()
    {//accesses array of appender
        import std.algorithm.iteration : sum;
        switch (front)
        {
        case 'f':   popFront(); g.put(Bytecode(IR.Char, '\f')); break;
        case 'n':   popFront(); g.put(Bytecode(IR.Char, '\n')); break;
        case 'r':   popFront(); g.put(Bytecode(IR.Char, '\r')); break;
        case 't':   popFront(); g.put(Bytecode(IR.Char, '\t')); break;
        case 'v':   popFront(); g.put(Bytecode(IR.Char, '\v')); break;

        case 'd':
            popFront();
            g.charsetToIr(unicode.Nd);
            break;
        case 'D':
            popFront();
            g.charsetToIr(unicode.Nd.inverted);
            break;
        case 'b':   popFront(); g.put(Bytecode(IR.Wordboundary, 0)); break;
        case 'B':   popFront(); g.put(Bytecode(IR.Notwordboundary, 0)); break;
        case 's':
            popFront();
            g.charsetToIr(unicode.White_Space);
            break;
        case 'S':
            popFront();
            g.charsetToIr(unicode.White_Space.inverted);
            break;
        case 'w':
            popFront();
            g.charsetToIr(wordCharacter);
            break;
        case 'W':
            popFront();
            g.charsetToIr(wordCharacter.inverted);
            break;
        case 'p': case 'P':
            bool casefold = cast(bool)(re_flags & RegexOption.casefold);
            auto set = unicode.parsePropertySpec(this, front == 'P', casefold);
            g.charsetToIr(set);
            break;
        case 'x':
            immutable code = parseUniHex(pat, 2);
            popFront();
            g.put(Bytecode(IR.Char,code));
            break;
        case 'u': case 'U':
            immutable code = parseUniHex(pat, front == 'u' ? 4 : 8);
            popFront();
            g.put(Bytecode(IR.Char, code));
            break;
        case 'c': //control codes
            Bytecode code = Bytecode(IR.Char, unicode.parseControlCode(this));
            popFront();
            g.put(code);
            break;
        case '0':
            popFront();
            g.put(Bytecode(IR.Char, 0));//NUL character
            break;
        case '1': .. case '9':
            uint nref = cast(uint) front - '0';
            immutable maxBackref = sum(g.groupStack.data);
            enforce(nref < maxBackref, "Backref to unseen group");
            //perl's disambiguation rule i.e.
            //get next digit only if there is such group number
            popFront();
            while (nref < maxBackref && !empty && std.ascii.isDigit(front))
            {
                nref = nref * 10 + front - '0';
                popFront();
            }
            if (nref >= maxBackref)
                nref /= 10;
            enforce(!g.isOpenGroup(nref), "Backref to open group");
            uint localLimit = maxBackref - g.groupStack.top;
            if (nref >= localLimit)
            {
                g.put(Bytecode(IR.Backref, nref-localLimit));
                g.ir[$-1].setLocalRef();
            }
            else
                g.put(Bytecode(IR.Backref, nref));
            g.markBackref(nref);
            break;
        default:
            if (front == '\\' && !pat.empty)
            {
                if (pat.front >= privateUseStart && pat.front <= privateUseEnd)
                    enforce(false, "invalid escape sequence");
            }
            if (front >= privateUseStart && front <= privateUseEnd)
            {
                g.endPattern(front - privateUseStart + 1);
                break;
            }
            auto op = Bytecode(IR.Char, front);
            popFront();
            g.put(op);
        }
    }

    //
    @trusted void error(string msg)
    {
        import std.conv : text;
        string app = msg;
        app ~= "\nPattern with error: `";
        app ~= origin[0..$-pat.length].text;
        app ~= "` <--HERE-- `";
        app ~= pat.text;
        app ~= "`";
        throw new RegexException(app);
    }

    alias Char = BasicElementOf!R;

    @property program()
    {
        return makeRegex(this);
    }
}

/+
    Postproces the IR, then optimize.
+/
@trusted void postprocess(Char)(ref Regex!Char zis)
{//@@@BUG@@@ write is @system
    with(zis)
    {
        struct FixedStack(T)
        {
            T[] arr;
            uint _top;
            //this(T[] storage){   arr = storage; _top = -1; }
            @property ref T top(){  assert(!empty); return arr[_top]; }
            void push(T x){  arr[++_top] = x; }
            T pop() { assert(!empty);   return arr[_top--]; }
            @property bool empty(){   return _top == -1; }
        }
        auto counterRange = FixedStack!uint(new uint[maxCounterDepth+1], -1);
        counterRange.push(1);
        ulong cumRange = 0;
        for (uint i = 0; i < ir.length; i += ir[i].length)
        {
            if (ir[i].hotspot)
            {
                assert(i + 1 < ir.length,
                    "unexpected end of IR while looking for hotspot");
                ir[i+1] = Bytecode.fromRaw(hotspotTableSize);
                hotspotTableSize += counterRange.top;
            }
            switch (ir[i].code)
            {
            case IR.RepeatStart, IR.RepeatQStart:
                uint repEnd = cast(uint)(i + ir[i].data + IRL!(IR.RepeatStart));
                assert(ir[repEnd].code == ir[i].paired.code);
                immutable max = ir[repEnd + 4].raw;
                ir[repEnd+2].raw = counterRange.top;
                ir[repEnd+3].raw *= counterRange.top;
                ir[repEnd+4].raw *= counterRange.top;
                ulong cntRange = cast(ulong)(max)*counterRange.top;
                cumRange += cntRange;
                enforce(cumRange < maxCumulativeRepetitionLength,
                    "repetition length limit is exceeded");
                counterRange.push(cast(uint) cntRange + counterRange.top);
                threadCount += counterRange.top;
                break;
            case IR.RepeatEnd, IR.RepeatQEnd:
                threadCount += counterRange.top;
                counterRange.pop();
                break;
            case IR.GroupStart:
                if (isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            case IR.GroupEnd:
                if (isBackref(ir[i].data))
                    ir[i].setBackrefence();
                threadCount += counterRange.top;
                break;
            default:
                threadCount += counterRange.top;
            }
        }
        checkIfOneShot();
        if (!(flags & RegexInfo.oneShot))
            kickstart = Kickstart!Char(zis, new uint[](256));
        debug(std_regex_allocation) writefln("IR processed, max threads: %d", threadCount);
        optimize(zis);
    }
}

void fixupBytecode()(Bytecode[] ir)
{
    Stack!uint fixups;

    with(IR) for (uint i=0; i<ir.length; i+= ir[i].length)
    {
        if (ir[i].isStart || ir[i].code == Option)
            fixups.push(i);
        else if (ir[i].code == OrEnd)
        {
            // Alternatives need more care
            auto j = fixups.pop(); // last Option
            ir[j].data = i -  j - ir[j].length;
            j = fixups.pop(); // OrStart
            ir[j].data = i - j - ir[j].length;
            ir[i].data = ir[j].data;

            // fixup all GotoEndOrs
            j = j + IRL!(OrStart);
            assert(ir[j].code == Option);
            for (;;)
            {
                auto next = j + ir[j].data + IRL!(Option);
                if (ir[next].code == IR.OrEnd)
                    break;
                ir[next - IRL!(GotoEndOr)].data = i - next;
                j = next;
            }
        }
        else if (ir[i].code == GotoEndOr)
        {
            auto j = fixups.pop(); // Option
            ir[j].data = i - j + IRL!(GotoEndOr)- IRL!(Option); // to the next option
        }
        else if (ir[i].isEnd)
        {
            auto j = fixups.pop();
            ir[i].data = i - j - ir[j].length;
            ir[j].data = ir[i].data;
        }
    }
    assert(fixups.empty);
}

void optimize(Char)(ref Regex!Char zis)
{
    import std.array : insertInPlace;
    CodepointSet nextSet(uint idx)
    {
        CodepointSet set;
        with(zis) with(IR)
    Outer:
        for (uint i = idx; i < ir.length; i += ir[i].length)
        {
            switch (ir[i].code)
            {
                case Char:
                    set.add(ir[i].data, ir[i].data+1);
                    goto default;
                //TODO: OrChar
                case Trie, CodepointSet:
                    set = zis.charsets[ir[i].data];
                    goto default;
                case GroupStart,GroupEnd:
                    break;
                default:
                    break Outer;
            }
        }
        return set;
    }

    with(zis) with(IR) for (uint i = 0; i < ir.length; i += ir[i].length)
    {
        if (ir[i].code == InfiniteEnd)
        {
            auto set = nextSet(i+IRL!(InfiniteEnd));
            if (!set.empty && set.length < 10_000)
            {
                ir[i] = Bytecode(InfiniteBloomEnd, ir[i].data);
                ir[i - ir[i].data - IRL!(InfiniteStart)] =
                    Bytecode(InfiniteBloomStart, ir[i].data);
                ir.insertInPlace(i+IRL!(InfiniteEnd),
                    Bytecode.fromRaw(cast(uint) zis.filters.length));
                zis.filters ~= BitTable(set);
                fixupBytecode(ir);
            }
        }
    }
}

//IR code validator - proper nesting, illegal instructions, etc.
@trusted void validateRe(Char)(ref Regex!Char zis)
{//@@@BUG@@@ text is @system
    import std.conv : text;
    with(zis)
    {
        for (uint pc = 0; pc < ir.length; pc += ir[pc].length)
        {
            if (ir[pc].isStart || ir[pc].isEnd)
            {
                immutable dest = ir[pc].indexOfPair(pc);
                assert(dest < ir.length, text("Wrong length in opcode at pc=",
                    pc, " ", dest, " vs ", ir.length));
                assert(ir[dest].paired ==  ir[pc],
                    text("Wrong pairing of opcodes at pc=", pc, "and pc=", dest));
            }
            else if (ir[pc].isAtom)
            {

            }
            else
               assert(0, text("Unknown type of instruction at pc=", pc));
        }
    }
}

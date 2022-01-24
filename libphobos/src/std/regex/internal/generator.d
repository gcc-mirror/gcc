/*
    Generators - components that generate strings for a given regex pattern.

    For the moment undocumented, and is subject to change.
*/
module std.regex.internal.generator;

/*
    Useful utility for self-testing, an infinite range of string samples
    that _have_ to match given compiled regex.
    Caveats: supports only a simple subset of bytecode.
*/
@trusted private struct SampleGenerator(Char)
{
    import std.array : appender, Appender;
    import std.format.write : formattedWrite;
    import std.random : Xorshift;
    import std.regex.internal.ir : Regex, IR, IRL;
    import std.utf : isValidDchar, byChar;
    Regex!Char re;
    Appender!(char[]) app;
    uint limit, seed;
    Xorshift gen;
    //generator for pattern r, with soft maximum of threshold elements
    //and a given random seed
    this(ref Regex!Char r, uint threshold, uint randomSeed)
    {
        re = r;
        limit = threshold;
        seed = randomSeed;
        app = appender!(Char[])();
        compose();
    }

    uint rand(uint x)
    {
        uint r = gen.front % x;
        gen.popFront();
        return r;
    }

    void compose()
    {
        uint pc = 0, counter = 0, dataLenOld = uint.max;
        for (;;)
        {
            switch (re.ir[pc].code)
            {
            case IR.Char:
                    formattedWrite(app,"%s", cast(dchar) re.ir[pc].data);
                    pc += IRL!(IR.Char);
                    break;
                case IR.OrChar:
                    uint len = re.ir[pc].sequence;
                    formattedWrite(app, "%s", cast(dchar) re.ir[pc + rand(len)].data);
                    pc += len;
                    break;
                case IR.CodepointSet:
                case IR.Trie:
                    auto set = re.charsets[re.ir[pc].data];
                    auto x = rand(cast(uint) set.byInterval.length);
                    auto y = rand(set.byInterval[x].b - set.byInterval[x].a);
                    formattedWrite(app, "%s", cast(dchar)(set.byInterval[x].a+y));
                    pc += IRL!(IR.CodepointSet);
                    break;
                case IR.Any:
                    uint x;
                    do
                    {
                        x = rand(0x11_000);
                    }while (x == '\r' || x == '\n' || !isValidDchar(x));
                    formattedWrite(app, "%s", cast(dchar) x);
                    pc += IRL!(IR.Any);
                    break;
                case IR.GotoEndOr:
                    pc += IRL!(IR.GotoEndOr)+re.ir[pc].data;
                    assert(re.ir[pc].code == IR.OrEnd);
                    goto case;
                case IR.OrEnd:
                    pc += IRL!(IR.OrEnd);
                    break;
                case IR.OrStart:
                    pc += IRL!(IR.OrStart);
                    goto case;
                case IR.Option:
                    uint next = pc + re.ir[pc].data + IRL!(IR.Option);
                    uint nOpt = 0;
                    //queue next Option
                    while (re.ir[next].code == IR.Option)
                    {
                        nOpt++;
                        next += re.ir[next].data + IRL!(IR.Option);
                    }
                    nOpt++;
                    nOpt = rand(nOpt);
                    for (;nOpt; nOpt--)
                    {
                        pc += re.ir[pc].data + IRL!(IR.Option);
                    }
                    assert(re.ir[pc].code == IR.Option);
                    pc += IRL!(IR.Option);
                    break;
                case IR.RepeatStart:case IR.RepeatQStart:
                    pc += IRL!(IR.RepeatStart)+re.ir[pc].data;
                    goto case IR.RepeatEnd;
                case IR.RepeatEnd:
                case IR.RepeatQEnd:
                    uint len = re.ir[pc].data;
                    uint step = re.ir[pc+2].raw;
                    uint min = re.ir[pc+3].raw;
                    if (counter < min)
                    {
                        counter += step;
                        pc -= len;
                        break;
                    }
                    uint max = re.ir[pc+4].raw;
                    if (counter < max)
                    {
                        if (app.data.length < limit && rand(3) > 0)
                        {
                            pc -= len;
                            counter += step;
                        }
                        else
                        {
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
                case IR.InfiniteStart, IR.InfiniteBloomStart, IR.InfiniteQStart:
                    pc += re.ir[pc].data + IRL!(IR.InfiniteStart);
                    goto case IR.InfiniteEnd; //both Q and non-Q
                case IR.InfiniteEnd, IR.InfiniteBloomEnd, IR.InfiniteQEnd:
                    uint len = re.ir[pc].data;
                    if (app.data.length == dataLenOld)
                    {
                        pc += IRL!(IR.InfiniteEnd);
                        break;
                    }
                    dataLenOld = cast(uint) app.data.length;
                    if (app.data.length < limit && rand(3) > 0)
                        pc = pc - len;
                    else
                        pc = pc + re.ir[pc].length;
                    break;
                case IR.GroupStart, IR.GroupEnd:
                    pc += IRL!(IR.GroupStart);
                    break;
                case IR.Bol, IR.Wordboundary, IR.Notwordboundary:
                case IR.LookaheadStart, IR.NeglookaheadStart, IR.LookbehindStart, IR.NeglookbehindStart:
                default:
                    return;
            }
        }
    }

    @property Char[] front()
    {
        return app.data;
    }

    enum empty = false;

    void popFront()
    {
        app.shrinkTo(0);
        compose();
    }
}

@system unittest
{
    import std.range, std.regex;
    auto re = regex(`P[a-z]{3,}q`);
    auto gen = SampleGenerator!char(re, 20, 3141592);
    static assert(isInputRange!(typeof(gen)));
    //@@@BUG@@@ somehow gen.take(1_000) doesn't work
    foreach (v; take(gen, 1_000))
        assert(v.match(re));
}

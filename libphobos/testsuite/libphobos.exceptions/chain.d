// Author: Ali Çehreli
// See https://forum.dlang.org/post/o2n7f8$2p1t$1@digitalmars.com

import core.stdc.stdio;

class TestException : Exception
{
    this(string msg)
    {
        super(typeof(this).stringof~": "~msg);
    }
}

class TestError : Error
{
    this(string msg)
    {
        super(typeof(this).stringof~": "~msg);
    }
}

// Causes an exception chain where the node at index errorIndex is an
// Error (others are all Exceptions).
void causeExceptionChain(size_t chainLength, size_t errorIndex)
{
    void throws(size_t n)
    {
        scope (exit)
        {
            string msg = [ cast(char)('a'+n) ].idup;
            if (n == errorIndex)
            {
                throw new TestError(msg);
            }
            else
            {
                throw new TestException(msg);
            }
        }

        if (n != 0)
        {
            // Redundant 'return' keyword due to
            // https://issues.dlang.org/show_bug.cgi?id=16960
            return throws(n - 1);
        }
    }

    throws(chainLength - 1);
}

void main()
{
    try
    {
        // -1 would mean "no Error in the chain". Change this to a
        // number between 0 and 4 (inclusive) then you will realize
        // that the Exception below will not be caught.
        size_t errorIndex = 3;
        causeExceptionChain(5, errorIndex);
    }
    catch (Error original)
    {
        printf("Caught\n");
        string prefix = "";
        for ({ size_t i; Throwable ex = original; } ex; ex = ex.next, ++i)
        {
            printf("%.*s%.*s\n", cast(int)prefix.length, prefix.ptr, cast(int)ex.msg.length, ex.msg.ptr);
            prefix = prefix~" ";
        }
        printf("Bypassed chain was:\n");
        prefix = "";
        for ({ size_t i; Throwable ex = original.bypassedException; } ex; ex = ex.next, ++i)
        {
            printf("%.*s%.*s\n", cast(int)prefix.length, prefix.ptr, cast(int)ex.msg.length, ex.msg.ptr);
            prefix = prefix~" ";
        }
    }
}

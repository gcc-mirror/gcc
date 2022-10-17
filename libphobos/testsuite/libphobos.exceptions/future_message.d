// { dg-options "-Wno-deprecated" }
import core.stdc.stdio;

// Make sure basic stuff works with future Throwable.message
class NoMessage : Throwable
{
    @nogc @safe pure nothrow this(string msg, Throwable next = null)
    {
        super(msg, next);
    }
}

class WithMessage : Throwable
{
    @nogc @safe pure nothrow this(string msg, Throwable next = null)
    {
        super(msg, next);
    }

    override const(char)[] message() const
    {
        return "I have a custom message.";
    }
}

class WithMessageNoOverride : Throwable
{
    @nogc @safe pure nothrow this(string msg, Throwable next = null)
    {
        super(msg, next);
    }

    const(char)[] message() const
    {
        return "I have a custom message and no override.";
    }
}

class WithMessageNoOverrideAndDifferentSignature : Throwable
{
    @nogc @safe pure nothrow this(string msg, Throwable next = null)
    {
        super(msg, next);
    }

    immutable(char)[] message()
    {
        return "I have a custom message and I'm nothing like Throwable.message.";
    }
}

void test(Throwable t)
{
    try
    {
        throw t;
    }
    catch (Throwable e)
    {
        fprintf(stderr, "%.*s ", cast(int)e.message.length, e.message.ptr);
    }
}

void main()
{
     test(new NoMessage("exception"));
     test(new WithMessage("exception"));
     test(new WithMessageNoOverride("exception"));
     test(new WithMessageNoOverrideAndDifferentSignature("exception"));
     fprintf(stderr, "\n");
}

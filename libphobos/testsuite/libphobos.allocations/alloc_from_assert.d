import core.exception;
import core.memory;

class FailFinalization
{
    int magic;

    ~this () @nogc nothrow
    {
        try
            assert(this.magic == 42);
        catch (AssertError) {}
    }
}

void foo ()
{
    auto dangling = new FailFinalization();
}

void main()
{
    foo();
    GC.collect();
}

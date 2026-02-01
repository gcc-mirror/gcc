import core.exception;
import core.thread;

__gshared bool caught;

void main()
{
    Thread t = new MyThread(&entry);
    t.start();
    t.join();

    assert(caught);
}

class MyThread : Thread
{
    this( void function() fn, size_t sz = 0 ) @safe pure nothrow @nogc
    {
        super(fn, sz);
    }

    override void filterCaughtThrowable(ref Throwable t) @system nothrow
    {
        if (auto t2 = cast(AssertError) t)
        {
            if (t2.message == "Hey!")
            {
                caught = true;
                t = null;
                return;
            }
        }

        super.filterCaughtThrowable(t);
    }
}

void entry()
{
    throw new AssertError("Hey!");
}

import core.exception;
import core.thread;

__gshared bool caught;

void main()
{
    filterThreadThrowableHandler = (ref Throwable t) {
        if (auto t2 = cast(AssertError) t)
        {
            if (t2.message != "Hey!")
                return;
        }
        else
            return;

        caught = true;
        t = null;
    };

    Thread t = new Thread(&entry);
    t.start();
    t.join();

    assert(caught);
}

void entry()
{
    throw new AssertError("Hey!");
}

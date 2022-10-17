import core.memory;
import core.sync.condition;
import core.sync.mutex;
import core.thread;

__gshared Condition g_cond;
__gshared Mutex g_mutex;
__gshared int g_step = 0;

class C
{
    ~this()
    {
        import core.stdc.stdlib;
        abort();    // this gets triggered although the instance always stays referenced
    }
}

C c;

static this()
{
    c = new C;
}

static ~this()
{
    import core.memory;
    GC.free(cast(void*)c); // free without destruction to avoid triggering abort()
}

void test()
{
    assert(c !is null);

    // notify the main thread of the finished initialization
    synchronized (g_mutex) g_step = 1;
    g_cond.notifyAll();

    // wait until the GC collection is done
    synchronized (g_mutex) {
        while (g_step != 2)
            g_cond.wait();
    }
}


void main()
{
    g_mutex = new Mutex;
    g_cond = new Condition(g_mutex);

    auto th = new Thread(&test);
    th.start();

    // wait for thread to be fully initialized
    synchronized (g_mutex) {
        while (g_step != 1)
            g_cond.wait();
    }

    // this causes the other thread's C instance to be reaped with the bug present
    GC.collect();

    // allow the thread to shut down
    synchronized (g_mutex) g_step = 2;
    g_cond.notifyAll();

    th.join();
}

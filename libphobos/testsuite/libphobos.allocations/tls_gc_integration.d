import core.memory, core.thread, core.volatile;

/*
 * This test repeatedly performs operations on GC-allocated objects which
 * are only reachable from TLS storage. Tests are performed in multiple threads
 * and GC collections are triggered repeatedly, so if the GC does not properly
 * scan TLS memory, this provokes a crash.
 */
class TestTLS
{
    uint a;
    void addNumber()
    {
        auto val = volatileLoad(&a);
        val++;
        volatileStore(&a, val);
    }
}

TestTLS tlsPtr;

static this()
{
    tlsPtr = new TestTLS();
}

void main()
{
    void runThread()
    {
        for (size_t i = 0; i < 100; i++)
        {
            Thread.sleep(10.msecs);
            tlsPtr.addNumber();
            GC.collect();
        }
    }

    Thread[] threads;
    for (size_t i = 0; i < 20; i++)
    {
        auto t = new Thread(&runThread);
        threads ~= t;
        t.start();
    }
    runThread();

    foreach (thread; threads)
        thread.join();
}

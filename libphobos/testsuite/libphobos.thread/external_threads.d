import core.sys.posix.pthread;
import core.memory;
import core.thread;

extern (C) void  rt_moduleTlsCtor();
extern (C) void  rt_moduleTlsDtor();

extern(C)
void* entry_point1(void*)
{
    // try collecting - GC must ignore this call because this thread
    // is not registered in runtime
    GC.collect();
    return null;
}

extern(C)
void* entry_point2(void*)
{
    // This thread gets registered in druntime, does some work and gets
    // unregistered to be cleaned up manually
    thread_attachThis();
    rt_moduleTlsCtor();

    auto x = new int[10];

    rt_moduleTlsDtor();
    thread_detachThis();
    return null;
}

void main()
{
    // allocate some garbage
    auto x = new int[1000];

    {
        pthread_t thread;
        auto status = pthread_create(&thread, null, &entry_point1, null);
        assert(status == 0);
        pthread_join(thread, null);
    }

    {
        pthread_t thread;
        auto status = pthread_create(&thread, null, &entry_point2, null);
        assert(status == 0);
        pthread_join(thread, null);
    }
}

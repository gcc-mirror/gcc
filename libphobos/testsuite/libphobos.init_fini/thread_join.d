// Bugzilla 11309 - std.concurrency: OwnerTerminated message doesn't work
// We need to assure that the thread dtors of parent threads run before the thread dtors of the child threads.
import core.thread, core.sync.semaphore;

__gshared Semaphore sem;

static ~this()
{
    if (sem !is null) sem.notify();
}

void main()
{
    sem = new Semaphore;
    auto thr = new Thread({assert(sem.wait(1.seconds));});
    thr.start();
}

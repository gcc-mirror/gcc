import core.thread;
import core.sync.semaphore;

__gshared Semaphore sem;

void thread_main ()
{
    sem.notify();
}

void main()
{
    auto th = new Thread(&thread_main);
    sem = new Semaphore();
    th.start();
    sem.wait();
    while (th.isRunning()) {}
    destroy(th); // force detach
    th.join();
}

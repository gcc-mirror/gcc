// { dg-shouldfail "segv or bus error" }
import core.thread;
import core.sys.posix.sys.mman;

// this should be true for most architectures
// (taken from core.thread)
version = StackGrowsDown;

enum stackSize = 4096;

// Simple method that causes a stack overflow
void stackMethod()
{
    // Over the stack size, so it overflows the stack
    int[stackSize/int.sizeof+100] x;
}

void main()
{
    auto test_fiber = new Fiber(&stackMethod, stackSize);

    // allocate a page below (above) the fiber's stack to make stack overflows possible (w/o segfaulting)
    version (StackGrowsDown)
    {
        static assert(__traits(identifier, test_fiber.tupleof[8]) == "m_pmem");
        auto stackBottom = test_fiber.tupleof[8];
        auto p = mmap(stackBottom - 8 * stackSize, 8 * stackSize,
                      PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        assert(p !is null, "failed to allocate page");
    }
    else
    {
        auto m_sz = test_fiber.tupleof[7];
        auto m_pmem = test_fiber.tupleof[8];
        static assert(__traits(identifier, test_fiber.tupleof[7]) == "m_size");
        static assert(__traits(identifier, test_fiber.tupleof[8]) == "m_pmem");

        auto stackTop = m_pmem + m_sz;
        auto p = mmap(stackTop, 8 * stackSize,
                      PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        assert(p !is null, "failed to allocate page");
    }

    // the guard page should prevent a mem corruption by stack
    // overflow and cause a segfault instead (or generate SIGBUS on *BSD flavors)
    test_fiber.call();
}

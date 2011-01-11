/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/licenses/publicdomain
 */

package java.util.concurrent;
import java.util.concurrent.locks.*;
import java.util.*;

/**
 * An {@link ExecutorService} that executes each submitted task using
 * one of possibly several pooled threads, normally configured
 * using {@link Executors} factory methods.
 *
 * <p>Thread pools address two different problems: they usually
 * provide improved performance when executing large numbers of
 * asynchronous tasks, due to reduced per-task invocation overhead,
 * and they provide a means of bounding and managing the resources,
 * including threads, consumed when executing a collection of tasks.
 * Each <tt>ThreadPoolExecutor</tt> also maintains some basic
 * statistics, such as the number of completed tasks.
 *
 * <p>To be useful across a wide range of contexts, this class
 * provides many adjustable parameters and extensibility
 * hooks. However, programmers are urged to use the more convenient
 * {@link Executors} factory methods {@link
 * Executors#newCachedThreadPool} (unbounded thread pool, with
 * automatic thread reclamation), {@link Executors#newFixedThreadPool}
 * (fixed size thread pool) and {@link
 * Executors#newSingleThreadExecutor} (single background thread), that
 * preconfigure settings for the most common usage
 * scenarios. Otherwise, use the following guide when manually
 * configuring and tuning this class:
 *
 * <dl>
 *
 * <dt>Core and maximum pool sizes</dt>
 *
 * <dd>A <tt>ThreadPoolExecutor</tt> will automatically adjust the
 * pool size
 * (see {@link ThreadPoolExecutor#getPoolSize})
 * according to the bounds set by corePoolSize
 * (see {@link ThreadPoolExecutor#getCorePoolSize})
 * and
 * maximumPoolSize
 * (see {@link ThreadPoolExecutor#getMaximumPoolSize}).
 * When a new task is submitted in method {@link
 * ThreadPoolExecutor#execute}, and fewer than corePoolSize threads
 * are running, a new thread is created to handle the request, even if
 * other worker threads are idle.  If there are more than
 * corePoolSize but less than maximumPoolSize threads running, a new
 * thread will be created only if the queue is full.  By setting
 * corePoolSize and maximumPoolSize the same, you create a fixed-size
 * thread pool. By setting maximumPoolSize to an essentially unbounded
 * value such as <tt>Integer.MAX_VALUE</tt>, you allow the pool to
 * accommodate an arbitrary number of concurrent tasks. Most typically,
 * core and maximum pool sizes are set only upon construction, but they
 * may also be changed dynamically using {@link
 * ThreadPoolExecutor#setCorePoolSize} and {@link
 * ThreadPoolExecutor#setMaximumPoolSize}. <dd>
 *
 * <dt> On-demand construction
 *
 * <dd> By default, even core threads are initially created and
 * started only when new tasks arrive, but this can be overridden
 * dynamically using method {@link
 * ThreadPoolExecutor#prestartCoreThread} or
 * {@link ThreadPoolExecutor#prestartAllCoreThreads}.
 * You probably want to prestart threads if you construct the
 * pool with a non-empty queue. </dd>
 *
 * <dt>Creating new threads</dt>
 *
 * <dd>New threads are created using a {@link
 * java.util.concurrent.ThreadFactory}.  If not otherwise specified, a
 * {@link Executors#defaultThreadFactory} is used, that creates threads to all
 * be in the same {@link ThreadGroup} and with the same
 * <tt>NORM_PRIORITY</tt> priority and non-daemon status. By supplying
 * a different ThreadFactory, you can alter the thread's name, thread
 * group, priority, daemon status, etc. If a <tt>ThreadFactory</tt> fails to create
 * a thread when asked by returning null from <tt>newThread</tt>,
 * the executor will continue, but might
 * not be able to execute any tasks. </dd>
 *
 * <dt>Keep-alive times</dt>
 *
 * <dd>If the pool currently has more than corePoolSize threads,
 * excess threads will be terminated if they have been idle for more
 * than the keepAliveTime (see {@link
 * ThreadPoolExecutor#getKeepAliveTime}). This provides a means of
 * reducing resource consumption when the pool is not being actively
 * used. If the pool becomes more active later, new threads will be
 * constructed. This parameter can also be changed dynamically using
 * method {@link ThreadPoolExecutor#setKeepAliveTime}. Using a value
 * of <tt>Long.MAX_VALUE</tt> {@link TimeUnit#NANOSECONDS} effectively
 * disables idle threads from ever terminating prior to shut down. By
 * default, the keep-alive policy applies only when there are more
 * than corePoolSizeThreads. But method {@link
 * ThreadPoolExecutor#allowCoreThreadTimeOut} can be used to apply
 * this time-out policy to core threads as well, so long as
 * the keepAliveTime value is non-zero. </dd>
 *
 * <dt>Queuing</dt>
 *
 * <dd>Any {@link BlockingQueue} may be used to transfer and hold
 * submitted tasks.  The use of this queue interacts with pool sizing:
 *
 * <ul>
 *
 * <li> If fewer than corePoolSize threads are running, the Executor
 * always prefers adding a new thread
 * rather than queuing.</li>
 *
 * <li> If corePoolSize or more threads are running, the Executor
 * always prefers queuing a request rather than adding a new
 * thread.</li>
 *
 * <li> If a request cannot be queued, a new thread is created unless
 * this would exceed maximumPoolSize, in which case, the task will be
 * rejected.</li>
 *
 * </ul>
 *
 * There are three general strategies for queuing:
 * <ol>
 *
 * <li> <em> Direct handoffs.</em> A good default choice for a work
 * queue is a {@link SynchronousQueue} that hands off tasks to threads
 * without otherwise holding them. Here, an attempt to queue a task
 * will fail if no threads are immediately available to run it, so a
 * new thread will be constructed. This policy avoids lockups when
 * handling sets of requests that might have internal dependencies.
 * Direct handoffs generally require unbounded maximumPoolSizes to
 * avoid rejection of new submitted tasks. This in turn admits the
 * possibility of unbounded thread growth when commands continue to
 * arrive on average faster than they can be processed.  </li>
 *
 * <li><em> Unbounded queues.</em> Using an unbounded queue (for
 * example a {@link LinkedBlockingQueue} without a predefined
 * capacity) will cause new tasks to wait in the queue when all
 * corePoolSize threads are busy. Thus, no more than corePoolSize
 * threads will ever be created. (And the value of the maximumPoolSize
 * therefore doesn't have any effect.)  This may be appropriate when
 * each task is completely independent of others, so tasks cannot
 * affect each others execution; for example, in a web page server.
 * While this style of queuing can be useful in smoothing out
 * transient bursts of requests, it admits the possibility of
 * unbounded work queue growth when commands continue to arrive on
 * average faster than they can be processed.  </li>
 *
 * <li><em>Bounded queues.</em> A bounded queue (for example, an
 * {@link ArrayBlockingQueue}) helps prevent resource exhaustion when
 * used with finite maximumPoolSizes, but can be more difficult to
 * tune and control.  Queue sizes and maximum pool sizes may be traded
 * off for each other: Using large queues and small pools minimizes
 * CPU usage, OS resources, and context-switching overhead, but can
 * lead to artificially low throughput.  If tasks frequently block (for
 * example if they are I/O bound), a system may be able to schedule
 * time for more threads than you otherwise allow. Use of small queues
 * generally requires larger pool sizes, which keeps CPUs busier but
 * may encounter unacceptable scheduling overhead, which also
 * decreases throughput.  </li>
 *
 * </ol>
 *
 * </dd>
 *
 * <dt>Rejected tasks</dt>
 *
 * <dd> New tasks submitted in method {@link
 * ThreadPoolExecutor#execute} will be <em>rejected</em> when the
 * Executor has been shut down, and also when the Executor uses finite
 * bounds for both maximum threads and work queue capacity, and is
 * saturated.  In either case, the <tt>execute</tt> method invokes the
 * {@link RejectedExecutionHandler#rejectedExecution} method of its
 * {@link RejectedExecutionHandler}.  Four predefined handler policies
 * are provided:
 *
 * <ol>
 *
 * <li> In the
 * default {@link ThreadPoolExecutor.AbortPolicy}, the handler throws a
 * runtime {@link RejectedExecutionException} upon rejection. </li>
 *
 * <li> In {@link
 * ThreadPoolExecutor.CallerRunsPolicy}, the thread that invokes
 * <tt>execute</tt> itself runs the task. This provides a simple
 * feedback control mechanism that will slow down the rate that new
 * tasks are submitted. </li>
 *
 * <li> In {@link ThreadPoolExecutor.DiscardPolicy},
 * a task that cannot be executed is simply dropped.  </li>
 *
 * <li>In {@link
 * ThreadPoolExecutor.DiscardOldestPolicy}, if the executor is not
 * shut down, the task at the head of the work queue is dropped, and
 * then execution is retried (which can fail again, causing this to be
 * repeated.) </li>
 *
 * </ol>
 *
 * It is possible to define and use other kinds of {@link
 * RejectedExecutionHandler} classes. Doing so requires some care
 * especially when policies are designed to work only under particular
 * capacity or queuing policies. </dd>
 *
 * <dt>Hook methods</dt>
 *
 * <dd>This class provides <tt>protected</tt> overridable {@link
 * ThreadPoolExecutor#beforeExecute} and {@link
 * ThreadPoolExecutor#afterExecute} methods that are called before and
 * after execution of each task.  These can be used to manipulate the
 * execution environment; for example, reinitializing ThreadLocals,
 * gathering statistics, or adding log entries. Additionally, method
 * {@link ThreadPoolExecutor#terminated} can be overridden to perform
 * any special processing that needs to be done once the Executor has
 * fully terminated.
 *
 * <p>If hook or callback methods throw
 * exceptions, internal worker threads may in turn fail and
 * abruptly terminate.</dd>
 *
 * <dt>Queue maintenance</dt>
 *
 * <dd> Method {@link ThreadPoolExecutor#getQueue} allows access to
 * the work queue for purposes of monitoring and debugging.  Use of
 * this method for any other purpose is strongly discouraged.  Two
 * supplied methods, {@link ThreadPoolExecutor#remove} and {@link
 * ThreadPoolExecutor#purge} are available to assist in storage
 * reclamation when large numbers of queued tasks become
 * cancelled.</dd>
 *
 * <dt>Finalization</dt>
 *
 * <dd> A pool that is no longer referenced in a program <em>AND</em>
 * has no remaining threads will be <tt>shutdown</tt>
 * automatically. If you would like to ensure that unreferenced pools
 * are reclaimed even if users forget to call {@link
 * ThreadPoolExecutor#shutdown}, then you must arrange that unused
 * threads eventually die, by setting appropriate keep-alive times,
 * using a lower bound of zero core threads and/or setting {@link
 * ThreadPoolExecutor#allowCoreThreadTimeOut}.  </dd> </dl>
 *
 * <p> <b>Extension example</b>. Most extensions of this class
 * override one or more of the protected hook methods. For example,
 * here is a subclass that adds a simple pause/resume feature:
 *
 * <pre>
 * class PausableThreadPoolExecutor extends ThreadPoolExecutor {
 *   private boolean isPaused;
 *   private ReentrantLock pauseLock = new ReentrantLock();
 *   private Condition unpaused = pauseLock.newCondition();
 *
 *   public PausableThreadPoolExecutor(...) { super(...); }
 *
 *   protected void beforeExecute(Thread t, Runnable r) {
 *     super.beforeExecute(t, r);
 *     pauseLock.lock();
 *     try {
 *       while (isPaused) unpaused.await();
 *     } catch (InterruptedException ie) {
 *       t.interrupt();
 *     } finally {
 *       pauseLock.unlock();
 *     }
 *   }
 *
 *   public void pause() {
 *     pauseLock.lock();
 *     try {
 *       isPaused = true;
 *     } finally {
 *       pauseLock.unlock();
 *     }
 *   }
 *
 *   public void resume() {
 *     pauseLock.lock();
 *     try {
 *       isPaused = false;
 *       unpaused.signalAll();
 *     } finally {
 *       pauseLock.unlock();
 *     }
 *   }
 * }
 * </pre>
 * @since 1.5
 * @author Doug Lea
 */
public class ThreadPoolExecutor extends AbstractExecutorService {
    /**
     * Only used to force toArray() to produce a Runnable[].
     */
    private static final Runnable[] EMPTY_RUNNABLE_ARRAY = new Runnable[0];

    /**
     * Permission for checking shutdown
     */
    private static final RuntimePermission shutdownPerm =
        new RuntimePermission("modifyThread");

    /**
     * Queue used for holding tasks and handing off to worker threads.
     */
    private final BlockingQueue<Runnable> workQueue;

    /**
     * Lock held on updates to poolSize, corePoolSize, maximumPoolSize, and
     * workers set.
     */
    private final ReentrantLock mainLock = new ReentrantLock();

    /**
     * Wait condition to support awaitTermination
     */
    private final Condition termination = mainLock.newCondition();

    /**
     * Set containing all worker threads in pool.
     */
    private final HashSet<Worker> workers = new HashSet<Worker>();

    /**
     * Timeout in nanoseconds for idle threads waiting for work.
     * Threads use this timeout only when there are more than
     * corePoolSize present. Otherwise they wait forever for new work.
     */
    private volatile long  keepAliveTime;

    /**
     * If false (default) core threads stay alive even when idle.
     * If true, core threads use keepAliveTime to time out waiting for work.
     */
    private volatile boolean allowCoreThreadTimeOut;

    /**
     * Core pool size, updated only while holding mainLock,
     * but volatile to allow concurrent readability even
     * during updates.
     */
    private volatile int   corePoolSize;

    /**
     * Maximum pool size, updated only while holding mainLock
     * but volatile to allow concurrent readability even
     * during updates.
     */
    private volatile int   maximumPoolSize;

    /**
     * Current pool size, updated only while holding mainLock
     * but volatile to allow concurrent readability even
     * during updates.
     */
    private volatile int   poolSize;

    /**
     * Lifecycle state
     */
    volatile int runState;

    // Special values for runState
    /** Normal, not-shutdown mode */
    static final int RUNNING    = 0;
    /** Controlled shutdown mode */
    static final int SHUTDOWN   = 1;
    /** Immediate shutdown mode */
    static final int STOP       = 2;
    /** Final state */
    static final int TERMINATED = 3;

    /**
     * Handler called when saturated or shutdown in execute.
     */
    private volatile RejectedExecutionHandler handler;

    /**
     * Factory for new threads.
     */
    private volatile ThreadFactory threadFactory;

    /**
     * Tracks largest attained pool size.
     */
    private int largestPoolSize;

    /**
     * Counter for completed tasks. Updated only on termination of
     * worker threads.
     */
    private long completedTaskCount;

    /**
     * The default rejected execution handler
     */
    private static final RejectedExecutionHandler defaultHandler =
        new AbortPolicy();

    /**
     * Invokes the rejected execution handler for the given command.
     */
    void reject(Runnable command) {
        handler.rejectedExecution(command, this);
    }

    /**
     * Creates and returns a new thread running firstTask as its first
     * task. Call only while holding mainLock.
     * @param firstTask the task the new thread should run first (or
     * null if none)
     * @return the new thread, or null if threadFactory fails to create thread
     */
    private Thread addThread(Runnable firstTask) {
        if (runState == TERMINATED) // Don't create thread if terminated
            return null;
        Worker w = new Worker(firstTask);
        Thread t = threadFactory.newThread(w);
        if (t != null) {
            w.thread = t;
            workers.add(w);
            int nt = ++poolSize;
            if (nt > largestPoolSize)
                largestPoolSize = nt;
        }
        return t;
    }

    /**
     * Creates and starts a new thread running firstTask as its first
     * task, only if fewer than corePoolSize threads are running.
     * @param firstTask the task the new thread should run first (or
     * null if none)
     * @return true if successful.
     */
    private boolean addIfUnderCorePoolSize(Runnable firstTask) {
        Thread t = null;
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            if (poolSize < corePoolSize)
                t = addThread(firstTask);
        } finally {
            mainLock.unlock();
        }
        if (t == null)
            return false;
        t.start();
        return true;
    }

    /**
     * Creates and starts a new thread only if fewer than maximumPoolSize
     * threads are running.  The new thread runs as its first task the
     * next task in queue, or if there is none, the given task.
     * @param firstTask the task the new thread should run first (or
     * null if none)
     * @return 0 if a new thread cannot be created, a positive number
     * if firstTask will be run in a new thread, or a negative number
     * if a new thread was created but is running some other task, in
     * which case the caller must try some other way to run firstTask
     * (perhaps by calling this method again).
     */
    private int addIfUnderMaximumPoolSize(Runnable firstTask) {
        Thread t = null;
        int status = 0;
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            if (poolSize < maximumPoolSize) {
                Runnable next = workQueue.poll();
                if (next == null) {
                    next = firstTask;
                    status = 1;
                } else
                    status = -1;
                t = addThread(next);
            }
        } finally {
            mainLock.unlock();
        }
        if (t == null)
            return 0;
        t.start();
        return status;
    }


    /**
     * Gets the next task for a worker thread to run.
     * @return the task
     */
    Runnable getTask() {
        for (;;) {
            try {
                switch (runState) {
                case RUNNING: {
                    // untimed wait if core and not allowing core timeout
                    if (poolSize <= corePoolSize && !allowCoreThreadTimeOut)
                        return workQueue.take();

                    long timeout = keepAliveTime;
                    if (timeout <= 0) // die immediately for 0 timeout
                        return null;
                    Runnable r = workQueue.poll(timeout, TimeUnit.NANOSECONDS);
                    if (r != null)
                        return r;
                    if (poolSize > corePoolSize || allowCoreThreadTimeOut)
                        return null; // timed out
                    // Else, after timeout, the pool shrank. Retry
                    break;
                }

                case SHUTDOWN: {
                    // Help drain queue
                    Runnable r = workQueue.poll();
                    if (r != null)
                        return r;

                    // Check if can terminate
                    if (workQueue.isEmpty()) {
                        interruptIdleWorkers();
                        return null;
                    }

                    // Else there could still be delayed tasks in queue.
                    return workQueue.take();
                }

                case STOP:
                    return null;
                default:
                    assert false;
                }
            } catch (InterruptedException ie) {
                // On interruption, re-check runstate
            }
        }
    }

    /**
     * Wakes up all threads that might be waiting for tasks.
     */
    void interruptIdleWorkers() {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            for (Worker w : workers)
                w.interruptIfIdle();
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Performs bookkeeping for a terminated worker thread.
     * @param w the worker
     */
    void workerDone(Worker w) {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            completedTaskCount += w.completedTasks;
            workers.remove(w);
            if (--poolSize > 0)
                return;

            // Else, this is the last thread. Deal with potential shutdown.

            int state = runState;
            assert state != TERMINATED;

            if (state != STOP) {
                // If there are queued tasks but no threads, create
                // replacement thread. We must create it initially
                // idle to avoid orphaned tasks in case addThread
                // fails.  This also handles case of delayed tasks
                // that will sometime later become runnable.
                if (!workQueue.isEmpty()) {
                    Thread t = addThread(null);
                    if (t != null)
                        t.start();
                    return;
                }

                // Otherwise, we can exit without replacement
                if (state == RUNNING)
                    return;
            }

            // Either state is STOP, or state is SHUTDOWN and there is
            // no work to do. So we can terminate.
            termination.signalAll();
            runState = TERMINATED;
            // fall through to call terminate() outside of lock.
        } finally {
            mainLock.unlock();
        }

        assert runState == TERMINATED;
        terminated();
    }

    /**
     *  Worker threads
     */
    private class Worker implements Runnable {

        /**
         * The runLock is acquired and released surrounding each task
         * execution. It mainly protects against interrupts that are
         * intended to cancel the worker thread from instead
         * interrupting the task being run.
         */
        private final ReentrantLock runLock = new ReentrantLock();

        /**
         * Initial task to run before entering run loop
         */
        private Runnable firstTask;

        /**
         * Per thread completed task counter; accumulated
         * into completedTaskCount upon termination.
         */
        volatile long completedTasks;

        /**
         * Thread this worker is running in.  Acts as a final field,
         * but cannot be set until thread is created.
         */
        Thread thread;

        Worker(Runnable firstTask) {
            this.firstTask = firstTask;
        }

        boolean isActive() {
            return runLock.isLocked();
        }

        /**
         * Interrupts thread if not running a task.
         */
        void interruptIfIdle() {
            final ReentrantLock runLock = this.runLock;
            if (runLock.tryLock()) {
                try {
                    thread.interrupt();
                } finally {
                    runLock.unlock();
                }
            }
        }

        /**
         * Interrupts thread even if running a task.
         */
        void interruptNow() {
            thread.interrupt();
        }

        /**
         * Runs a single task between before/after methods.
         */
        private void runTask(Runnable task) {
            final ReentrantLock runLock = this.runLock;
            runLock.lock();
            try {
                // If not shutting down then clear an outstanding interrupt.
                if (runState != STOP &&
                    Thread.interrupted() &&
                    runState == STOP) // Re-interrupt if stopped after clearing
                    thread.interrupt();
                boolean ran = false;
                beforeExecute(thread, task);
                try {
                    task.run();
                    ran = true;
                    afterExecute(task, null);
                    ++completedTasks;
                } catch (RuntimeException ex) {
                    if (!ran)
                        afterExecute(task, ex);
                    // Else the exception occurred within
                    // afterExecute itself in which case we don't
                    // want to call it again.
                    throw ex;
                }
            } finally {
                runLock.unlock();
            }
        }

        /**
         * Main run loop
         */
        public void run() {
            try {
                Runnable task = firstTask;
                firstTask = null;
                while (task != null || (task = getTask()) != null) {
                    runTask(task);
                    task = null; // unnecessary but can help GC
                }
            } finally {
                workerDone(this);
            }
        }
    }

    // Public methods

    /**
     * Creates a new <tt>ThreadPoolExecutor</tt> with the given initial
     * parameters and default thread factory and rejected execution handler.
     * It may be more convenient to use one of the {@link Executors} factory
     * methods instead of this general purpose constructor.
     *
     * @param corePoolSize the number of threads to keep in the
     * pool, even if they are idle.
     * @param maximumPoolSize the maximum number of threads to allow in the
     * pool.
     * @param keepAliveTime when the number of threads is greater than
     * the core, this is the maximum time that excess idle threads
     * will wait for new tasks before terminating.
     * @param unit the time unit for the keepAliveTime
     * argument.
     * @param workQueue the queue to use for holding tasks before they
     * are executed. This queue will hold only the <tt>Runnable</tt>
     * tasks submitted by the <tt>execute</tt> method.
     * @throws IllegalArgumentException if corePoolSize, or
     * keepAliveTime less than zero, or if maximumPoolSize less than or
     * equal to zero, or if corePoolSize greater than maximumPoolSize.
     * @throws NullPointerException if <tt>workQueue</tt> is null
     */
    public ThreadPoolExecutor(int corePoolSize,
                              int maximumPoolSize,
                              long keepAliveTime,
                              TimeUnit unit,
                              BlockingQueue<Runnable> workQueue) {
        this(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue,
             Executors.defaultThreadFactory(), defaultHandler);
    }

    /**
     * Creates a new <tt>ThreadPoolExecutor</tt> with the given initial
     * parameters and default rejected execution handler.
     *
     * @param corePoolSize the number of threads to keep in the
     * pool, even if they are idle.
     * @param maximumPoolSize the maximum number of threads to allow in the
     * pool.
     * @param keepAliveTime when the number of threads is greater than
     * the core, this is the maximum time that excess idle threads
     * will wait for new tasks before terminating.
     * @param unit the time unit for the keepAliveTime
     * argument.
     * @param workQueue the queue to use for holding tasks before they
     * are executed. This queue will hold only the <tt>Runnable</tt>
     * tasks submitted by the <tt>execute</tt> method.
     * @param threadFactory the factory to use when the executor
     * creates a new thread.
     * @throws IllegalArgumentException if corePoolSize, or
     * keepAliveTime less than zero, or if maximumPoolSize less than or
     * equal to zero, or if corePoolSize greater than maximumPoolSize.
     * @throws NullPointerException if <tt>workQueue</tt>
     * or <tt>threadFactory</tt> are null.
     */
    public ThreadPoolExecutor(int corePoolSize,
                              int maximumPoolSize,
                              long keepAliveTime,
                              TimeUnit unit,
                              BlockingQueue<Runnable> workQueue,
                              ThreadFactory threadFactory) {
        this(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue,
             threadFactory, defaultHandler);
    }

    /**
     * Creates a new <tt>ThreadPoolExecutor</tt> with the given initial
     * parameters and default thread factory.
     *
     * @param corePoolSize the number of threads to keep in the
     * pool, even if they are idle.
     * @param maximumPoolSize the maximum number of threads to allow in the
     * pool.
     * @param keepAliveTime when the number of threads is greater than
     * the core, this is the maximum time that excess idle threads
     * will wait for new tasks before terminating.
     * @param unit the time unit for the keepAliveTime
     * argument.
     * @param workQueue the queue to use for holding tasks before they
     * are executed. This queue will hold only the <tt>Runnable</tt>
     * tasks submitted by the <tt>execute</tt> method.
     * @param handler the handler to use when execution is blocked
     * because the thread bounds and queue capacities are reached.
     * @throws IllegalArgumentException if corePoolSize, or
     * keepAliveTime less than zero, or if maximumPoolSize less than or
     * equal to zero, or if corePoolSize greater than maximumPoolSize.
     * @throws NullPointerException if <tt>workQueue</tt>
     * or <tt>handler</tt> are null.
     */
    public ThreadPoolExecutor(int corePoolSize,
                              int maximumPoolSize,
                              long keepAliveTime,
                              TimeUnit unit,
                              BlockingQueue<Runnable> workQueue,
                              RejectedExecutionHandler handler) {
        this(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue,
             Executors.defaultThreadFactory(), handler);
    }

    /**
     * Creates a new <tt>ThreadPoolExecutor</tt> with the given initial
     * parameters.
     *
     * @param corePoolSize the number of threads to keep in the
     * pool, even if they are idle.
     * @param maximumPoolSize the maximum number of threads to allow in the
     * pool.
     * @param keepAliveTime when the number of threads is greater than
     * the core, this is the maximum time that excess idle threads
     * will wait for new tasks before terminating.
     * @param unit the time unit for the keepAliveTime
     * argument.
     * @param workQueue the queue to use for holding tasks before they
     * are executed. This queue will hold only the <tt>Runnable</tt>
     * tasks submitted by the <tt>execute</tt> method.
     * @param threadFactory the factory to use when the executor
     * creates a new thread.
     * @param handler the handler to use when execution is blocked
     * because the thread bounds and queue capacities are reached.
     * @throws IllegalArgumentException if corePoolSize, or
     * keepAliveTime less than zero, or if maximumPoolSize less than or
     * equal to zero, or if corePoolSize greater than maximumPoolSize.
     * @throws NullPointerException if <tt>workQueue</tt>
     * or <tt>threadFactory</tt> or <tt>handler</tt> are null.
     */
    public ThreadPoolExecutor(int corePoolSize,
                              int maximumPoolSize,
                              long keepAliveTime,
                              TimeUnit unit,
                              BlockingQueue<Runnable> workQueue,
                              ThreadFactory threadFactory,
                              RejectedExecutionHandler handler) {
        if (corePoolSize < 0 ||
            maximumPoolSize <= 0 ||
            maximumPoolSize < corePoolSize ||
            keepAliveTime < 0)
            throw new IllegalArgumentException();
        if (workQueue == null || threadFactory == null || handler == null)
            throw new NullPointerException();
        this.corePoolSize = corePoolSize;
        this.maximumPoolSize = maximumPoolSize;
        this.workQueue = workQueue;
        this.keepAliveTime = unit.toNanos(keepAliveTime);
        this.threadFactory = threadFactory;
        this.handler = handler;
    }


    /**
     * Executes the given task sometime in the future.  The task
     * may execute in a new thread or in an existing pooled thread.
     *
     * If the task cannot be submitted for execution, either because this
     * executor has been shutdown or because its capacity has been reached,
     * the task is handled by the current <tt>RejectedExecutionHandler</tt>.
     *
     * @param command the task to execute
     * @throws RejectedExecutionException at discretion of
     * <tt>RejectedExecutionHandler</tt>, if task cannot be accepted
     * for execution
     * @throws NullPointerException if command is null
     */
    public void execute(Runnable command) {
        if (command == null)
            throw new NullPointerException();
        for (;;) {
            if (runState != RUNNING) {
                reject(command);
                return;
            }
            if (poolSize < corePoolSize && addIfUnderCorePoolSize(command))
                return;
            if (workQueue.offer(command))
                return;
            int status = addIfUnderMaximumPoolSize(command);
            if (status > 0)      // created new thread
                return;
            if (status == 0) {   // failed to create thread
                reject(command);
                return;
            }
            // Retry if created a new thread but it is busy with another task
        }
    }

    /**
     * Initiates an orderly shutdown in which previously submitted
     * tasks are executed, but no new tasks will be
     * accepted. Invocation has no additional effect if already shut
     * down.
     * @throws SecurityException if a security manager exists and
     * shutting down this ExecutorService may manipulate threads that
     * the caller is not permitted to modify because it does not hold
     * {@link java.lang.RuntimePermission}<tt>("modifyThread")</tt>,
     * or the security manager's <tt>checkAccess</tt> method denies access.
     */
    public void shutdown() {
        // Fail if caller doesn't have modifyThread permission.
        SecurityManager security = System.getSecurityManager();
        if (security != null)
            security.checkPermission(shutdownPerm);

        boolean fullyTerminated = false;
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            if (workers.size() > 0) {
                // Check if caller can modify worker threads.  This
                // might not be true even if passed above check, if
                // the SecurityManager treats some threads specially.
                if (security != null) {
                    for (Worker w: workers)
                        security.checkAccess(w.thread);
                }

                int state = runState;
                if (state == RUNNING) // don't override shutdownNow
                    runState = SHUTDOWN;

                try {
                    for (Worker w: workers)
                        w.interruptIfIdle();
                } catch (SecurityException se) {
                    // If SecurityManager allows above checks, but
                    // then unexpectedly throws exception when
                    // interrupting threads (which it ought not do),
                    // back out as cleanly as we can. Some threads may
                    // have been killed but we remain in non-shutdown
                    // state.
                    runState = state;
                    throw se;
                }
            }
            else { // If no workers, trigger full termination now
                fullyTerminated = true;
                runState = TERMINATED;
                termination.signalAll();
            }
        } finally {
            mainLock.unlock();
        }
        if (fullyTerminated)
            terminated();
    }


    /**
     * Attempts to stop all actively executing tasks, halts the
     * processing of waiting tasks, and returns a list of the tasks
     * that were awaiting execution.
     *
     * <p>There are no guarantees beyond best-effort attempts to stop
     * processing actively executing tasks.  This implementation
     * cancels tasks via {@link Thread#interrupt}, so any task that
     * fails to respond to interrupts may never terminate.
     *
     * @return list of tasks that never commenced execution
     * @throws SecurityException if a security manager exists and
     * shutting down this ExecutorService may manipulate threads that
     * the caller is not permitted to modify because it does not hold
     * {@link java.lang.RuntimePermission}<tt>("modifyThread")</tt>,
     * or the security manager's <tt>checkAccess</tt> method denies access.
     */
    public List<Runnable> shutdownNow() {
        // Almost the same code as shutdown()
        SecurityManager security = System.getSecurityManager();
        if (security != null)
            security.checkPermission(shutdownPerm);

        boolean fullyTerminated = false;
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            if (workers.size() > 0) {
                if (security != null) {
                    for (Worker w: workers)
                        security.checkAccess(w.thread);
                }

                int state = runState;
                if (state != TERMINATED)
                    runState = STOP;
                try {
                    for (Worker w : workers)
                        w.interruptNow();
                } catch (SecurityException se) {
                    runState = state; // back out;
                    throw se;
                }
            }
            else { // If no workers, trigger full termination now
                fullyTerminated = true;
                runState = TERMINATED;
                termination.signalAll();
            }
        } finally {
            mainLock.unlock();
        }
        if (fullyTerminated)
            terminated();
        return Arrays.asList(workQueue.toArray(EMPTY_RUNNABLE_ARRAY));
    }

    public boolean isShutdown() {
        return runState != RUNNING;
    }

    /**
     * Returns true if this executor is in the process of terminating
     * after <tt>shutdown</tt> or <tt>shutdownNow</tt> but has not
     * completely terminated.  This method may be useful for
     * debugging. A return of <tt>true</tt> reported a sufficient
     * period after shutdown may indicate that submitted tasks have
     * ignored or suppressed interruption, causing this executor not
     * to properly terminate.
     * @return true if terminating but not yet terminated.
     */
    public boolean isTerminating() {
        return runState == STOP;
    }

    public boolean isTerminated() {
        return runState == TERMINATED;
    }

    public boolean awaitTermination(long timeout, TimeUnit unit)
        throws InterruptedException {
        long nanos = unit.toNanos(timeout);
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            for (;;) {
                if (runState == TERMINATED)
                    return true;
                if (nanos <= 0)
                    return false;
                nanos = termination.awaitNanos(nanos);
            }
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Invokes <tt>shutdown</tt> when this executor is no longer
     * referenced.
     */
    protected void finalize()  {
        shutdown();
    }

    /**
     * Sets the thread factory used to create new threads.
     *
     * @param threadFactory the new thread factory
     * @throws NullPointerException if threadFactory is null
     * @see #getThreadFactory
     */
    public void setThreadFactory(ThreadFactory threadFactory) {
        if (threadFactory == null)
            throw new NullPointerException();
        this.threadFactory = threadFactory;
    }

    /**
     * Returns the thread factory used to create new threads.
     *
     * @return the current thread factory
     * @see #setThreadFactory
     */
    public ThreadFactory getThreadFactory() {
        return threadFactory;
    }

    /**
     * Sets a new handler for unexecutable tasks.
     *
     * @param handler the new handler
     * @throws NullPointerException if handler is null
     * @see #getRejectedExecutionHandler
     */
    public void setRejectedExecutionHandler(RejectedExecutionHandler handler) {
        if (handler == null)
            throw new NullPointerException();
        this.handler = handler;
    }

    /**
     * Returns the current handler for unexecutable tasks.
     *
     * @return the current handler
     * @see #setRejectedExecutionHandler
     */
    public RejectedExecutionHandler getRejectedExecutionHandler() {
        return handler;
    }

    /**
     * Returns the task queue used by this executor. Access to the
     * task queue is intended primarily for debugging and monitoring.
     * This queue may be in active use.  Retrieving the task queue
     * does not prevent queued tasks from executing.
     *
     * @return the task queue
     */
    public BlockingQueue<Runnable> getQueue() {
        return workQueue;
    }

    /**
     * Removes this task from the executor's internal queue if it is
     * present, thus causing it not to be run if it has not already
     * started.
     *
     * <p> This method may be useful as one part of a cancellation
     * scheme.  It may fail to remove tasks that have been converted
     * into other forms before being placed on the internal queue. For
     * example, a task entered using <tt>submit</tt> might be
     * converted into a form that maintains <tt>Future</tt> status.
     * However, in such cases, method {@link ThreadPoolExecutor#purge}
     * may be used to remove those Futures that have been cancelled.
     *
     * @param task the task to remove
     * @return true if the task was removed
     */
    public boolean remove(Runnable task) {
        return getQueue().remove(task);
    }


    /**
     * Tries to remove from the work queue all {@link Future}
     * tasks that have been cancelled. This method can be useful as a
     * storage reclamation operation, that has no other impact on
     * functionality. Cancelled tasks are never executed, but may
     * accumulate in work queues until worker threads can actively
     * remove them. Invoking this method instead tries to remove them now.
     * However, this method may fail to remove tasks in
     * the presence of interference by other threads.
     */
    public void purge() {
        // Fail if we encounter interference during traversal
        try {
            Iterator<Runnable> it = getQueue().iterator();
            while (it.hasNext()) {
                Runnable r = it.next();
                if (r instanceof Future<?>) {
                    Future<?> c = (Future<?>)r;
                    if (c.isCancelled())
                        it.remove();
                }
            }
        }
        catch (ConcurrentModificationException ex) {
            return;
        }
    }

    /**
     * Sets the core number of threads.  This overrides any value set
     * in the constructor.  If the new value is smaller than the
     * current value, excess existing threads will be terminated when
     * they next become idle. If larger, new threads will, if needed,
     * be started to execute any queued tasks.
     *
     * @param corePoolSize the new core size
     * @throws IllegalArgumentException if <tt>corePoolSize</tt>
     * less than zero
     * @see #getCorePoolSize
     */
    public void setCorePoolSize(int corePoolSize) {
        if (corePoolSize < 0)
            throw new IllegalArgumentException();
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            int extra = this.corePoolSize - corePoolSize;
            this.corePoolSize = corePoolSize;
            if (extra < 0) {
                int n = workQueue.size();
                // We have to create initially-idle threads here
                // because we otherwise have no recourse about
                // what to do with a dequeued task if addThread fails.
                while (extra++ < 0 && n-- > 0 && poolSize < corePoolSize ) {
                    Thread t = addThread(null);
                    if (t != null)
                        t.start();
                    else
                        break;
                }
            }
            else if (extra > 0 && poolSize > corePoolSize) {
                Iterator<Worker> it = workers.iterator();
                while (it.hasNext() &&
                       extra-- > 0 &&
                       poolSize > corePoolSize &&
                       workQueue.remainingCapacity() == 0)
                    it.next().interruptIfIdle();
            }
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Returns the core number of threads.
     *
     * @return the core number of threads
     * @see #setCorePoolSize
     */
    public int getCorePoolSize() {
        return corePoolSize;
    }

    /**
     * Starts a core thread, causing it to idly wait for work. This
     * overrides the default policy of starting core threads only when
     * new tasks are executed. This method will return <tt>false</tt>
     * if all core threads have already been started.
     * @return true if a thread was started
     */
    public boolean prestartCoreThread() {
        return addIfUnderCorePoolSize(null);
    }

    /**
     * Starts all core threads, causing them to idly wait for work. This
     * overrides the default policy of starting core threads only when
     * new tasks are executed.
     * @return the number of threads started.
     */
    public int prestartAllCoreThreads() {
        int n = 0;
        while (addIfUnderCorePoolSize(null))
            ++n;
        return n;
    }

    /**
     * Returns true if this pool allows core threads to time out and
     * terminate if no tasks arrive within the keepAlive time, being
     * replaced if needed when new tasks arrive. When true, the same
     * keep-alive policy applying to non-core threads applies also to
     * core threads. When false (the default), core threads are never
     * terminated due to lack of incoming tasks.
     * @return <tt>true</tt> if core threads are allowed to time out,
     * else <tt>false</tt>
     *
     * @since 1.6
     */
    public boolean allowsCoreThreadTimeOut() {
        return allowCoreThreadTimeOut;
    }

    /**
     * Sets the policy governing whether core threads may time out and
     * terminate if no tasks arrive within the keep-alive time, being
     * replaced if needed when new tasks arrive. When false, core
     * threads are never terminated due to lack of incoming
     * tasks. When true, the same keep-alive policy applying to
     * non-core threads applies also to core threads. To avoid
     * continual thread replacement, the keep-alive time must be
     * greater than zero when setting <tt>true</tt>. This method
     * should in general be called before the pool is actively used.
     * @param value <tt>true</tt> if should time out, else <tt>false</tt>
     * @throws IllegalArgumentException if value is <tt>true</tt>
     * and the current keep-alive time is not greater than zero.
     *
     * @since 1.6
     */
    public void allowCoreThreadTimeOut(boolean value) {
        if (value && keepAliveTime <= 0)
            throw new IllegalArgumentException("Core threads must have nonzero keep alive times");

        allowCoreThreadTimeOut = value;
    }

    /**
     * Sets the maximum allowed number of threads. This overrides any
     * value set in the constructor. If the new value is smaller than
     * the current value, excess existing threads will be
     * terminated when they next become idle.
     *
     * @param maximumPoolSize the new maximum
     * @throws IllegalArgumentException if the new maximum is
     *         less than or equal to zero, or
     *         less than the {@linkplain #getCorePoolSize core pool size}
     * @see #getMaximumPoolSize
     */
    public void setMaximumPoolSize(int maximumPoolSize) {
        if (maximumPoolSize <= 0 || maximumPoolSize < corePoolSize)
            throw new IllegalArgumentException();
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            int extra = this.maximumPoolSize - maximumPoolSize;
            this.maximumPoolSize = maximumPoolSize;
            if (extra > 0 && poolSize > maximumPoolSize) {
                Iterator<Worker> it = workers.iterator();
                while (it.hasNext() &&
                       extra > 0 &&
                       poolSize > maximumPoolSize) {
                    it.next().interruptIfIdle();
                    --extra;
                }
            }
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Returns the maximum allowed number of threads.
     *
     * @return the maximum allowed number of threads
     * @see #setMaximumPoolSize
     */
    public int getMaximumPoolSize() {
        return maximumPoolSize;
    }

    /**
     * Sets the time limit for which threads may remain idle before
     * being terminated.  If there are more than the core number of
     * threads currently in the pool, after waiting this amount of
     * time without processing a task, excess threads will be
     * terminated.  This overrides any value set in the constructor.
     * @param time the time to wait.  A time value of zero will cause
     * excess threads to terminate immediately after executing tasks.
     * @param unit  the time unit of the time argument
     * @throws IllegalArgumentException if time less than zero or
     * if time is zero and allowsCoreThreadTimeOut
     * @see #getKeepAliveTime
     */
    public void setKeepAliveTime(long time, TimeUnit unit) {
        if (time < 0)
            throw new IllegalArgumentException();
        if (time == 0 && allowsCoreThreadTimeOut())
            throw new IllegalArgumentException("Core threads must have nonzero keep alive times");
        this.keepAliveTime = unit.toNanos(time);
    }

    /**
     * Returns the thread keep-alive time, which is the amount of time
     * which threads in excess of the core pool size may remain
     * idle before being terminated.
     *
     * @param unit the desired time unit of the result
     * @return the time limit
     * @see #setKeepAliveTime
     */
    public long getKeepAliveTime(TimeUnit unit) {
        return unit.convert(keepAliveTime, TimeUnit.NANOSECONDS);
    }

    /* Statistics */

    /**
     * Returns the current number of threads in the pool.
     *
     * @return the number of threads
     */
    public int getPoolSize() {
        return poolSize;
    }

    /**
     * Returns the approximate number of threads that are actively
     * executing tasks.
     *
     * @return the number of threads
     */
    public int getActiveCount() {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            int n = 0;
            for (Worker w : workers) {
                if (w.isActive())
                    ++n;
            }
            return n;
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Returns the largest number of threads that have ever
     * simultaneously been in the pool.
     *
     * @return the number of threads
     */
    public int getLargestPoolSize() {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            return largestPoolSize;
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Returns the approximate total number of tasks that have been
     * scheduled for execution. Because the states of tasks and
     * threads may change dynamically during computation, the returned
     * value is only an approximation, but one that does not ever
     * decrease across successive calls.
     *
     * @return the number of tasks
     */
    public long getTaskCount() {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            long n = completedTaskCount;
            for (Worker w : workers) {
                n += w.completedTasks;
                if (w.isActive())
                    ++n;
            }
            return n + workQueue.size();
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Returns the approximate total number of tasks that have
     * completed execution. Because the states of tasks and threads
     * may change dynamically during computation, the returned value
     * is only an approximation, but one that does not ever decrease
     * across successive calls.
     *
     * @return the number of tasks
     */
    public long getCompletedTaskCount() {
        final ReentrantLock mainLock = this.mainLock;
        mainLock.lock();
        try {
            long n = completedTaskCount;
            for (Worker w : workers)
                n += w.completedTasks;
            return n;
        } finally {
            mainLock.unlock();
        }
    }

    /**
     * Method invoked prior to executing the given Runnable in the
     * given thread.  This method is invoked by thread <tt>t</tt> that
     * will execute task <tt>r</tt>, and may be used to re-initialize
     * ThreadLocals, or to perform logging.
     *
     * <p>This implementation does nothing, but may be customized in
     * subclasses. Note: To properly nest multiple overridings, subclasses
     * should generally invoke <tt>super.beforeExecute</tt> at the end of
     * this method.
     *
     * @param t the thread that will run task r.
     * @param r the task that will be executed.
     */
    protected void beforeExecute(Thread t, Runnable r) { }

    /**
     * Method invoked upon completion of execution of the given Runnable.
     * This method is invoked by the thread that executed the task. If
     * non-null, the Throwable is the uncaught <tt>RuntimeException</tt>
     * or <tt>Error</tt> that caused execution to terminate abruptly.
     *
     * <p><b>Note:</b> When actions are enclosed in tasks (such as
     * {@link FutureTask}) either explicitly or via methods such as
     * <tt>submit</tt>, these task objects catch and maintain
     * computational exceptions, and so they do not cause abrupt
     * termination, and the internal exceptions are <em>not</em>
     * passed to this method.
     *
     * <p>This implementation does nothing, but may be customized in
     * subclasses. Note: To properly nest multiple overridings, subclasses
     * should generally invoke <tt>super.afterExecute</tt> at the
     * beginning of this method.
     *
     * @param r the runnable that has completed.
     * @param t the exception that caused termination, or null if
     * execution completed normally.
     */
    protected void afterExecute(Runnable r, Throwable t) { }

    /**
     * Method invoked when the Executor has terminated.  Default
     * implementation does nothing. Note: To properly nest multiple
     * overridings, subclasses should generally invoke
     * <tt>super.terminated</tt> within this method.
     */
    protected void terminated() { }

    /**
     * A handler for rejected tasks that runs the rejected task
     * directly in the calling thread of the <tt>execute</tt> method,
     * unless the executor has been shut down, in which case the task
     * is discarded.
     */
    public static class CallerRunsPolicy implements RejectedExecutionHandler {
        /**
         * Creates a <tt>CallerRunsPolicy</tt>.
         */
        public CallerRunsPolicy() { }

        /**
         * Executes task r in the caller's thread, unless the executor
         * has been shut down, in which case the task is discarded.
         * @param r the runnable task requested to be executed
         * @param e the executor attempting to execute this task
         */
        public void rejectedExecution(Runnable r, ThreadPoolExecutor e) {
            if (!e.isShutdown()) {
                r.run();
            }
        }
    }

    /**
     * A handler for rejected tasks that throws a
     * <tt>RejectedExecutionException</tt>.
     */
    public static class AbortPolicy implements RejectedExecutionHandler {
        /**
         * Creates an <tt>AbortPolicy</tt>.
         */
        public AbortPolicy() { }

        /**
         * Always throws RejectedExecutionException.
         * @param r the runnable task requested to be executed
         * @param e the executor attempting to execute this task
         * @throws RejectedExecutionException always.
         */
        public void rejectedExecution(Runnable r, ThreadPoolExecutor e) {
            throw new RejectedExecutionException();
        }
    }

    /**
     * A handler for rejected tasks that silently discards the
     * rejected task.
     */
    public static class DiscardPolicy implements RejectedExecutionHandler {
        /**
         * Creates a <tt>DiscardPolicy</tt>.
         */
        public DiscardPolicy() { }

        /**
         * Does nothing, which has the effect of discarding task r.
         * @param r the runnable task requested to be executed
         * @param e the executor attempting to execute this task
         */
        public void rejectedExecution(Runnable r, ThreadPoolExecutor e) {
        }
    }

    /**
     * A handler for rejected tasks that discards the oldest unhandled
     * request and then retries <tt>execute</tt>, unless the executor
     * is shut down, in which case the task is discarded.
     */
    public static class DiscardOldestPolicy implements RejectedExecutionHandler {
        /**
         * Creates a <tt>DiscardOldestPolicy</tt> for the given executor.
         */
        public DiscardOldestPolicy() { }

        /**
         * Obtains and ignores the next task that the executor
         * would otherwise execute, if one is immediately available,
         * and then retries execution of task r, unless the executor
         * is shut down, in which case task r is instead discarded.
         * @param r the runnable task requested to be executed
         * @param e the executor attempting to execute this task
         */
        public void rejectedExecution(Runnable r, ThreadPoolExecutor e) {
            if (!e.isShutdown()) {
                e.getQueue().poll();
                e.execute(r);
            }
        }
    }
}

/**
 * The osthread module provides types used in threads modules.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Walter Bright, Alex Rønne Petersen, Martin Nowak
 * Source:    $(DRUNTIMESRC core/thread/threadgroup.d)
 */

module core.thread.threadgroup;

import core.thread.osthread;


/**
 * This class is intended to simplify certain common programming techniques.
 */
class ThreadGroup
{
    /**
     * Creates and starts a new Thread object that executes fn and adds it to
     * the list of tracked threads.
     *
     * Params:
     *  fn = The thread function.
     *
     * Returns:
     *  A reference to the newly created thread.
     */
    final Thread create(void function() fn)
    {
        Thread t = new Thread(fn).start();

        synchronized(this)
        {
            m_all[t] = t;
        }
        return t;
    }


    /**
     * Creates and starts a new Thread object that executes dg and adds it to
     * the list of tracked threads.
     *
     * Params:
     *  dg = The thread function.
     *
     * Returns:
     *  A reference to the newly created thread.
     */
    final Thread create(void delegate() dg)
    {
        Thread t = new Thread(dg).start();

        synchronized(this)
        {
            m_all[t] = t;
        }
        return t;
    }


    /**
     * Add t to the list of tracked threads if it is not already being tracked.
     *
     * Params:
     *  t = The thread to add.
     *
     * In:
     *  t must not be null.
     */
    final void add(Thread t)
    in
    {
        assert(t);
    }
    do
    {
        synchronized(this)
        {
            m_all[t] = t;
        }
    }


    /**
     * Removes t from the list of tracked threads.  No operation will be
     * performed if t is not currently being tracked by this object.
     *
     * Params:
     *  t = The thread to remove.
     *
     * In:
     *  t must not be null.
     */
    final void remove(Thread t)
    in
    {
        assert(t);
    }
    do
    {
        synchronized(this)
        {
            m_all.remove(t);
        }
    }


    /**
     * Operates on all threads currently tracked by this object.
     */
    final int opApply(scope int delegate(ref Thread) dg)
    {
        synchronized(this)
        {
            int ret = 0;

            // NOTE: This loop relies on the knowledge that m_all uses the
            //       Thread object for both the key and the mapped value.
            foreach (Thread t; m_all.keys)
            {
                ret = dg(t);
                if (ret)
                    break;
            }
            return ret;
        }
    }


    /**
     * Iteratively joins all tracked threads.  This function will block add,
     * remove, and opApply until it completes.
     *
     * Params:
     *  rethrow = Rethrow any unhandled exception which may have caused the
     *            current thread to terminate.
     *
     * Throws:
     *  Any exception not handled by the joined threads.
     */
    final void joinAll(bool rethrow = true)
    {
        synchronized(this)
        {
            // NOTE: This loop relies on the knowledge that m_all uses the
            //       Thread object for both the key and the mapped value.
            foreach (Thread t; m_all.keys)
            {
                t.join(rethrow);
            }
        }
    }


private:
    Thread[Thread]  m_all;
}

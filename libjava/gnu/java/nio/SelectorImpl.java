/* SelectorImpl.java -- 
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.nio;

import java.io.IOException;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.AbstractSelector;
import java.nio.channels.spi.SelectorProvider;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

public class SelectorImpl extends AbstractSelector
{
  private Set keys;
  private Set selected;

  /**
   * A dummy object whose monitor regulates access to both our
   * selectThread and unhandledWakeup fields.
   */
  private Object selectThreadMutex = new Object ();
  
  /**
   * Any thread that's currently blocked in a select operation.
   */
  private Thread selectThread;
  
  /**
   * Indicates whether we have an unhandled wakeup call. This can
   * be due to either wakeup() triggering a thread interruption while
   * a thread was blocked in a select operation (in which case we need
   * to reset this thread's interrupt status after interrupting the
   * select), or else that no thread was on a select operation at the
   * time that wakeup() was called, in which case the following select()
   * operation should return immediately with nothing selected.
   */
  private boolean unhandledWakeup;

  public SelectorImpl (SelectorProvider provider)
  {
    super (provider);
    
    keys = new HashSet ();
    selected = new HashSet ();
  }

  protected void finalize() throws Throwable
  {
    close();
  }

  protected final void implCloseSelector()
    throws IOException
  {
    // Cancel any pending select operation.
    wakeup();
    
    synchronized (keys)
      {
        synchronized (selected)
          {
            synchronized (cancelledKeys ())
              {
                // FIXME: Release resources here.
              }
          }
      }
  }

  public final Set keys()
  {
    if (!isOpen())
      throw new ClosedSelectorException();

    return Collections.unmodifiableSet (keys);
  }
    
  public final int selectNow()
    throws IOException
  {
    // FIXME: We're simulating an immediate select
    // via a select with a timeout of one millisecond.
    return select (1);
  }

  public final int select()
    throws IOException
  {
    return select (0);
  }

  private final int[] getFDsAsArray (int ops)
  {
    int[] result;
    int counter = 0;
    Iterator it = keys.iterator ();

    // Count the number of file descriptors needed
    while (it.hasNext ())
      {
        SelectionKeyImpl key = (SelectionKeyImpl) it.next ();

        if ((key.interestOps () & ops) != 0)
          {
            counter++;
          }
      }

    result = new int[counter];

    counter = 0;
    it = keys.iterator ();

    // Fill the array with the file descriptors
    while (it.hasNext ())
      {
        SelectionKeyImpl key = (SelectionKeyImpl) it.next ();

        if ((key.interestOps () & ops) != 0)
          {
            result[counter] = key.getNativeFD();
            counter++;
          }
      }

    return result;
  }

  public synchronized int select (long timeout)
    throws IOException
  {
    if (!isOpen())
      throw new ClosedSelectorException();
      
    synchronized (keys)
      {
        synchronized (selected)
          {
            deregisterCancelledKeys();

            // Set only keys with the needed interest ops into the arrays.
            int[] read = getFDsAsArray (SelectionKey.OP_READ
                                        | SelectionKey.OP_ACCEPT);
            int[] write = getFDsAsArray (SelectionKey.OP_WRITE
                                         | SelectionKey.OP_CONNECT);

            // FIXME: We dont need to check this yet
            int[] except = new int [0];

            // Test to see if we've got an unhandled wakeup call,
            // in which case we return immediately. Otherwise,
            // remember our current thread and jump into the select.
            // The monitor for dummy object selectThreadMutex regulates
            // access to these fields.

            // FIXME: Not sure from the spec at what point we should
            // return "immediately". Is it here or immediately upon
            // entry to this function?
            
            // NOTE: There's a possibility of another thread calling
            // wakeup() immediately after our thread releases
            // selectThreadMutex's monitor here, in which case we'll
            // do the select anyway. Since calls to wakeup() and select()
            // among different threads happen in non-deterministic order,
            // I don't think this is an issue.
            synchronized (selectThreadMutex)
              {
                if (unhandledWakeup)
                  {
                    unhandledWakeup = false;
                    return 0;
                  }
                else
                  {
                    selectThread = Thread.currentThread ();
                  }
              }

            // Call the native select() on all file descriptors.
            int result = 0;
            try
              {
                begin();
                result = VMSelector.select (read, write, except, timeout);
              }
            finally
              {
                end();
              }

            // If our unhandled wakeup flag is set at this point,
            // reset our thread's interrupt flag because we were
            // awakened by wakeup() instead of an external thread
            // interruption.
            //
            // NOTE: If we were blocked in a select() and one thread
            // called Thread.interrupt() on the blocked thread followed
            // by another thread calling Selector.wakeup(), then race
            // conditions could make it so that the thread's interrupt
            // flag is reset even though the Thread.interrupt() call
            // "was there first". I don't think we need to care about
            // this scenario.
            synchronized (selectThreadMutex)
              {
                if (unhandledWakeup)
                  {
                    unhandledWakeup = false;
                    Thread.interrupted ();
                  }
                selectThread = null;
              }

            Iterator it = keys.iterator ();

            while (it.hasNext ())
              {
                int ops = 0;
                SelectionKeyImpl key = (SelectionKeyImpl) it.next ();

                // If key is already selected retrieve old ready ops.
                if (selected.contains (key))
                  {
                    ops = key.readyOps ();
                  }

                // Set new ready read/accept ops
                for (int i = 0; i < read.length; i++)
                  {
                    if (key.getNativeFD() == read[i])
                      {
                        if (key.channel () instanceof ServerSocketChannelImpl)
                          {
                            ops = ops | SelectionKey.OP_ACCEPT;
                          }
                        else
                          {
                            ops = ops | SelectionKey.OP_READ;
                          }
                      }
                  }

                // Set new ready write ops
                for (int i = 0; i < write.length; i++)
                  {
                    if (key.getNativeFD() == write[i])
                      {
                        ops = ops | SelectionKey.OP_WRITE;

        //                 if (key.channel ().isConnected ())
        //                   {
        //                     ops = ops | SelectionKey.OP_WRITE;
        //                   }
        //                 else
        //                   {
        //                     ops = ops | SelectionKey.OP_CONNECT;
        //                   }
                     }
                  }

                // FIXME: We dont handle exceptional file descriptors yet.

                // If key is not yet selected add it.
                if (!selected.contains (key))
                  {
                    selected.add (key);
                  }

                // Set new ready ops
                key.readyOps (key.interestOps () & ops);
              }
            deregisterCancelledKeys();
            
            return result;
          }
        }
  }
    
  public final Set selectedKeys()
  {
    if (!isOpen())
      throw new ClosedSelectorException();

    return selected;
  }

  public final Selector wakeup()
  {
    // IMPLEMENTATION NOTE: Whereas the specification says that
    // thread interruption should trigger a call to wakeup, we
    // do the reverse under the covers: wakeup triggers a thread
    // interrupt followed by a subsequent reset of the thread's
    // interrupt status within select().
    
    // First, acquire the monitor of the object regulating
    // access to our selectThread and unhandledWakeup fields.
    synchronized (selectThreadMutex)
      {
        unhandledWakeup = true;
        
        // Interrupt any thread which is currently blocked in
        // a select operation.
        if (selectThread != null)
          selectThread.interrupt ();
      }
      
    return this;
  }

  private final void deregisterCancelledKeys()
  {
    Set ckeys = cancelledKeys ();
    synchronized (ckeys)
    {
      Iterator it = ckeys.iterator();

      while (it.hasNext ())
        {
          keys.remove ((SelectionKeyImpl) it.next ());
          it.remove ();
        }
    }
  }

  protected SelectionKey register (SelectableChannel ch, int ops, Object att)
  {
    return register ((AbstractSelectableChannel) ch, ops, att);
  }

  protected final SelectionKey register (AbstractSelectableChannel ch, int ops,
                                         Object att)
  {
    SelectionKeyImpl result;
    
    if (ch instanceof SocketChannelImpl)
      result = new SocketChannelSelectionKey (ch, this);
    else if (ch instanceof DatagramChannelImpl)
      result = new DatagramChannelSelectionKey (ch, this);
    else if (ch instanceof ServerSocketChannelImpl)
      result = new ServerSocketChannelSelectionKey (ch, this);
    else
      throw new InternalError ("No known channel type");

    synchronized (keys)
      {
        keys.add (result);
      }

    result.interestOps (ops);
    result.attach (att);
    return result;
  }
}

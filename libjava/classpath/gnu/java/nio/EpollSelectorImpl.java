/* EpollSelectorImpl.java -- selector implementation using epoll
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import gnu.classpath.Configuration;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.AbstractSelector;
import java.nio.channels.spi.SelectorProvider;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * An implementation of {@link Selector} that uses the epoll event
 * notification mechanism on GNU/Linux.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class EpollSelectorImpl extends AbstractSelector
{
  // XXX is this reasonable? Does it matter?
  private static final int DEFAULT_EPOLL_SIZE = 128;
  private static final int sizeof_struct_epoll_event;

  private static final int OP_ACCEPT  = SelectionKey.OP_ACCEPT;
  private static final int OP_CONNECT = SelectionKey.OP_CONNECT;
  private static final int OP_READ    = SelectionKey.OP_READ;
  private static final int OP_WRITE   = SelectionKey.OP_WRITE;

  /** our epoll file descriptor. */
  private int epoll_fd;

  private final HashMap keys;
  private Set selectedKeys;
  private Thread waitingThread;
  private ByteBuffer events;

  private static final int INITIAL_CAPACITY;
  private static final int MAX_DOUBLING_CAPACITY;
  private static final int CAPACITY_INCREMENT;

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      System.loadLibrary("javanio");

    if (epoll_supported())
      sizeof_struct_epoll_event = sizeof_struct();
    else
      sizeof_struct_epoll_event = -1;

    INITIAL_CAPACITY = 64 * sizeof_struct_epoll_event;
    MAX_DOUBLING_CAPACITY = 1024 * sizeof_struct_epoll_event;
    CAPACITY_INCREMENT = 128 * sizeof_struct_epoll_event;
  }

  public EpollSelectorImpl(SelectorProvider provider)
    throws IOException
  {
    super(provider);
    epoll_fd = epoll_create(DEFAULT_EPOLL_SIZE);
    keys = new HashMap();
    selectedKeys = null;
    events = ByteBuffer.allocateDirect(INITIAL_CAPACITY);
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#keys()
   */
  public Set keys()
  {
    return new HashSet(keys.values());
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#select()
   */
  public int select() throws IOException
  {
    return doSelect(-1);
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#select(long)
   */
  public int select(long timeout) throws IOException
  {
    if (timeout > Integer.MAX_VALUE)
      throw new IllegalArgumentException("timeout is too large");
    if (timeout < 0)
      throw new IllegalArgumentException("invalid timeout");
    return doSelect((int) timeout);
  }

  private int doSelect(int timeout) throws IOException
  {
    synchronized (keys)
    {
      Set cancelledKeys = cancelledKeys();
      synchronized (cancelledKeys)
      {
        for (Iterator it = cancelledKeys.iterator(); it.hasNext(); )
          {
            EpollSelectionKeyImpl key = (EpollSelectionKeyImpl) it.next();
            epoll_delete(epoll_fd, key.fd);
            key.valid = false;
            keys.remove(Integer.valueOf(key.fd));
            it.remove();
            deregister(key);
          }

        // Clear out closed channels. The fds are removed from the epoll
        // fd when closed, so there is no need to remove them manually.
        for (Iterator it = keys.values().iterator(); it.hasNext(); )
          {
            EpollSelectionKeyImpl key = (EpollSelectionKeyImpl) it.next();
            SelectableChannel ch = key.channel();
            if (ch instanceof VMChannelOwner)
              {
                if (!((VMChannelOwner) ch).getVMChannel().getState().isValid())
                  it.remove();
              }
          }

        // Don't bother if we have nothing to select.
        if (keys.isEmpty())
          return 0;

        int ret;
        try
          {
            begin();
            waitingThread = Thread.currentThread();
            ret = epoll_wait(epoll_fd, events, keys.size(), timeout);
          }
        finally
          {
            Thread.interrupted();
            waitingThread = null;
            end();
          }

        HashSet s = new HashSet(ret);
        for (int i = 0; i < ret; i++)
          {
            events.position(i * sizeof_struct_epoll_event);
            ByteBuffer b = events.slice();
            int fd = selected_fd(b);
            EpollSelectionKeyImpl key
              = (EpollSelectionKeyImpl) keys.get(Integer.valueOf(fd));
            if (key == null)
              throw new IOException("fd was selected, but no key found");
            key.selectedOps = selected_ops(b) & key.interestOps;
            s.add(key);
          }

        reallocateBuffer();

        selectedKeys = s;
        return ret;
      }
    }
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#selectedKeys()
   */
  public Set selectedKeys()
  {
    if (selectedKeys == null)
      return Collections.EMPTY_SET;
    return selectedKeys;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#selectNow()
   */
  public int selectNow() throws IOException
  {
    return doSelect(0);
  }

  /* (non-Javadoc)
   * @see java.nio.channels.Selector#wakeup()
   */
  public Selector wakeup()
  {
    try
      {
        waitingThread.interrupt();
      }
    catch (NullPointerException npe)
      {
        // Ignored, thrown if we are not in a blocking op.
      }
    return this;
  }

  /* (non-Javadoc)
   * @see java.nio.channels.spi.AbstractSelector#implCloseSelector()
   */
  protected void implCloseSelector() throws IOException
  {
    VMChannel.close(epoll_fd);
  }

  /* (non-Javadoc)
   * @see java.nio.channels.spi.AbstractSelector#register(java.nio.channels.spi.AbstractSelectableChannel, int, java.lang.Object)
   */
  protected SelectionKey register(AbstractSelectableChannel ch, int ops, Object att)
  {
    if (!(ch instanceof VMChannelOwner))
      throw new IllegalArgumentException("unsupported channel type");

    VMChannel channel = ((VMChannelOwner) ch).getVMChannel();
    try
      {
        int native_fd = channel.getState().getNativeFD();
        synchronized (keys)
        {
          if (keys.containsKey(Integer.valueOf(native_fd)))
            throw new IllegalArgumentException("channel already registered");
          EpollSelectionKeyImpl result =
            new EpollSelectionKeyImpl(this, ch, native_fd);
          if ((ops & ~(ch.validOps())) != 0)
            throw new IllegalArgumentException("invalid ops for channel");
          result.interestOps = ops;
          result.selectedOps = 0;
          result.valid = true;
          result.attach(att);
          result.key = System.identityHashCode(result);
          epoll_add(epoll_fd, result.fd, ops);
          keys.put(Integer.valueOf(native_fd), result);
          reallocateBuffer();
          return result;
        }
      }
    catch (IOException ioe)
      {
        throw new IllegalArgumentException(ioe);
      }
  }

  private void reallocateBuffer()
  {
    // Ensure we have enough space for all potential events that may be
    // returned.
    if (events.capacity() < keys.size() * sizeof_struct_epoll_event)
      {
        int cap = events.capacity();
        if (cap < MAX_DOUBLING_CAPACITY)
          cap <<= 1;
        else
          cap += CAPACITY_INCREMENT;
        events = ByteBuffer.allocateDirect(cap);
      }
    // Ensure that the events buffer is not too large, given the number of
    // events registered.
    else if (events.capacity() > keys.size() * sizeof_struct_epoll_event * 2 + 1
             && events.capacity() > INITIAL_CAPACITY)
      {
        int cap = events.capacity() >>> 1;
        events = ByteBuffer.allocateDirect(cap);
      }
  }

  void epoll_modify(EpollSelectionKeyImpl key, int ops) throws IOException
  {
    epoll_modify(epoll_fd, key.fd, ops);
  }

  /**
   * Tell if epoll is supported by this system, and support was compiled in.
   *
   * @return True if this system supports event notification with epoll.
   */
  public static native boolean epoll_supported();


  /**
   * Returns the size of `struct epoll_event'.
   *
   * @return The size of `struct epoll_event'.
   */
  private static native int sizeof_struct();


  /**
   * Open a new epoll file descriptor.
   *
   * @param size The size hint for the new epoll descriptor.
   * @return The new file descriptor integer.
   * @throws IOException If allocating a new epoll descriptor fails.
   */
  private static native int epoll_create(int size) throws IOException;

  /**
   * Add a file descriptor to this selector.
   *
   * @param efd The epoll file descriptor.
   * @param fd  The file descriptor to add (or modify).
   * @param ops The interest opts.
   */
  private static native void epoll_add(int efd, int fd, int ops)
    throws IOException;

  /**
   * Modify the interest ops of the key selecting for the given FD.
   *
   * @param efd The epoll file descriptor.
   * @param fd  The file descriptor to modify.
   * @param ops The ops.
   * @throws IOException
   */
  private static native void epoll_modify(int efd, int fd, int ops)
    throws IOException;

  /**
   * Remove a file descriptor from this selector.
   *
   * @param efd The epoll file descriptor.
   * @param fd  The file descriptor.
   * @throws IOException
   */
  private static native void epoll_delete(int efd, int fd) throws IOException;

  /**
   * Select events.
   *
   * @param efd     The epoll file descriptor.
   * @param state   The buffer to hold selected events.
   * @param n       The number of events that may be put in `state'.
   * @param timeout The timeout.
   * @return The number of events selected.
   * @throws IOException
   */
  private static native int epoll_wait(int efd, ByteBuffer state, int n, int timeout)
    throws IOException;

  /**
   * Fetch the fd value from a selected struct epoll_event.
   *
   * @param struct The direct buffer holding the struct.
   * @return The fd value.
   */
  private static native int selected_fd(ByteBuffer struct);

  /**
   * Fetch the enabled operations from a selected struct epoll_event.
   *
   * @param struct The direct buffer holding the struct.
   * @return The selected operations.
   */
  private static native int selected_ops(ByteBuffer struct);
}

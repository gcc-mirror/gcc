/* SelectorProviderImpl.java --
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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


import gnu.classpath.SystemProperties;

import java.io.IOException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.Pipe;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.AbstractSelector;
import java.nio.channels.spi.SelectorProvider;

public class SelectorProviderImpl extends SelectorProvider
{
  private static final String SELECTOR_IMPL_KQUEUE = "kqueue";
  private static final String SELECTOR_IMPL_EPOLL = "epoll";
  private static final String SELECTOR_IMPL = "gnu.java.nio.selectorImpl";
  private static boolean epoll_failed = false;

  public SelectorProviderImpl ()
  {
  }

  public DatagramChannel openDatagramChannel ()
    throws IOException
  {
    return new DatagramChannelImpl (this);
  }

  public Pipe openPipe ()
    throws IOException
  {
    return new PipeImpl (this);
  }

  public AbstractSelector openSelector ()
    throws IOException
  {
    String selectorImpl = "default";
    if (KqueueSelectorImpl.kqueue_supported())
      selectorImpl = SELECTOR_IMPL_KQUEUE;
    if (EpollSelectorImpl.epoll_supported() && !epoll_failed)
      selectorImpl = SELECTOR_IMPL_EPOLL;
    selectorImpl = SystemProperties.getProperty(SELECTOR_IMPL, selectorImpl);

    if (selectorImpl.equals(SELECTOR_IMPL_KQUEUE))
      return new KqueueSelectorImpl(this);

    if (selectorImpl.equals(SELECTOR_IMPL_EPOLL))
      {
        // We jump through these hoops because even though epoll may look
        // like it's available (sys/epoll.h exists, and you can link against
        // all the epoll functions) it may not be available in the kernel
        // (especially 2.4 kernels), meaning you will get ENOSYS at run time.
        //
        // Madness!
        try
          {
            return new EpollSelectorImpl(this);
          }
        catch (InternalError e)
          {
            // epoll_create throws this on ENOSYS.
            epoll_failed = true;
          }
      }

    return new SelectorImpl (this);
  }

  public ServerSocketChannel openServerSocketChannel ()
    throws IOException
  {
    return new ServerSocketChannelImpl (this);
  }

  public SocketChannel openSocketChannel ()
    throws IOException
  {
    return new SocketChannelImpl (this);
  }

}

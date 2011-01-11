/* AbstractInterruptibleChannel.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

package java.nio.channels.spi;

import java.io.IOException;
import java.nio.channels.AsynchronousCloseException;
import java.nio.channels.Channel;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.InterruptibleChannel;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class AbstractInterruptibleChannel
  implements Channel, InterruptibleChannel
{
  private boolean closed;

  /**
   * Initializes the channel.
   */
  protected AbstractInterruptibleChannel()
  {
  }

  /**
   * Marks the beginning of an I/O operation that might block indefinitely.
   */
  protected final void begin()
  {
  }

  /**
   * Closes the channel.
   *
   * @exception IOException If an error occurs
   */
  public final void close() throws IOException
  {
    if (! closed)
      {
        closed = true;
        implCloseChannel();
      }
  }

  /**
   * Marks the end of an I/O operation that might block indefinitely.
   *
   * @param completed true if the task completed successfully,
   * false otherwise
   *
   * @exception AsynchronousCloseException If the channel was asynchronously
   * closed.
   * @exception ClosedByInterruptException If the thread blocked in the
   * I/O operation was interrupted.
   */
  protected final void end(boolean completed)
    throws AsynchronousCloseException
  {
    // FIXME: check more here.

    if (closed) throw new AsynchronousCloseException();
  }

  /**
   * Closes the channel.
   *
   * @exception IOException If an error occurs
   */
  protected abstract void implCloseChannel() throws IOException;

  /**
   * Tells whether or not this channel is open.
   *
   * @return true if the channel is open, false otherwise
   */
  public final boolean isOpen()
  {
    return ! closed;
  }
}

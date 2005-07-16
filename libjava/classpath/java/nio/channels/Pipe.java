/* Pipe.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.channels;

import java.io.IOException;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.channels.spi.SelectorProvider;


/**
 * @author Michael Koch
 * @since 1.4
 */
public abstract class Pipe
{
  public abstract static class SinkChannel extends AbstractSelectableChannel
    implements WritableByteChannel, GatheringByteChannel
  {
    /**
     * Initializes the channel.
     */
    protected SinkChannel(SelectorProvider provider)
    {
      super(provider);
    }

    /**
     * Returns an operation set that is valid on this channel.
     *
     * The only valid operation on this channel is @see SelectionKey.OP_WRITE.
     */
    public final int validOps()
    {
      return SelectionKey.OP_WRITE;
    }
  }

  public abstract static class SourceChannel extends AbstractSelectableChannel
    implements ReadableByteChannel, ScatteringByteChannel
  {
    /**
     * Initializes the channel.
     */
    protected SourceChannel(SelectorProvider provider)
    {
      super(provider);
    }

    /**
     * Returns an operation set that is valid on this channel.
     *
     * The only valid operation on this channel is @see SelectionKey.OP_READ.
     */
    public final int validOps()
    {
      return SelectionKey.OP_READ;
    }
  }

  /**
   * Initializes the pipe.
   */
  protected Pipe()
  {
  }

  /**
   * Opens a pipe.
   *
   * @exception IOException If an error occurs
   */
  public static Pipe open() throws IOException
  {
    return SelectorProvider.provider().openPipe();
  }

  /**
   * Returns a pipe's sink channel.
   */
  public abstract Pipe.SinkChannel sink();

  /**
   * Returns a pipe's source channel
   */
  public abstract Pipe.SourceChannel source();
}

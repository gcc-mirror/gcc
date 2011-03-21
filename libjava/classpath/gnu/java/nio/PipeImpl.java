/* PipeImpl.java --
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Pipe;
import java.nio.channels.spi.SelectorProvider;

class PipeImpl extends Pipe
{
  public static final class SourceChannelImpl extends Pipe.SourceChannel
    implements VMChannelOwner
  {
    private VMChannel vmch;

    public SourceChannelImpl (SelectorProvider selectorProvider,
                              VMChannel channel)
    {
      super (selectorProvider);
      vmch = channel;
    }

    protected final void implCloseSelectableChannel()
      throws IOException
    {
      vmch.close();
    }

    protected void implConfigureBlocking (boolean blocking)
      throws IOException
    {
      vmch.setBlocking(blocking);
    }

    public final int read (ByteBuffer src)
      throws IOException
    {
      return vmch.read(src);
    }

    public final long read (ByteBuffer[] srcs)
      throws IOException
    {
      return vmch.readScattering(srcs, 0, srcs.length);
    }

    public final synchronized long read (ByteBuffer[] srcs, int offset,
                                         int len)
      throws IOException
    {
      if (offset < 0
          || offset > srcs.length
          || len < 0
          || len > srcs.length - offset)
        throw new IndexOutOfBoundsException();

      return vmch.readScattering(srcs, offset, len);
    }

    public VMChannel getVMChannel()
    {
      return vmch;
    }
  }

  public static final class SinkChannelImpl extends Pipe.SinkChannel
    implements VMChannelOwner
  {
    private VMChannel vmch;

    public SinkChannelImpl (SelectorProvider selectorProvider,
                            VMChannel channel)
    {
      super (selectorProvider);
      vmch = channel;
    }

    protected final void implCloseSelectableChannel()
      throws IOException
    {
      vmch.close();
    }

    protected final void implConfigureBlocking (boolean blocking)
      throws IOException
    {
      vmch.setBlocking(blocking);
    }

    public final int write (ByteBuffer dst)
      throws IOException
    {
      return vmch.write(dst);
    }

    public final long write (ByteBuffer[] srcs)
      throws IOException
    {
      return vmch.writeGathering(srcs, 0, srcs.length);
    }

    public final synchronized long write (ByteBuffer[] srcs, int offset, int len)
      throws IOException
    {
      if (offset < 0
          || offset > srcs.length
          || len < 0
          || len > srcs.length - offset)
        throw new IndexOutOfBoundsException();

      return vmch.writeGathering(srcs, offset, len);
    }

    public VMChannel getVMChannel()
    {
      return vmch;
    }
  }

  private SinkChannelImpl sink;
  private SourceChannelImpl source;

  public PipeImpl (SelectorProvider provider)
    throws IOException
  {
    super();
    VMChannel[] pipe = VMPipe.pipe();
    sink = new SinkChannelImpl(provider, pipe[0]);
    source = new SourceChannelImpl(provider, pipe[1]);
  }

  public Pipe.SinkChannel sink()
  {
    return sink;
  }

  public Pipe.SourceChannel source()
  {
    return source;
  }
}

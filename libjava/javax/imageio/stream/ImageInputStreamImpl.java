/* ImageInputStream.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio.stream;

import java.io.IOException;
import java.nio.ByteOrder;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class ImageInputStreamImpl implements ImageInputStream
{
  private boolean closed;
  
  protected int bitOffset;
  protected ByteOrder byteOrder;
  protected long flushedPos;
  protected long streamPos;

  public ImageInputStreamImpl()
  {
    // Do nothing here.
  }

  protected final void checkClosed()
    throws IOException
  {
    if (closed)
      throw new IOException("stream closed");
  }

  public void close()
    throws IOException
  {
    checkClosed();
    closed = true;
  }
  
  protected void finalize()
    throws Throwable
  {
    close();
  }

  public void flush()
    throws IOException
  {
    flushBefore(getStreamPosition());
  }

  public void flushBefore(long position)
    throws IOException
  {
    if (position < flushedPos)
      throw new IndexOutOfBoundsException();

    if (position > streamPos)
      throw new IndexOutOfBoundsException();

    flushedPos = position;
  }

  public int getBitOffset()
    throws IOException
  {
    return bitOffset;
  }

  public ByteOrder getByteOrder()
  {
    return byteOrder;
  }

  public long getFlushedPosition()
  {
    return flushedPos;
  }

  public long getStreamPosition()
    throws IOException
  {
    return streamPos;
  }

  public boolean isCached()
  {
    return false;
  }

  public boolean isCachedFile()
  {
    return false;
  }

  public boolean isCachedMemory()
  {
    return false;
  }

  public long length()
  {
    return -1L;
  }

  public abstract int read()
    throws IOException;

  public int read(byte[] data)
    throws IOException
  {
    return read(data, 0, data.length);
  }

  public abstract int read(byte[] data, int offset, int len)
    throws IOException;

  public void setByteOrder (ByteOrder byteOrder)
  {
    this.byteOrder = byteOrder;
  }
}

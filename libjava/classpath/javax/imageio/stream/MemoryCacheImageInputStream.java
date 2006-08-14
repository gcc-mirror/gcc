/* MemoryCacheImageInputStream.java --
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


package javax.imageio.stream;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class MemoryCacheImageInputStream extends ImageInputStreamImpl
{
  private InputStream stream;
  private BufferedInputStream buffer;

  private int READLIMIT = 2048;
  
  public MemoryCacheImageInputStream(InputStream stream)
  {
    this.stream = stream;
    buffer = new BufferedInputStream(stream);
    buffer.mark(READLIMIT);
  }

  public void close()
    throws IOException
  {
    super.close();
    stream.close();
  }

  public void flushBefore(long position)
    throws IOException
  {
    long prevFlushedPosition = getFlushedPosition();
    super.flushBefore(position);
    buffer.reset();
    buffer.skip(getFlushedPosition() - prevFlushedPosition);
    buffer.mark(READLIMIT);
  }

  public boolean isCached()
  {
    return true;
  }

  public boolean isCachedFile()
  {
    return false;
  }
  
  public boolean isCachedMemory()
  {
    return true;
  }

  public int read()
    throws IOException
  {
    setBitOffset(0);
    int retval = buffer.read();
    
    if (retval != -1)
      streamPos++;

    return retval;
  }

  public int read(byte[] data, int offset, int len)
    throws IOException
  {
    setBitOffset(0);
    int retval = buffer.read(data, offset, len);

    if (retval != -1)
      {
        streamPos += retval;
      }

    return retval; 
  }
  
  public void seek(long position)
  throws IOException
  {
    super.seek(position);
    buffer.reset();
    buffer.skip(position - getFlushedPosition());
  }
}

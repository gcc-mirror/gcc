/* FileCacheImageOutputStream.java --
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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class FileCacheImageOutputStream extends ImageOutputStreamImpl
{
  private OutputStream stream;
  private File cacheDir;
  
  public FileCacheImageOutputStream(OutputStream stream, File cacheDir)
    throws IOException
  {
    super();
    this.stream = stream;
    // FIXME: We do not support caching yet.
    this.cacheDir = cacheDir;
  }

  public void close()
    throws IOException
  {
    if (stream != null)
      {
	stream.close();
	stream = null;
      }
  }

  private void checkStreamClosed()
    throws IOException
  {
    if (stream == null)
      throw new IOException("stream closed");
  }

  public boolean isCached()
  {
    return true;
  }

  public boolean isCachedFile()
  {
    return true;
  }
  
  public boolean isCachedMemory()
  {
    return false;
  }
  
  public int read()
    throws IOException
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
  }

  public int read(byte[] data, int offset, int len)
    throws IOException
  {
    // FIXME: Implement me.
    throw new Error("not implemented");
  }

  public void write(byte[] data, int offset, int len)
    throws IOException
  {
    checkStreamClosed();
    // FIXME: Flush pending bits.
    stream.write(data, offset, len);
  }

  public void write(int value)
    throws IOException
  {
    checkStreamClosed();
    // FIXME: Flush pending bits.
    stream.write(value);
  }
}

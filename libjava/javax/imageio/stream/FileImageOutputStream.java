/* FileImageOutputStream.java --
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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class FileImageOutputStream extends ImageOutputStreamImpl
{
  private RandomAccessFile file;
  
  public FileImageOutputStream(File file)
    throws FileNotFoundException, IOException
  {
    if (file == null)
      throw new IllegalArgumentException("file may not be null");

    // Do security check.
    file.canRead();

    this.file = new RandomAccessFile(file, "r");
  }

  public FileImageOutputStream(RandomAccessFile file)
  {
    if (file == null)
      throw new IllegalArgumentException("file may not be null");

    this.file = file;
  }

  public void close()
    throws IOException
  {
    file.close();
  }

  public long length()
  {
    try
      {
        return file.length();
      }
    catch (IOException e)
      {
        return -1L;
      }
  }

  public int read()
    throws IOException
  {
    checkClosed();
    
    setBitOffset(0);
    return file.read();
  }

  public int read(byte[] data, int offset, int len)
    throws IOException
  {
    checkClosed();
    
    setBitOffset(0);
    return file.read(data, offset, len);
  }

  public void seek(long position)
    throws IOException
  {
    super.seek(position);
    file.seek(position);
  }

  public void write(byte[] data, int offset, int len)
    throws IOException
  {
    checkClosed();
    
    flushBits();
    file.write(data, offset, len);
  }

  public void write(int value)
    throws IOException
  {
    checkClosed();
    
    // FIXME: Flush pending bits.
    file.write(value);
  }
}

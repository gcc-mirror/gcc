// Handler.java - URLStreamHandler for core protocol.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.protocol.core;

import java.io.*;
import gnu.gcj.Core;
import gnu.gcj.RawData;

public class CoreInputStream extends InputStream
{
  /* A pointer to the object in memory.  */
  protected RawData ptr;

  /* Position of the next byte in core to be read. */
  protected int pos;

  /* The currently marked position in the stream. */
  protected int mark;

  /* The index in core one greater than the last valid character. */
  protected int count;

  private native int unsafeGetByte (long offset);
  private native int copyIntoByteArray (byte[] dest, int offset, int numBytes);

  public CoreInputStream (Core core)
  {
    ptr = core.ptr;
    count = core.length;
  }

  public synchronized int available()
  {
    return count - pos;
  }

  public synchronized void mark(int readAheadLimit)
  {
    // readAheadLimit is ignored per Java Class Lib. book, p.220.
    mark = pos;
  }

  public boolean markSupported()
  {
    return true;
  }

  public synchronized int read()
  {
    if (pos < count)
      return ((int) unsafeGetByte(pos++)) & 0xFF;
    return -1;
  }

  public synchronized int read(byte[] b, int off, int len)
  {
    if (pos >= count)
      return -1;

    int numBytes = Math.min(count - pos, len);
    copyIntoByteArray (b, off, numBytes);
    pos += numBytes;
    return numBytes;
  }

  public synchronized void reset()
  {
    pos = mark;
  }

  public synchronized long skip(long n)
  {
    // Even though the var numBytes is a long, in reality it can never
    // be larger than an int since the result of subtracting 2 positive
    // ints will always fit in an int.  Since we have to return a long
    // anyway, numBytes might as well just be a long.
    long numBytes = Math.min((long) (count - pos), n < 0 ? 0L : n);
    pos += numBytes;
    return numBytes;
  }
}

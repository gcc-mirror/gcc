/* CRLFInputStream.java --
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package gnu.java.net;

import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;

/**
 * An input stream that filters out CR/LF pairs into LFs.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class CRLFInputStream
  extends FilterInputStream
{
  /**
   * The CR octet.
   */
  public static final int CR = 13;

  /**
   * The LF octet.
   */
  public static final int LF = 10;

  /**
   * Buffer.
   */
  protected int buf = -1;

  /**
   * Buffer at time of mark.
   */
  protected int markBuf = -1;

  /**
   * Constructs a CR/LF input stream connected to the specified input
   * stream.
   */
  public CRLFInputStream(InputStream in)
  {
    super(in);
  }

  /**
   * Reads the next byte of data from this input stream.
   * Returns -1 if the end of the stream has been reached.
   * @exception IOException if an I/O error occurs
   */
  public int read()
    throws IOException
  {
    int c;
    if (buf != -1)
      {
        c = buf;
        buf = -1;
        return c;
      }
    else
      {
        c = super.read();
        if (c == CR)
          {
            buf = super.read();
            if (buf == LF)
              {
                c = buf;
                buf = -1;
              }
          }
      }
    return c;
  }
  
  /**
   * Reads up to b.length bytes of data from this input stream into
   * an array of bytes.
   * Returns -1 if the end of the stream has been reached.
   * @exception IOException if an I/O error occurs
   */
  public int read(byte[] b)
    throws IOException
  {
    return read(b, 0, b.length);
  }

  /**
   * Reads up to len bytes of data from this input stream into an
   * array of bytes, starting at the specified offset.
   * Returns -1 if the end of the stream has been reached.
   * @exception IOException if an I/O error occurs
   */
  public int read(byte[] b, int off, int len)
    throws IOException
  {
    int shift = 0;
    if (buf != -1)
      {
        // Push buf onto start of byte array
        b[off] = (byte) buf;
        off++;
        len--;
        buf = -1;
        shift++;
      }
    int l = super.read(b, off, len);
    l = removeCRLF(b, off - shift, l);
    return l;
  }

  /**
   * Indicates whether this stream supports the mark and reset methods.
   */
  public boolean markSupported()
  {
    return in.markSupported();
  }

  /**
   * Marks the current position in this stream.
   */
  public void mark(int readlimit)
  {
    in.mark(readlimit);
    markBuf = buf;
  }

  /**
   * Repositions this stream to the position at the time the mark method was
   * called.
   */
  public void reset()
    throws IOException
  {
    in.reset();
    buf = markBuf;
  }

  private int removeCRLF(byte[] b, int off, int len)
  {
    int end = off + len;
    for (int i = off; i < end; i++)
      {
        if (b[i] == CR)
          {
            if (i + 1 == end)
              {
                // This is the last byte, impossible to determine whether CRLF
                buf = CR;
                len--;
              }
            else if (b[i + 1] == LF)
              {
                // Shift left
                end--;
                for (int j = i; j < end; j++)
                  {
                    b[j] = b[j + 1];
                  }
                  len--;
                  end = off + len;
              }
          }
      }
    return len;
  }
}

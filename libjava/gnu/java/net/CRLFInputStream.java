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

import java.io.BufferedInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

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

  private boolean doReset;

  /**
   * Constructs a CR/LF input stream connected to the specified input
   * stream.
   */
  public CRLFInputStream(InputStream in)
  {
    super(in.markSupported() ? in : new BufferedInputStream(in));
  }

  /**
   * Reads the next byte of data from this input stream.
   * Returns -1 if the end of the stream has been reached.
   * @exception IOException if an I/O error occurs
   */
  public int read()
    throws IOException
  {
    int c = in.read();
    if (c == CR)
      {
        in.mark(1);
        int d = in.read();
        if (d == LF)
          {
            c = d;
          }
        else
          {
            in.reset();
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
    in.mark(len + 1);
    int l = in.read(b, off, len);
    if (l > 0)
      {
        int i = indexOfCRLF(b, off, l);
        if (doReset)
          {
            in.reset();
            if (i != -1)
              {
                l = in.read(b, off, i + 1); // read to CR
                in.read(); // skip LF
                b[i] = LF; // fix CR as LF
              }
            else
              {
                l = in.read(b, off, len); // CR(s) but no LF
              }
          }
      }
    return l;
  }

  private int indexOfCRLF(byte[] b, int off, int len)
    throws IOException
  {
    doReset = false;
    int lm1 = len - 1;
    for (int i = off; i < len; i++)
      {
        if (b[i] == CR)
          {
            int d;
            if (i == lm1)
              {
                d = in.read();
                doReset = true;
              }
            else
              {
                d = b[i + 1];
              }
            if (d == LF)
              {
                doReset = true;
                return i;
              }
          }
      }
    return -1;
  }

}


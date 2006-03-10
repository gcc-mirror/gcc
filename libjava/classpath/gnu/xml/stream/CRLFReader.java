/* CRLFReader.java -- 
   Copyright (C) 2005,2006  Free Software Foundation, Inc.

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

package gnu.xml.stream;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

/**
 * Filtered reader that normalizes CRLF pairs into LFs.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class CRLFReader
  extends Reader
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

  protected Reader in;

  /**
   * Constructor.
   */
  protected CRLFReader(Reader in)
  {
    if (!in.markSupported())
      in = new BufferedReader(in);
    this.in = in;
  }

  public int read()
    throws IOException
  {
    int c = in.read();
    if (c == 13) // CR
      {
        in.mark(1);
        int d = in.read();
        if (d == 10) // LF
          c = d;
        else
          in.reset();
      }
    return c;
  }

  public int read(char[] b)
    throws IOException
  {
    return read(b, 0, b.length);
  }

  public int read(char[] b, int off, int len)
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
                l = in.read(b, off, (i + 1) - off); // read to CR
                in.read(); // skip LF
                b[i] = '\n'; // fix CR as LF
              }
            else
              l = in.read(b, off, len); // CR(s) but no LF
          } 
      }
    return l;
  }

  public boolean markSupported()
  {
    return in.markSupported();
  }

  public void mark(int limit)
    throws IOException
  {
    in.mark(limit);
  }

  public void reset()
    throws IOException
  {
    in.reset();
  }

  public long skip(long n)
    throws IOException
  {
    return in.skip(n);
  }

  public void close()
    throws IOException
  {
    in.close();
  }

  private int indexOfCRLF(char[] b, int off, int len)
    throws IOException
  {
    doReset = false;
    int end = off + len;
    int em1 = end - 1;
    for (int i = off; i < end; i++)
      {
        if (b[i] == '\r') // CR
          {
            int d;
            if (i == em1)
              {
                d = in.read();
                doReset = true;
              }
            else
              d = b[i + 1];
            if (d == '\n') // LF
              {
                doReset = true;
                return i;
              }
          }
      }
    return -1;
  }

}


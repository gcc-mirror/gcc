/* ReaderInputStream.java --
   Copyright (C) 1999, 2000, 2001, 2004 Free Software Foundation, Inc.

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

package gnu.xml.dom.ls;

import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;

/**
 * Character stream wrapper.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 * @author <a href='mailto:mark@klomp.org'>Mark Wielaard</a>
 */
public class ReaderInputStream
  extends InputStream
{

  private Reader reader;
  private String encoding;

  // Holds extra spillover data if necessary
  private byte extra[];
  private int pos;

  private byte extra_marked[];
  private int pos_marked;

  public ReaderInputStream(Reader reader)
  {
    this.reader = reader;
    this.encoding = "UTF-8";
  }

  void setEncoding(String encoding)
  {
    this.encoding = encoding;
  }

  public int read()
    throws IOException
  {
    if (extra != null)
      {
        int result = extra[pos];
        pos++;
        if (pos >= extra.length)
          {
            extra = null;
          }
        return result;
      }
    return reader.read();
  }

  public int read(byte[] b)
    throws IOException
  {
    return read(b, 0, b.length);
  }

  public int read(byte[] b, int off, int len)
    throws IOException
  {
    if (len == 0)
      {
        return 0;
      }

    if (extra != null)
      {
        int available = extra.length - pos;
        int l = available < len ? available : len;
        System.arraycopy(extra, 0, b, off, l);
        pos += l;
        if (pos >= extra.length)
          {
            extra = null;
          }
        return l;
      }

    char[] c = new char[len];
    int l = reader.read(c, 0, len);
    if (l == -1)
      {
        return -1;
      }

    String s = new String(c, 0, l);
    byte[] d = s.getBytes(encoding);

    int available = d.length;
    int more = d.length - len;
    if (more > 0)
      {
        extra = new byte[more];
        pos = 0;
        System.arraycopy(d, len, extra, 0, more);
        available -= more;
      }

    System.arraycopy(d, 0, b, off, available);
    return available;
  }

  public void close()
    throws IOException
  {
    reader.close();
  }

  public boolean markSupported()
  {
    return reader.markSupported();
  }

  public void mark(int limit)
  {
    if (extra != null)
      {
        extra_marked = new byte[extra.length];
        System.arraycopy(extra, 0, extra_marked, 0, extra.length);
        pos_marked = pos;
      }
    else
      {
        extra_marked = null;
      }

    try
      {
        // Note that this might be a bit more than asked for.
        // Because we might also have the extra_marked bytes.
        // That is fine (and necessary for reset() to work).
        reader.mark(limit);
      }
    catch (IOException ioe)
      {
        throw new RuntimeException(ioe);
      }
  }

  public void reset()
    throws IOException
  {
    extra = extra_marked;
    pos = pos_marked;
    extra_marked = null;

    reader.reset();
  }

  public long skip(long n)
    throws IOException
  {
    long done = 0;
    if (extra != null)
      {
        int available = extra.length - pos;
        done = available < n ? available : n;
        pos += done;
        if (pos >= extra.length)
          {
            extra = null;
          }
      }

    n -= done;
    if (n > 0)
      {
        return reader.skip(n) + done;
      }
    else
      {
        return done;
      }
  }

  /**
   *  Returns conservative number of bytes available without blocking.
   *  Actual number of bytes that can be read without blocking might
   *  be (much) bigger.
   */
  public int available()
    throws IOException
  {
    if (extra != null)
      {
        return pos - extra.length;
      }

    return reader.ready() ? 1 : 0;
  }

  public String toString()
  {
    return getClass().getName() + "[" + reader + ", " + encoding + "]";
  }

}

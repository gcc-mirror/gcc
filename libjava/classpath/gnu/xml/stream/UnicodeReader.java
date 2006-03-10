/* UnicodeReader.java -- 
   Copyright (C) 2005  Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.Reader;

/**
 * A reader that converts UTF-16 characters to Unicode code points.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class UnicodeReader
{

  final Reader in;

  UnicodeReader(Reader in)
  {
    this.in = in;
  }

  public void mark(int limit)
    throws IOException
  {
    in.mark(limit * 2);
  }

  public void reset()
    throws IOException
  {
    in.reset();
  }

  public int read()
    throws IOException
  {
    int ret = in.read();
    if (ret == -1)
      return ret;
    if (ret >= 0xd800 && ret < 0xdc00)
      {
        // Unicode surrogate?
        int low = in.read();
        if (low >= 0xdc00 && low < 0xe000)
          ret = Character.toCodePoint((char) ret, (char) low);
        else
          throw new IOException("unpaired surrogate: U+" +
                                Integer.toHexString(ret));
      }
    else if (ret >= 0xdc00 && ret < 0xe000)
      throw new IOException("unpaired surrogate: U+" +
                            Integer.toHexString(ret));
    return ret;
  }

  public int read(int[] buf, int off, int len)
    throws IOException
  {
    if (len == 0)
      return 0;
    char[] b2 = new char[len];
    int ret = in.read(b2, 0, len);
    if (ret <= 0)
      return ret;
    int l = ret - 1;
    int i = 0, j = off;
    for (; i < l; i++)
      {
        char c = b2[i];
        if (c >= 0xd800 && c < 0xdc00)
          {
            // Unicode surrogate?
            char d = b2[i + 1];
            if (d >= 0xdc00 && d < 0xe000)
              {
                buf[j++] = Character.toCodePoint(c, d);
                i++;
                continue;
              }
            else
              throw new IOException("unpaired surrogate: U+" +
                                    Integer.toHexString(c));
          }
        else if (c >= 0xdc00 && c < 0xe000)
          throw new IOException("unpaired surrogate: U+" +
                                Integer.toHexString(c));
        buf[j++] = (int) c;
      }
    if (i == l)
      {
        // last char
        char c = b2[l];
        if (c >= 0xd800 && c < 0xdc00)
          {
            int low = in.read();
            if (low >= 0xdc00 && low < 0xe000)
              {
                buf[j++] = Character.toCodePoint(c, (char) low);
                return j;
              }
            else
              throw new IOException("unpaired surrogate: U+" +
                                    Integer.toHexString(c));
          }
        else if (c >= 0xdc00 && c < 0xe000)
          throw new IOException("unpaired surrogate: U+" +
                                Integer.toHexString(c));
        buf[j++] = (int) c;
      }
    return j;
  }

  public void close()
    throws IOException
  {
    in.close();
  }

  /**
   * Returns the specified UTF-16 char array as an array of Unicode code
   * points.
   */
  public static int[] toCodePointArray(String text)
    throws IOException
  {
    char[] b2 = text.toCharArray();
    int[] buf = new int[b2.length];
    if (b2.length > 0)
      {
        int l = b2.length - 1;
        int i = 0, j = 0;
        for (; i < l; i++)
          {
            char c = b2[i];
            if (c >= 0xd800 && c < 0xdc00)
              {
                // Unicode surrogate?
                char d = b2[i + 1];
                if (d >= 0xdc00 && d < 0xe000)
                  {
                    buf[j++] = Character.toCodePoint(c, d);
                    i++;
                    continue;
                  }
                else
                  throw new IOException("unpaired surrogate: U+" +
                                        Integer.toHexString(c));
              }
            else if (c >= 0xdc00 && c < 0xe000)
              throw new IOException("unpaired surrogate: U+" +
                                    Integer.toHexString(c));
            buf[j++] = (int) c;
          }
        if (i == l)
          {
            // last char
            buf[j++] = (int) b2[l];
            if (j < buf.length)
              {
                int[] buf2 = new int[j];
                System.arraycopy(buf, 0, buf2, 0, j);
                buf = buf2;
              }
          }
      }
    return buf;
  }
  
}

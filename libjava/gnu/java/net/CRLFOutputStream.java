/* CRLFOutputStream.java --
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


package gnu.java.net;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

/** 
 * An output stream that filters LFs into CR/LF pairs.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
public class CRLFOutputStream
  extends FilterOutputStream
{
  static final String US_ASCII = "US-ASCII";

  /**
   * The CR octet.
   */
  public static final int CR = 13;

  /**
   * The LF octet.
   */
  public static final int LF = 10;

  /**
   * The CR/LF pair.
   */
  public static final byte[] CRLF = { CR, LF };

  /**
   * The last byte read.
   */
  protected int last;

  /**
   * Constructs a CR/LF output stream connected to the specified output stream.
   */
  public CRLFOutputStream(OutputStream out)
  {
    super(out);
    last = -1;
  }

  /**
   * Writes a character to the underlying stream.
   * @exception IOException if an I/O error occurred
   */
  public void write(int ch) throws IOException
  {
    if (ch == CR)
      {
        out.write(CRLF);
      }
    else if (ch == LF)
      {
        if (last != CR)
          {
            out.write(CRLF);
          }
      }
    else
      {
        out.write(ch);
      }
    last = ch;
  }
  
  /**
   * Writes a byte array to the underlying stream.
   * @exception IOException if an I/O error occurred
   */
  public void write(byte[] b)
    throws IOException
  {
    write(b, 0, b.length);
  }

  /**
   * Writes a portion of a byte array to the underlying stream.
   * @exception IOException if an I/O error occurred
   */
  public void write(byte[] b, int off, int len)
    throws IOException
  {
    int d = off;
    len += off;
    for (int i = off; i < len; i++)
      {
        switch (b[i])
          {
          case CR:
            out.write (b, d, i - d);
            out.write (CRLF, 0, 2);
            d = i + 1;
            break;
          case LF:
            if (last != CR)
              {
                out.write (b, d, i - d);
                out.write (CRLF, 0, 2);
              }
            d = i + 1;
            break;
          }
        last = b[i];
      }
    if (len - d > 0)
      {
        out.write (b, d, len - d);
      }
  }
  
  /**
   * Writes the specified ASCII string to the underlying stream.
   * @exception IOException if an I/O error occurred
   */
  public void write(String text)
    throws IOException
  {
    try
      {
        byte[] bytes = text.getBytes(US_ASCII);
        write(bytes, 0, bytes.length);
        }
    catch (UnsupportedEncodingException e)
      {
        throw new IOException("The US-ASCII encoding is not supported " +
                              "on this system");
      }
  }

  /**
   * Writes a newline to the underlying stream.
   * @exception IOException if an I/O error occurred
   */
  public void writeln()
    throws IOException
  {
    out.write(CRLF, 0, 2);
  }
}


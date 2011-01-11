/* CipherOutputStream.java -- Filters output through a cipher.
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


package javax.crypto;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * A filtered output stream that transforms data written to it with a
 * {@link Cipher} before sending it to the underlying output stream.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class CipherOutputStream extends FilterOutputStream
{
  /** The underlying cipher. */
  private Cipher cipher;

  /**
   * Create a new cipher output stream. The cipher argument must have already
   * been initialized.
   *
   * @param out The sink for transformed data.
   * @param cipher The cipher to transform data with.
   */
  public CipherOutputStream(OutputStream out, Cipher cipher)
  {
    super(out);
    this.cipher = (cipher != null) ? cipher : new NullCipher();
  }

  /**
   * Create a cipher output stream with no cipher.
   *
   * @param out The sink for transformed data.
   */
  protected CipherOutputStream(OutputStream out)
  {
    super(out);
  }

  /**
   * Close this output stream, and the sink output stream.
   * <p>
   * This method will first invoke the {@link Cipher#doFinal()} method of the
   * underlying {@link Cipher}, and writes the output of that method to the
   * sink output stream.
   *
   * @throws IOException If an I/O error occurs, or if an error is caused by
   *           finalizing the transformation.
   */
  public void close() throws IOException
  {
    try
      {
        out.write(cipher.doFinal());
        out.flush();
        out.close();
      }
    catch (Exception cause)
      {
        IOException ioex = new IOException(String.valueOf(cause));
        ioex.initCause(cause);
        throw ioex;
      }
  }

  /**
   * Flush any pending output.
   *
   * @throws IOException If an I/O error occurs.
   */
  public void flush() throws IOException
  {
    out.flush();
  }

  /**
   * Write a single byte to the output stream.
   *
   * @param b The next byte.
   * @throws IOException If an I/O error occurs, or if the underlying cipher is
   *           not in the correct state to transform data.
   */
  public void write(int b) throws IOException
  {
    write(new byte[] { (byte) b }, 0, 1);
  }

  /**
   * Write a byte array to the output stream.
   *
   * @param buf The next bytes.
   * @throws IOException If an I/O error occurs, or if the underlying cipher is
   *           not in the correct state to transform data.
   */
  public void write(byte[] buf) throws IOException
  {
    write(buf, 0, buf.length);
  }

  /**
   * Write a portion of a byte array to the output stream.
   *
   * @param buf The next bytes.
   * @param off The offset in the byte array to start.
   * @param len The number of bytes to write.
   * @throws IOException If an I/O error occurs, or if the underlying cipher is
   *           not in the correct state to transform data.
   */
  public void write(byte[] buf, int off, int len) throws IOException
  {
    byte[] b = cipher.update(buf, off, len);
    if (b != null)
      out.write(b);
  }
}

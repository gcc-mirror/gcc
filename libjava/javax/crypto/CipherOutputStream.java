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

  // Fields.
  // ------------------------------------------------------------------------

  /** The underlying cipher. */
  private Cipher cipher;

  private byte[][] inBuffer;

  private int inLength;

  private byte[] outBuffer;

  private static final int FIRST_TIME  = 0;
  private static final int SECOND_TIME = 1;
  private static final int SEASONED    = 2;
  private int state;

  /** True if the cipher is a stream cipher (blockSize == 1) */
  private boolean isStream;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new cipher output stream. The cipher argument must have
   * already been initialized.
   *
   * @param out    The sink for transformed data.
   * @param cipher The cipher to transform data with.
   */
  public CipherOutputStream(OutputStream out, Cipher cipher)
  {
    super(out);
    if (cipher != null)
      {
        this.cipher = cipher;
        if (!(isStream = cipher.getBlockSize() == 1))
          {
            inBuffer = new byte[2][];
            inBuffer[0] = new byte[cipher.getBlockSize()];
            inBuffer[1] = new byte[cipher.getBlockSize()];
            inLength = 0;
            state = FIRST_TIME;
          }
      }
    else
      this.cipher = new NullCipher();
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

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Close this output stream, and the sink output stream.
   *
   * <p>This method will first invoke the {@link Cipher#doFinal()}
   * method of the underlying {@link Cipher}, and writes the output of
   * that method to the sink output stream.
   *
   * @throws java.io.IOException If an I/O error occurs, or if an error
   *         is caused by finalizing the transformation.
   */
  public void close() throws IOException
  {
    try
      {
        int len;
        if (state != FIRST_TIME)
          {
            len = cipher.update(inBuffer[0], 0, inBuffer[0].length, outBuffer);
            out.write(outBuffer, 0, len);
          }
        len = cipher.doFinal(inBuffer[0], 0, inLength, outBuffer);
        out.write(outBuffer, 0, len);
      }
    catch (javax.crypto.IllegalBlockSizeException ibse)
      {
        throw new IOException(ibse.toString());
      }
    catch (javax.crypto.BadPaddingException bpe)
      {
        throw new IOException(bpe.toString());
      }
    catch (ShortBufferException sbe)
      {
        throw new IOException(sbe.toString());
      }
    out.flush();
    out.close();
  }

  /**
   * Flush any pending output.
   *
   * @throws java.io.IOException If an I/O error occurs.
   */
  public void flush() throws IOException
  {
    out.flush();
  }

  /**
   * Write a single byte to the output stream.
   *
   * @param b The next byte.
   * @throws java.io.IOException If an I/O error occurs, or if the
   *         underlying cipher is not in the correct state to transform
   *         data.
   */
  public void write(int b) throws IOException
  {
    if (isStream)
      {
        byte[] buf = new byte[] { (byte) b };
        try
          {
            cipher.update(buf, 0, 1, buf, 0);
          }
        catch (ShortBufferException sbe)
          {
            throw new IOException(sbe.toString());
          }
        out.write(buf);
        return;
      }
    inBuffer[1][inLength++] = (byte) b;
    if (inLength == inBuffer[1].length)
      process();
  }

  /**
   * Write a byte array to the output stream.
   *
   * @param buf The next bytes.
   * @throws java.io.IOException If an I/O error occurs, or if the
   *         underlying cipher is not in the correct state to transform
   *         data.
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
   * @throws java.io.IOException If an I/O error occurs, or if the
   *         underlying cipher is not in the correct state to transform
   *         data.
   */
  public void write(byte[] buf, int off, int len) throws IOException
  {
    if (isStream)
      {
        out.write(cipher.update(buf, off, len));
        return;
      }
    int count = 0;
    while (count < len)
      {
        int l = Math.min(inBuffer[1].length - inLength, len - count);
        System.arraycopy(buf, off+count, inBuffer[1], inLength, l);
        count += l;
        inLength += l;
        if (inLength == inBuffer[1].length)
          process();
      }
  }

  // Own method.
  // -------------------------------------------------------------------------

  private void process() throws IOException
  {
    if (state == SECOND_TIME)
      {
        state = SEASONED;
      }
    else
      {
        byte[] temp = inBuffer[0];
        inBuffer[0] = inBuffer[1];
        inBuffer[1] = temp;
      }
    if (state == FIRST_TIME)
      {
        inLength = 0;
        state = SECOND_TIME;
        return;
      }
    try
      {
        cipher.update(inBuffer[0], 0, inBuffer[0].length, outBuffer);
      }
    catch (ShortBufferException sbe)
      {
        throw new IOException(sbe.toString());
      }
    out.write(outBuffer);
    inLength = 0;
  }
}

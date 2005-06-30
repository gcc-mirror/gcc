/* CipherInputStream.java -- Filters input through a cipher.
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

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * This is an {@link java.io.InputStream} that filters its data
 * through a {@link Cipher} before returning it. The <code>Cipher</code>
 * argument must have been initialized before it is passed to the
 * constructor.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class CipherInputStream extends FilterInputStream
{

  // Constants and variables.
  // ------------------------------------------------------------------------

  /**
   * The underlying {@link Cipher} instance.
   */
  private Cipher cipher;

  /**
   * Data that has been transformed but not read.
   */
  private byte[] outBuffer;

  /**
   * The offset into {@link #outBuffer} where valid data starts.
   */
  private int outOffset;

  /**
   * The number of valid bytes in the {@link #outBuffer}.
   */
  private int outLength;

  /**
   * Byte buffer that is filled with raw data from the underlying input
   * stream.
   */
  private byte[][] inBuffer;

  /**
   * The amount of bytes in inBuffer[0] that may be input to the cipher.
   */
  private int inLength;

  /**
   * We set this when the cipher block size is 1, meaning that we can
   * transform any amount of data.
   */
  private boolean isStream;

  private static final int VIRGIN = 0;  // I am born.
  private static final int LIVING = 1;  // I am nailed to the hull.
  private static final int DYING  = 2;  // I am eaten by sharks.
  private static final int DEAD   = 3;
  private int state;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Creates a new input stream with a source input stream and cipher.
   *
   * @param in     The underlying input stream.
   * @param cipher The cipher to filter data through.
   */
  public CipherInputStream(InputStream in, Cipher cipher)
  {
    this(in);
    this.cipher = cipher;
    if (!(isStream = cipher.getBlockSize() == 1))
      {
        inBuffer = new byte[2][];
        inBuffer[0] = new byte[cipher.getBlockSize()];
        inBuffer[1] = new byte[cipher.getBlockSize()];
        inLength = 0;
        outBuffer = new byte[cipher.getBlockSize()];
        outOffset = outLength = 0;
        state = VIRGIN;
      }
  }

  /**
   * Creates a new input stream without a cipher. This constructor is
   * <code>protected</code> because this class does not work without an
   * underlying cipher.
   *
   * @param in The underlying input stream.
   */
  protected CipherInputStream(InputStream in)
  {
    super(in);
  }

  // Instance methods overriding java.io.FilterInputStream.
  // ------------------------------------------------------------------------

  /**
   * Returns the number of bytes available without blocking. The value
   * returned by this method is never greater than the underlying
   * cipher's block size.
   *
   * @return The number of bytes immediately available.
   * @throws java.io.IOException If an I/O exception occurs.
   */
  public int available() throws IOException
  {
    if (isStream)
      return super.available();
    return outLength - outOffset;
  }

  /**
   * Close this input stream. This method merely calls the {@link
   * java.io.InputStream#close()} method of the underlying input stream.
   *
   * @throws java.io.IOException If an I/O exception occurs.
   */
  public void close() throws IOException
  {
    super.close();
  }

  /**
   * Read a single byte from this input stream; returns -1 on the
   * end-of-file.
   *
   * @return The byte read, or -1 if there are no more bytes.
   * @throws java.io.IOExcpetion If an I/O exception occurs.
   */
  public int read() throws IOException
  {
    if (isStream)
      {
        byte[] buf = new byte[1];
        int in = super.read();
        if (in == -1)
          return -1;
        buf[0] = (byte) in;
        try
          {
            cipher.update(buf, 0, 1, buf, 0);
          }
        catch (ShortBufferException shouldNotHappen)
          {
            throw new IOException(shouldNotHappen.getMessage());
          }
        return buf[0] & 0xFF;
      }
    if (state == DEAD) return -1;
    if (available() == 0) nextBlock();
    if (state == DEAD) return -1;
    return outBuffer[outOffset++] & 0xFF;
  }

  /**
   * Read bytes into an array, returning the number of bytes read or -1
   * on the end-of-file.
   *
   * @param buf The byte array to read into.
   * @param off The offset in <code>buf</code> to start.
   * @param len The maximum number of bytes to read.
   * @return The number of bytes read, or -1 on the end-of-file.
   * @throws java.io.IOException If an I/O exception occurs.
   */
  public int read(byte[] buf, int off, int len) throws IOException
  {
    if (isStream)
      {
        len = super.read(buf, off, len);
        try
          {
            cipher.update(buf, off, len, buf, off);
          }
        catch (ShortBufferException shouldNotHappen)
          {
            throw new IOException(shouldNotHappen.getMessage());
          }
        return len;
      }

    int count = 0;
    while (count < len)
      {
        if (available() == 0)
          nextBlock();
        if (state == DEAD)
          {
            if (count > 0) return count;
            else return -1;
          }
        int l = Math.min(available(), len - count);
        System.arraycopy(outBuffer, outOffset, buf, count+off, l);
        count += l;
        outOffset = outLength = 0;
      }
    return count;
  }

  /**
   * Read bytes into an array, returning the number of bytes read or -1
   * on the end-of-file.
   *
   * @param buf The byte arry to read into.
   * @return The number of bytes read, or -1 on the end-of-file.
   * @throws java.io.IOException If an I/O exception occurs.
   */
  public int read(byte[] buf) throws IOException
  {
    return read(buf, 0, buf.length);
  }

  /**
   * Skip a number of bytes. This class only supports skipping as many
   * bytes as are returned by {@link #available()}, which is the number
   * of transformed bytes currently in this class's internal buffer.
   *
   * @param bytes The number of bytes to skip.
   * @return The number of bytes skipped.
   */
  public long skip(long bytes) throws IOException
  {
    if (isStream)
      {
        return super.skip(bytes);
      }
    long ret = 0;
    if (bytes > 0 && available() > 0)
      {
        ret = available();
        outOffset = outLength = 0;
      }
    return ret;
  }

  /**
   * Returns whether or not this input stream supports the {@link
   * #mark(long)} and {@link #reset()} methods; this input stream does
   * not, however, and invariably returns <code>false</code>.
   *
   * @return <code>false</code>
   */
  public boolean markSupported()
  {
    return false;
  }

  /**
   * Set the mark. This method is unsupported and is empty.
   *
   * @param mark Is ignored.
   */
  public void mark(int mark)
  {
  }

  /**
   * Reset to the mark. This method is unsupported and is empty.
   */
  public void reset() throws IOException
  {
    throw new IOException("reset not supported");
  }

  // Own methods.
  // -------------------------------------------------------------------------

  private void nextBlock() throws IOException
  {
    byte[] temp = inBuffer[0];
    inBuffer[0] = inBuffer[1];
    inBuffer[1] = temp;
    int count = 0;
    boolean eof = false;

    if (state == VIRGIN || state == LIVING)
      {
        do
          {
            int l = in.read(inBuffer[1], count, inBuffer[1].length - count);
            if (l == -1)
              {
                eof = true;
                break;
              }
            count += l;
          }
        while (count < inBuffer[1].length);
      }

    try
      {
        switch (state)
          {
          case VIRGIN:
            state = LIVING;
            nextBlock();
            break;
          case LIVING:
            if (eof)
              {
                if (count > 0)
                  {
                    outOffset = cipher.update(inBuffer[0], 0, inLength, outBuffer, 0);
                    state = DYING;
                  }
                else
                  {
                    outOffset = cipher.doFinal(inBuffer[0], 0, inLength, outBuffer, 0);
                    state = DEAD;
                  }
              }
            else
              {
                outOffset = cipher.update(inBuffer[0], 0, inLength, outBuffer, 0);
              }
            break;
          case DYING:
            outOffset = cipher.doFinal(inBuffer[0], 0, inLength, outBuffer, 0);
            state = DEAD;
            break;
          case DEAD:
          }
      }
    catch (ShortBufferException sbe)
      {
        throw new IOException(sbe.toString());
      }
    catch (BadPaddingException bpe)
      {
        throw new IOException(bpe.toString());
      }
    catch (IllegalBlockSizeException ibse)
      {
        throw new IOException(ibse.toString());
      }
    inLength = count;
  }
}

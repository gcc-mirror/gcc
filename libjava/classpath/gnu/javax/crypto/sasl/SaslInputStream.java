/* SaslInputStream.java --
   Copyright (C) 2003, 2006, 2010 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.sasl;

import gnu.java.security.Configuration;
import gnu.java.security.util.Util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.util.logging.Logger;

import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;
import javax.security.sasl.SaslServer;

/**
 * An input stream that uses either a {@link SaslClient} or a {@link SaslServer}
 * to process the data through these entities' security layer filter(s).
 */
public class SaslInputStream
    extends InputStream
{
  private static final Logger log = Configuration.DEBUG ?
                Logger.getLogger(SaslInputStream.class.getName()) : null;
  private SaslClient client;
  private SaslServer server;
  private int maxRawSendSize;
  private InputStream source;
  private byte[] internalBuf;

  public SaslInputStream(SaslClient client, InputStream source)
      throws IOException
  {
    super();

    this.client = client;
    String size = (String) client.getNegotiatedProperty(Sasl.RAW_SEND_SIZE);
    maxRawSendSize = Integer.parseInt(size);
    server = null;
    this.source = source;
  }

  public SaslInputStream(SaslServer server, InputStream source)
      throws IOException
  {
    super();

    this.server = server;
    String size = (String) server.getNegotiatedProperty(Sasl.RAW_SEND_SIZE);
    maxRawSendSize = Integer.parseInt(size);
    client = null;
    this.source = source;
  }

  public int available() throws IOException
  {
    return (internalBuf == null) ? 0 : internalBuf.length;
  }

  public void close() throws IOException
  {
    source.close();
  }

  /**
   * Reads the next byte of data from the input stream. The value byte is
   * returned as an <code>int</code> in the range <code>0</code> to
   * <code>255</code>. If no byte is available because the end of the stream
   * has been reached, the value <code>-1</code> is returned. This method
   * blocks until input data is available, the end of the stream is detected, or
   * an exception is thrown.
   * <p>
   * From a SASL mechanism provider's perspective, if a security layer has been
   * negotiated, the underlying <i>source</i> is expected to contain SASL
   * buffers, as defined in RFC 2222. Four octets in network byte order in the
   * front of each buffer identify the length of the buffer. The provider is
   * responsible for performing any integrity checking or other processing on
   * the buffer before returning the data as a stream of octets. For example,
   * the protocol driver's request for a single octet from the stream might;
   * i.e. an invocation of this method, may result in an entire SASL buffer
   * being read and processed before that single octet can be returned.
   *
   * @return the next byte of data, or <code>-1</code> if the end of the
   *         stream is reached.
   * @throws IOException if an I/O error occurs.
   */
  public int read() throws IOException
  {
    int result = -1;
    if (internalBuf != null && internalBuf.length > 0)
      {
        result = internalBuf[0] & 0xFF;
        if (internalBuf.length == 1)
          internalBuf = new byte[0];
        else
          {
            byte[] tmp = new byte[internalBuf.length - 1];
            System.arraycopy(internalBuf, 1, tmp, 0, tmp.length);
            internalBuf = tmp;
          }
      }
    else
      {
        byte[] buf = new byte[1];
        int check = read(buf);
        result = (check > 0) ? (buf[0] & 0xFF) : -1;
      }
    return result;
  }

  /**
   * Reads up to <code>len</code> bytes of data from the underlying <i>source</i>
   * input stream into an array of bytes. An attempt is made to read as many as
   * <code>len</code> bytes, but a smaller number may be read, possibly zero.
   * The number of bytes actually read is returned as an integer.
   * <p>
   * This method blocks until input data is available, end of file is detected,
   * or an exception is thrown.
   * <p>
   * If <code>b</code> is <code>null</code>, a {@link NullPointerException}
   * is thrown.
   * <p>
   * If <code>off</code> is negative, or <code>len</code> is negative, or
   * <code>off+len</code> is greater than the length of the array
   * <code>b</code>, then an {@link IndexOutOfBoundsException} is thrown.
   * <p>
   * If <code>len</code> is zero, then no bytes are read and <code>0</code>
   * is returned; otherwise, there is an attempt to read at least one byte. If
   * no byte is available because the stream is at end of file, the value
   * <code>-1</code> is returned; otherwise, at least one byte is read and
   * stored into <code>b</code>.
   * <p>
   * The first byte read is stored into element <code>b[off]</code>, the next
   * one into <code>b[off+1]</code>, and so on. The number of bytes read is,
   * at most, equal to <code>len</code>. Let <code>k</code> be the number
   * of bytes actually read; these bytes will be stored in elements
   * <code>b[off]</code> through <code>b[off+k-1]</code>, leaving elements
   * <code>b[off+k]</code> through <code>b[off+len-1]</code> unaffected.
   * <p>
   * In every case, elements <code>b[0]</code> through <code>b[off]</code>
   * and elements <code>b[off+len]</code> through <code>b[b.length-1]</code>
   * are unaffected.
   * <p>
   * If the first byte cannot be read for any reason other than end of file,
   * then an {@link IOException} is thrown. In particular, an
   * {@link IOException} is thrown if the input stream has been closed.
   * <p>
   * From the SASL mechanism provider's perspective, if a security layer has
   * been negotiated, the underlying <i>source</i> is expected to contain SASL
   * buffers, as defined in RFC 2222. Four octets in network byte order in the
   * front of each buffer identify the length of the buffer. The provider is
   * responsible for performing any integrity checking or other processing on
   * the buffer before returning the data as a stream of octets. The protocol
   * driver's request for a single octet from the stream might result in an
   * entire SASL buffer being read and processed before that single octet can be
   * returned.
   *
   * @param b the buffer into which the data is read.
   * @param off the start offset in array <code>b</code> at which the data is
   *          wricodeen.
   * @param len the maximum number of bytes to read.
   * @return the total number of bytes read into the buffer, or <code>-1</code>
   *         if there is no more data because the end of the stream has been
   *         reached.
   * @throws IOException if an I/O error occurs.
   */
  public int read(byte[] b, int off, int len) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "read", new Object[] {
          b, Integer.valueOf(off), Integer.valueOf(len)
      });
    if ((off < 0) || (off > b.length) || (len < 0) || ((off + len) > b.length)
        || ((off + len) < 0))
      throw new IndexOutOfBoundsException("off=" + off + ", len=" + len
                                          + ", b.length=" + b.length);
    if (len == 0)
      {
        if (Configuration.DEBUG)
          log.exiting(this.getClass().getName(), "read", Integer.valueOf(0));
        return 0;
      }
    if (Configuration.DEBUG)
      log.finer("Available: " + available());
    int result = 0;
    if (internalBuf == null || internalBuf.length < 1)
      try
        {
          internalBuf = readSaslBuffer();
          if (internalBuf == null)
            {
              if (Configuration.DEBUG)
                {
                  log.finer("Underlying stream empty. Returning -1");
                  log.exiting(this.getClass().getName(), "read",
                              Integer.valueOf(-1));
                }
              return -1;
            }
        }
      catch (InterruptedIOException x)
        {
          if (Configuration.DEBUG)
            {
              log.finer("Reading thread was interrupted. Returning -1");
              log.throwing(this.getClass().getName(), "read", x);
              log.exiting(this.getClass().getName(), "read",
                          Integer.valueOf(-1));
            }
          return -1;
        }
    if (len <= internalBuf.length)
      {
        result = len;
        System.arraycopy(internalBuf, 0, b, off, len);
        if (len == internalBuf.length)
          internalBuf = null;
        else
          {
            byte[] tmp = new byte[internalBuf.length - len];
            System.arraycopy(internalBuf, len, tmp, 0, tmp.length);
            internalBuf = tmp;
          }
      }
    else
      {
        // first copy the available bytes to b
        result = internalBuf.length;
        System.arraycopy(internalBuf, 0, b, off, result);
        internalBuf = null;
        off += result;
        len -= result;
        int remaining; // count of bytes remaining in buffer after an iteration
        int delta; // count of bytes moved to b after an iteration
        int datalen;
        byte[] data;
        while (len > 0)
          // we need to read SASL buffers, as long as there are at least
          // 4 bytes available at the source
          if (source.available() > 3)
            {
              // process a buffer
              data = readSaslBuffer();
              if (data == null)
                {
                  if (Configuration.DEBUG)
                    log.finer("Underlying stream exhausted. Breaking...");
                  break;
                }
              datalen = data.length;
              // copy [part of] the result to b
              remaining = (datalen <= len) ? 0 : datalen - len;
              delta = datalen - remaining;
              System.arraycopy(data, 0, b, off, delta);
              if (remaining > 0)
                {
                  internalBuf = new byte[remaining];
                  System.arraycopy(data, delta, internalBuf, 0, remaining);
                }
              // update off, result and len
              off += delta;
              result += delta;
              len -= delta;
            }
          else
            { // nothing much we can do except return what we have
              if (Configuration.DEBUG)
                log.finer("Not enough bytes in source to read a buffer. Breaking...");
              break;
            }
      }
    if (Configuration.DEBUG)
      {
        log.finer("Remaining: "
                  + (internalBuf == null ? 0 : internalBuf.length));
        log.exiting(this.getClass().getName(), "read()", String.valueOf(result));
      }
    return result;
  }

  /**
   * Reads a SASL buffer from the underlying source if at least 4 bytes are
   * available.
   *
   * @return the byte[] of decoded buffer contents, or null if the underlying
   *         source was exhausted.
   * @throws IOException if an I/O exception occurs during the operation.
   */
  private byte[] readSaslBuffer() throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "readSaslBuffer()");
    int realLength; // check if we read as many bytes as we're supposed to
    byte[] result = new byte[4];
    try
      {
        realLength = source.read(result);
        if (realLength == -1)
          {
            if (Configuration.DEBUG)
              log.exiting(this.getClass().getName(), "readSaslBuffer");
            return null;
          }
      }
    catch (IOException x)
      {
        if (Configuration.DEBUG)
          log.throwing(this.getClass().getName(), "readSaslBuffer", x);
        throw x;
      }
    if (realLength != 4)
      throw new IOException("Was expecting 4 but found " + realLength);
    int bufferLength =  result[0]         << 24
                     | (result[1] & 0xFF) << 16
                     | (result[2] & 0xFF) << 8
                     | (result[3] & 0xFF);
    if (Configuration.DEBUG)
      log.finer("SASL buffer size: " + bufferLength);
    if (bufferLength > maxRawSendSize || bufferLength < 0)
      throw new SaslEncodingException("SASL buffer (security layer) too long");

    result = new byte[bufferLength];
    try
      {
        realLength = source.read(result);
      }
    catch (IOException x)
      {
        if (Configuration.DEBUG)
          log.throwing(this.getClass().getName(), "readSaslBuffer", x);
        throw x;
      }
    if (realLength != bufferLength)
      throw new IOException("Was expecting " + bufferLength + " but found "
                            + realLength);
    if (Configuration.DEBUG)
      {
        log.finer("Incoming buffer (before security) (hex): "
                  + Util.dumpString(result));
        log.finer("Incoming buffer (before security) (str): \""
                  + new String(result) + "\"");
      }
    if (client != null)
      result = client.unwrap(result, 0, realLength);
    else
      result = server.unwrap(result, 0, realLength);
    if (Configuration.DEBUG)
      {
        log.finer("Incoming buffer (after security) (hex): "
                  + Util.dumpString(result));
        log.finer("Incoming buffer (after security) (str): \""
                  + new String(result) + "\"");
        log.exiting(this.getClass().getName(), "readSaslBuffer");
      }
    return result;
  }
}

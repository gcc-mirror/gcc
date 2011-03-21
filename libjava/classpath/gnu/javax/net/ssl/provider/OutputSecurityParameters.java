/* OutputSecurityParameters.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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


package gnu.javax.net.ssl.provider;

import gnu.classpath.debug.Component;
import gnu.classpath.debug.SystemLogger;
import gnu.java.security.util.ByteBufferOutputStream;

import java.nio.ByteBuffer;

import java.util.zip.DataFormatException;
import java.util.zip.Deflater;

import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.Mac;
import javax.crypto.ShortBufferException;

public class OutputSecurityParameters
{
  private static final SystemLogger logger = SystemLogger.SYSTEM;
  private final Cipher cipher;
  private final Mac mac;
  private final Deflater deflater;
  private final SessionImpl session;
  private final CipherSuite suite;
  private long sequence;

  public OutputSecurityParameters (final Cipher cipher, final Mac mac,
                                   final Deflater deflater, SessionImpl session,
                                   CipherSuite suite)
  {
    this.cipher = cipher;
    this.mac = mac;
    this.deflater = deflater;
    this.session = session;
    this.suite = suite;
    sequence = 0;
  }

  /**
   * Encrypt a record, storing the result in the given output buffer.
   *
   * @return The number of bytes taken from the input, and the number stored
   * into `output;' that is, the size of the encrypted fragment, plus the
   * encoding for the record.
   */
  public int[] encrypt (final ByteBuffer[] input, int offset, int length,
                        final ContentType contentType, final ByteBuffer output)
    throws DataFormatException, IllegalBlockSizeException, ShortBufferException
  {
    if (offset < 0 || offset >= input.length
        || length <= 0 || offset + length > input.length)
      throw new IndexOutOfBoundsException();

    if (Debug.DEBUG)
      for (int i = offset; i < offset+length; i++)
        logger.logv(Component.SSL_RECORD_LAYER, "encrypting record [{0}]: {1}",
                    i-offset, input[i]);

    int maclen = 0;
    if (mac != null)
      maclen = session.isTruncatedMac() ? 10 : mac.getMacLength ();

    int ivlen = 0;
    byte[] iv = null;
    if (session.version.compareTo(ProtocolVersion.TLS_1_1) >= 0
        && !suite.isStreamCipher())
      {
        ivlen = cipher.getBlockSize();
        iv = new byte[ivlen];
        session.random().nextBytes(iv);
      }

    int padaddlen = 0;
    if (!suite.isStreamCipher()
        && session.version.compareTo(ProtocolVersion.TLS_1) >= 0)
      {
        padaddlen = (session.random().nextInt(255 / cipher.getBlockSize())
                     * cipher.getBlockSize());
      }

    int fragmentLength = 0;
    ByteBuffer[] fragments = null;
    // Compress the content, if needed.
    if (deflater != null)
      {
        ByteBufferOutputStream deflated = new ByteBufferOutputStream();

        byte[] inbuf = new byte[1024];
        byte[] outbuf = new byte[1024];
        int written = 0;

        // Here we use the guarantee that the deflater won't increase the
        // output size by more than 1K -- we resign ourselves to only deflate
        // as much data as we have space for *uncompressed*,
        int limit = output.remaining() - (maclen + ivlen + padaddlen) - 1024;

        for (int i = offset; i < length && written < limit; i++)
          {
            ByteBuffer in = input[i];
            while (in.hasRemaining() && written < limit)
              {
                int l = Math.min(in.remaining(), inbuf.length);
                l = Math.min(limit - written, l);
                in.get(inbuf, 0, l);
                deflater.setInput(inbuf, 0, l);
                l = deflater.deflate(outbuf);
                deflated.write(outbuf, 0, l);
                written += l;
              }
          }
        deflater.finish();
        while (!deflater.finished())
          {
            int l = deflater.deflate(outbuf);
            deflated.write(outbuf, 0, l);
            written += l;
          }
        fragments = new ByteBuffer[] { deflated.buffer() };
        fragmentLength = ((int) deflater.getBytesWritten()) + maclen + ivlen;
        deflater.reset();
        offset = 0;
        length = 1;
      }
    else
      {
        int limit = output.remaining() - (maclen + ivlen + padaddlen);
        fragments = input;
        for (int i = offset; i < length && fragmentLength < limit; i++)
          {
            int l = Math.min(limit - fragmentLength, fragments[i].remaining());
            fragmentLength += l;
          }
        fragmentLength += maclen + ivlen;
      }

    // Compute padding...
    int padlen = 0;
    byte[] pad = null;
    if (!suite.isStreamCipher())
      {
        int bs = cipher.getBlockSize();
        padlen = bs - (fragmentLength % bs);
        if (Debug.DEBUG)
          logger.logv(Component.SSL_RECORD_LAYER,
                      "framentLen:{0} padlen:{1} blocksize:{2}",
                      fragmentLength, padlen, bs);
        if (session.version.compareTo(ProtocolVersion.TLS_1) >= 0)
          {
            // TLS 1.0 and later uses a random amount of padding, up to
            // 255 bytes. Each byte of the pad is equal to the padding
            // length, minus one.
            padlen += padaddlen;
            while (padlen > 255)
              padlen -= bs;
            pad = new byte[padlen];
            for (int i = 0; i < padlen; i++)
              pad[i] = (byte) (padlen - 1);
          }
        else
          {
            // SSL 3 uses a pad only as large as the block size, but the
            // pad may contain any values.
            pad = new byte[padlen];
            session.random().nextBytes(pad);
            pad[padlen - 1] = (byte) (padlen - 1);
          }
        fragmentLength += pad.length;
      }

    // If there is a MAC, compute it.
    byte[] macValue = null;
    if (mac != null)
      {
        mac.update((byte) (sequence >>> 56));
        mac.update((byte) (sequence >>> 48));
        mac.update((byte) (sequence >>> 40));
        mac.update((byte) (sequence >>> 32));
        mac.update((byte) (sequence >>> 24));
        mac.update((byte) (sequence >>> 16));
        mac.update((byte) (sequence >>>  8));
        mac.update((byte)  sequence);
        mac.update((byte) contentType.getValue());
        if (session.version != ProtocolVersion.SSL_3)
          {
            mac.update((byte) session.version.major ());
            mac.update((byte) session.version.minor ());
          }
        int toWrite = fragmentLength - maclen - ivlen - padlen;
        mac.update((byte) (toWrite >>> 8));
        mac.update((byte)  toWrite);
        int written = 0;
        for (int i = offset; i < length && written < toWrite; i++)
          {
            ByteBuffer fragment = fragments[i].duplicate();
            int l = Math.min(fragment.remaining(), toWrite - written);
            fragment.limit(fragment.position() + l);
            mac.update(fragment);
          }
        macValue = mac.doFinal();
      }

    Record outrecord = new Record(output);
    outrecord.setContentType(contentType);
    outrecord.setVersion(session.version);
    outrecord.setLength(fragmentLength);

    int consumed = 0;
    ByteBuffer outfragment = outrecord.fragment();

    if (cipher != null)
      {
        if (iv != null)
          cipher.update(ByteBuffer.wrap(iv), outfragment);
        int toWrite = fragmentLength - maclen - ivlen - padlen;
        for (int i = offset; i < offset + length && consumed < toWrite; i++)
          {
            ByteBuffer fragment = fragments[i].slice();
            int l = Math.min(fragment.remaining(), toWrite - consumed);
            fragment.limit(fragment.position() + l);
            cipher.update(fragment, outfragment);
            fragments[i].position(fragments[i].position() + l);
            consumed += l;
          }
        if (macValue != null)
          cipher.update(ByteBuffer.wrap(macValue), outfragment);
        if (pad != null)
          cipher.update(ByteBuffer.wrap(pad), outfragment);
      }
    else
      {
        // iv and pad are only used if we have a block cipher.
        int toWrite = fragmentLength - maclen;
        for (int i = offset; i < offset + length && consumed < toWrite; i++)
          {
            ByteBuffer fragment = fragments[i];
            int l = Math.min(fragment.remaining(), toWrite - consumed);
            fragment.limit(fragment.position() + l);
            outfragment.put(fragment);
            consumed += l;
          }
        if (macValue != null)
          outfragment.put(macValue);
      }

    // Advance the output buffer's position.
    output.position(output.position() + outrecord.length() + 5);
    sequence++;

    return new int[] { consumed, fragmentLength + 5 };
  }

  CipherSuite suite()
  {
    return suite;
  }
}

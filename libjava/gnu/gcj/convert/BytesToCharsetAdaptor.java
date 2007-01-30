/* Copyright (C) 2005, 2007  Free Software Foundation

This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.CoderResult;
import gnu.java.nio.charset.EncodingHelper;

/**
 * Adaptor class that allow any {@link Charset} to be used
 * as a BytesToUnicode converter.
 */
public class BytesToCharsetAdaptor extends BytesToUnicode
{
  /**
   * The CharsetDecoder that does all the work.
   */
  private final CharsetDecoder decoder;

  /**
   * ByteBuffer wrapper for this.buf.
   */
  private ByteBuffer inBuf;

  /**
   * Create a new BytesToCharsetAdaptor for the given Charset.
   *
   * @param cs the Charset.
   */
  public BytesToCharsetAdaptor(Charset cs)
  {
    this(cs.newDecoder());
  }

  /**
   * Create a new BytesToCharsetAdaptor for the given CharsetDecoder.
   *
   * @param dec the CharsetDecoder.
   */
  public BytesToCharsetAdaptor(CharsetDecoder dec)
  {
    decoder = dec;
    // Use default replacments on bad input so that we don't have to
    // deal with errors.
    decoder.onMalformedInput(CodingErrorAction.REPLACE);
    decoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
  }

  /**
   * Return the decoder's name.  The backing Charset's name is
   * returned.
   *
   * @return The name.
   */
  public String getName()
  {
    return EncodingHelper.getOldCanonical(decoder.charset().name());
  }

  public int read(char[] outbuffer, int outpos, int count)
  {
    if (inBuf == null || ! inBuf.hasArray() || inBuf.array() != inbuffer)
      inBuf = ByteBuffer.wrap(inbuffer);
    inBuf.limit(inlength);
    inBuf.position(inpos);

    CharBuffer outBuf = CharBuffer.wrap(outbuffer, outpos, count);
    decoder.decode(inBuf, outBuf, false);

    // Update this.inpos to reflect the bytes consumed.
    inpos = inBuf.position();
    // Return the number of characters that were written to outbuffer.
    return outBuf.position() - outpos;
  }

  // These aren't cached.
  public void done()
  {
  }
}

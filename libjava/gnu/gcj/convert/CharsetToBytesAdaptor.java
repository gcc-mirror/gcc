/* Copyright (C) 2005, 2006  Free Software Foundation

This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert; 

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.CoderResult;
import gnu.java.nio.charset.EncodingHelper;

/**
 * Adaptor class that allow any {@link Charset} to be used
 * as a UnicodeToBytes converter.
 */
public class CharsetToBytesAdaptor extends UnicodeToBytes
{
  /**
   * The CharsetEncoder that does all the work.
   */
  private final CharsetEncoder encoder;

  /**
   * ByteBuffer wrapper for this.buf.
   */
  private ByteBuffer outBuf;

  /**
   * True if we've told the CharsetEncoder that there are no more
   * characters available.
   */
  private boolean closedEncoder;

  /**
   * True if we're finished.
   */
  private boolean finished;

  /**
   * Create a new CharsetToBytesAdaptor for the given Charset.
   *
   * @param cs The Charset.
   */
  public CharsetToBytesAdaptor(Charset cs)
  {
    this(cs.newEncoder());
  }

  /**
   * Create a new CharsetToBytesAdaptor for the given CharsetEncoder.
   *
   * @param enc The CharsetEncoder.
   */
  public CharsetToBytesAdaptor(CharsetEncoder enc)
  {
    encoder = enc;
    // Use default replacments on bad input so that we don't have to
    // deal with errors.
    encoder.onMalformedInput(CodingErrorAction.REPLACE);
    encoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
  }

  /**
   * Return the encoder's name.  The backing Charset's name is
   * returned.
   *
   * @return The name.
   */
  public String getName()
  {
    return EncodingHelper.getOldCanonical(encoder.charset().name());
  }

  public int write (char[] inbuffer, int inpos, int inlength)
  {
    // Wrap the char array so it can be used by the encoder.
    CharBuffer b = CharBuffer.wrap(inbuffer, inpos, inlength);
    write(b);
    return b.position() - inpos; // Number of chars consumed.
  }

  public int write (String str, int inpos, int inlength, char work)
  {
    // Wrap the String so it can be used by the encoder.
    CharBuffer b = CharBuffer.wrap(str, inpos, inlength);
    write(b);
    return b.position() - inpos; // Number of chars consumed.
  }

  /**
   * Encode as much of inBuf as will fit in buf.  The number of
   * chars consumed is reflected by the new position of inBuf.  The
   * output is put in buf and count is incremented by the number of
   * bytes written.
   *
   * @param inBuf The input.
   */
  private void write(CharBuffer inBuf)
  {
    // Reuse existing outBuf if it is still wrapping the same array
    // it was created with.
    if (outBuf == null || !outBuf.hasArray() || outBuf.array() != buf)
      outBuf = ByteBuffer.wrap(buf);

    // Set the current position.
    outBuf.position(count);

    // If we've already said that there is no more input available,
    // then we simply try to flush again.
    if (closedEncoder)
      {
	CoderResult result = encoder.flush(outBuf);
	if (result == CoderResult.UNDERFLOW)
	  finished = true;
       }
    else
      {
	// Do the conversion.  If there are no characters to write,
	// then we are finished.
	closedEncoder = ! inBuf.hasRemaining();
	encoder.encode(inBuf, outBuf, closedEncoder);
      }

    // Mark the new end of buf.
    count = outBuf.position();
  }

  /**
   * Check for cached output in the converter.
   *
   * @return true if there is cached output that has not been
   * written to buf.
   */
  public boolean havePendingBytes()
  {
    return ! finished;
  }

  // These aren't cached.
  public void done()
  {
  }
}

/* CharsetEncoder.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;

/**
 * @author Jesse Rosenstock
 * @since 1.4
 */
public abstract class CharsetEncoder
{
  private static final int STATE_RESET   = 0;
  private static final int STATE_CODING  = 1;
  private static final int STATE_END     = 2;
  private static final int STATE_FLUSHED = 3;

  private static final byte[] DEFAULT_REPLACEMENT = {(byte)'?'};

  private final Charset charset;
  private final float averageBytesPerChar;
  private final float maxBytesPerChar;
  private byte[] replacement;

  private int state = STATE_RESET;

  private CodingErrorAction malformedInputAction
    = CodingErrorAction.REPORT;
  private CodingErrorAction unmappableCharacterAction
    = CodingErrorAction.REPORT;

  protected CharsetEncoder (Charset cs, float averageBytesPerChar,
                            float maxBytesPerChar)
  {
    this (cs, averageBytesPerChar, maxBytesPerChar, DEFAULT_REPLACEMENT);
  }

  protected CharsetEncoder (Charset cs, float averageBytesPerChar,
                            float maxBytesPerChar, byte[] replacement)
  {
    if (averageBytesPerChar <= 0.0f)
      throw new IllegalArgumentException ("Non-positive averageBytesPerChar");
    if (maxBytesPerChar <= 0.0f)
      throw new IllegalArgumentException ("Non-positive maxBytesPerChar");

    this.charset = cs;
    this.averageBytesPerChar
      = averageBytesPerChar;
    this.maxBytesPerChar
      = maxBytesPerChar;
    this.replacement = replacement;
    implReplaceWith (replacement);
  }

  public final float averageBytesPerChar ()
  {
    return averageBytesPerChar;
  }

  public boolean canEncode (char c)
  {
    CharBuffer cb = CharBuffer.allocate (1).put (c);
    cb.flip ();
    return canEncode (cb);
  }

  public boolean canEncode (CharSequence cs)
  {
    CharBuffer cb;
    if (cs instanceof CharBuffer)
      cb = ((CharBuffer) cs).duplicate ();
    else
      cb = CharBuffer.wrap (cs);
    return canEncode (cb);
  }

  private boolean canEncode (CharBuffer cb)
  {
    // It is an error if a coding operation is "in progress"
    // I take that to mean the state is not reset or flushed.
    // XXX: check "in progress" everywhere
    if (state == STATE_FLUSHED)
      reset ();
    else if (state != STATE_RESET)
      throw new IllegalStateException ();

    CodingErrorAction oldMalformedInputAction = malformedInputAction;
    CodingErrorAction oldUnmappableCharacterAction
      = unmappableCharacterAction;

    try
      {
        if (oldMalformedInputAction != CodingErrorAction.REPORT)
          onMalformedInput (CodingErrorAction.REPORT);
        if (oldUnmappableCharacterAction != CodingErrorAction.REPORT)
          onUnmappableCharacter (CodingErrorAction.REPORT);
      }
    catch (Exception e)
      {
        return false;
      }
    finally
      {
        if (oldMalformedInputAction != CodingErrorAction.REPORT)
          onMalformedInput (oldMalformedInputAction);
        if (oldUnmappableCharacterAction != CodingErrorAction.REPORT)
          onUnmappableCharacter (oldUnmappableCharacterAction);
      }

    return true;
  }

  public final Charset charset ()
  {
    return charset;
  }

  public final ByteBuffer encode (CharBuffer in)
    throws CharacterCodingException
  {
    // XXX: Sun's Javadoc seems to contradict itself saying an
    // IllegalStateException is thrown "if a decoding operation is already
    // in progress" and also that "it resets this Encoder".
    // Should we check to see that the state is reset, or should we
    // call reset()?
    if (state != STATE_RESET)
      throw new IllegalStateException ();

    // REVIEW: Using max instead of average may allocate a very large
    // buffer.  Maybe we should do something more efficient?
    int remaining = in.remaining ();
    int n = (int) (remaining * maxBytesPerChar ());
    ByteBuffer out = ByteBuffer.allocate (n);

    if (remaining == 0)
      {
        state = STATE_FLUSHED;
        return out;
      }

    CoderResult cr = encode (in, out, true);
    if (cr.isError ())
      cr.throwException ();

    cr = flush (out);
    if (cr.isError ())
      cr.throwException ();

    out.flip ();

    // Unfortunately, resizing the actual bytebuffer array is required.
    byte[] resized = new byte[out.remaining()];
    out.get(resized);
    return ByteBuffer.wrap(resized);
  }

  public final CoderResult encode (CharBuffer in, ByteBuffer out,
                                   boolean endOfInput)
  {
    int newState = endOfInput ? STATE_END : STATE_CODING;
    // XXX: Need to check for "previous step was an invocation [not] of
    // this method with a value of true for the endOfInput parameter but
    // a return value indicating an incomplete decoding operation"
    // XXX: We will not check the previous return value, just
    // that the previous call passed true for endOfInput
    if (state != STATE_RESET && state != STATE_CODING
        && !(endOfInput && state == STATE_END))
      throw new IllegalStateException ();
    state = newState;

    for (;;)
      {
        CoderResult cr;
        try
          {
            cr = encodeLoop (in, out);
          }
        catch (RuntimeException e)
          {
            throw new CoderMalfunctionError (e);
          }

        if (cr.isOverflow ())
          return cr;

        if (cr.isUnderflow ())
          {
            if (endOfInput && in.hasRemaining ())
              cr = CoderResult.malformedForLength (in.remaining ());
            else
              return cr;
          }

        CodingErrorAction action = cr.isMalformed ()
                                     ? malformedInputAction
                                     : unmappableCharacterAction;

        if (action == CodingErrorAction.REPORT)
          return cr;

        if (action == CodingErrorAction.REPLACE)
          {
            if (out.remaining () < replacement.length)
              return CoderResult.OVERFLOW;
            out.put (replacement);
          }

        in.position (in.position () + cr.length ());
      }
  }

  protected abstract CoderResult encodeLoop (CharBuffer in, ByteBuffer out);

  public final CoderResult flush (ByteBuffer out)
  {
    // It seems weird that you can flush after reset, but Sun's javadoc
    // says an IllegalStateException is thrown "If the previous step of the
    // current decoding operation was an invocation neither of the reset
    // method nor ... of the three-argument encode method with a value of
    // true for the endOfInput parameter."
    // Further note that flush() only requires that there not be
    // an IllegalStateException if the previous step was a call to
    // encode with true as the last argument.  It does not require
    // that the call succeeded.  encode() does require that it succeeded.
    // XXX: test this to see if reality matches javadoc
    if (state != STATE_RESET && state != STATE_END)
      throw new IllegalStateException ();

    state = STATE_FLUSHED;
    return implFlush (out);
  }

  protected CoderResult implFlush (ByteBuffer out)
  {
    return CoderResult.UNDERFLOW;
  }

  protected void implOnMalformedInput (CodingErrorAction newAction)
  {
    // default implementation does nothing
  }

  protected void implOnUnmappableCharacter (CodingErrorAction newAction)
  {
    // default implementation does nothing
  }

  protected void implReplaceWith (byte[] newReplacement)
  {
    // default implementation does nothing
  }

  protected void implReset ()
  {
    // default implementation does nothing
  }

  public boolean isLegalReplacement (byte[] replacement)
  {
    // TODO: cache the decoder
    // error actions will be REPORT after construction
    CharsetDecoder decoder = charset.newDecoder ();
    ByteBuffer bb = ByteBuffer.wrap (replacement);
    CharBuffer cb
      = CharBuffer.allocate ((int) (replacement.length
                                    * decoder.maxCharsPerByte ()));
    return !decoder.decode (bb, cb, true).isError ();
  }

  public CodingErrorAction malformedInputAction ()
  {
    return malformedInputAction;
  }

  public final float maxBytesPerChar ()
  {
    return maxBytesPerChar;
  }

  public final CharsetEncoder onMalformedInput (CodingErrorAction newAction)
  {
    if (newAction == null)
      throw new IllegalArgumentException ("Null action");

    malformedInputAction = newAction;
    implOnMalformedInput (newAction);
    return this;
  }

  public CodingErrorAction unmappableCharacterAction ()
  {
    return unmappableCharacterAction;
  }

  public final CharsetEncoder onUnmappableCharacter
    (CodingErrorAction newAction)
  {
    if (newAction == null)
      throw new IllegalArgumentException ("Null action");

    unmappableCharacterAction = newAction;
    implOnUnmappableCharacter (newAction);
    return this;
  }

  public final byte[] replacement ()
  {
    return replacement;
  }

  public final CharsetEncoder replaceWith (byte[] newReplacement)
  {
    if (newReplacement == null)
      throw new IllegalArgumentException ("Null replacement");
    if (newReplacement.length == 0)
      throw new IllegalArgumentException ("Empty replacement");
    // XXX: what about maxBytesPerChar?

      if (!isLegalReplacement (newReplacement))
        throw new IllegalArgumentException ("Illegal replacement");

    this.replacement = newReplacement;
    implReplaceWith (newReplacement);
    return this;
  }

  public final CharsetEncoder reset ()
  {
    state = STATE_RESET;
    implReset ();
    return this;
  }
}

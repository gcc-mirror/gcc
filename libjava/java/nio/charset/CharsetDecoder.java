/* CharsetDecoder.java -- 
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

package java.nio.charset;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;

/**
 * @author Jesse Rosenstock
 * @since 1.4
 */
public abstract class CharsetDecoder
{
  private static final int STATE_RESET   = 0;
  private static final int STATE_CODING  = 1;
  private static final int STATE_END     = 2;
  private static final int STATE_FLUSHED = 3;

  private static final String DEFAULT_REPLACEMENT = "\uFFFD";

  private final Charset charset;
  private final float averageCharsPerByte;
  private final float maxCharsPerByte;
  private String replacement;

  private int state = STATE_RESET;

  private CodingErrorAction malformedInputAction
    = CodingErrorAction.REPORT;
  private CodingErrorAction unmappableCharacterAction
    = CodingErrorAction.REPORT;

  private CharsetDecoder (Charset cs, float averageCharsPerByte,
                          float maxCharsPerByte, String replacement)
  {
    if (averageCharsPerByte <= 0.0f)
      throw new IllegalArgumentException ("Non-positive averageCharsPerByte");
    if (maxCharsPerByte <= 0.0f)
      throw new IllegalArgumentException ("Non-positive maxCharsPerByte");

    this.charset = cs;
    this.averageCharsPerByte
      = averageCharsPerByte;
    this.maxCharsPerByte
      = maxCharsPerByte;
    this.replacement = replacement;
    implReplaceWith (replacement);
  }

  protected CharsetDecoder (Charset cs, float averageCharsPerByte,
                            float maxCharsPerByte)
  {
    this (cs, averageCharsPerByte, maxCharsPerByte, DEFAULT_REPLACEMENT);
  }

  public final float averageCharsPerByte ()
  {
    return averageCharsPerByte;
  }

  public final Charset charset ()
  {
    return charset;
  }

  public final CharBuffer decode (ByteBuffer in)
    throws CharacterCodingException
  {
    // XXX: Sun's Javadoc seems to contradict itself saying an
    // IllegalStateException is thrown "if a decoding operation is already
    // in progress" and also that "it resets this Decoder".
    // Should we check to see that the state is reset, or should we
    // call reset()?
    if (state != STATE_RESET)
      throw new IllegalStateException ();

    // REVIEW: Using max instead of average may allocate a very large
    // buffer.  Maybe we should do something more efficient?
    int remaining = in.remaining ();
    int n = (int) (remaining * maxCharsPerByte ());
    CharBuffer out = CharBuffer.allocate (n);

    if (remaining == 0)
      {
        state = STATE_FLUSHED;
        return out;
      }

    CoderResult cr = decode (in, out, true);
    if (cr.isError ())
      cr.throwException ();

    cr = flush (out);
    if (cr.isError ())
      cr.throwException ();

    out.flip ();
    return out;
  }

  public final CoderResult decode (ByteBuffer in, CharBuffer out,
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
            cr = decodeLoop (in, out);
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
            if (out.remaining () < replacement.length ())
              return CoderResult.OVERFLOW;
            out.put (replacement);
          }

        in.position (in.position () + cr.length ());
      }
  }

  protected abstract CoderResult decodeLoop (ByteBuffer in, CharBuffer out);

  public Charset detectedCharset ()
  {
    throw new UnsupportedOperationException ();
  }
    
  public final CoderResult flush (CharBuffer out)
  {
    // It seems weird that you can flush after reset, but Sun's javadoc
    // says an IllegalStateException is thrown "If the previous step of the
    // current decoding operation was an invocation neither of the reset
    // method nor ... of the three-argument decode method with a value of
    // true for the endOfInput parameter."
    // Further note that flush() only requires that there not be
    // an IllegalStateException if the previous step was a call to
    // decode with true as the last argument.  It does not require
    // that the call succeeded.  decode() does require that it succeeded.
    // XXX: test this to see if reality matches javadoc
    if (state != STATE_RESET && state != STATE_END)
      throw new IllegalStateException ();

    state = STATE_FLUSHED;
    return implFlush (out);
  }

  protected CoderResult implFlush (CharBuffer out)
  {
    return CoderResult.UNDERFLOW;
  }

  public final CharsetDecoder onMalformedInput (CodingErrorAction newAction)
  {
    if (newAction == null)
      throw new IllegalArgumentException ("Null action");

    malformedInputAction = newAction;
    implOnMalformedInput (newAction);
    return this;
  }

  protected void implOnMalformedInput (CodingErrorAction newAction)
  {
    // default implementation does nothing
  }

  protected void implOnUnmappableCharacter (CodingErrorAction newAction)
  {
    // default implementation does nothing
  }

  protected void implReplaceWith (String newReplacement)
  {
    // default implementation does nothing
  }

  protected void implReset ()
  {
    // default implementation does nothing
  }

  public boolean isAutoDetecting ()
  {
    return false;
  }

  public boolean isCharsetDetected ()
  {
    throw new UnsupportedOperationException ();
  }

  public CodingErrorAction malformedInputAction ()
  {
    return malformedInputAction;
  }

  public final float maxCharsPerByte ()
  {
    return maxCharsPerByte;
  }

  public final CharsetDecoder onUnmappableCharacter
    (CodingErrorAction newAction)
  {
    if (newAction == null)
      throw new IllegalArgumentException ("Null action");

    unmappableCharacterAction = newAction;
    implOnUnmappableCharacter (newAction);
    return this;
  }

  public final String replacement ()
  {
    return replacement;
  }

  public final CharsetDecoder replaceWith (String newReplacement)
  {
    if (newReplacement == null)
      throw new IllegalArgumentException ("Null replacement");
    if (newReplacement.length () == 0)
      throw new IllegalArgumentException ("Empty replacement");
    // XXX: what about maxCharsPerByte?

    this.replacement = newReplacement;
    implReplaceWith (newReplacement);
    return this;
  }

  public final CharsetDecoder reset ()
  {
    state = STATE_RESET;
    implReset ();
    return this;
  }

  public CodingErrorAction unmappableCharacterAction ()
  {
    return unmappableCharacterAction;
  }
}

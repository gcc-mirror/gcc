/* Bidi.java -- Bidirectional Algorithm implementation
   Copyright (C) 2005, 2006, 2012  Free Software Foundation, Inc.

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


package java.text;

import java.awt.font.NumericShaper;
import java.awt.font.TextAttribute;
import java.util.ArrayList;


/**
 * Bidirectional Algorithm implementation.
 *
 * The full algorithm is
 * <a href="http://www.unicode.org/unicode/reports/tr9/">Unicode Standard
 * Annex #9: The Bidirectional Algorithm</a>.
 *
 * @since 1.4
 */
public final class Bidi
{
  /**
   * This indicates that a strongly directional character in the text should
   * set the initial direction, but if no such character is found, then the
   * initial direction will be left-to-right.
   */
  public static final int DIRECTION_DEFAULT_LEFT_TO_RIGHT = -2;

  /**
   * This indicates that a strongly directional character in the text should
   * set the initial direction, but if no such character is found, then the
   * initial direction will be right-to-left.
   */
  public static final int DIRECTION_DEFAULT_RIGHT_TO_LEFT = -1;

  /**
   * This indicates that the initial direction should be left-to-right.
   */
  public static final int DIRECTION_LEFT_TO_RIGHT = 0;

  /**
   * This indicates that the initial direction should be right-to-left.
   */
  public static final int DIRECTION_RIGHT_TO_LEFT = 1;

  // Flags used when computing the result.
  private static final int LTOR = 1 << DIRECTION_LEFT_TO_RIGHT;
  private static final int RTOL = 1 << DIRECTION_RIGHT_TO_LEFT;

  // The text we are examining, and the starting offset.
  // If we had a better way to handle createLineBidi, we wouldn't
  // need this at all -- which for the String case would be an
  // efficiency win.
  private char[] text;
  private int textOffset;
  // The embeddings corresponding to the text, and the starting offset.
  private byte[] embeddings;
  private int embeddingOffset;
  // The length of the text (and embeddings) to use.
  private int length;
  // The flags.
  private int flags;

  // All instance fields following this point are initialized
  // during analysis.  Fields before this must be set by the constructor.

  // The initial embedding level.
  private int baseEmbedding;
  // The type of each character in the text.
  private byte[] types;
  // The levels we compute.
  private byte[] levels;

  // A list of indices where a formatting code was found.  These
  // are indicies into the original text -- not into the text after
  // the codes have been removed.
  private ArrayList<Integer> formatterIndices;

  // Indices of the starts of runs in the text.
  private int[] runs;

  // A convenience field where we keep track of what kinds of runs
  // we've seen.
  private int resultFlags;

  /**
   * Create a new Bidi object given an attributed character iterator.
   * This constructor will examine various attributes of the text:
   * <ul>
   * <li> {@link TextAttribute#RUN_DIRECTION} is used to determine the
   * paragraph's base embedding level.  This constructor will recognize
   * either {@link TextAttribute#RUN_DIRECTION_LTR} or
   * {@link TextAttribute#RUN_DIRECTION_RTL}.  If neither is given,
   * {@link #DIRECTION_DEFAULT_LEFT_TO_RIGHT} is assumed.
   * </li>
   *
   * <li> If {@link TextAttribute#NUMERIC_SHAPING} is seen, then numeric
   * shaping will be done before the Bidi algorithm is run.
   * </li>
   *
   * <li> If {@link TextAttribute#BIDI_EMBEDDING} is seen on a given
   * character, then the value of this attribute will be used as an
   * embedding level override.
   * </li>
   * </ul>
   * @param iter the attributed character iterator to use
   */
  public Bidi(AttributedCharacterIterator iter)
  {
    // If set, this attribute should be set on all characters.
    // We don't check this (should we?) but we do assume that we
    // can simply examine the first character.
    Object val = iter.getAttribute(TextAttribute.RUN_DIRECTION);
    if (val == TextAttribute.RUN_DIRECTION_LTR)
      this.flags = DIRECTION_LEFT_TO_RIGHT;
    else if (val == TextAttribute.RUN_DIRECTION_RTL)
      this.flags = DIRECTION_RIGHT_TO_LEFT;
    else
      this.flags = DIRECTION_DEFAULT_LEFT_TO_RIGHT;

    // Likewise this attribute should be specified on the whole text.
    // We read it here and then, if it is set, we apply the numeric shaper
    // to the text before processing it.
    NumericShaper shaper = null;
    val = iter.getAttribute(TextAttribute.NUMERIC_SHAPING);
    if (val instanceof NumericShaper)
      shaper = (NumericShaper) val;

    text = new char[iter.getEndIndex() - iter.getBeginIndex()];
    embeddings = new byte[text.length];
    embeddingOffset = 0;
    length = text.length;
    for (int i = 0; i < text.length; ++i)
      {
        text[i] = iter.current();

        val = iter.getAttribute(TextAttribute.BIDI_EMBEDDING);
        if (val instanceof Integer)
          {
            int ival = ((Integer) val).intValue();
            byte bval;
            if (ival < -62 || ival > 62)
              bval = 0;
            else
              bval = (byte) ival;
            embeddings[i] = bval;
          }
      }

    // Invoke the numeric shaper, if specified.
    if (shaper != null)
      shaper.shape(text, 0, length);

    runBidi();
  }

  /**
   * Create a new Bidi object with the indicated text and, possibly, explicit
   * embedding settings.
   *
   * If the embeddings array is null, it is ignored.  Otherwise it is taken to
   * be explicit embedding settings corresponding to the text.  Positive values
   * from 1 to 61 are embedding levels, and negative values from -1 to -61 are
   * embedding overrides.  (FIXME: not at all clear what this really means.)
   *
   * @param text the text to use
   * @param offset the offset of the first character of the text
   * @param embeddings the explicit embeddings, or null if there are none
   * @param embedOffset the offset of the first embedding value to use
   * @param length the length of both the text and the embeddings
   * @param flags a flag indicating the base embedding direction
   */
  public Bidi(char[] text, int offset, byte[] embeddings, int embedOffset,
              int length, int flags)
  {
    if (flags != DIRECTION_DEFAULT_LEFT_TO_RIGHT
        && flags != DIRECTION_DEFAULT_RIGHT_TO_LEFT
        && flags != DIRECTION_LEFT_TO_RIGHT
        && flags != DIRECTION_RIGHT_TO_LEFT)
      throw new IllegalArgumentException("unrecognized 'flags' argument: "
                                         + flags);
    this.text = text;
    this.textOffset = offset;
    this.embeddings = embeddings;
    this.embeddingOffset = embedOffset;
    this.length = length;
    this.flags = flags;

    runBidi();
  }

  /**
   * Create a new Bidi object using the contents of the given String
   * as the text.
   * @param text the text to use
   * @param flags a flag indicating the base embedding direction
   */
  public Bidi(String text, int flags)
  {
    if (flags != DIRECTION_DEFAULT_LEFT_TO_RIGHT
        && flags != DIRECTION_DEFAULT_RIGHT_TO_LEFT
        && flags != DIRECTION_LEFT_TO_RIGHT
        && flags != DIRECTION_RIGHT_TO_LEFT)
      throw new IllegalArgumentException("unrecognized 'flags' argument: "
                                         + flags);

    // This is inefficient, but it isn't clear whether it matters.
    // If it does we can change our implementation a bit to allow either
    // a String or a char[].
    this.text = text.toCharArray();
    this.textOffset = 0;
    this.embeddings = null;
    this.embeddingOffset = 0;
    this.length = text.length();
    this.flags = flags;

    runBidi();
  }

  /**
   * Implementation function which computes the initial type of
   * each character in the input.
   */
  private void computeTypes()
  {
    types = new byte[length];
    for (int i = 0; i < length; ++i)
      types[i] = Character.getDirectionality(text[textOffset + i]);
  }

  /**
   * An internal function which implements rules P2 and P3.
   * This computes the base embedding level.
   * @return the paragraph's base embedding level
   */
  private int computeParagraphEmbeddingLevel()
  {
    // First check to see if the user supplied a directionality override.
    if (flags == DIRECTION_LEFT_TO_RIGHT
        || flags == DIRECTION_RIGHT_TO_LEFT)
      return flags;

    // This implements rules P2 and P3.
    // (Note that we don't need P1, as the user supplies
    // a paragraph.)
    for (int i = 0; i < length; ++i)
      {
        int dir = types[i];
        if (dir == Character.DIRECTIONALITY_LEFT_TO_RIGHT)
          return DIRECTION_LEFT_TO_RIGHT;
        if (dir == Character.DIRECTIONALITY_RIGHT_TO_LEFT
            || dir == Character.DIRECTIONALITY_RIGHT_TO_LEFT)
          return DIRECTION_RIGHT_TO_LEFT;
      }
    return (flags == DIRECTION_DEFAULT_LEFT_TO_RIGHT
            ? DIRECTION_LEFT_TO_RIGHT
            : DIRECTION_RIGHT_TO_LEFT);
  }

  /**
   * An internal function which implements rules X1 through X9.
   * This computes the initial levels for the text, handling
   * explicit overrides and embeddings.
   */
  private void computeExplicitLevels()
  {
    levels = new byte[length];
    byte currentEmbedding = (byte) baseEmbedding;
    // The directional override is a Character directionality
    // constant.  -1 means there is no override.
    byte directionalOverride = -1;
    // The stack of pushed embeddings, and the stack pointer.
    // Note that because the direction is inherent in the depth,
    // and because we have a bit left over in a byte, we can encode
    // the override, if any, directly in this value on the stack.
    final int MAX_DEPTH = 62;
    byte[] embeddingStack = new byte[MAX_DEPTH];
    int sp = 0;

    for (int i = 0; i < length; ++i)
      {
        // If we see an explicit embedding, we use that, even if
        // the current character is itself a directional override.
        if (embeddings != null && embeddings[embeddingOffset + i] != 0)
          {
            // It isn't at all clear what we're supposed to do here.
            // What does a negative value really mean?
            // Should we push on the embedding stack here?
            currentEmbedding = embeddings[embeddingOffset + i];
            if (currentEmbedding < 0)
              {
                currentEmbedding = (byte) -currentEmbedding;
                directionalOverride
                  = (((currentEmbedding % 2) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);
              }
            else
              directionalOverride = -1;
            continue;
          }
        // No explicit embedding.
        boolean isLtoR = false;
        boolean isSpecial = true;
        switch (types[i])
          {
            case Character.DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING:
            case Character.DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE:
              isLtoR = true;
              // Fall through.
            case Character.DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING:
            case Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE:
              {
                byte newEmbedding;
                if (isLtoR)
                  {
                    // Least greater even.
                    newEmbedding = (byte) ((currentEmbedding & ~1) + 2);
                  }
                else
                  {
                    // Least greater odd.
                    newEmbedding = (byte) ((currentEmbedding + 1) | 1);
                  }
                // FIXME: we don't properly handle invalid pushes.
                if (newEmbedding < MAX_DEPTH)
                  {
                    // The new level is valid.  Push the old value.
                    // See above for a comment on the encoding here.
                    if (directionalOverride != -1)
                      currentEmbedding |= Byte.MIN_VALUE;
                    embeddingStack[sp++] = currentEmbedding;
                    currentEmbedding = newEmbedding;
                    if (types[i] == Character.DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE)
                      directionalOverride = Character.DIRECTIONALITY_LEFT_TO_RIGHT;
                    else if (types[i] == Character.DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE)
                      directionalOverride = Character.DIRECTIONALITY_RIGHT_TO_LEFT;
                    else
                      directionalOverride = -1;
                  }
                }
              break;
            case Character.DIRECTIONALITY_POP_DIRECTIONAL_FORMAT:
              {
                // FIXME: we don't properly handle a pop with a corresponding
                // invalid push.
                if (sp == 0)
                  {
                    // We saw a pop without a push.  Just ignore it.
                    break;
                  }
                byte newEmbedding = embeddingStack[--sp];
                currentEmbedding = (byte) (newEmbedding & 0x7f);
                if (newEmbedding < 0)
                  directionalOverride
                  = (((newEmbedding & 1) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);
                else
                  directionalOverride = -1;
              }
              break;
            default:
              isSpecial = false;
              break;
          }
        levels[i] = currentEmbedding;
        if (isSpecial)
          {
            // Mark this character for removal.
            if (formatterIndices == null)
              formatterIndices = new ArrayList<Integer>();
            formatterIndices.add(Integer.valueOf(i));
          }
        else if (directionalOverride != -1)
          types[i] = directionalOverride;
      }

    // Remove the formatting codes and update both the arrays
    // and 'length'.  It would be more efficient not to remove
    // these codes, but it is also more complicated.  Also, the
    // Unicode algorithm reference does not properly describe
    // how this is to be done -- from what I can tell, their suggestions
    // in this area will not yield the correct results.
    if (formatterIndices == null)
      return;
    int output = 0, input = 0;
    final int size = formatterIndices.size();
    for (int i = 0; i <= size; ++i)
      {
        int nextFmt;
        if (i == size)
          nextFmt = length;
        else
          nextFmt = formatterIndices.get(i).intValue();
        // Non-formatter codes are from 'input' to 'nextFmt'.
        int len = nextFmt - input;
        System.arraycopy(levels, input, levels, output, len);
        System.arraycopy(types, input, types, output, len);
        output += len;
        input = nextFmt + 1;
      }
    length -= formatterIndices.size();
  }

  /**
   * An internal function to compute the boundaries of runs
   * in the text.  It isn't strictly necessary to do this, but
   * it lets us write some following passes in a less complicated
   * way.  Also it lets us efficiently implement some of the public
   * methods.  A run is simply a sequence of characters at the
   * same level.
   */
  private void computeRuns()
  {
    int runCount = 0;
    int currentEmbedding = baseEmbedding;
    for (int i = 0; i < length; ++i)
      {
        if (levels[i] != currentEmbedding)
          {
            currentEmbedding = levels[i];
            ++runCount;
          }
      }

    // This may be called multiple times.  If so, and if
    // the number of runs has not changed, then don't bother
    // allocating a new array.
    if (runs == null || runs.length != runCount + 1)
      runs = new int[runCount + 1];
    int where = 0;
    int lastRunStart = 0;
    currentEmbedding = baseEmbedding;
    for (int i = 0; i < length; ++i)
      {
        if (levels[i] != currentEmbedding)
          {
            runs[where++] = lastRunStart;
            lastRunStart = i;
            currentEmbedding = levels[i];
          }
      }
    runs[where++] = lastRunStart;
  }

  /**
   * An internal method to resolve weak types.  This implements
   * rules W1 through W7.
   */
  private void resolveWeakTypes()
  {
    final int runCount = getRunCount();

    int previousLevel = baseEmbedding;
    for (int run = 0; run < runCount; ++run)
      {
        int start = getRunStart(run);
        int end = getRunLimit(run);
        int level = getRunLevel(run);

        // These are the names used in the Bidi algorithm.
        byte sor = (((Math.max(previousLevel, level) % 2) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);
        int nextLevel;
        if (run == runCount - 1)
          nextLevel = baseEmbedding;
        else
          nextLevel = getRunLevel(run + 1);
        byte eor = (((Math.max(level, nextLevel) % 2) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);

        byte prevType = sor;
        byte prevStrongType = sor;
        for (int i = start; i < end; ++i)
          {
            final byte nextType = (i == end - 1) ? eor : types[i + 1];

            // Rule W1: change NSM to the prevailing direction.
            if (types[i] == Character.DIRECTIONALITY_NONSPACING_MARK)
              types[i] = prevType;
            else
              prevType = types[i];

            // Rule W2: change EN to AN in some cases.
            if (types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
              {
                if (prevStrongType == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC)
                  types[i] = Character.DIRECTIONALITY_ARABIC_NUMBER;
              }
            else if (types[i] == Character.DIRECTIONALITY_LEFT_TO_RIGHT
                     || types[i] == Character.DIRECTIONALITY_RIGHT_TO_LEFT
                     || types[i] == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC)
              prevStrongType = types[i];

            // Rule W3: change AL to R.
            if (types[i] == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC)
              types[i] = Character.DIRECTIONALITY_RIGHT_TO_LEFT;

            // Rule W4: handle separators between two numbers.
            if (prevType == Character.DIRECTIONALITY_EUROPEAN_NUMBER
                && nextType == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
              {
                if (types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR
                    || types[i] == Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR)
                  types[i] = nextType;
              }
            else if (prevType == Character.DIRECTIONALITY_ARABIC_NUMBER
                     && nextType == Character.DIRECTIONALITY_ARABIC_NUMBER
                     && types[i] == Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR)
              types[i] = nextType;

            // Rule W5: change a sequence of european terminators to
            // european numbers, if they are adjacent to european numbers.
            // We also include BN characters in this.
            if (types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
                || types[i] == Character.DIRECTIONALITY_BOUNDARY_NEUTRAL)
              {
                if (prevType == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
                  types[i] = prevType;
                else
                  {
                    // Look ahead to see if there is an EN terminating this
                    // sequence of ETs.
                    int j = i + 1;
                    while (j < end
                           && (types[j] == Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
                               || types[j] == Character.DIRECTIONALITY_BOUNDARY_NEUTRAL))
                      ++j;
                    if (j < end
                        && types[j] == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
                      {
                        // Change them all to EN now.
                        for (int k = i; k < j; ++k)
                          types[k] = Character.DIRECTIONALITY_EUROPEAN_NUMBER;
                      }
                  }
              }

            // Rule W6: separators and terminators change to ON.
            // Again we include BN.
            if (types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
                || types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
                || types[i] == Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR
                || types[i] == Character.DIRECTIONALITY_BOUNDARY_NEUTRAL)
              types[i] = Character.DIRECTIONALITY_OTHER_NEUTRALS;

            // Rule W7: change european number types.
            if (prevStrongType == Character.DIRECTIONALITY_LEFT_TO_RIGHT
                && types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
              types[i] = prevStrongType;
          }

        previousLevel = level;
      }
  }

  /**
   * An internal method to resolve neutral types.  This implements
   * rules N1 and N2.
   */
  private void resolveNeutralTypes()
  {
    // This implements rules N1 and N2.
    final int runCount = getRunCount();

    int previousLevel = baseEmbedding;
    for (int run = 0; run < runCount; ++run)
      {
        int start = getRunStart(run);
        int end = getRunLimit(run);
        int level = getRunLevel(run);

        byte embeddingDirection
          = (((level % 2) == 0) ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                                : Character.DIRECTIONALITY_RIGHT_TO_LEFT);
        // These are the names used in the Bidi algorithm.
        byte sor = (((Math.max(previousLevel, level) % 2) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);
        int nextLevel;
        if (run == runCount - 1)
          nextLevel = baseEmbedding;
        else
          nextLevel = getRunLevel(run + 1);
        byte eor = (((Math.max(level, nextLevel) % 2) == 0)
                      ? Character.DIRECTIONALITY_LEFT_TO_RIGHT
                      : Character.DIRECTIONALITY_RIGHT_TO_LEFT);

        byte prevStrong = sor;
        int neutralStart = -1;
        for (int i = start; i <= end; ++i)
          {
            byte newStrong = -1;
            byte thisType = i == end ? eor : types[i];
            switch (thisType)
              {
              case Character.DIRECTIONALITY_LEFT_TO_RIGHT:
                newStrong = Character.DIRECTIONALITY_LEFT_TO_RIGHT;
                break;
              case Character.DIRECTIONALITY_RIGHT_TO_LEFT:
              case Character.DIRECTIONALITY_ARABIC_NUMBER:
              case Character.DIRECTIONALITY_EUROPEAN_NUMBER:
                newStrong = Character.DIRECTIONALITY_RIGHT_TO_LEFT;
                break;
              case Character.DIRECTIONALITY_BOUNDARY_NEUTRAL:
              case Character.DIRECTIONALITY_OTHER_NEUTRALS:
              case Character.DIRECTIONALITY_SEGMENT_SEPARATOR:
              case Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR:
              case Character.DIRECTIONALITY_WHITESPACE:
                if (neutralStart == -1)
                  neutralStart = i;
                break;
              }
            // If we see a strong character, update all the neutrals.
            if (newStrong != -1)
              {
                if (neutralStart != -1)
                  {
                    byte override = (prevStrong == newStrong
                                     ? prevStrong
                                     : embeddingDirection);
                    for (int j = neutralStart; j < i; ++j)
                      types[j] = override;
                  }
                prevStrong = newStrong;
                neutralStart = -1;
              }
          }

        previousLevel = level;
      }
  }

  /**
   * An internal method to resolve implicit levels.
   * This implements rules I1 and I2.
   */
  private void resolveImplicitLevels()
  {
    // This implements rules I1 and I2.
    for (int i = 0; i < length; ++i)
      {
        if ((levels[i] & 1) == 0)
          {
            if (types[i] == Character.DIRECTIONALITY_RIGHT_TO_LEFT)
              ++levels[i];
            else if (types[i] == Character.DIRECTIONALITY_ARABIC_NUMBER
                     || types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
              levels[i] += 2;
          }
        else
          {
            if (types[i] == Character.DIRECTIONALITY_LEFT_TO_RIGHT
                || types[i] == Character.DIRECTIONALITY_ARABIC_NUMBER
                || types[i] == Character.DIRECTIONALITY_EUROPEAN_NUMBER)
              ++levels[i];
          }

        // Update the result flags.
        resultFlags |= 1 << (levels[i] & 1);
      }
    // One final update of the result flags, using the base level.
    resultFlags |= 1 << baseEmbedding;
  }

  /**
   * This reinserts the formatting codes that we removed early on.
   * Actually it does not insert formatting codes per se, but rather
   * simply inserts new levels at the appropriate locations in the
   * 'levels' array.
   */
  private void reinsertFormattingCodes()
  {
    if (formatterIndices == null)
      return;
    int input = length;
    int output = levels.length;
    // Process from the end as we are copying the array over itself here.
    for (int index = formatterIndices.size() - 1; index >= 0; --index)
      {
        int nextFmt = formatterIndices.get(index).intValue();

        // nextFmt points to a location in the original array.  So,
        // nextFmt+1 is the target of our copying.  output is the location
        // to which we last copied, thus we can derive the length of the
        // copy from it.
        int len = output - nextFmt - 1;
        output = nextFmt;
        input -= len;
        // Note that we no longer need 'types' at this point, so we
        // only edit 'levels'.
        if (nextFmt + 1 < levels.length)
          System.arraycopy(levels, input, levels, nextFmt + 1, len);

        // Now set the level at the reinsertion point.
        int rightLevel;
        if (output == levels.length - 1)
          rightLevel = baseEmbedding;
        else
          rightLevel = levels[output + 1];
        int leftLevel;
        if (input == 0)
          leftLevel = baseEmbedding;
        else
          leftLevel = levels[input];
        levels[output] = (byte) Math.max(leftLevel, rightLevel);
      }
    length = levels.length;
  }

  /**
   * This is the main internal entry point.  After a constructor
   * has initialized the appropriate local state, it will call
   * this method to do all the work.
   */
  private void runBidi()
  {
    computeTypes();
    baseEmbedding = computeParagraphEmbeddingLevel();
    computeExplicitLevels();
    computeRuns();
    resolveWeakTypes();
    resolveNeutralTypes();
    resolveImplicitLevels();
    // We're done with the types.  Let the GC clean up.
    types = null;
    reinsertFormattingCodes();
    // After resolving the implicit levels, the number
    // of runs may have changed.
    computeRuns();
  }

  /**
   * Return true if the paragraph base embedding is left-to-right,
   * false otherwise.
   */
  public boolean baseIsLeftToRight()
  {
    return baseEmbedding == DIRECTION_LEFT_TO_RIGHT;
  }

  /**
   * Create a new Bidi object for a single line of text, taken
   * from the text used when creating the current Bidi object.
   * @param start the index of the first character of the line
   * @param end the index of the final character of the line
   * @return a new Bidi object for the indicated line of text
   */
  public Bidi createLineBidi(int start, int end)
  {
    // This isn't the most efficient implementation possible.
    // This probably does not matter, so we choose simplicity instead.
    int level = getLevelAt(start);
    int flag = (((level % 2) == 0)
                ? DIRECTION_LEFT_TO_RIGHT
                : DIRECTION_RIGHT_TO_LEFT);
    return new Bidi(text, textOffset + start,
                    embeddings, embeddingOffset + start,
                    end - start, flag);
  }

  /**
   * Return the base embedding level of the paragraph.
   */
  public int getBaseLevel()
  {
    return baseEmbedding;
  }

  /**
   * Return the length of the paragraph, in characters.
   */
  public int getLength()
  {
    return length;
  }

  /**
   * Return the level at the indicated character.  If the
   * supplied index is less than zero or greater than the length
   * of the text, then the paragraph's base embedding level will
   * be returned.
   * @param offset the character to examine
   * @return the level of that character
   */
  public int getLevelAt(int offset)
  {
    if (offset < 0 || offset >= length)
      return getBaseLevel();
    return levels[offset];
  }

  /**
   * Return the number of runs in the result.  A run is
   * a sequence of characters at the same embedding level.
   */
  public int getRunCount()
  {
    return runs.length;
  }

  /**
   * Return the level of the indicated run.
   * @param which the run to examine
   * @return the level of that run
   */
  public int getRunLevel(int which)
  {
    return levels[runs[which]];
  }

  /**
   * Return the index of the character just following the end
   * of the indicated run.
   * @param which the run to examine
   * @return the index of the character after the final character
   * of the run
   */
  public int getRunLimit(int which)
  {
    if (which == runs.length - 1)
      return length;
    return runs[which + 1];
  }

  /**
   * Return the index of the first character in the indicated run.
   * @param which the run to examine
   * @return the index of the first character of the run
   */
  public int getRunStart(int which)
  {
    return runs[which];
  }

  /**
   * Return true if the text is entirely left-to-right, and the
   * base embedding is also left-to-right.
   */
  public boolean isLeftToRight()
  {
    return resultFlags == LTOR;
  }

  /**
   * Return true if the text consists of mixed left-to-right and
   * right-to-left runs, or if the text consists of one kind of run
   * which differs from the base embedding direction.
   */
  public boolean isMixed()
  {
    return resultFlags == (LTOR | RTOL);
  }

  /**
   * Return true if the text is entirely right-to-left, and the
   * base embedding is also right-to-left.
   */
  public boolean isRightToLeft()
  {
    return resultFlags == RTOL;
  }

  /**
   * Return a String describing the internal state of this object.
   * This is only useful for debugging.
   */
  public String toString()
  {
    return "Bidi Bidi Bidi I like you, Buck!";
  }

  /**
   * Reorder objects according to the levels passed in.  This implements
   * reordering as defined by the Unicode bidirectional layout specification.
   * The levels are integers from 0 to 62; even numbers represent left-to-right
   * runs, and odd numbers represent right-to-left runs.
   *
   * @param levels the levels associated with each object
   * @param levelOffset the index of the first level to use
   * @param objs the objects to reorder according to the levels
   * @param objOffset the index of the first object to use
   * @param count the number of objects (and levels) to manipulate
   */
  public static void reorderVisually(byte[] levels, int levelOffset,
                                     Object[] objs, int objOffset, int count)
  {
    // We need a copy of the 'levels' array, as we are going to modify it.
    // This is unfortunate but difficult to avoid.
    byte[] levelCopy = new byte[count];
    // Do this explicitly so we can also find the maximum depth at the
    // same time.
    int max = 0;
    int lowestOdd = 63;
    for (int i = 0; i < count; ++i)
      {
        levelCopy[i] = levels[levelOffset + i];
        max = Math.max(levelCopy[i], max);
        if (levelCopy[i] % 2 != 0)
          lowestOdd = Math.min(lowestOdd, levelCopy[i]);
      }

    // Reverse the runs starting with the deepest.
    for (int depth = max; depth >= lowestOdd; --depth)
      {
        int start = 0;
        while (start < count)
          {
            // Find the start of a run >= DEPTH.
            while (start < count && levelCopy[start] < depth)
              ++start;
            if (start == count)
              break;
            // Find the end of the run.
            int end = start + 1;
            while (end < count && levelCopy[end] >= depth)
              ++end;

            // Reverse this run.
            for (int i = 0; i < (end - start) / 2; ++i)
              {
                byte tmpb = levelCopy[end - i - 1];
                levelCopy[end - i - 1] = levelCopy[start + i];
                levelCopy[start + i] = tmpb;
                Object tmpo = objs[objOffset + end - i - 1];
                objs[objOffset + end - i - 1] = objs[objOffset + start + i];
                objs[objOffset + start + i] = tmpo;
              }

            // Handle the next run.
            start = end + 1;
          }
      }
  }

  /**
   * Returns false if all characters in the text between start and end
   * are all left-to-right text. This implementation is just calls
   * <code>Character.getDirectionality(char)</code> on all characters
   * and makes sure all characters are either explicitly left-to-right
   * or neutral in directionality (character types L, EN, ES, ET, AN,
   * CS, S and WS).
   */
  public static boolean requiresBidi(char[] text, int start, int end)
  {
    for (int i = start; i < end; i++)
      {
        byte dir = Character.getDirectionality(text[i]);
        if (dir != Character.DIRECTIONALITY_LEFT_TO_RIGHT
            && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER
            && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR
            && dir != Character.DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR
            && dir != Character.DIRECTIONALITY_ARABIC_NUMBER
            && dir != Character.DIRECTIONALITY_COMMON_NUMBER_SEPARATOR
            && dir != Character.DIRECTIONALITY_SEGMENT_SEPARATOR
            && dir != Character.DIRECTIONALITY_WHITESPACE
            && dir != Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR)
          return true;
      }

    return false;
  }
}

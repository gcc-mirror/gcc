/* Matcher.java -- Instance of a regular expression applied to a char sequence.
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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


package java.util.regex;

import gnu.java.lang.CPStringBuilder;

import gnu.java.util.regex.CharIndexed;
import gnu.java.util.regex.RE;
import gnu.java.util.regex.REMatch;

/**
 * Instance of a regular expression applied to a char sequence.
 *
 * @since 1.4
 */
public final class Matcher implements MatchResult
{
  private Pattern pattern;
  private CharSequence input;
  // We use CharIndexed as an input object to the getMatch method in order
  // that /\G/ (the end of the previous match) may work.  The information
  // of the previous match is stored in the CharIndexed object.
  private CharIndexed inputCharIndexed;
  private int position;
  private int appendPosition;
  private REMatch match;

  /**
   * The start of the region of the input on which to match.
   */
  private int regionStart;

  /**
   * The end of the region of the input on which to match.
   */
  private int regionEnd;

  /**
   * True if the match process should look beyond the
   * region marked by regionStart to regionEnd when
   * performing lookAhead, lookBehind and boundary
   * matching.
   */
  private boolean transparentBounds;

  /**
   * The flags that affect the anchoring bounds.
   * If {@link #hasAnchoringBounds()} is {@code true},
   * the match process will honour the
   * anchoring bounds: ^, \A, \Z, \z and $.  If
   * {@link #hasAnchoringBounds()} is {@code false},
   * the anchors are ignored and appropriate flags,
   * stored in this variable, are used to provide this
   * behaviour.
   */
  private int anchoringBounds;

  Matcher(Pattern pattern, CharSequence input)
  {
    this.pattern = pattern;
    this.input = input;
    this.inputCharIndexed = RE.makeCharIndexed(input, 0);
    regionStart = 0;
    regionEnd = input.length();
    transparentBounds = false;
    anchoringBounds = 0;
  }

  /**
   * Changes the pattern used by the {@link Matcher} to
   * the one specified.  Existing match information is lost,
   * but the input and the matcher's position within it is
   * retained.
   *
   * @param newPattern the new pattern to use.
   * @return this matcher.
   * @throws IllegalArgumentException if {@code newPattern} is
   *                                  {@code null}.
   * @since 1.5
   */
  public Matcher usePattern(Pattern newPattern)
  {
    if (newPattern == null)
      throw new IllegalArgumentException("The new pattern was null.");
    pattern = newPattern;
    match = null;

    return this;
  }

  /**
   * @param sb The target string buffer
   * @param replacement The replacement string
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public Matcher appendReplacement (StringBuffer sb, String replacement)
    throws IllegalStateException
  {
    assertMatchOp();
    sb.append(input.subSequence(appendPosition,
                                match.getStartIndex()).toString());
    sb.append(RE.getReplacement(replacement, match,
        RE.REG_REPLACE_USE_BACKSLASHESCAPE));
    appendPosition = match.getEndIndex();
    return this;
  }

  /**
   * @param sb The target string buffer
   */
  public StringBuffer appendTail (StringBuffer sb)
  {
    sb.append(input.subSequence(appendPosition, input.length()).toString());
    return sb;
  }

  /**
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public int end ()
    throws IllegalStateException
  {
    assertMatchOp();
    return match.getEndIndex();
  }

  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public int end (int group)
    throws IllegalStateException
  {
    assertMatchOp();
    return match.getEndIndex(group);
  }

  public boolean find ()
  {
    boolean first = (match == null);
    if (transparentBounds || (regionStart == 0 && regionEnd == input.length()))
      match = pattern.getRE().getMatch(inputCharIndexed, position, anchoringBounds);
    else
      match = pattern.getRE().getMatch(input.subSequence(regionStart, regionEnd),
                                       position, anchoringBounds);
    if (match != null)
      {
        int endIndex = match.getEndIndex();
        // Is the match within input limits?
        if (endIndex > input.length())
          {
            match = null;
            return false;
          }
        // Are we stuck at the same position?
        if (!first && endIndex == position)
          {
            match = null;
            // Not at the end of the input yet?
            if (position < input.length() - 1)
              {
                position++;
                return find(position);
              }
            else
              return false;
          }
        position = endIndex;
        return true;
      }
    return false;
  }

  /**
   * @param start The index to start the new pattern matching
   *
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public boolean find (int start)
  {
    if (transparentBounds || (regionStart == 0 && regionEnd == input.length()))
      match = pattern.getRE().getMatch(inputCharIndexed, start, anchoringBounds);
    else
      match = pattern.getRE().getMatch(input.subSequence(regionStart, regionEnd),
                                       start, anchoringBounds);
    if (match != null)
      {
        position = match.getEndIndex();
        return true;
      }
    return false;
  }

  /**
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public String group ()
  {
    assertMatchOp();
    return match.toString();
  }

  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public String group (int group)
    throws IllegalStateException
  {
    assertMatchOp();
    return match.toString(group);
  }

  /**
   * @param replacement The replacement string
   */
  public String replaceFirst (String replacement)
  {
    reset();
    // Semantics might not quite match
    return pattern.getRE().substitute(input, replacement, position,
        RE.REG_REPLACE_USE_BACKSLASHESCAPE);
  }

  /**
   * @param replacement The replacement string
   */
  public String replaceAll (String replacement)
  {
    reset();
    return pattern.getRE().substituteAll(input, replacement, position,
        RE.REG_REPLACE_USE_BACKSLASHESCAPE);
  }

  public int groupCount ()
  {
    return pattern.getRE().getNumSubs();
  }

  public boolean lookingAt ()
  {
    if (transparentBounds || (regionStart == 0 && regionEnd == input.length()))
      match = pattern.getRE().getMatch(inputCharIndexed, regionStart,
                                       anchoringBounds|RE.REG_FIX_STARTING_POSITION|RE.REG_ANCHORINDEX);
    else
      match = pattern.getRE().getMatch(input.subSequence(regionStart, regionEnd), 0,
                                       anchoringBounds|RE.REG_FIX_STARTING_POSITION);
    if (match != null)
      {
        if (match.getStartIndex() == 0)
          {
            position = match.getEndIndex();
            return true;
          }
        match = null;
      }
    return false;
  }

  /**
   * Attempts to match the entire input sequence against the pattern.
   *
   * If the match succeeds then more information can be obtained via the
   * start, end, and group methods.
   *
   * @see #start()
   * @see #end()
   * @see #group()
   */
  public boolean matches ()
  {
    if (transparentBounds || (regionStart == 0 && regionEnd == input.length()))
      match = pattern.getRE().getMatch(inputCharIndexed, regionStart,
                                       anchoringBounds|RE.REG_TRY_ENTIRE_MATCH|RE.REG_FIX_STARTING_POSITION|RE.REG_ANCHORINDEX);
    else
      match = pattern.getRE().getMatch(input.subSequence(regionStart, regionEnd), 0,
                                       anchoringBounds|RE.REG_TRY_ENTIRE_MATCH|RE.REG_FIX_STARTING_POSITION);
    if (match != null)
      {
        if (match.getStartIndex() == 0)
          {
            position = match.getEndIndex();
            if (position == input.length())
                return true;
          }
        match = null;
      }
    return false;
  }

  /**
   * Returns the Pattern that is interpreted by this Matcher
   */
  public Pattern pattern ()
  {
    return pattern;
  }

  /**
   * Resets the internal state of the matcher, including
   * resetting the region to its default state of encompassing
   * the whole input.  The state of {@link #hasTransparentBounds()}
   * and {@link #hasAnchoringBounds()} are unaffected.
   *
   * @return a reference to this matcher.
   * @see #regionStart()
   * @see #regionEnd()
   * @see #hasTransparentBounds()
   * @see #hasAnchoringBounds()
   */
  public Matcher reset ()
  {
    position = 0;
    match = null;
    regionStart = 0;
    regionEnd = input.length();
    appendPosition = 0;
    return this;
  }

  /**
   * Resets the internal state of the matcher, including
   * resetting the region to its default state of encompassing
   * the whole input.  The state of {@link #hasTransparentBounds()}
   * and {@link #hasAnchoringBounds()} are unaffected.
   *
   * @param input The new input character sequence.
   * @return a reference to this matcher.
   * @see #regionStart()
   * @see #regionEnd()
   * @see #hasTransparentBounds()
   * @see #hasAnchoringBounds()
   */
  public Matcher reset (CharSequence input)
  {
    this.input = input;
    this.inputCharIndexed = RE.makeCharIndexed(input, 0);
    return reset();
  }

  /**
   * @return the index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   */
  public int start ()
    throws IllegalStateException
  {
    assertMatchOp();
    return match.getStartIndex();
  }

  /**
   * @param group The index of a capturing group in this matcher's pattern
   *
   * @exception IllegalStateException If no match has yet been attempted,
   * or if the previous match operation failed
   * @exception IndexOutOfBoundsException If the replacement string refers
   * to a capturing group that does not exist in the pattern
   */
  public int start (int group)
    throws IllegalStateException
  {
    assertMatchOp();
    return match.getStartIndex(group);
  }

  /**
   * @return True if and only if the matcher hit the end of input.
   * @since 1.5
   */
  public boolean hitEnd()
  {
    return inputCharIndexed.hitEnd();
  }

  /**
   * @return A string expression of this matcher.
   */
  public String toString()
  {
    CPStringBuilder sb = new CPStringBuilder();
    sb.append(this.getClass().getName())
      .append("[pattern=").append(pattern.pattern())
      .append(" region=").append(regionStart).append(",").append(regionEnd)
      .append(" anchoringBounds=").append(anchoringBounds == 0)
      .append(" transparentBounds=").append(transparentBounds)
      .append(" lastmatch=").append(match == null ? "" : match.toString())
      .append("]");
    return sb.toString();
  }

  private void assertMatchOp()
  {
    if (match == null) throw new IllegalStateException();
  }

  /**
   * <p>
   * Defines the region of the input on which to match.
   * By default, the {@link Matcher} attempts to match
   * the whole string (from 0 to the length of the input),
   * but a region between {@code start} (inclusive) and
   * {@code end} (exclusive) on which to match may instead
   * be defined using this method.
   * </p>
   * <p>
   * The behaviour of region matching is further affected
   * by the use of transparent or opaque bounds (see
   * {@link #useTransparentBounds(boolean)}) and whether or not
   * anchors ({@code ^} and {@code $}) are in use
   * (see {@link #useAnchoringBounds(boolean)}).  With transparent
   * bounds, the matcher is aware of input outside the bounds
   * set by this method, whereas, with opaque bounds (the default)
   * only the input within the bounds is used.  The use of
   * anchors are affected by this setting; with transparent
   * bounds, anchors will match the beginning of the real input,
   * while with opaque bounds they match the beginning of the
   * region.  {@link #useAnchoringBounds(boolean)} can be used
   * to turn on or off the matching of anchors.
   * </p>
   *
   * @param start the start of the region (inclusive).
   * @param end the end of the region (exclusive).
   * @return a reference to this matcher.
   * @throws IndexOutOfBoundsException if either {@code start} or
   *                                   {@code end} are less than zero,
   *                                   if either {@code start} or
   *                                   {@code end} are greater than the
   *                                   length of the input, or if
   *                                   {@code start} is greater than
   *                                   {@code end}.
   * @see #regionStart()
   * @see #regionEnd()
   * @see #hasTransparentBounds()
   * @see #useTransparentBounds(boolean)
   * @see #hasAnchoringBounds()
   * @see #useAnchoringBounds(boolean)
   * @since 1.5
   */
  public Matcher region(int start, int end)
  {
    int length = input.length();
    if (start < 0)
      throw new IndexOutOfBoundsException("The start position was less than zero.");
    if (start >= length)
      throw new IndexOutOfBoundsException("The start position is after the end of the input.");
    if (end < 0)
      throw new IndexOutOfBoundsException("The end position was less than zero.");
    if (end > length)
      throw new IndexOutOfBoundsException("The end position is after the end of the input.");
    if (start > end)
      throw new IndexOutOfBoundsException("The start position is after the end position.");
    reset();
    regionStart = start;
    regionEnd = end;
    return this;
  }

  /**
   * The start of the region on which to perform matches (inclusive).
   *
   * @return the start index of the region.
   * @see #region(int,int)
   * #see #regionEnd()
   * @since 1.5
   */
  public int regionStart()
  {
    return regionStart;
  }

  /**
   * The end of the region on which to perform matches (exclusive).
   *
   * @return the end index of the region.
   * @see #region(int,int)
   * @see #regionStart()
   * @since 1.5
   */
  public int regionEnd()
  {
    return regionEnd;
  }

  /**
   * Returns true if the bounds of the region marked by
   * {@link #regionStart()} and {@link #regionEnd()} are
   * transparent.  When these bounds are transparent, the
   * matching process can look beyond them to perform
   * lookahead, lookbehind and boundary matching operations.
   * By default, the bounds are opaque.
   *
   * @return true if the bounds of the matching region are
   *         transparent.
   * @see #useTransparentBounds(boolean)
   * @see #region(int,int)
   * @see #regionStart()
   * @see #regionEnd()
   * @since 1.5
   */
  public boolean hasTransparentBounds()
  {
    return transparentBounds;
  }

  /**
   * Sets the transparency of the bounds of the region
   * marked by {@link #regionStart()} and {@link #regionEnd()}.
   * A value of {@code true} makes the bounds transparent,
   * so the matcher can see beyond them to perform lookahead,
   * lookbehind and boundary matching operations.  A value
   * of {@code false} (the default) makes the bounds opaque,
   * restricting the match to the input region denoted
   * by {@link #regionStart()} and {@link #regionEnd()}.
   *
   * @param transparent true if the bounds should be transparent.
   * @return a reference to this matcher.
   * @see #hasTransparentBounds()
   * @see #region(int,int)
   * @see #regionStart()
   * @see #regionEnd()
   * @since 1.5
   */
  public Matcher useTransparentBounds(boolean transparent)
  {
    transparentBounds = transparent;
    return this;
  }

  /**
   * Returns true if the matcher will honour the use of
   * the anchoring bounds: {@code ^}, {@code \A}, {@code \Z},
   * {@code \z} and {@code $}.  By default, the anchors
   * are used.  Note that the effect of the anchors is
   * also affected by {@link #hasTransparentBounds()}.
   *
   * @return true if the matcher will attempt to match
   *         the anchoring bounds.
   * @see #useAnchoringBounds(boolean)
   * @see #hasTransparentBounds()
   * @since 1.5
   */
  public boolean hasAnchoringBounds()
  {
    return anchoringBounds == 0;
  }

  /**
   * Enables or disables the use of the anchoring bounds:
   * {@code ^}, {@code \A}, {@code \Z}, {@code \z} and
   * {@code $}. By default, their use is enabled.  When
   * disabled, the matcher will not attempt to match
   * the anchors.
   *
   * @param useAnchors true if anchoring bounds should be used.
   * @return a reference to this matcher.
   * @since 1.5
   * @see #hasAnchoringBounds()
   */
  public Matcher useAnchoringBounds(boolean useAnchors)
  {
    if (useAnchors)
      anchoringBounds = 0;
    else
      anchoringBounds = RE.REG_NOTBOL|RE.REG_NOTEOL;
    return this;
  }

  /**
   * Returns a read-only snapshot of the current state of
   * the {@link Matcher} as a {@link MatchResult}.  Any
   * subsequent changes to this instance are not reflected
   * in the returned {@link MatchResult}.
   *
   * @return a {@link MatchResult} instance representing the
   *         current state of the {@link Matcher}.
   */
  public MatchResult toMatchResult()
  {
    Matcher snapshot = new Matcher(pattern, input);
    if (match != null)
      snapshot.match = (REMatch) match.clone();
    return snapshot;
  }

  /**
   * Returns a literalized string of s where characters {@code $} and {@code
   * \\} are escaped.
   *
   * @param s the string to literalize.
   * @return the literalized string.
   * @since 1.5
   */
  public static String quoteReplacement(String s)
  {
    if (s == null)
      throw new NullPointerException();
    CPStringBuilder sb = new CPStringBuilder();
    for (int i = 0; i < s.length(); i++)
    {
      char ch = s.charAt(i);
      if (ch == '$' || ch == '\\')
        sb.append('\\');
      sb.append(ch);
    }
    return sb.toString();
  }

}

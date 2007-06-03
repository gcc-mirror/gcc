/* StringTokenizer -- breaks a String into tokens
   Copyright (C) 1998, 1999, 2001, 2002, 2005  Free Software Foundation, Inc.

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


package java.util;

/**
 * This class splits a string into tokens.  The caller can set on which
 * delimiters the string should be split and if the delimiters should be
 * returned. This is much simpler than {@link java.io.StreamTokenizer}.
 *
 * <p>You may change the delimiter set on the fly by calling
 * nextToken(String).  But the semantic is quite difficult; it even
 * depends on calling <code>hasMoreTokens()</code>.  You should call
 * <code>hasMoreTokens()</code> before, otherwise the old delimiters
 * after the last token are candidates for being returned.
 *
 * <p>If you want to get the delimiters, you have to use the three argument
 * constructor.  The delimiters are returned as token consisting of a
 * single character.
 *
 * @author Jochen Hoenicke
 * @author Warren Levy (warrenl@cygnus.com)
 * @see java.io.StreamTokenizer
 * @status updated to 1.4
 */
public class StringTokenizer implements Enumeration<Object>
{
  // WARNING: StringTokenizer is a CORE class in the bootstrap cycle. See the
  // comments in vm/reference/java/lang/Runtime for implications of this fact.

  /**
   * The position in the str, where we currently are.
   */
  private int pos;

  /**
   * The string that should be split into tokens.
   */
  private final String str;

  /**
   * The length of the string.
   */
  private final int len;

  /**
   * The string containing the delimiter characters.
   */
  private String delim;

  /**
   * Tells, if we should return the delimiters.
   */
  private final boolean retDelims;

  /**
   * Creates a new StringTokenizer for the string <code>str</code>,
   * that should split on the default delimiter set (space, tab,
   * newline, return and formfeed), and which doesn't return the
   * delimiters.
   *
   * @param str The string to split
   * @throws NullPointerException if str is null
   */
  public StringTokenizer(String str)
  {
    this(str, " \t\n\r\f", false);
  }

  /**
   * Create a new StringTokenizer, that splits the given string on
   * the given delimiter characters.  It doesn't return the delimiter
   * characters.
   *
   * @param str the string to split
   * @param delim a string containing all delimiter characters
   * @throws NullPointerException if either argument is null
   */
  public StringTokenizer(String str, String delim)
  {
    this(str, delim, false);
  }

  /**
   * Create a new StringTokenizer, that splits the given string on
   * the given delimiter characters.  If you set
   * <code>returnDelims</code> to <code>true</code>, the delimiter
   * characters are returned as tokens of their own.  The delimiter
   * tokens always consist of a single character.
   *
   * @param str the string to split
   * @param delim a string containing all delimiter characters
   * @param returnDelims tells, if you want to get the delimiters
   * @throws NullPointerException if str or delim is null
   */
  public StringTokenizer(String str, String delim, boolean returnDelims)
  {
    len = str.length();
    this.str = str;
    this.delim = delim;
    this.retDelims = returnDelims;
    this.pos = 0;
  }

  /**
   * Tells if there are more tokens.
   *
   * @return true if the next call of nextToken() will succeed
   */
  public boolean hasMoreTokens()
  {
    if (! retDelims)
      {
        while (pos < len && delim.indexOf(str.charAt(pos)) >= 0)
          pos++;
      }
    return pos < len;
  }

  /**
   * Returns the nextToken, changing the delimiter set to the given
   * <code>delim</code>.  The change of the delimiter set is
   * permanent, ie. the next call of nextToken(), uses the same
   * delimiter set.
   *
   * @param delim a string containing the new delimiter characters
   * @return the next token with respect to the new delimiter characters
   * @throws NoSuchElementException if there are no more tokens
   * @throws NullPointerException if delim is null
   */
  public String nextToken(String delim) throws NoSuchElementException
  {
    this.delim = delim;
    return nextToken();
  }

  /**
   * Returns the nextToken of the string.
   *
   * @return the next token with respect to the current delimiter characters
   * @throws NoSuchElementException if there are no more tokens
   */
  public String nextToken() throws NoSuchElementException
  {
    if (pos < len && delim.indexOf(str.charAt(pos)) >= 0)
      {
        if (retDelims)
          return str.substring(pos, ++pos);
        while (++pos < len && delim.indexOf(str.charAt(pos)) >= 0)
          ;
      }
    if (pos < len)
      {
        int start = pos;
        while (++pos < len && delim.indexOf(str.charAt(pos)) < 0)
          ;
        
        return str.substring(start, pos);
      }
    throw new NoSuchElementException();
  }

  /**
   * This does the same as hasMoreTokens. This is the
   * <code>Enumeration</code> interface method.
   *
   * @return true, if the next call of nextElement() will succeed
   * @see #hasMoreTokens()
   */
  public boolean hasMoreElements()
  {
    return hasMoreTokens();
  }

  /**
   * This does the same as nextTokens. This is the
   * <code>Enumeration</code> interface method.
   *
   * @return the next token with respect to the current delimiter characters
   * @throws NoSuchElementException if there are no more tokens
   * @see #nextToken()
   */
  public Object nextElement() throws NoSuchElementException
  {
    return nextToken();
  }

  /**
   * This counts the number of remaining tokens in the string, with
   * respect to the current delimiter set.
   *
   * @return the number of times <code>nextTokens()</code> will succeed
   * @see #nextToken()
   */
  public int countTokens()
  {
    int count = 0;
    int delimiterCount = 0;
    boolean tokenFound = false; // Set when a non-delimiter is found
    int tmpPos = pos;

    // Note for efficiency, we count up the delimiters rather than check
    // retDelims every time we encounter one.  That way, we can
    // just do the conditional once at the end of the method
    while (tmpPos < len)
      {
        if (delim.indexOf(str.charAt(tmpPos++)) >= 0)
          {
            if (tokenFound)
              {
                // Got to the end of a token
                count++;
                tokenFound = false;
              }
            delimiterCount++; // Increment for this delimiter
          }
        else
          {
            tokenFound = true;
            // Get to the end of the token
            while (tmpPos < len
                   && delim.indexOf(str.charAt(tmpPos)) < 0)
              ++tmpPos;
          }
      }

    // Make sure to count the last token
    if (tokenFound)
      count++;

    // if counting delmiters add them into the token count
    return retDelims ? count + delimiterCount : count;
  }
} // class StringTokenizer

/* StringCharacterIterator.java -- Iterate over a character range in a string
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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

/**
 * This class iterates over a range of characters in a <code>String</code>.
 * For a given range of text, a beginning and ending index,
 * as well as a current index are defined.  These values can be queried
 * by the methods in this interface.  Additionally, various methods allow
 * the index to be set.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 */
public final class StringCharacterIterator implements CharacterIterator
{
  /**
   * This is the string to iterate over
   */
  private String text;

  /**
   * This is the value of the start position of the text range.
   */
  private int begin;

  /**
   * This is the value of the ending position of the text range.
   */
  private int end;

  /**
   * This is the current value of the scan index.
   */
  private int index;

  /**
   * This method initializes a new instance of
   * <code>StringCharacterIterator</code> to iterate over the entire
   * text of the specified <code>String</code>.  The initial index
   * value will be set to the first character in the string.
   *
   * @param text The <code>String</code> to iterate through (<code>null</code>
   *             not permitted).
   *
   * @throws NullPointerException if <code>text</code> is <code>null</code>.
   */
  public StringCharacterIterator (String text)
  {
    this (text, 0, text.length (), 0);
  }

  /*************************************************************************/

  /**
   * This method initializes a new instance of
   * <code>StringCharacterIterator</code> to iterate over the entire
   * text of the specified <code>String</code>.  The initial index
   * value will be set to the specified value.
   *
   * @param text The <code>String</code> to iterate through.
   * @param index The initial index position.
   */
  public StringCharacterIterator (String text, int index)
  {
    this (text, 0, text.length (), index);
  }

  /*************************************************************************/

  /**
   * This method initializes a new instance of
   * <code>StringCharacterIterator</code> that iterates over the text
   * in a subrange of the specified <code>String</code>.  The
   * beginning and end of the range are specified by the caller, as is
   * the initial index position.
   *
   * @param text The <code>String</code> to iterate through.
   * @param begin The beginning position in the character range.
   * @param end The ending position in the character range.
   * @param index The initial index position.
   *
   * @throws IllegalArgumentException If any of the range values are
   * invalid.
   */
  public StringCharacterIterator (String text, int begin, int end, int index)
  {
    int len = text.length ();

    if ((begin < 0) || (begin > len))
      throw new IllegalArgumentException ("Bad begin position");

    if ((end < begin) || (end > len))
      throw new IllegalArgumentException ("Bad end position");

    if ((index < begin) || (index > end))
      throw new IllegalArgumentException ("Bad initial index position");

    this.text = text;
    this.begin = begin;
    this.end = end;
    this.index = index;
  }

  /**
   * This is a package level constructor that copies the text out of
   * an existing StringCharacterIterator and resets the beginning and
   * ending index.
   *
   * @param sci The StringCharacterIterator to copy the info from
   * @param begin The beginning index of the range we are interested in.
   * @param end The ending index of the range we are interested in.
   */
  StringCharacterIterator (StringCharacterIterator sci, int begin, int end)
  {
    this (sci.text, begin, end, begin);
  }

  /**
   * This method returns the character at the current index position
   *
   * @return The character at the current index position.
   */
  public char current ()
  {
    return (index < end) ? text.charAt (index) : DONE;
  }

  /*************************************************************************/

  /**
   * This method increments the current index and then returns the
   * character at the new index value.  If the index is already at
   * <code>getEndIndex () - 1</code>, it will not be incremented.
   *
   * @return The character at the position of the incremented index
   * value, or <code>DONE</code> if the index has reached
   * getEndIndex () - 1.
   */
  public char next ()
  {
    if (index == end)
      return DONE;

    ++index;
    return current ();
  }

  /*************************************************************************/

  /**
   * This method decrements the current index and then returns the
   * character at the new index value.  If the index value is already
   * at the beginning index, it will not be decremented.
   *
   * @return The character at the position of the decremented index
   * value, or <code>DONE</code> if index was already equal to the
   * beginning index value.
   */
  public char previous ()
  {
    if (index == begin)
      return DONE;

    --index;
    return current ();
  }

  /*************************************************************************/

  /**
   * This method sets the index value to the beginning of the range and returns
   * the character there.
   *
   * @return The character at the beginning of the range, or
   * <code>DONE</code> if the range is empty.
   */
  public char first ()
  {
    index = begin;
    return current ();
  }

  /*************************************************************************/

  /**
   * This method sets the index value to <code>getEndIndex () - 1</code> and
   * returns the character there.  If the range is empty, then the index value
   * will be set equal to the beginning index.
   *
   * @return The character at the end of the range, or
   * <code>DONE</code> if the range is empty.
   */
  public char last ()
  {
    if (end == begin)
      return DONE;

    index = end - 1;
    return current ();
  }

  /*************************************************************************/

  /**
   * This method returns the current value of the index.
   *
   * @return The current index value
   */
  public int getIndex ()
  {
    return index;
  }

  /*************************************************************************/

  /**
   * This method sets the value of the index to the specified value, then
   * returns the character at that position.
   *
   * @param index The new index value.
   *
   * @return The character at the new index value or <code>DONE</code>
   * if the index value is equal to <code>getEndIndex</code>.
   *
   * @exception IllegalArgumentException If the specified index is not valid
   */
  public char setIndex (int index)
  {
    if ((index < begin) || (index > end))
      throw new IllegalArgumentException ("Bad index specified");

    this.index = index;
    return current ();
  }

  /*************************************************************************/

  /**
   * This method returns the character position of the first character in the
   * range.
   *
   * @return The index of the first character in the range.
   */
  public int getBeginIndex ()
  {
    return begin;
  }

  /*************************************************************************/

  /**
   * This method returns the character position of the end of the text range.
   * This will actually be the index of the first character following the
   * end of the range.  In the event the text range is empty, this will be
   * equal to the first character in the range.
   *
   * @return The index of the end of the range.
   */
  public int getEndIndex ()
  {
    return end;
  }

  /*************************************************************************/

  /**
   * This method creates a copy of this <code>CharacterIterator</code>.
   *
   * @return A copy of this <code>CharacterIterator</code>.
   */
  public Object clone ()
  {
    return new StringCharacterIterator (text, begin, end, index);
  }

  /*************************************************************************/

  /**
   * This method tests this object for equality againt the specified
   * object.  This will be true if and only if the specified object:
   * <p>
   * <ul>
   * <li>is not <code>null</code>.</li>
   * <li>is an instance of <code>StringCharacterIterator</code></li>
   * <li>has the same text as this object</li>
   * <li>has the same beginning, ending, and current index as this object.</li>
   * </ul>
   *
   * @param obj The object to test for equality against.
   *
   * @return <code>true</code> if the specified object is equal to this
   * object, <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof StringCharacterIterator))
      return false;

    StringCharacterIterator sci = (StringCharacterIterator) obj;

    return (begin == sci.begin
            && end == sci.end
            && index == sci.index
            && text.equals (sci.text));
  }

  /**
   * Return the hash code for this object.
   * @return the hash code
   */
  public int hashCode()
  {
    // Incorporate all the data in a goofy way.
    return begin ^ end ^ index ^ text.hashCode();
  }

  /*************************************************************************/

  /**
   * This method allows other classes in java.text to change the value
   * of the underlying text being iterated through.
   *
   * @param text The new <code>String</code> to iterate through.
   */
  public void setText (String text)
  {
    this.text = text;
    this.begin = 0;
    this.end = text.length ();
    this.index = 0;
  }
}

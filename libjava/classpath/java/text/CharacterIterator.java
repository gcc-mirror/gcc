/* CharacterIterator.java -- Iterate over a character range
   Copyright (C) 1998, 2001, 2005 Free Software Foundation, Inc.

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
  * This interface defines a mechanism for iterating over a range of
  * characters.  For a given range of text, a beginning and ending index,
  * as well as a current index are defined.  These values can be queried
  * by the methods in this interface.  Additionally, various methods allow
  * the index to be set. 
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface CharacterIterator extends Cloneable
{
  /**
   * This is a special constant value that is returned when the beginning or
   * end of the character range has been reached.
   */
  char DONE = '\uFFFF';

  /**
   * This method returns the character at the current index position
   *
   * @return The character at the current index position.
   */
  char current();

  /**
   * This method increments the current index and then returns the character
   * at the new index value.  If the index is already at 
   * <code>getEndIndex() - 1</code>, it will not be incremented.
   *
   * @return The character at the position of the incremented index value,
   * or {@link #DONE} if the index has reached getEndIndex() - 1
   */
  char next();

  /**
   * This method decrements the current index and then returns the character
   * at the new index value.  If the index value is already at the beginning
   * index, it will not be decremented.
   *
   * @return The character at the position of the decremented index value,
   *         or {@link #DONE} if index was already equal to the beginning index
   *         value.
   */
  char previous();

  /**
   * This method sets the index value to the beginning of the range and returns
   * the character there.
   *
   * @return The character at the beginning of the range, or {@link #DONE} if 
   *         the range is empty.
   */
  char first();

  /**
   * This method sets the index value to <code>getEndIndex() - 1</code> and
   * returns the character there.  If the range is empty, then the index value
   * will be set equal to the beginning index.
   *
   * @return The character at the end of the range, or {@link #DONE} if the 
   *         range is empty.
   */
  char last();  

  /**
   * This method returns the current value of the index.
   *
   * @return The current index value
   */
  int getIndex();

  /**
   * This method sets the value of the index to the specified value, then
   * returns the character at that position.
   *
   * @param index The new index value.
   *
   * @return The character at the new index value or {@link #DONE} if the index
   *         value is equal to {@link #getEndIndex()}.
   */
  char setIndex (int index) throws IllegalArgumentException;

  /**
   * This method returns the character position of the first character in the
   * range.
   *
   * @return The index of the first character in the range.
   */
  int getBeginIndex();

  /**
   * This method returns the character position of the end of the text range.
   * This will actually be the index of the first character following the
   * end of the range.  In the event the text range is empty, this will be
   * equal to the first character in the range.
   *
   * @return The index of the end of the range.
   */
  int getEndIndex();

  /**
   * This method creates a copy of this <code>CharacterIterator</code>.
   *
   * @return A copy of this <code>CharacterIterator</code>.
   */
  Object clone();

} // interface CharacterIterator

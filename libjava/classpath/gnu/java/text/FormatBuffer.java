/* FormatBuffer.java -- General interface to build attributed strings.
   Copyright (C) 2004 Free Software Foundation, Inc.

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
package gnu.java.text;

import java.text.AttributedCharacterIterator;
import java.util.HashMap;

/**
 * This interface describes a modifiable buffer which contains attributed
 * characters. The implementation may or may not implements attributes. It
 * aims to greatly simplify and clarify the implementation of java.text
 * formatters. The buffer may be appended or have its tail cut. It may also
 * be completely cleant up.
 *
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 * @date April 10, 2004
 */
public interface FormatBuffer
{
  /**
   * This method appends a simple string to the buffer. This part of
   * the buffer will be attributed using the default attribute.
   *
   * @param s The string to append to the buffer.
   */
  public void append(String s);

  /**
   * This method appends a simple string to the buffer. This part of
   * the buffer will have the specified attribute (and only this one).
   * The default attribute may be changed after calling this method.
   *
   * @param s The string to append to the buffer.
   * @param attr Attribute to use for the string in the buffer.
   */
  public void append(String s, AttributedCharacterIterator.Attribute attr);

  /**
   * This method appends a simple string to the buffer. This part of
   * the buffer will be attributed using the specified ranges and attributes.
   * To have an example on how to specify ranges see {@link gnu.java.text.FormatCharacterIterator}.
   *
   * @param s The string to append to the buffer.
   * @param ranges The ranges describing how the attributes should be applied
   * to the string.
   * @param attrs The attributes of the string in the buffer.
   */
  public void append(String s, int[] ranges, HashMap[] attrs);

  /**
   * This method appends a simple char to the buffer. This part of
   * the buffer will be attributed using the default attribute.
   *
   * @param c The character to append to the buffer.
   */
  public void append(char c);

  /**
   * This method appends a simple character to the buffer. This part of
   * the buffer will have the specified attribute (and only this one).
   * The default attribute may be changed after calling this method.
   *
   * @param c The character to append to the buffer.
   * @param attr Attribute to use for the character in the buffer.
   */
  public void append(char c, AttributedCharacterIterator.Attribute attr);

  /**
   * This method changes the current default attribute for the next string
   * or character which will be appended to the buffer.
   *
   * @param attr The attribute which will be used by default.
   */
  public void setDefaultAttribute(AttributedCharacterIterator.Attribute attr);

  /**
   * This method returns the current default attribute for the buffer.
   *
   * @return The default attribute for the buffer.
   */
  public AttributedCharacterIterator.Attribute getDefaultAttribute();

  /**
   * This method cuts the last characters of the buffer. The number of
   * characters to cut is given by "length".
   *
   * @param length Number of characters to cut at the end of the buffer.
   */
  public void cutTail(int length);

  /**
   * This method resets completely the buffer.
   */
  public void clear();

  /**
   * This method returns the number of character in the buffer.
   *
   * @return The number of character in the buffer.
   */
  public int length();
}

/* StringFormatBuffer.java -- Implements FormatBuffer using StringBuffer.
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
 * This class is an implementation of a FormatBuffer without attributes.
 * 
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 * @date April 10, 2004
 */
public class StringFormatBuffer implements FormatBuffer
{
  private final StringBuffer buffer;
  private AttributedCharacterIterator.Attribute defaultAttr;

  public StringFormatBuffer(int prebuffer)
  {
    buffer = new StringBuffer(prebuffer);
  }

  public StringFormatBuffer(StringBuffer buffer)
  {
    this.buffer = buffer;
  }

  public void append(String s)
  {
    buffer.append(s);
  }
  
  public void append(String s, AttributedCharacterIterator.Attribute attr)
  {
    buffer.append(s);
  }

  public void append(String s, int[] ranges, HashMap[] attrs)
  {
    buffer.append(s);
  }

  public void append(char c)
  {
    buffer.append(c);
  }

  public void append(char c, AttributedCharacterIterator.Attribute attr)
  {
    buffer.append(c);
  }

  public void setDefaultAttribute(AttributedCharacterIterator.Attribute attr)
  {
    defaultAttr = attr;
  }

  public AttributedCharacterIterator.Attribute getDefaultAttribute()
  {
    return defaultAttr;
  }

  public void cutTail(int length)
  {
    buffer.setLength(buffer.length()-length);
  }

  public int length()
  {
    return buffer.length();
  }

  public void clear()
  {
    buffer.setLength(0);
  }

  /**
   * This method returns the internal {@link java.lang.StringBuffer} which 
   * contains the string of character.
   */
  public StringBuffer getBuffer()
  {
    return buffer;
  }

  public String toString()
  {
    return buffer.toString();
  }

}

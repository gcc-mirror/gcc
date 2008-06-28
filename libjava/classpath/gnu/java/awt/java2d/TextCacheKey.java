/* TextCacheKey.java -- Key to use for caching texts with their rendered layout
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.java.awt.java2d;

import java.awt.Font;
import java.awt.font.FontRenderContext;

/**
 * A key object to be used when caching pre-rendered text.
 */
public class TextCacheKey
{

  /**
   * The actual string.
   */
  private String string;

  /**
   * The font render context.
   */
  private FontRenderContext fontRenderContext;

  /**
   * The font.
   */
  private Font font;

  /**
   * Creates a new TextCacheKey.
   *
   * This is intended to be used as search key. It is important to initialize
   * the values using the setter methods before using this key, otherwise
   * it will throw NPEs.
   */
  public TextCacheKey()
  {
    // No-arg constructor.
  }

  /**
   * Creates a new TextCacheKey with initial values.
   *
   * @param s the string
   * @param f the font
   * @param frc the font render context
   */
  public TextCacheKey(String s, Font f, FontRenderContext frc)
  {
    string = s;
    font = f;
    fontRenderContext = frc;
  }

  /**
   * Re-sets the string. This is intented to be used in search keys only.
   *
   * @param s the string to set
   */
  public void setString(String s)
  {
    string = s;
  }

  /**
   * Sets the font render context.
   * This is intented to be used in search keys only.
   *
   * @param frc the new font render context
   */
  public void setFontRenderContext(FontRenderContext frc)
  {
    fontRenderContext = frc;
  }

  /**
   * Sets the font.
   * This is intented to be used in search keys only.
   *
   * @param f the font to set
   */
  public void setFont(Font f)
  {
    font = f;
  }

  /**
   * Determines if two objects are equal.
   *
   * @see Object#equals(Object)
   */
  public boolean equals(Object o)
  {
    boolean eq;
    if (o instanceof TextCacheKey)
      {
        TextCacheKey other = (TextCacheKey) o;
        eq = other.string.equals(string)
             && other.font.equals(font)
             && other.fontRenderContext.equals(fontRenderContext);
      }
    else
      {
        eq = false;
      }
    return eq;
  }

  /**
   * Computes a hashcode for this key.
   *
   * @see Object#hashCode()
   */
  public int hashCode()
  {
    return string.hashCode() ^ font.hashCode() ^ fontRenderContext.hashCode();
  }
}

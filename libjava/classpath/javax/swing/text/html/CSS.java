/* CSS.java -- Provides CSS attributes
   Copyright (C) 2005 Free Software Foundation, Inc.

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

package javax.swing.text.html;

import java.io.Serializable;
import java.util.HashMap;

/**
 * Provides CSS attributes to be used by the HTML view classes. The constants
 * defined here are used as keys for text attributes for use in
 * {@link javax.swing.text.AttributeSet}s of {@link javax.swing.text.Element}s.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class CSS implements Serializable
{
  /**
   * Returns an array of all CSS attributes.
   *
   * @return All available CSS.Attribute objects.
   */
  public static CSS.Attribute[] getAllAttributeKeys()
  {
    Object[] src = Attribute.attributeMap.values().toArray();
    CSS.Attribute[] dst = new CSS.Attribute[ src.length ];
    System.arraycopy(src, 0, dst, 0, src.length);
    return dst;
  }

  /**
   * Returns an a given CSS attribute.
   *
   * @param name - The name of the attribute.
   * @return The CSS attribute with the given name, or <code>null</code> if
   * no attribute with that name exists.
   */
  public static CSS.Attribute getAttribute(String name)
  {
    return (CSS.Attribute)Attribute.attributeMap.get( name );
  }

  public static final class Attribute
  {
    /**
     * The CSS attribute 'background'.
     */
    public static final Attribute BACKGROUND =
      new Attribute("background", false, null);

    /**
     * The CSS attribute 'background-attachment'.
     */
    public static final Attribute BACKGROUND_ATTACHMENT =
      new Attribute("background-attachment", false, "scroll");

    /**
     * The CSS attribute 'background-color'.
     */
    public static final Attribute BACKGROUND_COLOR =
      new Attribute("background-color", false, "transparent");

    /**
     * The CSS attribute 'background-image'.
     */
    public static final Attribute BACKGROUND_IMAGE =
      new Attribute("background-image", false, "none");

    /**
     * The CSS attribute 'background-position'.
     */
    public static final Attribute BACKGROUND_POSITION =
      new Attribute("background-position", false, null);

    /**
     * The CSS attribute 'background-repeat'.
     */
    public static final Attribute BACKGROUND_REPEAT =
      new Attribute("background-repeat", false, "repeat");

    /**
     * The CSS attribute 'border'.
     */
    public static final Attribute BORDER = new Attribute("border", false, null);

    /**
     * The CSS attribute 'border-bottom'.
     */
    public static final Attribute BORDER_BOTTOM =
      new Attribute("border-bottom", false, null);

    /**
     * The CSS attribute 'border-bottom-width'.
     */
    public static final Attribute BORDER_BOTTOM_WIDTH =
      new Attribute("border-bottom-width", false, "medium");

    /**
     * The CSS attribute 'border-color'.
     */
    public static final Attribute BORDER_COLOR =
      new Attribute("border-color", false, "black");

    /**
     * The CSS attribute 'border-left'.
     */
    public static final Attribute BORDER_LEFT =
      new Attribute("border-left", false, null);

    /**
     * The CSS attribute 'border-left-width'.
     */
    public static final Attribute BORDER_LEFT_WIDTH =
      new Attribute("border-left-width", false, "medium");

    /**
     * The CSS attribute 'border-right'.
     */
    public static final Attribute BORDER_RIGHT =
      new Attribute("border-right", false, null);

    /**
     * The CSS attribute 'border-right-width'.
     */
    public static final Attribute BORDER_RIGHT_WIDTH =
      new Attribute("border-right-width", false, "medium");

    /**
     * The CSS attribute 'border-style'.
     */
    public static final Attribute BORDER_STYLE =
      new Attribute("border-style", false, "none");

    /**
     * The CSS attribute 'border-top'.
     */
    public static final Attribute BORDER_TOP =
      new Attribute("border-top", false, null);

    /**
     * The CSS attribute 'border-top-width'.
     */
    public static final Attribute BORDER_TOP_WIDTH =
      new Attribute("border-top-width", false, "medium");

    /**
     * The CSS attribute 'border-width'.
     */
    public static final Attribute BORDER_WIDTH =
      new Attribute("border-width", false, "medium");

    /**
     * The CSS attribute 'clear'.
     */
    public static final Attribute CLEAR = new Attribute("clear", false, "none");

    /**
     * The CSS attribute 'color'.
     */
    public static final Attribute COLOR = new Attribute("color", true, "black");

    /**
     * The CSS attribute 'display'.
     */
    public static final Attribute DISPLAY =
      new Attribute("display", false, "block");

    /**
     * The CSS attribute 'float'.
     */
    public static final Attribute FLOAT = new Attribute("float", false, "none");

    /**
     * The CSS attribute 'font'.
     */
    public static final Attribute FONT = new Attribute("font", true, null);

    /**
     * The CSS attribute 'font-family'.
     */
    public static final Attribute FONT_FAMILY =
      new Attribute("font-family", true, null);

    /**
     * The CSS attribute 'font-size'.
     */
    public static final Attribute FONT_SIZE =
      new Attribute("font-size", true, "medium");

    /**
     * The CSS attribute 'font-style'.
     */
    public static final Attribute FONT_STYLE =
      new Attribute("font-style", true, "normal");

    /**
     * The CSS attribute 'font-variant'.
     */
    public static final Attribute FONT_VARIANT =
      new Attribute("font-variant", true, "normal");

    /**
     * The CSS attribute 'font-weight'.
     */
    public static final Attribute FONT_WEIGHT =
      new Attribute("font-weight", true, "normal");

    /**
     * The CSS attribute 'height'.
     */
    public static final Attribute HEIGHT =
      new Attribute("height", false, "auto");

    /**
     * The CSS attribute 'letter-spacing'.
     */
    public static final Attribute LETTER_SPACING =
      new Attribute("letter-spacing", true, "normal");

    /**
     * The CSS attribute 'line-height'.
     */
    public static final Attribute LINE_HEIGHT =
      new Attribute("line-height", true, "normal");

    /**
     * The CSS attribute 'list-style'.
     */
    public static final Attribute LIST_STYLE =
      new Attribute("list-style", true, null);

    /**
     * The CSS attribute 'list-style-image'.
     */
    public static final Attribute LIST_STYLE_IMAGE =
      new Attribute("list-style-image", true, "none");

    /**
     * The CSS attribute 'list-style-position'.
     */
    public static final Attribute LIST_STYLE_POSITION =
      new Attribute("list-style-position", true, "outside");

    /**
     * The CSS attribute 'list-style-type'.
     */
    public static final Attribute LIST_STYLE_TYPE =
      new Attribute("list-style-type", true, "disc");

    /**
     * The CSS attribute 'margin'.
     */
    public static final Attribute MARGIN = new Attribute("margin", false, null);

    /**
     * The CSS attribute 'margin-bottom'.
     */
    public static final Attribute MARGIN_BOTTOM =
      new Attribute("margin-bottom", false, "0");

    /**
     * The CSS attribute 'margin-left'.
     */
    public static final Attribute MARGIN_LEFT =
      new Attribute("margin-left", false, "0");

    /**
     * The CSS attribute 'margin-right'.
     */
    public static final Attribute MARGIN_RIGHT =
      new Attribute("margin-right", false, "0");

    /**
     * The CSS attribute 'margin-top'.
     */
    public static final Attribute MARGIN_TOP =
      new Attribute("margin-top", false, "0");

    /**
     * The CSS attribute 'padding'.
     */
    public static final Attribute PADDING =
      new Attribute("padding", false, null);

    /**
     * The CSS attribute 'padding-bottom'.
     */
    public static final Attribute PADDING_BOTTOM =
      new Attribute("padding-bottom", false, "0");

    /**
     * The CSS attribute 'padding-left'.
     */
    public static final Attribute PADDING_LEFT =
      new Attribute("padding-left", false, "0");

    /**
     * The CSS attribute 'padding-right'.
     */
    public static final Attribute PADDING_RIGHT =
      new Attribute("padding-right", false, "0");

    /**
     * The CSS attribute 'padding-top'.
     */
    public static final Attribute PADDING_TOP =
      new Attribute("padding-top", false, "0");

    /**
     * The CSS attribute 'text-align'.
     */
    public static final Attribute TEXT_ALIGN =
      new Attribute("text-align", true, null);

    /**
     * The CSS attribute 'text-decoration'.
     */
    public static final Attribute TEXT_DECORATION =
      new Attribute("text-decoration", true, "none");

    /**
     * The CSS attribute 'text-indent'.
     */
    public static final Attribute TEXT_INDENT =
      new Attribute("text-indent", true, "0");

    /**
     * The CSS attribute 'text-transform'.
     */
    public static final Attribute TEXT_TRANSFORM =
      new Attribute("text-transform", true, "none");

    /**
     * The CSS attribute 'vertical-align'.
     */
    public static final Attribute VERTICAL_ALIGN =
      new Attribute("vertical-align", false, "baseline");

    /**
     * The CSS attribute 'white-space'.
     */
    public static final Attribute WHITE_SPACE =
      new Attribute("white-space", true, "normal");

    /**
     * The CSS attribute 'width'.
     */
    public static final Attribute WIDTH =
      new Attribute("width", false, "auto");

    /**
     * The CSS attribute 'word-spacing'.
     */
    public static final Attribute WORD_SPACING =
      new Attribute("word-spacing", true, "normal");

    /**
     * The attribute string.
     */
    String attStr;

    /**
     * Indicates if this attribute should be inherited from it's parent or
     * not.
     */
    boolean isInherited;

    /**
     * A default value for this attribute if one exists, otherwise null.
     */
    String defaultValue;

    /**
     * A HashMap of all attributes.
     */
    static HashMap attributeMap;

    /**
     * Creates a new Attribute instance with the specified values.
     *
     * @param attr the attribute string
     * @param inherited if the attribute should be inherited or not
     * @param def a default value; may be <code>null</code> 
     */
    Attribute(String attr, boolean inherited, String def)
    {
      attStr = attr;
      isInherited = inherited;
      defaultValue = def;
      if( attributeMap == null)
	attributeMap = new HashMap();
      attributeMap.put( attr, this );
    }

    /**
     * Returns the string representation of this attribute as specified
     * in the CSS specification.
     */
    public String toString()
    {
      return attStr;
    }

    /**
     * Returns <code>true</code> if the attribute should be inherited from
     * the parent, <code>false</code> otherwise.
     *
     * @return <code>true</code> if the attribute should be inherited from
     *         the parent, <code>false</code> otherwise
     */
    public boolean isInherited()
    {
      return isInherited;
    }

    /**
     * Returns the default value of this attribute if one exists,
     * <code>null</code> otherwise.
     *
     * @return the default value of this attribute if one exists,
     *         <code>null</code> otherwise
     */
    public String getDefaultValue()
    {
      return defaultValue;
    }
  }
}

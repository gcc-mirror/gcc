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

import gnu.javax.swing.text.html.css.BorderStyle;
import gnu.javax.swing.text.html.css.BorderWidth;
import gnu.javax.swing.text.html.css.CSSColor;
import gnu.javax.swing.text.html.css.FontSize;
import gnu.javax.swing.text.html.css.FontStyle;
import gnu.javax.swing.text.html.css.FontWeight;
import gnu.javax.swing.text.html.css.Length;

import java.io.Serializable;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.swing.text.MutableAttributeSet;

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

    // Some GNU Classpath specific extensions.
    static final Attribute BORDER_TOP_STYLE =
      new Attribute("border-top-style", false, null);
    static final Attribute BORDER_BOTTOM_STYLE =
      new Attribute("border-bottom-style", false, null);
    static final Attribute BORDER_LEFT_STYLE =
      new Attribute("border-left-style", false, null);
    static final Attribute BORDER_RIGHT_STYLE =
      new Attribute("border-right-style", false, null);
    static final Attribute BORDER_TOP_COLOR =
      new Attribute("border-top-color", false, null);
    static final Attribute BORDER_BOTTOM_COLOR =
      new Attribute("border-bottom-color", false, null);
    static final Attribute BORDER_LEFT_COLOR =
      new Attribute("border-left-color", false, null);
    static final Attribute BORDER_RIGHT_COLOR =
      new Attribute("border-right-color", false, null);
    static final Attribute BORDER_SPACING =
      new Attribute("border-spacing", false, null);
    static final Attribute POSITION =
      new Attribute("position", false, null);
    static final Attribute LEFT =
      new Attribute("left", false, null);
    static final Attribute RIGHT =
      new Attribute("right", false, null);
    static final Attribute TOP =
      new Attribute("top", false, null);
    static final Attribute BOTTOM =
      new Attribute("bottom", false, null);

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

  /**
   * Maps attribute values (String) to some converter class, based on the
   * key.
   *
   * @param att the key
   * @param v the value
   *
   * @return the wrapped value
   */
  static Object getValue(Attribute att, String v)
  {
    Object o;
    if (att == Attribute.FONT_SIZE)
      o = new FontSize(v);
    else if (att == Attribute.FONT_WEIGHT)
      o = new FontWeight(v);
    else if (att == Attribute.FONT_STYLE)
      o = new FontStyle(v);
    else if (att == Attribute.COLOR || att == Attribute.BACKGROUND_COLOR
             || att == Attribute.BORDER_COLOR
             || att == Attribute.BORDER_TOP_COLOR
             || att == Attribute.BORDER_BOTTOM_COLOR
             || att == Attribute.BORDER_LEFT_COLOR
             || att == Attribute.BORDER_RIGHT_COLOR)
      o = new CSSColor(v);
    else if (att == Attribute.MARGIN || att == Attribute.MARGIN_BOTTOM
             || att == Attribute.MARGIN_LEFT || att == Attribute.MARGIN_RIGHT
             || att == Attribute.MARGIN_TOP || att == Attribute.WIDTH
             || att == Attribute.HEIGHT
             || att == Attribute.PADDING || att == Attribute.PADDING_BOTTOM
             || att == Attribute.PADDING_LEFT || att == Attribute.PADDING_RIGHT
             || att == Attribute.PADDING_TOP
             || att == Attribute.LEFT || att == Attribute.RIGHT
             || att == Attribute.TOP || att == Attribute.BOTTOM)
      o = new Length(v);
    else if (att == Attribute.BORDER_WIDTH || att == Attribute.BORDER_TOP_WIDTH
             || att == Attribute.BORDER_LEFT_WIDTH
             || att == Attribute.BORDER_RIGHT_WIDTH
             || att == Attribute.BORDER_BOTTOM_WIDTH)
      o = new BorderWidth(v);
    else
      o = v;
    return o;
  }

  static void addInternal(MutableAttributeSet atts, Attribute a, String v)
  {
    if (a == Attribute.BACKGROUND)
      parseBackgroundShorthand(atts, v);
    else if (a == Attribute.PADDING)
      parsePaddingShorthand(atts, v);
    else if (a == Attribute.MARGIN)
      parseMarginShorthand(atts, v);
    else if (a == Attribute.BORDER || a == Attribute.BORDER_LEFT
             || a == Attribute.BORDER_RIGHT || a == Attribute.BORDER_TOP
             || a == Attribute.BORDER_BOTTOM)
      parseBorderShorthand(atts, v, a);
  }

  /**
   * Parses the background shorthand and translates it to more specific
   * background attributes.
   *
   * @param atts the attributes
   * @param v the value
   */
  private static void parseBackgroundShorthand(MutableAttributeSet atts,
                                               String v)
  {
    StringTokenizer tokens = new StringTokenizer(v, " ");
    while (tokens.hasMoreElements())
      {
        String token = tokens.nextToken();
        if (CSSColor.isValidColor(token))
          atts.addAttribute(Attribute.BACKGROUND_COLOR,
                            new CSSColor(token));
      }
  }

  /**
   * Parses the padding shorthand and translates to the specific padding
   * values.
   *
   * @param atts the attributes
   * @param v the actual value
   */
  private static void parsePaddingShorthand(MutableAttributeSet atts, String v)
  {
    StringTokenizer tokens = new StringTokenizer(v, " ");
    int numTokens = tokens.countTokens();
    if (numTokens == 1)
      {
        Length l = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.PADDING_BOTTOM, l);
        atts.addAttribute(Attribute.PADDING_LEFT, l);
        atts.addAttribute(Attribute.PADDING_RIGHT, l);
        atts.addAttribute(Attribute.PADDING_TOP, l);
      }
    else if (numTokens == 2)
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.PADDING_BOTTOM, l1);
        atts.addAttribute(Attribute.PADDING_TOP, l1);
        atts.addAttribute(Attribute.PADDING_LEFT, l2);
        atts.addAttribute(Attribute.PADDING_RIGHT, l2);
      }
    else if (numTokens == 3)
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        Length l3 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.PADDING_TOP, l1);
        atts.addAttribute(Attribute.PADDING_LEFT, l2);
        atts.addAttribute(Attribute.PADDING_RIGHT, l2);
        atts.addAttribute(Attribute.PADDING_BOTTOM, l3);
      }
    else
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        Length l3 = new Length(tokens.nextToken());
        Length l4 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.PADDING_TOP, l1);
        atts.addAttribute(Attribute.PADDING_RIGHT, l2);
        atts.addAttribute(Attribute.PADDING_BOTTOM, l3);
        atts.addAttribute(Attribute.PADDING_LEFT, l4);
      }
  }

  /**
   * Parses the margin shorthand and translates to the specific margin
   * values.
   *
   * @param atts the attributes
   * @param v the actual value
   */
  private static void parseMarginShorthand(MutableAttributeSet atts, String v)
  {
    StringTokenizer tokens = new StringTokenizer(v, " ");
    int numTokens = tokens.countTokens();
    if (numTokens == 1)
      {
        Length l = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.MARGIN_BOTTOM, l);
        atts.addAttribute(Attribute.MARGIN_LEFT, l);
        atts.addAttribute(Attribute.MARGIN_RIGHT, l);
        atts.addAttribute(Attribute.MARGIN_TOP, l);
      }
    else if (numTokens == 2)
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.MARGIN_BOTTOM, l1);
        atts.addAttribute(Attribute.MARGIN_TOP, l1);
        atts.addAttribute(Attribute.MARGIN_LEFT, l2);
        atts.addAttribute(Attribute.MARGIN_RIGHT, l2);
      }
    else if (numTokens == 3)
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        Length l3 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.MARGIN_TOP, l1);
        atts.addAttribute(Attribute.MARGIN_LEFT, l2);
        atts.addAttribute(Attribute.MARGIN_RIGHT, l2);
        atts.addAttribute(Attribute.MARGIN_BOTTOM, l3);
      }
    else
      {
        Length l1 = new Length(tokens.nextToken());
        Length l2 = new Length(tokens.nextToken());
        Length l3 = new Length(tokens.nextToken());
        Length l4 = new Length(tokens.nextToken());
        atts.addAttribute(Attribute.MARGIN_TOP, l1);
        atts.addAttribute(Attribute.MARGIN_RIGHT, l2);
        atts.addAttribute(Attribute.MARGIN_BOTTOM, l3);
        atts.addAttribute(Attribute.MARGIN_LEFT, l4);
      }
  }

  /**
   * Parses the CSS border shorthand attribute and translates it to the
   * more specific border attributes.
   *
   * @param atts the attribute
   * @param value the value
   */
  private static void parseBorderShorthand(MutableAttributeSet atts,
                                           String value, Attribute cssAtt)
  {
    StringTokenizer tokens = new StringTokenizer(value, " ");
    while (tokens.hasMoreTokens())
      {
        String token = tokens.nextToken();
        if (BorderStyle.isValidStyle(token))
          {
            if (cssAtt == Attribute.BORDER_LEFT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_LEFT_STYLE, token);
            if (cssAtt == Attribute.BORDER_RIGHT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_RIGHT_STYLE, token);
            if (cssAtt == Attribute.BORDER_BOTTOM || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_BOTTOM_STYLE, token);
            if (cssAtt == Attribute.BORDER_TOP || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_TOP_STYLE, token);
          }
        else if (BorderWidth.isValid(token))
          {
            BorderWidth w = new BorderWidth(token);
            if (cssAtt == Attribute.BORDER_LEFT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_LEFT_WIDTH, w);
            if (cssAtt == Attribute.BORDER_RIGHT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_RIGHT_WIDTH, w);
            if (cssAtt == Attribute.BORDER_BOTTOM || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_BOTTOM_WIDTH, w);
            if (cssAtt == Attribute.BORDER_TOP || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_TOP_WIDTH, w);
          }
        else if (CSSColor.isValidColor(token))
          {
            CSSColor c = new CSSColor(token);
            if (cssAtt == Attribute.BORDER_LEFT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_LEFT_COLOR, c);
            if (cssAtt == Attribute.BORDER_RIGHT || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_RIGHT_COLOR, c);
            if (cssAtt == Attribute.BORDER_BOTTOM || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_BOTTOM_COLOR, c);
            if (cssAtt == Attribute.BORDER_TOP || cssAtt == Attribute.BORDER)
              atts.addAttribute(Attribute.BORDER_TOP_COLOR, c);
          }
      }
  }
}

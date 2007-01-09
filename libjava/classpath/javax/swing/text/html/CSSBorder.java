/* CSSBorder.java -- A border for rendering CSS border styles
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import gnu.javax.swing.text.html.css.BorderWidth;
import gnu.javax.swing.text.html.css.CSSColor;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.border.Border;
import javax.swing.text.AttributeSet;

/**
 * A border implementation to render CSS border styles.
 */
class CSSBorder
  implements Border
{

  /**
   * The CSS border styles.
   */

  private static final int STYLE_NOT_SET = -1;
  private static final int STYLE_NONE = 0;
  private static final int STYLE_HIDDEN = 1;
  private static final int STYLE_DOTTED = 2;
  private static final int STYLE_DASHED = 3;
  private static final int STYLE_SOLID = 4;
  private static final int STYLE_DOUBLE = 5;
  private static final int STYLE_GROOVE = 6;
  private static final int STYLE_RIDGE = 7;
  private static final int STYLE_INSET = 8;
  private static final int STYLE_OUTSET = 9;

  /**
   * The left insets.
   */
  private int left;

  /**
   * The right insets.
   */
  private int right;

  /**
   * The top insets.
   */
  private int top;

  /**
   * The bottom insets.
   */
  private int bottom;

  /**
   * The border style on the left.
   */
  private int leftStyle;

  /**
   * The border style on the right.
   */
  private int rightStyle;

  /**
   * The border style on the top.
   */
  private int topStyle;

  /**
   * The color for the top border.
   */
  private Color topColor;

  /**
   * The color for the bottom border.
   */
  private Color bottomColor;

  /**
   * The color for the left border.
   */
  private Color leftColor;

  /**
   * The color for the right border.
   */
  private Color rightColor;

  /**
   * The border style on the bottom.
   */
  private int bottomStyle;

  /**
   * Creates a new CSS border and fetches its attributes from the specified
   * attribute set.
   *
   * @param atts the attribute set that contains the border spec
   */
  CSSBorder(AttributeSet atts, StyleSheet ss)
  {
    // Determine the border styles.
    int style = getBorderStyle(atts, CSS.Attribute.BORDER_STYLE);
    if (style == STYLE_NOT_SET)
      style = STYLE_NONE; // Default to none.
    topStyle = bottomStyle = leftStyle = rightStyle = style;
    style =  getBorderStyle(atts, CSS.Attribute.BORDER_TOP_STYLE);
    if (style != STYLE_NOT_SET)
      topStyle = style;
    style =  getBorderStyle(atts, CSS.Attribute.BORDER_BOTTOM_STYLE);
    if (style != STYLE_NOT_SET)
      bottomStyle = style;
    style =  getBorderStyle(atts, CSS.Attribute.BORDER_LEFT_STYLE);
    if (style != STYLE_NOT_SET)
      leftStyle = style;
    style =  getBorderStyle(atts, CSS.Attribute.BORDER_RIGHT_STYLE);
    if (style != STYLE_NOT_SET)
      rightStyle = style;

    // Determine the border colors.
    Color color = getBorderColor(atts, CSS.Attribute.BORDER_COLOR);
    if (color == null)
      color = Color.BLACK;
    topColor = bottomColor = leftColor = rightColor = color;
    color = getBorderColor(atts, CSS.Attribute.BORDER_TOP_COLOR);
    if (color != null)
      topColor = color;
    color = getBorderColor(atts, CSS.Attribute.BORDER_BOTTOM_COLOR);
    if (color != null)
      bottomColor = color;
    color = getBorderColor(atts, CSS.Attribute.BORDER_LEFT_COLOR);
    if (color != null)
      leftColor = color;
    color = getBorderColor(atts, CSS.Attribute.BORDER_RIGHT_COLOR);
    if (color != null)
      rightColor = color;

    // Determine the border widths.
    int width = getBorderWidth(atts, CSS.Attribute.BORDER_WIDTH, ss);
    if (width == -1)
      width = 0;
    top = bottom = left = right = width;
    width = getBorderWidth(atts, CSS.Attribute.BORDER_TOP_WIDTH, ss);
    if (width >= 0)
      top = width;
    width = getBorderWidth(atts, CSS.Attribute.BORDER_BOTTOM_WIDTH, ss);
    if (width >= 0)
      bottom = width;
    width = getBorderWidth(atts, CSS.Attribute.BORDER_LEFT_WIDTH, ss);
    if (width >= 0)
      left = width;
    width = getBorderWidth(atts, CSS.Attribute.BORDER_RIGHT_WIDTH, ss);
    if (width >= 0)
      right = width;
  }

  /**
   * Determines the border style for a given CSS attribute.
   *
   * @param atts the attribute set
   * @param key the CSS key
   *
   * @return the border style according to the constants defined in this class
   */
  private int getBorderStyle(AttributeSet atts, CSS.Attribute key)
  {
    int style = STYLE_NOT_SET;
    Object o = atts.getAttribute(key);
    if (o != null)
      {
        String cssStyle = o.toString();
        if (cssStyle.equals("none"))
          style = STYLE_NONE;
        else if (cssStyle.equals("hidden"))
          style = STYLE_HIDDEN;
        else if (cssStyle.equals("dotted"))
          style = STYLE_DOTTED;
        else if (cssStyle.equals("dashed"))
          style = STYLE_DASHED;
        else if (cssStyle.equals("solid"))
          style = STYLE_SOLID;
        else if (cssStyle.equals("double"))
          style = STYLE_DOUBLE;
        else if (cssStyle.equals("groove"))
          style = STYLE_GROOVE;
        else if (cssStyle.equals("ridge"))
          style = STYLE_RIDGE;
        else if (cssStyle.equals("inset"))
          style = STYLE_INSET;
        else if (cssStyle.equals("outset"))
          style = STYLE_OUTSET;
      }
    return style;
  }

  /**
   * Determines the border color for the specified key.
   *
   * @param atts the attribute set from which to fetch the color
   * @param key the CSS key
   *
   * @return the border color
   */
  private Color getBorderColor(AttributeSet atts, CSS.Attribute key)
  {
    Object o = atts.getAttribute(key);
    Color color = null;
    if (o instanceof CSSColor)
      {
        CSSColor cssColor = (CSSColor) o;
        color = cssColor.getValue();
      }
    return color;
  }

  /**
   * Returns the width for the specified key.
   *
   * @param atts the attributes to fetch the width from
   * @param key the CSS key
   *
   * @return the width, or -1 of none has been set
   */
  private int getBorderWidth(AttributeSet atts, CSS.Attribute key,
                             StyleSheet ss)
  {
    int width = -1;
    Object o = atts.getAttribute(key);
    if (o instanceof BorderWidth)
      {
        BorderWidth w = (BorderWidth) o;
        w.setFontBases(ss.getEMBase(atts), ss.getEXBase(atts));
        width = (int) ((BorderWidth) o).getValue();
      }
    return width;
  }

  /**
   * Returns the border insets.
   */
  public Insets getBorderInsets(Component c)
  {
    return new Insets(top, left, bottom, right);
  }

  /**
   * CSS borders are generally opaque so return true here.
   */
  public boolean isBorderOpaque()
  {
    return true;
  }

  public void paintBorder(Component c, Graphics g, int x, int y, int width,
                          int height)
  {
    // Top border.
    paintBorderLine(g, x, y + top / 2, x + width, y + top / 2, topStyle, top,
                    topColor, false);
    // Left border.
    paintBorderLine(g, x + left / 2, y, x + left / 2, y + height, leftStyle,
                    left, leftColor, true);
    // Bottom border.
    paintBorderLine(g, x, y + height - bottom / 2, x + width,
                    y + height - bottom / 2, topStyle, bottom, bottomColor,
                    false);
    // Right border.
    paintBorderLine(g, x + width - right / 2, y, x + width - right / 2,
                    y + height, topStyle, right, rightColor, true);
    
  }

  private void paintBorderLine(Graphics g, int x1, int y1, int x2, int y2,
                               int style, int width, Color color,
                               boolean vertical)
  {
    switch (style)
      {
        case STYLE_DOTTED:
          paintDottedLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_DASHED:
          paintDashedLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_SOLID:
          paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_DOUBLE:
          paintDoubleLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_GROOVE:
          paintGrooveLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_RIDGE:
          paintRidgeLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_OUTSET:
          paintOutsetLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_INSET:
          paintInsetLine(g, x1, y1, x2, y2, width, color, vertical);
          break;
        case STYLE_NONE:
        case STYLE_HIDDEN:
        default:
          // Nothing to do in these cases.
      }
  }

  private void paintDottedLine(Graphics g, int x1, int y1, int x2, int y2,
                               int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintDashedLine(Graphics g, int x1, int y1, int x2, int y2,
                               int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintSolidLine(Graphics g, int x1, int y1, int x2, int y2,
                              int width, Color color, boolean vertical)
  {
    int x = Math.min(x1, x2);
    int y = Math.min(y1, y1);
    int w = Math.abs(x2 - x1);
    int h = Math.abs(y2 - y1);
    if (vertical)
      {
        w = width;
        x -= width / 2;
      }
    else
      {
        h = width;
        y -= width / 2;
      }
    g.setColor(color);
    g.fillRect(x, y, w, h);
  }

  private void paintDoubleLine(Graphics g, int x1, int y1, int x2, int y2,
                               int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintGrooveLine(Graphics g, int x1, int y1, int x2, int y2,
                               int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintRidgeLine(Graphics g, int x1, int y1, int x2, int y2,
                              int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintOutsetLine(Graphics g, int x1, int y1, int x2, int y2,
                               int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

  private void paintInsetLine(Graphics g, int x1, int y1, int x2, int y2,
                              int width, Color color, boolean vertical)
  {
    // FIXME: Implement this.
    paintSolidLine(g, x1, y1, x2, y2, width, color, vertical);
  }

}

/* BorderFactory.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Color;
import java.awt.Font;

import javax.swing.border.BevelBorder;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.MatteBorder;
import javax.swing.border.TitledBorder;

/**
 * A factory for commonly used borders.
 *
 * @author original author unknown
 */
public class BorderFactory
{
  private BorderFactory()
  {
    // Do nothing.
  }

  /**
   * Creates a line border withe the specified color.
   *
   * @param color A color to use for the line.
   *
   * @return The Border object
   */
  public static Border createLineBorder(Color color)
  {
    return createLineBorder(color, 1);
  }

  /**
   * Creates a line border withe the specified color and width. The width
   * applies to all 4 sides of the border. To specify widths individually for
   * the top, bottom, left, and right, use
   * createMatteBorder(int,int,int,int,Color).
   *
   * @param color A color to use for the line.
   * @param thickness An int specifying the width in pixels.
   *
   * @return The Border object
   */
  public static Border createLineBorder(Color color, int thickness)
  {
    return new LineBorder(color, thickness);
  }

  /**
   * Created a border with a raised beveled edge, using brighter shades of
   * the component's current background color for highlighting, and darker
   * shading for shadows. (In a raised border, highlights are on top and
   * shadows are underneath.)
   *
   * @return The Border object
   */
  public static Border createRaisedBevelBorder()
  {
    return new BevelBorder(BevelBorder.RAISED);
  }

  /**
   * Created a border with a lowered beveled edge, using brighter shades of
   * the component's current background color for highlighting, and darker
   * shading for shadows. (In a lowered border, shadows are on top and
   * highlights are underneath.)
   *
   * @return The Border object
   */
  public static Border createLoweredBevelBorder()
  {
    return new BevelBorder(BevelBorder.LOWERED);
  }

  /**
   * Create a beveled border of the specified type, using brighter shades of
   * the component's current background color for highlighting, and darker
   * shading for shadows. (In a lowered border, shadows are on top and
   * highlights are underneath.).
   *
   * @param type An int specifying either BevelBorder.LOWERED or
   *     BevelBorder.RAISED
   *
   * @return The Border object
   */
  public static Border createBevelBorder(int type)
  {
    return new BevelBorder(type);
  }

  /**
   * Create a beveled border of the specified type, using the specified
   * highlighting and shadowing. The outer edge of the highlighted area uses
   * a brighter shade of the highlight color. The inner edge of the shadow
   * area uses a brighter shade of the shadaw color.
   *
   * @param type An int specifying either BevelBorder.LOWERED or
   *     BevelBorder.RAISED
   * @param highlight A Color object for highlights
   * @param shadow A Color object for shadows
   *
   * @return The Border object
   */
  public static Border createBevelBorder(int type, Color highlight, Color shadow)
  {
    return new BevelBorder(type, highlight, shadow);
  }

  /**
   * Create a beveled border of the specified type, using the specified colors
   * for the inner and outer highlight and shadow areas.
   *
   * @param type An int specifying either BevelBorder.LOWERED or
   *     BevelBorder.RAISED
   * @param highlightOuter A Color object for the outer edge of the
   *     highlight area
   * @param highlightInner A Color object for the inner edge of the
   *     highlight area
   * @param shadowOuter A Color object for the outer edge of the shadow area
   * @param shadowInner A Color object for the inner edge of the shadow area
   *
   * @return The Border object
   */
  public static Border createBevelBorder(int type, Color highlightOuter,
                                         Color highlightInner,
                                         Color shadowOuter, Color shadowInner)
  {
    return new BevelBorder(type, highlightOuter, highlightInner, shadowOuter,
                           shadowInner);
  }

  /**
   * Create a border with an "etched" look using the component's current
   * background color for highlighting and shading.
   *
   * @return The Border object
   */
  public static Border createEtchedBorder()
  {
    return new EtchedBorder();
  }

  /**
   * Create a border with an "etched" look using the component's current
   * background color for highlighting and shading.
   *
   * @return The Border object
   */
  public static Border createEtchedBorder(int etchType)
  {
    return new EtchedBorder(etchType);
  }

  /**
   * Create a border with an "etched" look using the specified highlighting and
   * shading colors.
   *
   * @param highlight A Color object for the border highlights
   * @param shadow A Color object for the border shadows
   *
   * @return The Border object
   */
  public static Border createEtchedBorder(Color highlight, Color shadow)
  {
    return new EtchedBorder(highlight, shadow);
  }

  /**
   * Create a border with an "etched" look using the specified highlighting and
   * shading colors.
   *
   * @param highlight A Color object for the border highlights
   * @param shadow A Color object for the border shadows
   *
   * @return The Border object
   */
  public static Border createEtchedBorder(int etchType, Color highlight,
                                          Color shadow)
  {
    return new EtchedBorder(etchType, highlight, shadow);
  }

  /**
   * Create a new title border specifying the text of the title, using the
   * default border (etched), using the default text position (sitting on the
   * top line) and default justification (left) and using the default font and
   * text color determined by the current look and feel.
   *
   * @param title A String containing the text of the title
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(String title)
  {
    return new TitledBorder(title);
  }

  /**
   * Create a new title border with an empty title specifying the border
   * object, using the default text position (sitting on the top line) and
   * default justification (left) and using the default font, text color,
   * and border determined by the current look and feel. (The Motif and Windows
   * look and feels use an etched border; The Java look and feel use a
   * gray border.)
   *
   * @param border The Border object to add the title to
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(Border border)
  {
    return new TitledBorder(border);
  }

  /**
   * Add a title to an existing border, specifying the text of the title, using
   * the default positioning (sitting on the top line) and default
   * justification (left) and using the default font and text color determined
   * by the current look and feel.
   *
   * @param border The Border object to add the title to
   * @param title A String containing the text of the title
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(Border border, String title)
  {
    return new TitledBorder(border, title);
  }

  /**
   * Add a title to an existing border, specifying the text of the title along
   * with its positioning, using the default font and text color determined by
   * the current look and feel.
   *
   * @param border The Border object to add the title to
   * @param title A String containing the text of the title
   * @param titleJustification An int specifying the left/right position of
   *     the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or
   *     TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).
   * @param titlePosition An int specifying the vertical position of the text
   *     in relation to the border -- one of: TitledBorder.ABOVE_TOP,
   *     TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP,
   *     TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom
   *     line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION
   *     (top).
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(Border border, String title,
                                                int titleJustification,
                                                int titlePosition)
  {
    return new TitledBorder(border, title, titleJustification, titlePosition);
  }

  /**
   * Add a title to an existing border, specifying the text of the title along
   * with its positioning and font, using the default text color determined by
   * the current look and feel.
   *
   * @param border - the Border object to add the title to
   * @param title - a String containing the text of the title
   * @param titleJustification - an int specifying the left/right position of
   *     the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or
   *     TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).
   * @param titlePosition - an int specifying the vertical position of the
   *     text in relation to the border -- one of: TitledBorder.ABOVE_TOP,
   *     TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP,
   *     TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom
   *     line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION (top).
   * @param titleFont - a Font object specifying the title font
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(Border border, String title,
                                                int titleJustification,
                                                int titlePosition,
                                                Font titleFont)
  {
    return new TitledBorder(border, title, titleJustification, titlePosition,
                            titleFont);
  }

  /**
   * Add a title to an existing border, specifying the text of the title along
   * with its positioning, font, and color.
   *
   * @param border - the Border object to add the title to
   * @param title - a String containing the text of the title
   * @param titleJustification - an int specifying the left/right position of
   *     the title -- one of TitledBorder.LEFT, TitledBorder.CENTER, or
   *     TitledBorder.RIGHT, TitledBorder.DEFAULT_JUSTIFICATION (left).
   * @param titlePosition - an int specifying the vertical position of the text
   *     in relation to the border -- one of: TitledBorder.ABOVE_TOP,
   *     TitledBorder.TOP (sitting on the top line), TitledBorder.BELOW_TOP,
   *     TitledBorder.ABOVE_BOTTOM, TitledBorder.BOTTOM (sitting on the bottom
   *     line), TitledBorder.BELOW_BOTTOM, or TitledBorder.DEFAULT_POSITION (top).
   * @param titleFont - a Font object specifying the title font
   * @param titleColor - a Color object specifying the title color
   *
   * @return The TitledBorder object
   */
  public static TitledBorder createTitledBorder(Border border, String title,
                                                int titleJustification,
                                                int titlePosition,
                                                Font titleFont, Color titleColor)
  {
    return new TitledBorder(border, title, titleJustification, titlePosition,
                            titleFont, titleColor);
  }

  /**
   * Creates an empty border that takes up no space. (The width of the top,
   * bottom, left, and right sides are all zero.)
   *
   * @return The Border object
   */
  public static Border createEmptyBorder()
  {
    return new EmptyBorder(0, 0, 0, 0);
  }

  /**
   * Creates an empty border that takes up no space but which does no drawing,
   * specifying the width of the top, left, bottom, and right sides.
   *
   * @param top An int specifying the width of the top in pixels
   * @param left An int specifying the width of the left side in pixels
   * @param bottom An int specifying the width of the right side in pixels
   * @param right An int specifying the width of the bottom in pixels
   *
   * @return The Border object
   */
  public static Border createEmptyBorder(int top, int left, int bottom,
                                         int right)
  {
    return new EmptyBorder(top, left, bottom, right);
  }

  /**
   * Create a compound border with a null inside edge and a null outside edge.
   *
   * @return The CompoundBorder object
   */
  public static CompoundBorder createCompoundBorder()
  {
    return new CompoundBorder();
  }

  /**
   * Create a compound border specifying the border objects to use for the
   * outside and inside edges.
   *
   * @param outsideBorder A Border object for the outer edge of the
   *     compound border
   * @param insideBorder A Border object for the inner edge of the
   *     compound border
   *
   * @return The CompoundBorder object
   */
  public static CompoundBorder createCompoundBorder(Border outsideBorder,
                                                    Border insideBorder)
  {
    return new CompoundBorder(outsideBorder, insideBorder);
  }

  /**
   * Create a matte-look border using a solid color. (The difference between
   * this border and a line border is that you can specify the individual border
   * dimensions.)
   * 
   * @param top
   *          An int specifying the width of the top in pixels
   * @param left
   *          An int specifying the width of the left side in pixels
   * @param bottom
   *          An int specifying the width of the right side in pixels
   * @param right
   *          An int specifying the width of the bottom in pixels
   * @param color
   *          A Color to use for the border
   * @return The MatteBorder object
   */
  public static MatteBorder createMatteBorder(int top, int left, int bottom,
                                              int right, Color color)
  {
    return new MatteBorder(top, left, bottom, right, color);
  }

  /**
   * Create a matte-look border that consists of multiple tiles of a specified
   * icon. Multiple copies of the icon are placed side-by-side to fill up the
   * border area.
   *
   * Note:
   * If the icon doesn't load, the border area is painted gray.
   *
   * @param top An int specifying the width of the top in pixels
   * @param left An int specifying the width of the left side in pixels
   * @param bottom An int specifying the width of the right side in pixels
   * @param right An int specifying the width of the bottom in pixels
   * @param tileIcon The Icon object used for the border tiles
   *
   * @return The MatteBorder object
   */
  public static MatteBorder createMatteBorder(int top, int left, int bottom,
                                              int right, Icon tileIcon)
  {
    return new MatteBorder(top, left, bottom, right, tileIcon);
  }
}

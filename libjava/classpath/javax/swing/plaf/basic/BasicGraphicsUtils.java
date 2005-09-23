/* BasicGraphicsUtils.java
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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

package javax.swing.plaf.basic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.font.FontRenderContext;
import java.awt.font.LineMetrics;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;


/**
 * A utility class providing commonly used drawing and measurement
 * routines.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class BasicGraphicsUtils
{
  /**
   * Constructor. It is utterly unclear why this class should
   * be constructable, but this is what the API specification
   * says.
   */
  public BasicGraphicsUtils()
  {
  }


  /**
   * Draws a rectangle that appears etched into the surface, given
   * four colors that are used for drawing.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-1.png" width="360"
   * height="200" alt="[An illustration that shows which pixels
   * get painted in what color]" />
   *
   * @param g the graphics into which the rectangle is drawn.
   * @param x the x coordinate of the rectangle.
   * @param y the y coordinate of the rectangle.
   * @param width the width of the rectangle in pixels.
   * @param height the height of the rectangle in pixels.
   *
   * @param shadow the color that will be used for painting
   *        the outer side of the top and left edges.
   *
   * @param darkShadow the color that will be used for painting
   *        the inner side of the top and left edges.
   *
   * @param highlight the color that will be used for painting
   *        the inner side of the bottom and right edges.
   *
   * @param lightHighlight the color that will be used for painting
   *        the outer side of the bottom and right edges.
   *
   * @see #getEtchedInsets()
   * @see javax.swing.border.EtchedBorder
   */
  public static void drawEtchedRect(Graphics g,
                                    int x, int y, int width, int height,
                                    Color shadow, Color darkShadow,
                                    Color highlight, Color lightHighlight)
  {
    Color oldColor;
    int x2, y2;

    oldColor = g.getColor();
    x2 = x + width - 1;
    y2 = y + height - 1;

    try
    {
      /* To understand this code, it might be helpful to look at the
       * image "BasicGraphicsUtils-1.png" that is included with the
       * JavaDoc. The file is located in the "doc-files" subdirectory.
       *
       * (x2, y2) is the coordinate of the most right and bottom pixel
       * to be painted.
       */
      g.setColor(shadow);
      g.drawLine(x, y, x2 - 1, y);                     // top, outer
      g.drawLine(x, y + 1, x, y2 - 1);                 // left, outer

      g.setColor(darkShadow);
      g.drawLine(x + 1, y + 1, x2 - 2, y + 1);         // top, inner
      g.drawLine(x + 1, y + 2, x + 1, y2 - 2);         // left, inner
      
      g.setColor(highlight);
      g.drawLine(x + 1, y2 - 1, x2 - 1, y2 - 1);       // bottom, inner
      g.drawLine(x2 - 1, y + 1, x2 - 1, y2 - 2);       // right, inner

      g.setColor(lightHighlight);
      g.drawLine(x, y2, x2, y2);                       // bottom, outer
      g.drawLine(x2, y, x2, y2 - 1);                   // right, outer
    }
    finally
    {
      g.setColor(oldColor);
    }
  }
  
  
  /**
   * Determines the width of the border that gets painted by
   * {@link #drawEtchedRect}.
   *
   * @return an <code>Insets</code> object whose <code>top</code>,
   *         <code>left</code>, <code>bottom</code> and
   *         <code>right</code> field contain the border width at the
   *         respective edge in pixels.
   */
  public static Insets getEtchedInsets()
  {
    return new Insets(2, 2, 2, 2);
  }


  /**
   * Draws a rectangle that appears etched into the surface, given
   * two colors that are used for drawing.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-2.png" width="360"
   * height="200" alt="[An illustration that shows which pixels
   * get painted in what color]" />
   *
   * @param g the graphics into which the rectangle is drawn.
   * @param x the x coordinate of the rectangle.
   * @param y the y coordinate of the rectangle.
   * @param width the width of the rectangle in pixels.
   * @param height the height of the rectangle in pixels.
   *
   * @param shadow the color that will be used for painting the outer
   *        side of the top and left edges, and for the inner side of
   *        the bottom and right ones.
   *
   * @param highlight the color that will be used for painting the
   *        inner side of the top and left edges, and for the outer
   *        side of the bottom and right ones.
   *
   * @see #getGrooveInsets()
   * @see javax.swing.border.EtchedBorder
   */
  public static void drawGroove(Graphics g,
                                int x, int y, int width, int height,
                                Color shadow, Color highlight)
  {
    /* To understand this, it might be helpful to look at the image
     * "BasicGraphicsUtils-2.png" that is included with the JavaDoc,
     * and to compare it with "BasicGraphicsUtils-1.png" which shows
     * the pixels painted by drawEtchedRect.  These image files are
     * located in the "doc-files" subdirectory.
     */
    drawEtchedRect(g, x, y, width, height,
                   /* outer topLeft */     shadow,
                   /* inner topLeft */     highlight,
                   /* inner bottomRight */ shadow,
                   /* outer bottomRight */ highlight);
  }


  /**
   * Determines the width of the border that gets painted by
   * {@link #drawGroove}.
   *
   * @return an <code>Insets</code> object whose <code>top</code>,
   *         <code>left</code>, <code>bottom</code> and
   *         <code>right</code> field contain the border width at the
   *         respective edge in pixels.
   */
  public static Insets getGrooveInsets()
  {
    return new Insets(2, 2, 2, 2);
  }
  

  /**
   * Draws a border that is suitable for buttons of the Basic look and
   * feel.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-3.png" width="500"
   * height="300" alt="[An illustration that shows which pixels
   * get painted in what color]" />
   *
   * @param g the graphics into which the rectangle is drawn.
   * @param x the x coordinate of the rectangle.
   * @param y the y coordinate of the rectangle.
   * @param width the width of the rectangle in pixels.
   * @param height the height of the rectangle in pixels.
   *
   * @param isPressed <code>true</code> to draw the button border
   *        with a pressed-in appearance; <code>false</code> for
   *        normal (unpressed) appearance.
   *
   * @param isDefault <code>true</code> to draw the border with
   *        the appearance it has when hitting the enter key in a
   *        dialog will simulate a click to this button;
   *        <code>false</code> for normal appearance.
   *
   * @param shadow the shadow color.
   * @param darkShadow a darker variant of the shadow color.
   * @param highlight the highlight color.
   * @param lightHighlight a brighter variant of the highlight  color.
   */
  public static void drawBezel(Graphics g,
                               int x, int y, int width, int height,
                               boolean isPressed, boolean isDefault,
                               Color shadow, Color darkShadow,
                               Color highlight, Color lightHighlight)
  {
    Color oldColor = g.getColor();

    /* To understand this, it might be helpful to look at the image
     * "BasicGraphicsUtils-3.png" that is included with the JavaDoc,
     * and to compare it with "BasicGraphicsUtils-1.png" which shows
     * the pixels painted by drawEtchedRect.  These image files are
     * located in the "doc-files" subdirectory.
     */
    try
    {
      if ((isPressed == false) && (isDefault == false))
      {
        drawEtchedRect(g, x, y, width, height,
                       lightHighlight, highlight,
                       shadow, darkShadow);
      }

      if ((isPressed == true) && (isDefault == false))
      {
        g.setColor(shadow);
        g.drawRect(x + 1, y + 1, width - 2, height - 2);
      }

      if ((isPressed == false) && (isDefault == true))
      {
        g.setColor(darkShadow);
        g.drawRect(x, y, width - 1, height - 1);
        drawEtchedRect(g, x + 1, y + 1, width - 2, height - 2,
                       lightHighlight, highlight,
                       shadow, darkShadow);
      }

      if ((isPressed == true) && (isDefault == true))
      {
        g.setColor(darkShadow);
        g.drawRect(x, y, width - 1, height - 1);
        g.setColor(shadow);
        g.drawRect(x + 1, y + 1, width - 3, height - 3);
      }
    }
    finally
    {
      g.setColor(oldColor);
    }
  }
  
  
  /**
   * Draws a rectangle that appears lowered into the surface, given
   * four colors that are used for drawing.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-4.png" width="360"
   * height="200" alt="[An illustration that shows which pixels
   * get painted in what color]" />
   *
   * <p><strong>Compatibility with the Sun reference
   * implementation:</strong> The Sun reference implementation seems
   * to ignore the <code>x</code> and <code>y</code> arguments, at
   * least in JDK 1.3.1 and 1.4.1_01.  The method always draws the
   * rectangular area at location (0, 0). A bug report has been filed
   * with Sun; its &#x201c;bug ID&#x201d; is 4880003.  The GNU Classpath
   * implementation behaves correctly, thus not replicating this bug.
   *
   * @param g the graphics into which the rectangle is drawn.
   * @param x the x coordinate of the rectangle.
   * @param y the y coordinate of the rectangle.
   * @param width the width of the rectangle in pixels.
   * @param height the height of the rectangle in pixels.
   *
   * @param shadow the color that will be used for painting
   *        the inner side of the top and left edges.
   *
   * @param darkShadow the color that will be used for painting
   *        the outer side of the top and left edges.
   *
   * @param highlight the color that will be used for painting
   *        the inner side of the bottom and right edges.
   *
   * @param lightHighlight the color that will be used for painting
   *        the outer side of the bottom and right edges.
   */
  public static void drawLoweredBezel(Graphics g,
                                      int x, int y, int width, int height,
                                      Color shadow, Color darkShadow,
                                      Color highlight, Color lightHighlight)
  {
    /* Like drawEtchedRect, but swapping darkShadow and shadow.
     *
     * To understand this, it might be helpful to look at the image
     * "BasicGraphicsUtils-4.png" that is included with the JavaDoc,
     * and to compare it with "BasicGraphicsUtils-1.png" which shows
     * the pixels painted by drawEtchedRect.  These image files are
     * located in the "doc-files" subdirectory.
     */
    drawEtchedRect(g, x, y, width, height,
                   darkShadow, shadow,
                   highlight, lightHighlight);
  }
  
  
  /**
   * Draws a String at the given location, underlining the first
   * occurence of a specified character. The algorithm for determining
   * the underlined position is not sensitive to case. If the
   * character is not part of <code>text</code>, the text will be
   * drawn without underlining. Drawing is performed in the current
   * color and font of <code>g</code>.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-5.png" width="500"
   * height="100" alt="[An illustration showing how to use the
   * method]" />
   *
   * @param g the graphics into which the String is drawn.
   *
   * @param text the String to draw.
   *
   * @param underlinedChar the character whose first occurence in
   *        <code>text</code> will be underlined. It is not clear
   *        why the API specification declares this argument to be
   *        of type <code>int</code> instead of <code>char</code>.
   *        While this would allow to pass Unicode characters outside
   *        Basic Multilingual Plane 0 (U+0000 .. U+FFFE), at least
   *        the GNU Classpath implementation does not underline
   *        anything if <code>underlinedChar</code> is outside
   *        the range of <code>char</code>.
   *        
   * @param x the x coordinate of the text, as it would be passed to
   *        {@link java.awt.Graphics#drawString(java.lang.String,
   *        int, int)}.
   *
   * @param y the y coordinate of the text, as it would be passed to
   *        {@link java.awt.Graphics#drawString(java.lang.String,
   *        int, int)}.
   */
  public static void drawString(Graphics g, String text,
                                int underlinedChar, int x, int y)
  {
    int index = -1;

    /* It is intentional that lower case is used. In some languages,
     * the set of lowercase characters is larger than the set of
     * uppercase ones. Therefore, it is good practice to use lowercase
     * for such comparisons (which really means that the author of this
     * code can vaguely remember having read some Unicode techreport
     * with this recommendation, but is too lazy to look for the URL).
     */
    if ((underlinedChar >= 0) || (underlinedChar <= 0xffff))
      index = text.toLowerCase().indexOf(
        Character.toLowerCase((char) underlinedChar));

    drawStringUnderlineCharAt(g, text, index, x, y);
  }


  /**
   * Draws a String at the given location, underlining the character
   * at the specified index. Drawing is performed in the current color
   * and font of <code>g</code>.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-5.png" width="500"
   * height="100" alt="[An illustration showing how to use the
   * method]" />
   *
   * @param g the graphics into which the String is drawn.
   *
   * @param text the String to draw.
   *
   * @param underlinedIndex the index of the underlined character in
   *        <code>text</code>.  If <code>underlinedIndex</code> falls
   *        outside the range <code>[0, text.length() - 1]</code>, the
   *        text will be drawn without underlining anything.
   *        
   * @param x the x coordinate of the text, as it would be passed to
   *        {@link java.awt.Graphics#drawString(java.lang.String,
   *        int, int)}.
   *
   * @param y the y coordinate of the text, as it would be passed to
   *        {@link java.awt.Graphics#drawString(java.lang.String,
   *        int, int)}.
   *
   * @since 1.4
   */
  public static void drawStringUnderlineCharAt(Graphics g, String text,
                                               int underlinedIndex,
                                               int x, int y)
  {
    Graphics2D g2;
    Rectangle2D.Double underline;
    FontRenderContext frc;
    FontMetrics fmet;
    LineMetrics lineMetrics;
    Font font;
    TextLayout layout;
    double underlineX1, underlineX2;
    boolean drawUnderline;
    int textLength;

    textLength = text.length();
    if (textLength == 0)
      return;

    drawUnderline = (underlinedIndex >= 0) && (underlinedIndex < textLength);

    // FIXME: unfortunately pango and cairo can't agree on metrics
    // so for the time being we continue to *not* use TextLayouts.
    if (true || !(g instanceof Graphics2D))
    {
      /* Fall-back. This is likely to produce garbage for any text
       * containing right-to-left (Hebrew or Arabic) characters, even
       * if the underlined character is left-to-right.
       */
      g.drawString(text, x, y);
      if (drawUnderline)
      {
        fmet = g.getFontMetrics();
        g.fillRect(
          /* x */ x + fmet.stringWidth(text.substring(0, underlinedIndex)),
          /* y */ y + fmet.getDescent() - 1,
          /* width */ fmet.charWidth(text.charAt(underlinedIndex)),
          /* height */ 1);
      }

      return;
    }

    g2 = (Graphics2D) g;
    font = g2.getFont();
    frc = g2.getFontRenderContext();
    lineMetrics = font.getLineMetrics(text, frc);
    layout = new TextLayout(text, font, frc);

    /* Draw the text. */
    layout.draw(g2, x, y);
    if (!drawUnderline)
      return;

    underlineX1 = x + layout.getLogicalHighlightShape(
     underlinedIndex, underlinedIndex).getBounds2D().getX();
    underlineX2 = x + layout.getLogicalHighlightShape(
     underlinedIndex + 1, underlinedIndex + 1).getBounds2D().getX();

    underline = new Rectangle2D.Double();
    if (underlineX1 < underlineX2)
    {
      underline.x = underlineX1;
      underline.width = underlineX2 - underlineX1;
    }
    else
    {
      underline.x = underlineX2;
      underline.width = underlineX1 - underlineX2;
    }

    
    underline.height = lineMetrics.getUnderlineThickness();
    underline.y = lineMetrics.getUnderlineOffset();
    if (underline.y == 0)
    {
      /* Some fonts do not specify an underline offset, although they
       * actually should do so. In that case, the result of calling
       * lineMetrics.getUnderlineOffset() will be zero. Since it would
       * look very ugly if the underline was be positioned immediately
       * below the baseline, we check for this and move the underline
       * below the descent, as shown in the following ASCII picture:
       *
       *   #####       ##### #
       *  #     #     #     #
       *  #     #     #     #
       *  #     #     #     #
       *   #####       ######        ---- baseline (0)
       *                    #
       *                    #
       * ------------------###----------- lineMetrics.getDescent()
       */
      underline.y = lineMetrics.getDescent();
    }

    underline.y += y;
    g2.fill(underline);
  }


  /**
   * Draws a rectangle, simulating a dotted stroke by painting only
   * every second pixel along the one-pixel thick edge. The color of
   * those pixels is the current color of the Graphics <code>g</code>.
   * Any other pixels are left unchanged.
   *
   * <p><img src="doc-files/BasicGraphicsUtils-7.png" width="360"
   * height="200" alt="[An illustration that shows which pixels
   * get painted]" />
   *
   * @param g the graphics into which the rectangle is drawn.
   * @param x the x coordinate of the rectangle.
   * @param y the y coordinate of the rectangle.
   * @param width the width of the rectangle in pixels.
   * @param height the height of the rectangle in pixels.
   */
  public static void drawDashedRect(Graphics g,
                                    int x, int y, int width, int height)
  {
    int right = x + width - 1;
    int bottom = y + height - 1;

    /* Draw the top and bottom edge of the dotted rectangle. */
    for (int i = x; i <= right; i += 2)
    {
      g.drawLine(i, y, i, y);
      g.drawLine(i, bottom, i, bottom);
    }

    /* Draw the left and right edge of the dotted rectangle. */
    for (int i = y; i <= bottom; i += 2)
    {
      g.drawLine(x, i, x, i);
      g.drawLine(right, i, right, i);
    }
  }


  /**
   * Determines the preferred width and height of an AbstractButton,
   * given the gap between the button&#x2019;s text and icon.
   *
   * @param b the button whose preferred size is determined.
   *
   * @param textIconGap the gap between the button&#x2019;s text and
   *        icon.
   *
   * @return a <code>Dimension</code> object whose <code>width</code>
   *         and <code>height</code> fields indicate the preferred
   *         extent in pixels.
   *
   * @see javax.swing.SwingUtilities#layoutCompoundLabel(JComponent, 
   *      FontMetrics, String, Icon, int, int, int, int, Rectangle, Rectangle, 
   *      Rectangle, int)
   */
  public static Dimension getPreferredButtonSize(AbstractButton b,
                                                 int textIconGap)
  {
    Rectangle contentRect;
    Rectangle viewRect;
    Rectangle iconRect = new Rectangle();
    Rectangle textRect = new Rectangle();
    Insets insets = b.getInsets();
    
    viewRect = new Rectangle();

     /* java.awt.Toolkit.getFontMetrics is deprecated. However, it
     * seems not obvious how to get to the correct FontMetrics object
     * otherwise. The real problem probably is that the method
     * javax.swing.SwingUtilities.layoutCompundLabel should take a
     * LineMetrics, not a FontMetrics argument. But fixing this that
     * would change the public API.
     */
   SwingUtilities.layoutCompoundLabel(
      b, // for the component orientation
      b.getToolkit().getFontMetrics(b.getFont()), // see comment above
      b.getText(),
      b.getIcon(),
      b.getVerticalAlignment(), 
      b.getHorizontalAlignment(),
      b.getVerticalTextPosition(),
      b.getHorizontalTextPosition(),
      viewRect, iconRect, textRect,
      textIconGap);

    /*  +------------------------+       +------------------------+
     *  |                        |       |                        |
     *  | ICON                   |       | CONTENTCONTENTCONTENT  |
     *  |          TEXTTEXTTEXT  |  -->  | CONTENTCONTENTCONTENT  |
     *  |          TEXTTEXTTEXT  |       | CONTENTCONTENTCONTENT  |
     *  +------------------------+       +------------------------+
     */

    contentRect = textRect.union(iconRect);
    
    return new Dimension(insets.left
			 + contentRect.width 
			 + insets.right + b.getHorizontalAlignment(),
                         insets.top
			 + contentRect.height 
			 + insets.bottom);
  }
}

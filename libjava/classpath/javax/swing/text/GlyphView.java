/* GlyphView.java -- A view to render styled text
   Copyright (C) 2005  Free Software Foundation, Inc.

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


package javax.swing.text;

import gnu.classpath.SystemProperties;

import java.awt.Color;
import java.awt.Container;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.TextHitInfo;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;

import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.text.Position.Bias;

/**
 * Renders a run of styled text. This {@link View} subclass paints the
 * characters of the <code>Element</code> it is responsible for using
 * the style information from that <code>Element</code>.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class GlyphView extends View implements TabableView, Cloneable
{

  /**
   * An abstract base implementation for a glyph painter for
   * <code>GlyphView</code>.
   */
  public abstract static class GlyphPainter
  {
    /**
     * Creates a new <code>GlyphPainer</code>.
     */
    public GlyphPainter()
    {
      // Nothing to do here.
    }

    /**
     * Returns the ascent of the font that is used by this glyph painter.
     *
     * @param v the glyph view
     *
     * @return the ascent of the font that is used by this glyph painter
     */
    public abstract float getAscent(GlyphView v);

    /**
     * Returns the descent of the font that is used by this glyph painter.
     *
     * @param v the glyph view
     *
     * @return the descent of the font that is used by this glyph painter
     */
    public abstract float getDescent(GlyphView v);

    /**
     * Returns the full height of the rendered text.
     *
     * @return the full height of the rendered text
     */
    public abstract float getHeight(GlyphView view);

    /**
     * Determines the model offset, so that the text between <code>p0</code>
     * and this offset fits within the span starting at <code>x</code> with
     * the length of <code>len</code>. 
     *
     * @param v the glyph view
     * @param p0 the starting offset in the model
     * @param x the start location in the view
     * @param len the length of the span in the view
     */
    public abstract int getBoundedPosition(GlyphView v, int p0, float x,
                                           float len);

    /**
     * Paints the glyphs.
     *
     * @param view the glyph view to paint
     * @param g the graphics context to use for painting
     * @param a the allocation of the glyph view
     * @param p0 the start position (in the model) from which to paint
     * @param p1 the end position (in the model) to which to paint
     */
    public abstract void paint(GlyphView view, Graphics g, Shape a, int p0,
                               int p1);

    /**
     * Maps a position in the document into the coordinate space of the View.
     * The output rectangle usually reflects the font height but has a width
     * of zero.
     *
     * @param view the glyph view
     * @param pos the position of the character in the model
     * @param a the area that is occupied by the view
     * @param b either {@link Position.Bias#Forward} or
     *        {@link Position.Bias#Backward} depending on the preferred
     *        direction bias. If <code>null</code> this defaults to
     *        <code>Position.Bias.Forward</code>
     *
     * @return a rectangle that gives the location of the document position
     *         inside the view coordinate space
     *
     * @throws BadLocationException if <code>pos</code> is invalid
     * @throws IllegalArgumentException if b is not one of the above listed
     *         valid values
     */
    public abstract Shape modelToView(GlyphView view, int pos, Position.Bias b,
                                      Shape a)
      throws BadLocationException;

    /**
     * Maps a visual position into a document location.
     *
     * @param v the glyph view
     * @param x the X coordinate of the visual position
     * @param y the Y coordinate of the visual position
     * @param a the allocated region
     * @param biasRet filled with the bias of the model location on method exit
     *
     * @return the model location that represents the specified view location
     */
    public abstract int viewToModel(GlyphView v, float x, float y, Shape a,
                                    Position.Bias[] biasRet);

    /**
     * Determine the span of the glyphs from location <code>p0</code> to
     * location <code>p1</code>. If <code>te</code> is not <code>null</code>,
     * then TABs are expanded using this <code>TabExpander</code>.
     * The parameter <code>x</code> is the location at which the view is
     * located (this is important when using TAB expansion).
     *
     * @param view the glyph view
     * @param p0 the starting location in the document model
     * @param p1 the end location in the document model
     * @param te the tab expander to use
     * @param x the location at which the view is located
     *
     * @return the span of the glyphs from location <code>p0</code> to
     *         location <code>p1</code>, possibly using TAB expansion
     */
    public abstract float getSpan(GlyphView view, int p0, int p1,
                                  TabExpander te, float x);


    /**
     * Returns the model location that should be used to place a caret when
     * moving the caret through the document.
     *
     * @param v the glyph view
     * @param pos the current model location
     * @param b the bias for <code>p</code>
     * @param a the allocated region for the glyph view
     * @param direction the direction from the current position; Must be one of
     *        {@link SwingConstants#EAST}, {@link SwingConstants#WEST},
     *        {@link SwingConstants#NORTH} or {@link SwingConstants#SOUTH}
     * @param biasRet filled with the bias of the resulting location when method
     *        returns
     *
     * @return the location within the document that should be used to place the
     *         caret when moving the caret around the document
     *
     * @throws BadLocationException if <code>pos</code> is an invalid model
     *         location
     * @throws IllegalArgumentException if <code>d</code> is invalid
     */
    public int getNextVisualPositionFrom(GlyphView v, int pos, Position.Bias b,
                                         Shape a, int direction,
                                         Position.Bias[] biasRet)
      throws BadLocationException

    {
      int result = pos;
      switch (direction)
      {
        case SwingConstants.EAST:
          result = pos + 1;
          break;
        case SwingConstants.WEST:
          result = pos - 1;
          break;
        case SwingConstants.NORTH:
        case SwingConstants.SOUTH:
        default:
          // This should be handled in enclosing view, since the glyph view
          // does not layout vertically.
          break;
      }
      return result;
    }

    /**
     * Returns a painter that can be used to render the specified glyph view.
     * If this glyph painter is stateful, then it should return a new instance.
     * However, if this painter is stateless it should return itself. The
     * default behaviour is to return itself.
     *
     * @param v the glyph view for which to create a painter
     * @param p0 the start offset of the rendered area
     * @param p1 the end offset of the rendered area
     *
     * @return a painter that can be used to render the specified glyph view
     */
    public GlyphPainter getPainter(GlyphView v, int p0, int p1)
    {
      return this;
    }
  }

  /**
   * A GlyphPainter implementation based on TextLayout. This should give
   * better performance in Java2D environments.
   */
  private static class J2DGlyphPainter
    extends GlyphPainter
  {

    /**
     * The text layout.
     */
    TextLayout textLayout;

    /**
     * Creates a new J2DGlyphPainter.
     *
     * @param str the string
     * @param font the font
     * @param frc the font render context
     */
    J2DGlyphPainter(String str, Font font, FontRenderContext frc)
    {
      textLayout = new TextLayout(str, font, frc);
    }

    /**
     * Returns null so that GlyphView.checkPainter() creates a new instance.
     */
    public GlyphPainter getPainter(GlyphView v, int p0, int p1)
    {
      return null;
    }

    /**
     * Delegates to the text layout.
     */
    public float getAscent(GlyphView v)
    {
      return textLayout.getAscent();
    }

    /**
     * Delegates to the text layout.
     */
    public int getBoundedPosition(GlyphView v, int p0, float x, float len)
    {
      int pos;
      TextHitInfo hit = textLayout.hitTestChar(len, 0);
      if (hit.getCharIndex() == -1 && ! textLayout.isLeftToRight())
        pos = v.getEndOffset();
      else
        {
          pos = hit.isLeadingEdge() ? hit.getInsertionIndex()
                                    : hit.getInsertionIndex() - 1;
          pos += v.getStartOffset();
        }
      return pos;
    }

    /**
     * Delegates to the text layout.
     */
    public float getDescent(GlyphView v)
    {
      return textLayout.getDescent();
    }

    /**
     * Delegates to the text layout.
     */
    public float getHeight(GlyphView view)
    {
      return textLayout.getAscent() + textLayout.getDescent()
             + textLayout.getLeading();
    }

    /**
     * Delegates to the text layout.
     */
    public float getSpan(GlyphView v, int p0, int p1, TabExpander te, float x)
    {
      float span;
      if (p0 == v.getStartOffset() && p1 == v.getEndOffset())
        span = textLayout.getAdvance();
      else
        {
          int start = v.getStartOffset();
          int i0 = p0 - start;
          int i1 = p1 - start;
          TextHitInfo hit0 = TextHitInfo.afterOffset(i0);
          TextHitInfo hit1 = TextHitInfo.afterOffset(i1);
          float x0 = textLayout.getCaretInfo(hit0)[0];
          float x1 = textLayout.getCaretInfo(hit1)[0];
          span = Math.abs(x1 - x0);
        }
      return span;
    }

    /**
     * Delegates to the text layout.
     */
    public Shape modelToView(GlyphView v, int pos, Bias b, Shape a)
      throws BadLocationException
    {
      int offs = pos - v.getStartOffset();
      // Create copy here to protect original shape.
      Rectangle2D bounds = a.getBounds2D();
      TextHitInfo hit =
        b == Position.Bias.Forward ? TextHitInfo.afterOffset(offs)
                                   : TextHitInfo.beforeOffset(offs);
      float[] loc = textLayout.getCaretInfo(hit);
      bounds.setRect(bounds.getX() + loc[0], bounds.getY(), 1,
                     bounds.getHeight());
      return bounds;
    }

    /**
     * Delegates to the text layout.
     */
    public void paint(GlyphView view, Graphics g, Shape a, int p0, int p1)
    {
      // Can't paint this with plain graphics.
      if (g instanceof Graphics2D)
        {
          Graphics2D g2d = (Graphics2D) g;
          Rectangle2D b = a instanceof Rectangle2D ? (Rectangle2D) a
                                                   : a.getBounds2D();
          float x = (float) b.getX();
          float y = (float) b.getY() + textLayout.getAscent()
                    + textLayout.getLeading();
          // TODO: Try if clipping makes things faster for narrow views.
          textLayout.draw(g2d, x, y);
        }
    }

    /**
     * Delegates to the text layout.
     */
    public int viewToModel(GlyphView v, float x, float y, Shape a,
                           Bias[] biasRet)
    {
      Rectangle2D bounds = a instanceof Rectangle2D ? (Rectangle2D) a
                                                    : a.getBounds2D();
      TextHitInfo hit = textLayout.hitTestChar(x - (float) bounds.getX(), 0);
      int pos = hit.getInsertionIndex();
      biasRet[0] = hit.isLeadingEdge() ? Position.Bias.Forward
                                       : Position.Bias.Backward;
      return pos + v.getStartOffset();
    }
    
  }

  /**
   * The default <code>GlyphPainter</code> used in <code>GlyphView</code>.
   */
  static class DefaultGlyphPainter extends GlyphPainter
  {
    FontMetrics fontMetrics;

    /**
     * Returns the full height of the rendered text.
     *
     * @return the full height of the rendered text
     */
    public float getHeight(GlyphView view)
    {
      updateFontMetrics(view);
      float height = fontMetrics.getHeight();
      return height;
    }
    
    /**
     * Paints the glyphs.
     *
     * @param view the glyph view to paint
     * @param g the graphics context to use for painting
     * @param a the allocation of the glyph view
     * @param p0 the start position (in the model) from which to paint
     * @param p1 the end position (in the model) to which to paint
     */
    public void paint(GlyphView view, Graphics g, Shape a, int p0,
                      int p1)
    {
      updateFontMetrics(view);
      Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
      TabExpander tabEx = view.getTabExpander();
      Segment txt = view.getText(p0, p1);

      // Find out the X location at which we have to paint.
      int x = r.x;
      int p = view.getStartOffset();
      if (p != p0)
        {
          int width = Utilities.getTabbedTextWidth(txt, fontMetrics,x, tabEx,
                                                   p);
          x += width;
        }
      // Find out Y location.
      int y = r.y + fontMetrics.getHeight() - fontMetrics.getDescent();

      // Render the thing.
      g.setFont(fontMetrics.getFont());
      Utilities.drawTabbedText(txt, x, y, g, tabEx, p0);

    }

    /**
     * Maps a position in the document into the coordinate space of the View.
     * The output rectangle usually reflects the font height but has a width
     * of zero.
     *
     * @param view the glyph view
     * @param pos the position of the character in the model
     * @param a the area that is occupied by the view
     * @param b either {@link Position.Bias#Forward} or
     *        {@link Position.Bias#Backward} depending on the preferred
     *        direction bias. If <code>null</code> this defaults to
     *        <code>Position.Bias.Forward</code>
     *
     * @return a rectangle that gives the location of the document position
     *         inside the view coordinate space
     *
     * @throws BadLocationException if <code>pos</code> is invalid
     * @throws IllegalArgumentException if b is not one of the above listed
     *         valid values
     */
    public Shape modelToView(GlyphView view, int pos, Position.Bias b,
                             Shape a)
      throws BadLocationException
    {
      updateFontMetrics(view);
      Element el = view.getElement();
      Segment txt = view.getText(el.getStartOffset(), pos);
      Rectangle bounds = a instanceof Rectangle ? (Rectangle) a
                                                : a.getBounds();
      TabExpander expander = view.getTabExpander();
      int width = Utilities.getTabbedTextWidth(txt, fontMetrics, bounds.x,
                                               expander,
                                               view.getStartOffset());
      int height = fontMetrics.getHeight();
      Rectangle result = new Rectangle(bounds.x + width, bounds.y,
                                       0, height);
      return result;
    }

    /**
     * Determine the span of the glyphs from location <code>p0</code> to
     * location <code>p1</code>. If <code>te</code> is not <code>null</code>,
     * then TABs are expanded using this <code>TabExpander</code>.
     * The parameter <code>x</code> is the location at which the view is
     * located (this is important when using TAB expansion).
     *
     * @param view the glyph view
     * @param p0 the starting location in the document model
     * @param p1 the end location in the document model
     * @param te the tab expander to use
     * @param x the location at which the view is located
     *
     * @return the span of the glyphs from location <code>p0</code> to
     *         location <code>p1</code>, possibly using TAB expansion
     */
    public float getSpan(GlyphView view, int p0, int p1,
                         TabExpander te, float x)
    {
      updateFontMetrics(view);
      Segment txt = view.getText(p0, p1);
      int span = Utilities.getTabbedTextWidth(txt, fontMetrics, (int) x, te,
                                              p0);
      return span;
    }

    /**
     * Returns the ascent of the text run that is rendered by this
     * <code>GlyphPainter</code>.
     *
     * @param v the glyph view
     *
     * @return the ascent of the text run that is rendered by this
     *         <code>GlyphPainter</code>
     *
     * @see FontMetrics#getAscent()
     */
    public float getAscent(GlyphView v)
    {
      updateFontMetrics(v);
      return fontMetrics.getAscent();
    }

    /**
     * Returns the descent of the text run that is rendered by this
     * <code>GlyphPainter</code>.
     *
     * @param v the glyph view
     *
     * @return the descent of the text run that is rendered by this
     *         <code>GlyphPainter</code>
     *
     * @see FontMetrics#getDescent()
     */
    public float getDescent(GlyphView v)
    {
      updateFontMetrics(v);
      return fontMetrics.getDescent();
    }

    /**
     * Determines the model offset, so that the text between <code>p0</code>
     * and this offset fits within the span starting at <code>x</code> with
     * the length of <code>len</code>. 
     *
     * @param v the glyph view
     * @param p0 the starting offset in the model
     * @param x the start location in the view
     * @param len the length of the span in the view
     */
    public int getBoundedPosition(GlyphView v, int p0, float x, float len)
    {
      updateFontMetrics(v);
      TabExpander te = v.getTabExpander();
      Segment txt = v.getText(p0, v.getEndOffset());
      int pos = Utilities.getTabbedTextOffset(txt, fontMetrics, (int) x,
                                              (int) (x + len), te, p0, false);
      return pos + p0;
    }

    /**
     * Maps a visual position into a document location.
     *
     * @param v the glyph view
     * @param x the X coordinate of the visual position
     * @param y the Y coordinate of the visual position
     * @param a the allocated region
     * @param biasRet filled with the bias of the model location on method exit
     *
     * @return the model location that represents the specified view location
     */
    public int viewToModel(GlyphView v, float x, float y, Shape a,
                           Bias[] biasRet)
    {
      Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
      int p0 = v.getStartOffset();
      int p1 = v.getEndOffset();
      TabExpander te = v.getTabExpander();
      Segment s = v.getText(p0, p1);
      int offset = Utilities.getTabbedTextOffset(s, fontMetrics, r.x, (int) x,
                                                 te, p0);
      int ret = p0 + offset;
      if (ret == p1)
        ret--;
      biasRet[0] = Position.Bias.Forward;
      return ret;
    }

    private void updateFontMetrics(GlyphView v)
    {
      Font font = v.getFont();
      if (fontMetrics == null || ! font.equals(fontMetrics.getFont()))
        {
          Container c = v.getContainer();
          FontMetrics fm;
          if (c != null)
            fm = c.getFontMetrics(font);
          else
            fm = Toolkit.getDefaultToolkit().getFontMetrics(font);
          fontMetrics = fm;
        }
    }
  }

  /**
   * The GlyphPainer used for painting the glyphs.
   */
  GlyphPainter glyphPainter;

  /**
   * The start offset within the document for this view.
   */
  private int offset;

  /**
   * The end offset within the document for this view.
   */
  private int length;

  /**
   * The x location against which the tab expansion is done.
   */
  private float tabX;

  /**
   * The tab expander that is used in this view.
   */
  private TabExpander tabExpander;

  /**
   * Creates a new <code>GlyphView</code> for the given <code>Element</code>.
   *
   * @param element the element that is rendered by this GlyphView
   */
  public GlyphView(Element element)
  {
    super(element);
    offset = 0;
    length = 0;
  }

  /**
   * Returns the <code>GlyphPainter</code> that is used by this
   * <code>GlyphView</code>. If no <code>GlyphPainer</code> has been installed
   * <code>null</code> is returned.
   *
   * @return the glyph painter that is used by this
   *         glyph view or <code>null</code> if no glyph painter has been
   *         installed
   */
  public GlyphPainter getGlyphPainter()
  {
    return glyphPainter;
  }

  /**
   * Sets the {@link GlyphPainter} to be used for this <code>GlyphView</code>.
   *
   * @param painter the glyph painter to be used for this glyph view
   */
  public void setGlyphPainter(GlyphPainter painter)
  {
    glyphPainter = painter;
  }

  /**
   * Checks if a <code>GlyphPainer</code> is installed. If this is not the
   * case, a default painter is installed.
   */
  protected void checkPainter()
  {
    if (glyphPainter == null)
      {
        if ("true".equals(
                 SystemProperties.getProperty("gnu.javax.swing.noGraphics2D")))
          {
            glyphPainter = new DefaultGlyphPainter();
          }
        else
          {
            Segment s = getText(getStartOffset(), getEndOffset());
            glyphPainter = new J2DGlyphPainter(s.toString(), getFont(),
                                               new FontRenderContext(null,
                                                                     false,
                                                                     false));
          }
      }
  }

  /**
   * Renders the <code>Element</code> that is associated with this
   * <code>View</code>.
   *
   * @param g the <code>Graphics</code> context to render to
   * @param a the allocated region for the <code>Element</code>
   */
  public void paint(Graphics g, Shape a)
  {
    checkPainter();
    int p0 = getStartOffset();
    int p1 = getEndOffset();

    Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
    Container c = getContainer();

    Color fg = getForeground();
    JTextComponent tc = null;
    if (c instanceof JTextComponent)
      {
        tc = (JTextComponent) c;
        if (! tc.isEnabled())
          fg = tc.getDisabledTextColor();
      }
    Color bg = getBackground();
    if (bg != null)
      {
        g.setColor(bg);
        System.err.println("fill background: " + bg);
        g.fillRect(r.x, r.y, r.width, r.height);
      }

    
    // Paint layered highlights if there are any.
    if (tc != null)
      {
        Highlighter h = tc.getHighlighter();
        if (h instanceof LayeredHighlighter)
          {
            LayeredHighlighter lh = (LayeredHighlighter) h;
            lh.paintLayeredHighlights(g, p0, p1, a, tc, this);
          }
      }

    g.setColor(fg);
    glyphPainter.paint(this, g, a, p0, p1);
    boolean underline = isUnderline();
    boolean striked = isStrikeThrough();
    if (underline || striked)
      {
        View parent = getParent();
        // X coordinate.
        if (parent != null && parent.getEndOffset() == p1)
          {
            // Strip whitespace.
            Segment s = getText(p0, p1);
            while (s.count > 0 && Character.isWhitespace(s.array[s.count - 1]))
              {
                p1--;
                s.count--;
              }
          }
        int x0 = r.x;
        int p = getStartOffset();
        TabExpander tabEx = getTabExpander();
        if (p != p0)
          x0 += (int) glyphPainter.getSpan(this, p, p0, tabEx, x0);
        int x1 = x0 + (int) glyphPainter.getSpan(this, p0, p1, tabEx, x0);
        // Y coordinate.
        int y = r.y + r.height - (int) glyphPainter.getDescent(this);
        if (underline)
          {
            int yTmp = y;
            yTmp += 1;
            g.drawLine(x0, yTmp, x1, yTmp);
          }
        if (striked)
          {
            int yTmp = y;
            yTmp -= (int) glyphPainter.getAscent(this);
            g.drawLine(x0, yTmp, x1, yTmp);
          }
      }
  }


  /**
   * Returns the preferred span of the content managed by this
   * <code>View</code> along the specified <code>axis</code>.
   *
   * @param axis the axis
   *
   * @return the preferred span of this <code>View</code>.
   */
  public float getPreferredSpan(int axis)
  {
    float span = 0;
    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    switch (axis)
      {
      case X_AXIS:
        TabExpander tabEx = null;
        View parent = getParent();
        if (parent instanceof TabExpander)
          tabEx = (TabExpander) parent;
        span = painter.getSpan(this, getStartOffset(), getEndOffset(),
                               tabEx, 0.F);
        break;
      case Y_AXIS:
        span = painter.getHeight(this);
        if (isSuperscript())
          span += span / 3;
        break;
      default:
        throw new IllegalArgumentException("Illegal axis");
      }
    return span;
  }

  /**
   * Maps a position in the document into the coordinate space of the View.
   * The output rectangle usually reflects the font height but has a width
   * of zero.
   *
   * @param pos the position of the character in the model
   * @param a the area that is occupied by the view
   * @param b either {@link Position.Bias#Forward} or
   *        {@link Position.Bias#Backward} depending on the preferred
   *        direction bias. If <code>null</code> this defaults to
   *        <code>Position.Bias.Forward</code>
   *
   * @return a rectangle that gives the location of the document position
   *         inside the view coordinate space
   *
   * @throws BadLocationException if <code>pos</code> is invalid
   * @throws IllegalArgumentException if b is not one of the above listed
   *         valid values
   */
  public Shape modelToView(int pos, Shape a, Position.Bias b)
    throws BadLocationException
  {
    GlyphPainter p = getGlyphPainter();
    return p.modelToView(this, pos, b, a);
  }

  /**
   * Maps coordinates from the <code>View</code>'s space into a position
   * in the document model.
   *
   * @param x the x coordinate in the view space
   * @param y the y coordinate in the view space
   * @param a the allocation of this <code>View</code>
   * @param b the bias to use
   *
   * @return the position in the document that corresponds to the screen
   *         coordinates <code>x, y</code>
   */
  public int viewToModel(float x, float y, Shape a, Position.Bias[] b)
  {
    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    return painter.viewToModel(this, x, y, a, b);
  }

  /**
   * Return the {@link TabExpander} to use.
   *
   * @return the {@link TabExpander} to use
   */
  public TabExpander getTabExpander()
  {
    return tabExpander;
  }

  /**
   * Returns the preferred span of this view for tab expansion.
   *
   * @param x the location of the view
   * @param te the tab expander to use
   *
   * @return the preferred span of this view for tab expansion
   */
  public float getTabbedSpan(float x, TabExpander te)
  {
    checkPainter();
    TabExpander old = tabExpander;
    tabExpander = te;
    if (tabExpander != old)
      {
        // Changing the tab expander will lead to a relayout in the X_AXIS.
        preferenceChanged(null, true, false);
      }
    tabX = x;
    return getGlyphPainter().getSpan(this, getStartOffset(),
                                     getEndOffset(), tabExpander, x);
  }

  /**
   * Returns the span of a portion of the view. This is used in TAB expansion
   * for fragments that don't contain TABs.
   *
   * @param p0 the start index
   * @param p1 the end index
   *
   * @return the span of the specified portion of the view
   */
  public float getPartialSpan(int p0, int p1)
  {
    checkPainter();
    return glyphPainter.getSpan(this, p0, p1, tabExpander, tabX);
  }

  /**
   * Returns the start offset in the document model of the portion
   * of text that this view is responsible for.
   *
   * @return the start offset in the document model of the portion
   *         of text that this view is responsible for
   */
  public int getStartOffset()
  {
    Element el = getElement();
    int offs = el.getStartOffset();
    if (length > 0)
      offs += offset;
    return offs;
  }

  /**
   * Returns the end offset in the document model of the portion
   * of text that this view is responsible for.
   *
   * @return the end offset in the document model of the portion
   *         of text that this view is responsible for
   */
  public int getEndOffset()
  {
    Element el = getElement();
    int offs;
    if (length > 0)
      offs = el.getStartOffset() + offset + length;
    else
      offs = el.getEndOffset();
    return offs;
  }

  private Segment cached = new Segment();

  /**
   * Returns the text segment that this view is responsible for.
   *
   * @param p0 the start index in the document model
   * @param p1 the end index in the document model
   *
   * @return the text segment that this view is responsible for
   */
  public Segment getText(int p0, int p1)
  {
    try
      {
        getDocument().getText(p0, p1 - p0, cached);
      }
    catch (BadLocationException ex)
      {
	AssertionError ae;
        ae = new AssertionError("BadLocationException should not be "
				+ "thrown here. p0 = " + p0 + ", p1 = " + p1);
	ae.initCause(ex);
	throw ae;
      }

    return cached;
  }

  /**
   * Returns the font for the text run for which this <code>GlyphView</code>
   * is responsible.
   *
   * @return the font for the text run for which this <code>GlyphView</code>
   *         is responsible
   */
  public Font getFont()
  {
    Document doc = getDocument();
    Font font = null;
    if (doc instanceof StyledDocument)
      {
        StyledDocument styledDoc = (StyledDocument) doc;
        font = styledDoc.getFont(getAttributes());
      }
    else
      {
        Container c = getContainer();
        if (c != null)
          font = c.getFont();
      }
    return font;
  }

  /**
   * Returns the foreground color which should be used to paint the text.
   * This is fetched from the associated element's text attributes using
   * {@link StyleConstants#getForeground}.
   *
   * @return the foreground color which should be used to paint the text
   */
  public Color getForeground()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    return StyleConstants.getForeground(atts);
  }

  /**
   * Returns the background color which should be used to paint the text.
   * This is fetched from the associated element's text attributes using
   * {@link StyleConstants#getBackground}.
   *
   * @return the background color which should be used to paint the text
   */
  public Color getBackground()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    // We cannot use StyleConstants.getBackground() here, because that returns
    // BLACK as default (when background == null). What we need is the
    // background setting of the text component instead, which is what we get
    // when background == null anyway.
    return (Color) atts.getAttribute(StyleConstants.Background);
  }

  /**
   * Determines whether the text should be rendered strike-through or not. This
   * is determined using the method
   * {@link StyleConstants#isStrikeThrough(AttributeSet)} on the element of
   * this view.
   *
   * @return whether the text should be rendered strike-through or not
   */
  public boolean isStrikeThrough()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    return StyleConstants.isStrikeThrough(atts);
  }

  /**
   * Determines whether the text should be rendered as subscript or not. This
   * is determined using the method
   * {@link StyleConstants#isSubscript(AttributeSet)} on the element of
   * this view.
   *
   * @return whether the text should be rendered as subscript or not
   */
  public boolean isSubscript()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    return StyleConstants.isSubscript(atts);
  }

  /**
   * Determines whether the text should be rendered as superscript or not. This
   * is determined using the method
   * {@link StyleConstants#isSuperscript(AttributeSet)} on the element of
   * this view.
   *
   * @return whether the text should be rendered as superscript or not
   */
  public boolean isSuperscript()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    return StyleConstants.isSuperscript(atts);
  }

  /**
   * Determines whether the text should be rendered as underlined or not. This
   * is determined using the method
   * {@link StyleConstants#isUnderline(AttributeSet)} on the element of
   * this view.
   *
   * @return whether the text should be rendered as underlined or not
   */
  public boolean isUnderline()
  {
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    return StyleConstants.isUnderline(atts);
  }

  /**
   * Creates and returns a shallow clone of this GlyphView. This is used by
   * the {@link #createFragment} and {@link #breakView} methods.
   *
   * @return a shallow clone of this GlyphView
   */
  protected final Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException ex)
      {
        AssertionError err = new AssertionError("CloneNotSupportedException "
                                                + "must not be thrown here");
        err.initCause(ex);
        throw err;
      }
  }

  /**
   * Tries to break the view near the specified view span <code>len</code>.
   * The glyph view can only be broken in the X direction. For Y direction it
   * returns itself.
   *
   * @param axis the axis for breaking, may be {@link View#X_AXIS} or
   *        {@link View#Y_AXIS}
   * @param p0 the model location where the fragment should start
   * @param pos the view position along the axis where the fragment starts
   * @param len the desired length of the fragment view
   *
   * @return the fragment view, or <code>this</code> if breaking was not
   *         possible
   */
  public View breakView(int axis, int p0, float pos, float len)
  {
    View brokenView = this;
    if (axis == X_AXIS)
      {
        checkPainter();
        int end = glyphPainter.getBoundedPosition(this, p0, pos, len);
        int breakLoc = getBreakLocation(p0, end);
        if (breakLoc != -1)
          end = breakLoc;
        if (p0 != getStartOffset() || end != getEndOffset())
          {
            brokenView = createFragment(p0, end);
            if (brokenView instanceof GlyphView)
              ((GlyphView) brokenView).tabX = pos;
          }
      }
    return brokenView;
  }

  /**
   * Determines how well the specified view location is suitable for inserting
   * a line break. If <code>axis</code> is <code>View.Y_AXIS</code>, then
   * this method forwards to the superclass, if <code>axis</code> is
   * <code>View.X_AXIS</code> then this method returns
   * {@link View#ExcellentBreakWeight} if there is a suitable break location
   * (usually whitespace) within the specified view span, or
   * {@link View#GoodBreakWeight} if not.
   *
   * @param axis the axis along which the break weight is requested
   * @param pos the starting view location
   * @param len the length of the span at which the view should be broken
   *
   * @return the break weight
   */
  public int getBreakWeight(int axis, float pos, float len)
  {
    int weight;
    if (axis == Y_AXIS)
      weight = super.getBreakWeight(axis, pos, len);
    else
      {
        checkPainter();
        int start = getStartOffset();
        int end = glyphPainter.getBoundedPosition(this, start, pos, len);
        if (end == 0)
          weight = BadBreakWeight;
        else
          {
            if (getBreakLocation(start, end) != -1)
              weight = ExcellentBreakWeight;
            else
              weight = GoodBreakWeight;
          }
      }
    return weight;
  }

  private int getBreakLocation(int start, int end)
  {
    int loc = -1;
    Segment s = getText(start, end);
    for (char c = s.last(); c != Segment.DONE && loc == -1; c = s.previous())
      {
        if (Character.isWhitespace(c))
          {
            loc = s.getIndex() - s.getBeginIndex() + 1 + start;
          }
      }
    return loc;
  }

  /**
   * Receives notification that some text attributes have changed within the
   * text fragment that this view is responsible for. This calls
   * {@link View#preferenceChanged(View, boolean, boolean)} on the parent for
   * both width and height.
   *
   * @param e the document event describing the change; not used here
   * @param a the view allocation on screen; not used here
   * @param vf the view factory; not used here
   */
  public void changedUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    preferenceChanged(null, true, true);
  }

  /**
   * Receives notification that some text has been inserted within the
   * text fragment that this view is responsible for. This calls
   * {@link View#preferenceChanged(View, boolean, boolean)} for the
   * direction in which the glyphs are rendered.
   *
   * @param e the document event describing the change; not used here
   * @param a the view allocation on screen; not used here
   * @param vf the view factory; not used here
   */
  public void insertUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    preferenceChanged(null, true, false);
  }

  /**
   * Receives notification that some text has been removed within the
   * text fragment that this view is responsible for. This calls
   * {@link View#preferenceChanged(View, boolean, boolean)} on the parent for
   * width.
   *
   * @param e the document event describing the change; not used here
   * @param a the view allocation on screen; not used here
   * @param vf the view factory; not used here
   */
  public void removeUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    preferenceChanged(null, true, false);
  }

  /**
   * Creates a fragment view of this view that starts at <code>p0</code> and
   * ends at <code>p1</code>.
   *
   * @param p0 the start location for the fragment view
   * @param p1 the end location for the fragment view
   *
   * @return the fragment view
   */
  public View createFragment(int p0, int p1)
  {
    checkPainter();
    Element el = getElement();
    GlyphView fragment = (GlyphView) clone();
    fragment.offset = p0 - el.getStartOffset();
    fragment.length = p1 - p0;
    fragment.glyphPainter = glyphPainter.getPainter(fragment, p0, p1);
    return fragment;
  }

  /**
   * Returns the alignment of this view along the specified axis. For the Y
   * axis this is <code>(height - descent) / height</code> for the used font,
   * so that it is aligned along the baseline.
   * For the X axis the superclass is called.
   */
  public float getAlignment(int axis)
  {
    checkPainter();
    float align;
    if (axis == Y_AXIS)
      {
        GlyphPainter painter = getGlyphPainter();
        float height = painter.getHeight(this);
        float descent = painter.getDescent(this);
        float ascent = painter.getAscent(this);
        if (isSuperscript())
          align = 1.0F;
        else if (isSubscript())
          align = height > 0 ? (height - (descent + (ascent / 2))) / height
                             : 0;
        else
          align = height > 0 ? (height - descent) / height : 0;
      }
    else
      align = super.getAlignment(axis);

    return align;
  }

  /**
   * Returns the model location that should be used to place a caret when
   * moving the caret through the document.
   *
   * @param pos the current model location
   * @param bias the bias for <code>p</code>
   * @param a the allocated region for the glyph view
   * @param direction the direction from the current position; Must be one of
   *        {@link SwingConstants#EAST}, {@link SwingConstants#WEST},
   *        {@link SwingConstants#NORTH} or {@link SwingConstants#SOUTH}
   * @param biasRet filled with the bias of the resulting location when method
   *        returns
   *
   * @return the location within the document that should be used to place the
   *         caret when moving the caret around the document
   *
   * @throws BadLocationException if <code>pos</code> is an invalid model
   *         location
   * @throws IllegalArgumentException if <code>d</code> is invalid
   */
  public int getNextVisualPositionFrom(int pos, Position.Bias bias, Shape a,
                                       int direction, Position.Bias[] biasRet)
    throws BadLocationException
  {
    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    return painter.getNextVisualPositionFrom(this, pos, bias, a, direction,
                                             biasRet);
  }
}

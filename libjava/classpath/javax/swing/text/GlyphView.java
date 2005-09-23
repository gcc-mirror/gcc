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

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

/**
 * Renders a run of styled text. This {@link View} subclass paints the
 * characters of the <code>Element</code> it is responsible for using
 * the style information from that <code>Element</code>.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class GlyphView
  extends View
  implements TabableView, Cloneable
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
    }

    /**
     * Returns the full height of the rendered text.
     *
     * @return the full height of the rendered text
     */
    public abstract float getHeight(GlyphView view);
    
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
     * @param bias either {@link Position.Bias.Forward} or
     *        {@link Position.Bias.Backward} depending on the preferred
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
     * Determine the span of the glyphs from location <code>p0</code> to
     * location <code>p1</code>. If <code>te</code> is not <code>null</code>,
     * then TABs are expanded using this <code>TabExpander</code>.
     * The parameter <code>x</code> is the location at which the view is
     * located (this is important when using TAB expansion).
     *
     * @param view the glyph view
     * @param p0 the starting location in the document model
     * @param p0 the end location in the document model
     * @param te the tab expander to use
     * @param x the location at which the view is located
     *
     * @return the span of the glyphs from location <code>p0</code> to
     *         location <code>p1</code>, possibly using TAB expansion
     */
    public abstract float getSpan(GlyphView view, int p0, int p1,
                                  TabExpander te, float x);

  }

  /**
   * The default <code>GlyphPainter</code> used in <code>GlyphView</code>.
   */
  static class DefaultGlyphPainter extends GlyphPainter
  {
    /**
     * Returns the full height of the rendered text.
     *
     * @return the full height of the rendered text
     */
    public float getHeight(GlyphView view)
    {
      Font font = view.getFont();
      FontMetrics metrics = view.getContainer().getFontMetrics(font);
      float height = metrics.getHeight();
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
      int height = (int) getHeight(view);
      Segment txt = view.getText(p0, p1);
      Rectangle bounds = a.getBounds();

      TabExpander tabEx = null;
      View parent = view.getParent();
      if (parent instanceof TabExpander)
        tabEx = (TabExpander) parent;

      // FIXME: Set character attributes like font-family, font-size, colors.
      Color foreground = view.getForeground();
      g.setColor(foreground);
      Utilities.drawTabbedText(txt, bounds.x, bounds.y, g, tabEx,
                               txt.offset);
    }

    /**
     * Maps a position in the document into the coordinate space of the View.
     * The output rectangle usually reflects the font height but has a width
     * of zero.
     *
     * @param view the glyph view
     * @param pos the position of the character in the model
     * @param a the area that is occupied by the view
     * @param bias either {@link Position.Bias.Forward} or
     *        {@link Position.Bias.Backward} depending on the preferred
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
      Element el = view.getElement();
      Font font = view.getFont();
      FontMetrics fm = view.getContainer().getFontMetrics(font);
      Segment txt = view.getText(el.getStartOffset(), pos);
      int width = fm.charsWidth(txt.array, txt.offset, txt.count);
      int height = fm.getHeight();
      Rectangle bounds = a.getBounds();
      Rectangle result = new Rectangle(bounds.x + width, bounds.y,
                                       bounds.x + width, height);
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
     * @param p0 the end location in the document model
     * @param te the tab expander to use
     * @param x the location at which the view is located
     *
     * @return the span of the glyphs from location <code>p0</code> to
     *         location <code>p1</code>, possibly using TAB expansion
     */
    public float getSpan(GlyphView view, int p0, int p1,
                         TabExpander te, float x)
    {
      Element el = view.getElement();
      Font font = view.getFont();
      FontMetrics fm = view.getContainer().getFontMetrics(font);
      Segment txt = view.getText(p0, p1);
      int span = Utilities.getTabbedTextWidth(txt, fm, (int) x, te, p0);
      return span;
    }
  }

  /**
   * The GlyphPainer used for painting the glyphs.
   */
  GlyphPainter glyphPainter;

  /**
   * Creates a new <code>GlyphView</code> for the given <code>Element</code>.
   *
   * @param element the element that is rendered by this GlyphView
   */
  public GlyphView(Element element)
  {
    super(element);
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
      glyphPainter = new DefaultGlyphPainter();
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
    Element el = getElement();
    checkPainter();
    getGlyphPainter().paint(this, g, a, el.getStartOffset(),
                            el.getEndOffset());
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
    Element el = getElement();
    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    TabExpander tabEx = null;
    View parent = getParent();
    if (parent instanceof TabExpander)
      tabEx = (TabExpander) parent;
    // FIXME: Figure out how to determine the x parameter.
    float span = painter.getSpan(this, el.getStartOffset(), el.getEndOffset(),
                                 tabEx, 0.F);
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
    // FIXME: not implemented
    return 0;
  }

  /**
   * Return the {@link TabExpander} to use.
   *
   * @return the {@link TabExpander} to use
   */
  public TabExpander getTabExpander()
  {
    // TODO: Figure out if this is correct.
    TabExpander te = null;
    View parent = getParent();

    if (parent instanceof ParagraphView)
      te = (ParagraphView) parent;
    
    return te;
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
    Element el = getElement();
    return getGlyphPainter().getSpan(this, el.getStartOffset(),
                                     el.getEndOffset(), te, x);
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
    Element el = getElement();
    Document doc = el.getDocument();
    Segment seg = new Segment();
    try
      {
        doc.getText(p0, p1 - p0, seg);
      }
    catch (BadLocationException ex)
      {
        throw new AssertionError("BadLocationException must not be thrown "
                                 + "here");
      }
    FontMetrics fm = null; // Fetch font metrics somewhere.
    return Utilities.getTabbedTextWidth(seg, fm, 0, null, p0);
  }

  /**
   * Returns the starting offset in the document model of the portion
   * of text that this view is responsible for.
   *
   * @return the starting offset in the document model of the portion
   *         of text that this view is responsible for
   */
  public int getBeginIndex()
  {
    return getElement().getStartOffset();
  }

  /**
   * Returns the end offset in the document model of the portion
   * of text that this view is responsible for.
   *
   * @return the end offset in the document model of the portion
   *         of text that this view is responsible for
   */
  public int getEndIndex()
  {
    return getElement().getEndOffset();
  }

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
    Segment txt = new Segment();
    try
      {
        getDocument().getText(p0, p1 - p0, txt);
      }
    catch (BadLocationException ex)
      {
        throw new AssertionError("BadLocationException should not be "
                                 + "thrown here. p0 = " + p0 + ", p1 = " + p1);
      }

    return txt;
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
    Element el = getElement();
    AttributeSet atts = el.getAttributes();
    String family = StyleConstants.getFontFamily(atts);
    int size = StyleConstants.getFontSize(atts);
    int style = Font.PLAIN;
    if (StyleConstants.isBold(atts))
        style |= Font.BOLD;
    if (StyleConstants.isItalic(atts))
      style |= Font.ITALIC;
    Font font = new Font(family, style, size);
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
}

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
import java.awt.Toolkit;
import java.text.BreakIterator;

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
      FontMetrics metrics = Toolkit.getDefaultToolkit().getFontMetrics(font);
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

      // Fill the background of the text run.
      Color background = view.getBackground();
      g.setColor(background);
      int width = Utilities.getTabbedTextWidth(txt, g.getFontMetrics(),
                                               bounds.x, tabEx, txt.offset);
      g.fillRect(bounds.x, bounds.y, width, height);

      // Draw the actual text.
      g.setColor(view.getForeground());
      g.setFont(view.getFont());
      if (view.isSuperscript())
        // TODO: Adjust font for superscripting.
        Utilities.drawTabbedText(txt, bounds.x, bounds.y - height / 2, g, tabEx,
                                   txt.offset);
      else if (view.isSubscript())
        // TODO: Adjust font for subscripting.
        Utilities.drawTabbedText(txt, bounds.x, bounds.y + height / 2, g, tabEx,
                                 txt.offset);
      else
        Utilities.drawTabbedText(txt, bounds.x, bounds.y, g, tabEx,
                                 txt.offset);

      if (view.isStikeThrough())
        {
          int strikeHeight = (int) (getAscent(view) / 2);
          g.drawLine(bounds.x, bounds.y + strikeHeight, bounds.height + width,
                     bounds.y + strikeHeight);
        }
      if (view.isUnderline())
        {
          int lineHeight = (int) getAscent(view);
          g.drawLine(bounds.x, bounds.y + lineHeight, bounds.height + width,
                     bounds.y + lineHeight);
        }
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
      Element el = view.getElement();
      Font font = view.getFont();
      FontMetrics fm = Toolkit.getDefaultToolkit().getFontMetrics(font);
      Segment txt = view.getText(p0, p1);
      int span = Utilities.getTabbedTextWidth(txt, fm, (int) x, te, p0);
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
      Font font = v.getFont();
      FontMetrics fm = v.getContainer().getFontMetrics(font);
      return fm.getAscent();
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
      Font font = v.getFont();
      FontMetrics fm = v.getContainer().getFontMetrics(font);
      return fm.getDescent();
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
      TabExpander te = v.getTabExpander();
      Segment txt = v.getText(p0, v.getEndOffset());
      Font font = v.getFont();
      FontMetrics fm = v.getContainer().getFontMetrics(font);
      int pos = Utilities.getTabbedTextOffset(txt, fm, (int) x,
                                              (int) (x + len), te, p0, false);
      return pos;
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
      Rectangle b = a.getBounds();
      int pos = getBoundedPosition(v, v.getStartOffset(), b.x, x - b.x);
      return pos;
    }
  }

  /**
   * The GlyphPainer used for painting the glyphs.
   */
  GlyphPainter glyphPainter;

  /**
   * The start offset within the document for this view.
   */
  int startOffset;

  /**
   * The end offset within the document for this view.
   */
  int endOffset;

  /**
   * Creates a new <code>GlyphView</code> for the given <code>Element</code>.
   *
   * @param element the element that is rendered by this GlyphView
   */
  public GlyphView(Element element)
  {
    super(element);
    startOffset = element.getStartOffset();
    endOffset = element.getEndOffset();
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
    float span = 0;
    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    if (axis == X_AXIS)
      {
        Element el = getElement();
        TabExpander tabEx = null;
        View parent = getParent();
        if (parent instanceof TabExpander)
          tabEx = (TabExpander) parent;
        span = painter.getSpan(this, getStartOffset(), getEndOffset(),
                               tabEx, 0.F);
      }
    else
      span = painter.getHeight(this); 
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
    TabExpander te = null;
    View parent = getParent();

    if (parent instanceof TabExpander)
      te = (TabExpander) parent;
    
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
	AssertionError ae;
        ae = new AssertionError("BadLocationException must not be thrown "
				+ "here");
	ae.initCause(ex);
	throw ae;
      }
    FontMetrics fm = null; // Fetch font metrics somewhere.
    return Utilities.getTabbedTextWidth(seg, fm, 0, null, p0);
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
    return startOffset;
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
    return endOffset;
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
	AssertionError ae;
        ae = new AssertionError("BadLocationException should not be "
				+ "thrown here. p0 = " + p0 + ", p1 = " + p1);
	ae.initCause(ex);
	throw ae;
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
    return StyleConstants.getBackground(atts);
  }

  /**
   * Determines whether the text should be rendered strike-through or not. This
   * is determined using the method
   * {@link StyleConstants#isStrikeThrough(AttributeSet)} on the element of
   * this view.
   *
   * @return whether the text should be rendered strike-through or not
   */
  public boolean isStikeThrough()
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
    if (axis == Y_AXIS)
      return this;

    checkPainter();
    GlyphPainter painter = getGlyphPainter();
    int breakLocation = painter.getBoundedPosition(this, p0, pos, len);
    // Try to find a suitable line break.
    BreakIterator lineBreaker = BreakIterator.getLineInstance();
    Segment txt = new Segment();
    try
      {
        getDocument().getText(getStartOffset(), getEndOffset(), txt);
      }
    catch (BadLocationException ex)
      {
        AssertionError err = new AssertionError("BadLocationException must not "
                                                + "be thrown here.");
        err.initCause(ex);
        throw err;
      }
    lineBreaker.setText(txt);
    int goodBreakLocation = lineBreaker.previous();
    if (goodBreakLocation != BreakIterator.DONE)
      breakLocation = goodBreakLocation;

    View brokenView = createFragment(p0, breakLocation);
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
        // Determine the model locations at pos and pos + len.
        int spanX = (int) getPreferredSpan(X_AXIS);
        int spanY = (int) getPreferredSpan(Y_AXIS);
        Rectangle dummyAlloc = new Rectangle(0, 0, spanX, spanY);
        Position.Bias[] biasRet = new Position.Bias[1];
        int offset1 = viewToModel(pos, spanY / 2, dummyAlloc, biasRet);
        int offset2 = viewToModel(pos, spanY / 2, dummyAlloc, biasRet);
        Segment txt = getText(offset1, offset2);
        BreakIterator lineBreaker = BreakIterator.getLineInstance();
        lineBreaker.setText(txt);
        int breakLoc = lineBreaker.previous();
        if (breakLoc == offset1)
          weight = View.BadBreakWeight;
        else if(breakLoc ==  BreakIterator.DONE)
          weight = View.GoodBreakWeight;
        else
          weight = View.ExcellentBreakWeight;
      }
    return weight;
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
    getParent().preferenceChanged(this, true, true);
  }

  /**
   * Receives notification that some text has been inserted within the
   * text fragment that this view is responsible for. This calls
   * {@link View#preferenceChanged(View, boolean, boolean)} on the parent for
   * width.
   *
   * @param e the document event describing the change; not used here
   * @param a the view allocation on screen; not used here
   * @param vf the view factory; not used here
   */
  public void insertUpdate(DocumentEvent e, Shape a, ViewFactory vf)
  {
    getParent().preferenceChanged(this, true, false);
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
    getParent().preferenceChanged(this, true, false);
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
    GlyphView fragment = (GlyphView) clone();
    fragment.startOffset = p0;
    fragment.endOffset = p1;
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
    float align;
    if (axis == Y_AXIS)
      {
        checkPainter();
        GlyphPainter painter = getGlyphPainter();
        float height = painter.getHeight(this);
        float descent = painter.getDescent(this);
        align = (height - descent) / height; 
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

  /**
   * Returns the document position that is (visually) nearest to the given
   * document position <code>pos</code> in the given direction <code>d</code>.
   *
   * @param c the text component
   * @param pos the document position
   * @param b the bias for <code>pos</code>
   * @param d the direction, must be either {@link SwingConstants#NORTH},
   *        {@link SwingConstants#SOUTH}, {@link SwingConstants#WEST} or
   *        {@link SwingConstants#EAST}
   * @param biasRet an array of {@link Position.Bias} that can hold at least
   *        one element, which is filled with the bias of the return position
   *        on method exit
   *
   * @return the document position that is (visually) nearest to the given
   *         document position <code>pos</code> in the given direction
   *         <code>d</code>
   *
   * @throws BadLocationException if <code>pos</code> is not a valid offset in
   *         the document model
   */
  public int getNextVisualPositionFrom(JTextComponent c, int pos,
                                       Position.Bias b, int d,
                                       Position.Bias[] biasRet)
    throws BadLocationException
  {
    // TODO: Implement this properly.
    throw new AssertionError("Not implemented yet.");
  }
}

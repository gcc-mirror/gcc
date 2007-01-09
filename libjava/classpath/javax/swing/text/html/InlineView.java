/* InlineView.java -- Renders HTML content
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

import java.awt.FontMetrics;
import java.awt.Shape;
import java.text.BreakIterator;

import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.LabelView;
import javax.swing.text.Segment;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * Renders HTML content (identified by {@link HTML.Tag#CONTENT}). This is
 * basically a {@link LabelView} that is adjusted to understand styles defined
 * by stylesheets.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class InlineView
  extends LabelView
{

  /**
   * The attributes used by this view.
   */
  private AttributeSet attributes;

  /**
   * The span of the longest word in this view.
   *
   * @see #getLongestWord()
   */
  private float longestWord;

  /**
   * Indicates if we may wrap or not.
   */
  private boolean nowrap;

  /**
   * Creates a new <code>InlineView</code> that renders the specified element.
   *
   * @param element the element for this view
   */
  public InlineView(Element element)
  {
    super(element);
  }

  /**
   * Receives notification that something was inserted into the document in
   * a location that this view is responsible for.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void insertUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    // FIXME: What to do here?
    super.insertUpdate(e, a, f);
  }

  /**
   * Receives notification that something was removed from the document in
   * a location that this view is responsible for.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void removeUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    // FIXME: What to do here?
    super.removeUpdate(e, a, f);
  }

  /**
   * Receives notification that attributes have changed in the document in
   * a location that this view is responsible for. This calls
   * {@link #setPropertiesFromAttributes}.
   *
   * @param e the document event
   * @param a the current allocation of this view
   * @param f the view factory for creating new views
   *
   * @since 1.5
   */
  public void changedUpdate(DocumentEvent e, Shape a, ViewFactory f)
  {
    super.changedUpdate(e, a, f);
    StyleSheet ss = getStyleSheet();
    attributes = ss.getViewAttributes(this);
    preferenceChanged(null, true, true);
    setPropertiesFromAttributes();
  }

  /**
   * Returns the attributes that are used for rendering. This is implemented
   * to multiplex the attributes specified in the model with a stylesheet.
   *
   * @return the attributes that are used for rendering
   */
  public AttributeSet getAttributes()
  {
    if (attributes == null)
      {
        StyleSheet ss = getStyleSheet();
        attributes = ss.getViewAttributes(this);
      }
    return attributes;
  }

  
  public int getBreakWeight(int axis, float pos, float len)
  {
    int weight;
    if (nowrap)
      weight = BadBreakWeight;
    else
      weight = super.getBreakWeight(axis, pos, len);
    return weight;
  }

  public View breakView(int axis, int offset, float pos, float len)
  {
    // FIXME: Implement this.
    return super.breakView(axis, offset, pos, len);
  }

  /**
   * Loads the character style properties from the stylesheet.
   */
  protected void setPropertiesFromAttributes()
  {
    super.setPropertiesFromAttributes();
    AttributeSet atts = getAttributes();
    Object o = atts.getAttribute(CSS.Attribute.TEXT_DECORATION);

    // Check for underline.
    boolean b = false;
    if (o != null && o.toString().contains("underline"))
      b = true;
    setUnderline(b);

    // Check for line-through.
    b = false;
    if (o != null && o.toString().contains("line-through"))
      b = true;
    setStrikeThrough(b);

    // Check for vertical alignment (subscript/superscript).
    o = atts.getAttribute(CSS.Attribute.VERTICAL_ALIGN);

    // Subscript.
    b = false;
    if (o != null && o.toString().contains("sub"))
      b = true;
    setSubscript(b);

    // Superscript.
    b = false;
    if (o != null && o.toString().contains("sup"))
      b = true;
    setSuperscript(b);

    // Fetch nowrap setting.
    o = atts.getAttribute(CSS.Attribute.WHITE_SPACE);
    if (o != null && o.equals("nowrap"))
      nowrap = true;
    else
      nowrap = false;
  }

  /**
   * Returns the stylesheet used by this view. This returns the stylesheet
   * of the <code>HTMLDocument</code> that is rendered by this view.
   *
   * @return the stylesheet used by this view
   */
  protected StyleSheet getStyleSheet()
  {
    Document doc = getDocument();
    StyleSheet styleSheet = null;
    if (doc instanceof HTMLDocument)
      styleSheet = ((HTMLDocument) doc).getStyleSheet();
    return styleSheet;
  }

  /**
   * Returns the minimum span for the specified axis. This returns the
   * width of the longest word for the X axis and the super behaviour for
   * the Y axis. This is a slight deviation from the reference implementation.
   * IMO this should improve rendering behaviour so that an InlineView never
   * gets smaller than the longest word in it.
   */
  public float getMinimumSpan(int axis)
  {
    float min = super.getMinimumSpan(axis);
    if (axis == X_AXIS)
      min = Math.max(getLongestWord(), min);
    return min;
  }

  /**
   * Returns the span of the longest word in this view.
   *
   * @return the span of the longest word in this view
   */
  private float getLongestWord()
  {
    if (longestWord == -1)
      longestWord = calculateLongestWord();
    return longestWord;
  }

  /**
   * Calculates the span of the longest word in this view.
   *
   * @return the span of the longest word in this view
   */
  private float calculateLongestWord()
  {
    float span = 0;
    try
      {
        Document doc = getDocument();
        int p0 = getStartOffset();
        int p1 = getEndOffset();
        Segment s = new Segment();
        doc.getText(p0, p1 - p0, s);
        BreakIterator iter = BreakIterator.getWordInstance();
        iter.setText(s);
        int wordStart = p0;
        int wordEnd = p0;
        int start = iter.first();
        for (int end = iter.next(); end != BreakIterator.DONE;
             start = end, end = iter.next())
          {
            if ((end - start) > (wordEnd - wordStart))
              {
                wordStart = start;
                wordEnd = end;
              }
          }
        if (wordEnd - wordStart > 0)
          {
            FontMetrics fm = getFontMetrics();
            int offset = s.offset + wordStart - s.getBeginIndex();
            span = fm.charsWidth(s.array, offset, wordEnd - wordStart);
          }
      }
    catch (BadLocationException ex)
      {
        // Return 0.
      }
    return span;
  }

}

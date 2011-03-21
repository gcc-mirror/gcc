/* ParagraphView.java -- Renders a paragraph in HTML
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

import gnu.javax.swing.text.html.css.Length;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;

/**
 * Renders a paragraph in HTML. This is a subclass of
 * {@link javax.swing.text.ParagraphView} with some adjustments for
 * understanding stylesheets.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ParagraphView
  extends javax.swing.text.ParagraphView
{

  /**
   * The attributes used by this view.
   */
  private AttributeSet attributes;

  /**
   * The stylesheet's box painter.
   */
  private StyleSheet.BoxPainter painter;

  /**
   * The width as specified in the stylesheet or null if not specified.
   */
  private Length cssWidth;

  /**
   * The height as specified in the stylesheet or null if not specified.
   */
  private Length cssHeight;

  /**
   * Creates a new ParagraphView for the specified element.
   *
   * @param element the element
   */
  public ParagraphView(Element element)
  {
    super(element);
  }

  /**
   * Sets the parent of this view. This is implemented to call the parent
   * functionality and then trigger {@link #setPropertiesFromAttributes} in
   * order to load the stylesheet attributes.
   *
   * @param parent the parent view to set
   */
  public void setParent(View parent)
  {
    super.setParent(parent);
    if (parent != null)
      setPropertiesFromAttributes();
  }

  /**
   * Returns the attributes used by this view. This is implemented to multiplex
   * the attributes of the model with the attributes of the stylesheet.
   */
  public AttributeSet getAttributes()
  {
    if (attributes == null)
      {
        attributes = getStyleSheet().getViewAttributes(this);
      }
    return attributes;
  }

  /**
   * Loads the visual properties of the ParagraphView from the element's
   * attributes and the stylesheet of the HTML document.
   */
  protected void setPropertiesFromAttributes()
  {
    super.setPropertiesFromAttributes();

    // Fetch CSS attributes.
    attributes = getAttributes();
    if (attributes != null)
      {
        super.setPropertiesFromAttributes();
        Object o = attributes.getAttribute(CSS.Attribute.TEXT_ALIGN);
        if (o != null)
          {
            String align = o.toString();
            if (align.equals("left"))
              setJustification(StyleConstants.ALIGN_LEFT);
            else if (align.equals("right"))
              setJustification(StyleConstants.ALIGN_RIGHT);
            else if (align.equals("center"))
              setJustification(StyleConstants.ALIGN_CENTER);
            else if (align.equals("justify"))
              setJustification(StyleConstants.ALIGN_JUSTIFIED);
          }

        // Fetch StyleSheet's box painter.
        painter = getStyleSheet().getBoxPainter(attributes);
        setInsets((short) painter.getInset(TOP, this),
                  (short) painter.getInset(LEFT, this),
                  (short) painter.getInset(BOTTOM, this),
                  (short) painter.getInset(RIGHT, this));

        StyleSheet ss = getStyleSheet();
        float emBase = ss.getEMBase(attributes);
        float exBase = ss.getEXBase(attributes);
        cssWidth = (Length) attributes.getAttribute(CSS.Attribute.WIDTH);
        if (cssWidth != null)
          cssWidth.setFontBases(emBase, exBase);
        cssHeight = (Length) attributes.getAttribute(CSS.Attribute.WIDTH);
        if (cssHeight != null)
          cssHeight.setFontBases(emBase, exBase);
      }
  }

  /**
   * Returns the stylesheet used by this view.
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
   * Calculates the minor axis requirements of this view. This is implemented
   * to return the super class'es requirements and modifies the minimumSpan
   * slightly so that it is not smaller than the length of the longest word.
   *
   * @param axis the axis
   * @param r the SizeRequirements object to be used as return parameter;
   *        if <code>null</code> a new one will be created
   *
   * @return the requirements along the minor layout axis
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    r = super.calculateMinorAxisRequirements(axis, r);
    if (! setCSSSpan(r, axis))
      {
        int margin = axis == X_AXIS ? getLeftInset() + getRightInset()
                                    : getTopInset() + getBottomInset();
        r.minimum -= margin;
        r.preferred -= margin;
        r.maximum -= margin;
      }
    return r;
  }

  /**
   * Sets the span on the SizeRequirements object according to the
   * according CSS span value, when it is set.
   *
   * @param r the size requirements
   * @param axis the axis
   *
   * @return <code>true</code> when the CSS span has been set,
   *         <code>false</code> otherwise
   */
  private boolean setCSSSpan(SizeRequirements r, int axis)
  {
    boolean ret = false;
    if (axis == X_AXIS)
      {
        if (cssWidth != null && ! cssWidth.isPercentage())
          {
            r.minimum = (int) cssWidth.getValue();
            r.preferred = (int) cssWidth.getValue();
            r.maximum = (int) cssWidth.getValue();
            ret = true;
          }
      }
    else
      {
        if (cssHeight != null && ! cssWidth.isPercentage())
          {
            r.minimum = (int) cssHeight.getValue();
            r.preferred = (int) cssHeight.getValue();
            r.maximum = (int) cssHeight.getValue();
            ret = true;
          }
      }
    return ret;
  }

  /**
   * Determines if this view is visible or not. If none of the children is
   * visible and the only visible child is the break that ends the paragraph,
   * this paragraph is not considered to be visible.
   *
   * @return the visibility of this paragraph
   */
  public boolean isVisible()
  {
    // FIXME: Implement the above specified behaviour.
    return super.isVisible();
  }

  /**
   * Paints this view. This paints the box using the stylesheet's
   * box painter for this view and delegates to the super class paint()
   * afterwards.
   *
   * @param g the graphics object
   * @param a the current allocation of this view
   */
  public void paint(Graphics g, Shape a)
  {
    if (a != null)
      {
        Rectangle r = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
        painter.paint(g, r.x, r.y, r.width, r.height, this);
      }
    super.paint(g, a);
  }

  /**
   * Returns the preferred span of this view. If this view is not visible,
   * we return <code>0</code>, otherwise the super class is called.
   *
   * @param axis the axis
   *
   * @return the preferred span of this view
   */
  public float getPreferredSpan(int axis)
  {
    float span = 0;
    if (isVisible())
      span = super.getPreferredSpan(axis);
    return span;
  }

  /**
   * Returns the minimum span of this view. If this view is not visible,
   * we return <code>0</code>, otherwise the super class is called.
   *
   * @param axis the axis
   *
   * @return the minimum span of this view
   */
  public float getMinimumSpan(int axis)
  {
    float span = 0;
    if (isVisible())
      span = super.getMinimumSpan(axis);
    return span;
  }

  /**
   * Returns the maximum span of this view. If this view is not visible,
   * we return <code>0</code>, otherwise the super class is called.
   *
   * @param axis the axis
   *
   * @return the maximum span of this view
   */
  public float getMaximumSpan(int axis)
  {
    float span = 0;
    if (isVisible())
      span = super.getMaximumSpan(axis);
    return span;
  }
}

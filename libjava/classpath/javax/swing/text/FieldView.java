/* FieldView.java -- 
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.Container;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.BoundedRangeModel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;

public class FieldView extends PlainView
{
  BoundedRangeModel horizontalVisibility;
  
  /** Caches the preferred span of the X axis. It is invalidated by
   * setting it to -1f. This is done when text in the document
   * is inserted, removed or changed. The value is corrected as
   * soon as calculateHorizontalSpan() is called. 
   */
  float cachedSpan = -1f;

  public FieldView(Element elem)
  {
    super(elem);
    
  }
  
  /** Checks whether the given container is a JTextField. If so
   * it retrieves the textfield's horizontalVisibility instance.
   * 
   * <p>This method should be only called when the view's container
   * is valid. Naturally that would be the setParent() method however
   * that method is not overridden in the RI and that is why we chose
   * paint() instead.</p>
   */ 
  private void checkContainer()
  {
    Container c = getContainer();
    
    if (c instanceof JTextField)
      {
        horizontalVisibility = ((JTextField) c).getHorizontalVisibility();
        
        // Provokes a repaint when the BoundedRangeModel's values change
        // (which is what the RI does).
        horizontalVisibility.addChangeListener(new ChangeListener(){
          public void stateChanged(ChangeEvent event) {
            getContainer().repaint();
          }
        });

        // It turned out that the span calculated at this point is wrong
        // and needs to be recalculated (e.g. a different font setting is
        // not taken into account).
        calculateHorizontalSpan();
        
        // Initializes the BoundedRangeModel properly.
        updateVisibility();
      }
    
  }
  
  private void updateVisibility()
  {
    JTextField tf = (JTextField) getContainer();
    Insets insets = tf.getInsets();

    int width = tf.getWidth() - insets.left - insets.right;
        
    horizontalVisibility.setMaximum(Math.max((int) ((cachedSpan != -1f)
                                                 ? cachedSpan
                                                 : calculateHorizontalSpan()),
                                             width));
        
    horizontalVisibility.setExtent(width - 1);
  }

  protected FontMetrics getFontMetrics()
  {
    Component container = getContainer();
    return container.getFontMetrics(container.getFont());
  }

  /**
   * Vertically centers the single line of text within the
   * bounds of the input shape. The returned Rectangle is centered
   * vertically within <code>shape</code> and has a height of the
   * preferred span along the Y axis. Horizontal adjustment is done according
   * to the horizontalAligment property of the component that is rendered.
   *
   * @param shape the shape within which the line is beeing centered
   */
  protected Shape adjustAllocation(Shape shape)
  {
    // Return null when the original allocation is null (like the RI).
    if (shape == null)
      return null;
    
    Rectangle rectIn = shape.getBounds();
    // vertical adjustment
    int height = (int) getPreferredSpan(Y_AXIS);
    int y = rectIn.y + (rectIn.height - height) / 2;
    // horizontal adjustment
    JTextField textField = (JTextField) getContainer();
    int width = (int) ((cachedSpan != -1f) ? cachedSpan : calculateHorizontalSpan());
    int x;
    if (horizontalVisibility != null && horizontalVisibility.getExtent() < width)
        x = rectIn.x - horizontalVisibility.getValue();
    else
      switch (textField.getHorizontalAlignment())
        {
        case JTextField.CENTER:
          x = rectIn.x + (rectIn.width - width) / 2;
          break;
        case JTextField.RIGHT:
          x = rectIn.x + (rectIn.width - width - 1);
          break;
        case JTextField.TRAILING:
          if (textField.getComponentOrientation().isLeftToRight())
            x = rectIn.x + (rectIn.width - width - 1);
          else
            x = rectIn.x;
          break;
        case JTextField.LEADING:
          if (textField.getComponentOrientation().isLeftToRight())
            x = rectIn.x;
          else
            x = rectIn.x + (rectIn.width - width - 1);
          break;
        case JTextField.LEFT:
        default:
          x = rectIn.x;
          break;
        }
    
    return new Rectangle(x, y, width, height);
  }

  public float getPreferredSpan(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException();


    if (axis == Y_AXIS)
      return super.getPreferredSpan(axis);

    if (cachedSpan != -1f)
      return cachedSpan;
    
    return calculateHorizontalSpan();
  }
  
  /** Calculates and sets the horizontal span and stores the value
   * in cachedSpan.
   */ 
  private float calculateHorizontalSpan()
  {
    Segment s = getLineBuffer();
    Element elem = getElement();

    try
      {
        elem.getDocument().getText(elem.getStartOffset(),
                                          elem.getEndOffset() - 1,
                                          s);
        
        return cachedSpan = Utilities.getTabbedTextWidth(s, getFontMetrics(), 0, this, s.offset);
      }
    catch (BadLocationException e)
      {
	// Should never happen
	AssertionError ae = new AssertionError();
	ae.initCause(e);
	throw ae;
      }
  }

  public int getResizeWeight(int axis)
  {
    return axis == X_AXIS ? 1 : 0;
  }
  
  public Shape modelToView(int pos, Shape a, Position.Bias bias)
    throws BadLocationException
  {
    Shape newAlloc = adjustAllocation(a);
    return super.modelToView(pos, newAlloc, bias);
  }
  
  public void paint(Graphics g, Shape s)
  {
    if (horizontalVisibility == null)
      checkContainer();

    Shape newAlloc = adjustAllocation(s);
    
    Shape clip = g.getClip();
    if (clip != null)
      {
        // Reason for this: The allocation area is always determined by the
        // size of the component (and its insets) regardless of whether
        // parts of the component are invisible or not (e.g. when the
        // component is part of a JScrollPane and partly moved out of
        // the user-visible range). However the clip of the Graphics
        // instance may be adjusted properly to that condition but
        // does not handle insets. By calculating the intersection
        // we get the correct clip to paint the text in all cases.
        Rectangle r = s.getBounds();
        Rectangle cb = clip.getBounds();
        SwingUtilities.computeIntersection(r.x, r.y, r.width, r.height, cb);

        g.setClip(cb);
      }
    else
      g.setClip(s);

    super.paint(g, newAlloc);
    g.setClip(clip);
    
  }

  public void insertUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    cachedSpan = -1f;
    
    if (horizontalVisibility != null)
      updateVisibility();
    
    Shape newAlloc = adjustAllocation(shape);
    
    super.insertUpdate(ev, newAlloc, vf);
    getContainer().repaint();
  }

  public void removeUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    cachedSpan = -1f;
    
    if (horizontalVisibility != null)
      updateVisibility();

    Shape newAlloc = adjustAllocation(shape);
    super.removeUpdate(ev, newAlloc, vf);
    getContainer().repaint();
  }

  public void changedUpdate(DocumentEvent ev, Shape shape, ViewFactory vf)
  {
    cachedSpan = -1f;
    
    if (horizontalVisibility != null)
      updateVisibility();

    Shape newAlloc = adjustAllocation(shape);
    super.changedUpdate(ev, newAlloc, vf);
    getContainer().repaint();
  }

  public int viewToModel(float fx, float fy, Shape a, Position.Bias[] bias)
  {
    return super.viewToModel(fx, fy, adjustAllocation(a), bias);
  }
  
}

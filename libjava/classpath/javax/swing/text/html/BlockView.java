/* BlockView.java -- 
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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;
import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

/**
 * @author Lillian Angel <langel@redhat.com>
 */
public class BlockView extends BoxView
{
  
  /**
   * Creates a new view that represents an html box. 
   * This can be used for a number of elements.
   * 
   * @param elem - the element to create a view for
   * @param axis - either View.X_AXIS or View.Y_AXIS
   */
  public BlockView(Element elem, int axis)
  {
    super(elem, axis);
  }
  
  /**
   * Creates the parent view for this. It is called before
   * any other methods, if the parent view is working properly.
   * Implemented to forward to the superclass and call
   * setPropertiesFromAttributes to set the paragraph 
   * properties.
   * 
   * @param parent - the new parent, or null if the view
   * is being removed from a parent it was added to. 
   */
  public void setParent(View parent)
  {
    super.setParent(parent);
    
    if (parent != null)
      setPropertiesFromAttributes();
  }
  
  /**
   * Calculates the requirements along the major axis.
   * This is implemented to call the superclass and then
   * adjust it if the CSS width or height attribute is specified
   * and applicable.
   * 
   * @param axis - the axis to check the requirements for.
   * @param r - the SizeRequirements. If null, one is created.
   * @return the new SizeRequirements object.
   */
  protected SizeRequirements calculateMajorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    SizeRequirements sr = super.calculateMajorAxisRequirements(axis, r);
    // FIXME: adjust it if the CSS width or height attribute is specified
    // and applicable
    return sr;
  }
  
  /**
   * Calculates the requirements along the minor axis.
   * This is implemented to call the superclass and then
   * adjust it if the CSS width or height attribute is specified
   * and applicable.
   * 
   * @param axis - the axis to check the requirements for.
   * @param r - the SizeRequirements. If null, one is created.
   * @return the new SizeRequirements object.
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    SizeRequirements sr = super.calculateMinorAxisRequirements(axis, r);
    // FIXME: adjust it if the CSS width or height attribute is specified
    // and applicable.
    return sr;
  }
  
  /**
   * Lays out the box along the minor axis (the axis that is
   * perpendicular to the axis that it represents). The results
   * of the layout are placed in the given arrays which are
   * the allocations to the children along the minor axis.
   * 
   * @param targetSpan - the total span given to the view, also 
   * used to layout the children.
   * @param axis - the minor axis
   * @param offsets - the offsets from the origin of the view for
   * all the child views. This is a return value and is filled in by this
   * function.
   * @param spans - the span of each child view. This is a return value and is 
   * filled in by this function.
   */
  protected void layoutMinorAxis(int targetSpan, int axis,
                                 int[] offsets, int[] spans)
  {
    // FIXME: Not implemented.
    super.layoutMinorAxis(targetSpan, axis, offsets, spans);
  }
  
  /**
   * Paints using the given graphics configuration and shape.
   * This delegates to the css box painter to paint the
   * border and background prior to the interior.
   * 
   * @param g - Graphics configuration
   * @param a - the Shape to render into.
   */
  public void paint(Graphics g, Shape a)
  {
    Rectangle rect = (Rectangle) a;
    // FIXME: not fully implemented
    getStyleSheet().getBoxPainter(getAttributes()).paint(g, rect.x, rect.y,
                                                         rect.width,
                                                         rect.height, this);
    super.paint(g, a);
  }
  
  /**
   * Fetches the attributes to use when painting.
   * 
   * @return the attributes of this model.
   */
  public AttributeSet getAttributes()
  {
    return getStyleSheet().getViewAttributes(this);
  }
  
  /**
   * Gets the resize weight.
   * 
   * @param axis - the axis to get the resize weight for.
   * @return the resize weight.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public int getResizeWeight(int axis) throws IllegalArgumentException
  {
    // Can't resize the Y_AXIS
    if (axis == Y_AXIS)
      return 0;
    if (axis == X_AXIS)
      return 1;
    throw new IllegalArgumentException("Invalid Axis");
  }
  
  /**
   * Gets the alignment.
   * 
   * @param axis - the axis to get the alignment for.
   * @return the alignment.
   */
  public float getAlignment(int axis)
  {
    if (axis == X_AXIS)
      return 0.0F;
    if (axis == Y_AXIS)
      {
        if (getViewCount() == 0)
          return 0.0F;
        float prefHeight = getPreferredSpan(Y_AXIS);
        float firstRowHeight = getView(0).getPreferredSpan(Y_AXIS);
        return (firstRowHeight / 2.F) / prefHeight;
      }
    throw new IllegalArgumentException("Invalid Axis");
  }
  
  /**
   * Gives notification from the document that attributes were
   * changed in a location that this view is responsible for.
   * 
   * @param ev - the change information
   * @param a - the current shape of the view
   * @param f - the factory to use to rebuild if the view has children.
   */
  public void changedUpdate(DocumentEvent ev,
                            Shape a, ViewFactory f)
  {
    super.changedUpdate(ev, a, f);
    
    // If more elements were added, then need to set the properties for them
    int currPos = ev.getOffset();
    if (currPos <= getStartOffset() && (currPos + ev.getLength()) >= getEndOffset())
        setPropertiesFromAttributes();
  }

  /**
   * Determines the preferred span along the axis.
   * 
   * @param axis - the view to get the preferred span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned, 
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getPreferredSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getPreferredSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }
  
  /**
   * Determines the minimum span along the axis.
   * 
   * @param axis - the axis to get the minimum span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned, 
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getMinimumSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getMinimumSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }
  
  /**
   * Determines the maximum span along the axis.
   * 
   * @param axis - the axis to get the maximum span for.
   * @return the span the view would like to be painted into >=0/
   * The view is usually told to paint into the span that is returned, 
   * although the parent may choose to resize or break the view.
   * @throws IllegalArgumentException - for an invalid axis
   */
  public float getMaximumSpan(int axis) throws IllegalArgumentException
  {
    if (axis == X_AXIS || axis == Y_AXIS)
      return super.getMaximumSpan(axis);
    throw new IllegalArgumentException("Invalid Axis");
  }
  
  /**
   * Updates any cached values that come from attributes.
   */
  protected void setPropertiesFromAttributes()
  {
    // FIXME: Not implemented (need to use StyleSheet).
  }
  
  /**
   * Gets the default style sheet.
   * 
   * @return the style sheet
   */
  protected StyleSheet getStyleSheet()
  {
    StyleSheet styleSheet = new StyleSheet();
    styleSheet.importStyleSheet(getClass().getResource(HTMLEditorKit.DEFAULT_CSS));
    return styleSheet;
  }
}

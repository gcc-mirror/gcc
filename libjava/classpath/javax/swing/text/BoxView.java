/* BoxView.java -- An composite view
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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;

/**
 * An implementation of {@link CompositeView} that arranges its children in
 * a box along one axis. This is comparable to how the <code>BoxLayout</code>
 * works, but for <code>View</code> children.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class BoxView
  extends CompositeView
{

  /**
   * The axis along which this <code>BoxView</code> is laid out.
   */
  int myAxis;

  /**
   * Indicates wether the layout in X_AXIS is valid.
   */
  boolean xLayoutValid;

  /**
   * Indicates whether the layout in Y_AXIS is valid.
   */
  boolean yLayoutValid;

  /**
   * The spans in X direction of the children.
   */
  int[] spansX;

  /**
   * The spans in Y direction of the children.
   */
  int[] spansY;

  /**
   * The offsets of the children in X direction relative to this BoxView's
   * inner bounds.
   */
  int[] offsetsX;

  /**
   * The offsets of the children in Y direction relative to this BoxView's
   * inner bounds.
   */
  int[] offsetsY;

  /**
   * The current width.
   */
  int width;

  /**
   * The current height.
   */
  int height;

  /**
   * Creates a new <code>BoxView</code> for the given
   * <code>Element</code> and axis. Valid values for the axis are
   * {@link View#X_AXIS} and {@link View#Y_AXIS}.
   *
   * @param element the element that is rendered by this BoxView
   * @param axis the axis along which the box is laid out
   */
  public BoxView(Element element, int axis)
  {
    super(element);
    myAxis = axis;
    xLayoutValid = false;
    yLayoutValid = false;

    // Initialize the cache arrays.
    spansX = new int[0];
    spansY = new int[0];
    offsetsX = new int[0];
    offsetsY = new int[0];

    width = 0;
    height = 0;
  }

  /**
   * Returns the axis along which this <code>BoxView</code> is laid out.
   *
   * @return the axis along which this <code>BoxView</code> is laid out
   */
  public int getAxis()
  {
    return myAxis;
  }

  /**
   * Sets the axis along which this <code>BoxView</code> is laid out.
   *
   * Valid values for the axis are {@link View#X_AXIS} and
   * {@link View#Y_AXIS}.
   *
   * @param axis the axis along which this <code>BoxView</code> is laid out
   */
  public void setAxis(int axis)
  {
    myAxis = axis;
  }

  /**
   * Marks the layout along the specified axis as invalid. This is triggered
   * automatically when any of the child view changes its preferences
   * via {@link #preferenceChanged(View, boolean, boolean)}.
   *
   * The layout will be updated the next time when {@link #setSize()} is
   * called, typically from within the {@link #paint()} method.
   *
   * Valid values for the axis are {@link View#X_AXIS} and
   * {@link View#Y_AXIS}.
   *
   * @param axis an <code>int</code> value
   */
  public void layoutChanged(int axis)
  {
    switch (axis)
      {
      case X_AXIS:
        xLayoutValid = false;
        break;
      case Y_AXIS:
        yLayoutValid = false;
        break;
      default:
        throw new IllegalArgumentException("Invalid axis parameter.");
      }
  }

  /**
   * Returns <code>true</code> if the layout along the specified
   * <code>axis</code> is valid, <code>false</code> otherwise.
   *
   * Valid values for the axis are {@link View#X_AXIS} and
   * {@link View#Y_AXIS}.
   *
   * @param axis the axis
   *
   * @return <code>true</code> if the layout along the specified
   *         <code>axis</code> is valid, <code>false</code> otherwise
   */
  protected boolean isLayoutValid(int axis)
  {
    boolean valid = false;
    switch (axis)
      {
      case X_AXIS:
        valid = xLayoutValid;
        break;
      case Y_AXIS:
        valid = yLayoutValid;
        break;
      default:
        throw new IllegalArgumentException("Invalid axis parameter.");
      }
    return valid;
  }

  /**
   * Paints the child <code>View</code> at the specified <code>index</code>.
   * This method modifies the actual values in <code>alloc</code> so make
   * sure you have a copy of the original values if you need them.
   *
   * @param g the <code>Graphics</code> context to paint to
   * @param alloc the allocated region for the child to paint into
   * @param index the index of the child to be painted
   *
   * @see {@link #childAllocation}
   */
  protected void paintChild(Graphics g, Rectangle alloc, int index)
  {
    View child = getView(index);
    childAllocation(index, alloc);
    child.paint(g, alloc);
  }

  /**
   * Replaces child views by some other child views. If there are no views to
   * remove (<code>length == 0</code>), the result is a simple insert, if
   * there are no children to add (<code>view == null</code>) the result
   * is a simple removal.
   *
   * In addition this invalidates the layout and resizes the internal cache
   * for the child allocations. The old children's cached allocations can
   * still be accessed (although they are not guaranteed to be valid), and
   * the new children will have an initial offset and span of 0.
   *
   * @param offset the start offset from where to remove children
   * @param length the number of children to remove
   * @param views the views that replace the removed children
   */
  public void replace(int offset, int length, View[] views)
  {
    // Resize and copy data for cache arrays.
    // The spansX cache.
    int oldSize = getViewCount();

    int[] newSpansX = new int[oldSize - length + views.length];
    System.arraycopy(spansX, 0, newSpansX, 0, offset);
    System.arraycopy(spansX, offset + length, newSpansX,
                     offset + views.length,
                     oldSize - (offset + length));
    spansX = newSpansX;

    // The spansY cache.
    int[] newSpansY = new int[oldSize - length + views.length];
    System.arraycopy(spansY, 0, newSpansY, 0, offset);
    System.arraycopy(spansY, offset + length, newSpansY,
                     offset + views.length,
                     oldSize - (offset + length));
    spansY = newSpansY;

    // The offsetsX cache.
    int[] newOffsetsX = new int[oldSize - length + views.length];
    System.arraycopy(offsetsX, 0, newOffsetsX, 0, offset);
    System.arraycopy(offsetsX, offset + length, newOffsetsX,
                     offset + views.length,
                     oldSize - (offset + length));
    offsetsX = newOffsetsX;

    // The offsetsY cache.
    int[] newOffsetsY = new int[oldSize - length + views.length];
    System.arraycopy(offsetsY, 0, newOffsetsY, 0, offset);
    System.arraycopy(offsetsY, offset + length, newOffsetsY,
                     offset + views.length,
                     oldSize - (offset + length));
    offsetsY = newOffsetsY;

    // Actually perform the replace.
    super.replace(offset, length, views);

    // Invalidate layout information.
    layoutChanged(X_AXIS);
    layoutChanged(Y_AXIS);
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
    // Adjust size if the size is changed.
    Rectangle bounds = a.getBounds();

    if (bounds.width != getWidth() || bounds.height != getHeight())
      setSize(bounds.width, bounds.height);

    Rectangle inside = getInsideAllocation(a);

    Rectangle copy = new Rectangle(inside);
    int count = getViewCount();
    for (int i = 0; i < count; ++i)
      {
        // TODO: Figure out if the parameter to paintChild is meant to
        // be the child allocation or the allocation of this BoxView.
        // I assume the second option here.
        // We pass this method a copy of the inside rectangle here because
        // it modifies the actual values.
        copy.setBounds(inside);
        paintChild(g, copy, i);
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
    SizeRequirements sr = new SizeRequirements();
    int pref = baselineRequirements(axis, sr).preferred;
    return (float) pref;
  }

  public float getMaximumSpan(int axis)
  {
    if (axis == getAxis())
      return getPreferredSpan(axis);
    else
      return Integer.MAX_VALUE;
  }

  /**
   * Calculates the size requirements for this <code>BoxView</code> along
   * the specified axis.
   *
   * @param axis the axis that is examined
   * @param sr the <code>SizeRequirements</code> object to hold the result,
   *        if <code>null</code>, a new one is created
   *
   * @return the size requirements for this <code>BoxView</code> along
   *         the specified axis
   */
  protected SizeRequirements baselineRequirements(int axis,
                                                  SizeRequirements sr)
  {
    SizeRequirements result;
    if (axis == myAxis)
      result = calculateMajorAxisRequirements(axis, sr);
    else
      result = calculateMinorAxisRequirements(axis, sr);
    return result;
  }

  /**
   * Calculates the size requirements of this <code>BoxView</code> along
   * its major axis, that is the axis specified in the constructor.
   *
   * @param axis the axis that is examined
   * @param sr the <code>SizeRequirements</code> object to hold the result,
   *        if <code>null</code>, a new one is created
   *
   * @return the size requirements for this <code>BoxView</code> along
   *         the specified axis
   */
  protected SizeRequirements calculateMajorAxisRequirements(int axis,
                                                           SizeRequirements sr)
  {
    if (sr == null)
      sr = new SizeRequirements();
    else
      {
        sr.maximum = 0;
        sr.minimum = 0;
        sr.preferred = 0;
        sr.alignment = 0.5F;
      }

    int count = getViewCount();

    // Sum up the sizes of the children along the specified axis.
    for (int i = 0; i < count; ++i)
      {
        View child = getView(i);
        sr.minimum += child.getMinimumSpan(axis);
        sr.preferred += child.getPreferredSpan(axis);
        sr.maximum += child.getMaximumSpan(axis);
      }
    return sr;
  }

  /**
   * Calculates the size requirements of this <code>BoxView</code> along
   * its minor axis, that is the axis opposite to the axis specified in the
   * constructor.
   *
   * @param axis the axis that is examined
   * @param sr the <code>SizeRequirements</code> object to hold the result,
   *        if <code>null</code>, a new one is created
   *
   * @return the size requirements for this <code>BoxView</code> along
   *         the specified axis
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                           SizeRequirements sr)
  {
    if (sr == null)
      sr = new SizeRequirements();
    else
      {
        sr.maximum = 0;
        sr.minimum = 0;
        sr.preferred = 0;
        sr.alignment = 0.5F;
      }

    int count = getViewCount();

    int aboveBaseline = 0;
    int belowBaseline = 0;
    int aboveBaselineMin = 0;
    int belowBaselineMin = 0;
    int aboveBaselineMax = 0;
    int belowBaselineMax = 0;
    
    for (int i = 0; i < count; ++i)
      {
        View child = getView(i);
        float align = child.getAlignment(axis);
        int pref = (int) child.getPreferredSpan(axis);
        int min = (int) child.getMinimumSpan(axis);
        int max = (int) child.getMaximumSpan(axis);
        aboveBaseline += (int) (align * pref);
        belowBaseline += (int) ((1.F - align) * pref);
        aboveBaselineMin += (int) (align * min);
        belowBaselineMin += (int) ((1.F - align) * min);
        aboveBaselineMax += (int) (align * max);
        belowBaselineMax += (int) ((1.F - align) * max);
      }
    sr.minimum = aboveBaselineMin + belowBaselineMin;
    sr.maximum = aboveBaselineMax + belowBaselineMax;
    sr.preferred = aboveBaseline + belowBaseline;
    if (aboveBaseline == 0)
      sr.alignment = 1.0F;
    else
      sr.alignment = (float) (sr.preferred / aboveBaseline);

    return sr;
  }

  /**
   * Returns <code>true</code> if the specified point lies before the
   * given <code>Rectangle</code>, <code>false</code> otherwise.
   *
   * &quot;Before&quot; is typically defined as being to the left or above.
   *
   * @param x the X coordinate of the point
   * @param y the Y coordinate of the point
   * @param r the rectangle to test the point against
   *
   * @return <code>true</code> if the specified point lies before the
   *         given <code>Rectangle</code>, <code>false</code> otherwise
   */
  protected boolean isBefore(int x, int y, Rectangle r)
  {
    boolean result = false;

    if (myAxis == X_AXIS)
      result = x < r.x;
    else
      result = y < r.y;

    return result;
  }

  /**
   * Returns <code>true</code> if the specified point lies after the
   * given <code>Rectangle</code>, <code>false</code> otherwise.
   *
   * &quot;After&quot; is typically defined as being to the right or below.
   *
   * @param x the X coordinate of the point
   * @param y the Y coordinate of the point
   * @param r the rectangle to test the point against
   *
   * @return <code>true</code> if the specified point lies after the
   *         given <code>Rectangle</code>, <code>false</code> otherwise
   */
  protected boolean isAfter(int x, int y, Rectangle r)
  {
    boolean result = false;

    if (myAxis == X_AXIS)
      result = x > r.x;
    else
      result = y > r.y;

    return result;
  }

  /**
   * Returns the child <code>View</code> at the specified location.
   *
   * @param x the X coordinate
   * @param y the Y coordinate
   * @param r the inner allocation of this <code>BoxView</code> on entry,
   *        the allocation of the found child on exit
   *
   * @return the child <code>View</code> at the specified location
   */
  protected View getViewAtPoint(int x, int y, Rectangle r)
  {
    View result = null;

    int count = getViewCount();
    Rectangle copy = new Rectangle(r);

    for (int i = 0; i < count; ++i)
      {
        copy.setBounds(r);
        childAllocation(i, r);
        if (copy.contains(x, y))
          {
            result = getView(i);
            break;
          }
      }

    return result;
  }

  /**
   * Computes the allocation for a child <code>View</code>. The parameter
   * <code>a</code> stores the allocation of this <code>CompositeView</code>
   * and is then adjusted to hold the allocation of the child view.
   *
   * @param index the index of the child <code>View</code>
   * @param a the allocation of this <code>CompositeView</code> before the
   *        call, the allocation of the child on exit
   */
  protected void childAllocation(int index, Rectangle a)
  {
    if (! isAllocationValid())
      layout(a.width, a.height);

    a.x += offsetsX[index];
    a.y += offsetsY[index];
    a.width = spansX[index];
    a.height = spansY[index];
  }

  /**
   * Lays out the children of this <code>BoxView</code> with the specified
   * bounds.
   *
   * @param width the width of the allocated region for the children (that
   *        is the inner allocation of this <code>BoxView</code>
   * @param height the height of the allocated region for the children (that
   *        is the inner allocation of this <code>BoxView</code>
   */
  protected void layout(int width, int height)
  {
    this.width = width;
    this.height = height;

    if (myAxis == X_AXIS)
      {
        layoutMajorAxis(width, X_AXIS, offsetsX, spansX);
        layoutMinorAxis(height, Y_AXIS, offsetsY, spansY);
      }
    else
      {
        layoutMajorAxis(height, Y_AXIS, offsetsY, spansY);
        layoutMinorAxis(width, X_AXIS, offsetsX, spansX);
      }
  }

  /**
   * Performs the layout along the major axis of a <code>BoxView</code>.
   *
   * @param targetSpan the (inner) span of the <code>BoxView</code> in which
   *        to layout the children
   * @param axis the axis along which the layout is performed
   * @param offsets the array that holds the offsets of the children on exit
   * @param offsets the array that holds the spans of the children on exit
   */
  protected void layoutMajorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    // Allocate SizeRequirements for each child view.
    int count = getViewCount();
    SizeRequirements[] childReqs = new SizeRequirements[count];
    for (int i = 0; i < count; ++i)
      {
        View view = getView(i);
        childReqs[i] = new SizeRequirements((int) view.getMinimumSpan(axis),
                                            (int) view.getPreferredSpan(axis),
                                            (int) view.getMaximumSpan(axis),
                                            view.getAlignment(axis));
      }

    // Calculate the spans and offsets using the SizeRequirements uility
    // methods.
    SizeRequirements.calculateTiledPositions(targetSpan, null, childReqs,
                                             offsets, spans);

    validateLayout(axis);
  }

  /**
   * Performs the layout along the minor axis of a <code>BoxView</code>.
   *
   * @param targetSpan the (inner) span of the <code>BoxView</code> in which
   *        to layout the children
   * @param axis the axis along which the layout is performed
   * @param offsets the array that holds the offsets of the children on exit
   * @param offsets the array that holds the spans of the children on exit
   */
  protected void layoutMinorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    // Allocate SizeRequirements for each child view.
    int count = getViewCount();
    SizeRequirements[] childReqs = new SizeRequirements[count];
    for (int i = 0; i < count; ++i)
      {
        View view = getView(i);
        childReqs[i] = new SizeRequirements((int) view.getMinimumSpan(axis),
                                            (int) view.getPreferredSpan(axis),
                                            (int) view.getMaximumSpan(axis),
                                            view.getAlignment(axis));
      }

    // Calculate the spans and offsets using the SizeRequirements uility
    // methods.
    SizeRequirements.calculateAlignedPositions(targetSpan, null, childReqs,
                                               offsets, spans);
    validateLayout(axis);
  }

  /**
   * Returns <code>true</code> if the cached allocations for the children
   * are still valid, <code>false</code> otherwise.
   *
   * @return <code>true</code> if the cached allocations for the children
   *         are still valid, <code>false</code> otherwise
   */
  protected boolean isAllocationValid()
  {
    return isLayoutValid(X_AXIS) && isLayoutValid(Y_AXIS);
  }

  /**
   * Return the current width of the box. This is the last allocated width.
   *
   * @return the current width of the box
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * Return the current height of the box. This is the last allocated height.
   *
   * @return the current height of the box
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * Sets the size of the view. If the actual size has changed, the layout
   * is updated accordingly.
   *
   * @param width the new width
   * @param height the new height
   */
  public void setSize(float width, float height)
  {
    if (this.width != (int) width)
      layoutChanged(X_AXIS);
    if (this.height != (int) height)
      layoutChanged(Y_AXIS);

    Rectangle outside = new Rectangle(0, 0, this.width, this.height);
    Rectangle inside = getInsideAllocation(outside);
    if (!isAllocationValid())
      layout(inside.width, inside.height);
  }

  /**
   * Sets the layout to valid for a specific axis.
   *
   * @param axis the axis for which to validate the layout
   */
  void validateLayout(int axis)
  {
    if (axis == X_AXIS)
      xLayoutValid = true;
    if (axis == Y_AXIS)
      yLayoutValid = true;
  }
}

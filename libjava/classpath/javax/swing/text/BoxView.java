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
import javax.swing.event.DocumentEvent;

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
  private int myAxis;

  /**
   * Indicates if the layout is valid along X_AXIS or Y_AXIS.
   */
  private boolean[] layoutValid = new boolean[2];

  /**
   * The spans along the X_AXIS and Y_AXIS.
   */
  private int[][] spans = new int[2][];

  /**
   * The offsets of the children along the X_AXIS and Y_AXIS.
   */
  private int[][] offsets = new int[2][];

  /**
   * The size requirements along the X_AXIS and Y_AXIS.
   */
  private SizeRequirements[] requirements = new SizeRequirements[2];

  /**
   * The current span along X_AXIS or Y_AXIS.
   */
  private int[] span = new int[2];

  /**
   * The SizeRequirements of the child views along the X_AXIS and Y_AXIS.
   */
  private SizeRequirements[][] childReqs = new SizeRequirements[2][];

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
    layoutValid[0] = false;
    layoutValid[1] = false;
    span[0] = 0;
    span[1] = 0;
    requirements[0] = new SizeRequirements();
    requirements[1] = new SizeRequirements();

    // Initialize the cache arrays.
    spans[0] = new int[0];
    spans[1] = new int[0];
    offsets[0] = new int[0];
    offsets[1] = new int[0];
  }

  /**
   * Returns the axis along which this <code>BoxView</code> is laid out.
   *
   * @return the axis along which this <code>BoxView</code> is laid out
   *
   * @since 1.3
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
   *
   * @since 1.3
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
   * The layout will be updated the next time when 
   * {@link #setSize(float, float)} is called, typically from within the 
   * {@link #paint(Graphics, Shape)} method.
   *
   * Valid values for the axis are {@link View#X_AXIS} and
   * {@link View#Y_AXIS}.
   *
   * @param axis an <code>int</code> value
   *
   * @since 1.3
   */
  public void layoutChanged(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Invalid axis parameter.");
    layoutValid[axis] = false;
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
   *
   * @since 1.4
   */
  protected boolean isLayoutValid(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Invalid axis parameter.");
    return layoutValid[axis];
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
   * @see #childAllocation(int, Rectangle)
   */
  protected void paintChild(Graphics g, Rectangle alloc, int index)
  {
    View child = getView(index);
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
    int numViews = 0;
    if (views != null)
      numViews = views.length;

    // Resize and copy data for cache arrays.
    // The spansX cache.
    int oldSize = getViewCount();

    int[] newSpansX = new int[oldSize - length + numViews];
    System.arraycopy(spans[X_AXIS], 0, newSpansX, 0, offset);
    System.arraycopy(spans[X_AXIS], offset + length, newSpansX,
                     offset + numViews,
                     oldSize - (offset + length));
    spans[X_AXIS] = newSpansX;

    // The spansY cache.
    int[] newSpansY = new int[oldSize - length + numViews];
    System.arraycopy(spans[Y_AXIS], 0, newSpansY, 0, offset);
    System.arraycopy(spans[Y_AXIS], offset + length, newSpansY,
                     offset + numViews,
                     oldSize - (offset + length));
    spans[Y_AXIS] = newSpansY;

    // The offsetsX cache.
    int[] newOffsetsX = new int[oldSize - length + numViews];
    System.arraycopy(offsets[X_AXIS], 0, newOffsetsX, 0, offset);
    System.arraycopy(offsets[X_AXIS], offset + length, newOffsetsX,
                     offset + numViews,
                     oldSize - (offset + length));
    offsets[X_AXIS] = newOffsetsX;

    // The offsetsY cache.
    int[] newOffsetsY = new int[oldSize - length + numViews];
    System.arraycopy(offsets[Y_AXIS], 0, newOffsetsY, 0, offset);
    System.arraycopy(offsets[Y_AXIS], offset + length, newOffsetsY,
                     offset + numViews,
                     oldSize - (offset + length));
    offsets[Y_AXIS] = newOffsetsY;

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
    Rectangle inside = getInsideAllocation(a);
    // TODO: Used for debugging.
    //g.drawRect(inside.x, inside.y, inside.width, inside.height);

    Rectangle copy = new Rectangle(inside);
    int count = getViewCount();
    for (int i = 0; i < count; ++i)
      {
        copy.setBounds(inside);
        childAllocation(i, copy);
        if (!copy.isEmpty()
            && g.hitClip(copy.x, copy.y, copy.width, copy.height))
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
    updateRequirements(axis);
    return requirements[axis].preferred;
  }

  /**
   * Returns the maximum span of this view along the specified axis.
   * This returns <code>Integer.MAX_VALUE</code> for the minor axis
   * and the preferred span for the major axis.
   *
   * @param axis the axis
   *
   * @return the maximum span of this view along the specified axis
   */
  public float getMaximumSpan(int axis)
  {
    float max;
    if (axis == myAxis)
      max = getPreferredSpan(axis);
    else
      max = Integer.MAX_VALUE;
    return max;
  }

  /**
   * Returns the minimum span of this view along the specified axis.
   * This calculates the minimum span using
   * {@link #calculateMajorAxisRequirements} or
   * {@link #calculateMinorAxisRequirements} (depending on the axis) and
   * returns the resulting minimum span.
   *
   * @param axis the axis
   *
   * @return the minimum span of this view along the specified axis
   */
  public float getMinimumSpan(int axis)
  {
    updateRequirements(axis);
    return requirements[axis].minimum;
  }

  /**
   * This method is obsolete and no longer in use. It is replaced by
   * {@link #calculateMajorAxisRequirements(int, SizeRequirements)} and
   * {@link #calculateMinorAxisRequirements(int, SizeRequirements)}.
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
    updateChildRequirements(axis);

    SizeRequirements res = sr;
    if (res == null)
      res = new SizeRequirements();

    float minLeft = 0;
    float minRight = 0;
    float prefLeft = 0;
    float prefRight = 0;
    float maxLeft = 0;
    float maxRight = 0;
    for (int i = 0; i < childReqs[axis].length; i++)
      {
        float myMinLeft = childReqs[axis][i].minimum * childReqs[axis][i].alignment;
        float myMinRight = childReqs[axis][i].minimum - myMinLeft;
        minLeft = Math.max(myMinLeft, minLeft);
        minRight = Math.max(myMinRight, minRight);
        float myPrefLeft = childReqs[axis][i].preferred * childReqs[axis][i].alignment;
        float myPrefRight = childReqs[axis][i].preferred - myPrefLeft;
        prefLeft = Math.max(myPrefLeft, prefLeft);
        prefRight = Math.max(myPrefRight, prefRight);
        float myMaxLeft = childReqs[axis][i].maximum * childReqs[axis][i].alignment;
        float myMaxRight = childReqs[axis][i].maximum - myMaxLeft;
        maxLeft = Math.max(myMaxLeft, maxLeft);
        maxRight = Math.max(myMaxRight, maxRight);
      }
    int minSize = (int) (minLeft + minRight);
    int prefSize = (int) (prefLeft + prefRight);
    int maxSize = (int) (maxLeft + maxRight);
    float align = prefLeft / (prefRight + prefLeft);
    if (Float.isNaN(align))
      align = 0;

    res.alignment = align;
    res.maximum = maxSize;
    res.preferred = prefSize;
    res.minimum = minSize;
    return res;
  }

  /**
   * Calculates the layout of the children of this <code>BoxView</code> along
   * the specified axis.
   *
   * @param span the target span
   * @param axis the axis that is examined
   * @param offsets an empty array, filled with the offsets of the children
   * @param spans an empty array, filled with the spans of the children
   */
  protected void baselineLayout(int span, int axis, int[] offsets,
                                int[] spans)
  {
    updateChildRequirements(axis);
    updateRequirements(axis);

    // Calculate the spans and offsets using the SizeRequirements uility
    // methods.
    SizeRequirements.calculateAlignedPositions(span, requirements[axis],
                                               childReqs[axis], offsets, spans);
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
    updateChildRequirements(axis);

    SizeRequirements result = sr;
    if (result == null)
      result = new SizeRequirements();

    long minimum = 0;
    long preferred = 0;
    long maximum = 0;
    for (int i = 0; i < children.length; i++)
      {
        minimum += childReqs[axis][i].minimum;
        preferred += childReqs[axis][i].preferred;
        maximum += childReqs[axis][i].maximum;
      }
    // Overflow check.
    if (minimum > Integer.MAX_VALUE)
      minimum = Integer.MAX_VALUE;
    if (preferred > Integer.MAX_VALUE)
      preferred = Integer.MAX_VALUE;
    if (maximum > Integer.MAX_VALUE)
      maximum = Integer.MAX_VALUE;

    result.minimum = (int) minimum;
    result.preferred = (int) preferred;
    result.maximum = (int) maximum;
    result.alignment = 0.5F;
    return result;
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
    updateChildRequirements(axis);

    SizeRequirements res = sr;
    if (res == null)
      res = new SizeRequirements();

    float minLeft = 0;
    float minRight = 0;
    float prefLeft = 0;
    float prefRight = 0;
    float maxLeft = 0;
    float maxRight = 0;
    for (int i = 0; i < childReqs[axis].length; i++)
      {
        float myMinLeft = childReqs[axis][i].minimum * childReqs[axis][i].alignment;
        float myMinRight = childReqs[axis][i].minimum - myMinLeft;
        minLeft = Math.max(myMinLeft, minLeft);
        minRight = Math.max(myMinRight, minRight);
        float myPrefLeft = childReqs[axis][i].preferred * childReqs[axis][i].alignment;
        float myPrefRight = childReqs[axis][i].preferred - myPrefLeft;
        prefLeft = Math.max(myPrefLeft, prefLeft);
        prefRight = Math.max(myPrefRight, prefRight);
        float myMaxLeft = childReqs[axis][i].maximum * childReqs[axis][i].alignment;
        float myMaxRight = childReqs[axis][i].maximum - myMaxLeft;
        maxLeft = Math.max(myMaxLeft, maxLeft);
        maxRight = Math.max(myMaxRight, maxRight);
      }
    int minSize = (int) (minLeft + minRight);
    int prefSize = (int) (prefLeft + prefRight);
    int maxSize = (int) (maxLeft + maxRight);
    float align = prefLeft / (prefRight + prefLeft);
    if (Float.isNaN(align))
      align = 0;

    res.alignment = align;
    res.maximum = maxSize;
    res.preferred = prefSize;
    res.minimum = minSize;
    return res;
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
    
    if (result == null && count > 0)
      return getView(count - 1);
    return result;
  }

  /**
   * Computes the allocation for a child <code>View</code>. The parameter
   * <code>a</code> stores the allocation of this <code>CompositeView</code>
   * and is then adjusted to hold the allocation of the child view.
   * 
   * @param index
   *          the index of the child <code>View</code>
   * @param a
   *          the allocation of this <code>CompositeView</code> before the
   *          call, the allocation of the child on exit
   */
  protected void childAllocation(int index, Rectangle a)
  {
    if (! isAllocationValid())
      layout(a.width, a.height);

    a.x += offsets[X_AXIS][index];
    a.y += offsets[Y_AXIS][index];
    a.width = spans[X_AXIS][index];
    a.height = spans[Y_AXIS][index];
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
    int[] newSpan = new int[]{ width, height };
    int count = getViewCount();

    // Update minor axis as appropriate. We need to first update the minor
    // axis layout because that might affect the children's preferences along
    // the major axis.
    int minorAxis = myAxis == X_AXIS ? Y_AXIS : X_AXIS;
    if ((! isLayoutValid(minorAxis)) || newSpan[minorAxis] != span[minorAxis])
      {
        layoutValid[minorAxis] = false;
        span[minorAxis] = newSpan[minorAxis];
        layoutMinorAxis(span[minorAxis], minorAxis, offsets[minorAxis],
                        spans[minorAxis]);

        // Update the child view's sizes.
        for (int i = 0; i < count; ++i)
          {
            getView(i).setSize(spans[X_AXIS][i], spans[Y_AXIS][i]);
          }
        layoutValid[minorAxis] = true;
      }


    // Update major axis as appropriate.
    if ((! isLayoutValid(myAxis)) || newSpan[myAxis] != span[myAxis])
      {
        layoutValid[myAxis] = false;
        span[myAxis] = newSpan[myAxis];
        layoutMajorAxis(span[myAxis], myAxis, offsets[myAxis],
                        spans[myAxis]);

        // Update the child view's sizes.
        for (int i = 0; i < count; ++i)
          {
            getView(i).setSize(spans[X_AXIS][i], spans[Y_AXIS][i]);
          }
        layoutValid[myAxis] = true;
      }

    if (layoutValid[myAxis] == false)
	  System.err.println("WARNING: Major axis layout must be valid after layout");
    if (layoutValid[minorAxis] == false)
      System.err.println("Minor axis layout must be valid after layout");
  }

  /**
   * Performs the layout along the major axis of a <code>BoxView</code>.
   *
   * @param targetSpan the (inner) span of the <code>BoxView</code> in which
   *        to layout the children
   * @param axis the axis along which the layout is performed
   * @param offsets the array that holds the offsets of the children on exit
   * @param spans the array that holds the spans of the children on exit
   */
  protected void layoutMajorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    updateChildRequirements(axis);
    updateRequirements(axis);

    // Calculate the spans and offsets using the SizeRequirements uility
    // methods.
    SizeRequirements.calculateTiledPositions(targetSpan, requirements[axis],
                                             childReqs[axis],
                                             offsets, spans);

  }

  /**
   * Performs the layout along the minor axis of a <code>BoxView</code>.
   *
   * @param targetSpan the (inner) span of the <code>BoxView</code> in which
   *        to layout the children
   * @param axis the axis along which the layout is performed
   * @param offsets the array that holds the offsets of the children on exit
   * @param spans the array that holds the spans of the children on exit
   */
  protected void layoutMinorAxis(int targetSpan, int axis, int[] offsets,
                                 int[] spans)
  {
    updateChildRequirements(axis);
    updateRequirements(axis);

    // Calculate the spans and offsets using the SizeRequirements uility
    // methods.
    SizeRequirements.calculateAlignedPositions(targetSpan, requirements[axis],
                                               childReqs[axis], offsets,
                                               spans);
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
    return span[X_AXIS];
  }

  /**
   * Return the current height of the box. This is the last allocated height.
   *
   * @return the current height of the box
   */
  public int getHeight()
  {
    return span[Y_AXIS];
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
    layout((int) width, (int) height);
  }

  /**
   * Returns the span for the child view with the given index for the specified
   * axis.
   *
   * @param axis the axis to examine, either <code>X_AXIS</code> or
   *        <code>Y_AXIS</code>
   * @param childIndex the index of the child for for which to return the span
   *
   * @return the span for the child view with the given index for the specified
   *         axis
   */
  protected int getSpan(int axis, int childIndex)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis argument");
    return spans[axis][childIndex];
  }

  /**
   * Returns the offset for the child view with the given index for the
   * specified axis.
   *
   * @param axis the axis to examine, either <code>X_AXIS</code> or
   *        <code>Y_AXIS</code>
   * @param childIndex the index of the child for for which to return the span
   *
   * @return the offset for the child view with the given index for the
   *         specified axis
   */
  protected int getOffset(int axis, int childIndex)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis argument");
    return offsets[axis][childIndex];
  }

  /**
   * Returns the alignment for this box view for the specified axis. The
   * axis that is tiled (the major axis) will be requested to be aligned
   * centered (0.5F). The minor axis alignment depends on the child view's
   * total alignment.
   *
   * @param axis the axis which is examined
   *
   * @return the alignment for this box view for the specified axis
   */
  public float getAlignment(int axis)
  {
    float align;
    if (axis == myAxis)
      align = 0.5F;
    else
      {
        updateRequirements(axis);
        align = requirements[axis].alignment;
      }
    return align;
  }
  
  /**
   * Called by a child View when its preferred span has changed.
   * 
   * @param width indicates that the preferred width of the child changed.
   * @param height indicates that the preferred height of the child changed.
   * @param child the child View. 
   */
  public void preferenceChanged(View child, boolean width, boolean height)
  {
    if (width)
      layoutValid[X_AXIS] = false;
    if (height)
      layoutValid[Y_AXIS] = false;
    super.preferenceChanged(child, width, height);
  }
  
  /**
   * Maps the document model position <code>pos</code> to a Shape
   * in the view coordinate space.  This method overrides CompositeView's
   * method to make sure the children are allocated properly before
   * calling the super's behaviour.
   */
  public Shape modelToView(int pos, Shape a, Position.Bias bias)
      throws BadLocationException
  {
    // Make sure everything is allocated properly and then call super
    if (! isAllocationValid())
      {
        Rectangle bounds = a.getBounds();
        layout(bounds.width, bounds.height);
      }
    return super.modelToView(pos, a, bias);
  }

  /**
   * Returns the resize weight of this view. A value of <code>0</code> or less
   * means this view is not resizeable. Positive values make the view
   * resizeable. This implementation returns <code>0</code> for the major
   * axis and <code>1</code> for the minor axis of this box view.
   *
   * @param axis the axis
   *
   * @return the resizability of this view along the specified axis
   *
   * @throws IllegalArgumentException if <code>axis</code> is invalid
   */
  public int getResizeWeight(int axis)
  {
    if (axis != X_AXIS && axis != Y_AXIS)
      throw new IllegalArgumentException("Illegal axis argument");
    int weight = 1;
    if (axis == myAxis)
      weight = 0;
    return weight;
  }

  /**
   * Returns the child allocation for the child view with the specified
   * <code>index</code>. If the layout is invalid, this returns
   * <code>null</code>.
   *
   * @param index the child view index
   * @param a the allocation to this view
   *
   * @return the child allocation for the child view with the specified
   *         <code>index</code> or <code>null</code> if the layout is invalid
   *         or <code>a</code> is null
   */
  public Shape getChildAllocation(int index, Shape a)
  {
    Shape ret = null;
    if (isAllocationValid() && a != null)
      ret = super.getChildAllocation(index, a);
    return ret;
  }

  protected void forwardUpdate(DocumentEvent.ElementChange ec, DocumentEvent e,
                               Shape a, ViewFactory vf)
  {
    // FIXME: What to do here?
    super.forwardUpdate(ec, e, a, vf);
  }

  public int viewToModel(float x, float y, Shape a, Position.Bias[] bias)
  {
    // FIXME: What to do here?
    return super.viewToModel(x, y, a, bias);
  }

  protected boolean flipEastAndWestEnds(int position, Position.Bias bias)
  {
    // FIXME: What to do here?
    return super.flipEastAndWestAtEnds(position, bias);
  }

  /**
   * Updates the child requirements along the specified axis. The requirements
   * are only updated if the layout for the specified axis is marked as
   * invalid.
   *
   * @param axis the axis to be updated
   */
  private void updateChildRequirements(int axis)
  {
    if (! isLayoutValid(axis))
      {
        int numChildren = getViewCount();
        if (childReqs[axis] == null || childReqs[axis].length != numChildren)
          childReqs[axis] = new SizeRequirements[numChildren];
        for (int i = 0; i < numChildren; ++i)
          {
            View child = getView(i);
            childReqs[axis][i] =
              new SizeRequirements((int) child.getMinimumSpan(axis),
                                   (int) child.getPreferredSpan(axis),
                                   (int) child.getMaximumSpan(axis),
                                   child.getAlignment(axis));
          }
      }
  }

  /**
   * Updates the view's cached requirements along the specified axis if
   * necessary. The requirements are only updated if the layout for the
   * specified axis is marked as invalid.
   *
   * @param axis the axis
   */
  private void updateRequirements(int axis)
  {
    if (! layoutValid[axis])
      {
        if (axis == myAxis)
          requirements[axis] = calculateMajorAxisRequirements(axis,
                                                           requirements[axis]);
        else
          requirements[axis] = calculateMinorAxisRequirements(axis,
                                                           requirements[axis]);
      }
  }
}

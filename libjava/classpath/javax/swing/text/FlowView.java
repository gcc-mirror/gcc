/* FlowView.java -- A composite View
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

import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.SizeRequirements;
import javax.swing.event.DocumentEvent;

/**
 * A <code>View</code> that can flows it's children into it's layout space.
 *
 * The <code>FlowView</code> manages a set of logical views (that are
 * the children of the {@link #layoutPool} field). These are translated
 * at layout time into a set of physical views. These are the views that
 * are managed as the real child views. Each of these child views represents
 * a row and are laid out within a box using the superclasses behaviour.
 * The concrete implementation of the rows must be provided by subclasses.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public abstract class FlowView extends BoxView
{
  /**
   * A strategy for translating the logical views of a <code>FlowView</code>
   * into the real views.
   */
  public static class FlowStrategy
  {
    /**
     * Creates a new instance of <code>FlowStragegy</code>.
     */
    public FlowStrategy()
    {
      // Nothing to do here.
    }

    /**
     * Receives notification from a <code>FlowView</code> that some content
     * has been inserted into the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void insertUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      // The default implementation does nothing.
    }

    /**
     * Receives notification from a <code>FlowView</code> that some content
     * has been removed from the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void removeUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      // The default implementation does nothing.
    }

    /**
     * Receives notification from a <code>FlowView</code> that some attributes
     * have changed in the document at a location that the
     * <code>FlowView</code> is responsible for.
     *
     * The default implementation simply calls {@link #layout}.
     *
     * @param fv the flow view that sends the notification
     * @param e the document event describing the change
     * @param alloc the current allocation of the flow view
     */
    public void changedUpdate(FlowView fv, DocumentEvent e, Rectangle alloc)
    {
      // The default implementation does nothing.
    }

    /**
     * Returns the logical view of the managed <code>FlowView</code>.
     *
     * @param fv the flow view for which to return the logical view
     *
     * @return the logical view of the managed <code>FlowView</code>
     */
    protected View getLogicalView(FlowView fv)
    {
      return fv.layoutPool;
    }

    /**
     * Performs the layout for the whole view. By default this rebuilds
     * all the physical views from the logical views of the managed FlowView.
     *
     * This is called by {@link FlowView#layout} to update the layout of
     * the view.
     *
     * @param fv the flow view for which we perform the layout
     */
    public void layout(FlowView fv)
    {
      fv.removeAll();
      Element el = fv.getElement();

      int rowStart = el.getStartOffset();
      int end = el.getEndOffset();
      int rowIndex = 0;
      while (rowStart >= 0 && rowStart < end)
        {
          View row = fv.createRow();
          fv.append(row);
          rowStart = layoutRow(fv, rowIndex, rowStart);
          rowIndex++;
        }
    }

    /**
     * Lays out one row of the flow view. This is called by {@link #layout}
     * to fill one row with child views until the available span is exhausted.
     *
     * The default implementation fills the row by calling
     * {@link #createView(FlowView, int, int, int)} until the available space
     * is exhausted, a forced break is encountered or there are no more views
     * in the logical view. If the available space is exhausted,
     * {@link #adjustRow(FlowView, int, int, int)} is called to fit the row
     * into the available span.
     *
     * @param fv the flow view for which we perform the layout
     * @param rowIndex the index of the row
     * @param pos the model position for the beginning of the row
     *
     * @return the start position of the next row
     */
    protected int layoutRow(FlowView fv, int rowIndex, int pos)
    {
      View row = fv.getView(rowIndex);
      int axis = fv.getFlowAxis();
      int span = fv.getFlowSpan(rowIndex);
      int x = fv.getFlowStart(rowIndex);
      int offset = pos;
      View logicalView = getLogicalView(fv);
      // Special case when span == 0. We need to layout the row as if it had
      // a span of Integer.MAX_VALUE.
      if (span == 0)
        span = Integer.MAX_VALUE;

      while (span > 0)
        {
          if (logicalView.getViewIndex(offset, Position.Bias.Forward) == -1)
            break;
          View view = createView(fv, offset, span, rowIndex);
          if (view == null)
            break;
          int viewSpan = (int) view.getPreferredSpan(axis);
          row.append(view);
          int breakWeight = view.getBreakWeight(axis, x, span);
          if (breakWeight >= View.ForcedBreakWeight)
            break;
          x += viewSpan;
          span -= viewSpan;
          offset += (view.getEndOffset() - view.getStartOffset());
        }
      if (span < 0)
        {
          int flowStart = fv.getFlowStart(axis);
          int flowSpan = fv.getFlowSpan(axis);
          adjustRow(fv, rowIndex, flowSpan, flowStart);
          int rowViewCount = row.getViewCount();
          if (rowViewCount > 0)
            offset = row.getView(rowViewCount - 1).getEndOffset();
          else
            offset = -1;
        }
      return offset != pos ? offset : -1;
    }

    /**
     * Creates physical views that form the rows of the flow view. This
     * can be an entire view from the logical view (if it fits within the
     * available span), a fragment of such a view (if it doesn't fit in the
     * available span and can be broken down) or <code>null</code> (if it does
     * not fit in the available span and also cannot be broken down).
     *
     * The default implementation fetches the logical view at the specified
     * <code>startOffset</code>. If that view has a different startOffset than
     * specified in the argument, a fragment is created using
     * {@link View#createFragment(int, int)} that has the correct startOffset
     * and the logical view's endOffset.
     *
     * @param fv the flow view
     * @param startOffset the start offset for the view to be created
     * @param spanLeft the available span
     * @param rowIndex the index of the row
     *
     * @return a view to fill the row with, or <code>null</code> if there
     *         is no view or view fragment that fits in the available span
     */
    protected View createView(FlowView fv, int startOffset, int spanLeft,
                              int rowIndex)
    {
       View logicalView = getLogicalView(fv);
       // FIXME: Handle the bias thing correctly.
       int index = logicalView.getViewIndex(startOffset, Position.Bias.Forward);
       View retVal = null;
       if (index >= 0)
         {
           retVal = logicalView.getView(index);
           if (retVal.getStartOffset() != startOffset)
             retVal = retVal.createFragment(startOffset, retVal.getEndOffset());
         }
       return retVal;
    }

    /**
     * Tries to adjust the specified row to fit within the desired span. The
     * default implementation iterates through the children of the specified
     * row to find the view that has the highest break weight and - if there
     * is more than one view with such a break weight - which is nearest to
     * the end of the row. If there is such a view that has a break weight >
     * {@link View#BadBreakWeight}, this view is broken using the
     * {@link View#breakView(int, int, float, float)} method and this view and
     * all views after the now broken view are replaced by the broken view.
     *
     * @param fv the flow view
     * @param rowIndex the index of the row to be adjusted
     * @param desiredSpan the layout span
     * @param x the X location at which the row starts
     */
    protected void adjustRow(FlowView fv, int rowIndex, int desiredSpan, int x) {
      // Determine the last view that has the highest break weight.
      int axis = fv.getFlowAxis();
      View row = fv.getView(rowIndex);
      int count = row.getViewCount();
      int breakIndex = -1;
      int maxBreakWeight = View.BadBreakWeight;
      int breakX = x;
      int breakSpan = desiredSpan;
      int currentX = x;
      int currentSpan = desiredSpan;
      for (int i = 0; i < count; ++i)
        {
          View view = row.getView(i);
          int weight = view.getBreakWeight(axis, currentX, currentSpan);
          if (weight >= maxBreakWeight)
            {
              breakIndex = i;
              breakX = currentX;
              breakSpan = currentSpan;
              maxBreakWeight = weight;
            }
          int size = (int) view.getPreferredSpan(axis);
          currentX += size;
          currentSpan -= size;
        }

      // If there is a potential break location found, break the row at
      // this location.
      if (breakIndex > -1)
        {
          View toBeBroken = row.getView(breakIndex);
          View brokenView = toBeBroken.breakView(axis,
                                                 toBeBroken.getStartOffset(),
                                                 breakX, breakSpan);
          row.replace(breakIndex, count - breakIndex,
                      new View[]{brokenView});
        }
    }
  }

  /**
   * This special subclass of <code>View</code> is used to represent
   * the logical representation of this view. It does not support any
   * visual representation, this is handled by the physical view implemented
   * in the <code>FlowView</code>.
   */
  class LogicalView extends BoxView
  {
    /**
     * Creates a new LogicalView instance.
     */
    LogicalView(Element el, int axis)
    {
      super(el, axis);
    }
  }

  /**
   * The shared instance of FlowStrategy.
   */
  static final FlowStrategy sharedStrategy = new FlowStrategy();

  /**
   * The span of the <code>FlowView</code> that should be flowed.
   */
  protected int layoutSpan;

  /**
   * Represents the logical child elements of this view, encapsulated within
   * one parent view (an instance of a package private <code>LogicalView</code>
   * class). These will be translated to a set of real views that are then
   * displayed on screen. This translation is performed by the inner class
   * {@link FlowStrategy}.
   */
  protected View layoutPool;

  /**
   * The <code>FlowStrategy</code> to use for translating between the
   * logical and physical view.
   */
  protected FlowStrategy strategy;

  /**
   * Indicates if the flow should be rebuild during the next layout.
   */
  private boolean layoutDirty;

  /**
   * Creates a new <code>FlowView</code> for the given
   * <code>Element</code> and <code>axis</code>.
   *
   * @param element the element that is rendered by this FlowView
   * @param axis the axis along which the view is tiled, either
   *        <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>, the flow
   *        axis is orthogonal to this one
   */
  public FlowView(Element element, int axis)
  {
    super(element, axis);
    strategy = sharedStrategy;
    layoutDirty = true;
  }

  /**
   * Returns the axis along which the view should be flowed. This is
   * orthogonal to the axis along which the boxes are tiled.
   *
   * @return the axis along which the view should be flowed
   */
  public int getFlowAxis()
  {
    int axis = getAxis();
    int flowAxis;
 
    if (axis == X_AXIS)
      flowAxis = Y_AXIS;
    else
      flowAxis = X_AXIS;

    return flowAxis;

  }

  /**
   * Returns the span of the flow for the specified child view. A flow
   * layout can be shaped by providing different span values for different
   * child indices. The default implementation returns the entire available
   * span inside the view.
   *
   * @param index the index of the child for which to return the span
   *
   * @return the span of the flow for the specified child view
   */
  public int getFlowSpan(int index)
  {
    return layoutSpan;
  }

  /**
   * Returns the location along the flow axis where the flow span starts
   * given a child view index. The flow can be shaped by providing
   * different values here.
   *
   * @param index the index of the child for which to return the flow location
   *
   * @return the location along the flow axis where the flow span starts
   */
  public int getFlowStart(int index)
  {
    return getLeftInset(); // TODO: Is this correct?
  }

  /**
   * Creates a new view that represents a row within a flow.
   *
   * @return a view for a new row
   */
  protected abstract View createRow();

  /**
   * Loads the children of this view. The <code>FlowView</code> does not
   * directly load its children. Instead it creates a logical view
   * ({@link #layoutPool}) which is filled by the logical child views.
   * The real children are created at layout time and each represent one
   * row.
   *
   * This method is called by {@link View#setParent} in order to initialize
   * the view.
   *
   * @param vf the view factory to use for creating the child views
   */
  protected void loadChildren(ViewFactory vf)
  {
    if (layoutPool == null)
      {
        layoutPool = new LogicalView(getElement(), getAxis());
        layoutPool.setParent(this);
      }
  }

  /**
   * Performs the layout of this view. If the span along the flow axis changed,
   * this first calls {@link FlowStrategy#layout} in order to rebuild the
   * rows of this view. Then the superclass's behaviour is called to arrange
   * the rows within the box.
   *
   * @param width the width of the view
   * @param height the height of the view
   */
  protected void layout(int width, int height)
  {
    int flowAxis = getFlowAxis();
    if (flowAxis == X_AXIS)
      {
        if (layoutSpan != width)
          {
            layoutChanged(Y_AXIS);
            layoutSpan = width;
          }
      }
    else
      {
        if (layoutSpan != height)
          {
            layoutChanged(X_AXIS);
            layoutSpan = height;
          }
      }

    if (layoutDirty)
      {
        strategy.layout(this);
        layoutDirty = false;
      }

    if (getPreferredSpan(getAxis()) != height)
      preferenceChanged(this, false, true);

    super.layout(width, height);
  }

  /**
   * Receice notification that some content has been inserted in the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#insertUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void insertUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    // First we must send the insertUpdate to the logical view so it can
    // be updated accordingly.
    layoutPool.insertUpdate(changes, a, vf);
    strategy.insertUpdate(this, changes, getInsideAllocation(a));
    layoutDirty = true;
  }

  /**
   * Receice notification that some content has been removed from the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#removeUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void removeUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    strategy.removeUpdate(this, changes, getInsideAllocation(a));
    layoutDirty = true;
  }

  /**
   * Receice notification that some attributes changed in the region
   * that this view is responsible for. This calls
   * {@link FlowStrategy#changedUpdate}.
   *
   * @param changes the document event describing the changes
   * @param a the current allocation of the view
   * @param vf the view factory that is used for creating new child views
   */
  public void changedUpdate(DocumentEvent changes, Shape a, ViewFactory vf)
  {
    strategy.changedUpdate(this, changes, getInsideAllocation(a));
    layoutDirty = true;
  }

  /**
   * Returns the index of the child <code>View</code> for the given model
   * position.
   *
   * This is implemented to iterate over the children of this
   * view (the rows) and return the index of the first view that contains
   * the given position.
   *
   * @param pos the model position for whicht the child <code>View</code> is
   *        queried
   *
   * @return the index of the child <code>View</code> for the given model
   *         position
   */
  protected int getViewIndexAtPosition(int pos)
  {
    // First make sure we have a valid layout.
    if (!isAllocationValid())
      layout(getWidth(), getHeight());

    int count = getViewCount();
    int result = -1;

    for (int i = 0; i < count; ++i)
      {
        View child = getView(i);
        int start = child.getStartOffset();
        int end = child.getEndOffset();
        if (start <= pos && end > pos)
          {
            result = i;
            break;
          }
      }
    return result;
  }

  /**
   * Calculates the size requirements of this <code>BoxView</code> along
   * its minor axis, that is the axis opposite to the axis specified in the
   * constructor.
   *
   * This is overridden and forwards the request to the logical view.
   *
   * @param axis the axis that is examined
   * @param r the <code>SizeRequirements</code> object to hold the result,
   *        if <code>null</code>, a new one is created
   *
   * @return the size requirements for this <code>BoxView</code> along
   *         the specified axis
   */
  protected SizeRequirements calculateMinorAxisRequirements(int axis,
                                                            SizeRequirements r)
  {
    // We need to call super here so that the alignment is properly
    // calculated.
    SizeRequirements res = super.calculateMinorAxisRequirements(axis, r);
    res.minimum = (int) layoutPool.getMinimumSpan(axis);
    res.preferred = (int) layoutPool.getPreferredSpan(axis);
    res.maximum = (int) layoutPool.getMaximumSpan(axis);
    return res;
  }
}

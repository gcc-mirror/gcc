/* AsyncBoxView.java -- A box view that performs layout asynchronously
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


package javax.swing.text;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;

import javax.swing.event.DocumentEvent;
import javax.swing.text.Position.Bias;

/**
 * A {@link View} implementation that lays out its child views in a box, either
 * vertically or horizontally. The difference to {@link BoxView} is that the
 * layout is performed in an asynchronous manner. This helps to keep the
 * eventqueue free from non-GUI related tasks.
 *
 * This view is currently not used in standard text components. In order to
 * use it you would have to implement a special {@link EditorKit} with a
 * {@link ViewFactory} that returns this view. For example:
 *
 * <pre>
 * static class AsyncEditorKit extends StyledEditorKit implements ViewFactory
 * {
 *   public View create(Element el)
 *   {
 *     if (el.getName().equals(AbstractDocument.SectionElementName))
 *       return new AsyncBoxView(el, View.Y_AXIS);
 *     return super.getViewFactory().create(el);
 *   }
 *   public ViewFactory getViewFactory() {
 *     return this;
 *   }
 * }
 * </pre>
 *
 * @author Roman Kennke (kennke@aicas.com)
 *
 * @since 1.3
 */
public class AsyncBoxView
  extends View
{

  /**
   * Manages the effective position of child views. That keeps the visible
   * layout stable while the AsyncBoxView might be changing until the layout
   * thread decides to publish the new layout.
   */
  public class ChildLocator
  {

    /**
     * The last valid location.
     */
    protected ChildState lastValidOffset;

    /**
     * The last allocation.
     */
    protected Rectangle lastAlloc;

    /**
     * A Rectangle used for child allocation calculation to avoid creation
     * of lots of garbage Rectangle objects.
     */
    protected Rectangle childAlloc;

    /**
     * Creates a new ChildLocator.
     */
    public ChildLocator()
    {
      lastAlloc = new Rectangle();
      childAlloc = new Rectangle();
    }

    /**
     * Receives notification that a child has changed. This is called by
     * child state objects that have changed it's major span.
     *
     * This sets the {@link #lastValidOffset} field to <code>cs</code> if
     * the new child state's view start offset is smaller than the start offset
     * of the current child state's view or when <code>lastValidOffset</code>
     * is <code>null</code>.
     *
     * @param cs the child state object that has changed
     */
    public synchronized void childChanged(ChildState cs)
    {
      if (lastValidOffset == null
          || cs.getChildView().getStartOffset()
             < lastValidOffset.getChildView().getStartOffset())
        {
          lastValidOffset = cs;
        }
    }

    /**
     * Returns the view index of the view that occupies the specified area, or
     * <code>-1</code> if there is no such child view.
     *
     * @param x the x coordinate (relative to <code>a</code>)
     * @param y the y coordinate (relative to <code>a</code>)
     * @param a the current allocation of this view
     *
     * @return the view index of the view that occupies the specified area, or
     *         <code>-1</code> if there is no such child view
     */
    public int getViewIndexAtPoint(float x, float y, Shape a)
    {
      setAllocation(a);
      float targetOffset = (getMajorAxis() == X_AXIS) ? x - lastAlloc.x
                                                      : y - lastAlloc.y;
      int index = getViewIndexAtVisualOffset(targetOffset);
      return index;
    }

    /**
     * Returns the current allocation for a child view. This updates the
     * offsets for all children <em>before</em> the requested child view.
     *
     * @param index the index of the child view
     * @param a the current allocation of this view
     * 
     * @return the current allocation for a child view
     */
    public synchronized Shape getChildAllocation(int index, Shape a)
    {
      if (a == null)
        return null;
      setAllocation(a);
      ChildState cs = getChildState(index);
      if (cs.getChildView().getStartOffset()
          > lastValidOffset.getChildView().getStartOffset())
        {
          updateChildOffsetsToIndex(index);
        }
      Shape ca = getChildAllocation(index);
      return ca;
    }

    /**
     * Paints all child views.
     *
     * @param g the graphics context to use
     */
    public synchronized void paintChildren(Graphics g)
    {
      Rectangle clip = g.getClipBounds();
      float targetOffset = (getMajorAxis() == X_AXIS) ? clip.x - lastAlloc.x
                                                      : clip.y - lastAlloc.y;
      int index = getViewIndexAtVisualOffset(targetOffset);
      int n = getViewCount();
      float offs = getChildState(index).getMajorOffset();
      for (int i = index; i < n; i++)
        {
          ChildState cs = getChildState(i);
          cs.setMajorOffset(offs);
          Shape ca = getChildAllocation(i);
          if (ca.intersects(clip))
            {
              synchronized (cs)
                {
                  View v = cs.getChildView();
                  v.paint(g, ca);
                }
            }
          else
            {
              // done painting intersection
              break;
            }
          offs += cs.getMajorSpan();
        }
    }

    /**
     * Returns the current allocation of the child view with the specified
     * index. Note that this will <em>not</em> update any location information.
     * 
     * @param index the index of the requested child view
     *
     * @return the current allocation of the child view with the specified
     *         index
     */
    protected Shape getChildAllocation(int index)
    {
      ChildState cs = getChildState(index);
      if (! cs.isLayoutValid())
          cs.run();

      if (getMajorAxis() == X_AXIS)
        {
          childAlloc.x = lastAlloc.x + (int) cs.getMajorOffset();
          childAlloc.y = lastAlloc.y + (int) cs.getMinorOffset();
          childAlloc.width = (int) cs.getMajorSpan();
          childAlloc.height = (int) cs.getMinorSpan();
        }
      else
        {
          childAlloc.y = lastAlloc.y + (int) cs.getMajorOffset();
          childAlloc.x = lastAlloc.x + (int) cs.getMinorOffset();
          childAlloc.height = (int) cs.getMajorSpan();
          childAlloc.width = (int) cs.getMinorSpan();
        }
      return childAlloc;
    }

    /**
     * Sets the current allocation for this view.
     *
     * @param a the allocation to set
     */
    protected void setAllocation(Shape a)
    {
      if (a instanceof Rectangle)
        lastAlloc.setBounds((Rectangle) a);
      else
        lastAlloc.setBounds(a.getBounds());

      setSize(lastAlloc.width, lastAlloc.height);
    }

    /**
     * Returns the index of the view at the specified offset along the major
     * layout axis.
     *
     * @param targetOffset the requested offset
     *
     * @return the index of the view at the specified offset along the major
     * layout axis
     */
    protected int getViewIndexAtVisualOffset(float targetOffset)
    {
      int n = getViewCount();
      if (n > 0)
        {
          if (lastValidOffset == null)
            lastValidOffset = getChildState(0);
          if (targetOffset > majorSpan)
            return 0;
          else if (targetOffset > lastValidOffset.getMajorOffset())
            return updateChildOffsets(targetOffset);
          else
            {
              float offs = 0f;
              for (int i = 0; i < n; i++)
                {
                  ChildState cs = getChildState(i);
                  float nextOffs = offs + cs.getMajorSpan();
                  if (targetOffset < nextOffs)
                    return i;
                  offs = nextOffs;
                }
            }
        }
      return n - 1;
    }

    /**
     * Updates all the child view offsets up to the specified targetOffset.
     *
     * @param targetOffset the offset up to which the child view offsets are
     *        updated
     *
     * @return the index of the view at the specified offset
     */
    private int updateChildOffsets(float targetOffset)
    {
      int n = getViewCount();
      int targetIndex = n - 1;
      int pos = lastValidOffset.getChildView().getStartOffset();
      int startIndex = getViewIndexAtPosition(pos, Position.Bias.Forward);
      float start = lastValidOffset.getMajorOffset();
      float lastOffset = start;
      for (int i = startIndex; i < n; i++)
        {
          ChildState cs = getChildState(i);
          cs.setMajorOffset(lastOffset);
          lastOffset += cs.getMajorSpan();
          if (targetOffset < lastOffset)
            {
              targetIndex = i;
              lastValidOffset = cs;
              break;
            }
        }
      return targetIndex;
    }

    /**
     * Updates the offsets of the child views up to the specified index.
     *
     * @param index the index up to which the offsets are updated
     */
    private void updateChildOffsetsToIndex(int index)
    {
      int pos = lastValidOffset.getChildView().getStartOffset();
      int startIndex = getViewIndexAtPosition(pos, Position.Bias.Forward);
      float lastOffset = lastValidOffset.getMajorOffset();
      for (int i = startIndex; i <= index; i++)
        {
          ChildState cs = getChildState(i);
          cs.setMajorOffset(lastOffset);
          lastOffset += cs.getMajorSpan();
        }
    }
  }

  /**
   * Represents the layout state of a child view.
   */
  public class ChildState
    implements Runnable
  {

    /**
     * The child view for this state record.
     */
    private View childView;

    /**
     * Indicates if the minor axis requirements of this child view are valid
     * or not.
     */
    private boolean minorValid;

    /**
     * Indicates if the major axis requirements of this child view are valid
     * or not.
     */
    private boolean majorValid;

    /**
     * Indicates if the current child size is valid. This is package private
     * to avoid synthetic accessor method.
     */
    boolean childSizeValid;

    /**
     * The child views minimumSpan. This is package private to avoid accessor
     * method.
     */
    float minimum;

    /**
     * The child views preferredSpan. This is package private to avoid accessor
     * method.
     */
    float preferred;

    /**
     * The current span of the child view along the major axis.
     */
    private float majorSpan;

    /**
     * The current offset of the child view along the major axis.
     */
    private float majorOffset;

    /**
     * The current span of the child view along the minor axis.
     */
    private float minorSpan;

    /**
     * The current offset of the child view along the major axis.
     */
    private float minorOffset;

    /**
     * The child views maximumSpan.
     */
    private float maximum;

    /**
     * Creates a new <code>ChildState</code> object for the specified child
     * view.
     *
     * @param view the child view for which to create the state record
     */
    public ChildState(View view)
    {
      childView = view;
    }

    /**
     * Returns the child view for which this <code>ChildState</code> represents
     * the layout state.
     *
     * @return the child view for this child state object 
     */
    public View getChildView()
    {
      return childView;
    }

    /**
     * Returns <code>true</code> if the current layout information is valid,
     * <code>false</code> otherwise.
     *
     * @return <code>true</code> if the current layout information is valid,
     *         <code>false</code> otherwise
     */
    public boolean isLayoutValid()
    {
      return minorValid && majorValid && childSizeValid;
    }

    /**
     * Performs the layout update for the child view managed by this
     * <code>ChildState</code>.
     */
    public void run()
    {
      Document doc = getDocument();
      if (doc instanceof AbstractDocument)
        {
          AbstractDocument abstractDoc = (AbstractDocument) doc;
          abstractDoc.readLock();
        }

      try
        {

          if (!(minorValid &&  majorValid && childSizeValid)
              && childView.getParent() == AsyncBoxView.this)
            {
              synchronized(AsyncBoxView.this)
              {
                changing = this;
              }
              update();
              synchronized(AsyncBoxView.this)
              {
                changing = null;
              }
              // Changing the major axis may cause the minor axis
              // requirements to have changed, so we need to do this again.
              update();
            }
        }
      finally
        {
          if (doc instanceof AbstractDocument)
            {
              AbstractDocument abstractDoc = (AbstractDocument) doc;
              abstractDoc.readUnlock();
            }
        }
    }

    /**
     * Performs the actual update after the run methods has made its checks
     * and locked the document.
     */
    private void update()
    {
      int majorAxis = getMajorAxis();
      boolean minorUpdated = false;
      synchronized (this)
        {
          if (! minorValid)
            {
              int minorAxis = getMinorAxis();
              minimum = childView.getMinimumSpan(minorAxis);
              preferred = childView.getPreferredSpan(minorAxis);
              maximum = childView.getMaximumSpan(minorAxis);
              minorValid = true;
              minorUpdated = true;
            }
        }
      if (minorUpdated)
        minorRequirementChange(this);

      boolean majorUpdated = false;
      float delta = 0.0F;
      synchronized (this)
        {
          if (! majorValid)
            {
              float oldSpan = majorSpan;
              majorSpan = childView.getPreferredSpan(majorAxis);
              delta = majorSpan - oldSpan;
              majorValid = true;
              majorUpdated = true;
            }
        }
      if (majorUpdated)
        {
          majorRequirementChange(this, delta);
          locator.childChanged(this);
        }

      synchronized (this)
        {
          if (! childSizeValid)
            {
              float w;
              float h;
              if (majorAxis == X_AXIS)
                {
                  w = majorSpan;
                  h = getMinorSpan();
                }
              else
                {
                  w = getMinorSpan();
                  h = majorSpan;
                }
              childSizeValid = true;
              childView.setSize(w, h);
            }
        }
    }

    /**
     * Returns the span of the child view along the minor layout axis.
     *
     * @return the span of the child view along the minor layout axis
     */
    public float getMinorSpan()
    {
      float retVal;
      if (maximum < minorSpan)
        retVal = maximum;
      else
        retVal = Math.max(minimum, minorSpan);
      return retVal;
    }

    /**
     * Returns the offset of the child view along the minor layout axis.
     *
     * @return the offset of the child view along the minor layout axis
     */
    public float getMinorOffset()
    {
      float retVal;
      if (maximum < minorSpan)
        {
          float align = childView.getAlignment(getMinorAxis());
          retVal = ((minorSpan - maximum) * align);
        }
      else
        retVal = 0f;

      return retVal;
    }

    /**
     * Returns the span of the child view along the major layout axis.
     *
     * @return the span of the child view along the major layout axis
     */

    public float getMajorSpan()
    {
      return majorSpan;
    }

    /**
     * Returns the offset of the child view along the major layout axis.
     *
     * @return the offset of the child view along the major layout axis
     */
    public float getMajorOffset()
    {
      return majorOffset;
    }

    /**
     * Sets the offset of the child view along the major layout axis. This
     * should only be called by the ChildLocator of that child view.
     *
     * @param offset the offset to set
     */
    public void setMajorOffset(float offset)
    {
      majorOffset = offset;
    }

    /**
     * Mark the preferences changed for that child. This forwards to
     * {@link AsyncBoxView#preferenceChanged}.
     *
     * @param width <code>true</code> if the width preference has changed
     * @param height <code>true</code> if the height preference has changed
     */
    public void preferenceChanged(boolean width, boolean height)
    {
      if (getMajorAxis() == X_AXIS)
        {
          if (width)
            majorValid = false;
          if (height)
            minorValid = false;
        }
      else
        {
          if (width)
            minorValid = false;
          if (height)
            majorValid = false;
        }
      childSizeValid = false;
    }
  }

  /**
   * Flushes the requirements changes upwards asynchronously.
   */
  private class FlushTask implements Runnable
  {
    /**
     * Starts the flush task. This obtains a readLock on the document
     * and then flushes all the updates using
     * {@link AsyncBoxView#flushRequirementChanges()} after updating the
     * requirements.
     */
    public void run()
    {
      try
        {
          // Acquire a lock on the document.
          Document doc = getDocument();
          if (doc instanceof AbstractDocument)
            {
              AbstractDocument abstractDoc = (AbstractDocument) doc;
              abstractDoc.readLock();
            }

          int n = getViewCount();
          if (minorChanged && (n > 0))
            {
              LayoutQueue q = getLayoutQueue();
              ChildState min = getChildState(0);
              ChildState pref = getChildState(0);
              for (int i = 1; i < n; i++)
                {
                  ChildState cs = getChildState(i);
                  if (cs.minimum > min.minimum)
                    min = cs;
                  if (cs.preferred > pref.preferred)
                    pref = cs;
                }
              synchronized (AsyncBoxView.this)
              {
                minReq = min;
                prefReq = pref;
              }
            }

          flushRequirementChanges();
        }
      finally
      {
        // Release the lock on the document.
        Document doc = getDocument();
        if (doc instanceof AbstractDocument)
          {
            AbstractDocument abstractDoc = (AbstractDocument) doc;
            abstractDoc.readUnlock();
          }
      }
    }

  }

  /**
   * The major layout axis.
   */
  private int majorAxis;

  /**
   * The top inset.
   */
  private float topInset;

  /**
   * The bottom inset.
   */
  private float bottomInset;

  /**
   * The left inset.
   */
  private float leftInset;

  /**
   * Indicates if the major span should be treated as beeing estimated or not.
   */
  private boolean estimatedMajorSpan;

  /**
   * The right inset.
   */
  private float rightInset;

  /**
   * The children and their layout statistics.
   */
  private ArrayList childStates;

  /**
   * The currently changing child state. May be null if there is no child state
   * updating at the moment. This is package private to avoid a synthetic
   * accessor method inside ChildState.
   */
  ChildState changing;

  /**
   * Represents the minimum requirements. This is used in
   * {@link #getMinimumSpan(int)}.
   */
  ChildState minReq;

  /**
   * Represents the minimum requirements. This is used in
   * {@link #getPreferredSpan(int)}.
   */
  ChildState prefReq;

  /**
   * Indicates that the major axis requirements have changed.
   */
  private boolean majorChanged;

  /**
   * Indicates that the minor axis requirements have changed. This is package
   * private to avoid synthetic accessor method.
   */
  boolean minorChanged;

  /**
   * The current span along the major layout axis. This is package private to
   * avoid synthetic accessor method.
   */
  float majorSpan;

  /**
   * The current span along the minor layout axis. This is package private to
   * avoid synthetic accessor method.
   */
  float minorSpan;

  /**
   * This tasked is placed on the layout queue to flush updates up to the
   * parent view.
   */
  private Runnable flushTask;

  /**
   * The child locator for this view.
   */
  protected ChildLocator locator;

  /**
   * Creates a new <code>AsyncBoxView</code> that represents the specified
   * element and layouts its children along the specified axis.
   *
   * @param elem the element
   * @param axis the layout axis
   */
  public AsyncBoxView(Element elem, int axis)
  {
    super(elem);
    majorAxis = axis;
    childStates = new ArrayList();
    flushTask = new FlushTask();
    locator = new ChildLocator();
    minorSpan = Short.MAX_VALUE;
  }

  /**
   * Returns the major layout axis.
   *
   * @return the major layout axis
   */
  public int getMajorAxis()
  {
    return majorAxis;
  }

  /**
   * Returns the minor layout axis, that is the axis orthogonal to the major
   * layout axis.
   *
   * @return the minor layout axis
   */
  public int getMinorAxis()
  {
    return majorAxis == X_AXIS ? Y_AXIS : X_AXIS;
  }

  /**
   * Returns the view at the specified <code>index</code>.
   *
   * @param index the index of the requested child view
   *
   * @return the view at the specified <code>index</code>
   */
  public View getView(int index)
  {
    View view = null;
    synchronized(childStates)
      {
        if ((index >= 0) && (index < childStates.size()))
          {
            ChildState cs = (ChildState) childStates.get(index);
            view = cs.getChildView();
          }
      }
    return view;
  }

  /**
   * Returns the number of child views.
   *
   * @return the number of child views
   */
  public int getViewCount()
  {
    synchronized(childStates)
    {
      return childStates.size();
    }
  }

  /**
   * Returns the view index of the child view that represents the specified
   * model position.
   *
   * @param pos the model position for which we search the view index
   * @param bias the bias
   *
   * @return the view index of the child view that represents the specified
   *         model position
   */
  public int getViewIndex(int pos, Position.Bias bias)
  {
    int retVal = -1;

    if (bias == Position.Bias.Backward)
      pos = Math.max(0, pos - 1);

    // TODO: A possible optimization would be to implement a binary search
    // here.
    int numChildren = childStates.size();
    if (numChildren > 0)
      {
        for (int i = 0; i < numChildren; ++i)
          {
            View child = ((ChildState) childStates.get(i)).getChildView();
            if (child.getStartOffset() <= pos && child.getEndOffset() > pos)
              {
                retVal = i;
                break;
              }
          }
      }
    return retVal;
  }

  /**
   * Returns the top inset.
   *
   * @return the top inset
   */
  public float getTopInset()
  {
    return topInset;
  }

  /**
   * Sets the top inset.
   *
   * @param top the top inset
   */
  public void setTopInset(float top)
  {
    topInset = top;
  }

  /**
   * Returns the bottom inset.
   *
   * @return the bottom inset
   */
  public float getBottomInset()
  {
    return bottomInset;
  }

  /**
   * Sets the bottom inset.
   *
   * @param bottom the bottom inset
   */
  public void setBottomInset(float bottom)
  {
    bottomInset = bottom;
  }

  /**
   * Returns the left inset.
   *
   * @return the left inset
   */
  public float getLeftInset()
  {
    return leftInset;
  }

  /**
   * Sets the left inset.
   *
   * @param left the left inset
   */
  public void setLeftInset(float left)
  {
    leftInset = left;
  }

  /**
   * Returns the right inset.
   *
   * @return the right inset
   */
  public float getRightInset()
  {
    return rightInset;
  }

  /**
   * Sets the right inset.
   *
   * @param right the right inset
   */
  public void setRightInset(float right)
  {
    rightInset = right;
  }

  /**
   * Loads the child views of this view. This is triggered by
   * {@link #setParent(View)}.
   *
   * @param f the view factory to build child views with
   */
  protected void loadChildren(ViewFactory f)
  {
    Element e = getElement();
    int n = e.getElementCount();
    if (n > 0)
      {
        View[] added = new View[n];
        for (int i = 0; i < n; i++)
          {
            added[i] = f.create(e.getElement(i));
          }
        replace(0, 0, added);
      }
  }
  
  /**
   * Returns the span along an axis that is taken up by the insets.
   *
   * @param axis the axis
   *
   * @return the span along an axis that is taken up by the insets
   *
   * @since 1.4
   */
  protected float getInsetSpan(int axis)
  {
    float span;
    if (axis == X_AXIS)
      span = leftInset + rightInset;
    else
      span = topInset + bottomInset;
    return span;
  }

  /**
   * Sets the <code>estimatedMajorSpan</code> property that determines if
   * the major span should be treated as beeing estimated.
   *
   * @param estimated if the major span should be treated as estimated or not
   *
   * @since 1.4
   */
  protected void setEstimatedMajorSpan(boolean estimated)
  {
    estimatedMajorSpan = estimated;
  }

  /**
   * Determines whether the major span should be treated as estimated or as
   * beeing accurate.
   *
   * @return <code>true</code> if the major span should be treated as
   *         estimated, <code>false</code> if the major span should be treated
   *         as accurate
   *
   * @since 1.4
   */
  protected boolean getEstimatedMajorSpan()
  {
    return estimatedMajorSpan;
  }

  /**
   * Receives notification from the child states that the requirements along
   * the minor axis have changed.
   *
   * @param cs the child state from which this notification is messaged
   */
  protected synchronized void minorRequirementChange(ChildState cs)
  {
    minorChanged = true;
  }

  /**
   * Receives notification from the child states that the requirements along
   * the major axis have changed.
   *
   * @param cs the child state from which this notification is messaged
   */
  protected void majorRequirementChange(ChildState cs, float delta)
  {
    if (! estimatedMajorSpan)
      majorSpan += delta;
    majorChanged = true;
  }

  /**
   * Sets the parent for this view. This calls loadChildren if
   * <code>parent</code> is not <code>null</code> and there have not been any
   * child views initializes.
   *
   * @param parent the new parent view; <code>null</code> if this view is
   *        removed from the view hierarchy
   *
   * @see View#setParent(View)
   */
  public void setParent(View parent)
  {
    super.setParent(parent);
    if ((parent != null) && (getViewCount() == 0))
      {
        ViewFactory f = getViewFactory();
        loadChildren(f);
      }
  }

  /**
   * Sets the size of this view. This is ususally called before {@link #paint}
   * is called to make sure the view has a valid layout.
   *
   * This implementation queues layout requests for every child view if the
   * minor axis span has changed. (The major axis span is requested to never
   * change for this view).
   *
   * @param width the width of the view
   * @param height the height of the view
   */
  public void setSize(float width, float height)
  {
    float targetSpan;
    if (majorAxis == X_AXIS)
      targetSpan = height - getTopInset() - getBottomInset();
    else
      targetSpan = width - getLeftInset() - getRightInset();

    if (targetSpan != minorSpan)
      {
        minorSpan = targetSpan;

        int n = getViewCount();
        LayoutQueue q = getLayoutQueue();
        for (int i = 0; i < n; i++)
          {
            ChildState cs = getChildState(i);
            cs.childSizeValid = false;
            q.addTask(cs);
          }
        q.addTask(flushTask);
    }
  }

  /**
   * Replaces child views with new child views.
   *
   * This creates ChildState objects for all the new views and adds layout
   * requests for them to the layout queue.
   *
   * @param offset the offset at which to remove/insert
   * @param length the number of child views to remove
   * @param views the new child views to insert
   */
  public void replace(int offset, int length, View[] views)
  {
    synchronized(childStates)
      {
        LayoutQueue q = getLayoutQueue();
        for (int i = 0; i < length; i++)
          childStates.remove(offset);

        for (int i = views.length - 1; i >= 0; i--)
          childStates.add(offset, createChildState(views[i]));

        // We need to go through the new child states _after_ they have been
        // added to the childStates list, otherwise the layout tasks may find
        // an incomplete child list. That means we have to loop through
        // them again, but what else can we do?
        if (views.length != 0)
          {
            for (int i = 0; i < views.length; i++)
              {
                ChildState cs = (ChildState) childStates.get(i + offset);
                cs.getChildView().setParent(this);
                q.addTask(cs);
              }
            q.addTask(flushTask);
          }
      }
  }

  /**
   * Paints the view. This requests the {@link ChildLocator} to paint the views
   * after setting the allocation on it.
   *
   * @param g the graphics context to use
   * @param s the allocation for this view
   */
  public void paint(Graphics g, Shape s)
  {
    synchronized (locator)
      {
        locator.setAllocation(s);
        locator.paintChildren(g);
      }
  }

  /**
   * Returns the preferred span of this view along the specified layout axis.
   *
   * @return the preferred span of this view along the specified layout axis
   */
  public float getPreferredSpan(int axis)
  {
    float retVal;
    if (majorAxis == axis)
      retVal = majorSpan;

    else if (prefReq != null)
      {
        View child = prefReq.getChildView();
        retVal = child.getPreferredSpan(axis);
      }

    // If we have no layout information yet, then return insets + 30 as
    // an estimation.
    else
      {
        if (axis == X_AXIS)
          retVal = getLeftInset() + getRightInset() + 30;
        else
          retVal = getTopInset() + getBottomInset() + 30;
      }
    return retVal;
  }

  /**
   * Maps a model location to view coordinates.
   *
   * @param pos the model location
   * @param a the current allocation of this view
   * @param b the bias
   *
   * @return the view allocation for the specified model location
   */
  public Shape modelToView(int pos, Shape a, Bias b)
    throws BadLocationException
  {
    int index = getViewIndexAtPosition(pos, b);
    Shape ca = locator.getChildAllocation(index, a);

    ChildState cs = getChildState(index);
    synchronized (cs)
      {
        View cv = cs.getChildView();
        Shape v = cv.modelToView(pos, ca, b);
        return v;
      }
  }

  /**
   * Maps view coordinates to a model location.
   *
   * @param x the x coordinate (relative to <code>a</code>)
   * @param y the y coordinate (relative to <code>a</code>)
   * @param b holds the bias of the model location on method exit
   *
   * @return the model location for the specified view location
   */
  public int viewToModel(float x, float y, Shape a, Bias[] b)
  {
    int pos;
    int index;
    Shape ca;

    synchronized (locator)
      {
        index = locator.getViewIndexAtPoint(x, y, a);
        ca = locator.getChildAllocation(index, a);
      }

    ChildState cs = getChildState(index);
    synchronized (cs)
      {
        View v = cs.getChildView();
        pos = v.viewToModel(x, y, ca, b);
      }
    return pos;
  }

  /**
   * Returns the child allocation for the child view with the specified
   * <code>index</code>.
   *
   * @param index the index of the child view
   * @param a the current allocation of this view
   *
   * @return the allocation of the child view
   */
  public Shape getChildAllocation(int index, Shape a)
  {
    Shape ca = locator.getChildAllocation(index, a);
    return ca;
  }

  /**
   * Returns the maximum span of this view along the specified axis.
   * This is implemented to return the <code>preferredSpan</code> for the
   * major axis (that means the box can't be resized along the major axis) and
   * {@link Short#MAX_VALUE} for the minor axis.
   *
   * @param axis the axis
   *
   * @return the maximum span of this view along the specified axis
   */
  public float getMaximumSpan(int axis)
  {
    float max;
    if (axis == majorAxis)
      max = getPreferredSpan(axis);
    else
      max = Short.MAX_VALUE;
    return max;
  }

  /**
   * Returns the minimum span along the specified axis.
   */
  public float getMinimumSpan(int axis)
  {
    float min;
    if (axis == majorAxis)
      min = getPreferredSpan(axis);
    else
      {
        if (minReq != null)
          {
            View child = minReq.getChildView();
            min = child.getMinimumSpan(axis);
          }
        else
          {
            // No layout information yet. Return insets + 5 as some kind of
            // estimation.
            if (axis == X_AXIS)
              min = getLeftInset() + getRightInset() + 5;
            else
              min = getTopInset() + getBottomInset() + 5;
          }
      }
    return min;
  }

  /**
   * Receives notification that one of the child views has changed its
   * layout preferences along one or both axis.
   *
   * This queues a layout request for that child view if necessary.
   *
   * @param view the view that has changed its preferences
   * @param width <code>true</code> if the width preference has changed
   * @param height <code>true</code> if the height preference has changed
   */
  public synchronized void preferenceChanged(View view, boolean width,
                                             boolean height)
  {
    if (view == null)
      getParent().preferenceChanged(this, width, height);
    else
      {
        if (changing != null)
          {
            View cv = changing.getChildView();
            if (cv == view)
              {
                changing.preferenceChanged(width, height);
                return;
              }
          }
        int index = getViewIndexAtPosition(view.getStartOffset(), 
                                           Position.Bias.Forward);
        ChildState cs = getChildState(index);
        cs.preferenceChanged(width, height);
        LayoutQueue q = getLayoutQueue();
        q.addTask(cs);
        q.addTask(flushTask);
      }    
  }

  /**
   * Updates the layout for this view. This is implemented to trigger
   * {@link ChildLocator#childChanged} for the changed view, if there is
   * any.
   *
   * @param ec the element change, may be <code>null</code> if there were
   *        no changes to the element of this view
   * @param e the document event
   * @param a the current allocation of this view
   */
  protected void updateLayout(DocumentEvent.ElementChange ec, 
                              DocumentEvent e, Shape a)
  {
    if (ec != null)
      {
        int index = Math.max(ec.getIndex() - 1, 0);
        ChildState cs = getChildState(index);
        locator.childChanged(cs);
      }
  }
  
  
  /**
   * Returns the <code>ChildState</code> object associated with the child view
   * at the specified <code>index</code>.
   *
   * @param index the index of the child view for which to query the state
   *
   * @return the child state for the specified child view
   */
  protected ChildState getChildState(int index) {
    synchronized (childStates)
      {
        return (ChildState) childStates.get(index);
      }
  }

  /**
   * Returns the <code>LayoutQueue</code> used for layouting the box view.
   * This simply returns {@link LayoutQueue#getDefaultQueue()}.
   *
   * @return the <code>LayoutQueue</code> used for layouting the box view
   */
  protected LayoutQueue getLayoutQueue()
  {
    return LayoutQueue.getDefaultQueue();
  }

  /**
   * Returns the child view index of the view that represents the specified
   * position in the document model.
   * 
   * @param pos the position in the model
   * @param b the bias
   *
   * @return the child view index of the view that represents the specified
   *         position in the document model
   */
  protected synchronized int getViewIndexAtPosition(int pos, Position.Bias b)
  {
    if (b == Position.Bias.Backward)
      pos = Math.max(0, pos - 1);
    Element elem = getElement();
    return elem.getElementIndex(pos);
  }

  /**
   * Creates a <code>ChildState</code> object for the specified view.
   *
   * @param v the view for which to create a child state object
   *
   * @return the created child state
   */
  protected ChildState createChildState(View v)
  {
    return new ChildState(v);
  }

  /**
   * Flushes the requirements changes upwards to the parent view. This is
   * called from the layout thread.
   */
  protected synchronized void flushRequirementChanges()
  {
    if (majorChanged || minorChanged)
      {
        View p = getParent();
        if (p != null)
          {
            boolean horizontal;
            boolean vertical;
            if (majorAxis == X_AXIS)
              {
                horizontal = majorChanged;
                vertical = minorChanged;
              }
            else
              {
                vertical = majorChanged;
                horizontal = minorChanged;
              }

            p.preferenceChanged(this, horizontal, vertical);
            majorChanged = false;
            minorChanged = false;

            Component c = getContainer();
            if (c != null)
              c.repaint();
          }
      }
  }
}

/* Scrollbar.java -- AWT Scrollbar widget
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.peer.ScrollbarPeer;
import java.util.EventListener;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.accessibility.AccessibleValue;

/**
 * This class implements a scrollbar widget.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class Scrollbar extends Component implements Accessible, Adjustable
{
  // FIXME: Serialization readObject/writeObject

  /**
   * Constant indicating that a scrollbar is horizontal.
   */
  public static final int HORIZONTAL = 0;

  /**
   * Constant indicating that a scrollbar is vertical.
   */
  public static final int VERTICAL = 1;

  /**
   * Serialization Constant.
   */
  private static final long serialVersionUID = 8451667562882310543L;

  /**
   * @serial The amount by which the value of the scrollbar is changed
   * when incrementing in line mode.
   */
  private int lineIncrement;

  /**
   * @serial The amount by which the value of the scrollbar is changed
   * when incrementing in page mode.
   */
  private int pageIncrement;

  /**
   * @serial The maximum value for this scrollbar
   */
  private int maximum;

  /**
   * @serial The minimum value for this scrollbar
   */
  private int minimum;

  /**
   * @serial The orientation of this scrollbar, which will be either
   * the <code>HORIZONTAL</code> or <code>VERTICAL</code> constant
   * from this class.
   */
  private int orientation;

  /**
   * @serial The current value of this scrollbar.
   */
  private int value;

  /**
   * @serial The width of the scrollbar's thumb, which is relative
   * to the minimum and maximum value of the scrollbar.
   */
  private int visibleAmount;

  /**
   * List of AdjustmentListener's.
   */
  private AdjustmentListener adjustment_listeners;

  /**
   * true if the scrollbar is adjusting, false otherwise.
   */
  private transient boolean valueIsAdjusting = false;

  /**
   * The number used to generate the name returned by getName.
   */
  private static transient long next_scrollbar_number;

  /**
   * Initializes a new instance of <code>Scrollbar</code> with a
   * vertical orientation and default values for all other parameters.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true,
   */
  public Scrollbar()
  {
    this(VERTICAL);
  }

  /**
   * Initializes a new instance of <code>Scrollbar</code> with the
   * specified orientation and default values for all other parameters.
   * The orientation must be either the constant <code>HORIZONTAL</code> or
   * <code>VERTICAL</code> from this class.  An incorrect value will throw
   * an exception.
   *
   * @param orientation The orientation of this scrollbar.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true,
   * @exception IllegalArgumentException If the orientation value is not valid.
   */
  public Scrollbar(int orientation) throws IllegalArgumentException
  {
    this(orientation, 0, 10, 0, 100);
  }

  /**
   * Initializes a new instance of <code>Scrollbar</code> with the
   * specified parameters.  The orientation must be either the constant
   * <code>HORIZONTAL</code> or <code>VERTICAL</code>.  An incorrect value
   * will throw an exception.  Inconsistent values for other parameters
   * are silently corrected to valid values.
   *
   * @param orientation The orientation of this scrollbar.
   * @param value The initial value of the scrollbar.
   * @param visibleAmount The width of the scrollbar thumb.
   * @param minimum The minimum value of the scrollbar.
   * @param maximum The maximum value of the scrollbar.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true,
   * @exception IllegalArgumentException If the orientation value is not valid.
   */
  public Scrollbar(int orientation, int value, int visibleAmount, int minimum,
                   int maximum) throws IllegalArgumentException
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();

    if ((orientation != HORIZONTAL) && (orientation != VERTICAL))
      throw new IllegalArgumentException("Bad orientation value: "
                                         + orientation);

    this.orientation = orientation;

    setValues(value, visibleAmount, minimum, maximum);

    // Default is 1 according to online docs.
    lineIncrement = 1;

    // Default is 10 according to javadocs.
    pageIncrement = 10;
  }

  /**
   * Returns the orientation constant for this object.
   *
   * @return The orientation constant for this object.
   */
  public int getOrientation()
  {
    return orientation;
  }

  /**
   * Sets the orientation of this scrollbar to the specified value.  This
   * value must be either the constant <code>HORIZONTAL</code> or
   * <code>VERTICAL</code> from this class or an exception will be thrown.
   *
   * @param orientation The new orientation value.
   *
   * @exception IllegalArgumentException If the orientation value is not valid.
   */
  public void setOrientation(int orientation)
  {
    if ((orientation != HORIZONTAL) && (orientation != VERTICAL))
      throw new IllegalArgumentException("Bad orientation value: "
                                         + orientation);

    // FIXME: Communicate to peer?  Or must this be called before peer creation?
    this.orientation = orientation;
  }

  /**
   * Returns the current value for this scrollbar.
   *
   * @return The current value for this scrollbar.
   */
  public int getValue()
  {
    return value;
  }

  /**
   * Sets the current value for this scrollbar to the specified value.
   * If this is inconsistent with the minimum and maximum values for this
   * scrollbar, the value is silently adjusted.
   *
   * @param value The new value for this scrollbar.
   */
  public void setValue(int value)
  {
    setValues(value, visibleAmount, minimum, maximum);
  }

  /**
   * Returns the maximum value for this scrollbar.
   *
   * @return The maximum value for this scrollbar.
   */
  public int getMaximum()
  {
    return maximum;
  }

  /**
   * Sets the maximum value for this scrollbar to the specified value.
   * If the value is less than the current minimum value, it is silent
   * set to equal the minimum value.
   *
   * @param maximum The new maximum value for this scrollbar.
   */
  public void setMaximum(int maximum)
  {
    setValues(value, visibleAmount, minimum, maximum);
  }

  /**
   * Returns the minimum value for this scrollbar.
   *
   * @return The minimum value for this scrollbar.
   */
  public int getMinimum()
  {
    return minimum;
  }

  /**
   * Sets the minimum value for this scrollbar to the specified value.  If
   * this is not consistent with the current value and maximum, it is
   * silently adjusted to be consistent.
   *
   * @param minimum The new minimum value for this scrollbar.
   */
  public void setMinimum(int minimum)
  {
    setValues(value, visibleAmount, minimum, maximum);
  }

  /**
   * Returns the width of the scrollbar's thumb, in units relative to the
   * maximum and minimum value of the scrollbar.
   *
   * @return The width of the scrollbar's thumb.
   */
  public int getVisibleAmount()
  {
    return getVisible();
  }

  /**
   * Returns the width of the scrollbar's thumb, in units relative to the
   * maximum and minimum value of the scrollbar.
   *
   * @return The width of the scrollbar's thumb.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getVisibleAmount()</code>.
   */
  public int getVisible()
  {
    return visibleAmount;
  }

  /**
   * Sets the width of the scrollbar's thumb, in units relative to the
   * maximum and minimum value of the scrollbar.
   *
   * @param visibleAmount The new visible amount value of the scrollbar.
   */
  public void setVisibleAmount(int visibleAmount)
  {
    setValues(value, visibleAmount, minimum, maximum);
  }

  /**
   * Sets the current value, visible amount, minimum, and maximum for this
   * scrollbar.  These values are adjusted to be internally consistent
   * if necessary.
   *
   * @param value The new value for this scrollbar.
   * @param visibleAmount The new visible amount for this scrollbar.
   * @param minimum The new minimum value for this scrollbar.
   * @param maximum The new maximum value for this scrollbar.
   */
  public synchronized void setValues(int value, int visibleAmount,
                                     int minimum, int maximum)
  {
    if (visibleAmount <= 0)
      visibleAmount = 1;

    if (maximum <= minimum)
      maximum = minimum + 1;

    if (value < minimum)
      value = minimum;

    if (visibleAmount > maximum - minimum)
      visibleAmount = maximum - minimum;

    // According to documentation, the actual maximum
    // value is (maximum - visibleAmount)
    if (value > maximum - visibleAmount)
      value = maximum - visibleAmount;

    ScrollbarPeer peer = (ScrollbarPeer) getPeer();
    if (peer != null
        && (this.value != value || this.visibleAmount != visibleAmount
            || this.minimum != minimum || this.maximum != maximum))
      peer.setValues(value, visibleAmount, minimum, maximum);

    this.value = value;
    this.visibleAmount = visibleAmount;
    this.minimum = minimum;
    this.maximum = maximum;
  }

  /**
   * Returns the value added or subtracted when the user activates the scrollbar
   * scroll by a "unit" amount.
   *
   * @return The unit increment value.
   */
  public int getUnitIncrement()
  {
    return getLineIncrement();
  }

  /**
   * Returns the value added or subtracted when the user selects the scrollbar
   * scroll by a "unit" amount control.
   *
   * @return The unit increment value.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getUnitIncrement()</code>.
   */
  public int getLineIncrement()
  {
    return lineIncrement;
  }

  /**
   * Sets the value added or subtracted to the scrollbar value when the
   * user selects the scroll by a "unit" amount control.
   *
   * @param unitIncrement The new unit increment amount.
   */
  public synchronized void setUnitIncrement(int unitIncrement)
  {
    setLineIncrement(unitIncrement);
  }

  /**
   * Sets the value added or subtracted to the scrollbar value when the
   * user selects the scroll by a "unit" amount control.
   *
   * @param lineIncrement The new unit increment amount.
   *
   * @deprecated This method is deprecated in favor of
   * <code>setUnitIncrement()</code>.
   */
  public void setLineIncrement(int lineIncrement)
  {
    if (lineIncrement < 0)
      throw new IllegalArgumentException("Unit increment less than zero.");

    if (lineIncrement == 0)
      lineIncrement = 1;

   if (lineIncrement == this.lineIncrement)
      return;

    this.lineIncrement = lineIncrement;

    ScrollbarPeer peer = (ScrollbarPeer) getPeer();
    if (peer != null)
      peer.setLineIncrement(this.lineIncrement);
  }

  /**
   * Returns the value added or subtracted when the user activates the scrollbar
   * scroll by a "block" amount.
   *
   * @return The block increment value.
   */
  public int getBlockIncrement()
  {
    return getPageIncrement();
  }

  /**
   * Returns the value added or subtracted when the user selects the scrollbar
   * scroll by a "block" amount control.
   *
   * @return The block increment value.
   *
   * @deprecated This method is deprecated in favor of
   * <code>getBlockIncrement()</code>.
   */
  public int getPageIncrement()
  {
    return pageIncrement;
  }

  /**
   * Sets the value added or subtracted to the scrollbar value when the
   * user selects the scroll by a "block" amount control.
   *
   * @param blockIncrement The new block increment amount.
   */
  public synchronized void setBlockIncrement(int blockIncrement)
  {
    setPageIncrement(blockIncrement);
  }

  /**
   * Sets the value added or subtracted to the scrollbar value when the
   * user selects the scroll by a "block" amount control.
   *
   * @param pageIncrement The new block increment amount.
   *
   * @deprecated This method is deprecated in favor of
   * <code>setBlockIncrement()</code>.
   */
  public void setPageIncrement(int pageIncrement)
  {
    if (pageIncrement < 0)
      throw new IllegalArgumentException("Block increment less than zero.");

    if (pageIncrement == 0)
      pageIncrement = 1;

    if (pageIncrement == this.pageIncrement)
      return;

    this.pageIncrement = pageIncrement;

    ScrollbarPeer peer = (ScrollbarPeer) getPeer();
    if (peer != null)
      peer.setPageIncrement(this.pageIncrement);
  }

  /**
   * Notifies this object to create its native peer.
   */
  public synchronized void addNotify()
  {
    if (peer == null)
      peer = getToolkit().createScrollbar(this);
    super.addNotify();
  }

  /**
   * Adds a new adjustment listener to the list of registered listeners
   * for this object.
   *
   * @param listener The listener to add.
   */
  public synchronized void addAdjustmentListener(AdjustmentListener listener)
  {
    adjustment_listeners = AWTEventMulticaster.add(adjustment_listeners,
                                                   listener);
    enableEvents(AWTEvent.ADJUSTMENT_EVENT_MASK);
  }

  /**
   * Removes the specified listener from the list of registered listeners
   * for this object.
   *
   * @param listener The listener to remove.
   */
  public synchronized void removeAdjustmentListener(AdjustmentListener listener)
  {
    adjustment_listeners = AWTEventMulticaster.remove(adjustment_listeners,
                                                      listener);
  }

  /**
   * Processes events for this scrollbar.  It does this by calling
   * <code>processAdjustmentEvent()</code> if the event is an instance of
   * <code>AdjustmentEvent</code>, otherwise it calls the superclass to
   * process the event.
   *
   * @param event The event to process.
   */
  protected void processEvent(AWTEvent event)
  {
    if (event instanceof AdjustmentEvent)
      processAdjustmentEvent((AdjustmentEvent) event);
    else
      super.processEvent(event);
  }

  /**
   * Processes adjustment events for this object by dispatching them to
   * any registered listeners.  Note that this method will only be called
   * if adjustment events are enabled.  This will happen automatically if
   * any listeners are registered.  Otherwise, it can be enabled by a
   * call to <code>enableEvents()</code>.
   *
   * @param event The event to process.
   */
  protected void processAdjustmentEvent(AdjustmentEvent event)
  {
    value = event.getValue();
    if (adjustment_listeners != null)
      adjustment_listeners.adjustmentValueChanged(event);
  }

  /**
   * Package private method to determine whether to call
   * processEvent() or not.  Will handle events from peer and update
   * the current value.
   */
  void dispatchEventImpl(AWTEvent e)
  {
    if (e.id <= AdjustmentEvent.ADJUSTMENT_LAST
        && e.id >= AdjustmentEvent.ADJUSTMENT_FIRST)
      {
        AdjustmentEvent ae = (AdjustmentEvent) e;
        boolean adjusting = ae.getValueIsAdjusting();
        if (adjusting)
          setValueIsAdjusting(true);
        try
          {
            setValue(((AdjustmentEvent) e).getValue());
            if (adjustment_listeners != null
                || (eventMask & AWTEvent.ADJUSTMENT_EVENT_MASK) != 0)
              processEvent(e);
          }
        finally
          {
            if (adjusting)
              setValueIsAdjusting(false);
          }
      }
    else
      super.dispatchEventImpl(e);
  }

  /**
   * Returns a debugging string for this object.
   *
   * @return A debugging string for this object.
   */
  protected String paramString()
  {
    return ("value=" + getValue() + ",visibleAmount=" + getVisibleAmount()
            + ",minimum=" + getMinimum() + ",maximum=" + getMaximum()
            + ",pageIncrement=" + pageIncrement + ",lineIncrement="
            + lineIncrement + ",orientation="
            + (orientation == HORIZONTAL ? "HORIZONTAL" : "VERTICAL")
            + super.paramString());
  }

  /**
   * Returns an array of all the objects currently registered as FooListeners
   * upon this <code>Scrollbar</code>. FooListeners are registered using the
   * addFooListener method.
   *
   * @exception ClassCastException If listenerType doesn't specify a class or
   * interface that implements java.util.EventListener.
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
  {
    if (listenerType == AdjustmentListener.class)
      return AWTEventMulticaster.getListeners(adjustment_listeners,
                                              listenerType);

    return super.getListeners(listenerType);
  }

  /**
   * Returns an array of all registered adjustment listeners.
   */
  public AdjustmentListener[] getAdjustmentListeners()
  {
    return (AdjustmentListener[]) getListeners(AdjustmentListener.class);
  }

  /**
   * Returns true if the value is in the process of changing.
   *
   * @since 1.4
   */
  public boolean getValueIsAdjusting()
  {
    return valueIsAdjusting;
  }

  /**
   * Sets the value of valueIsAdjusting.
   *
   * @since 1.4
   */
  public void setValueIsAdjusting(boolean valueIsAdjusting)
  {
    this.valueIsAdjusting = valueIsAdjusting;
  }

  /**
   * Generate a unique name for this scroll bar.
   *
   * @return A unique name for this scroll bar.
   */
  String generateName()
  {
    return "scrollbar" + getUniqueLong();
  }

  private static synchronized long getUniqueLong()
  {
    return next_scrollbar_number++;
  }

  /**
   * This class provides accessibility support for the
   * scrollbar.
   *
   * @author Jerry Quinn (jlquinn@optonline.net)
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  protected class AccessibleAWTScrollBar extends AccessibleAWTComponent
    implements AccessibleValue
  {
    /**
     * Serialization constant to match JDK 1.5
     */
    private static final long serialVersionUID = -344337268523697807L;

    /**
     * Returns the role of this accessible object.
     *
     * @return the instance of <code>AccessibleRole</code>,
     * which describes this object.
     *
     * @see javax.accessibility.AccessibleRole
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.SCROLL_BAR;
    }

    /**
     * Returns the state set of this accessible object.
     *
     * @return a set of <code>AccessibleState</code>s which
     * represent the current state of the accessible object.
     *
     * @see javax.accessibility.AccessibleState
     * @see javax.accessibility.AccessibleStateSet
     */
    public AccessibleStateSet getAccessibleStateSet()
    {
      AccessibleStateSet states = super.getAccessibleStateSet();
      if (getOrientation() == HORIZONTAL)
        states.add(AccessibleState.HORIZONTAL);
      else
        states.add(AccessibleState.VERTICAL);
      if (getValueIsAdjusting())
        states.add(AccessibleState.BUSY);
      return states;
    }

    /**
     * Returns an implementation of the <code>AccessibleValue</code>
     * interface for this accessible object.  In this case, the
     * current instance is simply returned (with a more appropriate
     * type), as it also implements the accessible value as well as
     * the context.
     *
     * @return the accessible value associated with this context.
     *
     * @see javax.accessibility.AccessibleValue
     */
    public AccessibleValue getAccessibleValue()
    {
      return this;
    }

    /**
     * Returns the current value of this accessible object.
     * In this case, this is the same as the value for
     * the scrollbar, wrapped in an <code>Integer</code>
     * object.
     *
     * @return the numeric value of this scrollbar.
     *
     * @see javax.accessibility.AccessibleValue#getCurrentAccessibleValue()
     */
    public Number getCurrentAccessibleValue()
    {
      return new Integer(getValue());
    }

    /**
     * Sets the current value of this accessible object
     * to that supplied.  In this case, the value of the
     * scrollbar is set, and this method always returns
     * true.
     *
     * @param number the new accessible value.
     *
     * @return true if the value was set.
     *
     * @see javax.accessibility.AccessibleValue#setCurrentAccessibleValue(java.lang.Number)
     */
    public boolean setCurrentAccessibleValue(Number number)
    {
      setValue(number.intValue());
      return true;
    }

    /**
     * Returns the minimum acceptable accessible value used
     * by this object.  In this case, this is the same as
     * the minimum value of the scrollbar, wrapped in an
     * object.
     *
     * @return the minimum value of this scrollbar.
     *
     * @see javax.accessibility.AccessibleValue#getMinimumAccessibleValue()
     */
    public Number getMinimumAccessibleValue()
    {
      return new Integer(getMinimum());
    }

    /**
     * Returns the maximum acceptable accessible value used
     * by this object.  In this case, this is the same as
     * the maximum value of the scrollbar, wrapped in an
     * object.
     *
     * @return the maximum value of this scrollbar.
     *
     * @see javax.accessibility.AccessibleValue#getMaximumAccessibleValue()
     */
    public Number getMaximumAccessibleValue()
    {
      return new Integer(getMaximum());
    }
  }

  /**
   * Gets the AccessibleContext associated with this <code>Scrollbar</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTScrollBar();

    return accessibleContext;
  }
}

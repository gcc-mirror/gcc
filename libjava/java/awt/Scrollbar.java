/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.awt.peer.ScrollbarPeer;
import java.awt.peer.ComponentPeer;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 12, 2000
 */

public class Scrollbar extends Component implements Adjustable
{
  public Scrollbar ()
  {
    this (VERTICAL, 0, 10, 0, 100);
  }

  public Scrollbar (int orientation)
  {
    this (orientation, 0, 10, 0, 100);
  }

  public Scrollbar (int orientation, int value, int visible,
		    int minimum, int maximum)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException ();

    if (maximum < minimum)
      maximum = minimum;
    if (value < minimum)
      value = minimum;
    if (value > maximum)
      value = maximum;

    this.orientation = orientation;
    this.value = value;
    this.visible = visible;
    this.minimum = minimum;
    this.maximum = maximum;
    this.unit = 1;

    this.block = 0; // FIXME
  }

  public void addNotify ()
  {
    if (peer == null)
      peer = getToolkit ().createScrollbar (this);
    super.addNotify ();
  }

  public int getOrientation ()
  {
    return orientation;
  }

  public void setOrientation (int orientation)
  {
    if (orientation != HORIZONTAL && orientation != VERTICAL)
      throw new IllegalArgumentException ();
    this.orientation = orientation;
  }

  public int getValue ()
  {
    return value;
  }

  public void setValue (int value)
  {
    setValues (value, visible, minimum, maximum);
  }

  public int getMinimum ()
  {
    return minimum;
  }

  public void setMinimum (int minimum)
  {
    setValues (value, visible, minimum, maximum);
  }

  public int getMaximum ()
  {
    return maximum;
  }

  public void setMaximum (int maximum)
  {
    setValues (value, visible, minimum, maximum);
  }

  public int getVisibleAmount ()
  {
    return visible;
  }

  public int getVisible ()
  {
    return visible;
  }

  public void setVisibleAmount (int visible)
  {
    setValues (value, visible, minimum, maximum);
  }

  public void setUnitIncrement (int v)
  {
    unit = v;
    if (peer != null)
      {
	ScrollbarPeer sp = (ScrollbarPeer) peer;
	sp.setLineIncrement (v);
      }
  }

  /** @deprecated */
  public void setLineIncrement (int v)
  {
    setUnitIncrement (v);
  }

  public int getUnitIncrement ()
  {
    return unit;
  }

  public int getLineIncrement ()
  {
    return unit;
  }

  public void setBlockIncrement (int v)
  {
    block = v;
    if (peer != null)
      {
	ScrollbarPeer sp = (ScrollbarPeer) peer;
	sp.setPageIncrement (v);
      }
  }

  public void setPageIncrement (int v)
  {
    setBlockIncrement (v);
  }

  public int getBlockIncrement ()
  {
    return block;
  }

  public int getPageIncrement ()
  {
    return block;
  }

  public synchronized void setValues (int value, int visible,
				      int minimum, int maximum)
  {
    if (maximum < minimum)
      maximum = minimum;
    if (value < minimum)
      value = minimum;
    if (value > maximum)
      value = maximum;

    this.value = value;
    this.visible = visible;
    this.minimum = minimum;
    this.maximum = maximum;

    if (peer != null)
      {
	ScrollbarPeer sp = (ScrollbarPeer) peer;
	sp.setValues (value, visible, minimum, maximum);
      }
  }

  public void addAdjustmentListener (AdjustmentListener l)
  {
    listeners = AWTEventMulticaster.add (listeners, l);
  }

  public void removeAdjustmentListener (AdjustmentListener l)
  {
    listeners = AWTEventMulticaster.remove (listeners, l);
  }

  protected void processEvent (AWTEvent e)
  {
    if (e instanceof AdjustmentEvent)
      processAdjustmentEvent ((AdjustmentEvent) e);
    else
      super.processEvent (e);
  }

  protected void processAdjustmentEvent (AdjustmentEvent e)
  {
    if (listeners != null)
      listeners.adjustmentValueChanged (e);
  }

  protected String paramString ()
  {
    return ("Scrollbar["
	    + ((orientation == VERTICAL) ? "VERTICAL" : "HORIZONTAL") + ","
	    + value + ","
	    + visible + ","
	    + minimum + ","
	    + maximum + ","
	    + unit + ","
	    + block + "]");
  }

  private AdjustmentListener listeners;
  private int orientation;
  private int value;
  private int visible;
  private int minimum;
  private int maximum;
  private int unit;
  private int block;
}

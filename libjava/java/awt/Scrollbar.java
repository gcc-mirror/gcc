/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;
import java.util.Vector;
import java.util.Enumeration;

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
    this.listeners = new Vector ();

    this.block = 0; // FIXME
  }

  public void addNotify ()
  {
    // FIXME
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
    if (value < minimum)
      value = minimum;
    else if (value > maximum)
      value = maximum;
    this.value = value;
  }

  public int getMinimum ()
  {
    return minimum;
  }

  public void setMinimum (int minimum)
  {
    // FIXME: what if it is > max?
    this.minimum = minimum;
  }

  public int getMaximum ()
  {
    return maximum;
  }

  public void setMaximum (int maximum)
  {
    // FIXME: what if it is < min?
    this.maximum = maximum;
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
    this.visible = visible;
  }

  public void setUnitIncrement (int v)
  {
    unit = v;
  }

  public void setLineIncrement (int v)
  {
    unit = v;
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
  }

  public void setPageIncrement (int v)
  {
    block = v;
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
    // fixme;
  }

  public void addAdjustmentListener (AdjustmentListener l)
  {
    if (l != null)
      {
	listeners.addElement (l);
	enableEvents (0);	// FIXME
      }
  }

  public void removeAdjustmentListener (AdjustmentListener l)
  {
    if (l != null)
      listeners.remove (l);
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
    Enumeration en = listeners.elements ();
    while (en.hasMoreElements ())
      {
	AdjustmentListener l = (AdjustmentListener) en.nextElement ();
	l.adjustmentValueChanged (e);
      }
  }

  protected String paramString ()
  {
    return null;		// FIXME
  }

  private Vector listeners;
  private int orientation;
  private int value;
  private int visible;
  private int minimum;
  private int maximum;
  private int unit;
  private int block;
}

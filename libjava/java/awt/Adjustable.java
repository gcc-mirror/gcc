/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.event.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed complete and correct to JDK 1.2.  */

public interface Adjustable
{
  static final int HORIZONTAL;
  static final int VERTICAL;

  public void addAdjustmentListener (AdjustmentListener l);
  public int getBlockIncrement ();
  public int getMaximum ();
  public int getMinimum ();
  public int getOrientation ();
  public int getUnitIncrement ();
  public int getValue ();
  public int getVisibleAmount ();
  public void removeAdjustmentListener (AdjustmentListener l);
  public void setBlockIncrement (int b);
  public void setMaximum (int max);
  public void setMinimum (int min);
  public void setUnitIncrement (int u);
  public void setValue (int v);
  public void setVisibleAmount (int v);
}

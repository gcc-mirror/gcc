/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 8, 2000
 */

/* Status: Believed complete and correct to JDK 1.2.  */

public class AdjustmentEvent extends AWTEvent
{
  public static final int ADJUSTMENT_FIRST = 601;
  public static final int ADJUSTMENT_LAST = 601;
  public static final int ADJUSTMENT_VALUE_CHANGED = 601;
  public static final int BLOCK_DECREMENT = 3;
  public static final int BLOCK_INCREMENT = 4;
  public static final int TRACK = 5;
  public static final int UNIT_DECREMENT = 2;
  public static final int UNIT_INCREMENT = 1;

  public AdjustmentEvent (Adjustable source, int id, int type, int value)
  {
    super (source, id);
    this.type = type;
    this.value = value;
  }

  public Adjustable getAdjustable ()
  {
    return (Adjustable) source;
  }

  public int getAdjustmentType ()
  {
    return type;
  }

  public int getValue ()
  {
    return value;
  }

  public String paramString ()
  {
    return ("AdjustmentEvent[" + type + "," + value
	    + ";" + super.paramString () + "]");
  }

  private int type;
  private int value;
}

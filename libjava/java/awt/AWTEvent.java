/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt;

/* Written using on-line Java 2 Platform Standard Edition v1.3 API 
 * Specification, as well as "The Java Class Libraries", 2nd edition 
 * (Addison-Wesley, 1998).
 * Status:  Believed complete and correct, except for the java.awt.Event 
 * compatibility constructor.
 */

/**
 * AWTEvent is the root event class for all AWT events in the JDK 1.1 event 
 * model. It supersedes the Event class from JDK 1.0.
 */

public abstract class AWTEvent extends java.util.EventObject
{
  protected boolean consumed;
  protected int id;

  /* Event selection masks */
  public static final long COMPONENT_EVENT_MASK    = 1 << 0,
			   CONTAINER_EVENT_MASK    = 1 << 1,
			   FOCUS_EVENT_MASK        = 1 << 2,
			   KEY_EVENT_MASK          = 1 << 3,
			   MOUSE_EVENT_MASK        = 1 << 4, 
			   MOUSE_MOTION_EVENT_MASK = 1 << 5,
			   WINDOW_EVENT_MASK       = 1 << 6,
			   ACTION_EVENT_MASK       = 1 << 7,
			   ADJUSTMENT_EVENT_MASK   = 1 << 8,
			   ITEM_EVENT_MASK         = 1 << 9,
			   TEXT_EVENT_MASK         = 1 << 10,
			   INPUT_METHOD_EVENT_MASK = 1 << 11;

  /* Additional event selection masks from JDK 1.3 javadocs */
  public static final long PAINT_EVENT_MASK            = 1 << 13,
			   INVOCATION_EVENT_MASK       = 1 << 14,
			   HIERARCHY_EVENT_MASK        = 1 << 15,
			   HIERARCHY_BOUNDS_EVENT_MASK = 1 << 16;

  public static final int RESERVED_ID_MAX = 0x7cf;

  public AWTEvent(Event event)
  {
    // FIXME??
    super(event.target);
    this.id = event.id;
  }
  
  public AWTEvent(Object source, int id)
  {
    super(source);
    this.id = id;
  }

  public int getID()
  {
    return id;
  }

  public String paramString ()
  {
    return "";
  }

  public String toString ()
  {
    return getClass().getName() + "[" + paramString() + "] on " + source;
  }
  
  protected void consume()
  {
    consumed = true;
  }
  
  protected boolean isConsumed()
  {
    return consumed;
  }
}

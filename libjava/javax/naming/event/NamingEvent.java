/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
package javax.naming.event;
import javax.naming.*;
import java.util.EventObject;
 
/**
 * @author Warren Levy <warrenl@redhat.com>
 * @date June 5, 2001
 */

public class NamingEvent extends EventObject
{
  public static final int OBJECT_ADDED = 0;
  public static final int OBJECT_REMOVED = 1;
  public static final int OBJECT_RENAMED = 2;
  public static final int OBJECT_CHANGED = 3;

  // Serialized fields.
  protected Object changeInfo;
  protected int type;
  protected Binding oldBinding;
  protected Binding newBinding;

  public NamingEvent(EventContext source, int type, Binding newBd,
  		     Binding oldBd, Object changeInfo)
  {
    super(source);
    this.type = type;
    this.oldBinding = oldBd;
    this.newBinding = newBd;
    this.changeInfo = changeInfo;
    // FIXME: for OBJECT_ADDED, newBd must not be null;
    // FIXME: for OBJECT_CHANGED, newBd and oldBd must not be null;
    // FIXME: for OBJECT_RENAMED, one of newBd or oldBd may be null if newBd or
    // FIXME: oldBd is outside of the scope for which listener has registered.
    // FIXME: namingExceptionThrown() is called for the listener in question.
  }

  public int getType()
  {
    return type;
  }

  public EventContext getEventContext()
  {
    return (EventContext) getSource();
  }

  public Binding getOldBinding()
  {
    return oldBinding;
  }

  public Binding getNewBinding()
  {
    return newBinding;
  }

  public Object getChangeInfo()
  {
    return changeInfo;
  }

  public void dispatch(NamingListener listener)
  {
    switch (type)
      {
        case OBJECT_ADDED:
	  ((NamespaceChangeListener) listener).objectAdded(this);
	  break;
        case OBJECT_REMOVED:
	  ((NamespaceChangeListener) listener).objectRemoved(this);
	  break;
        case OBJECT_RENAMED:
	  ((NamespaceChangeListener) listener).objectRenamed(this);
	  break;
        case OBJECT_CHANGED:
	  ((ObjectChangeListener) listener).objectChanged(this);
	  break;
      }
  }
}

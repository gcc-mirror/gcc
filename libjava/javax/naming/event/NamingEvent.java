/* NamingEvent.java --
   Copyright (C) 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.naming.event;

import java.util.EventObject;
import javax.naming.Binding;
 
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

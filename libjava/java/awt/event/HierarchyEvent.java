/* Copyright (C) 2000, 2002  Free Software Foundation

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.event;
import java.awt.*;

/**
 * @since 1.3
 * @author Bryce McKinlay
 */

/* Status: thought to be complete and correct.  */

public class HierarchyEvent extends AWTEvent
{
  public static final int PARENT_CHANGED         = 1 << 0,
			  DISPLAYABILITY_CHANGED = 1 << 1,
			  SHOWING_CHANGED        = 1 << 2,
			  HIERARCHY_FIRST        = 1400,
			  HIERARCHY_CHANGED      = 1400,
			  ANCESTOR_MOVED         = 1401,
			  ANCESTOR_RESIZED       = 1402,
			  HIERARCHY_LAST         = 1402;
  
  /* Serialized fields from the serialization spec. */
  Component changed;
  Container changedParent;
  long changeFlags = 0;
  
  public HierarchyEvent(Component source, int id, Component changed,
                	Container changedParent)
  {
    super(source, id);
    this.changed = changed;
    this.changedParent = changedParent;
  }
  
  public HierarchyEvent(Component source, int id, Component changed, 
                        Container changedParent, long changeFlags)
  {
    super(source,id);
    this.changed = changed;
    this.changedParent = changedParent;
    this.changeFlags = changeFlags;
  }
  
  public Component getComponent()
  {
    return (Component) source;
  }
  
  public Component getChanged()
  {
    return changed;
  }
  
  public Container getChangedParent()
  {
    return changedParent;
  }
    
  public long getChangeFlags()
  {
    return changeFlags;
  }
  
  public String paramString()
  {
    String r;
    switch (id)
      {
      	case HIERARCHY_CHANGED:
	  r = "HIERARCHY_CHANGED";
	  break;

	case ANCESTOR_MOVED:   
	  r = "ANCESTOR_MOVED";
	  break;
	  
	case ANCESTOR_RESIZED:
	  r = "ANCESTOR_RESIZED";
	  break;
	  	
	default:
	  return "unknown type";
      }
    
    r += "(" + changed + "," + changedParent + ")";
    return r;
  }
}

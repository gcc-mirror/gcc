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

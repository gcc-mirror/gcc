/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

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

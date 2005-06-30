/* TabSet.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

package javax.swing.text;

import java.io.Serializable;

public class TabStop implements Serializable
{
  public static final int ALIGN_LEFT = 0;
  public static final int ALIGN_RIGHT = 1;
  public static final int ALIGN_CENTER = 2;
  public static final int ALIGN_DECIMAL = 4;
  public static final int ALIGN_BAR = 5;

  public static final int LEAD_NONE = 0;
  public static final int LEAD_DOTS = 1;
  public static final int LEAD_HYPHENS = 2;
  public static final int LEAD_UNDERLINE = 3;
  public static final int LEAD_THICKLINE = 4;
  public static final int LEAD_EQUALS = 5;

  float pos;
  int align;
  int leader;

  public TabStop(float pos) 
  {
    this(pos, ALIGN_LEFT, LEAD_NONE);
  }
  
  public TabStop(float pos, int align, int leader)
  {
    this.pos = pos;
    this.align = align;
    this.leader = leader;
  }
    
  public boolean equals(Object other) 
  {
    return (other != null)
      && (other instanceof TabStop)
      && (((TabStop)other).getPosition() == this.getPosition())
      && (((TabStop)other).getLeader() == this.getLeader())
      && (((TabStop)other).getAlignment() == this.getAlignment());
  }

  public int getAlignment() 
  {
    return align;
  }

  public int getLeader() 
  {
    return leader;
  }

  public float getPosition() 
  {
    return pos;
  }

  public int hashCode() 
  {
    return (int) pos + (int) leader + (int) align;
  }

  public String toString() 
  {
    String prefix = "";
    switch (align)
      {
      case ALIGN_LEFT:
        prefix = "left ";
        break;
      case ALIGN_RIGHT:
        prefix = "right ";
        break;

      case ALIGN_CENTER:
        prefix = "center ";
        break;

      case ALIGN_DECIMAL:
        prefix = "decimal ";
        break;
        
      case ALIGN_BAR:
        prefix = "bar ";
        break;

      default:
        break;
      }
    
    return (prefix + "tab @" + pos + ((leader == LEAD_NONE) ? "" : "(w/leaders)"));
  }

}

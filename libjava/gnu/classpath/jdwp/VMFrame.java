/* VMFrame.java -- Reference implementation of VM hooks for JDWP Frame access.
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp;

import gnu.classpath.jdwp.util.Location;

/**
 * Reference implementation of VM hooks for JDWP Frame access.
 * 
 * @author aluchko 
 */

public class VMFrame
{
  // The object this frame resides in
  private Object obj;
  
  // The current location of this frame
  private Location loc;
  
  // id of this frame
  private long id;
  
  /**
   * Gets the current location of the frame.
   */
  public Location getLocation()
  {
    return loc;
  }

  /**
   * Returns the value of the variable in the given slot.
   * 
   * @param slot the slot containing the variable
   */
  public Object getValue(int slot) { return null; }

  /**
   * Assigns the given variable to the given value. 
   * @param slot The slot which contains the variable
   * @param value The value to assign the variable to
   */
  public void setValue(int slot, Object value) { }

  /**
   * Get the object which is represented by 'this' in the context of the frame,
   * returns null if the method is native or static.
   */
  public Object getObject()
  {
    return obj;
  }

  /**
   * Get the frameID
   * @return an id which is unique within the scope of the VM
   */
  public long getId()
  {
    return id;
  }

}

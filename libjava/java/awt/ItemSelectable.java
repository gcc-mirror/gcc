/* ItemSelectable.java -- Items that can be selected
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.ItemListener;

/**
  * This interface is for objects that can have one or more items
  * selected.  For example, radio buttons.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public interface ItemSelectable
{

/**
  * Returns the list of objects that are selected in this component.
  *
  * @return The list of objects that are selected, or <code>null</code> if
  * no objects are selected.
  */
public abstract Object[]
getSelectedObjects();

/*************************************************************************/

/**
  * Adds an item listener to this object.  It will receive
  * selection events for this object.
  *
  * @param listener The item listener to add.
  */
public abstract void
addItemListener(ItemListener listener);

/*************************************************************************/

/**
  * Removes an item listener from this object.  It will no longer receive
  * selection change events.
  *
  * @param listener The item listener to remove.
  */
public abstract void
removeItemListener(ItemListener listener);

} // interface ItemSelectable


/* java.beans.PropertyChangeListener
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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


package java.beans;

import java.util.EventListener;

/**
 ** PropertyChangeListener allows a class to monitor
 ** properties of a Bean for changes.<P>
 **
 ** A propertyChange() event will only be fired
 ** <EM>after</EM> the property has changed.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 29 Jul 1998
 ** @see java.beans.PropertyChangeSupport
 **/

public interface PropertyChangeListener extends EventListener
{
  /** Fired after a Bean's property has changed.
   ** @param e the change (containing the old and new values)
   **/
  public abstract void propertyChange(PropertyChangeEvent e);
}

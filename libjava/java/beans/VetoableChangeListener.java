/* java.beans.VetoableChangeListener
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


package java.beans;

import java.util.EventListener;

/**
 ** VetoableChangeListener allows a class to monitor
 ** proposed changes to properties of a Bean and, if
 ** desired, prevent them from occurring.<P>
 **
 ** A vetoableChange() event will be fired <EM>before</EM>
 ** the property has changed.  If any listener rejects the
 ** change by throwing the PropertyChangeException, a new
 ** vetoableChange() event will be fired to all listeners
 ** who received a vetoableChange() event in the first
 ** place informing them of a reversion to the old value.
 ** The value, of course, never actually changed.<P>
 **
 ** <STRONG>Note:</STRONG> This class may not be reliably
 ** used to determine whether a property has actually
 ** changed.  Use the PropertyChangeListener interface
 ** for that instead.
 **
 ** @author John Keiser
 ** @version 1.1.0, 29 Jul 1998
 ** @since JDK1.1
 ** @see java.beans.PropertyChangeListener
 ** @see java.beans.VetoableChangeSupport
 **/

public interface VetoableChangeListener extends EventListener
{
  /** Fired before a Bean's property changes.
   ** @param e the change (containing the old and new values)
   ** @exception PropertyChangeException if the listener
   **            does not desire the change to be made.
   **/
  public abstract void vetoableChange(PropertyChangeEvent e)
    throws PropertyVetoException;
}

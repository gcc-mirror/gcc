/* java.beans.PropertyVetoException
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

/**
 ** PropertyVetoException is thrown when a VetoableChangeListener doesn't like the proposed change.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 31 May 1998
 ** @see java.beans.VetoableChangeListener
 **/

public class PropertyVetoException extends Exception {
	PropertyChangeEvent evt;

	private static final long serialVersionUID = 129596057694162164L;

	/** Instantiate this exception with the given message and property change.
	 ** @param msg the reason for the veto.
	 ** @param changeEvent the PropertyChangeEvent that was thrown.
	 **/
	public PropertyVetoException(String msg, PropertyChangeEvent changeEvent) {
		super(msg);
		evt = changeEvent;
	}

	/** Get the PropertyChange event that was vetoed. **/
	public PropertyChangeEvent getPropertyChangeEvent() {
		return evt;
	}
}

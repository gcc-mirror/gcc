/* java.beans.Customizer
   Copyright (C) 1998 Free Software Foundation, Inc.

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
 ** You may explicitly provide a Customizer for your Bean
 ** class, which allows you complete control of the editing
 ** of the Bean.<P>
 **
 ** A Customizer is meant to be embedded in an RAD tool,
 ** and thus must be a descendant of <CODE>java.awt.Component</CODE>.<P>
 **
 ** It must also have a constructor with no arguments.  This
 ** is the constructor that will be called by the RAD tool to
 ** instantiate the Customizer.<P>
 **
 ** Over its lifetime, an instance of a Customizer will only
 ** customize one single Bean.  A new instance of the
 ** Customizer will be instantiated to edit any other Beans.<P>
 **
 ** The Customizer is responsible for notifying its
 ** PropertyChangeListeners of any changes that are made,
 ** according to the rules of PropertyChangeListeners (i.e.
 ** notify the clients <EM>after</EM> the property has
 ** changed).
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 29 Jul 1998
 ** @see java.beans.BeanDescriptor.getCustomizerClass()
 **/

public interface Customizer {
	/** Set the object to Customize.  This will always be a
	 ** Bean that had a BeanDescriptor indicating this
	 ** Customizer.
	 ** @param bean the Bean to customize.
	 **/
	public abstract void setObject(Object bean);

	/** Add a PropertyChangeListener.
	 ** @param l the PropertyChangeListener to add.
	 **/
	public abstract void addPropertyChangeListener(PropertyChangeListener l);

	/** Remove a PropertyChangeListener.
	 ** @param l the PropertyChangeListener to remove.
	 **/
	public abstract void removePropertyChangeListener(PropertyChangeListener l);
}

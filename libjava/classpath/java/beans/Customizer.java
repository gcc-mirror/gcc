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
	void setObject(Object bean);

	/** Add a PropertyChangeListener.
	 ** @param l the PropertyChangeListener to add.
	 **/
	void addPropertyChangeListener(PropertyChangeListener l);

	/** Remove a PropertyChangeListener.
	 ** @param l the PropertyChangeListener to remove.
	 **/
	void removePropertyChangeListener(PropertyChangeListener l);
}

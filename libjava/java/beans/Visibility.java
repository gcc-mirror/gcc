/* java.beans.Visibility
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/**
 * Visibility is an interface a Bean may implement so that the environment
 * can tell the Bean whether there is a GUI or not, and so that the Bean
 * can tell the environment whether it needs one or can run without one.
 * <P>
 *
 * Sun decided not to use standard Introspection patterns so that these
 * methods did not get included when the Introspector made its sweep on
 * the class.
 *
 * @author John Keiser
 * @since JDK1.1
 * @version 1.1.0, 29 Jul 1998
 */

public interface Visibility {
	/**
	 * Tells whether the Bean can run without a GUI or not.
	 * @return false if Bean can run without a GUI, else true.
	 */
	boolean needsGui();

	/**
	 * Tells whether Bean is trying not to use the GUI.
	 * If needsGui() is true, this method should always return false.
	 * @return true if definitely not using GUI, otherwise false.
	 */
	boolean avoidingGui();

	/**
	 * Tells the Bean not to use GUI methods.
	 * If needsGUI() is false, then after this method is called,
	 * avoidingGui() should return true.
	 */
	void dontUseGui();

	/**
	 * Tells the Bean it may use the GUI.
	 * The Bean is not required to use the GUI in this case, it is
	 * merely being <EM>permitted</EM> to use it.  If needsGui() is
	 * false, avoidingGui() may return true or false after this method
	 * is called.
	 */
	void okToUseGui();
}

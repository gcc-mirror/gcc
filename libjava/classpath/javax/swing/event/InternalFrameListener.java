/* InternalFrameListener.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

package javax.swing.event;

// Imports
import java.util.EventListener;

/**
 * InternalFrameListener public interface
 * @author Andrew Selkirk
 */
public interface InternalFrameListener extends EventListener {

	/**
	 * Internal frame activated
	 * @param event Internal Frame Event
	 */
	void internalFrameActivated(InternalFrameEvent event);

	/**
	 * Internal frame closed
	 * @param event Internal Frame Event
	 */
	void internalFrameClosed(InternalFrameEvent event);

	/**
	 * Internal frame closing
	 * @param event Internal Frame Event
	 */
	void internalFrameClosing(InternalFrameEvent event);

	/**
	 * Internal frame deactivated
	 * @param event Internal Frame Event
	 */
	void internalFrameDeactivated(InternalFrameEvent event);

	/**
	 * Internal frame deiconified
	 * @param event Internal Frame Event
	 */
	void internalFrameDeiconified(InternalFrameEvent event);

	/**
	 * Internal frame iconified
	 * @param event Internal Frame Event
	 */
	void internalFrameIconified(InternalFrameEvent event);

	/**
	 * Internal frame opened
	 * @param event Internal Frame Event
	 */
	void internalFrameOpened(InternalFrameEvent event);


} // InternalFrameListener

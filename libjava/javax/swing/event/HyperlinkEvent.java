/* HyperlinkEvent.java --
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

package javax.swing.event;

// Imports
import java.net.*;
import java.util.*;

/**
 * HyperlinkEvent
 * @author Andrew Selkirk
 * @author Ronald Veldema
 */
public class HyperlinkEvent extends EventObject {

	//-------------------------------------------------------------
	// Classes ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * EventType
	 */
	public static final class EventType {

		//-------------------------------------------------------------
		// Variables --------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * ENTERED
		 */
		public static final EventType ENTERED = new EventType("ENTERED"); // TODO

		/**
		 * EXITED
		 */
		public static final EventType EXITED = new EventType("EXITED"); // TODO

		/**
		 * ACTIVATED
		 */
		public static final EventType ACTIVATED = new EventType("ACTIVATED"); // TODO

		/**
		 * type
		 */
		private String type;


		//-------------------------------------------------------------
		// Initialization ---------------------------------------------
		//-------------------------------------------------------------

		/**
		 * Constructor EventType
		 * @param type TODO
		 */
		private EventType(String type) {
			this.type = type;
		} // EventType()


		//-------------------------------------------------------------
		// Methods ----------------------------------------------------
		//-------------------------------------------------------------

		/**
		 * toString
		 * @returns String
		 */
		public String toString() {
			return type; // TODO
		} // toString()


	} // EventType


	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * type
	 */
	private EventType type;

	/**
	 * url
	 */
	private URL url;

	/**
	 * description
	 */
	private String description;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor HyperlinkEvent
	 * @param source TODO
	 * @param type TODO
	 * @param url TODO
	 */
	public HyperlinkEvent(Object source, EventType type, URL url) {
		super(source);
		this.type = type;
		this.url = url;
		this.description = null;
	} // HyperlinkEvent()

	/**
	 * Constructor HyperlinkEvent
	 * @param source TODO
	 * @param type TODO
	 * @param url TODO
	 * @param description TODO
	 */
	public HyperlinkEvent(Object source, EventType type, URL url, String description) {
		super(source);
		this.type = type;
		this.url = url;
		this.description = null;
	} // HyperlinkEvent()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getURL
	 * @returns URL
	 */
	public URL getURL() {
		return url;
	} // getURL()

	/**
	 * getEventType
	 * @returns EventType
	 */
	public EventType getEventType() {
		return type;
	} // getEventType()

	/**
	 * getDescription
	 * @returns String
	 */
	public String getDescription() {
		return description;
	} // getDescription()


} // HyperlinkEvent

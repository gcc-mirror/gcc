/* java.beans.FeatureDescriptor
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

import java.util.*;

/**
 ** FeatureDescriptor is the common superclass for all JavaBeans Descriptor classes.
 ** JavaBeans descriptors are abstract descriptors of properties,
 ** events, methods, beans, etc.<P>
 **
 ** <STRONG>Documentation Convention:</STRONG> for proper
 ** Internalization of Beans inside an RAD tool, sometimes there
 ** are two names for a property or method: a programmatic, or
 ** locale-independent name, which can be used anywhere, and a
 ** localized, display name, for ease of use.  In the
 ** documentation I will specify different String values as
 ** either <EM>programmatic</EM> or <EM>localized</EM> to
 ** make this distinction clear.
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 31 May 1998
 **/

public class FeatureDescriptor {
	String name;
	String displayName;
	String shortDescription;
	boolean expert;
	boolean hidden;

	Hashtable valueHash;

	/** Instantiate this FeatureDescriptor with appropriate default values.**/
	public FeatureDescriptor() {
		valueHash = new Hashtable();
	}

	/** Get the programmatic name of this feature. **/
	public String getName() {
		return name;
	}

	/** Set the programmatic name of this feature.
	 ** @param name the new name for this feature.
	 **/
	public void setName(String name) {
		this.name = name;
	}

	/** Get the localized (display) name of this feature. **/
	public String getDisplayName() {
		return displayName;
	}

	/** Set the localized (display) name of this feature.
	 ** @param displayName the new display name for this feature.
	 **/
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/** Get the localized short description for this feature. **/
	public String getShortDescription() {
		return shortDescription;
	}

	/** Set the localized short description for this feature.
	 ** @param shortDescription the new short description for this feature.
	 **/
	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}

	/** Indicates whether this feature is for expert use only.
	 ** @return true if for use by experts only, or false if anyone can use it.
	 **/
	public boolean isExpert() {
		return expert;
	}

	/** Set whether this feature is for expert use only.
	 ** @param expert true if for use by experts only, or false if anyone can use it.
	 **/
	public void setExpert(boolean expert) {
		this.expert = expert;
	}

	/** Indicates whether this feature is for use by tools only.
	 ** If it is for use by tools only, then it should not be displayed.
	 ** @return true if tools only should use it, or false if anyone can see it.
	 **/
	public boolean isHidden() {
		return hidden;
	}

	/** Set whether this feature is for use by tools only.
	 ** If it is for use by tools only, then it should not be displayed.
	 ** @param hidden true if tools only should use it, or false if anyone can see it.
	 **/
	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}


	/** Get an arbitrary value set with setValue().
	 ** @param name the programmatic name of the key.
	 ** @return the value associated with this name, or null if there is none.
	 **/
	public Object getValue(String name) {
		return valueHash.get(name);
	}

	/** Set an arbitrary string-value pair with this feature.
	 ** @param name the programmatic name of the key.
	 ** @param value the value to associate with the name.
	 **/
	public void setValue(String name, Object value) {
		valueHash.put(name, value);
	}

	/** Get a list of the programmatic key names set with setValue().
	 ** @return an Enumerator over all the programmatic key names associated
	 ** with this feature.
	 **/
	public Enumeration attributeNames() {
		return valueHash.keys();
	}
}

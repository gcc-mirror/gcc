/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

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

package java.rmi.activation;

import java.io.Serializable;
import java.rmi.MarshalledObject;

public final class ActivationDesc implements Serializable
{
  static final long serialVersionUID = 7455834104417690957L;

private ActivationGroupID groupid;
private String classname;
private String location;
private MarshalledObject data;
private boolean restart;

public ActivationDesc(String className, String location, MarshalledObject data) throws ActivationException {
	this(ActivationGroup.currentGroupID(), className, location, data, false);
}

public ActivationDesc(String className, String location, MarshalledObject data, boolean restart) throws ActivationException {
	this(ActivationGroup.currentGroupID(), className, location, data, restart);
}

public ActivationDesc(ActivationGroupID groupID, String className, String location, MarshalledObject data) {
	this(groupID, className, location, data, false);
}

public ActivationDesc(ActivationGroupID groupID, String className, String location, MarshalledObject data, boolean restart) {
	this.groupid = groupID;
	this.classname = className;
	this.location = location;
	this.data = data;
	this.restart = restart;
}

public ActivationGroupID getGroupID() {
	return (groupid);
}

public String getClassName() {
	return (classname);
}

public String getLocation() {
	return (location);
}

public MarshalledObject getData() {
	return (data);
}

public boolean getRestartMode() {
	return (restart);
}

public boolean equals(Object obj) {
	if (!(obj instanceof ActivationDesc)) {
		return (false);
	}
	ActivationDesc that = (ActivationDesc)obj;

	if (this.groupid.equals(that.groupid) &&
	    this.classname.equals(that.classname) &&
	    this.location.equals(that.location) &&
	    this.data.equals(that.data) &&
	    this.restart == that.restart) {
		return (true);
	}
	return (false);
}

public int hashCode() {
	return (groupid.hashCode() ^ classname.hashCode() ^ location.hashCode() ^ data.hashCode());
}

}

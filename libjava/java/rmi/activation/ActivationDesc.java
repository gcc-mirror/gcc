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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package java.rmi.activation;

import java.io.Serializable;
import java.rmi.MarshalledObject;

public final class ActivationDesc
	implements Serializable {

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

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

package java.rmi.server;

import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.NoSuchObjectException;
import java.rmi.UnmarshalException;
import java.rmi.server.RemoteRef;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.lang.ClassNotFoundException;
import java.lang.InstantiationException;
import java.lang.IllegalAccessException;

public abstract class RemoteObject
	implements Remote, Serializable {

public static final long serialVersionUID = -3215090123894869218l;

protected transient RemoteRef ref;

protected RemoteObject() {
	this(null);
}

protected RemoteObject(RemoteRef newref) {
	ref = newref;
}

public RemoteRef getRef() {
	return (ref);
}

public static Remote toStub(Remote obj) throws NoSuchObjectException {
	throw new Error("Not implemented");
}

public int hashCode() {
	if (ref == null) {
		return (0);
	}
	else {
		return (ref.hashCode());
	}
}

public boolean equals(Object obj) {
	// We only compare references.
	return (this == obj);
}

public String toString() {
	return (ref.toString());
}

private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
	String cname = in.readUTF();
	if (!cname.equals("")) {
		cname = RemoteRef.packagePrefix + '.' + cname;
		try {
			Class cls = Class.forName(cname);
			ref = (RemoteRef)cls.newInstance();
		}
		catch (InstantiationException e1) {
			throw new UnmarshalException("failed to create ref");
		}
		catch (IllegalAccessException e2) {
			throw new UnmarshalException("failed to create ref");
		}
		ref.readExternal(in);
	}
	else {
		ref = (RemoteRef)in.readObject();
	}
}

private void writeObject(ObjectOutputStream out) throws IOException, ClassNotFoundException {
	if (ref == null) {
		throw new UnmarshalException("no ref to serialize");
	}
	String cname = ref.getRefClass(out);
	if (cname != null && cname.length() > 0) {
		out.writeUTF(cname);
		ref.writeExternal(out);
	}
	else {
		out.writeUTF("");
		out.writeObject(ref);
	}
}

}

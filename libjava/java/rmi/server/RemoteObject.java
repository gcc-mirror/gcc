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
import java.lang.reflect.Constructor;

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

  public static Remote toStub(Remote obj) throws NoSuchObjectException 
  {
    Class cls = obj.getClass();
    String classname = cls.getName();
    ClassLoader cl = cls.getClassLoader();
    try 
      {
	Class scls = cl.loadClass(classname + "_Stub");
	// JDK 1.2 stubs
	Class[] stubprototype = new Class[] { RemoteRef.class };
	Constructor con = scls.getConstructor(stubprototype);
	return (Remote)(con.newInstance(new Object[]{obj}));
      }
    catch (Exception e) {}
    throw new NoSuchObjectException(obj.getClass().getName());
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

  public String toString() 
  {
    if (ref == null)
      return getClass ().toString ();
    return (ref.toString ());
  }
  
  private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException 
  {
    String cname = in.readUTF();
    if (!cname.equals("")) 
      {
	if (cname.equals ("UnicastRef2"))
	  { 
	    // hack for interoperating with JDK
	    cname = "UnicastRef";
	    in.read (); //some unknown UnicastRef2 field
	  }

	cname = RemoteRef.packagePrefix + '.' + cname;
	try 
	  {
	    Class cls = Class.forName(cname);
	    ref = (RemoteRef)cls.newInstance();
	  }
	catch (InstantiationException e1) 
	  {
	    throw new UnmarshalException("failed to create ref", e1);
	  }
	catch (IllegalAccessException e2) 
	  {
	    throw new UnmarshalException("failed to create ref", e2);
	  }
	ref.readExternal(in);
      }
    else 
      {
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

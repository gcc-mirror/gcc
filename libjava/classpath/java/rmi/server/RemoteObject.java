/* RemoteObject.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2004  Free Software Foundation, Inc.

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

package java.rmi.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.UnmarshalException;
import java.util.WeakHashMap;

public abstract class RemoteObject
	implements Remote, Serializable {

private static final long serialVersionUID = -3215090123894869218l;

protected transient RemoteRef ref;

private static final WeakHashMap stubs = new WeakHashMap();

protected RemoteObject() {
	this(null);
}

protected RemoteObject(RemoteRef newref) {
	ref = newref;
}

public RemoteRef getRef() {
	return (ref);
}

synchronized static void addStub(Remote obj, Remote stub)
{
  stubs.put(obj, stub);
}

synchronized static void deleteStub(Remote obj)
{
  stubs.remove(obj);
}

  public static Remote toStub(Remote obj) throws NoSuchObjectException 
  {
    Remote stub = (Remote)stubs.get(obj);

    if (stub == null)
      throw new NoSuchObjectException(obj.getClass().getName());

    return stub;
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

/**
 * Get the string representation of this remote object.
 */
  public String toString() 
  {
    if (ref == null)
      return getClass ().toString ();
    return (ref.toString ());
  }
  
  /**
   * Read the remote object from the input stream. Expects the class name
   * without package first. Then the method creates and assigns the {@link #ref}
   * an instance of this class and calls its .readExternal method. The standard
   * packageless class names are UnicastRef, UnicastRef2, UnicastServerRef,
   * UnicastServerRef2, ActivatableRef or ActivatableServerRef.
   * 
   * @param in the stream to read from
   * @throws IOException if the IO exception occurs
   * @throws ClassNotFoundException if the class with the given name is not
   *           present in the package gnu.java.rmi.server (for the case of the
   *           GNU Classpath.
   */
  private void readObject(ObjectInputStream in) throws IOException,
      ClassNotFoundException
  {
    String cname = in.readUTF();
    if (! cname.equals(""))
      {
        if (cname.equals("UnicastRef2"))
          {
            // hack for interoperating with JDK
            cname = "UnicastRef";
            in.read(); // some unknown UnicastRef2 field
          }

        // It would be nice to use RemoteRef.packagePrefix here, but for binary
        // compatibility with the JDK that has to contain "sun.rmi.server"...
        cname = "gnu.java.rmi.server." + cname;
        try
          {
            Class cls = Class.forName(cname);
            ref = (RemoteRef) cls.newInstance();
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
        ref = (RemoteRef) in.readObject();
      }
  }

  /**
   * Write the remote object to the output stream. This method first calls
   * {@link RemoteRef#getRefClass(ObjectOutput)} on the {@link #ref} to get the
   * class name without package, writes this name and then calls the
   * ref.writeObject to write the data. The standard packageless class names are
   * UnicastRef, UnicastRef2, UnicastServerRef, UnicastServerRef2,
   * ActivatableRef or ActivatableServerRef. The empty string with the
   * subsequently following serialized ref instance be written if the
   * ref.getRefClass returns null.
   * 
   * @param out the stream to write to
   * @throws IOException if one occurs during writing
   * @throws ClassNotFoundException never in this implementation (specified as
   *           part of the API standard)
   * @throws UnmarshalException if the remote reference of this remote object is
   *           null.
   */
  private void writeObject(ObjectOutputStream out) throws IOException,
      ClassNotFoundException
  {
    if (ref == null)
      {
        throw new UnmarshalException("no ref to serialize");
      }
    String cname = ref.getRefClass(out);
    if (cname != null && cname.length() > 0)
      {
        out.writeUTF(cname);
        ref.writeExternal(out);
      }
    else
      {
        out.writeUTF("");
        out.writeObject(ref);
      }
  }

}

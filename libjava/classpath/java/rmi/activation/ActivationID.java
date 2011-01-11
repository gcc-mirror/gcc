/* ActivationID.java -- the object activation identifier
   Copyright (c) 1996, 1997, 1998, 1999, 2006 Free Software Foundation, Inc.

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


package java.rmi.activation;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UID;

/**
 * Denotes the object that can be activated over time. The instance of the
 * ActivationID for the given object can be obtained in the following ways:
 * <ul>
 * <li>via {@link Activatable#register(ActivationDesc)}</li>
 * <li>via Activatable constructor</li>
 * <li>via Activatable.exportObject
 * <li>
 * </ul>
 * An instance of the ActivationID has the {@link UID} as its component and
 * hence is globally unique.
 *
 * @author Audrius Meskauskas (audriusa@bioinformatics.org) (from stub)
 */
public class ActivationID
    implements Serializable
{
  /**
   * Use SVUID for interoperability.
   */
  static final long serialVersionUID = - 4608673054848209235L;

  /**
   * The activator.
   */
  transient Activator activator;

  /**
   * The UID, making this instance unique.
   */
  transient UID uid;

  /**
   * The activation group that has activated the object with this
   * activation id. The field is filled in inside the group and is used
   * to notify the group about the request to inactivated the object.
   */
  transient ActivationGroup group;

  /**
   * Create a new instance with the given activator.
   *
   * @param an_activator tha activator that should activate the object.
   */
  public ActivationID(Activator an_activator)
  {
    activator = an_activator;
    uid = new UID();
  }

  /**
   * Activate the object.
   *
   * @param force if true, always contact the group. Otherwise, the cached value
   *          may be returned.
   * @return the activated object
   * @throws UnknownObjectException if the object is unknown
   * @throws ActivationException if the activation has failed
   * @throws RemoteException if the remote call has failed
   */
  public Remote activate(boolean force) throws ActivationException,
      UnknownObjectException, RemoteException
  {
        try
          {
            return (Remote) activator.activate(this, force).get();
          }
        catch (IOException e)
          {
            ActivationException acex = new ActivationException("id "+uid, e);
            throw acex;
          }
        catch (ClassNotFoundException e)
          {
            ActivationException acex = new ActivationException("id "+uid, e);
            throw acex;
          }
  }

  /**
   * Returns the hash code of the activator.
   */
  public int hashCode()
  {
    return uid == null ? 0 : uid.hashCode();
  }

  /**
   * Compares the activators for equality.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ActivationID)
      {
        ActivationID that = (ActivationID) obj;
        return eq(uid, that.uid);
      }
    else
      return false;
  }

  /**
   * Read the object from the input stream.
   *
   * @param in the stream to read from
   *
   * @throws IOException if thrown by the stream
   * @throws ClassNotFoundException
   */
  private void readObject(ObjectInputStream in) throws IOException,
      ClassNotFoundException
  {
     uid = (UID) in.readObject();
     activator = (Activator) in.readObject();
  }

  /**
   * Write the object to the output stream.
   *
   * @param out the stream to write int
   * @throws IOException if thrown by the stream
   * @throws ClassNotFoundException
   */
  private void writeObject(ObjectOutputStream out) throws IOException,
      ClassNotFoundException
  {
    out.writeObject(uid);
    out.writeObject(activator);
  }

  /**
   * Compare by .equals if both a and b are not null, compare directly if at
   * least one of them is null.
   */
  static final boolean eq(Object a, Object b)
  {
    if (a == null || b == null)
      return a == b;
    else
      return a.equals(b);
  }

  /**
   * Return the content based string representation.
   */
  public String toString()
  {
    return uid.toString();
  }

}

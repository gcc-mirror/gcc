/* StubDelegate.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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


package javax.rmi.CORBA;

import org.omg.CORBA.ORB;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import java.rmi.RemoteException;

/**
 * A delegate, implementing the functionality, provided by the {@link Stub}.
 * The default delegate can be altered by setting the system property
 * "javax.rmi.CORBA.StubClass" to the name of the alternative class that must
 * implement StubDelegate.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface StubDelegate
{
  /**
   * <p>
   * Makes the stub ready for remote communication using the given ORB.
   * </p>
   * <p>
   * It is frequently easier to call {@link PortableRemoteObject#connect} rather
   * than this method.
   * </p>
   *
   * @param orb the ORB where the Stub must be connected.
   *
   * @throws RemoteException if the stub is already connected to some other ORB.
   * If the stub is already connected to the ORB that was passed as parameter,
   * the method returns without action.
   */
  void connect(Stub self, ORB orb)
    throws RemoteException;

  /**
   * The objects stubs are equal if they refer the same remote object.
   */
  boolean equals(Stub self, Object obj);

  /**
   * Get the hashcode fo this delegate.
   */
  int hashCode(Stub self);

  /**
   * Read this stub from the object input stream.
   */
  void readObject(Stub self, ObjectInputStream s)
    throws IOException, ClassNotFoundException;

  /**
   * Write this stub to the object output stream.
   */
  void writeObject(Stub self, ObjectOutputStream s)
    throws IOException;

  /**
   * Get the string representation of this stub.
   */
  String toString(Stub self);
}

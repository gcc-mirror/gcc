/* CurrentOperations.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package org.omg.PortableServer;

import org.omg.PortableServer.CurrentPackage.NoContext;

/**
 * Provides the Id of the object currently being served and POA
 * to that this object is connected. Both Id and POA can be much
 * simpler obtained from the servant by {@link Servant#_object_id() }
 * and {@link Servant#_poa()} that use the CurrentOperations indirectly.
 *
 * As long as the ORB reference is still available, the current information
 * is available via {@link Current} that is returned by
 * ORB.resolve_initial_references("POACurrent"). To support this call,
 * the ORB maintains the thread to invocation data map for all calls that
 * are currently being processed.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface CurrentOperations
  extends org.omg.CORBA.CurrentOperations
{
  /**
   * Returns the Id of the object currently being served. The returned
   * value is also correct if the calling code is running is several
   * paralled threads.
   *
   * @return the Id of the object that is currently being served by this
   * thread.
   */
  byte[] get_object_id()
                throws NoContext;

  /**
   * Returns POA to that the object currently being served is connected.
   * The returned value is also correct if the calling code is running is several
   * paralled threads.
   *
   * @return the Id of the object that is currently being served by this
   * thread.
   */
  POA get_POA()
       throws NoContext;
}
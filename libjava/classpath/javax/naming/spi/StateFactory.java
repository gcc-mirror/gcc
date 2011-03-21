/* StateFactory.java --
   Copyright (C) 2001, 2004, 2006  Free Software Foundation, Inc.

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


package javax.naming.spi;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NamingException;

/**
 * Represents a factory, producing the object states for binding. The operation,
 * performed by this factory, is the reverse operation with related to the
 * operation, performed by the {@link ObjectFactory}. Classes, implementing
 * this interface, must be public and have public parameterless constructor.
 *
 * @see DirStateFactory
 * @see ObjectFactory
 * @author Warren Levy (warrenl@redhat.com)
 */
public interface StateFactory
{
  /**
   * Get the object state for binding.
   *
   * @param obj the object, for that the binding state must be retrieved. Cannot
   *          be null.
   * @param name the name of this object, related to the nameCtx. Can be null if
   *          not specified.
   * @param nameCtx the naming context, to that the object name is related. Can
   *          be null if the name is related to the initial default context.
   * @param environment the properties for creating the object state. Can be
   *          null if no properties are provided.
   * @return the object state for binding, may be null if no changes are
   *         returned by the factory
   * @throws NamingException
   *
   * @see NamingManager#getStateToBind
   * @see DirectoryManager#getStateToBind
   */
  Object getStateToBind(Object obj, Name name, Context nameCtx,
                               Hashtable<?, ?> environment) throws NamingException;
}

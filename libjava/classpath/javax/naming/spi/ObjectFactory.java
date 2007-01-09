/* ObjectFactory.java --
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

/**
 * Represents a factory for creating the object. ObjectFactory performs the
 * operation, that is the opposite to the operation, performed by the
 * {@link StateFactory}. Classes, implementing this interface, must be public
 * and have public parameterless constructor.
 */
public interface ObjectFactory
{
  /**
   * Creates the object, using the specified name and location information. The
   * call of this method must be thread safe.
   * 
   * @param refObj may provide the reference and location information. Can be null.
   * @param name the name of the new object in the scope of the specified naming
   *          context. Can be null if the name is not specified.
   * @param nameCtx the context, in which the object name is specified. Can be
   *          null if the name is specified in the scope of the default initial
   *          context.
   * @param environment the properties, providing additional information on how
   *          to create an object. Can be null if not additional information is
   *          provided.
   * @return the newly created object or null if the object cannot be created
   * @throws Exception if this factory suggest not to try creating of this
   *           object by other alternative factories
   *           
   * @see NamingManager#getObjectInstance(Object, Name, Context, Hashtable)           
   */
  Object getObjectInstance (Object refObj, Name name, Context nameCtx,
                            Hashtable<?, ?> environment)
    throws Exception;
}

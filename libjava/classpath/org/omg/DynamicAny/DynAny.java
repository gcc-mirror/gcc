/* DynAny.java --
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


package org.omg.DynamicAny;

import java.io.Serializable;

import org.omg.CORBA.Any;
import org.omg.CORBA.portable.IDLEntity;

/**
 * <p>
 * The DynAny interface provides possibility to access the components of the
 * CORBA object, stored inside the {@link Any}. The DynAny and derived classes
 * additionally allows to access the members of the sequence, structure, union
 * and get the data about enumeration, value type and CORBA <code>fixed</code>
 * without knowing the exact type at the run time. The returned members are also
 * wrapped into DynAny objects, allowing them to be the nested structures.
 * </p>
 * <p>
 * The DynAny's are usually produced by {@link DynAnyFactory}. This factory is
 * obtained from the ORB: <br>
 * <code>
 * DynAnyFactory f = DynAnyFactoryHelper.narrow
 *   (orb.resolve_initial_references("DynAnyFactory"));
 * </code>
 * </p>
 * <p>
 * DynAny can also be returned by a method, invoked on another DynAny.
 * </p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynAny
  extends DynAnyOperations, IDLEntity, org.omg.CORBA.Object, Serializable
{
}
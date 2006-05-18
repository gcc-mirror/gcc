/* NamingContext.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CosNaming;

import org.omg.CosNaming.NamingContextPackage.AlreadyBound;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.InvalidName;
import org.omg.CosNaming.NamingContextPackage.NotEmpty;
import org.omg.CosNaming.NamingContextPackage.NotFound;

/**
 * The naming context operations. The naming context can
 * store (bound) and retrieve (resolve) the named objects or
 * named child contexts.
 *
 * @see NamingContextExtOperations
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface NamingContextOperations
{
  /**
   * Gives the object a name, valid in this context.
   *
   * @param a_name the name, being given to the object.
   * @param an_object the object, being named.
   *
   * @throws AlreadyBound if the object is already named in this context.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  void bind(NameComponent[] a_name, org.omg.CORBA.Object an_object)
     throws NotFound, CannotProceed, InvalidName, AlreadyBound;

  /**
   * Gives a child context name, valid in this context.
   *
   * @param a_name the name, being given to the child context.
   * @param a_context the child context being named.
   *
   * @throws AlreadyBound if the child context is already named in
   * the current context.
   */
  void bind_context(NameComponent[] a_name, NamingContext a_context)
             throws NotFound, CannotProceed, InvalidName, AlreadyBound;

  /**
   * Create a new context and give it a given name (bound it)
   * in the current context.
   *
   * @param a_name the name being given to the new context.
   *
   * @return the newly created context.
   *
   * @throws AlreadyBound if the name is already in use.
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  NamingContext bind_new_context(NameComponent[] a_name)
                          throws NotFound, AlreadyBound, CannotProceed,
                                 InvalidName;

  /**
   * Destroy this context (must be empty).
   * @throws NotEmpty if the context being destroyed is not empty.
   */
  void destroy()
        throws NotEmpty;

  /**
   * Iterate over all bindings, defined in this namind context.
   *
   * @param amount the maximal number of context to return in the
   * holder a_list. The remaining bindings are accessible via iterator
   * an_iter. If the parameter amount is zero, all bindings are accessed only
   * via this iterator.
   *
   * @param a_list the holder, where the returned bindigs are stored.
   * @param an_iter the iterator that can be used to access the remaining
   * bindings.
   */
  void list(int amount, BindingListHolder a_list, BindingIteratorHolder an_iter);

  /**
   * Creates a new naming context, not bound to any name.
   */
  NamingContext new_context();

  /**
   * Names or renames the object.
   *
   * @param a_name the new name, being given to the object. If
   * the object is already named in this context, it is renamed.
   *
   * @param an_object the object, being named.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  void rebind(NameComponent[] a_name, org.omg.CORBA.Object an_object)
       throws NotFound, CannotProceed, InvalidName;

  /**
   * Names or renames the child context.
   * If the child context is already named in
   * the current context, it is renamed.
   *
   * @param a_name the name, being given to the child context.
   * @param a_context the child context being named.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  void rebind_context(NameComponent[] a_name, NamingContext a_context)
               throws NotFound, CannotProceed, InvalidName;

  /**
   * Get the object, bound to the specified name in this
   * context.
   *
   * @param a_name the object name.
   *
   * @return the object, matching this name. The client
   * usually casts or narrows (using the helper) the returned value
   * to the more specific type.
   *
   * @throws NotFound
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  org.omg.CORBA.Object resolve(NameComponent[] a_name)
                        throws NotFound, CannotProceed, InvalidName;

  /**
   * Removes the name from the binding context.
   *
   * @param a_name a name to remove.
   *
   * @throws InvalidName if the name has zero length or otherwise invalid.
   */
  void unbind(NameComponent[] a_name)
       throws NotFound, CannotProceed, InvalidName;
}

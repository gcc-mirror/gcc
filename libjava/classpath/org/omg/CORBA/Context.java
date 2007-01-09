/* Context.java --
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


package org.omg.CORBA;


/**
 *
 * This class holds the list of the named properties. It is normally
 * used to represent information about various circumstances of the
 * invocation. A Context if first created by
 * {@link org.omg.CORBA.ORB#get_default_context() } and then invoking
 * {@link #create_child(String)} of the default context.
 *
 * The contexts are named.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 *
 */
public abstract class Context
{
  public Context()
  {
  }

  /**
   * Get the context name.
   * @return the name of this context.
   */
  public abstract String context_name();

  /**
   * Create a child of this Context, giving it a name.
   * @param child a name of the child context.
   *
   * @return the newly created context.
   */
  public abstract Context create_child(String child);

  /**
   * Delete one or several (identically named) given properties.
   *
   * @param property the name of the property to delete, may
   * end by wildchar character '*'. The search scope is always
   * limited to the current context.
   */
  public abstract void delete_values(String property);

  /**
   * Search the values.
   *
   * @param start_scope the context at which to initiate the search.
   * @param flags the search operation flags. 
   * The flag {@link CTX_RESTRICT_SCOPE} means
   * that search is restricted to the start_scope.
   * @param pattern the property being searched, can be
   * either name or name with the optional trailing wildchar character
   * '*'.
   * @return the list of the found properties.
   */
  public abstract NVList get_values(String start_scope, int flags,
                                    String pattern
                                   );

  /**
   * Get the parent of this context.
   * @return the parent of this context.
   */
  public abstract Context parent();

  /**
   * Set a property.
   * @param name the property name.
   * @param value the property value (the {@link Any} must hold string).
   */
  public abstract void set_one_value(String name, Any value);
  
  /**
   * Set multiple properties.
   * 
   * @param values a list of properties, the {@link Any}'s 
   * in the list components must hold strings.
   */
  public abstract void set_values(NVList values);
  
  
}

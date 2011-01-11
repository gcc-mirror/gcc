/* NameClassPair.java --
   Copyright (C) 2001, 2005, 2006  Free Software Foundation, Inc.

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


package javax.naming;

import java.io.Serializable;

/**
 * <code>NameClassPair</code> represents the name-classname mapping pair
 * of a binding in a context.
 * <p>
 * Bindings are mappings of a name to an object and this class is used to
 * specify the mapping of the name to the class type of the bound object.
 * As classname the fully qualified classname is used.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @since 1.3
 */
public class NameClassPair implements Serializable
{
  private static final long serialVersionUID = 5620776610160863339L;

  /**
   * Constructs an instance with the given name and classname.
   *
   * @param name the name of the binding relative to the target context
   * (may not be <code>null</code>)
   * @param className the name of the class. If <code>null</code> the bound
   * object is also <code>null</code>
   */
  public NameClassPair (String name, String className)
  {
    this (name, className, true);
  }

  /**
   * Constructs an instance with the given name and classname and a
   * flag indicating if the name is relative to the target context.
   *
   * @param name the name of the binding (may not be <code>null</code>)
   * @param className the name of the class. If <code>null</code> the bound
   * object is also <code>null</code>
   * @param isRelative flag indicating if the name is relative or not
   */
  public NameClassPair (String name, String className, boolean isRelative)
  {
    this.name = name;
    this.className = className;
    this.isRel = isRelative;
  }

  /**
   * Returns the classname of the binding.
   * @return The fully qualified classname or <code>null</code> if the
   * bound object is null.
   */
  public String getClassName ()
  {
    return className;
  }

  /**
   * Returns the name of the binding.
   * @return The name.
   */
  public String getName ()
  {
    return name;
  }

  /**
   * Checks whether the name is relative to the target context or not.
   * @return <code>true</code> if the name is relative,
   * <code>false</code> otherwise.
   */
  public boolean isRelative ()
  {
    return isRel;
  }

  /**
   * Sets the classname of the bound object.
   * @param name the classname to set (maybe <code>null</code>)
   */
  public void setClassName (String name)
  {
    this.className = name;
  }

  /**
   * Sets the name of the binding.
   * @param name the name to set
   */
  public void setName (String name)
  {
    this.name = name;
  }

  /**
   * Sets if the name is relative to the target context.
   * @param r <code>true</code> to mark as relative
   */
  public void setRelative (boolean r)
  {
    this.isRel = r;
  }

  /**
   * Sets the full name for this binding. Setting the full name by this
   * method is the only way to initialize full names of bindings if
   * supported by a specific naming system.
   *
   * @param fullName the full name of this binding. If not set or set to
   * <code>null</code> the <code>getNameInNamespace()</code> method will
   * throw an exception
   *
   * @see #getNameInNamespace()
   *
   * @since 1.5
   */
  public void setNameInNamespace(String fullName)
  {
    this.fullName = fullName;
  }

  /**
   * Returns the full name for this binding. The full name of a binding is
   * defined as the absolute name in its own namespace and is not valid
   * outside.
   *
   * @return The full name in the bindings namespace.
   * @throws UnsupportedOperationException if no full name is applicable in
   * the specific naming system.
   *
   * @see Context#getNameInNamespace()
   *
   * @since 1.5
   */
  public String getNameInNamespace()
  {
    if (this.fullName == null)
      throw new UnsupportedOperationException();

    return this.fullName;
  }

  /**
   * Returns the string representation.
   * @return The string <code>getName() + ":" + getClassName()</code>.
   */
  public String toString ()
  {
    // Specified by class documentation.
    return name + ":" + className;
  }

  // These field names are fixed by the serialization spec.
  private String name;
  private String className;
  private boolean isRel;
  private String fullName;
}

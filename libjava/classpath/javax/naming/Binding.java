/* Binding.java --
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

/**
 * <code>Binding</code> represents the name-object mapping of a
 * binding in a context.
 * <p>
 * Bindings are mappings of a name to an object and this class is used to
 * specify such mappings. The bindings of a context are retrieved by the
 * <code>Context#listBindings()</code> methods.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @since 1.3
 */
public class Binding extends NameClassPair
{
  private static final long serialVersionUID = 8839217842691845890L;

  /**
   * Constructs an instance with the given name and object.
   *
   * @param name the name of the binding relative to the target context
   * (may not be <code>null</code>)
   * @param obj the bound object
   */
  public Binding (String name, Object obj)
  {
    super (name, null);
    boundObj = obj;
  }

  /**
   * Constructs an instance with the given name and object and a
   * flag indicating if the name is relative to the target context.
   *
   * @param name the name of the binding relative to the target context
   * (may not be <code>null</code>)
   * @param obj the bound object
   * @param isRelative flag indicating if the name is relative or not
   */
  public Binding (String name, Object obj, boolean isRelative)
  {
    super (name, null, isRelative);
    boundObj = obj;
  }

  /**
   * Constructs an instance with the given name, classname and object.
   *
   * @param name the name of the binding relative to the target context
   * (may not be <code>null</code>)
   * @param className the classname to set (maybe <code>null</code>)
   * @param obj the bound object
   */
  public Binding (String name, String className, Object obj)
  {
    super (name, className);
    boundObj = obj;
  }

  /**
   * Constructs an instance with the given name, classname, object and a
   * flag indicating if the name is relative to the target context.
   *
   * @param name the name of the binding relative to the target context
   * (may not be <code>null</code>)
   * @param className the classname to set (maybe <code>null</code>)
   * @param isRelative flag indicating if the name is relative or not
   * @param obj the bound object
   */
  public Binding (String name, String className, Object obj,
                  boolean isRelative)
  {
    super (name, className, isRelative);
    boundObj = obj;
  }

  /**
   * Returns the classname of the bound object.
   * <p>
   * Returns the classname if set explicitly. If not and the bound object is
   * not <code>null</code> the classname of the bound object is used.
   * </p>
   *
   * @return The fully qualified classname (may be <code>null</code>).
   */
  public String getClassName ()
  {
    String r = super.getClassName ();
    if (r != null)
      return r;
    return boundObj == null ? null : boundObj.getClass ().getName ();
  }

  /**
   * Returns the bound object of this binding.
   * @return The bound object (maybe <code>null</code>).
   */
  public Object getObject ()
  {
    return boundObj;
  }

  /**
   * Sets the bound object of this binding.
   * @param obj the bound object.
   */
  public void setObject (Object obj)
  {
    boundObj = obj;
  }

  /**
   * Returns the string representation.
   * @return The string as given by the NameClassPair superclass plus
   * the bound objects string representation seperated by a colon.
   */
  public String toString ()
  {
    // Format specified by the documentation.
    return super.toString () + ":" + boundObj.toString ();
  }

  // This name is fixed by the serialization spec.
  private Object boundObj;
}

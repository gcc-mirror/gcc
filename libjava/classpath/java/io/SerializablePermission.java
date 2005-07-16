/* SerializablePermission.java -- Basic permissions related to serialization.
   Copyright (C) 1998, 2000, 2003 Free Software Foundation, Inc.

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


package java.io;

import java.security.BasicPermission;

/**
  * This class models permissions related to serialization.  As a subclass
  * of <code>BasicPermission</code>, this class has permissions that have
  * a name only.  There is no associated action list.
  * <p>
  * There are currently two allowable permission names for this class:
  * <ul>
  * <li><code>enableSubclassImplementation</code> - Allows a subclass to
  * override the default serialization behavior of objects.</li>
  * <li><code>enableSubstitution</code> - Allows substitution of one object
  * for another during serialization or deserialization.</li>
  * </ul>
  *
  * @see java.security.BasicPermission
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public final class SerializablePermission extends BasicPermission
{
  static final long serialVersionUID = 8537212141160296410L;
	
  /*
   * Class Variables
   */

  private static final String[] legal_names = { "enableSubclassImplementation",
  					      "enableSubstitution" };
  /*
   * Constructors
   */

  /**
    * This method initializes a new instance of 
    * <code>SerializablePermission</code>
    * that has the specified name.
    *
    * @param name The name of the permission.
    *
    * @exception IllegalArgumentException If the name is not valid for
    * this class.
    */
  public SerializablePermission(String name)
  {
    this(name, null);
  }

  /**
    * This method initializes a new instance of 
    * <code>SerializablePermission</code>
    * that has the specified name and action list.  Note that the action list
    * is unused in this class.
    *
    * @param name The name of the permission.
    * @param actions The action list (unused).
    *
    * @exception IllegalArgumentException If the name is not valid for 
    * this class.
    */
  public SerializablePermission(String name, String actions)
  {
    super(name, actions);

    for (int i = 0; i < legal_names.length; i++)
      if (legal_names[i].equals(name))
        return;

    throw new IllegalArgumentException("Bad permission name:  " + name);
  }

} // class SerializablePermission


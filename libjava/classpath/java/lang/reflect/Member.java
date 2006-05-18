/* java.lang.reflect.Member - common query methods in reflection
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * Member is an interface that represents any member of a class (field or
 * method) or a constructor. You can get information about the declaring
 * class, name or modifiers of the member with this interface.
 *
 * @author John Keiser
 * @author Per Bothner (bothner@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Class
 * @see Field
 * @see Method
 * @see Constructor
 * @since 1.1
 * @status updated to 1.4
 */
public interface Member
{
  /**
   * Represents all members, whether public, private, protected or
   * package-protected, but only which are declared in this class.
   * Used in SecurityManager.checkMemberAccess() to determine the
   * type of members to access.
   * @see SecurityManager#checkMemberAccess(Class, int)
   */
  int DECLARED = 1;

  /**
   * Represents public members only, but includes all inherited members.
   *  Used in SecurityManager.checkMemberAccess() to determine the type of
   * members to access.
   * @see SecurityManager#checkMemberAccess(Class, int)
   */
  int PUBLIC = 0;

  /**
   * Gets the class that declared this member. This is not the class where
   * this method was called, or even the class where this Member object
   * came to life, but the class that declares the member this represents.
   *
   * @return the class that declared this member
   */
  Class getDeclaringClass();

  /**
   * Gets the simple name of this member. This will be a valid Java
   * identifier, with no qualification.
   *
   * @return the name of this member
   */
  String getName();

  /**
   * Gets the modifiers this member uses.  Use the <code>Modifier</code>
   * class to interpret the values.
   *
   * @return an integer representing the modifiers to this Member
   * @see Modifier
   */
  int getModifiers();

  /**
   * Return true if this member is synthetic, meaning that it was
   * created by the compiler and does not appear in the user's
   * source code.
   * @return true if the member is synthetic
   * @since 1.5
   */
  boolean isSynthetic();
}

/* java.lang.reflect.Member - common query methods in reflection
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.lang.reflect;

/**
 * Member is an interface that represents any member of a class (field or
 * method) or a constructor. You can get information about the declaring
 * class, name or modifiers of the member with this interface.
 *
 * @author John Keiser
 * @author Per Bothner <bothner@cygnus.com>
 * @author Eric Blake <ebb9@email.byu.edu>
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
   * @see SecurityManager#checkMemberAccess()
   */
  int DECLARED = 1;

  /**
   * Represents public members only, but includes all inherited members.
   *  Used in SecurityManager.checkMemberAccess() to determine the type of
   * members to access.
   * @see SecurityManager#checkMemberAccess()
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
}

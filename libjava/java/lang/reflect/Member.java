/* java.lang.reflect.Member
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

/* Written using "Java Class Libraries", 2nd edition.
 * Status:  Believed complete and correct.
 */

/**
 * Member is an interface that represents any member of a class.
 * i.e. a field, a method or a constructor.
 * You can get information about the declaring class, name or modifiers of
 * the member with this interface.
 *
 * @author  John Keiser
 * @author Per Bothner <bothner@cygnus.com> 
 */
public interface Member {
    /**
     * Represents all members, whether public, private, protected or
     * package-protected.
     * Used in java.lang.SecurityManager.checkMemberAccess() to determine the
     * type of members to access.
     */
    static final int DECLARED = 1;

    /**
     * Represents public members only.  Used inr
     * java.lang.SecurityManager.checkMemberAccess() to determine the type of
     * members to access.
     */
    static final int PUBLIC = 0;

    /**
     * Gets the class that declared this member.
     * <STRONG>It is unclear whether this returns the class that actually
     * syntactically declared the member, or the class where the
     * <code>Member</code> object was gotten from.</STRONG>
     * @return the class that declared this member.
     */
    Class getDeclaringClass();

    /**
     * Gets the modifiers this member uses.  Use the <code>Modifier</code>
     * class to interpret the values.
     * @see Modifier
     * @return an integer representing the modifiers to this Member.
     */
    int getModifiers();

    /**
     * Gets the name of this member.
     * @return the name of this member.
     */
    String getName();
}

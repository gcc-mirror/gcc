/* Principal.java -- A security entity
   Copyright (C) 1998 Free Software Foundation, Inc.

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

package java.security;

/**
   * This interface models an entity (such as a user or a certificate authority)
   * for the purposes of applying the Java security model.
   *
   * @version 0.0
   *
   * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface Principal
{
  /**
   * This method returns a <code>String</code> that names this 
   * <code>Principal</code>.
   *
   * @return The name of this <code>Principal</code>.
   */
  public abstract String getName();

  /**
   * This method tests another <code>Principal</code> object for equality
   * with this one.
   * 
   * @param obj The <code>Object</code> (which is a <code>Principal</code>) to test for equality against.
   *
   * @return <code>true</code> if the specified <code>Principal</code> is equal to this one, <code>false</code> otherwise.
   */
  public abstract boolean equals(Object obj);

  /**
   * This method returns a hash code value for this <code>Principal</code>.
   *
   * @return A hash value
   */
  public abstract int hashCode();

  /**
   * This method returns a <code>String</code> representation of this
   * <code>Principal</code>.
   *
   * @return This <code>Principal</code> represented as a <code>String</code>.
   */
  public abstract String toString();
}

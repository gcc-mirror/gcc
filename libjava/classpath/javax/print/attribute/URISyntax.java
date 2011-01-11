/* URISyntax.java --
   Copyright (C) 2003, 2005 Free Software Foundation, Inc.

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

package javax.print.attribute;

import java.io.Serializable;
import java.net.URI;

/**
 * <code>URISyntax</code> is the abstract base class of all attribute
 * classes having an Uniform Resource Identifier URI as value.
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public abstract class URISyntax
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = -7842661210486401678L;

  private URI uri;

  /**
   * Creates a <code>URISyntax</code> object.
   *
   * @param uri the URI value for the syntax
   *
   * @exception NullPointerException if uri is null
   */
  protected URISyntax(URI uri)
  {
    if (uri == null)
      throw new NullPointerException("uri may not be null");

    this.uri = uri;
  }

  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof URISyntax))
      return false;

    return uri.equals(((URISyntax) obj).getURI());
  }

  /**
   * Returns the URI value of this syntax object.
   *
   * @return The URI.
   */
  public URI getURI()
  {
    return uri;
  }

  /**
   * Returns the hashcode for this object.
   *
   * @return The hashcode.
   */
  public int hashCode()
  {
    return uri.hashCode();
  }

  /**
   * Returns the string representation for this object.
   *
   * @return The string representation.
   */
  public String toString()
  {
    return uri.toString();
  }
}

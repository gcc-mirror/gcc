/* RefAddr.java -- Abstract superclass of addresses used in References
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

package javax.naming;

import java.io.Serializable;

/**
 * Abstract superclass of addresses used in References.
 * A <code>Reference</code> object contains a <code>Vector</code> of
 * <code>RefAddr</code>s which are used to reference/address the object.
 * This abstract superclass keeps track of the type of address, which will be
 * returned by <code>getType()</code>. And defines a abstract method
 * <code>getContent()</code> which must be implemented in concrete subclasses
 * such as <code>BinaryRefAddr</code> and <code>StringRefAddr</code>.
 *
 * @see Reference
 * @see BinaryRefAddr
 * @see StringRefAddr
 * @since 1.3
 * @author Anthony Green (green@redhat.com)
 * @author Mark Wielaard (mark@klomp.org)
 */
public abstract class RefAddr implements Serializable
{
  /**
   * The string resprenstation of the type of address.
   * Set by the constructor and returned by the getType() method.
   */
  protected final String addrType;

  /**
   * Protected constructor for use by subclasses.
   * Sets the addrType field of this object to the supplied String.
   *
   * @exception NullPointerException if the supplied String is null.
   */
  protected RefAddr(String addrType)
  {
    if (addrType == null)
      throw new NullPointerException("addrType cannot be null");

    this.addrType = addrType;
  }
  
  /**
   * Returns the non-null address type given to the constructor.
   */
  public String getType()
  {
    return addrType;
  }
  
  /**
   * Returns the possibly null content of this RefAddr.
   * The actual value is defined by the non-abstract subclass.
   */
  public abstract Object getContent();
  
  /**
   * Checks if the object is a RefAddr with the same type and content.
   *
   * @return true if the given object is an instance of RefAddr, the addrType
   *         is the same as this addrType and the content is equals to the
   *         content of this object.
   */
  public boolean equals (Object o)
  {
    if (o instanceof RefAddr)
      {
        RefAddr refAddr = (RefAddr) o;
        if (this.getType().equals(refAddr.getType()))
	  {
	    Object c1 = this.getContent();
	    Object c2 = refAddr.getContent();
	    if (c1 == null)
	      return c2 == null;
	    else
	      return c1.equals(c2);
	  }
      }
    return false;
  }

  /**
   * Returns the hashCode which is the hasCode of the String returned by
   * <code>getType()</code> plus the hashCode of the Object returned by
   * <code>getContent</code> (when not null).
   */
  public int hashCode()
  {
    int result = getType().hashCode();
    Object o = getContent();
    if (o != null)
      result += o.hashCode();

    return result;
  }

  /**
   * Returns a String representation of the RefAddr.
   * Should only be used for debugging purposes.
   */
  public String toString()
  {
    return "[RefAddr type: " + getType() + " content: " + getContent() + ']';
  }
}

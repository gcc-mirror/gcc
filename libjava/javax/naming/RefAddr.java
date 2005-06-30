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
  protected String addrType;

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
  public boolean equals(Object o)
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

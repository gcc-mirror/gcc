/* CollectionCertStoreParameters -- collection-based cert store parameters
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.security.cert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 * An implementation of {@link CertStoreParameters} with a simple,
 * in-memory {@link Collection} of certificates and certificate
 * revocation list.
 *
 * <p>Note that this class is not thread-safe, and its underlying
 * collection may be changed at any time.
 *
 * @see CertStore
 */
public class CollectionCertStoreParameters implements CertStoreParameters
{

  // Constants and fields.
  // ------------------------------------------------------------------------

  /** The underlying collection. */
  private final Collection collection;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Creates a new CollectionCertStoreParameters with an empty,
   * immutable collection.
   */
  public CollectionCertStoreParameters()
  {
    this(Collections.EMPTY_LIST);
  }

  /**
   * Create a new CollectionCertStoreParameters with the specified
   * collection. The argument is not copied, and subsequent changes to
   * the collection will change this class's collection.
   *
   * @param collection The collection.
   * @throws NullPointerException If <i>collection</i> is null.
   */
  public CollectionCertStoreParameters(Collection collection)
  {
    if (collection == null)
      throw new NullPointerException();
    this.collection = collection;
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  public Object clone()
  {
    return new CollectionCertStoreParameters(new ArrayList(collection));
  }

  /**
   * Return the underlying collection. The collection is not copied
   * before being returned, so callers may update the collection that is
   * returned.
   *
   * @return The collection.
   */
  public Collection getCollection()
  {
    return collection;
  }

  /**
   * Return a string representation of these parameters.
   *
   * @return The string representation of these parameters.
   */
  public String toString()
  {
    return "CollectionCertStoreParameters: [ collection: "
      + collection + " ]";
  }
}

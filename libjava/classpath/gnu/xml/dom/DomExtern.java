/* DomExtern.java --
   Copyright (C) 1999,2000,2001,2004 Free Software Foundation, Inc.

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

package gnu.xml.dom;

/**
 * <p> Abstract implemention of nodes describing external DTD-related
 * objects.  This facilitates reusing code for Entity, Notation, and
 * DocumentType (really, external subset) nodes.  Such support is not
 * part of the core DOM; it's for the "XML" feature.  </p>
 *
 * <p> Note that you are strongly advised to avoid using the DOM
 * features that take advantage of this class, since (as of L2) none
 * of them is defined fully enough to permit full use of the
 * XML feature they partially expose. </p>
 *
 * @author David Brownell
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class DomExtern
  extends DomNode
{

  private final String name;
  private final String publicId;
  private final String systemId;

  /**
   * Constructs a node associated with the specified document,
   * with the specified descriptive data.
   *
   * @param owner The document with which this object is associated
   * @param name Name of this object
   * @param publicId If non-null, provides the entity's PUBLIC identifier
   * @param systemId If non-null, provides the entity's SYSTEM identifier
   */
  // package private
  DomExtern(short nodeType,
            DomDocument owner,
            String name,
            String publicId,
            String systemId)
  {
    super(nodeType, owner);
    this.name = name;
    this.publicId = publicId;
    this.systemId = systemId;
  }

  /**
   * <b>DOM L1</b>
   * Returns the SYSTEM identifier associated with this object, if any.
   */
  public final String getSystemId()
  {
    return systemId;
  }

  /**
   * <b>DOM L1</b>
   * Returns the PUBLIC identifier associated with this object, if any.
   */
  public final String getPublicId()
  {
    return publicId;
  }

  /**
   * <b>DOM L1</b>
   * Returns the object's name.
   */
  public final String getNodeName()
  {
    return name;
  }

  public final String getLocalName()
  {
    return name;
  }

}

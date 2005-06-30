/* DomEntity.java -- 
   Copyright (C) 1999,2000,2004 Free Software Foundation, Inc.

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

import org.w3c.dom.Entity;

/**
 * <p> "Entity" implementation.  This is a non-core DOM class, supporting the
 * "XML" feature.  There are two types of entities, neither of which works
 * particularly well in this API:</p><dl>
 *
 * <dt><em>Unparsed Entities</em></dt>
 *	<dd>Since ENTITY/ENTITIES attributes, the only legal use of unparsed
 *	entities in XML, can't be detected with DOM, there isn't much point in
 *	trying to use unparsed entities in DOM applications.  (XML Linking is
 *	working to provide a better version of this functionality.) </dd>
 *
 * <dt><em>Parsed Entities</em></dt>
 *	<dd> While the DOM specification permits nodes for parsed entities
 *	to have a readonly set of children, this is not required and there
 *	is no portable way to provide such children.  <em>This implementation
 *	currently does not permit children to be added to Entities.</em>
 *	There are related issues with the use of EntityReference nodes.  </dd>
 *
 * </dl>
 *
 * <p> In short, <em>avoid using this DOM functionality</em>.
 *
 * @see DomDoctype
 * @see DomEntityReference
 * @see DomNotation
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomEntity
  extends DomExtern
  implements Entity
{
  
  private String notation;

  /**
   * Constructs an Entity node associated with the specified document,
   * with the specified descriptive data.
   *
   * <p>This constructor should only be invoked by a DomDoctype as part
   * of its declareEntity functionality, or through a subclass which is
   * similarly used in a "Sub-DOM" style layer.
   *
   * @param owner The document with which this entity is associated
   * @param name Name of this entity
   * @param publicId If non-null, provides the entity's PUBLIC identifier
   * @param systemId Provides the entity's SYSTEM identifier (URI)
   * @param notation If non-null, provides the unparsed entity's notation.
   */
  protected DomEntity(DomDocument owner,
                      String name,
                      String publicId,
                      String systemId,
                      String notation)
  {
    super(ENTITY_NODE, owner, name, publicId, systemId);
    this.notation = notation;

    // NOTE:  if notation == null, this is a parsed entity
    // which could reasonably be given child nodes ...
    makeReadonly();
  }

  /**
   * <b>DOM L1</b>
   * Returns the NOTATION identifier associated with this entity, if any.
   */
  final public String getNotationName()
  {
    return notation;
  }

  // DOM Level 3 methods
  
  public String getInputEncoding()
  {
    // TODO
    return null;    
  }
  
  public String getXmlEncoding()
  {
    // TODO
    return null;
  }

  public String getXmlVersion()
  {
    // TODO
    return null;
  }

  /**
   * The base URI of an external entity is its system ID.
   * The base URI of an internal entity is the parent document's base URI.
   * @since DOM Level 3 Core
   */
  public String getBaseURI()
  {
    String systemId = getSystemId();
    return (systemId == null) ? owner.getBaseURI() : systemId;
  }
  
}


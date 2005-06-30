/* DomNotation.java -- 
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

import org.w3c.dom.Notation;

/**
 * <p> "Notation" implementation.  This is a non-core DOM class, supporting
 * the "XML" feature. </p>
 *
 * <p> Although unparsed entities using this notation can be detected using
 * DOM, neither NOTATIONS nor ENTITY/ENTITIES attributes can be so detected.
 * More, there is no portable way to construct a Notation node, so there's
 * no way that vendor-neutral DOM construction APIs could even report a
 * NOTATION used to identify the intended meaning of a ProcessingInstruction.
 * </p>
 *
 * <p> In short, <em>avoid using this DOM functionality</em>.
 *
 * @see DomDoctype
 * @see DomEntity
 * @see DomPI
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomNotation
  extends DomExtern
  implements Notation
{
    
  /**
   * Constructs a Notation node associated with the specified document,
   * with the specified descriptive data.  Note that at least one of
   * the PUBLIC and SYSTEM identifiers must be provided; unlike other
   * external objects in XML, notations may have only a PUBLIC identifier.
   *
   * <p>This constructor should only be invoked by a DomDoctype object
   * as part of its declareNotation functionality, or through a subclass
   * which is similarly used in a "Sub-DOM" style layer. 
   *
   * @param owner The document with which this notation is associated
   * @param name Name of this notation
   * @param publicId If non-null, provides the notation's PUBLIC identifier
   * @param systemId If non-null, rovides the notation's SYSTEM identifier
   */
  protected DomNotation(DomDocument owner,
                        String name,
                        String publicId,
                        String systemId)
  {
    super(NOTATION_NODE, owner, name, publicId, systemId);
    makeReadonly();
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


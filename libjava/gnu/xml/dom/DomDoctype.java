/* DomDoctype.java -- 
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

import java.util.HashMap;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Entity;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Notation;

/**
 * <p> "DocumentType" implementation (with no extensions for supporting
 * any document typing information).  This is a non-core DOM class,
 * supporting the "XML" feature. </p>
 *
 * <p> <em>Few XML applications will actually care about this partial
 * DTD support</em>, since it doesn't expose any (!) of the data typing
 * facilities which can motivate applications to use DTDs.  It does not
 * expose element content models, or information about attribute typing
 * rules.  Plus the information it exposes isn't very useful; as one example,
 * DOM exposes information about unparsed ENTITY objects, which is only used
 * with certain element attributes, but does not expose the information about
 * those attributes which is needed to apply that data! </p>
 *
 * <p> Also, note that there are no nonportable ways to associate even the
 * notation and entity information exposed by DOM with a DocumentType.  While
 * there is a DOM L2 method to construct a DocumentType, it only gives access
 * to the textual content of the &lt;!DOCTYPE ...&gt; declaration.  </p>
 *
 * <p> In short, <em>you are strongly advised not to rely on this incomplete
 * DTD functionality</em> in your application code.</p>
 *
 * @see DomEntity
 * @see DomEntityReference
 * @see DomNotation
 *
 * @author David Brownell 
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class DomDoctype
  extends DomExtern
  implements DocumentType
{
  
  private DomNamedNodeMap notations;
  private DomNamedNodeMap entities;
  private final DOMImplementation implementation;
  private String subset;
  
  private HashMap elements = new HashMap();
  private boolean ids;
  
  /**
   * Constructs a DocumentType node associated with the specified
   * implementation, with the specified name.
   *
   * <p>This constructor should only be invoked by a DOMImplementation as
   * part of its createDocumentType functionality, or through a subclass
   * which is similarly used in a "Sub-DOM" style layer.
   *
   * <p> Note that at this time there is no standard SAX API granting
   * access to the internal subset text, so that relying on that value
   * is not currently portable.
   *
   * @param impl The implementation with which this object is associated
   * @param name Name of this root element
   * @param publicId If non-null, provides the external subset's
   *	PUBLIC identifier
   * @param systemId If non-null, provides the external subset's
   *	SYSTEM identifier
   * @param internalSubset Provides the literal value (unparsed, no
   *	entities expanded) of the DTD's internal subset.
   */
  protected DomDoctype(DOMImplementation impl,
                       String name,
                       String publicId,
                       String systemId,
                       String internalSubset)
  {
    super(DOCUMENT_TYPE_NODE, null, name, publicId, systemId);
    implementation = impl;
    subset = internalSubset;
  }

  /**
   * JAXP builder constructor.
   * @param doc the document
   * @param name the name of the document element
   * @param publicId the public ID of the document type declaration
   * @param systemId the system ID of the document type declaration
   */
  public DomDoctype(DomDocument doc,
                    String name,
                    String publicId,
                    String systemId)
  {
    super(DOCUMENT_TYPE_NODE, doc, name, publicId, systemId);
    implementation = doc.getImplementation();
  }

  /**
   * <b>DOM L1</b>
   * Returns the root element's name (just like getNodeName).
   */
  final public String getName()
  {
    return getNodeName();
  }

  /**
   * <b>DOM L1</b>
   * Returns information about any general entities declared
   * in the DTD.
   *
   * <p><em>Note:  DOM L1 doesn't throw a DOMException here, but
   * then it doesn't have the strange construction rules of L2.</em>
   *
   * @exception DOMException HIERARCHY_REQUEST_ERR if the DocumentType
   *	is not associated with a document.
   */
  public NamedNodeMap getEntities()
  {
    if (entities == null)
      {
        entities = new DomNamedNodeMap(this, Node.ENTITY_NODE);
      }
    return entities;
  }

  /**
   * Records the declaration of a general entity in this DocumentType.
   *
   * @param name Name of the entity
   * @param publicId If non-null, provides the entity's PUBLIC identifier
   * @param systemId Provides the entity's SYSTEM identifier
   * @param notation If non-null, provides the entity's notation
   *	(indicating an unparsed entity)
   * @return The Entity that was declared, or null if the entity wasn't
   *	recorded (because it's a parameter entity or because an entity with
   *	this name was already declared).
   *
   * @exception DOMException NO_MODIFICATION_ALLOWED_ERR if the
   *	DocumentType is no longer writable.
   * @exception DOMException HIERARCHY_REQUEST_ERR if the DocumentType
   *	is not associated with a document.
   */
  public Entity declareEntity(String name,
                              String publicId,
                              String systemId,
                              String notation)
  {
    DomEntity entity;
    
    if (name.charAt(0) == '%' || "[dtd]".equals(name))
      {
        return null;
      }
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    getEntities();
    
    DomDocument.checkName(name, (owner != null) ?
                          "1.1".equals(owner.getXmlVersion()) : false);
    if (entities.getNamedItem(name) != null)
      {
        return null;
      }
    
    entity = new DomEntity(owner, name, publicId, systemId, notation);
    entities.setNamedItem(entity);
    return entity;
  }
  
  /**
   * <b>DOM L1</b>
   * Returns information about any notations declared in the DTD.
   *
   * <p><em>Note:  DOM L1 doesn't throw a DOMException here, but
   * then it doesn't have the strange construction rules of L2.</em>
   *
   * @exception DOMException HIERARCHY_REQUEST_ERR if the DocumentType
   *	is not associated with a document.
   */
  public NamedNodeMap getNotations()
  {
    if (notations == null)
      {
        notations = new DomNamedNodeMap(this, Node.NOTATION_NODE);
      }
    return notations;
  }

  /**
   * Records the declaration of a notation in this DocumentType.
   *
   * @param name Name of the notation
   * @param publicId If non-null, provides the notation's PUBLIC identifier
   * @param systemId If non-null, provides the notation's SYSTEM identifier
   * @return The notation that was declared.
   *
   * @exception DOMException NO_MODIFICATION_ALLOWED_ERR if the
   *	DocumentType is no longer writable.
   * @exception DOMException HIERARCHY_REQUEST_ERR if the DocumentType
   *	is not associated with a document.
   */
  public Notation declareNotation(String name,
                                  String publicId,
                                  String systemId)
  {
    DomNotation notation;
    
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    getNotations();
    
    DomDocument.checkName(name, (owner != null) ?
                          "1.1".equals(owner.getXmlVersion()) : false);
    
    notation = new DomNotation(owner, name, publicId, systemId);
    notations.setNamedItem(notation);
    return notation;
  }

  /**
   * <b>DOM L2</b>
   * Returns the internal subset of the document, as a string of unparsed
   * XML declarations (and comments, PIs, whitespace); or returns null if
   * there is no such subset.  There is no vendor-independent expectation
   * that this attribute be set, or that declarations found in it be
   * reflected in the <em>entities</em> or <em>notations</em> attributes
   * of this Document "Type" object.
   *
   * <p> Some application-specific XML profiles require that documents
   * only use specific PUBLIC identifiers, without an internal subset
   * to modify the interperetation of the declarations associated with
   * that PUBLIC identifier through some standard.
   */
  public String getInternalSubset()
  {
    return subset;
  }

  /**
   * The base URI of a DocumentType is always <code>null</code>.
   * See the Infoset Mapping, appendix C.
   */
  public final String getBaseURI()
  {
    return null;
  }
    
  /**
   * Sets the internal "readonly" flag so the node and its associated
   * data (only lists of entities and notations, no type information
   * at the moment) can't be changed.
   */
  public void makeReadonly()
  {
    super.makeReadonly();
    if (entities != null)
      {
        entities.makeReadonly();
      }
    if (notations != null)
      {
        notations.makeReadonly();
      }
  }

  void setOwner(DomDocument doc)
  {
    if (entities != null)
      {
        for (DomNode ctx = entities.first; ctx != null; ctx = ctx.next)
          {
            ctx.setOwner(doc);
          }
      }
    if (notations != null)
      {
        for (DomNode ctx = notations.first; ctx != null; ctx = ctx.next)
          {
            ctx.setOwner(doc);
          }
      }
    super.setOwner(doc);
  }

  /**
   * <b>DOM L2</b>
   * Consults the DOM implementation to determine if the requested
   * feature is supported.
   */
  final public boolean supports(String feature, String version)
  {
    return implementation.hasFeature(feature, version);
  }
    
  /**
   * Returns the implementation associated with this document type.
   */
  final public DOMImplementation getImplementation()
  {
    return implementation;
  }

  public void elementDecl(String name, String model)
  {
    DTDElementTypeInfo info = getElementTypeInfo(name);
    if (info == null)
      {
        info = new DTDElementTypeInfo(name, model);
        elements.put(name, info);
      }
    else
      {
        info.model = model;
      }
  }

  DTDElementTypeInfo getElementTypeInfo(String name)
  {
    return (DTDElementTypeInfo) elements.get(name);
  }

  public void attributeDecl(String eName, String aName, String type,
                            String mode, String value)
  {
    DTDAttributeTypeInfo info = new DTDAttributeTypeInfo(eName, aName, type,
                                                         mode, value);
    DTDElementTypeInfo elementInfo = getElementTypeInfo(eName);
    if (elementInfo == null)
      {
        elementInfo = new DTDElementTypeInfo(eName, null);
        elements.put(eName, elementInfo);
      }
    elementInfo.setAttributeTypeInfo(aName, info);
    if ("ID".equals(type))
      {
        ids = true;
      }
  }

  DTDAttributeTypeInfo getAttributeTypeInfo(String elementName, String name)
  {
    DTDElementTypeInfo elementInfo =
      (DTDElementTypeInfo) elements.get(elementName);
    return (elementInfo == null) ? null :
      elementInfo.getAttributeTypeInfo(name);
  }

  boolean hasIds()
  {
    return ids;
  }
  
  public boolean isSameNode(Node arg)
  {
    if (equals(arg))
      {
        return true;
      }
    if (!(arg instanceof DocumentType))
      {
        return false;
      }
    DocumentType doctype = (DocumentType) arg;
    if (!equal(getPublicId(), doctype.getPublicId()))
      {
        return false;
      }
    if (!equal(getSystemId(), doctype.getSystemId()))
      {
        return false;
      }
    if (!equal(getInternalSubset(), doctype.getInternalSubset()))
      {
        return false;
      }
    // TODO entities
    // TODO notations
    return true;
  }
  
  /**
   * Shallow clone of the doctype, except that associated
   * entities and notations are (deep) cloned.
   */
  public Object clone()
  {
    DomDoctype node = (DomDoctype) super.clone();

    if (entities != null)
      {
        node.entities = new DomNamedNodeMap(node, Node.ENTITY_NODE);
        for (DomNode ctx = entities.first; ctx != null; ctx = ctx.next)
          {
            node.entities.setNamedItem(ctx.cloneNode(true));
          }
      }
    if (notations != null)
      {
        node.notations = new DomNamedNodeMap(node, Node.NOTATION_NODE);
        for (DomNode ctx = notations.first; ctx != null; ctx = ctx.next)
          {
            node.notations.setNamedItem(ctx.cloneNode(true));
          }
      }
    return node;
  }


}

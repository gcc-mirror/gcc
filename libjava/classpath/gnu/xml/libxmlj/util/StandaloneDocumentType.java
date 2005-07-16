/* StandaloneDocumentType.java - 
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.xml.libxmlj.util;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMException;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.UserDataHandler;

/**
 * A "standalone" document type, i.e. one that isn't attached to a document
 * node.
 * This can be used to create new documents.
 */
public final class StandaloneDocumentType
implements DocumentType
{

  private final String name;
  private final String publicId;
  private final String systemId;

  public StandaloneDocumentType (String name, String publicId, String systemId)
  {
    this.name = name;
    this.publicId = publicId;
    this.systemId = systemId;
  }

  public String getName ()
  {
    return name;
  }

  public NamedNodeMap getEntities ()
  {
    // TODO
    return null;
  }
  
  public NamedNodeMap getNotations ()
  {
    // TODO
    return null;
  }

  public String getPublicId ()
  {
    return publicId;
  }

  public String getSystemId ()
  {
    return systemId;
  }

  public String getInternalSubset ()
  {
    return null;
  }
  
  // -- Node --

  public String getNodeName ()
  {
    return getName ();
  }

  public String getNodeValue ()
    throws DOMException
  {
    return null;
  }
  
  public void setNodeValue (String nodeValue)
    throws DOMException
  {
  }

  public short getNodeType ()
  {
    return DOCUMENT_TYPE_NODE;
  }

  public Node getParentNode ()
  {
    return null;
  }

  public NodeList getChildNodes ()
  {
    return new EmptyNodeList ();
  }

  public Node getFirstChild ()
  {
    return null;
  }

  public Node getLastChild ()
  {
    return null;
  }

  public Node getPreviousSibling ()
  {
    return null;
  }

  public Node getNextSibling ()
  {
    return null;
  }

  public NamedNodeMap getAttributes ()
  {
    return null;
  }

  public Document getOwnerDocument ()
  {
    return null;
  }

  public Node insertBefore (Node newChild, Node refChild)
    throws DOMException
  {
    throw new DOMException (DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public Node replaceChild (Node newChild, Node oldChild)
    throws DOMException
  {
    throw new DOMException (DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public Node removeChild (Node oldChild)
    throws DOMException
  {
    throw new DOMException (DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public Node appendChild (Node oldChild)
    throws DOMException
  {
    throw new DOMException (DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public boolean hasChildNodes ()
  {
    return false;
  }

  public Node cloneNode (boolean deep)
  {
    return new StandaloneDocumentType (name, publicId, systemId);
  }

  public void normalize ()
  {
  }

  public boolean isSupported (String feature, String version)
  {
    return false;
  }

  public String getNamespaceURI ()
  {
    return null;
  }

  public String getPrefix ()
  {
    return null;
  }

  public void setPrefix (String prefix)
  {
    throw new DOMException (DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public String getLocalName ()
  {
    return getName ();
  }

  public boolean hasAttributes ()
  {
    return false;
  }

  // DOM Level 3

  public String getBaseURI ()
  {
    return null;
  }

  public short compareDocumentPosition (Node node)
  {
    return -1;
  }

  public String getTextContent ()
  {
    return null;
  }

  public void setTextContent (String content)
  {
    throw new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR, null);
  }

  public boolean isSameNode (Node other)
  {
    return equals (other);
  }
  
  public String lookupPrefix (String namespace)
  {
    return null;
  }
  
  public boolean isDefaultNamespace (String namespace)
  {
    return false;
  }

  public String lookupNamespaceURI (String prefix)
  {
    return null;
  }

  public boolean isEqualNode (Node other)
  {
    return equals (other);
  }

  public Object getFeature (String feature, String version)
  {
    return null;
  }

  public Object setUserData (String name, Object value,
                             UserDataHandler handler)
  {
    return null;
  }

  public Object getUserData (String name)
  {
    return null;
  }

}

/* GnomeDocument.java - 
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

package gnu.xml.libxmlj.dom;

import gnu.java.lang.CPStringBuilder;
import gnu.xml.dom.DomNodeIterator;

import java.util.Iterator;

import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.DocumentType;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMErrorHandler;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.DOMStringList;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;
import org.w3c.dom.UserDataHandler;
import org.w3c.dom.traversal.DocumentTraversal;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.NodeIterator;
import org.w3c.dom.traversal.TreeWalker;
import org.w3c.dom.xpath.XPathEvaluator;
import org.w3c.dom.xpath.XPathException;
import org.w3c.dom.xpath.XPathExpression;
import org.w3c.dom.xpath.XPathNSResolver;

/**
 * A DOM document node implemented in libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class GnomeDocument
  extends GnomeNode
  implements Document, DOMConfiguration, XPathEvaluator, DocumentTraversal
{

  DOMImplementation dom;

  /**
   * Not currently used.
   */
  boolean strictErrorChecking;

  /* DOMConfiguration */
  boolean canonicalForm = false;
  boolean cdataSections = true;
  boolean checkCharacterNormalization = false;
  boolean comments = true;
  boolean datatypeNormalization = false;
  boolean elementContentWhitespace = true;
  boolean entities = true;
  DOMErrorHandler errorHandler;
  boolean namespaces = true;
  boolean namespaceDeclarations = true;
  boolean normalizeCharacters = false;
  boolean splitCdataSections = true;
  boolean validate = false;
  boolean validateIfSchema = false;
  boolean wellFormed = true;
  
  GnomeDocument(Object id)
  {
    super(id);
    strictErrorChecking = true;
  }
  
  protected void finalize()
  {
    free(id);
  }
  
  private native void free(Object id);

  public native DocumentType getDoctype();

  public DOMImplementation getImplementation()
  {
    return dom;
  }

  public native Element getDocumentElement();

  public Element createElement(String tagName)
    throws DOMException
  {
    return createElementNS(null, tagName);
  }

  public native DocumentType createDocumentType(String name, String publicId,
                                                String systemId);

  public native DocumentFragment createDocumentFragment();

  public native Text createTextNode(String data);

  public native Comment createComment(String data);

  public native CDATASection createCDATASection(String data)
    throws DOMException;

  public native ProcessingInstruction createProcessingInstruction(String target,
                                                                  String data)
    throws DOMException;

  public Attr createAttribute(String name)
    throws DOMException
  {
    return createAttributeNS(null, name);
  }

  public native EntityReference createEntityReference(String name)
    throws DOMException;

  public native NodeList getElementsByTagName(String tagName);

  public Node importNode(Node importedNode, boolean deep)
    throws DOMException
  {
    Node ret = xmljImportNode(importedNode, deep);
    if (importedNode instanceof GnomeNode)
      {
        ((GnomeNode) importedNode)
          .notifyUserDataHandlers(UserDataHandler.NODE_IMPORTED,
                                  importedNode, ret);
      }
    return ret;
  }

  private native Node xmljImportNode(Node importedNode, boolean deep)
    throws DOMException;

  public native Element createElementNS(String namespaceURI, String
                                        qualifiedName)
    throws DOMException;
  
  public native Attr createAttributeNS(String namespaceURI, String
                                       qualifiedName)
    throws DOMException;

  public native NodeList getElementsByTagNameNS(String namespaceURI,
      String localName);

  public Element getElementById(String elementId)
  {
    Element element = xmljGetElementById(elementId);
    if (element == null)
      {
        TreeWalker walker = createTreeWalker(this, NodeFilter.SHOW_ELEMENT,
                                             null, false);
        for (Node node = walker.nextNode(); node != null;
             node = walker.nextNode())
          {
            GnomeElement e = (GnomeElement) node;
            if (e.userIdAttrs != null)
              {
                for (Iterator i = e.userIdAttrs.iterator(); i.hasNext(); )
                  {
                    Attr attr = (Attr) i.next();
                    if (attr.getNodeValue().equals(elementId))
                      {
                        return e;
                      }
                  }
              }
          }
      }
    return element;
  }
  
  private native Element xmljGetElementById(String elementId);

  // DOM Level 3 methods

  public native String getInputEncoding();

  public native String getXmlEncoding();

  public native boolean getXmlStandalone();

  public native void setXmlStandalone(boolean xmlStandalone);

  public native String getXmlVersion();

  public native void setXmlVersion(String xmlVersion);

  public boolean getStrictErrorChecking()
  {
    return strictErrorChecking;
  }

  public void setStrictErrorChecking(boolean strictErrorChecking)
  {
    this.strictErrorChecking = strictErrorChecking;
  }
  
  public native String getDocumentURI();

  public native void setDocumentURI(String documentURI);

  public Node adoptNode(Node source)
    throws DOMException
  {
    if (source == null || !(source instanceof GnomeNode))
      {
        return null;
      }
    Node ret = xmljAdoptNode(source);
    if (source instanceof GnomeNode)
      {
        ((GnomeNode) source).
          notifyUserDataHandlers(UserDataHandler.NODE_ADOPTED,
                                 source, ret);
      }
    return ret;
  }

  private native Node xmljAdoptNode(Node source)
    throws DOMException;

  public DOMConfiguration getDomConfig()
  {
    return this;
  }

  public void normalizeDocument()
  {
    normalize();
  }

  public native Node renameNode(Node n, String namespaceURI,
                                String qualifiedName);

  // -- DOMConfiguration methods --

  public void setParameter(String name, Object value)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("canonical-form".equals(name))
      {
        /* optional
        canonicalForm = getBooleanValue(value);*/
      }
    else if ("cdata-sections".equals(name))
      {
        cdataSections = getBooleanValue(value);
      }
    else if ("check-character-normalization".equals(name))
      {
        /* optional
        checkCharacterNormalization = getBooleanValue(value);*/
      }
    else if ("comments".equals(name))
      {
        comments = getBooleanValue(value);
      }
    else if ("datatype-normalization".equals(name))
      {
        /* optional
        datatypeNormalization = getBooleanValue(value);*/
      }
    else if ("element-content-whitespace".equals(name))
      {
        /* optional
        elementContentWhitespace = getBooleanValue(value);*/
      }
    else if ("entities".equals(name))
      {
        entities = getBooleanValue(value);
      }
    else if ("error-handler".equals(name))
      {
        errorHandler = (DOMErrorHandler) value;
      }
    else if ("infoset".equals(name))
      {
        if (getBooleanValue(value))
          {
            validateIfSchema = false;
            entities = false;
            datatypeNormalization = false;
            cdataSections = false;
            namespaceDeclarations = true;
            wellFormed = true;
            elementContentWhitespace = true;
            comments = true;
            namespaces = true;
          }
      }
    else if ("namespaces".equals(name))
      {
        /* optional
        namespaces = getBooleanValue(value);*/
      }
    else if ("namespace-declarations".equals(name))
      {
        namespaceDeclarations = getBooleanValue(value);
      }
    else if ("normalize-characters".equals(name))
      {
        /* optional
        normalizeCharacters = getBooleanValue(value);*/
      }
    else if ("split-cdata-sections".equals(name))
      {
        splitCdataSections = getBooleanValue(value);
      }
    else if ("validate".equals(name))
      {
        /* optional
        validate = getBooleanValue(value);*/
      }
    else if ("validate-if-schema".equals(name))
      {
        /* optional
        validateIfSchema = getBooleanValue(value);*/
      }
    else if ("well-formed".equals(name))
      {
        /* optional
        wellFormed = getBooleanValue(value);*/
      }
    else
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, name);
      }
  }

  public Object getParameter(String name)
    throws DOMException
  {
    name = name.toLowerCase();
    if ("canonical-form".equals(name))
      {
        return Boolean.valueOf(canonicalForm);
      }
    else if ("cdata-sections".equals(name))
      {
        return Boolean.valueOf(cdataSections);
      }
    else if ("check-character-normalization".equals(name))
      {
        return Boolean.valueOf(checkCharacterNormalization);
      }
    else if ("comments".equals(name))
      {
        return Boolean.valueOf(comments);
      }
    else if ("datatype-normalization".equals(name))
      {
        return Boolean.valueOf(datatypeNormalization);
      }
    else if ("element-content-whitespace".equals(name))
      {
        return Boolean.valueOf(elementContentWhitespace);
      }
    else if ("entities".equals(name))
      {
        return Boolean.valueOf(entities);
      }
    else if ("error-handler".equals(name))
      {
        return errorHandler;
      }
    else if ("infoset".equals(name))
      {
        return Boolean.valueOf(!validateIfSchema &&
			       !entities &&
			       !datatypeNormalization &&
			       !cdataSections &&
			       namespaceDeclarations &&
			       wellFormed &&
			       elementContentWhitespace &&
			       comments &&
			       namespaces);
      }
    else if ("namespaces".equals(name))
      {
        return Boolean.valueOf(namespaces);
      }
    else if ("namespace-declarations".equals(name))
      {
        return Boolean.valueOf(namespaceDeclarations);
      }
    else if ("normalize-characters".equals(name))
      {
        return Boolean.valueOf(normalizeCharacters);
      }
    else if ("split-cdata-sections".equals(name))
      {
        return Boolean.valueOf(splitCdataSections);
      }
    else if ("validate".equals(name))
      {
        return Boolean.valueOf(validate);
      }
    else if ("validate-if-schema".equals(name))
      {
        return Boolean.valueOf(validateIfSchema);
      }
    else if ("well-formed".equals(name))
      {
        return Boolean.valueOf(wellFormed);
      }
    else
      {
        throw new GnomeDOMException(DOMException.NOT_FOUND_ERR, name);
      }
  }

  public boolean canSetParameter(String name, Object value)
  {
    name = name.toLowerCase();
    if ("error-handler".equals(name))
      {
        return (value == null || value instanceof DOMErrorHandler);
      }
    return ("cdata-sections".equals(name) ||
            "comments".equals(name) ||
            "entities".equals(name) ||
            "namespace-declarations".equals(name) ||
            "split-cdata-sections".equals(name));
  }
  
  public DOMStringList getParameterNames()
  {
    String[] names = new String[] {
      "canonical-form",
      "cdata-sections",
      "check-character-normalization",
      "comments",
      "datatype-normalization",
      "element-content-whitespace",
      "entities",
      "error-handler",
      "infoset",
      "namespaces",
      "namespace-declarations",
      "normalize-characters",
      "split-cdata-sections",
      "validate",
      "validate-if-schema",
      "well-formed"
    };
    return new GnomeDOMStringList(names);
  }

  private boolean getBooleanValue(Object value)
  {
    if (value instanceof Boolean)
      {
        return ((Boolean) value).booleanValue();
      }
    else if (value instanceof String)
      {
        return Boolean.valueOf ((String) value).booleanValue();
      }
    return false;
  }

  // -- XPathEvaluator methods --

  public XPathExpression createExpression(String expression,
                                          XPathNSResolver resolver)
    throws XPathException, DOMException
  {
    return new GnomeXPathExpression(this, expression, resolver);
  }

  public XPathNSResolver createNSResolver(Node nodeResolver)
  {
    return new GnomeXPathNSResolver(nodeResolver);
  }

  public native Object evaluate(String expression,
                                Node contextNode,
                                XPathNSResolver resolver,
                                short type,
                                Object result)
    throws XPathException, DOMException;

  // -- DocumentTraversal methods --

  public NodeIterator createNodeIterator(Node root,
                                         int whatToShow,
                                         NodeFilter filter,
                                         boolean entityReferenceExpansion)
    throws DOMException
  {
    return new DomNodeIterator(root, whatToShow, filter,
                               entityReferenceExpansion, false);
  }

  public TreeWalker createTreeWalker(Node root,
                                    int whatToShow,
                                    NodeFilter filter,
                                    boolean entityReferenceExpansion)
    throws DOMException
  {
    return new DomNodeIterator(root, whatToShow, filter,
                               entityReferenceExpansion, true);
  }

  // -- Debugging --
  
  public String toString()
  {
    CPStringBuilder buffer = new CPStringBuilder(getClass().getName());
    buffer.append("[version=");
    buffer.append(getXmlVersion());
    buffer.append(",standalone=");
    buffer.append(getXmlStandalone());
    buffer.append("]");
    return buffer.toString();
  }

}

/* XMLSchemaValidatorHandler.java -- 
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.xml.validation.xmlschema;

import java.util.ArrayList;
import java.util.LinkedList;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.validation.TypeInfoProvider;
import javax.xml.validation.ValidatorHandler;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.DatatypeLibrary;
import org.relaxng.datatype.helpers.DatatypeLibraryLoader;
import org.w3c.dom.TypeInfo;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.ext.Attributes2Impl;
import org.xml.sax.helpers.NamespaceSupport;
import gnu.xml.validation.datatype.SimpleType;
import gnu.xml.validation.datatype.Type;

/**
 * Streaming validator.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class XMLSchemaValidatorHandler
  extends ValidatorHandler
{

  final XMLSchema schema;
  final TypeInfoProvider typeInfoProvider;
  final NamespaceSupport namespaceSupport;
  final DatatypeLibrary typeLibrary;
  Locator loc;
  ContentHandler contentHandler;
  ErrorHandler errorHandler;
  LSResourceResolver resourceResolver;
  final LinkedList context; // element context
  final ArrayList attributes; // attribute context;

  XMLSchemaValidatorHandler(XMLSchema schema)
  {
    this.schema = schema;
    typeInfoProvider = new XMLSchemaTypeInfoProvider(this);
    namespaceSupport = new NamespaceSupport();
    context = new LinkedList();
    attributes = new ArrayList();
    final String ns = XMLConstants.W3C_XML_SCHEMA_NS_URI;
    typeLibrary = new DatatypeLibraryLoader().createDatatypeLibrary(ns);
  }

  public ContentHandler getContentHandler()
  {
    return contentHandler;
  }

  public void setContentHandler(ContentHandler contentHandler)
  {
    this.contentHandler = contentHandler;
  }

  public ErrorHandler getErrorHandler()
  {
    return errorHandler;
  }

  public void setErrorHandler(ErrorHandler errorHandler)
  {
    this.errorHandler = errorHandler;
  }

  public LSResourceResolver getResourceResolver()
  {
    return resourceResolver;
  }

  public void setResourceResolver(LSResourceResolver resourceResolver)
  {
    this.resourceResolver = resourceResolver;
  }

  public TypeInfoProvider getTypeInfoProvider()
  {
    return typeInfoProvider;
  }

  TypeInfo getElementTypeInfo()
  {
    return (XMLSchemaElementTypeInfo) context.getFirst();
  }

  TypeInfo getAttributeTypeInfo(int index)
  {
    return (XMLSchemaAttributeTypeInfo) attributes.get(index);
  }

  boolean isIdAttribute(int index)
  {
    XMLSchemaAttributeTypeInfo typeInfo =
      (XMLSchemaAttributeTypeInfo) attributes.get(index);
    return typeInfo.id;
  }

  boolean isSpecified(int index)
  {
    XMLSchemaAttributeTypeInfo typeInfo =
      (XMLSchemaAttributeTypeInfo) attributes.get(index);
    return typeInfo.specified;
  }

  public void setDocumentLocator(Locator locator)
  {
    loc = locator;
    if (contentHandler != null)
      {
        contentHandler.setDocumentLocator(locator);
      }
  }

  public void startDocument()
    throws SAXException
  {
    namespaceSupport.reset();
    context.clear();
    attributes.clear();
    if (contentHandler != null)
      {
        contentHandler.startDocument();
      }
  }

  public void endDocument()
    throws SAXException
  {
    if (contentHandler != null)
      {
        contentHandler.endDocument();
      }
  }

  public void startPrefixMapping(String prefix, String uri)
    throws SAXException
  {
    namespaceSupport.declarePrefix(prefix, uri);
    if (contentHandler != null)
      {
        contentHandler.startPrefixMapping(prefix, uri);
      }
  }

  public void endPrefixMapping(String prefix)
    throws SAXException
  {
    if (contentHandler != null)
      {
        contentHandler.endPrefixMapping(prefix);
      }
  }

  public void startElement(String uri, String localName, String qName,
                           Attributes atts)
    throws SAXException
  {
    namespaceSupport.pushContext();
    QName name = new QName(uri, localName);
    ElementDeclaration decl =
      (ElementDeclaration) schema.elementDeclarations.get(name);
    // Validation Rule: Element Locally Valid (Element)
    String xsiType =
      atts.getValue(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "type");
    xsiType = xsiType.trim(); // normalise
    String xsiNil =
      atts.getValue(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil");
    Type type = decl.datatype;
    if (xsiType.length() > 0)
      {
        try
          {
            Type specifiedType = resolveType(xsiType);
            //  TODO 4.3
            type = specifiedType;
          }
        catch (DatatypeException e) // 4.1, 4.2
          {
            ValidationException e2 =
              new ValidationException("Can't resolve type " + xsiType,
                                      loc);
            e2.initCause(e);
            throw e2;
          }
      }
    XMLSchemaElementTypeInfo typeInfo =
      new XMLSchemaElementTypeInfo(schema, decl, type);
    if (decl == null) // 1
      {
        throw new ValidationException("No declaration for " + name, loc);
      }
    if (decl.isAbstract) // 2
      {
        throw new ValidationException("Declaration for " + name +
                                      " is abstract", loc);
      }
    if (xsiNil.length() > 0)
      {
        if (!decl.nillable) // 3.1
          {
            throw new ValidationException("Declaration for " + name +
                                          " is nillable but xsi:nil present",
                                          loc);
          }
        else if ("true".equals(xsiNil)) // 3.2
          {
            typeInfo.nil = true;
            if (decl.type == XMLSchema.CONSTRAINT_FIXED) // 3.2.2
              {
                throw new ValidationException("Declaration for " + name +
                                              " is fixed but xsi:nil is true",
                                              loc);
              }
          }
      }
    // TODO 5, 6, 7
    
    // parent
    if (!context.isEmpty())
      {
        XMLSchemaElementTypeInfo parent =
          (XMLSchemaElementTypeInfo) context.getFirst();
        if (parent.nil) // Element Locally Valid (Element) 3.2.1
          {
            throw new ValidationException("Parent of " + qName +
                                          " is declared xsi:nil", loc);
          }
        // TODO
      }
    context.addFirst(typeInfo);
    // attributes
    int len = atts.getLength();
    Attributes2Impl atts2 = new Attributes2Impl();
    int count = 0;
    for (int i = 0; i < len; i++)
      {
        String attUri = atts.getURI(i);
        String attLocalName = atts.getLocalName(i);
        String attQName = atts.getQName(i);
        String attValue = atts.getValue(i);

        if (XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI.equals(attUri))
          {
            continue; // ?
          }

        QName attName = new QName(attUri, attLocalName);
        AttributeDeclaration attDecl =
          (AttributeDeclaration) schema.attributeDeclarations.get(attName);
        boolean declared = (attDecl != null);

        String attType = (attDecl != null) ?
          attDecl.datatype.toString() : "CDATA";
        XMLSchemaAttributeTypeInfo attTypeInfo =
          new XMLSchemaAttributeTypeInfo(schema, attDecl, true);
        attributes.add(attTypeInfo);
        
        atts2.addAttribute(attUri, attLocalName, attQName, attType, attValue);
        atts2.setDeclared(count, declared);
        atts2.setSpecified(count, true);
        count++;
      }
    // add defaulted attributes to atts2
    // TODO
    // atts2.setSpecified(count, false);
    if (contentHandler != null)
      {
        contentHandler.startElement(uri, localName, qName, atts2);
      }
  }

  public void endElement(String uri, String localName, String qName)
    throws SAXException
  {
    // TODO check all required have been seen
    context.removeFirst();
    attributes.clear();
    namespaceSupport.popContext();
    if (contentHandler != null)
      {
        contentHandler.endElement(uri, localName, qName);
      }
  }

  public void characters(char[] ch, int start, int length)
    throws SAXException
  {
    XMLSchemaElementTypeInfo parent =
      (XMLSchemaElementTypeInfo) context.getFirst();
    if (parent.nil) // Element Locally Valid (Element) 3.2.1
      {
        throw new ValidationException(parent.decl.name.toString() +
                                      " is declared xsi:nil",
                                      loc);
      }
    // TODO
    if (contentHandler != null)
      {
        contentHandler.characters(ch, start, length);
      }
  }

  public void ignorableWhitespace(char[] ch, int start, int length)
    throws SAXException
  {
    if (contentHandler != null)
      {
        contentHandler.ignorableWhitespace(ch, start, length);
      }
  }

  public void processingInstruction(String target, String data)
    throws SAXException
  {
    if (contentHandler != null)
      {
        contentHandler.processingInstruction(target, data);
      }
  }
  
  public void skippedEntity(String name)
    throws SAXException
  {
    if (contentHandler != null)
      {
        contentHandler.skippedEntity(name);
      }
  }

  Type resolveType(String value)
    throws DatatypeException
  {
    QName name = QName.valueOf(value);
    String prefix = name.getPrefix();
    String localName = name.getLocalPart();
    if (prefix != null && prefix.length() > 0)
      {
        String uri = namespaceSupport.getURI(prefix);
        if (!XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri))
          return null;
      }
    if ("anyType".equals(localName))
      return Type.ANY_TYPE;
    return (SimpleType) typeLibrary.createDatatype(localName);
  }
  
}


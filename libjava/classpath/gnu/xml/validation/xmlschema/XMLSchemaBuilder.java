/* XMLSchemaBuilder.java -- 
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

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.DatatypeLibrary;
import org.relaxng.datatype.helpers.DatatypeLibraryLoader;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import gnu.xml.validation.datatype.Annotation;
import gnu.xml.validation.datatype.AtomicSimpleType;
import gnu.xml.validation.datatype.ListSimpleType;
import gnu.xml.validation.datatype.SimpleType;
import gnu.xml.validation.datatype.Type;
import gnu.xml.validation.datatype.UnionSimpleType;

/**
 * Parses an XML Schema DOM tree, constructing a compiled internal
 * representation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class XMLSchemaBuilder
{

  XMLSchema schema;
  final DatatypeLibrary typeLibrary;

  XMLSchemaBuilder()
  {
    final String ns = XMLConstants.W3C_XML_SCHEMA_NS_URI;
    typeLibrary = new DatatypeLibraryLoader().createDatatypeLibrary(ns);
  }

  void parseSchema(Node node)
    throws DatatypeException
  {
    String uri = node.getNamespaceURI();
    String name = node.getLocalName();
    if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
        node.getNodeType() == Node.ELEMENT_NODE)
      {
        if ("schema".equals(name))
          {
            NamedNodeMap attrs = node.getAttributes();
            String targetNamespace = getAttribute(attrs, "targetNamespace");
            String version = getAttribute(attrs, "version");
            String fd = getAttribute(attrs, "finalDefault");
            int finalDefault = parseFullDerivationSet(fd);
            String bd = getAttribute(attrs, "blockDefault");
            int blockDefault = parseBlockSet(bd);
            String afd = getAttribute(attrs, "attributeFormDefault");
            boolean attributeFormQualified = "qualified".equals(afd);
            String efd = getAttribute(attrs, "elementFormDefault");
            boolean elementFormQualified = "qualified".equals(efd);
            schema = new XMLSchema(targetNamespace, version,
                                   finalDefault, blockDefault,
                                   attributeFormQualified,
                                   elementFormQualified);
            for (Node child = node.getFirstChild(); child != null;
                 child = child.getNextSibling())
              {
                parseTopLevelElement(child);
              }
            return;
          }
      }
    // TODO throw schema exception
  }

  void parseTopLevelElement(Node node)
    throws DatatypeException
  {
    String uri = node.getNamespaceURI();
    String name = node.getLocalName();
    if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
        node.getNodeType() == Node.ELEMENT_NODE)
      {
        if ("element".equals(name))
          {
            ElementDeclaration ed =
              (ElementDeclaration) parseElement(node, null);
            schema.elementDeclarations.put(ed.name, ed);
            // TODO
          }
        else if ("attribute".equals(name))
          {
            AttributeDeclaration ad =
              (AttributeDeclaration) parseAttribute(node, true);
            schema.attributeDeclarations.put(ad.name, ad);
            // TODO
          }
        else if ("type".equals(name))
          {
            // TODO
          }
        else if ("group".equals(name))
          {
            // TODO
          }
        else if ("attributeGroup".equals(name))
          {
            // TODO
          }
        else if ("notation".equals(name))
          {
            // TODO
          }
        else if ("identityConstraint".equals(name))
          {
            // TODO
          }
      }
    // TODO throw schema exception
  }

  Object parseAttribute(Node node, boolean scope)
    throws DatatypeException
  {
    NamedNodeMap attrs = node.getAttributes();
    String def = getAttribute(attrs, "default");
    String fixed = getAttribute(attrs, "fixed");
    int constraintType = AttributeDeclaration.NONE;
    String constraintValue = null;
    if (def != null)
      {
        constraintType = AttributeDeclaration.DEFAULT;
        constraintValue = def;
      }
    else if (fixed != null)
      {
        constraintType = AttributeDeclaration.FIXED;
        constraintValue = fixed;
      }
    // TODO form = (qualified | unqualified)
    String attrName = getAttribute(attrs, "name");
    String attrNamespace = getAttribute(attrs, "targetNamespace");
    String ref = getAttribute(attrs, "ref");
    String use = getAttribute(attrs, "use");
    String type = getAttribute(attrs, "type");
    SimpleType datatype = (type == null) ? null :
      parseSimpleType(asQName(type, node));
    Annotation annotation = null;
    for (Node child = node.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                annotation = parseAnnotation(child);
              }
            else if ("simpleType".equals(name))
              {
                datatype = parseSimpleType(child);
              }
            else
              {
                // TODO throw schema exception
              }
          }
      }
    if (scope)
      {
        return new AttributeDeclaration(scope,
                                        constraintType,
                                        constraintValue,
                                        new QName(attrNamespace, attrName),
                                        datatype,
                                        annotation);
      }
    else 
      {
        boolean required = "required".equals(use);
        // TODO ref
        AttributeDeclaration decl = (ref == null) ?
              new AttributeDeclaration(scope,
                                       AttributeDeclaration.NONE,
                                       null,
                                       new QName(attrNamespace, attrName),
                                       datatype,
                                       annotation) :
              /*schema.getAttributeDeclaration(ref)*/ null;
        return new AttributeUse(required,
                                constraintType,
                                constraintValue,
                                decl);
      }
  }

  int parseFullDerivationSet(String value)
  {
    int ret = XMLSchema.FINAL_NONE;
    if ("#all".equals(value))
      {
        ret = XMLSchema.FINAL_ALL;
      }
    else
      {
        StringTokenizer st = new StringTokenizer(value, " ");
        while (st.hasMoreTokens())
          {
            String token = st.nextToken();
            if ("extension".equals(token))
              {
                ret |= XMLSchema.FINAL_EXTENSION;
              }
            else if ("restriction".equals(token))
              {
                ret |= XMLSchema.FINAL_RESTRICTION;
              }
            else if ("list".equals(token))
              {
                ret |= XMLSchema.FINAL_LIST;
              }
            else if ("union".equals(token))
              {
                ret |= XMLSchema.FINAL_UNION;
              }
          }
      }
    return ret;
  }

  int parseSimpleTypeDerivationSet(String value)
  {
    int ret = XMLSchema.FINAL_NONE;
    if ("#all".equals(value))
      {
        ret = XMLSchema.FINAL_LIST |
          XMLSchema.FINAL_UNION |
          XMLSchema.FINAL_RESTRICTION;
      }
    else
      {
        StringTokenizer st = new StringTokenizer(value, " ");
        while (st.hasMoreTokens())
          {
            String token = st.nextToken();
            if ("list".equals(token))
              {
                ret |= XMLSchema.FINAL_LIST;
              }
            else if ("union".equals(token))
              {
                ret |= XMLSchema.FINAL_UNION;
              }
            else if ("restriction".equals(token))
              {
                ret |= XMLSchema.FINAL_RESTRICTION;
              }
          }
      }
    return ret;
  }

  int parseComplexTypeDerivationSet(String value)
  {
    int ret = XMLSchema.FINAL_NONE;
    if ("#all".equals(value))
      {
        ret = XMLSchema.FINAL_EXTENSION | XMLSchema.FINAL_RESTRICTION;
      }
    else
      {
        StringTokenizer st = new StringTokenizer(value, " ");
        while (st.hasMoreTokens())
          {
            String token = st.nextToken();
            if ("extension".equals(token))
              {
                ret |= XMLSchema.FINAL_EXTENSION;
              }
            else if ("restriction".equals(token))
              {
                ret |= XMLSchema.FINAL_RESTRICTION;
              }
          }
      }
    return ret;
  }

  int parseBlockSet(String value)
  {
    int ret = XMLSchema.BLOCK_NONE;
    if ("#all".equals(value))
      {
        ret = XMLSchema.BLOCK_ALL;
      }
    else
      {
        StringTokenizer st = new StringTokenizer(value, " ");
        while (st.hasMoreTokens())
          {
            String token = st.nextToken();
            if ("extension".equals(token))
              {
                ret |= XMLSchema.BLOCK_EXTENSION;
              }
            else if ("restriction".equals(token))
              {
                ret |= XMLSchema.BLOCK_RESTRICTION;
              }
            else if ("substitution".equals(token))
              {
                ret |= XMLSchema.BLOCK_SUBSTITUTION;
              }
          }
      }
    return ret;
  }

  int parseComplexTypeBlockSet(String value)
  {
    int ret = XMLSchema.BLOCK_NONE;
    if ("#all".equals(value))
      {
        ret = XMLSchema.BLOCK_EXTENSION | XMLSchema.BLOCK_RESTRICTION;
      }
    else
      {
        StringTokenizer st = new StringTokenizer(value, " ");
        while (st.hasMoreTokens())
          {
            String token = st.nextToken();
            if ("extension".equals(token))
              {
                ret |= XMLSchema.BLOCK_EXTENSION;
              }
            else if ("restriction".equals(token))
              {
                ret |= XMLSchema.BLOCK_RESTRICTION;
              }
          }
      }
    return ret;
  }

  Object parseElement(Node node, ElementDeclaration parent)
    throws DatatypeException
  {
    NamedNodeMap attrs = node.getAttributes();
    Integer minOccurs = null;
    Integer maxOccurs = null;
    Node parentNode = node.getParentNode();
    boolean notTopLevel = !"schema".equals(parentNode.getLocalName());
    if (notTopLevel)
      {
        String ref = getAttribute(attrs, "ref");
        if (ref != null)
          {
            minOccurs = getOccurrence(getAttribute(attrs, "minOccurs"));
            maxOccurs = getOccurrence(getAttribute(attrs, "maxOccurs"));
            // TODO resolve top-level element declaration
            ElementDeclaration ad = null;
            return new Particle(minOccurs, maxOccurs, ad);
          }
      }
    String elementName = getAttribute(attrs, "name");
    String elementNamespace = getAttribute(attrs, "targetNamespace");
    String type = getAttribute(attrs, "type");
    Type datatype = (type != null) ?
      parseSimpleType(asQName(type, node)) : null;
    int scope = (parent == null) ?
      XMLSchema.GLOBAL :
      XMLSchema.LOCAL;
    String def = getAttribute(attrs, "default");
    String fixed = getAttribute(attrs, "fixed");
    int constraintType = AttributeDeclaration.NONE;
    String constraintValue = null;
    if (def != null)
      {
        constraintType = AttributeDeclaration.DEFAULT;
        constraintValue = def;
      }
    else if (fixed != null)
      {
        constraintType = AttributeDeclaration.FIXED;
        constraintValue = fixed;
      }
    String sg = getAttribute(attrs, "substitutionGroup");
    QName substitutionGroup = QName.valueOf(sg);
    String sgPrefix = substitutionGroup.getPrefix();
    if (sgPrefix != null && !"".equals(sgPrefix))
      {
        String sgName = substitutionGroup.getLocalPart();
        String sgNamespace = node.lookupNamespaceURI(sgPrefix);
        substitutionGroup = new QName(sgNamespace, sgName);
      }
    
    String block = getAttribute(attrs, "block");
    int substitutionGroupExclusions = (block == null) ?
      schema.blockDefault :
      parseBlockSet(block);
    String final_ = getAttribute(attrs, "final");
    int disallowedSubstitutions = (final_ == null) ?
      schema.finalDefault :
      parseFullDerivationSet(final_);
    
    boolean nillable = "true".equals(getAttribute(attrs, "nillable"));
    boolean isAbstract = "true".equals(getAttribute(attrs, "abstract"));
    
    if (notTopLevel)
      {
        minOccurs = getOccurrence(getAttribute(attrs, "minOccurs"));
        maxOccurs = getOccurrence(getAttribute(attrs, "maxOccurs"));
        String form = getAttribute(attrs, "form");
        if (form != null)
          {
            if ("qualified".equals(form))
              {
                elementNamespace = schema.targetNamespace;
              }
          }
        else if (schema.elementFormQualified)
          {
            elementNamespace = schema.targetNamespace;
          }
      }
    ElementDeclaration ed =
      new ElementDeclaration(new QName(elementNamespace, elementName),
                             datatype,
                             scope, parent,
                             constraintType, constraintValue,
                             nillable,
                             substitutionGroup, 
                             substitutionGroupExclusions, 
                             disallowedSubstitutions,
                             isAbstract);
    
    for (Node child = node.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                ed.annotation = parseAnnotation(child);
              }
            else if ("simpleType".equals(name) && datatype == null)
              {
                ed.datatype = parseSimpleType(child);
              }
            else if ("complexType".equals(name) && datatype == null)
              {
                ed.datatype = parseComplexType(child, ed);
              }
            else
              {
                // throw schema exception
              }
          }
      }

    if (notTopLevel)
      {
        return new Particle(minOccurs, maxOccurs, ed);
      }
    else
      {
        return ed;
      }
  }

  Integer getOccurrence(String value)
  {
    if (value == null)
      {
        return new Integer(1);
      }
    else if ("unbounded".equals(value))
      {
        return null;
      }
    else
      {
        return new Integer(value);
      }
  }

  SimpleType parseSimpleType(QName typeName)
    throws DatatypeException
  {
    SimpleType type = (SimpleType) schema.types.get(typeName);
    if (!XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(typeName.getNamespaceURI()))
      return null;
    String localName = typeName.getLocalPart();
    return (SimpleType) typeLibrary.createDatatype(localName);
  }

  SimpleType parseSimpleType(Node simpleType)
    throws DatatypeException
  {
    NamedNodeMap attrs = simpleType.getAttributes();
    String typeFinal = getAttribute(attrs, "final");
    if (typeFinal == null)
      {
        Node schema = simpleType.getParentNode();
        while (schema != null && !"schema".equals(schema.getLocalName()))
          {
            schema = schema.getParentNode();
          }
        if (schema != null)
          {
            NamedNodeMap schemaAttrs = schema.getAttributes();
            typeFinal = getAttribute(schemaAttrs, "finalDefault");
          }
      }
    int typeFinality = parseSimpleTypeDerivationSet(typeFinal);
    QName typeName = asQName(getAttribute(attrs, "name"), simpleType);
    int variety = 0;
    Set facets = new LinkedHashSet();
    int fundamentalFacets = 0; // TODO
    SimpleType baseType = null; // TODO
    Annotation annotation = null;
    // TODO use DatatypeBuilder
    for (Node child = simpleType.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                annotation = parseAnnotation(child);
              }
            else if ("restriction".equals(name))
              {
                // TODO
              }
            else if ("list".equals(name))
              {
                variety = SimpleType.LIST;
                // TODO
              }
            else if ("union".equals(name))
              {
                variety = SimpleType.UNION;
                // TODO
              }
          }
      }
    return new SimpleType(typeName, variety, facets, fundamentalFacets,
                          baseType, annotation);
  }

  Type parseComplexType(Node complexType, ElementDeclaration parent)
    throws DatatypeException
  {
    NamedNodeMap attrs = complexType.getAttributes();
    QName typeName = asQName(getAttribute(attrs, "name"), complexType);
    boolean isAbstract = "true".equals(getAttribute(attrs, "abstract"));
    String block = getAttribute(attrs, "block");
    int prohibitedSubstitutions = (block == null) ?
      schema.blockDefault :
      parseComplexTypeBlockSet(block);
    String final_ = getAttribute(attrs, "final");
    int finality = (final_ == null) ?
      schema.finalDefault :
      parseComplexTypeDerivationSet(final_);
    ComplexType type = new ComplexType(typeName, isAbstract,
                                       prohibitedSubstitutions, finality);
    boolean mixed = "true".equals(getAttribute(attrs, "mixed"));
    for (Node child = complexType.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("simpleContent".equals(name))
              {
                parseSimpleContent(child, type);
              }
          }
      }
    if (mixed)
      {
        type.contentType = XMLSchema.CONTENT_MIXED;
      } 
    return type;
  }

  void parseSimpleContent(Node simpleContent, ComplexType type)
    throws DatatypeException
  {
    for (Node child = simpleContent.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                type.annotations.add(parseAnnotation(child));
              }
            else if ("restriction".equals(name))
              {
                type.derivationMethod = XMLSchema.FINAL_RESTRICTION;
                parseRestriction(child, type);
              }
            else if ("extension".equals(name))
              {
                type.derivationMethod = XMLSchema.FINAL_EXTENSION;
                parseExtension(child, type);
              }
          }
      }
  }

  void parseRestriction(Node restriction, ComplexType type)
    throws DatatypeException
  {
    NamedNodeMap attrs = restriction.getAttributes();
    String base = getAttribute(attrs, "base");
    QName baseType = asQName(base, restriction);
    SimpleType simpleType = null;
    for (Node child = restriction.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                type.annotations.add(parseAnnotation(child));
              }
            else if ("simpleType".equals(name))
              {
                type.contentType = XMLSchema.CONTENT_SIMPLE;
                simpleType = parseSimpleType(child);
              }
            else if ("minExclusive".equals(name))
              {
              }
            else if ("minInclusive".equals(name))
              {
              }
            else if ("maxExclusive".equals(name))
              {
              }
            else if ("maxInclusive".equals(name))
              {
              }
            else if ("totalDigits".equals(name))
              {
              }
            else if ("fractionDigits".equals(name))
              {
              }
            else if ("length".equals(name))
              {
              }
            else if ("minLength".equals(name))
              {
              }
            else if ("maxLength".equals(name))
              {
              }
            else if ("enumeration".equals(name))
              {
              }
            else if ("whiteSpace".equals(name))
              {
              }
            else if ("pattern".equals(name))
              {
              }
            else if ("attribute".equals(name))
              {
                AttributeUse use =
                  (AttributeUse) parseAttribute(child, false);
                schema.attributeDeclarations.put(use.declaration.name,
                                                 use.declaration);
                type.attributeUses.add(use);
              }
            else if ("attributeGroup".equals(name))
              {
                NamedNodeMap agAttrs = child.getAttributes();
                String ref = getAttribute(agAttrs, "ref");
                QName ag = asQName(ref, child);
                type.attributeUses.add(ag);
              }
            else if ("anyAttribute".equals(name))
              {
                type.attributeWildcard = parseAnyAttribute(child);
              }
          }
      }
  }

  void parseExtension(Node extension, ComplexType type)
    throws DatatypeException
  {
    NamedNodeMap attrs = extension.getAttributes();
    String base = getAttribute(attrs, "base");
    QName baseType = asQName(base, extension);
    for (Node child = extension.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                type.annotations.add(parseAnnotation(child));
              }
            else if ("attribute".equals(name))
              {
                AttributeUse use =
                  (AttributeUse) parseAttribute(child, false);
                schema.attributeDeclarations.put(use.declaration.name,
                                                 use.declaration);
                type.attributeUses.add(use);
              }
            else if ("attributeGroup".equals(name))
              {
                NamedNodeMap agAttrs = child.getAttributes();
                String ref = getAttribute(agAttrs, "ref");
                QName ag = asQName(ref, child);
                type.attributeUses.add(ag);
              }
            else if ("anyAttribute".equals(name))
              {
                type.attributeWildcard = parseAnyAttribute(child);
              }
          }
      }
  }

  AnyAttribute parseAnyAttribute(Node node)
  {
    NamedNodeMap attrs = node.getAttributes();
    String namespace = getAttribute(attrs, "namespace");
    String pc = getAttribute(attrs, "processContents");
    int processContents = AnyAttribute.STRICT;
    if ("lax".equals(pc))
      {
        processContents = AnyAttribute.LAX;
      }
    else if ("skip".equals(pc))
      {
        processContents = AnyAttribute.SKIP;
      }
    AnyAttribute ret = new AnyAttribute(namespace, processContents);
    for (Node child = node.getFirstChild(); child != null;
         child = child.getNextSibling())
      {
        String uri = child.getNamespaceURI();
        String name = child.getLocalName();
        if (XMLConstants.W3C_XML_SCHEMA_NS_URI.equals(uri) &&
            child.getNodeType() == Node.ELEMENT_NODE)
          {
            if ("annotation".equals(name))
              {
                ret.annotation = parseAnnotation(child);
              }
          }
      }
    return ret;
  }

  Annotation parseAnnotation(Node node)
  {
    // TODO
    return null;
  }

  private static String getAttribute(NamedNodeMap attrs, String name)
  {
    Node attr = attrs.getNamedItem(name);
    return (attr == null) ? null : attr.getNodeValue();
  }

  private static QName asQName(String text, Node resolver)
  {
    QName name = QName.valueOf(text);
    String prefix = name.getPrefix();
    if (prefix != null && prefix.length() > 0)
      {
        String uri = resolver.lookupNamespaceURI(prefix);
        name = new QName(uri, name.getLocalPart());
      }
    return name;
  }
  
}


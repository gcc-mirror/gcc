/* FullSyntaxBuilder.java -- 
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

package gnu.xml.validation.relaxng;

import java.io.InputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.DatatypeLibrary;
import org.relaxng.datatype.helpers.DatatypeLibraryLoader;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import gnu.xml.stream.XMLParser;

/**
 * Parses a RELAX NG XML DOM tree, constructing a compiled internal
 * representation.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class FullSyntaxBuilder
{

  /**
   * Complete vocabulary (elements and attributes) of the full syntax.
   */
  static final Map VOCABULARY = new HashMap();
  static final Set STRIPPED_ATTRIBUTES = new HashSet();
  static final Set PATTERN_ELEMENTS = new HashSet();
  static
  {
    Set elementAttrs = Collections.singleton("name");
    Set dataAttrs = new HashSet();
    dataAttrs.add("type");
    dataAttrs.add("datatypeLibrary");
    Set valueAttrs = new HashSet();
    valueAttrs.add("type");
    valueAttrs.add("datatypeLibrary");
    valueAttrs.add("ns");
    Set externalAttrs = Collections.singleton("href");
    Set startAttrs = Collections.singleton("combine");
    Set defineAttrs = new HashSet();
    defineAttrs.add("name");
    defineAttrs.add("combine");
    Set nsAttrs = Collections.singleton("ns");
    
    VOCABULARY.put("element", elementAttrs);
    VOCABULARY.put("attribute", elementAttrs);
    VOCABULARY.put("group", Collections.EMPTY_SET);
    VOCABULARY.put("interleave", Collections.EMPTY_SET);
    VOCABULARY.put("choice", Collections.EMPTY_SET);
    VOCABULARY.put("optional", Collections.EMPTY_SET);
    VOCABULARY.put("zeroOrMore", Collections.EMPTY_SET);
    VOCABULARY.put("oneOrMore", Collections.EMPTY_SET);
    VOCABULARY.put("list", Collections.EMPTY_SET);
    VOCABULARY.put("mixed", Collections.EMPTY_SET);
    VOCABULARY.put("ref", elementAttrs);
    VOCABULARY.put("parentRef", elementAttrs);
    VOCABULARY.put("empty", Collections.EMPTY_SET);
    VOCABULARY.put("text", Collections.EMPTY_SET);
    VOCABULARY.put("value", valueAttrs);
    VOCABULARY.put("data", dataAttrs);
    VOCABULARY.put("notAllowed", Collections.EMPTY_SET);
    VOCABULARY.put("externalRef", externalAttrs);
    VOCABULARY.put("grammar", Collections.EMPTY_SET);
    VOCABULARY.put("param", elementAttrs);
    VOCABULARY.put("except", Collections.EMPTY_SET);
    VOCABULARY.put("div", Collections.EMPTY_SET);
    VOCABULARY.put("include", externalAttrs);
    VOCABULARY.put("start", startAttrs);
    VOCABULARY.put("define", defineAttrs);
    VOCABULARY.put("name", nsAttrs);
    VOCABULARY.put("anyName", Collections.EMPTY_SET);
    VOCABULARY.put("nsName", nsAttrs);

    STRIPPED_ATTRIBUTES.add("name");
    STRIPPED_ATTRIBUTES.add("type");
    STRIPPED_ATTRIBUTES.add("combine");

    PATTERN_ELEMENTS.add("element");
    PATTERN_ELEMENTS.add("attribute");
    PATTERN_ELEMENTS.add("group");
    PATTERN_ELEMENTS.add("interleave");
    PATTERN_ELEMENTS.add("choice");
    PATTERN_ELEMENTS.add("optional");
    PATTERN_ELEMENTS.add("zeroOrMore");
    PATTERN_ELEMENTS.add("oneOrMore");
    PATTERN_ELEMENTS.add("list");
    PATTERN_ELEMENTS.add("mixed");
    PATTERN_ELEMENTS.add("ref");
    PATTERN_ELEMENTS.add("parentRef");
    PATTERN_ELEMENTS.add("empty");
    PATTERN_ELEMENTS.add("text");
    PATTERN_ELEMENTS.add("value");
    PATTERN_ELEMENTS.add("data");
    PATTERN_ELEMENTS.add("notAllowed");
    PATTERN_ELEMENTS.add("externalRef");
    PATTERN_ELEMENTS.add("grammar");
  }

  private Set urls; // recursion checking
  private int refCount; // creation of ref names
  private Map datatypeLibraries;

  /**
   * Parse the specified document into a grammar.
   */
  synchronized Grammar parse(Document doc)
    throws IOException
  {
    urls = new HashSet();
    refCount = 1;
    
    doc.normalizeDocument(); // Normalize XML document
    transform(doc); // Apply transformation rules to provide simple syntax
    
    // 4.18. grammar element
    Element p = doc.getDocumentElement();
    Element grammar =
      doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "grammar");
    Element start =
      doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "start");
    doc.removeChild(p);
    doc.appendChild(grammar);
    grammar.appendChild(start);
    start.appendChild(p);
    transformGrammar(grammar, p);
    Element define = getNextSiblingElement(start);
    while (define != null)
      {
        Element next = getNextSiblingElement(define);
        String name = define.getAttribute("new-name");
        if (name != null)
          {
            define.setAttribute("name", name);
            define.removeAttribute("new-name");
          }
        else
          grammar.removeChild(define); // unreferenced
        define = next;
      }

    // 4.19. define and ref elements
    Set allDefines = new HashSet(), reachableDefines = new HashSet();
    getDefines(allDefines, grammar, grammar, false);
    getDefines(reachableDefines, grammar, start, true);
    allDefines.removeAll(reachableDefines);
    for (Iterator i = allDefines.iterator(); i.hasNext(); )
      {
        // remove unreachable defines
        Element d = (Element) i.next();
        Node parent = d.getParentNode();
        parent.removeChild(d);
      }
    // replace all elements that are not children of defines by refs to new
    // defines
    Set elements = new HashSet();
    getElements(elements, grammar, grammar);
    for (Iterator i = elements.iterator(); i.hasNext(); )
      {
        Element element = (Element) i.next();
        Node parent = element.getParentNode();
        if (!reachableDefines.contains(parent))
          {
            define =
              doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "define");
            Element ref =
              doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "ref");
            String name = createRefName();
            define.setAttribute("name", name);
            ref.setAttribute("name", name);
            parent.insertBefore(ref, element);
            define.appendChild(element);
            grammar.appendChild(define);
            reachableDefines.add(define);
          }
      }
    // Get defines that don't have element children
    for (Iterator i = reachableDefines.iterator(); i.hasNext(); )
      {
        Element d = (Element) i.next();
        Element child = getFirstChildElement(d);
        if (child != null && "element".equals(child.getLocalName()))
          i.remove();
      }
    // Expand refs that refer to these defines
    expandRefs(reachableDefines, grammar);
    // Remove any defines that don't have element children
    for (Iterator i = reachableDefines.iterator(); i.hasNext(); )
      {
        Element d = (Element) i.next();
        Node parent = d.getParentNode();
        parent.removeChild(d);
      }

    transform2(p); // Apply second stage transformation rules
    
    Grammar ret = parseGrammar(grammar);
    datatypeLibraries = null; // free datatype libraries cache
    return ret;
  }

  private void getDefines(Set defines, Element grammar, Element node,
                          boolean followRefs)
  {
    String elementName = node.getLocalName();
    if ("define".equals(elementName))
      defines.add(node);
    else if ("ref".equals(elementName) && followRefs)
      {
        String rname = node.getAttribute("name");
        Element define = getFirstChildElement(grammar);
        define = getNextSiblingElement(define);
        while (define != null)
          {
            String dname = define.getAttribute("name");
            if (rname.equals(dname))
              {
                getDefines(defines, grammar, node, followRefs);
                break;
              }
            define = getNextSiblingElement(define);
          }
      }
    for (Element child = getFirstChildElement(node); child != null;
         child = getNextSiblingElement(child))
      getDefines(defines, grammar, child, followRefs);
  }

  private void getElements(Set elements, Element grammar, Element node)
  {
    String elementName = node.getLocalName();
    if ("element".equals(elementName))
      elements.add(node);
    for (Element child = getFirstChildElement(node); child != null;
         child = getNextSiblingElement(child))
      getElements(elements, grammar, child);
  }

  private void expandRefs(Set defines, Element node)
    throws GrammarException
  {
    String elementName = node.getLocalName();
    if ("ref".equals(elementName))
      {
        String rname = node.getAttribute("name");
        for (Iterator i = defines.iterator(); i.hasNext(); )
          {
            Element define = (Element) i.next();
            String dname = define.getAttribute("name");
            if (rname.equals(dname))
              {
                Element child = getFirstChildElement(define);
                forbidRefs(child, rname);
                Element refChild = (Element) child.cloneNode(true);
                Node parent = node.getParentNode();
                parent.insertBefore(refChild, node);
                parent.removeChild(node);
                node = refChild;
                break;
              }
          }
      }
    for (Element child = getFirstChildElement(node); child != null;
         child = getNextSiblingElement(child))
      expandRefs(defines, child);
  }

  private void forbidRefs(Element node, String name)
    throws GrammarException
  {
    String elementName = node.getLocalName();
    if ("ref".equals(elementName))
      {
        String rname = node.getAttribute("name");
        if (name.equals(rname))
          throw new GrammarException("cannot expand ref with name '" + name +
                                     "' due to circularity");
      }
    for (Element child = getFirstChildElement(node); child != null;
         child = getNextSiblingElement(child))
      forbidRefs(child, name);
  }

  private void transform(Node node)
    throws IOException
  {
    Node parent = node.getParentNode();
    switch (node.getNodeType())
      {
      case Node.ELEMENT_NODE:
        // 4.1 Annotations
        String elementNs = node.getNamespaceURI();
        String elementName = node.getLocalName();
        if (!XMLConstants.RELAXNG_NS_URI.equals(elementNs) ||
            !VOCABULARY.containsKey(elementName))
          parent.removeChild(node);
        else
          {
            Set allowedAttrs = (Set) VOCABULARY.get(elementName);
            NamedNodeMap attrs = node.getAttributes();
            int len = attrs.getLength();
            for (int i = len - 1; i >= 0; i--)
              {
                Node attr = attrs.item(i);
                String attrNs = attr.getNamespaceURI();
                String attrName = attr.getLocalName();
                if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(attrNs))
                  continue; // ignore namespace nodes
                if (!(XMLConstants.RELAXNG_NS_URI.equals(attrNs) ||
                      attrNs == null) ||
                    !allowedAttrs.contains(attrName))
                  attrs.removeNamedItemNS(attrNs, attrName);
                else
                  {
                    // 4.2 Whitespace
                    if (STRIPPED_ATTRIBUTES.contains(attrName))
                      attr.setNodeValue(attr.getNodeValue().trim());
                    // 4.3 datatypeLibrary attribute
                    else if ("datatypeLibrary".equals(attrName))
                      {
                        String dl = attr.getNodeValue();
                        attr.setNodeValue(escapeURL(dl));
                      }
                    // 4.5. href attribute
                    else if ("href".equals(attrName))
                      {
                        String href = attr.getNodeValue();
                        href = XMLParser.absolutize(node.getBaseURI(),
                                                    escapeURL(href));
                        attr.setNodeValue(href);
                      }
                  }
              }
            // 4.3 datatypeLibrary attribute
            if ("data".equals(elementName) || "value".equals(elementName))
              {
                Element element = (Element) node;
                String dl = element.getAttribute("datatypeLibrary");
                if (dl == null)
                  {
                    Node p = parent;
                    while (dl == null && p != null &&
                           p.getNodeType() == Node.ELEMENT_NODE)
                      {
                        dl = ((Element) p)
                          .getAttribute("datatypeLibrary");
                        p = p.getParentNode();
                      }
                    if (dl == null)
                      dl = "";
                    element.setAttribute("datatypeLibrary", dl);
                  }
                // 4.4. type attribute of value element
                if ("value".equals(elementName))
                  {
                    String type = element.getAttribute("type");
                    if (type == null)
                      {
                        element.setAttribute("type", "token");
                        element.setAttribute("datatypeLibrary", "");
                      }
                  }
                // 4.16. Constraints
                // TODO validate type
              }
            // 4.6. externalRef element
            else if ("externalRef".equals(elementName))
              {
                Element externalRef = (Element) node;
                String href = externalRef.getAttribute("href");
                // check for recursion
                if (urls.contains(href))
                  throw new GrammarException("recursive href");
                urls.add(href);
                Element element = resolve(href);
                String eNs = element.getNamespaceURI();
                String eName = element.getLocalName();
                if (!(XMLConstants.RELAXNG_NS_URI.equals(eNs) ||
                      eNs == null) ||
                    !PATTERN_ELEMENTS.contains(eName))
                  throw new GrammarException("externally referenced element " +
                                             "is not a pattern");
                transform(element);
                urls.remove(href);
                String ns = element.getAttribute("ns");
                if (ns != null)
                  element.setAttribute("ns",
                                       externalRef.getAttribute("ns"));
                element = (Element) externalRef.getOwnerDocument()
                  .importNode(element, true);
                parent.replaceChild(element, externalRef);
                return;
              }
            // 4.7 include element
            else if ("include".equals(elementName))
              {
                Element include = (Element) node;
                String href = include.getAttribute("href");
                // check for recursion
                if (urls.contains(href))
                  throw new GrammarException("recursive href");
                urls.add(href);
                Element element = resolve(href);
                String eNs = element.getNamespaceURI();
                String eName = element.getLocalName();
                if (!(XMLConstants.RELAXNG_NS_URI.equals(eNs) ||
                      eNs == null) ||
                    !"grammar".equals(eName))
                  throw new GrammarException("included element is not " +
                                             "a grammar");
                
                transform(element);
                urls.remove(href);
                // handle components
                List includeComponents = getComponents(include);
                List grammarComponents = getComponents(element);
                for (Iterator i = includeComponents.iterator(); i.hasNext(); )
                  {
                    Element comp = (Element) i.next();
                    String compName = comp.getLocalName();
                    if ("start".equals(compName))
                      {
                        boolean found = false;
                        for (Iterator j = grammarComponents.iterator();
                             j.hasNext(); )
                          {
                            Element c2 = (Element) j.next();
                            if ("start".equals(c2.getLocalName()))
                              {
                                c2.getParentNode().removeChild(c2);
                                found = true;
                              }
                          }
                        if (!found)
                          throw new GrammarException("no start component in " +
                                                     "included grammar");
                      }
                    else if ("define".equals(compName))
                      {
                        String name = comp.getAttribute("name");
                        boolean found = false;
                        for (Iterator j = grammarComponents.iterator();
                             j.hasNext(); )
                          {
                            Element c2 = (Element) j.next();
                            if ("define".equals(c2.getLocalName()) &&
                                name.equals(c2.getAttribute("name")))
                              {
                                c2.getParentNode().removeChild(c2);
                                found = true;
                              }
                          }
                        if (!found)
                          throw new GrammarException("no define component " +
                                                     "with name '" + name +
                                                     "' in included grammar");
                      }
                  }
                // transform to div element
                Document doc = include.getOwnerDocument();
                Element includeDiv = 
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "div");
                Element grammarDiv = 
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "div");
                // XXX copy include non-href attributes (none defined?)
                element = (Element) doc.importNode(element, true);
                Node ctx = element.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    grammarDiv.appendChild(ctx);
                    ctx = next;
                  }
                includeDiv.appendChild(grammarDiv);
                ctx = include.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    includeDiv.appendChild(ctx);
                    ctx = next;
                  }
                parent.replaceChild(includeDiv, include);
                transform(includeDiv);
                return;
              }
            // 4.8. name attribute of element and attribute elements
            else if ("attribute".equals(elementName) ||
                     "element".equals(elementName))
              {
                Element element = (Element) node;
                String name = element.getAttribute("name");
                if (name != null)
                  {
                    Document doc = element.getOwnerDocument();
                    Element n =
                      doc.createElementNS(XMLConstants.RELAXNG_NS_URI, "name");
                    n.appendChild(doc.createTextNode(name));
                    Node first = element.getFirstChild();
                    if (first != null)
                      element.insertBefore(n, first);
                    else
                      element.appendChild(n);
                    if ("attribute".equals(elementName))
                      {
                        String ns = element.getAttribute("ns");
                        if (ns != null)
                          {
                            n.setAttribute("ns", ns);
                            element.removeAttribute("ns");
                          }
                      }
                    element.removeAttribute("name");
                  }
                // 4.12. Number of child elements
                if ("attribute".equals(elementName))
                  {
                    if (getComponents(node).size() == 1)
                      {
                        Document doc = node.getOwnerDocument();
                        Element text =
                          doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                              "text");
                        node.appendChild(text);
                      }
                  }
                else // element
                  {
                    if (node.getChildNodes().getLength() > 2)
                      {
                        // transform to 2 child elements
                        Document doc = node.getOwnerDocument();
                        Element child =
                          doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                              "group");
                        Node ctx = getFirstChildElement(node);
                        ctx = getNextSiblingElement(ctx); // skip 1
                        while (ctx != null)
                          {
                            Node next = getNextSiblingElement(ctx);
                            child.appendChild(ctx);
                            ctx = next;
                          }
                        node.appendChild(child);
                      }
                  }
              }
            // 4.11. div element
            else if ("div".equals(elementName))
              {
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    parent.insertBefore(ctx, node);
                    transform(ctx);
                    ctx = next;
                  }
                parent.removeChild(node);
                return;
              }
            else if ("mixed".equals(elementName))
              {
                // 4.12. Number of child elements
                transformToOneChildElement(node, "group");
                // 4.13. mixed element
                Document doc = node.getOwnerDocument();
                Node interleave =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "interleave");
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    interleave.appendChild(ctx);
                    ctx = next;
                  }
                Node text =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "text");
                interleave.appendChild(text);
                parent.insertBefore(interleave, node);
                parent.removeChild(node);
                node = interleave;
              }
            else if ("optional".equals(elementName))
              {
                // 4.12. Number of child elements
                transformToOneChildElement(node, "group");
                // 4.14. optional element
                Document doc = node.getOwnerDocument();
                Node choice =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "choice");
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    choice.appendChild(ctx);
                    ctx = next;
                  }
                Node empty =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "empty");
                choice.appendChild(empty);
                parent.insertBefore(choice, node);
                parent.removeChild(node);
                node = choice;
              }
            else if ("zeroOrMore".equals(elementName))
              {
                // 4.12. Number of child elements
                transformToOneChildElement(node, "group");
                // 4.15. zeroOrMore element
                Document doc = node.getOwnerDocument();
                Node choice =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "choice");
                Node oneOrMore =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "oneOrMore");
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    oneOrMore.appendChild(ctx);
                    ctx = next;
                  }
                Node empty =
                  doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                      "empty");
                choice.appendChild(oneOrMore);
                choice.appendChild(empty);
                parent.insertBefore(choice, node);
                parent.removeChild(node);
                node = choice;
              }
            else if ("list".equals(elementName) ||
                     "oneOrMore".equals(elementName) ||
                     "define".equals(elementName))
              {
                // 4.12. Number of child elements
                transformToOneChildElement(node, "group");
              }
            else if ("except".equals(elementName))
              {
                // 4.12. Number of child elements
                transformToOneChildElement(node, "choice");
                // 4.16. Constraints
                String parentName = parent.getLocalName();
                if ("anyName".equals(parentName))
                  forbidDescendants(node, Collections.singleton("anyName"));
                else if ("nsName".equals(parentName))
                  {
                    Set names = new HashSet();
                    names.add("nsName");
                    names.add("anyName");
                    forbidDescendants(node, names);
                  }
              }
            else if ("choice".equals(elementName) ||
                     "group".equals(elementName) ||
                     "interleave".equals(elementName))
              {
                // 4.12. Number of child elements
                Node ctx = getFirstChildElement(node);
                Node next = getNextSiblingElement(ctx);
                if (next == null)
                  {
                    // replace
                    parent.insertBefore(ctx, node);
                    parent.removeChild(node);
                    transform(ctx);
                    return;
                  }
                else
                  {
                    // transform to 2 child elements
                    Node next2 = getNextSiblingElement(next);
                    if (next2 != null)
                      {
                        Document doc = node.getOwnerDocument();
                        Node child =
                          doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                              elementName);
                        child.appendChild(ctx);
                        child.appendChild(next);
                        node.insertBefore(next2, child);
                        transform(node); // recurse
                      }
                  }
              }
            // 4.17. combine attribute
            else if ("grammar".equals(elementName))
              {
                String combine = null;
                List nodes = new LinkedList();
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    if ("start".equals(ctx.getLocalName()))
                      {
                        String c = ((Element) ctx).getAttribute("combine");
                        if (combine != null && !combine.equals(c))
                          throw new GrammarException("multiple start elements "+
                                                     "but no combine attribute");
                        combine = c;
                        nodes.add(ctx);
                      }
                    ctx = next;
                  }
                if (!nodes.isEmpty())
                  combineNodes(node, combine, "start", nodes);
                // defines
                Map defines = new HashMap();
                Map defineCombines = new HashMap();
                ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    if ("define".equals(ctx.getLocalName()))
                      {
                        String name = ((Element) ctx).getAttribute("name");
                        combine = (String) defineCombines.get(name);
                        String c = ((Element) ctx).getAttribute("combine");
                        if (combine != null && !combine.equals(c))
                          throw new GrammarException("multiple define " +
                                                     "elements with name '"+
                                                     name + "' but no " +
                                                     "combine attribute");
                        defineCombines.put(name, c);
                        nodes = (List) defines.get(name);
                        if (nodes == null)
                          {
                            nodes = new LinkedList();
                            defines.put(name, nodes);
                          }
                        nodes.add(ctx);
                      }
                    ctx = next;
                  }
                for (Iterator i = defines.keySet().iterator(); i.hasNext(); )
                  {
                    String name = (String) i.next();
                    combine = (String) defineCombines.get(name);
                    nodes = (List) defines.get(name);
                    if (!nodes.isEmpty())
                      combineNodes(node, combine, "define", nodes);
                  }
              }
            // 4.9. ns attribute
            if ("name".equals(elementName) ||
                "nsName".equals(elementName) ||
                "value".equals(elementName))
              {
                Element element = (Element) node;
                String ns = element.getAttribute("ns");
                if (ns == null)
                  {
                    Node ctx = parent;
                    while (ns == null && ctx != null &&
                           ctx.getNodeType() == Node.ELEMENT_NODE)
                      {
                        ns = ((Element) ctx).getAttribute("ns");
                        ctx = ctx.getParentNode();
                      }
                    element.setAttribute("ns", (ns == null) ? "" : ns);
                  }
                if ("name".equals(elementName))
                  {
                    // 4.10. QNames
                    String name = element.getTextContent();
                    int ci = name.indexOf(':');
                    if (ci != -1)
                      {
                        String prefix = name.substring(0, ci);
                        element.setTextContent(name.substring(ci + 1));
                        ns = element.lookupNamespaceURI(prefix);
                        element.setAttribute("ns", (ns == null) ? "" : ns);
                      }
                    // 4.16. Constraints
                    if (isDescendantOfFirstChildOfAttribute(element) &&
                        "".equals(element.getAttribute("ns")) &&
                        "xmlns".equals(element.getTextContent()))
                      throw new GrammarException("name cannot be xmlns");
                  }
                else if ("nsName".equals(elementName))
                  {
                    // 4.16. Constraints
                    if (isDescendantOfFirstChildOfAttribute(element) &&
                        "http://www.w3.org/2000/xmlns"
                        .equals(element.getAttribute("ns")))
                      throw new GrammarException("nsName cannot be XMLNS URI");
                  }
              }
          }
        
        break;
      case Node.TEXT_NODE:
      case Node.CDATA_SECTION_NODE:
        // 4.2 Whitespace
        String parentName = parent.getLocalName();
        if ("name".equals(parentName))
          node.setNodeValue(node.getNodeValue().trim());
        if (!"param".equals(parentName) &&
            !"value".equals(parentName) &&
            isWhitespace(node.getNodeValue()))
          parent.removeChild(node);
        break;
      case Node.DOCUMENT_NODE:
        break;
      default:
        parent.removeChild(node);
      }
    // Transform children
    Node ctx = node.getFirstChild();
    while (ctx != null)
      {
        Node next = ctx.getNextSibling();
        transform(ctx);
        ctx = next;
      }
  }

  /**
   * Transforms the schema to place all defines under the top-level grammar
   * element and replace all other grammar elements by their start child.
   */
  private void transformGrammar(Node grammar, Node node)
    throws GrammarException
  {
    if (node.getNodeType() == Node.ELEMENT_NODE)
      {
        String elementName = node.getLocalName();
        if ("grammar".equals(elementName))
          {
            handleRefs(grammar, node, node);
            Node start = null;
            Node ctx = node.getFirstChild();
            while (ctx != null)
              {
                Node next = ctx.getNextSibling();
                String childName = ctx.getLocalName();
                if ("define".equals(childName))
                  grammar.appendChild(ctx);
                else if ("start".equals(childName))
                  start = ctx;
                ctx = next;
              }
            if (start == null)
              throw new GrammarException("no start element for grammar");
            Node p = getFirstChildElement(start);
            Node parent = node.getParentNode();
            parent.insertBefore(p, node);
            parent.removeChild(node);
            node = p;
          }
        Node ctx = node.getFirstChild();
        while (ctx != null)
          {
            Node next = ctx.getNextSibling();
            transformGrammar(grammar, ctx);
            ctx = next;
          }
      }
  }

  /**
   * Checks that all references in the specified grammar match a define in
   * the grammar.
   */
  private void handleRefs(Node grammar1, Node grammar2, Node node)
    throws GrammarException
  {
    if (node.getNodeType() == Node.ELEMENT_NODE)
      {
        String elementName = node.getLocalName();
        if ("ref".equals(elementName) || "parentRef".equals(elementName))
          {
            Node grammar = grammar2;
            if ("parentRef".equals(elementName))
              grammar = grammar1;
            
            String name = ((Element) node).getAttribute("name");
            if (name != null)
              throw new GrammarException("no name attribute on " +
                                         elementName);
            Node define = null;
            for (Node ctx = grammar.getFirstChild();
                 define == null && ctx != null;
                 ctx = ctx.getNextSibling())
              {
                if ("define".equals(ctx.getLocalName()))
                  {
                    String dname = ((Element) ctx).getAttribute("name");
                    if (name.equals(dname))
                      define = ctx;
                  }
              }
            if (define == null)
              throw new GrammarException("no define for '" + name + "'");
            name = ((Element) define).getAttribute("new-name");
            if (name == null)
              {
                name = createRefName();
                ((Element) define).setAttribute("new-name", name);
              }
            if ("parentRef".equals(elementName))
              {
                Document doc = node.getOwnerDocument();
                Node ref = doc.createElementNS(XMLConstants.RELAXNG_NS_URI,
                                               "ref");
                Node ctx = node.getFirstChild();
                while (ctx != null)
                  {
                    Node next = ctx.getNextSibling();
                    ref.appendChild(ctx);
                    ctx = next;
                  }
                Node parent = node.getParentNode();
                parent.insertBefore(ref, node);
                parent.removeChild(node);
                node = ref;
              }
            ((Element) node).setAttribute("name", name);
          }
        else if ("grammar".equals(elementName))
          {
            grammar1 = grammar2;
            grammar2 = node;
          }
        Node ctx = node.getFirstChild();
        while (ctx != null)
          {
            Node next = ctx.getNextSibling();
            handleRefs(grammar1, grammar2, ctx);
            ctx = next;
          }
      }
  }

  private String createRefName()
  {
    return "ref" + Integer.toString(refCount++);
  }

  private void transform2(Node node)
    throws GrammarException
  {
    Node parent = node.getParentNode();
    if (node.getNodeType() == Node.ELEMENT_NODE)
      {
        String elementName = node.getLocalName();
        // 4.20. notAllowed element
        if ("notAllowed".equals(elementName))
          {
            String parentName = parent.getLocalName();
            if ("attribute".equals(parentName) ||
                "list".equals(parentName) ||
                "group".equals(parentName) ||
                "interleave".equals(parentName) ||
                "oneOrMore".equals(parentName))
              {
                Node pp = parent.getParentNode();
                pp.insertBefore(node, parent);
                pp.removeChild(parent);
                transform2(node); // apply recursively
                return;
              }
            else if ("choice".equals(parentName))
              {
                Node p1 = getFirstChildElement(parent);
                Node p2 = getNextSiblingElement(p1);
                if (p1 == null || p2 == null)
                  throw new GrammarException("choice does not have two " +
                                             "children");
                String p1Name = p1.getLocalName();
                String p2Name = p2.getLocalName();
                Node pp = parent.getParentNode();
                if ("notAllowed".equals(p1Name) &&
                    "notAllowed".equals(p2Name))
                  {
                    pp.insertBefore(p1, parent);
                    pp.removeChild(parent);
                    transform2(p1); //apply recursively
                    return;
                  }
                else if ("notAllowed".equals(p1Name))
                  {
                    pp.insertBefore(p2, parent);
                    pp.removeChild(parent);
                    transform2(p2);
                    return;
                  }
                else
                  {
                    pp.insertBefore(p1, parent);
                    pp.removeChild(parent);
                    transform2(p1);
                    return;
                  }
              }
            else if ("except".equals(parentName))
              {
                Node pp = parent.getParentNode();
                pp.removeChild(parent);
                return;
              }
          }
        // 4.21. empty element
        else if ("empty".equals(elementName))
          {
            String parentName = parent.getLocalName();
            if ("group".equals(parentName) ||
                "interleave".equals(parentName))
              {
                Node p1 = getFirstChildElement(parent);
                Node p2 = getNextSiblingElement(p1);
                if (p1 == null || p2 == null)
                  throw new GrammarException(parentName + " does not have " +
                                             "two children");
                String p1Name = p1.getLocalName();
                String p2Name = p2.getLocalName();
                Node pp = parent.getParentNode();
                if ("empty".equals(p1Name) &&
                    "empty".equals(p2Name))
                  {
                    pp.insertBefore(p1, parent);
                    pp.removeChild(parent);
                    transform2(p1);
                    return;
                  }
                else if ("empty".equals(p1Name))
                  {
                    pp.insertBefore(p2, parent);
                    pp.removeChild(parent);
                    transform2(p2);
                    return;
                  }
                else
                  {
                    pp.insertBefore(p1, parent);
                    pp.removeChild(parent);
                    transform2(p1);
                    return;
                  }
              }
            else if ("choice".equals(parentName))
              {
                Node p1 = getFirstChildElement(parent);
                Node p2 = getNextSiblingElement(p1);
                if (p1 == null || p2 == null)
                  throw new GrammarException(parentName + " does not have " +
                                             "two children");
                String p1Name = p1.getLocalName();
                String p2Name = p2.getLocalName();
                Node pp = parent.getParentNode();
                if ("empty".equals(p1Name) &&
                    "empty".equals(p2Name))
                  {
                    pp.insertBefore(p1, parent);
                    pp.removeChild(parent);
                    transform2(p1);
                    return;
                  }
              }
            else if ("oneOrMore".equals(parentName))
              {
                Node pp = parent.getParentNode();
                pp.insertBefore(node, parent);
                pp.removeChild(parent);
                transform2(node);
                return;
              }
          }
        Node ctx = node.getFirstChild();
        while (ctx != null)
          {
            Node next = ctx.getNextSibling();
            transform2(ctx);
            ctx = next;
          }
      }
  }

  private static boolean isWhitespace(String text)
  {
    int len = text.length();
    for (int i = 0; i < len; i++)
      {
        char c = text.charAt(i);
        if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
          return false;
      }
    return true;
  }

  private static String escapeURL(String url)
  {
    try
      {
        return URLEncoder.encode(url, "UTF-8");
      }
    catch (UnsupportedEncodingException e)
      {
        RuntimeException e2 = new RuntimeException("UTF-8 is unsupported");
        e2.initCause(e);
        throw e2;
      }
  }

  /**
   * Resolve a URL to an element, as described in section 4.5.
   */
  private static Element resolve(String url)
    throws IOException
  {
    try
      {
        URL u = new URL(url);
        InputStream in = u.openStream();
        DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
        f.setNamespaceAware(true);
        f.setCoalescing(true);
        f.setExpandEntityReferences(true);
        f.setIgnoringComments(true);
        f.setIgnoringElementContentWhitespace(true);
        DocumentBuilder b = f.newDocumentBuilder();
        Document doc = b.parse(in, url);
        in.close();
        String fragment = u.getRef();
        if (fragment != null)
          return doc.getElementById(fragment);
        return doc.getDocumentElement();
      }
    catch (SAXException e)
      {
        IOException e2 = new IOException("error parsing included element");
        e2.initCause(e);
        throw e2;
      }
    catch (ParserConfigurationException e)
      {
        IOException e2 = new IOException("error parsing included element");
        e2.initCause(e);
        throw e2;
      }
  }

  /**
   * Returns the "components" of an element, as described in section 4.7.
   */
  private List getComponents(Node node)
  {
    List ret = new LinkedList();
    for (Node ctx = node.getFirstChild(); ctx != null;
         ctx = ctx.getNextSibling())
      {
        if (ctx.getNodeType() != Node.ELEMENT_NODE)
          continue;
        String ns = ctx.getNamespaceURI();
        if (ns != null && !ns.equals(XMLConstants.RELAXNG_NS_URI))
          continue;
        String name = ctx.getLocalName();
        if ("div".equals(name))
          ret.addAll(getComponents(ctx));
        else if (VOCABULARY.containsKey(name))
          ret.add(ctx);
      }
    return ret;
  }

  private static void transformToOneChildElement(Node node, String name)
  {
    if (node.getChildNodes().getLength() < 2)
      return;
    Document doc = node.getOwnerDocument();
    Element child = doc.createElementNS(XMLConstants.RELAXNG_NS_URI, name);
    Node ctx = getFirstChildElement(node);
    while (ctx != null)
      {
        Node next = getNextSiblingElement(ctx);
        child.appendChild(ctx);
        ctx = next;
      }
    node.appendChild(child);
  }
  
  private static Element getFirstChildElement(Node node)
  {
    Node ctx = node.getFirstChild();
    while (ctx != null && ctx.getNodeType() != Node.ELEMENT_NODE)
      ctx = ctx.getNextSibling();
    return (Element) ctx;
  }

  private static Element getNextSiblingElement(Node node)
  {
    Node ctx = node.getNextSibling();
    while (ctx != null && ctx.getNodeType() != Node.ELEMENT_NODE)
      ctx = ctx.getNextSibling();
    return (Element) ctx;
  }

  private static void forbidDescendants(Node node, Set names)
    throws GrammarException
  {
    for (Node ctx = node.getFirstChild(); ctx != null;
         ctx = ctx.getNextSibling())
      {
        String ns = ctx.getNamespaceURI();
        if (!XMLConstants.RELAXNG_NS_URI.equals(ns))
          continue;
        String name = ctx.getLocalName();
        if (names.contains(name))
          throw new GrammarException("name not allowed: " + name);
        forbidDescendants(ctx, names);
      }
  }

  private static boolean isDescendantOfFirstChildOfAttribute(Node node)
  {
    Node child = node;
    Node parent = node.getParentNode();
    while (parent != null && !"attribute".equals(parent.getLocalName()))
      {
        child = parent;
        parent = child.getParentNode();
      }
    if (parent == null)
      return false;
    Node firstChild = getFirstChildElement(parent);
    return firstChild == child;
  }

  private static void combineNodes(Node node, String combine, String name,
                                   List nodes)
  {
    Document doc = node.getOwnerDocument();
    Node child =
      doc.createElementNS(XMLConstants.RELAXNG_NS_URI, name);
    Node combineNode =
      doc.createElementNS(XMLConstants.RELAXNG_NS_URI, combine);
    child.appendChild(combineNode);
    boolean inserted = false;
    for (Iterator i = nodes.iterator(); i.hasNext(); )
      {
        Node startNode = (Node) i.next();
        if (!inserted)
          {
            node.insertBefore(child, startNode);
            inserted = true;
          }
        Node ctx = startNode.getFirstChild();
        while (ctx != null)
          {
            Node next = ctx.getNextSibling();
            combineNode.appendChild(ctx);
            ctx = next;
          }
        node.removeChild(startNode);
      }
  }

  Grammar parseGrammar(Element node)
    throws GrammarException
  {
    checkName(node, "grammar");
    Grammar grammar = new Grammar();
    Element start = getFirstChildElement(node);
    grammar.start = parsePattern(getFirstChildElement(start));
    for (Element define = getNextSiblingElement(start); define != null;
         define = getNextSiblingElement(define))
      grammar.defines.add(parseDefine(define));
    return grammar;
  }

  Define parseDefine(Element node)
    throws GrammarException
  {
    checkName(node, "define");
    Define define = new Define();
    define.name = node.getAttribute("name");
    define.element = parseElement(getFirstChildElement(node));
    return define;
  }

  Pattern parseTop(Element node)
    throws GrammarException
  {
    String name = node.getLocalName();
    if ("notAllowed".equals(name))
      return parseNotAllowed(node);
    return parsePattern(node);
  }

  Pattern parsePattern(Element node)
    throws GrammarException
  {
    String name = node.getLocalName();
    if ("empty".equals(name))
      return parseEmpty(node);
    return parseNonEmptyPattern(node);
  }

  Pattern parseNonEmptyPattern(Element node)
    throws GrammarException
  {
    String name = node.getLocalName();
    if ("text".equals(name))
      return parseText(node);
    else if ("data".equals(name))
      return parseData(node);
    else if ("value".equals(name))
      return parseValue(node);
    else if ("list".equals(name))
      return parseList(node);
    else if ("attribute".equals(name))
      return parseAttribute(node);
    else if ("ref".equals(name))
      return parseRef(node);
    else if ("oneOrMore".equals(name))
      return parseOneOrMore(node);
    else if ("choice".equals(name))
      return parseChoice(node);
    else if ("group".equals(name))
      return parseGroup(node);
    else if ("interleave".equals(name))
      return parseInterleave(node);
    throw new GrammarException("invalid pattern: " + name);
  }

  ElementPattern parseElement(Element node)
    throws GrammarException
  {
    checkName(node, "element");
    ElementPattern element = new ElementPattern();
    Element nameClass = getFirstChildElement(node);
    element.nameClass = parseNameClass(nameClass);
    element.pattern = parseTop(getNextSiblingElement(nameClass));
    return element;
  }

  NotAllowedPattern parseNotAllowed(Element node)
    throws GrammarException
  {
    checkName(node, "notAllowed");
    return NotAllowedPattern.INSTANCE;
  }

  EmptyPattern parseEmpty(Element node)
    throws GrammarException
  {
    checkName(node, "empty");
    return EmptyPattern.INSTANCE;
  }

  TextPattern parseText(Element node)
    throws GrammarException
  {
    checkName(node, "text");
    return TextPattern.INSTANCE;
  }

  DataPattern parseData(Element node)
    throws GrammarException
  {
    checkName(node, "data");
    DataPattern data = new DataPattern();
    DatatypeLibrary dl =
      getDatatypeLibrary(node.getAttribute("datatypeLibrary"));
    String type = node.getAttribute("type");
    try
      {
        data.type = dl.createDatatype(type);
        data.datatypeLibrary = dl;
      }
    catch (DatatypeException e)
      {
        GrammarException e2 = new GrammarException(type);
        e2.initCause(e);
        throw e2;
      }
    Element ctx = getFirstChildElement(node);
    while (ctx != null)
      {
        Element next = getNextSiblingElement(ctx);
        String name = ctx.getLocalName();
        if ("param".equals(name))
          data.params.add(parseParam(ctx));
        else if ("except".equals(name) && next == null)
          data.exceptPattern = parsePattern(getFirstChildElement(ctx));
        else
          throw new GrammarException("invalid element: " + name);
        ctx = next;
      }
    return data;
  }

  Param parseParam(Element node)
    throws GrammarException
  {
    checkName(node, "param");
    Param param  = new Param();
    param.name = node.getAttribute("name");
    param.value = node.getTextContent();
    return param;
  }

  ValuePattern parseValue(Element node)
    throws GrammarException
  {
    checkName(node, "value");
    ValuePattern value = new ValuePattern();
    DatatypeLibrary dl =
      getDatatypeLibrary(node.getAttribute("datatypeLibrary"));
    String type = node.getAttribute("type");
    try
      {
        value.type = dl.createDatatype(type);
        value.datatypeLibrary = dl;
      }
    catch (DatatypeException e)
      {
        GrammarException e2 = new GrammarException(type);
        e2.initCause(e);
        throw e2;
      }
    value.ns = node.getAttribute("ns");
    value.value = node.getTextContent();
    return value;
  }
  
  ListPattern parseList(Element node)
    throws GrammarException
  {
    checkName(node, "list");
    ListPattern list = new ListPattern();
    list.pattern = parsePattern(getFirstChildElement(node));
    return list;
  }

  AttributePattern parseAttribute(Element node)
    throws GrammarException
  {
    checkName(node, "attribute");
    AttributePattern attribute = new AttributePattern();
    Element nameClass = getFirstChildElement(node);
    attribute.nameClass = parseNameClass(nameClass);
    attribute.pattern = parsePattern(getNextSiblingElement(nameClass));
    return attribute;
  }

  RefPattern parseRef(Element node)
    throws GrammarException
  {
    checkName(node, "ref");
    RefPattern ref = new RefPattern();
    ref.name = node.getAttribute("name");
    return ref;
  }

  OneOrMorePattern parseOneOrMore(Element node)
    throws GrammarException
  {
    checkName(node, "oneOrMore");
    OneOrMorePattern oneOrMore = new OneOrMorePattern();
    oneOrMore.pattern = parseNonEmptyPattern(getFirstChildElement(node));
    return oneOrMore;
  }

  ChoicePattern parseChoice(Element node)
    throws GrammarException
  {
    checkName(node, "choice");
    ChoicePattern choice = new ChoicePattern();
    Element p1 = getFirstChildElement(node);
    Element p2 = getNextSiblingElement(p1);
    choice.pattern1 = parsePattern(p1);
    choice.pattern2 = parseNonEmptyPattern(p2);
    return choice;
  }

  GroupPattern parseGroup(Element node)
    throws GrammarException
  {
    checkName(node, "group");
    GroupPattern group = new GroupPattern();
    Element p1 = getFirstChildElement(node);
    Element p2 = getNextSiblingElement(p1);
    group.pattern1 = parseNonEmptyPattern(p1);
    group.pattern2 = parseNonEmptyPattern(p2);
    return group;
  }

  InterleavePattern parseInterleave(Element node)
    throws GrammarException
  {
    checkName(node, "interleave");
    InterleavePattern interleave = new InterleavePattern();
    Element p1 = getFirstChildElement(node);
    Element p2 = getNextSiblingElement(p1);
    interleave.pattern1 = parseNonEmptyPattern(p1);
    interleave.pattern2 = parseNonEmptyPattern(p2);
    return interleave;
  }

  NameClass parseNameClass(Element node)
    throws GrammarException
  {
    String name = node.getLocalName();
    if ("anyName".equals(name))
      return parseAnyName(node);
    else if ("name".equals(name))
      return parseName(node);
    else if ("nsName".equals(name))
      return parseNsName(node);
    else if ("choice".equals(name))
      return parseChoiceNameClass(node);
    throw new GrammarException("invalid name class: " + name);
  }

  AnyNameNameClass parseAnyName(Element node)
    throws GrammarException
  {
    checkName(node, "anyName");
    AnyNameNameClass anyName = new AnyNameNameClass();
    Element except = getFirstChildElement(node);
    if (except != null) {
      checkName(except, "except");
      anyName.exceptNameClass = parseNameClass(getFirstChildElement(except));
    }
    return anyName;
  }

  NameNameClass parseName(Element node)
    throws GrammarException
  {
    checkName(node, "name");
    NameNameClass name = new NameNameClass();
    name.ns = node.getAttribute("ns");
    name.name = node.getTextContent();
    return name;
  }

  NSNameNameClass parseNsName(Element node)
    throws GrammarException
  {
    checkName(node, "nsName");
    NSNameNameClass nsName = new NSNameNameClass();
    nsName.ns = node.getAttribute("ns");
    Element except = getFirstChildElement(node);
    if (except != null) {
      checkName(except, "except");
      nsName.exceptNameClass = parseNameClass(getFirstChildElement(except));
    }
    return nsName;
  }

  ChoiceNameClass parseChoiceNameClass(Element node)
    throws GrammarException
  {
    checkName(node, "choice");
    ChoiceNameClass choice = new ChoiceNameClass();
    Element c1 = getFirstChildElement(node);
    Element c2 = getNextSiblingElement(c1);
    choice.name1 = parseNameClass(c1);
    choice.name2 = parseNameClass(c2);
    return choice;
  }

  void checkName(Element node, String name)
    throws GrammarException
  {
    if (!name.equals(node.getLocalName()))
      throw new GrammarException("expecting " + name);
  }

  DatatypeLibrary getDatatypeLibrary(String uri)
    throws GrammarException
  {
    if (datatypeLibraries == null)
      datatypeLibraries = new HashMap();
    DatatypeLibrary library = (DatatypeLibrary) datatypeLibraries.get(uri);
    if (library == null)
      {
        library = new DatatypeLibraryLoader().createDatatypeLibrary(uri);
        if (library == null)
          throw new GrammarException("Datatype library not supported: " + uri);
        datatypeLibraries.put(uri, library);
      }
    return library;
  }

}

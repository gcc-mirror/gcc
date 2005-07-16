/* ElementNode.java -- 
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

package gnu.xml.transform;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.StringTokenizer;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * A template node representing an XSL <code>element</code> instruction.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class ElementNode
  extends TemplateNode
{

  final TemplateNode name;
  final TemplateNode namespace;
  final String uas;
  final Node source;
  final Collection elementExcludeResultPrefixes;
  
  ElementNode(TemplateNode name,
              TemplateNode namespace, String uas, Node source)
  {
    this.name = name;
    this.namespace = namespace;
    this.uas = uas;
    this.source = source;
    NamedNodeMap attrs = source.getAttributes();
    Node attr = attrs.getNamedItemNS(Stylesheet.XSL_NS,
                                     "exclude-result-prefixes");
    if (attr != null)
      {
        elementExcludeResultPrefixes = new HashSet();
        StringTokenizer st = new StringTokenizer(attr.getNodeValue());
        while (st.hasMoreTokens())
          {
            elementExcludeResultPrefixes.add(st.nextToken());
          }
      }
    else
      {
        elementExcludeResultPrefixes = Collections.EMPTY_SET;
      }
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new ElementNode(name.clone(stylesheet),
                                       (namespace == null) ? null :
                                       namespace.clone(stylesheet),
                                       uas, source);
    if (children != null)
      {
        ret.children = children.clone(stylesheet);
      }
    if (next != null)
      {
        ret.next = next.clone(stylesheet);
      }
    return ret;
  }

  void doApply(Stylesheet stylesheet, QName mode,
             Node context, int pos, int len,
             Node parent, Node nextSibling)
    throws TransformerException
  {
    Document doc = (parent instanceof Document) ? (Document) parent :
      parent.getOwnerDocument();
    // Create a document fragment to hold the name
    DocumentFragment fragment = doc.createDocumentFragment();
    // Apply name to the fragment
    name.apply(stylesheet, mode,
               context, pos, len,
               fragment, null);
    // Use XPath string-value of fragment
    String nameValue = Expr.stringValue(fragment);

    String namespaceValue = null;
    if (namespace != null)
      {
        // Create a document fragment to hold the namespace
        fragment = doc.createDocumentFragment();
        // Apply namespace to the fragment
        namespace.apply(stylesheet, mode,
                        context, pos, len,
                        fragment, null);
        // Use XPath string-value of fragment
        namespaceValue = Expr.stringValue(fragment);
        if (namespaceValue.length() == 0)
          {
            namespaceValue = null;
          }
      }
    
    String prefix = getPrefix(nameValue);
    if (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix))
      {
        int ci = nameValue.indexOf(':');
        nameValue = nameValue.substring(ci + 1);
      }
    else
      {
        // Namespace aliasing
        if (prefix == null)
          {
            prefix = "#default";
          }
        String resultPrefix =
          (String) stylesheet.namespaceAliases.get(prefix);
        if (resultPrefix != null)
          {
            if ("#default".equals(resultPrefix))
              {
                resultPrefix = null;
              }
            namespaceValue = source.lookupNamespaceURI(resultPrefix);
          }
        if (prefix == "#default")
          {
            prefix = null;
          }
        // Look up ordinary namespace for this prefix
        if (namespaceValue == null)
          {
            if (XMLConstants.XML_NS_PREFIX.equals(prefix))
              {
                namespaceValue = XMLConstants.XML_NS_URI;
              }
            else
              {
                // Resolve namespace for this prefix
                namespaceValue = source.lookupNamespaceURI(prefix);
              }
          }
        /*if (prefix == null)
          {
            // Resolve prefix for this namespace
            prefix = parent.lookupPrefix(namespaceValue);
            if (prefix != null)
              {
                nameValue = prefix + ":" + nameValue;
              }
          }*/
      }
    // Create element
    Element element = (namespaceValue != null) ?
      doc.createElementNS(namespaceValue, nameValue) :
          doc.createElement(nameValue);
    if (nextSibling != null)
      {
        parent.insertBefore(element, nextSibling);
      }
    else
      {
        parent.appendChild(element);
      }
    stylesheet.addNamespaceNodes(source, element, doc,
                                 elementExcludeResultPrefixes);
    if (uas != null)
      {
        StringTokenizer st = new StringTokenizer(uas, " ");
        while (st.hasMoreTokens())
          {
            addAttributeSet(stylesheet, mode, context, pos, len,
                            element, null, st.nextToken());
          }
      }
    if (children != null)
      {
        children.apply(stylesheet, mode,
                       context, pos, len,
                       element, null);
      }
    if (next != null)
      {
        next.apply(stylesheet, mode,
                   context, pos, len,
                   parent, nextSibling);
      }
  }

  final String getPrefix(String name)
  {
    int ci = name.indexOf(':');
    return (ci == -1) ? null : name.substring(0, ci);
  }

  void addAttributeSet(Stylesheet stylesheet, QName mode,
                       Node context, int pos, int len,
                       Node parent, Node nextSibling, String attributeSet)
    throws TransformerException
  {
    for (Iterator i = stylesheet.attributeSets.iterator(); i.hasNext(); )
      {
        AttributeSet as = (AttributeSet) i.next();
        if (!as.name.equals(attributeSet))
          {
            continue;
          }
        if (as.uas != null)
          {
            StringTokenizer st = new StringTokenizer(as.uas, " ");
            while (st.hasMoreTokens())
              {
                addAttributeSet(stylesheet, mode, context, pos, len,
                                parent, nextSibling, st.nextToken());
              }
          }
        if (as.children != null)
          {
            as.children.apply(stylesheet, mode,
                              context, pos, len,
                              parent, nextSibling);
          }
      }
  }

  public boolean references(QName var)
  {
    if (name != null && name.references(var))
      {
        return true;
      }
    if (namespace != null && namespace.references(var))
      {
        return true;
      }
    return super.references(var);
  }
  
  public String toString()
  {
    StringBuffer buf = new StringBuffer(getClass().getName());
    buf.append('[');
    buf.append("name=");
    buf.append(name);
    if (uas != null)
      {
        buf.append(",uas=");
        buf.append(uas);
      }
    buf.append(']');
    return buf.toString();
  }
  
}

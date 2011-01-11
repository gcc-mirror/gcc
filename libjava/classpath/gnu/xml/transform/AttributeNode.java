/* AttributeNode.java --
   Copyright (C) 2004,2006 Free Software Foundation, Inc.

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

import gnu.java.lang.CPStringBuilder;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Attr;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * A template node representing an XSL <code>attribute</code> instruction.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class AttributeNode
  extends TemplateNode
{

  final TemplateNode name;
  final TemplateNode namespace;
  final Node source;

  AttributeNode(TemplateNode name,
                TemplateNode namespace, Node source)
  {
    this.name = name;
    this.namespace = namespace;
    this.source = source;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new AttributeNode(name.clone(stylesheet),
                                         (namespace == null) ? null :
                                         namespace.clone(stylesheet),
                                         source);
    if (children != null)
      ret.children = children.clone(stylesheet);
    if (next != null)
      ret.next = next.clone(stylesheet);
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
          namespaceValue = null;
      }

    String prefix = getPrefix(nameValue);
    if (namespaceValue == null)
      {
        if (prefix != null)
          {
            if (XMLConstants.XML_NS_PREFIX.equals(prefix))
              namespaceValue = XMLConstants.XML_NS_URI;
            else
              {
                // Resolve namespace for this prefix
                namespaceValue = source.lookupNamespaceURI(prefix);
              }
          }
      }
    else
      {
        if (prefix != null)
          {
            String ns2 = source.lookupNamespaceURI(prefix);
            if (ns2 != null && !ns2.equals(namespaceValue))
              {
                // prefix clashes, reset it
                prefix = null;
                int ci = nameValue.indexOf(':');
                nameValue = nameValue.substring(ci + 1);
              }
          }
      }
    if (prefix == null)
      {
        // Resolve prefix for this namespace
        prefix = source.lookupPrefix(namespaceValue);
        if (prefix != null)
          nameValue = prefix + ":" + nameValue;
        else
          {
            if (namespaceValue != null)
              {
                // Must invent a prefix
                prefix = inventPrefix(parent);
                nameValue = prefix + ":" + nameValue;
              }
          }
      }
    NamedNodeMap attrs = parent.getAttributes();
    boolean insert = true;
    if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespaceValue) ||
        XMLConstants.XMLNS_ATTRIBUTE.equals(nameValue) ||
        nameValue.startsWith("xmlns:"))
      {
        // Namespace declaration, do not output
        insert = false;
      }
    if (prefix != null && namespaceValue == null)
      {
        // Not a QName
        insert = false;
      }
    if (parent.getNodeType() == Node.ELEMENT_NODE &&
        parent.getFirstChild() != null)
      {
        // XSLT 7.1.3 Adding an attribute to an element after children have
        // been added to it is an error
        insert = false;
      }
    if (insert)
      {
        // Insert attribute
        Attr attr = (namespaceValue != null) ?
          doc.createAttributeNS(namespaceValue, nameValue) :
              doc.createAttribute(nameValue);
        if (attrs != null)
          {
            if (namespace != null)
              attrs.setNamedItemNS(attr);
            else
              attrs.setNamedItem(attr);
          }
        if (children != null)
          children.apply(stylesheet, mode,
                         context, pos, len,
                         attr, null);
      }
    if (next != null)
      next.apply(stylesheet, mode,
                 context, pos, len,
                 parent, nextSibling);
  }

  final String getPrefix(String name)
  {
    int ci = name.indexOf(':');
    return (ci == -1) ? null : name.substring(0, ci);
  }

  final String inventPrefix(Node parent)
  {
    String base = "ns";
    int count = 0;
    String ret = base + Integer.toString(count);
    while (parent.lookupNamespaceURI(ret) != null)
      {
        count++;
        ret = base + Integer.toString(count);
      }
    return ret;
  }

  public boolean references(QName var)
  {
    if (name != null && name.references(var))
      return true;
    if (namespace != null && namespace.references(var))
      return true;
    return super.references(var);
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder("attribute");
    buf.append('[');
    buf.append("name=");
    buf.append(name);
    buf.append(']');
    return buf.toString();
  }

}

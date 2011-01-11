/* LiteralNode.java --
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

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * A template node that copies a DOM node in the template to the result
 * tree.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class LiteralNode
  extends TemplateNode
{

  /**
   * The source node in the XSL template.
   */
  final Node source;

  final Collection elementExcludeResultPrefixes;

  LiteralNode(Node source)
  {
    this.source = source;
    if (source.getNodeType() == Node.ELEMENT_NODE)
      {
        NamedNodeMap attrs = source.getAttributes();
        Node attr = attrs.getNamedItemNS(Stylesheet.XSL_NS,
                                         "exclude-result-prefixes");
        if (attr != null)
          {
            elementExcludeResultPrefixes = new HashSet();
            StringTokenizer st = new StringTokenizer(attr.getNodeValue());
            while (st.hasMoreTokens())
              elementExcludeResultPrefixes.add(st.nextToken());
          }
        else
          elementExcludeResultPrefixes = Collections.EMPTY_SET;
      }
    else
      elementExcludeResultPrefixes = null;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new LiteralNode(source);
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
    Node result = null;
    Document doc = (parent instanceof Document) ? (Document) parent :
      parent.getOwnerDocument();
    short nodeType = source.getNodeType();
    if (nodeType == Node.ATTRIBUTE_NODE &&
        parent.getFirstChild() != null)
      {
        // Ignore attributes added after child elements
      }
    else
      {
        // Namespace aliasing
        if (nodeType == Node.ELEMENT_NODE)
          {
            String prefix = source.getPrefix();
            if (prefix == null)
              prefix = "#default";
            String resultPrefix =
              (String) stylesheet.namespaceAliases.get(prefix);
            if (resultPrefix != null)
              {
                if ("#default".equals(resultPrefix))
                  resultPrefix = null;
                String uri = source.lookupNamespaceURI(resultPrefix);
                String name = source.getNodeName();
                // Create a new element node in the result document
                result = doc.createElementNS(uri, name);
                // copy attributes
                NamedNodeMap srcAttrs = source.getAttributes();
                NamedNodeMap dstAttrs = result.getAttributes();
                int l = srcAttrs.getLength();
                for (int i = 0; i < l; i++)
                  {
                    Node attr = srcAttrs.item(i);
                    if (!Stylesheet.XSL_NS.equals(attr.getNamespaceURI()))
                      {
                        attr = attr.cloneNode(true);
                        attr = doc.adoptNode(attr);
                        dstAttrs.setNamedItemNS(attr);
                      }
                  }
              }
          }
        if (result == null)
          {
            // Create result node
            result = source.cloneNode(false);
            // Remove any XSL attributes
            NamedNodeMap attrs = result.getAttributes();
            if (attrs != null)
              {
                int l = attrs.getLength();
                for (int i = 0; i < l; i++)
                  {
                    Node attr = attrs.item(i);
                    if (Stylesheet.XSL_NS.equals(attr.getNamespaceURI()))
                      {
                        attrs.removeNamedItem(attr.getNodeName());
                        i--;
                        l--;
                      }
                  }
              }
            Node result2 = doc.adoptNode(result);
            if (result2 == null)
              {
                String msg = "Error adopting node to result tree: " +
                  result + " (" + result.getClass().getName() + ")";
                DOMSourceLocator l = new DOMSourceLocator(context);
                throw new TransformerException(msg, l);
              }
            result = result2;
          }
        if (nextSibling != null)
          parent.insertBefore(result, nextSibling);
        else
          parent.appendChild(result);
        if (nodeType == Node.ELEMENT_NODE)
          stylesheet.addNamespaceNodes(source, result, doc,
                                       elementExcludeResultPrefixes);
        // children
        if (children != null)
          children.apply(stylesheet, mode,
                         context, pos, len,
                         result, null);
      }
    // next sibling
    if (next != null)
      next.apply(stylesheet, mode,
                 context, pos, len,
                 parent, nextSibling);
  }

  public String toString()
  {
    return source.toString();
  }

}

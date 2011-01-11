/* CopyNode.java --
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

import java.util.Iterator;
import java.util.StringTokenizer;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * A template node representing the XSL <code>copy</code> instruction.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class CopyNode
  extends TemplateNode
{

  final String uas;

  CopyNode(String uas)
  {
    this.uas = uas;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new CopyNode(uas);
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
    Node copy = parent;
    switch (context.getNodeType())
      {
      case Node.TEXT_NODE:
      case Node.ATTRIBUTE_NODE:
      case Node.ELEMENT_NODE:
      case Node.PROCESSING_INSTRUCTION_NODE:
      case Node.COMMENT_NODE:
        Document doc = (parent instanceof Document) ? (Document) parent :
          parent.getOwnerDocument();
        copy = context.cloneNode(false);
        copy = doc.adoptNode(copy);
        if (copy.getNodeType() == Node.ATTRIBUTE_NODE)
          {
            if (parent.getFirstChild() != null)
              {
                // Ignore attempt to add attribute after children
              }
            else
              {
                NamedNodeMap attrs = parent.getAttributes();
                if (attrs != null)
                  attrs.setNamedItemNS(copy);
              }
          }
        else
          {
            if (nextSibling != null)
              parent.insertBefore(copy, nextSibling);
            else
              parent.appendChild(copy);
          }
      }
    if (uas != null)
      {
        StringTokenizer st = new StringTokenizer(uas, " ");
        while (st.hasMoreTokens())
          addAttributeSet(stylesheet, mode, context, pos, len,
                          copy, null, st.nextToken());
      }
    if (children != null)
      children.apply(stylesheet, mode,
                     context, pos, len,
                     copy, null);
    if (next != null)
      next.apply(stylesheet, mode,
                 context, pos, len,
                 parent, nextSibling);
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
          continue;
        if (as.uas != null)
          {
            StringTokenizer st = new StringTokenizer(as.uas, " ");
            while (st.hasMoreTokens())
              addAttributeSet(stylesheet, mode, context, pos, len,
                              parent, nextSibling, st.nextToken());
          }
        if (as.children != null)
          as.children.apply(stylesheet, mode,
                            context, pos, len,
                            parent, nextSibling);
      }
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder("copy");
    if (uas != null)
      {
        buf.append('[');
        buf.append("uas=");
        buf.append(uas);
        buf.append(']');
      }
    return buf.toString();
  }

}

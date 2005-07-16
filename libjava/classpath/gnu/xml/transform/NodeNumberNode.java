/* NodeNumberNode.java -- 
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Pattern;
import gnu.xml.xpath.Selector;
import gnu.xml.xpath.UnionExpr;

/**
 * A template node representing the XSL <code>number</code> instruction
 * with no <code>value</code> expression, i.e. the value is computed from
 * the document position of the context node.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class NodeNumberNode
  extends AbstractNumberNode
{

  static final int SINGLE = 0;
  static final int MULTIPLE = 1;
  static final int ANY = 2;

  final int level;
  final Pattern count;
  final Pattern from;

  NodeNumberNode(int level, Pattern count, Pattern from,
                 TemplateNode format, String lang,
                 int letterValue, String groupingSeparator, int groupingSize)
  {
    super(format, lang, letterValue, groupingSeparator, groupingSize);
    this.level = level;
    this.count = count;
    this.from = from;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new NodeNumberNode(level,
                                          (count == null) ? null :
                                          (Pattern) count.clone(stylesheet),
                                          (from == null) ? from :
                                          (Pattern) from.clone(stylesheet),
                                          format, lang, letterValue,
                                          groupingSeparator, groupingSize);
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

  int[] compute(Stylesheet stylesheet, Node context, int pos, int len)
    throws TransformerException
  {
    /*if (from != null)
      {
        Object ret = from.evaluate(context, pos, len);
        if (ret instanceof Collection)
          {
            Collection ns = (Collection) ret;
            if (ns.size() > 0)
              {
                List list = new ArrayList(ns);
                Collections.sort(list, documentOrderComparator);
                context = (Node) list.get(0);
              }
            else
              {
                return new int[0];
              }
          }
        else
          {
            return new int[0];
          }
      }*/
    Node current = context;
    switch (level)
      {
      case SINGLE:
        if (from == null)
          {
            while (context != null && !countMatches(current, context))
              {
                context = context.getParentNode();
              }
          }
        else
          {
            while (context != null && !countMatches(current, context) &&
                   !fromMatches(context))
              {
                context = context.getParentNode();
              }
          }
        return (context == null) ? new int[0] :
          new int[] { (context == current) ? pos : getIndex(current, context) };
      case MULTIPLE:
        List ancestors = new ArrayList();
        while (context != null)
          {
            if (countMatches(current, context))
              {
                if (from == null || fromMatches(context))
                  {
                    ancestors.add(context);
                  }
              }
            context = context.getParentNode();
          }
        Collections.sort(ancestors, documentOrderComparator);
        int[] ret = new int[ancestors.size()];
        for (int i = 0; i < ret.length; i++)
          {
            ret[i] = getIndex(current, (Node) ancestors.get(i));
          }
        return ret;
      case ANY:
        Expr preceding = new Selector(Selector.PRECEDING,
                                      Collections.EMPTY_LIST);
        Expr ancestorOrSelf = new Selector(Selector.ANCESTOR_OR_SELF,
                                           Collections.EMPTY_LIST);
        Expr any = new UnionExpr(preceding, ancestorOrSelf);
        Object eval = any.evaluate(context, pos, len);
        if (eval instanceof Collection)
          {
            Collection ns = (Collection) eval;
            List candidates = new ArrayList();
            for (Iterator i = ns.iterator(); i.hasNext(); )
              {
                Node candidate = (Node) i.next();
                if (countMatches(current, candidate))
                  {
                    candidates.add(candidate);
                    if (from != null && from.matches(candidate))
                      {
                        break;
                      }
                  }
              }
            return new int[] { candidates.size() };
          }
        return new int[0];
      default:
        throw new TransformerException("invalid level");
      }
  }

  boolean countMatches(Node current, Node node)
  {
    if (count == null)
      {
        int cnt = current.getNodeType();
        int nnt = node.getNodeType();
        if (cnt != nnt)
          {
            return false;
          }
        if (nnt == Node.ELEMENT_NODE || nnt == Node.ATTRIBUTE_NODE)
          {
            String curi = current.getNamespaceURI();
            String nuri = node.getNamespaceURI();
            if ((curi == null && nuri != null) ||
                (curi != null && !curi.equals(nuri)))
              {
                return false;
              }
            String cn = current.getLocalName();
            if (cn == null)
              {
                cn = current.getNodeName();
              }
            String nn = node.getLocalName();
            if (nn == null)
              {
                nn = node.getNodeName();
              }
            if (!cn.equals(nn))
              {
                return false;
              }
          }
        return true;
      }
    else
      {
        return count.matches(node);
      }
  }

  boolean fromMatches(Node node)
  {
    for (Node ctx = node.getParentNode(); ctx != null;
         ctx = ctx.getParentNode())
      {
        if (from.matches(ctx))
          {
            return true;
          }
      }
    return false;
  }

  int getIndex(Node current, Node node)
  {
    int index = 0;
    do
      {
        do
          {
            node = node.getPreviousSibling();
          }
        while (node != null && !countMatches(current, node));
        index++;
      }
    while (node != null);
    return index;
  }
  
}

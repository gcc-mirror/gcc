/* ForEachNode.java --
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * A template node representing an XSLT <code>for-each</code> instruction.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class ForEachNode
  extends TemplateNode
{

  final Expr select;
  final List<SortKey> sortKeys;

  ForEachNode(Expr select, List<SortKey> sortKeys)
  {
    this.select = select;
    this.sortKeys = sortKeys;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    int len = sortKeys.size();
    List<SortKey> sortKeys2 = new ArrayList<SortKey>(len);
    for (int i = 0; i < len; i++)
      sortKeys2.add(sortKeys.get(i).clone(stylesheet));
    TemplateNode ret = new ForEachNode(select.clone(stylesheet),
                                       sortKeys2);
    if (children != null)
      ret.children = children.clone(stylesheet);
    if (next != null)
      ret.next = next.clone(stylesheet);
    return ret;
  }

  @Override
  void doApply(Stylesheet stylesheet, QName mode,
             Node context, int pos, int len,
             Node parent, Node nextSibling)
    throws TransformerException
  {
    if (children != null)
      {
        // Set current template to null
        Template saved = stylesheet.currentTemplate;
        stylesheet.currentTemplate = null;
        Object ret = select.evaluate(context, pos, len);
        //System.err.println(toString() + ": " + context+" -> "+ret);
        if (ret instanceof Collection)
          {
            /* Suppression is safe, as we know context produces Collection<Node> */
            @SuppressWarnings("unchecked")
              Collection<Node> ns = (Collection<Node>) ret;
            List<Node> list = new ArrayList<Node>(ns);
            if (!sortKeys.isEmpty())
              {
                for (SortKey sortKey : sortKeys)
                  {
                    sortKey.init(stylesheet, mode, context, pos, len, parent,
                                 nextSibling);
                  }
                Collections.sort(list, new XSLComparator(sortKeys));
              }
            else
              Collections.sort(list, documentOrderComparator);
            // Perform children for each node
            int l = list.size();
            int p = 1;
            for (Node node : list)
              {
                stylesheet.current = node;
                children.apply(stylesheet, mode,
                               node, p++, l,
                               parent, nextSibling);
              }
          }
        // Restore current template
        stylesheet.currentTemplate = saved;
      }
    if (next != null)
      next.apply(stylesheet, mode,
                 context, pos, len,
                 parent, nextSibling);
  }

  @Override
  public boolean references(QName var)
  {
    if (select != null && select.references(var))
      return true;
    for (Iterator<SortKey> i = sortKeys.iterator(); i.hasNext(); )
      {
        if (i.next().references(var))
          return true;
      }
    return super.references(var);
  }

  @Override
  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder("for-each");
    buf.append('[');
    buf.append("select=");
    buf.append(select);
    buf.append(']');
    return buf.toString();
  }

}

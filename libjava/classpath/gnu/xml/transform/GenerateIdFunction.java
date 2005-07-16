/* GenerateIdFunction.java -- 
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
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;

/**
 * The XSLT <code>generate-id()</code>function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class GenerateIdFunction
  extends Expr
  implements XPathFunction, Function
{

  List args;

  public Object evaluate(List args)
    throws XPathFunctionException
  {
    // Useless...
    return Collections.EMPTY_SET;
  }

  public void setArguments(List args)
  {
    this.args = args;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    int arity = args.size();
    List values = new ArrayList(arity);
    for (int i = 0; i < arity; i++)
      {
        Expr arg = (Expr) args.get(i);
        values.add(arg.evaluate(context, pos, len));
      }
    Node node;
    Collection ns = (arity == 0) ? Collections.EMPTY_SET :
      (Collection) values.get(0);
    if (ns.isEmpty())
      {
        node = context;
      }
    else
      {
        List list = new ArrayList(ns);
        Collections.sort(list, documentOrderComparator);
        node = (Node) list.get(0);
      }

    String name = node.getNodeName();
    int index = 0, depth = 0;
    for (Node ctx = node.getPreviousSibling(); ctx != null;
         ctx = ctx.getPreviousSibling())
      {
        index++;
      }
    for (Node ctx = node.getParentNode(); ctx != null;
         ctx = ctx.getParentNode())
      {
        depth++;
      }
    return name + "-" + index + "-" + depth;
  }

  public Expr clone(Object context)
  {
    GenerateIdFunction f = new GenerateIdFunction();
    int len = args.size();
    List args2 = new ArrayList(len);
    for (int i = 0; i < len; i++)
      {
        args2.add(((Expr) args.get(i)).clone(context));
      }
    f.setArguments(args2);
    return f;
  }

  public boolean references(QName var)
  {
    for (Iterator i = args.iterator(); i.hasNext(); )
      {
        if (((Expr) i.next()).references(var))
          {
            return true;
          }
      }
    return false;
  }

}


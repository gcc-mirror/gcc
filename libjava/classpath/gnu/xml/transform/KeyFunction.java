/* KeyFunction.java -- 
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
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;
import gnu.xml.xpath.Pattern;

/**
 * The XSLT <code>key()</code>function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class KeyFunction
  extends Pattern
  implements XPathFunction, Function
{

  final Stylesheet stylesheet;
  List args;

  KeyFunction(Stylesheet stylesheet)
  {
    this.stylesheet = stylesheet;
  }

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

  public boolean matches(Node context)
  {
    Object ret = evaluate(context, 1, 1);
    return !((Collection) ret).isEmpty();
  }

  public Object evaluate(Node context, int pos, int len)
  {
    // Evaluate arguments
    int arity = args.size();
    List values = new ArrayList(arity);
    for (int i = 0; i < arity; i++)
      {
        Expr arg = (Expr) args.get(i);
        values.add(arg.evaluate(context, pos, len));
      }
    // Get key name
    QName keyName = QName.valueOf(_string(context, values.get(0)));
    // Expand qualified name
    String uri = keyName.getNamespaceURI();
    String prefix = keyName.getPrefix();
    if ((uri == null || uri.length() == 0) && 
        (prefix != null && prefix.length() > 0))
      {
        uri = stylesheet.getNamespaceURI(prefix);
        if (uri != null && uri.length() > 0)
          {
            String localName = keyName.getLocalPart();
            keyName = new QName(uri, localName, prefix);
          }
      }
    // Compute matching key set
    Collection keySet = new LinkedList();
    for (Iterator i = stylesheet.keys.iterator(); i.hasNext(); )
      {
        Key key = (Key) i.next();
        if (key.name.equals(keyName))
          {
            keySet.add(key);
          }
      }
    // Get target
    Object target = values.get(1);
    Collection acc = new LinkedHashSet();
    Document doc = (context instanceof Document) ? (Document) context :
      context.getOwnerDocument();
    if (target instanceof Collection)
      {
        for (Iterator i = ((Collection) target).iterator(); i.hasNext(); )
          {
            String val = Expr.stringValue((Node) i.next());
            addKeyNodes(doc, keySet, val, acc);
          }
      }
    else
      {
        String val = Expr._string(context, target);
        addKeyNodes(doc, keySet, val, acc);
      }
    List ret = new ArrayList(acc);
    Collections.sort(ret, documentOrderComparator);
    return ret;
  }

  final void addKeyNodes(Node node, Collection keySet,
                         String value, Collection acc)
  {
    addKeyNodeIfMatch(node, keySet, value, acc);
    // Apply children
    for (Node ctx = node.getFirstChild(); ctx != null;
         ctx = ctx.getNextSibling())
      {
        addKeyNodes(ctx, keySet, value, acc);
      }
  }
  
  final void addKeyNodeIfMatch(Node node, Collection keySet,
                               String value, Collection acc)
  {
    for (Iterator i = keySet.iterator(); i.hasNext(); )
      {
        Key key = (Key) i.next();
        if (key.match.matches(node))
          {
            Object eval = key.use.evaluate(node, 1, 1);
            if (eval instanceof Collection)
              {
                for (Iterator j = ((Collection) eval).iterator();
                     j.hasNext(); )
                  {
                    String keyValue = Expr.stringValue((Node) j.next());
                    if (value.equals(keyValue))
                      {
                        acc.add(node);
                        return;
                      }
                  }
              }
            else
              {
                String keyValue = Expr._string(node, eval);
                if (value.equals(keyValue))
                  {
                    acc.add(node);
                    return;
                  }
              }
          }
      }
  }

  public Expr clone(Object context)
  {
    Stylesheet s = stylesheet;
    if (context instanceof Stylesheet)
      {
        s = (Stylesheet) context;
      }
    KeyFunction f = new KeyFunction(s);
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


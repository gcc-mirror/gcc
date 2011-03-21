/* FunctionAvailableFunction.java --
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
import java.util.TreeSet;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;

/**
 * The XSLT <code>function-available</code> function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class FunctionAvailableFunction
  extends Expr
  implements Function, XPathFunction
{

  static final Collection xsltFunctions;
  static final Collection xpathFunctions;
  static
  {
    TreeSet acc = new TreeSet();
    acc.add("document");
    acc.add("key");
    acc.add("format-number");
    acc.add("current");
    acc.add("unparsed-entity-uri");
    acc.add("generate-id");
    acc.add("system-property");
    acc.add("element-available");
    acc.add("function-available");
    xsltFunctions = Collections.unmodifiableSet(acc);
    acc = new TreeSet();
    acc.add("boolean");
    acc.add("ceiling");
    acc.add("concat");
    acc.add("contains");
    acc.add("count");
    acc.add("false");
    acc.add("floor");
    acc.add("id");
    acc.add("lang");
    acc.add("last");
    acc.add("local-name");
    acc.add("name");
    acc.add("namespace-uri");
    acc.add("normalize-space");
    acc.add("not");
    acc.add("number");
    acc.add("position");
    acc.add("round");
    acc.add("starts-with");
    acc.add("string");
    acc.add("string-length");
    acc.add("substring-after");
    acc.add("substring-before");
    acc.add("substring");
    acc.add("sum");
    acc.add("translate");
    acc.add("true");
    xpathFunctions = Collections.unmodifiableSet(acc);
  }

  final NamespaceContext nsctx;
  List args;

  FunctionAvailableFunction(NamespaceContext nsctx)
  {
    this.nsctx = nsctx;
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

  public Object evaluate(Node context, int pos, int len)
  {
    Expr arg = (Expr) args.get(0);
    Object val = arg.evaluate(context, pos, len);
    String name = _string(context, val);
    String prefix, localName, uri;
    int ci = name.indexOf(':');
    if (ci == -1)
      {
        prefix = null;
        localName = name;
      }
    else
      {
        prefix = name.substring(0, ci);
        localName = name.substring(ci + 1);
      }
    uri = nsctx.getNamespaceURI(prefix);
    if (uri == null)
      {
        return (xpathFunctions.contains(localName)  ||
                xsltFunctions.contains(localName)) ?
          Boolean.TRUE : Boolean.FALSE;
      }
    else if (Stylesheet.XSL_NS.equals(uri))
      {
        return xsltFunctions.contains(localName) ?
          Boolean.TRUE : Boolean.FALSE;
      }
    // TODO extension functions
    return Boolean.FALSE;
  }

  public Expr clone(Object context)
  {
    NamespaceContext n = nsctx;
    if (context instanceof NamespaceContext)
      n = (NamespaceContext) context;
    FunctionAvailableFunction f = new FunctionAvailableFunction(n);
    int len = args.size();
    List args2 = new ArrayList(len);
    for (int i = 0; i < len; i++)
      args2.add(((Expr) args.get(i)).clone(context));
    f.setArguments(args2);
    return f;
  }

  public boolean references(QName var)
  {
    for (Iterator i = args.iterator(); i.hasNext(); )
      {
        if (((Expr) i.next()).references(var))
          return true;
      }
    return false;
  }

  public String toString()
  {
    return "function-available(" + args.get(0) + ")";
  }

}

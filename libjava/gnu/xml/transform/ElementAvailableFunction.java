/* ElementAvailableFunction.java -- 
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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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
import java.util.List;
import java.util.TreeSet;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;

/**
 * The XSLT <code>element-available</code> function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class ElementAvailableFunction
  extends Expr
  implements Function, XPathFunction
{

  static final Collection elements;
  static
  {
    TreeSet acc = new TreeSet();
    acc.add("stylesheet");
    acc.add("template");
    acc.add("param");
    acc.add("variable");
    acc.add("include");
    acc.add("import");
    acc.add("output");
    acc.add("preserve-space");
    acc.add("strip-space");
    acc.add("key");
    acc.add("decimal-format");
    acc.add("namespace-alias");
    acc.add("attribute-set");
    acc.add("apply-templates");
    acc.add("call-template");
    acc.add("value-of");
    acc.add("for-each");
    acc.add("if");
    acc.add("choose");
    acc.add("when");
    acc.add("otherwise");
    acc.add("element");
    acc.add("attribute");
    acc.add("text");
    acc.add("copy");
    acc.add("processing-instruction");
    acc.add("comment");
    acc.add("number");
    acc.add("copy-of");
    acc.add("message");
    acc.add("sort");
    acc.add("with-param");
    acc.add("fallback");
    acc.add("apply-imports");
    elements = Collections.unmodifiableSet(acc);
  }

  final NamespaceContext nsctx;
  List args;

  ElementAvailableFunction(NamespaceContext nsctx)
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
    if (Stylesheet.XSL_NS.equals(uri))
      {
        return elements.contains(localName) ?
          Boolean.TRUE : Boolean.FALSE;
        // TODO extension elements
      }
    return Boolean.FALSE;
  }
  
  public Expr clone(Object context)
  {
    NamespaceContext n = nsctx;
    if (context instanceof NamespaceContext)
      {
        n = (NamespaceContext) context;
      }
    ElementAvailableFunction f = new ElementAvailableFunction(n);
    int len = args.size();
    List args2 = new ArrayList(len);
    for (int i = 0; i < len; i++)
      {
        args2.add(((Expr) args.get(i)).clone(context));
      }
    f.setArguments(args2);
    return f;
  }

}


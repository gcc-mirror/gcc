/* DocumentFunction.java -- 
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
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Constant;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;
import gnu.xml.xpath.IdFunction;

/**
 * The XSLT <code>document()</code>function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class DocumentFunction
  extends Expr
  implements Function, XPathFunction
{

  final Stylesheet stylesheet;
  final Node base;
  List args;
  List values;

  DocumentFunction(Stylesheet stylesheet, Node base)
  {
    this.stylesheet = stylesheet;
    this.base = base;
  }

  public Object evaluate(List args)
    throws XPathFunctionException
  {
    values = args;
    return evaluate(null, 1, 1);
  }

  public void setArguments(List args)
  {
    this.args = args;
  }
  
  public Object evaluate(Node context, int pos, int len)
  {
    int arity = args.size();
    if (values == null)
      {
        values = new ArrayList(arity);
        for (int i = 0; i < arity; i++)
          {
            Expr arg = (Expr) args.get(i);
            values.add(arg.evaluate(context, pos, len));
          }
      }
    Object ret;
    switch (arity)
      {
      case 1:
        Object arg = values.get(0);
        if (arg instanceof Collection)
          {
            Collection ns = (Collection) arg;
            Collection acc = new TreeSet();
            for (Iterator i = ns.iterator(); i.hasNext(); )
              {
                Node node = (Node) i.next();
                String s = Expr.stringValue(node);
                acc.addAll(document(s, node.getBaseURI()));
              }
            ret = acc;
          }
        else
          {
            String s = Expr._string(context, arg);
            ret = document(s, base.getBaseURI());
          }
        break;
      case 2:
        Object arg1 = values.get(0);
        Object arg2 = values.get(1);
        if (!(arg2 instanceof Collection))
          throw new RuntimeException("second argument is not a node-set");
        Collection arg2ns = (Collection) arg2;
        String base2 = arg2ns.isEmpty() ? null :
          ((Node) arg2ns.iterator().next()).getBaseURI();
        if (arg1 instanceof Collection)
          {
            Collection arg1ns = (Collection) arg1;
            Collection acc = new TreeSet();
            for (Iterator i = arg1ns.iterator(); i.hasNext(); )
              {
                Node node = (Node) i.next();
                String s = Expr.stringValue(node);
                acc.addAll(document(s, base2));
              }
            ret = acc;
          }
        else
          {
            String s = Expr._string(context, arg1);
            ret = document(s, base2);
          }
        break;
      default:
        throw new RuntimeException("invalid arity");
      }
    values = null;
    return ret;
  }

  /**
   * The XSL <code>document</code> function.
   * @see XSLT 12.1
   * @param uri the URI from which to retrieve nodes
   * @param base the base URI for relative URIs
   */
  Collection document(String uri, String base)
  {
    if ("".equals(uri) || uri == null)
      uri = this.base.getBaseURI();
    
    // Get fragment
    Expr fragment = null;
    int hi = uri.indexOf('#');
    if (hi != -1)
      {
        String f = uri.substring(hi + 1);
        uri = uri.substring(0, hi);
        // TODO handle xpointer() here
        // this only handles IDs
        fragment = new IdFunction(new Constant(f));
      }

    // Get document source
    try
      {
        DOMSource source;
        XSLURIResolver resolver = stylesheet.factory.resolver;
        synchronized (resolver)
          {
            if (stylesheet.transformer != null)
              {
                resolver.setUserResolver(stylesheet.transformer.uriResolver);
                resolver.setUserListener(stylesheet.transformer.errorListener);
              }
            source = resolver.resolveDOM(null, base, uri);
          }
        Node node = source.getNode();
        // Strip whitespace
        TransformerImpl.strip(stylesheet, node);
        if (fragment == null)
          return Collections.singleton(node);
        else
          {
            Object ret = fragment.evaluate(node, 1, 1);
            if (!(ret instanceof Collection))
              {
                // XXX Report error?
                return Collections.EMPTY_SET;
              }
            return (Collection) ret;
          }
      }
    catch (TransformerException e)
      {
        String msg = "can't open " + uri;
        if (base != null)
          msg += " with base " + base;
        throw new RuntimeException(msg);
      }
  }

  public Expr clone(Object context)
  {
    Stylesheet s = stylesheet;
    if (context instanceof Stylesheet)
      s = (Stylesheet) context;
    DocumentFunction f = new DocumentFunction(s, base);
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
  
}

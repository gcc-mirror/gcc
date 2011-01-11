/* FunctionCall.java --
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

package gnu.xml.xpath;

import gnu.java.lang.CPStringBuilder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import javax.xml.xpath.XPathFunctionResolver;
import org.w3c.dom.Node;

/**
 * Executes an XPath core or extension function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class FunctionCall
  extends Expr
{

  final XPathFunctionResolver resolver;
  final String name;
  final List<Expr> args;

  public FunctionCall(XPathFunctionResolver resolver, String name)
  {
    this(resolver, name, null);
  }

  public FunctionCall(XPathFunctionResolver resolver, String name, List<Expr> args)
  {
    this.resolver = resolver;
    this.name = name;
    if (args == null)
      this.args = Collections.emptyList();
    else
      this.args = args;
  }

  @Override
  public Object evaluate(Node context, int pos, int len)
  {
    if (resolver != null)
      {
        QName qname = QName.valueOf(name);
                                int arity = args.size();
        XPathFunction function = resolver.resolveFunction(qname, arity);
        if (function != null)
          {
            //System.err.println("Calling "+toString()+" with "+values);
            if (function instanceof Expr)
              {
                if (function instanceof Function)
                  {
                    ((Function) function).setArguments(args);
                  }
                return ((Expr) function).evaluate(context, pos, len);
              }
            else
              {
                List<Object> values = new ArrayList<Object>(arity);
                for (int i = 0; i < arity; i++)
                  {
                    Expr arg = (Expr) args.get(i);
                    values.add(arg.evaluate(context, pos, len));
                  }
                try
                  {
                    return function.evaluate(values);
                  }
                catch (XPathFunctionException e)
                  {
                    e.printStackTrace(System.err); // FIXME
                    throw new RuntimeException(e.getMessage(), e);
                  }
              }
          }
      }
    throw new IllegalArgumentException("Invalid function call: " +
                                       toString());
  }

  public Expr clone(Object context)
  {
    int len = args.size();
    List<Expr> args2 = new ArrayList<Expr>(len);
    for (int i = 0; i < len; i++)
      {
        args2.add(args.get(i).clone(context));
      }
    XPathFunctionResolver r = resolver;
    if (context instanceof XPathFunctionResolver)
      {
        r = (XPathFunctionResolver) context;
      }
    return new FunctionCall(r, name, args2);
  }

  public boolean references(QName var)
  {
    for (Iterator<Expr> i = args.iterator(); i.hasNext(); )
      {
        if (i.next().references(var))
          {
            return true;
          }
      }
    return false;
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder();
    buf.append(name);
    buf.append('(');
    int len = args.size();
    for (int i = 0; i < len; i++)
      {
        if (i > 0)
          {
            buf.append(',');
          }
        buf.append(args.get(i));
      }
    buf.append(')');
    return buf.toString();
  }

}

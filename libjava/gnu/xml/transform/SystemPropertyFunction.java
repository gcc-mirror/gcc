/* SystemPropertyFunction.java -- 
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
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathFunction;
import javax.xml.xpath.XPathFunctionException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;
import gnu.xml.xpath.Function;

/**
 * The XSLT <code>system-property()</code>function.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class SystemPropertyFunction
  extends Expr
  implements XPathFunction, Function
{

  List args;

  public Object evaluate(List args)
    throws XPathFunctionException
  {
    String name = (String) args.get(0);
    return systemProperty(QName.valueOf(name));
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
    String name = _string(context, values.get(0));
    return systemProperty(QName.valueOf(name));
  }

  Object systemProperty(QName name)
  {
    String localName = name.getLocalPart();
    String prefix = name.getPrefix();
    String uri = name.getNamespaceURI();
    if (Stylesheet.XSL_NS.equals(uri) ||
        "xsl".equals(prefix))
      {
        if ("version".equals(localName))
          {
            return new Double(1.0d);
          }
        else if ("vendor".equals(localName))
          {
            return "The Free Software Foundation";
          }
        else if ("vendor-url".equals(localName))
          {
            return "http://www.gnu.org/";
          }
        else
          {
            return "";
          }
      }
    return System.getProperty(localName);
  }

  public Expr clone(Object context)
  {
    SystemPropertyFunction f = new SystemPropertyFunction();
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


/* SumFunction.java -- 
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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * The <code>sum</code> function returns the sum, for each node in the
 * argument node-set, of the result of converting the string-values of the
 * node to a number.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class SumFunction
  extends Expr
{

  final Expr arg;

  SumFunction(List<Expr> args)
  {
    this(args.get(0));
  }

  SumFunction(Expr arg)
  {
    this.arg = arg;
  }

  @Override 
  public Object evaluate(Node context, int pos, int len)
  {
    Object val = arg.evaluate(context, pos, len);
    double sum = 0.0d;
    if (val instanceof Collection)
      {
	/* Suppression is safe, as we know context produces
	   Collection<Node> */
	@SuppressWarnings("unchecked")
	  Collection<Node> nodes = (Collection<Node>) val;
	for (Node node : nodes)
          {
            String s = stringValue(node);
            sum += _number(context, s);
          }
      }
    return new Double(sum);
  }

  public Expr clone(Object context)
  {
    return new SumFunction(arg.clone(context));
  }

  public boolean references(QName var)
  {
    return arg.references(var);
  }

  public String toString()
  {
    return "sum(" + arg + ")";
  }
  
}

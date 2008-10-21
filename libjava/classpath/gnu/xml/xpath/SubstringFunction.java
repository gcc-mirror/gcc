/* SubstringFunction.java -- 
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

import java.util.List;
import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * The substring function returns the substring of the first argument
 * starting at the position specified in the second argument with length
 * specified in the third argument. For example, substring("12345",2,3)
 * returns "234". If the third argument is not specified, it returns the
 * substring starting at the position specified in the second argument and
 * continuing to the end of the string. For example, substring("12345",2)
 * returns "2345".
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class SubstringFunction
  extends Expr
{

  final Expr arg1;
  final Expr arg2;
  final Expr arg3;

  SubstringFunction(List<Expr> args)
  {
    this(args.get(0), args.get(1),
         (args.size() > 2) ? args.get(2) : null);
  }

  SubstringFunction(Expr arg1, Expr arg2, Expr arg3)
  {
    this.arg1 = arg1;
    this.arg2 = arg2;
    this.arg3 = arg3;
  }

  @Override
  public Object evaluate(Node context, int pos, int len)
  {
    Object val1 = arg1.evaluate(context, pos, len);
    Object val2 = arg2.evaluate(context, pos, len);
    String s = _string(context, val1);
    int p = Expr.intValue(val2) - 1;
    if (p < 0)
      p = 0;

    int l = s.length() - p;
    if (l <= 0)
      return "";

    if (arg3 != null)
      {
        Object val3 = arg3.evaluate(context, pos, len);
        int v3 = Expr.intValue(val3);
        if (v3 < l) 
          l = v3;
      }

    return s.substring(p, p + l);
  }

  public Expr clone(Object context)
  {
    return new SubstringFunction(arg1.clone(context), arg2.clone(context),
                                 (arg3 == null) ? null : arg3.clone(context));
  }

  public boolean references(QName var)
  {
    return (arg1.references(var) || arg2.references(var) ||
            (arg3 == null) ? false : arg3.references(var));
  }

  public String toString()
  {
    return (arg3 == null) ? "substring(" + arg1 + "," + arg2 + ")" :
      "substring(" + arg1 + "," + arg2 + "," + arg3 + ")";
  }
  
}

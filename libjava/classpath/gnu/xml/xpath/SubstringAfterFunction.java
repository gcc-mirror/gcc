/* SubstringAfterFunction.java --
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
 * The <code>substring-after</code> function returns the substring of the
 * first argument string that follows the first occurrence of the second
 * argument string in the first argument string, or the empty string if the
 * first argument string does not contain the second argument string. For
 * example, substring-after("1999/04/01","/") returns 04/01, and
 * substring-after("1999/04/01","19") returns 99/04/01.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class SubstringAfterFunction
  extends Expr
{

  final Expr arg1;
  final Expr arg2;

  SubstringAfterFunction(List<Expr> args)
  {
    this(args.get(0), args.get(1));
  }

  SubstringAfterFunction(Expr arg1, Expr arg2)
  {
    this.arg1 = arg1;
    this.arg2 = arg2;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    Object val1 = arg1.evaluate(context, pos, len);
    Object val2 = arg2.evaluate(context, pos, len);
    String s1 = _string(context, val1);
    String s2 = _string(context, val2);
    int index = s1.indexOf(s2);
    return (index == -1) ? "" : s1.substring(index + s2.length());
  }

  public Expr clone(Object context)
  {
    return new SubstringAfterFunction(arg1.clone(context),
                                      arg2.clone(context));
  }

  public boolean references(QName var)
  {
    return (arg1.references(var) || arg2.references(var));
  }

  public String toString()
  {
    return "substring-after(" + arg1 + "," + arg2 + ")";
  }

}

/* ContainsFunction.java -- 
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

package gnu.xml.xpath;

import java.util.List;
import org.w3c.dom.Node;

/**
 * The <code>contains</code> function returns true if the first argument
 * string contains the second argument string, and otherwise returns false.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class ContainsFunction
  extends Expr
{

  final Expr arg1;
  final Expr arg2;

  ContainsFunction(List args)
  {
    this((Expr) args.get(0), (Expr) args.get(1));
  }

  ContainsFunction(Expr arg1, Expr arg2)
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
    return (s1.indexOf(s2) != -1) ? Boolean.TRUE : Boolean.FALSE;
  }

  public Expr clone(Object context)
  {
    return new ContainsFunction(arg1.clone(context), arg2.clone(context));
  }

  public String toString()
  {
    return "contains(" + arg1 + "," + arg2 + ")";
  }
  
}

/* BooleanFunction.java -- 
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
 * The <code>boolean</code> function converts its argument to a boolean as
 * follows:
 * <ul>
 * <li>a number is true if and only if it is neither positive or negative
 * zero nor NaN</li>
 * <li>a node-set is true if and only if it is non-empty</li>
 * <li>a string is true if and only if its length is non-zero</li>
 * <li>an object of a type other than the four basic types is converted to a
 * boolean in a way that is dependent on that type</li>
 * </ul>
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class BooleanFunction
  extends Expr
{

  final Expr arg;
  
  BooleanFunction(List args)
  {
    this((Expr) args.get(0));
  }

  BooleanFunction(Expr arg)
  {
    this.arg = arg;
  }
  
  public Object evaluate(Node context, int pos, int len)
  {
    Object val = arg.evaluate(context, pos, len);
    return _boolean(context, val) ? Boolean.TRUE : Boolean.FALSE;
  }

  public Expr clone(Object context)
  {
    return new BooleanFunction(arg.clone(context));
  }
  
  public String toString()
  {
    return "boolean(" + arg + ")";
  }
  
}

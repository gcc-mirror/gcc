/* IdFunction.java -- 
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

import java.util.Collection;
import java.util.List;
import org.w3c.dom.Node;

/**
 * The <code>id</code> function selects elements by their unique ID.
 * When the argument to id is of type node-set, then the result is
 * the union of the result of applying id to the string-value of each of the
 * nodes in the argument node-set. When the argument to id is of any other
 * type, the argument is converted to a string as if by a call to the string
 * function; the string is split into a whitespace-separated list of tokens
 * (whitespace is any sequence of characters matching the production S); the
 * result is a node-set containing the elements in the same document as the
 * context node that have a unique ID equal to any of the tokens in the
 * list.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class IdFunction
  extends Pattern
{

  final Expr arg;

  IdFunction(List args)
  {
    this((Expr) args.get(0));
  }

  public IdFunction(Expr arg)
  {
    this.arg = arg;
  }

  public boolean matches(Node context)
  {
    Object ret = evaluate(context, 1, 1);
    return !((Collection) ret).isEmpty();
  }

  public Object evaluate(Node context, int pos, int len)
  {
    Object val = arg.evaluate(context, pos, len);
    return _id(context, val);
  }

  public Expr clone(Object context)
  {
    return new IdFunction(arg.clone(context));
  }

  public String toString()
  {
    return "id(" + arg + ")";
  }
  
}

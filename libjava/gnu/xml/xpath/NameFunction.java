/* NameFunction.java -- 
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
 * The <code>name</code> function returns a string containing a QName
 * representing the expanded-name of the node in the argument node-set that
 * is first in document order. The QName must represent the expanded-name
 * with respect to the namespace declarations in effect on the node whose
 * expanded-name is being represented. Typically, this will be the QName
 * that occurred in the XML source. This need not be the case if there are
 * namespace declarations in effect on the node that associate multiple
 * prefixes with the same namespace. However, an implementation may include
 * information about the original prefix in its representation of nodes; in
 * this case, an implementation can ensure that the returned string is
 * always the same as the QName used in the XML source. If the argument
 * node-set is empty or the first node has no expanded-name, an empty string
 * is returned. If the argument it omitted, it defaults to a node-set with
 * the context node as its only member.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class NameFunction
  extends Expr
{

  final Expr arg;

  NameFunction(List args)
  {
    this(args.size() > 0 ? (Expr) args.get(0) : null);
  }

  NameFunction(Expr arg)
  {
    this.arg = arg;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    Object val = (arg == null) ? null : arg.evaluate(context, pos, len);
    return _name(context, (Collection) val);
  }

  public Expr clone(Object context)
  {
    return new NameFunction((arg == null) ? null :
                            arg.clone(context));
  }
  
  public String toString()
  {
    return (arg == null) ? "name()" : "name(" + arg + ")";
  }
  
}

/* Constant.java -- 
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

import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * Constant value (string literal or number).
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public final class Constant
  extends Expr
{

  final Object value;

  public Constant(Object value)
  {
    this.value = value;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    return value;
  }

  public Expr clone(Object context)
  {
    return new Constant(value);
  }

  public boolean references(QName var)
  {
    return false;
  }

  public String toString()
  {
    String ret = value.toString();
    if (value instanceof String)
      {
        if (ret.indexOf('\'') == -1)
          {
            return '\'' + ret + '\'';
          }
        else
          {
            return '"' + ret + '"';
          }
      }
    if (value instanceof Double)
      {
        if (ret.endsWith(".0"))
          {
            ret = ret.substring(0, ret.length() - 2);
          }
      }
    return ret;
  }
  
} 

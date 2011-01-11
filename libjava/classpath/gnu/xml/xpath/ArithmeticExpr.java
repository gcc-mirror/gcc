/* ArithmeticExpr.java --
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

import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * Binary arithmetic expression.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class ArithmeticExpr
  extends Expr
{

  static final int ADD = 0;
  static final int SUBTRACT = 1;
  static final int MULTIPLY = 2;
  static final int DIVIDE = 3;
  static final int MODULO = 4;

  final Expr lhs;
  final Expr rhs;
  final int op;

  ArithmeticExpr(Expr lhs, Expr rhs, int op)
  {
    this.lhs = lhs;
    this.rhs = rhs;
    switch (op)
      {
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case MODULO:
                                this.op = op;
                                break;
      default:
        throw new IllegalArgumentException();
      }
  }

  public Object evaluate(Node context, int pos, int len)
  {
    Object left = lhs.evaluate(context, pos, len);
    Object right = rhs.evaluate(context, pos, len);

    double ln = _number(context, left);
    double rn = _number(context, right);
    switch (op)
      {
      case ADD:
        return new Double(ln + rn);
      case SUBTRACT:
        return new Double(ln - rn);
      case MULTIPLY:
        return new Double(ln * rn);
      case DIVIDE:
        if (rn == 0.0d || rn == -0.0d)
          {
            if (ln == 0.0d || ln == -0.0d)
              {
                return new Double(Double.NaN);
              }
            else
              {
                return new Double(ln < 0.0d ?
                                  Double.NEGATIVE_INFINITY :
                                  Double.POSITIVE_INFINITY);
              }
          }
        return new Double(ln / rn);
      case MODULO:
        if (rn == 0.0d || rn == 0.0d)
          {
            if (ln == 0.0d || ln == -0.0d)
              {
                return new Double(Double.NaN);
              }
            else
              {
                return new Double(ln < 0.0d ?
                                  Double.NEGATIVE_INFINITY :
                                  Double.POSITIVE_INFINITY);
              }
          }
        return new Double(ln % rn);
      default:
        throw new IllegalStateException();
      }
  }

  public Expr clone(Object context)
  {
    return new ArithmeticExpr(lhs.clone(context), rhs.clone(context), op);
  }

  public boolean references(QName var)
  {
    return (lhs.references(var) || rhs.references(var));
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder();
    buf.append(lhs);
    buf.append(' ');
    switch (op)
      {
      case ADD:
        buf.append('+');
        break;
      case SUBTRACT:
        buf.append('-');
        break;
      case MULTIPLY:
        buf.append('*');
        break;
      case DIVIDE:
        buf.append("div");
        break;
      case MODULO:
        buf.append("mod");
        break;
      }
    buf.append(' ');
    buf.append(rhs);
    return buf.toString();
  }

}

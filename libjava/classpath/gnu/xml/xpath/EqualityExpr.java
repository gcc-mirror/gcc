/* EqualityExpr.java --
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
import javax.xml.namespace.QName;
import org.w3c.dom.Node;

/**
 * Boolean equality expression.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class EqualityExpr
  extends Expr
{

  final Expr lhs;
  final Expr rhs;
  final boolean invert;

  EqualityExpr(Expr lhs, Expr rhs, boolean invert)
  {
    this.lhs = lhs;
    this.rhs = rhs;
    this.invert = invert;
  }

  @Override
  public Object evaluate(Node context, int pos, int len)
  {
    boolean val = evaluateImpl(context, pos, len);
    if (invert)
      {
        return val ? Boolean.FALSE : Boolean.TRUE;
      }
    else
      {
        return val ? Boolean.TRUE : Boolean.FALSE;
      }
  }

  private boolean evaluateImpl(Node context, int pos, int len)
  {
    Object left = lhs.evaluate(context, pos, len);
    Object right = rhs.evaluate(context, pos, len);

    /*
     * If both objects to be compared are node-sets, then the comparison
     * will be true if and only if there is a node in the first node-set and
     * a node in the second node-set such that the result of performing the
     * comparison on the string-values of the two nodes is true.
     */
    boolean flns = left instanceof Collection;
    boolean frns = right instanceof Collection;
    if (flns && frns)
      {
        /* Suppression is safe, as we know context produces Collection<Node> */
        @SuppressWarnings("unchecked")
          Collection<Node> lns = (Collection<Node>) left;
        @SuppressWarnings("unchecked")
          Collection<Node> rns = (Collection<Node>) right;
        if (lns.isEmpty())
          {
            return false;
          }
        boolean all = true;
        for (Node ltest : lns)
          {
            for (Node rtest : rns)
              {
                if (ltest == rtest || ltest.equals(rtest))
                  {
                    // much shorter
                    if (!invert)
                      {
                        return true;
                      }
                  }
                else if (stringValue(ltest).equals(stringValue(rtest)))
                  {
                    if (!invert)
                      {
                        return true;
                      }
                  }
                else
                  {
                    all = false;
                  }
              }
          }
        return all;
      }
    /*
     * If one object to be compared is a node-set and the other is a number,
     * then the comparison will be true if and only if there is a node in
     * the node-set such that the result of performing the comparison on the
     * number to be compared and on the result of converting the
     * string-value of that node to a number using the number function is
     * true.
     */
    boolean fln = left instanceof Double;
    boolean frn = right instanceof Double;
    if ((flns && frn) || (frns && fln))
      {
        /* Suppression is safe, as we know context produces Collection<Node> */
        @SuppressWarnings("unchecked")
          Collection<Node> ns = flns ? (Collection<Node>) left : (Collection<Node>) right;
        double n = fln ? ((Double) left).doubleValue() :
          ((Double) right).doubleValue();
        boolean all = true;
        for (Node test : ns)
          {
            double nn = _number(context, stringValue(test));
            if (nn == n)
              {
                if (!invert)
                  {
                    return true;
                  }
              }
            else
              {
                all = false;
              }
          }
        return invert ? all : false;
      }
    /*
     * If one object to be compared is a node-set and the other is a
     * string, then the comparison will be true if and only if there is a
     * node in the node-set such that the result of performing the
     * comparison on the string-value of the node and the other string is
     * true.
     */
    boolean fls = left instanceof String;
    boolean frs = right instanceof String;
    if ((flns && frs) || (frns && fls))
      {
        /* Suppression is safe, as we know context produces Collection<Node> */
        @SuppressWarnings("unchecked")
          Collection<Node> ns = flns ? (Collection<Node>) left : (Collection<Node>) right;
        String s = fls ? (String) left : (String) right;
        boolean all = true;
        for (Node test : ns)
          {
            if (stringValue(test).equals(s))
              {
                if (!invert)
                  {
                    return true;
                  }
              }
            else
              {
                all = false;
              }
          }
        return invert ? all : false;
      }
    /*
     * If one object to be compared is a node-set and the other is a
     * boolean, then the comparison will be true if and only if the result
     * of performing the comparison on the boolean and on the result of
     * converting the node-set to a boolean using the boolean function is
     * true.
     */
    boolean flb = left instanceof Boolean;
    boolean frb = right instanceof Boolean;
    if ((flns && frb) || (frns && flb))
      {
        /* Suppression is safe, as we know context produces Collection<Node> */
        @SuppressWarnings("unchecked")
          Collection<Node> ns = flns ? (Collection<Node>) left : (Collection<Node>) right;
        boolean b = flb ? ((Boolean) left).booleanValue() :
          ((Boolean) right).booleanValue();
        return _boolean(context, ns) == b;
      }
    /*
     * If at least one object to be compared is a boolean, then each object
     * to be compared is converted to a boolean as if by applying the
     * boolean function.
     */
    if (flb || frb)
      {
        boolean lb = flb ? ((Boolean) left).booleanValue() :
          _boolean(context, left);
        boolean rb = frb ? ((Boolean) right).booleanValue() :
          _boolean(context, right);
        return lb == rb;
      }
    /*
     * Otherwise, if at least one object to be compared is
     * a number, then each object to be compared is converted to a number as
     * if by applying the number function.
     */
    if (fln || frn)
      {
        double ln = fln ? ((Double) left).doubleValue() :
          _number(context, left);
        double rn = frn ? ((Double) right).doubleValue() :
          _number(context, right);
        return ln == rn;
      }
    /*
     * Otherwise, both objects to be
     * compared are converted to strings as if by applying the string
     * function.
     */
    String ls = fls ? (String) left : _string(context, left);
    String rs = frs ? (String) right : _string(context, right);
    return ls.equals(rs);
  }

  public Expr clone(Object context)
  {
    return new EqualityExpr(lhs.clone(context), rhs.clone(context), invert);
  }

  public boolean references(QName var)
  {
    return (lhs.references(var) || rhs.references(var));
  }

  public String toString()
  {
    if (invert)
      {
        return lhs + " != " + rhs;
      }
    else
      {
        return lhs + " = " + rhs;
      }
  }

}

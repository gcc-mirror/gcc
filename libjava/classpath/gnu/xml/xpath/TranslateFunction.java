/* TranslateFunction.java -- 
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
 * The <code>translate</code> function returns the first argument string
 * with occurrences of characters in the second argument string replaced by
 * the character at the corresponding position in the third argument string.
 * For example, translate("bar","abc","ABC") returns the string BAr. If
 * there is a character in the second argument string with no character at a
 * corresponding position in the third argument string (because the second
 * argument string is longer than the third argument string), then
 * occurrences of that character in the first argument string are removed.
 * For example, translate("--aaa--","abc-","ABC") returns "AAA". If a
 * character occurs more than once in the second argument string, then the
 * first occurrence determines the replacement character. If the third
 * argument string is longer than the second argument string, then excess
 * characters are ignored.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class TranslateFunction
  extends Expr
{

  final Expr arg1;
  final Expr arg2;
  final Expr arg3;

  TranslateFunction(List args)
  {
    this((Expr) args.get(0), (Expr) args.get(1), (Expr) args.get(2));
  }

  TranslateFunction(Expr arg1, Expr arg2, Expr arg3)
  {
    this.arg1 = arg1;
    this.arg2 = arg2;
    this.arg3 = arg3;
  }

  public Object evaluate(Node context, int pos, int len)
  {
    Object val1 = arg1.evaluate(context, pos, len);
    Object val2 = arg2.evaluate(context, pos, len);
    Object val3 = arg3.evaluate(context, pos, len);
    String string = _string(context, val1);
    String search = _string(context, val2);
    String replace = _string(context, val3);
    StringBuffer buf = new StringBuffer();
    int l1 = string.length();
    int l2 = search.length();
    int l3 = replace.length();
    for (int i = 0; i < l1; i++)
      {
        char c = string.charAt(i);
        boolean replaced = false;
        for (int j = 0; j < l2; j++)
          {
            if (c == search.charAt(j))
              {
                if (j < l3)
                  {
                    buf.append(replace.charAt(j));
                  }
                replaced = true;
              }
          }
        if (!replaced)
          {
            buf.append(c);
          } 
      } 
    return new String(buf);
  }

  public Expr clone(Object context)
  {
    return new TranslateFunction(arg1.clone(context), arg2.clone(context),
                                 arg3.clone(context));
  }

  public boolean references(QName var)
  {
    return (arg1.references(var) || arg2.references(var) ||
            arg3.references(var));
  }
  
  public String toString()
  {
    return "translate(" + arg1 + "," + arg2 + "," + arg3 + ")";
  }
  
}

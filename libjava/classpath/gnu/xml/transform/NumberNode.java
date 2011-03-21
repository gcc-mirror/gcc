/* NumberNode.java --
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

package gnu.xml.transform;

import javax.xml.transform.TransformerException;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * A template node representing the XSL <code>number</code> instruction
 * with a <code>value</code> expression.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class NumberNode
  extends AbstractNumberNode
{

  final Expr value;

  NumberNode(Expr value, TemplateNode format, String lang,
             int letterValue, String groupingSeparator, int groupingSize)
  {
    super(format, lang, letterValue, groupingSeparator, groupingSize);
    this.value = value;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new NumberNode(value.clone(stylesheet),
                                      format, lang, letterValue,
                                      groupingSeparator, groupingSize);
    if (children != null)
      {
        ret.children = children.clone(stylesheet);
      }
    if (next != null)
      {
        ret.next = next.clone(stylesheet);
      }
    return ret;
  }

  int[] compute(Stylesheet stylesheet, Node context, int pos, int len)
    throws TransformerException
  {
    Object ret = value.evaluate(context, pos, len);
    Double d = (ret instanceof Double) ? ((Double) ret) :
      new Double(Expr._number(context, ret));
    return new int[] { d.intValue() };
  }

}

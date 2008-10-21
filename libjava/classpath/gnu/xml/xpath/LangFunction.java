/* LangFunction.java -- 
   Copyright (C) 2004,2006 Free Software Foundation, Inc.

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
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The <code>lang</code> function returns true or false depending on whether
 * the language of the context node as specified by xml:lang attributes is
 * the same as or is a sublanguage of the language specified by the argument
 * string. The language of the context node is determined by the value of
 * the xml:lang attribute on the context node, or, if the context node has
 * no xml:lang attribute, by the value of the xml:lang attribute on the
 * nearest ancestor of the context node that has an xml:lang attribute. If
 * there is no such attribute, then lang returns false. If there is such an
 * attribute, then lang returns true if the attribute value is equal to the
 * argument ignoring case, or if there is some suffix starting with - such
 * that the attribute value is equal to the argument ignoring that suffix of
 * the attribute value and ignoring case.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class LangFunction
  extends Expr
{

  final Expr arg;

  LangFunction(List<Expr> args)
  {
    this(args.get(0));
  }

  LangFunction(Expr arg)
  {
    this.arg = arg;
  }

  @Override
  public Object evaluate(Node context, int pos, int len)
  {
    Object val = arg.evaluate(context, pos, len);
    String lang = _string(context, val);
    String clang = getLang(context);
    while (clang == null && context != null)
      {
        context = context.getParentNode();
        clang = getLang(context);
      }
    boolean ret = (clang == null) ? false :
      clang.toLowerCase().startsWith(lang.toLowerCase());
    return ret ? Boolean.TRUE : Boolean.FALSE;
  }

  String getLang(Node node)
  {
    while (node != null)
      {
        if (node.getNodeType() == Node.ELEMENT_NODE)
          {
            String lang = ((Element) node).getAttribute("xml:lang");
            if (lang != null)
              return lang;
          }
        node = node.getParentNode();
      }
    return null;
  }
  
  public Expr clone(Object context)
  {
    return new IdFunction(arg.clone(context));
  }

  public boolean references(QName var)
  {
    return arg.references(var);
  }

  public String toString()
  {
    return "lang(" + arg + ")";
  }
  
}

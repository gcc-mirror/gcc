/* ParameterNode.java -- 
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

import java.util.Collections;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;
import gnu.xml.xpath.Expr;

/**
 * A template node that sets a variable or parameter during template
 * processing.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class ParameterNode
  extends TemplateNode
  implements Comparable
{

  final QName name;
  final Expr select;
  final int type;

  ParameterNode(QName name, Expr select, int type)
  {
    this.name = name;
    this.select = select;
    this.type = type;
  }

  TemplateNode clone(Stylesheet stylesheet)
  {
    TemplateNode ret = new ParameterNode(name,
                                         select.clone(stylesheet),
                                         type);
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

  void doApply(Stylesheet stylesheet, QName mode,
               Node context, int pos, int len,
               Node parent, Node nextSibling)
    throws TransformerException
  {
    // push the variable context
    stylesheet.bindings.push(type);
    // set the variable
    Object value = getValue(stylesheet, mode, context, pos, len);
    if (value != null)
      {
        stylesheet.bindings.set(name, value, type);
        if (stylesheet.debug)
          {
            System.err.println(this + ": set to " + value);
          }
      }
    // variable and param don't process children as such
    // all subsequent instructions are processed with that variable context
    if (next != null)
      {
        next.apply(stylesheet, mode,
                   context, pos, len,
                   parent, nextSibling);
      }
    // pop the variable context
    stylesheet.bindings.pop(type);
  }
  
  Object getValue(Stylesheet stylesheet, QName mode,
                  Node context, int pos, int len)
    throws TransformerException
  {
    if (select != null)
      {
        return select.evaluate(context, pos, len);
      }
    else if (children != null)
      {
        Document doc = (context instanceof Document) ? (Document) context :
          context.getOwnerDocument();
        DocumentFragment fragment = doc.createDocumentFragment();
        children.apply(stylesheet, mode, context, pos, len, fragment, null);
        return Collections.singleton(fragment);
      }
    else
      {
        return null;
      }
  }
  
  public boolean references(QName var)
  {
    if (select != null && select.references(var))
      {
        return true;
      }
    return super.references(var);
  }

  public int compareTo(Object other)
  {
    if (other instanceof ParameterNode)
      {
        ParameterNode pn = (ParameterNode) other;
        boolean r1 = references(pn.name);
        boolean r2 = pn.references(name);
        if (r1 && r2)
          {
            throw new IllegalArgumentException("circular definitions");
          }
        if (r1)
          {
            return 1;
          }
        if (r2)
          {
            return -1;
          }
      }
    return 0;
  }
  
  public String toString()
  {
    StringBuffer buf = new StringBuffer(getClass().getName());
    buf.append('[');
    buf.append("name=");
    buf.append(name);
    if (select != null)
      {
        buf.append(",select=");
        buf.append(select);
      }
    buf.append(",type=");
    switch (type)
      {
      case Bindings.VARIABLE:
        buf.append("variable");
        break;
      case Bindings.PARAM:
        buf.append("param");
        break;
      case Bindings.WITH_PARAM:
        buf.append("with-param");
        break;
      }
    buf.append(']');
    return buf.toString();
  }

}


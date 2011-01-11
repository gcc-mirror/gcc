/* WithParam.java --
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
 * A specification for setting a variable or parameter during template
 * processing with <code>apply-templates</code> or
 * <code>call-template</code>.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class WithParam
{

  final QName name;
  final Expr select;
  final TemplateNode content;

  WithParam(QName name, Expr select)
  {
    this.name = name;
    this.select = select;
    content = null;
  }

  WithParam(QName name, TemplateNode content)
  {
    this.name = name;
    this.content = content;
    select = null;
  }

  Object getValue(Stylesheet stylesheet, QName mode,
                  Node context, int pos, int len)
    throws TransformerException
  {
    if (select != null)
      {
        return select.evaluate(context, pos, len);
      }
    else if (content == null)
      {
        return "";
      }
    else
      {
        Document doc = (context instanceof Document) ? (Document) context :
          context.getOwnerDocument();
        DocumentFragment fragment = doc.createDocumentFragment();
        content.apply(stylesheet, mode,
                      context, pos, len,
                      fragment, null);
        return Collections.singleton(fragment);
      }
  }

  WithParam clone(Stylesheet stylesheet)
  {
    if (content == null)
      {
        return new WithParam(name,
                             select.clone(stylesheet));
      }
    else
      {
        return new WithParam(name,
                             content.clone(stylesheet));
      }
  }

  boolean references(QName var)
  {
    if (select != null && select.references(var))
      {
        return true;
      }
    if (content != null && content.references(var))
      {
        return true;
      }
    return false;
  }

}

/* Bindings.java -- 
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

package gnu.xml.transform;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathVariableResolver;
import org.w3c.dom.Node;

/**
 * The set of variable bindings in effect for a stylesheet.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class Bindings
  implements XPathVariableResolver, Cloneable
{

  final Stylesheet stylesheet;

  /**
   * Global variables.
   */
  final LinkedList variables;

  /**
   * Parameter value stack.
   */
  final LinkedList parameters;

  Bindings(Stylesheet stylesheet)
  {
    this.stylesheet = stylesheet;
    variables = new LinkedList();
    parameters = new LinkedList();
    push(true);
    push(false);
  }

  public Object clone()
  {
    try
      {
        return (Bindings) super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw new Error(e.getMessage());
      }
  }

  void push(boolean global)
  {
    if (global)
      {
        variables.addFirst(new HashMap());
      }
    else
      {
        parameters.addFirst(new HashMap());
      }
  }

  void pop(boolean global)
  {
    if (global)
      {
        variables.removeFirst();
      }
    else
      {
        parameters.removeFirst();
      }
  }

  public boolean containsKey(String name, boolean global)
  {
    Iterator i = global ? variables.iterator() : parameters.iterator();
    while (i.hasNext())
      {
        Map ctx = (Map) i.next();
        if (ctx.containsKey(name))
          {
            return true;
          }
      }
    return false;
  }

  public Object get(String name, Node context, int pos, int len)
  {
    //System.err.println("bindings.get: "+name);
    //System.err.println("\t"+toString());
    Object ret = null;
    for (Iterator i = variables.iterator(); i.hasNext() && ret == null; )
      {
        Map vctx = (Map) i.next();
        ret = vctx.get(name);
      }
    if (ret == null)
      {
        for (Iterator i = parameters.iterator(); i.hasNext() && ret == null; )
          {
            Map pctx = (Map) i.next();
            ret = pctx.get(name);
          }
      }
    /*if (ret instanceof Expr && context != null)
      {
        Expr expr = (Expr) ret;
        ret = expr.evaluate(context, 1, 1);
      }*/
    if (ret instanceof Node)
      {
        ret = Collections.singleton(ret);
      }
    if (ret == null)
      {
        ret = "";
      }
    //System.err.println("\tret="+ret);
    return ret;
  }

  void set(String name, Object value, boolean global)
  {
    if (global)
      {
        Map context = (Map) variables.getFirst();
        context.put(name, value);
      }
    else
      {
        Map context = (Map) parameters.getFirst();
        context.put(name, value);
      }
  }

  public Object resolveVariable(QName qName)
  {
    return get(qName.toString(), null, 1, 1);
  }
  
  public String toString()
  {
    StringBuffer buf = new StringBuffer();
    boolean next = false;
    Collection seen = new HashSet();
    buf.append('{');
    for (Iterator i = variables.iterator(); i.hasNext(); )
      {
        Map ctx = (Map) i.next();
        for (Iterator j = ctx.entrySet().iterator(); j.hasNext(); )
          {
            if (next)
              {
                buf.append(',');
              }
            else
              {
                next = true;
              }
            Map.Entry entry = (Map.Entry) j.next();
            Object key = entry.getKey();
            if (!seen.contains(key))
              {
                buf.append(key);
                buf.append('=');
                buf.append(entry.getValue());
                seen.add(key);
              }
          } 
      }
    for (Iterator i = parameters.iterator(); i.hasNext(); )
      {
        Map ctx = (Map) i.next();
        for (Iterator j = ctx.entrySet().iterator(); j.hasNext(); )
          {
            if (next)
              {
                buf.append(',');
              }
            else
              {
                next = true;
              }
            Map.Entry entry = (Map.Entry) j.next();
            Object key = entry.getKey();
            if (!seen.contains(key))
              {
                buf.append(key);
                buf.append('=');
                buf.append(entry.getValue());
                seen.add(key);
              }
          } 
      }
    buf.append('}');
    return buf.toString();
  }
}

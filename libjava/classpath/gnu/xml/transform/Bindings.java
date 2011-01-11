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

import gnu.java.lang.CPStringBuilder;

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

  static final int VARIABLE = 0;
  static final int PARAM = 1;
  static final int WITH_PARAM = 2;

  final Stylesheet stylesheet;

  /**
   * Global variables.
   */
  final LinkedList<Map<QName,Object>> variables;

  /**
   * Parameter value stack.
   */
  final LinkedList<Map<QName,Object>> parameters;

  /**
   * Argument (with-param) value stack.
   */
  final LinkedList<Map<QName,Object>> withParameters;

  /**
   * Only search globals.
   */
  boolean global;

  Bindings(Stylesheet stylesheet)
  {
    this.stylesheet = stylesheet;
    variables = new LinkedList<Map<QName,Object>>();
    parameters = new LinkedList<Map<QName,Object>>();
    withParameters = new LinkedList<Map<QName,Object>>();
    for (int i = 0; i < 3; i++)
      {
        push(i);
      }
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

  void push(int type)
  {
    switch (type)
      {
      case VARIABLE:
        variables.addFirst(new HashMap<QName,Object>());
        break;
      case PARAM:
        parameters.addFirst(new HashMap<QName,Object>());
        break;
      case WITH_PARAM:
        withParameters.addFirst(new HashMap<QName,Object>());
        break;
      }
  }

  void pop(int type)
  {
    switch (type)
      {
      case VARIABLE:
        variables.removeFirst();
        break;
      case PARAM:
        parameters.removeFirst();
        break;
      case WITH_PARAM:
        withParameters.removeFirst();
        break;
      }
  }

  public boolean containsKey(QName name, int type)
  {
    if (global)
      {
        Map<QName,Object> ctx1 = variables.getLast();
        Map<QName,Object> ctx2 = parameters.getLast();
        return (ctx1.containsKey(name) || ctx2.containsKey(name));
      }
    Iterator<Map<QName,Object>> i = null;
    switch (type)
      {
      case VARIABLE:
        i = variables.iterator();
        break;
      case PARAM:
        i = parameters.iterator();
        break;
      case WITH_PARAM:
        Map<QName,Object> ctx = withParameters.getFirst();
        return ctx.containsKey(name);
      }
    if (i != null)
      {
        while (i.hasNext())
          {
            Map<QName,Object> ctx = i.next();
            if (ctx.containsKey(name))
              {
                return true;
              }
          }
      }
    return false;
  }

  public Object get(QName name, Node context, int pos, int len)
  {
    if (global)
      {
        Map<QName,Object> ctx = variables.getLast();
        Object ret = ctx.get(name);
        if (ret == null)
          {
            ctx = parameters.getLast();
            ret = ctx.get(name);
          }
        return ret;
      }
    //System.err.println("bindings.get: "+name);
    //System.err.println("\t"+toString());
    Object ret = null;
    //if (parameters.size() > 1 && containsKey(name, PARAM))
      // check that template defines parameter
      {
        Map<QName,Object> cwp = withParameters.getFirst();
        ret = cwp.get(name);
        //System.err.println("\twith-param: ret="+ret);
      }
    if (ret == null)
      {
        for (Iterator<Map<QName,Object>> i = variables.iterator();
             i.hasNext() && ret == null; )
          {
            Map<QName,Object> vctx = i.next();
            ret = vctx.get(name);
          }
        //System.err.println("\tvariable: ret="+ret);
      }
    if (ret == null)
      {
        for (Iterator<Map<QName,Object>> i = parameters.iterator();
             i.hasNext() && ret == null; )
          {
            Map<QName,Object> pctx = i.next();
            ret = pctx.get(name);
          }
        //System.err.println("\tparam: ret="+ret);
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

  void set(QName name, Object value, int type)
  {
    switch (type)
      {
      case VARIABLE:
        Map<QName,Object> vctx = variables.getFirst();
        vctx.put(name, value);
        break;
      case PARAM:
        Map<QName,Object> pctx = parameters.getFirst();
        pctx.put(name, value);
        break;
      case WITH_PARAM:
        Map<QName,Object> wctx = withParameters.getFirst();
        wctx.put(name, value);
        break;
      }
    //System.err.println("Set "+name+"="+value);
  }

  public Object resolveVariable(QName qName)
  {
    return get(qName, null, 1, 1);
  }

  public String toString()
  {
    CPStringBuilder buf = new CPStringBuilder();
    boolean next = false;
    Collection<QName> seen = new HashSet<QName>();
    Map<QName,Object> wctx = withParameters.getFirst();
    buf.append('(');
    for (Map.Entry<QName,Object> entry : wctx.entrySet())
      {
        if (next)
          {
            buf.append(',');
          }
        else
          {
            next = true;
          }
        QName key = entry.getKey();
        if (!seen.contains(key))
          {
            buf.append(key);
            buf.append('=');
            buf.append(entry.getValue());
            seen.add(key);
          }
      }
    buf.append(')');
    next = false;
    seen.clear();
    buf.append('{');
    for (Map<QName,Object> ctx : variables)
      {
        for (Map.Entry<QName,Object> entry : ctx.entrySet())
          {
            if (next)
              {
                buf.append(',');
              }
            else
              {
                next = true;
              }
            QName key = entry.getKey();
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
    next = false;
    seen.clear();
    buf.append('[');
    for (Map<QName,Object> ctx : parameters)
      {
        for (Map.Entry<QName,Object> entry : ctx.entrySet())
          {
            if (next)
              {
                buf.append(',');
              }
            else
              {
                next = true;
              }
            QName key = entry.getKey();
            if (!seen.contains(key))
              {
                buf.append(key);
                buf.append('=');
                buf.append(entry.getValue());
                seen.add(key);
              }
          }
      }
    buf.append(']');
    return buf.toString();
  }

}

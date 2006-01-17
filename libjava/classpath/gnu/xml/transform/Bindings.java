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
  final LinkedList variables;

  /**
   * Parameter value stack.
   */
  final LinkedList parameters;

  /**
   * Argument (with-param) value stack.
   */
  final LinkedList withParameters;

  /**
   * Only search globals.
   */
  boolean global;

  Bindings(Stylesheet stylesheet)
  {
    this.stylesheet = stylesheet;
    variables = new LinkedList();
    parameters = new LinkedList();
    withParameters = new LinkedList();
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
        variables.addFirst(new HashMap());
        break;
      case PARAM:
        parameters.addFirst(new HashMap());
        break;
      case WITH_PARAM:
        withParameters.addFirst(new HashMap());
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
        Map ctx1 = (Map) variables.getLast();
        Map ctx2 = (Map) parameters.getLast();
        return (ctx1.containsKey(name) || ctx2.containsKey(name));
      }
    Iterator i = null;
    switch (type)
      {
      case VARIABLE:
        i = variables.iterator();
        break;
      case PARAM:
        i = parameters.iterator();
        break;
      case WITH_PARAM:
        Map ctx = (Map) withParameters.getFirst();
        return ctx.containsKey(name);
      }
    if (i != null)
      {
        while (i.hasNext())
          {
            Map ctx = (Map) i.next();
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
        Map ctx = (Map) variables.getLast();
        Object ret = ctx.get(name);
        if (ret == null)
          {
            ctx = (Map) parameters.getLast();
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
        Map cwp = (Map) withParameters.getFirst();
        ret = cwp.get(name);
        //System.err.println("\twith-param: ret="+ret);
      }
    if (ret == null)
      {
        for (Iterator i = variables.iterator(); i.hasNext() && ret == null; )
          {
            Map vctx = (Map) i.next();
            ret = vctx.get(name);
          }
        //System.err.println("\tvariable: ret="+ret);
      }
    if (ret == null)
      {
        for (Iterator i = parameters.iterator(); i.hasNext() && ret == null; )
          {
            Map pctx = (Map) i.next();
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
        Map vctx = (Map) variables.getFirst();
        vctx.put(name, value);
        break;
      case PARAM:
        Map pctx = (Map) parameters.getFirst();
        pctx.put(name, value);
        break;
      case WITH_PARAM:
        Map wctx = (Map) withParameters.getFirst();
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
    StringBuffer buf = new StringBuffer();
    boolean next = false;
    Collection seen = new HashSet();
    Map wctx = (Map) withParameters.getFirst();
    buf.append('(');
    for (Iterator i = wctx.entrySet().iterator(); i.hasNext(); )
      {
        if (next)
          {
            buf.append(',');
          }
        else
          {
            next = true;
          }
        Map.Entry entry = (Map.Entry) i.next();
        Object key = entry.getKey();
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
    buf.append('}');
    next = false;
    seen.clear();
    buf.append('[');
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
    buf.append(']');
    return buf.toString();
  }

}

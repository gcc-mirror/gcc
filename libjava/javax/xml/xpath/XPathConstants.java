/* XPathConstants.java -- 
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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

package javax.xml.xpath;

import javax.xml.namespace.QName;

/**
 * XPath constants.
 *
 * @author (a href='mailto:dog@gnu.org'>Chris Burdess</a)
 * @since 1.3
 */
public class XPathConstants
{

  /**
   * The XPath 1.0 number data type.
   */
  public static final QName NUMBER =
    new QName("http://java.sun.com/jaxp/xpath/dom#number", "");
  
  /**
   * The XPath 1.0 string data type.
   */
  public static final QName STRING =
    new QName("http://java.sun.com/jaxp/xpath/dom#string", "");
  
  /**
   * The XPath 1.0 boolean data type.
   */
  public static final QName BOOLEAN =
    new QName("http://java.sun.com/jaxp/xpath/dom#boolean", "");
  
  /**
   * The XPath 1.0 node-set data type.
   */
  public static final QName NODESET =
    new QName("http://java.sun.com/jaxp/xpath/dom#node-set", "");
  
  /**
   * The XPath 1.0 node data type.
   */
  public static final QName NODE =
    new QName("http://java.sun.com/jaxp/xpath/dom#node", "");

  /**
   * The URL for the JAXP DOM object model.
   */
  public static final String DOM_OBJECT_MODEL =
    "http://java.sun.com/jaxp/xpath/dom";
  
}

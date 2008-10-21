/* GnomeText.java - 
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

package gnu.xml.libxmlj.dom;

import gnu.java.lang.CPStringBuilder;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * A DOM text node implemented in libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class GnomeText
extends GnomeCharacterData
implements Text
{

  GnomeText (Object id)
  {
    super (id);
  }
  
  public Text splitText (int offset)
    throws DOMException
  {
    String value = getNodeValue ();
    String part1 = value.substring (0, offset);
    String part2 = value.substring (offset);
    Text text = getOwnerDocument ().createTextNode (part1);
    getParentNode ().insertBefore (text, this);
    setNodeValue (part2);
    return text;
  }
  
  // DOM Level 3

  public boolean isElementContentWhitespace ()
  {
    return getTextContent ().trim ().length () == 0;
  }
  
  public String getWholeText ()
  {
    Node first = this;
    Node node = getPreviousSibling ();
    while (node != null && node instanceof Text)
      {
        first = node;
        node = node.getPreviousSibling ();
      }
    CPStringBuilder buf = new CPStringBuilder (first.getNodeValue ());
    node = first.getNextSibling ();
    while (node != null && node instanceof Text)
      {
        buf.append (node.getNodeValue ());
        node = node.getNextSibling ();
      }
    return buf.toString ();
  }

  public Text replaceWholeText (String content) throws DOMException
  {
    boolean isEmpty = (content == null || content.length () == 0);
    if (!isEmpty)
      {
        setNodeValue (content);
      }
    
    Node first = this;
    Node node = getPreviousSibling ();
    while (node != null && node instanceof Text)
      {
        first = node;
        node = node.getPreviousSibling ();
      }
    node = first.getNextSibling ();
    Node parent = getParentNode ();
    if (first != this || isEmpty)
      {
        parent.removeChild (first);
      }
    while (node != null && node instanceof Text)
      {
        Node tmp = node;
        node = node.getNextSibling ();
        if (tmp != this || isEmpty)
          {
            parent.removeChild (tmp);
          }
      }
    return (isEmpty) ? null : this;
  }

}

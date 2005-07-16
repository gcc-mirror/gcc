/* GnomeXPathResult.java - 
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

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;
import org.w3c.dom.xpath.XPathException;
import org.w3c.dom.xpath.XPathResult;

/**
 * An XPath result object implemented in libxml2.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
class GnomeXPathResult
implements XPathResult
{

  /**
   * xmlXPathObjectPtr
   */
  final Object obj;

  GnomeXPathResult (Object obj)
  {
    this.obj = obj;
  }

  protected void finalize ()
  {
    free (obj);
  }

  private native void free (Object obj);
  
  public native short getResultType ();

  public native double getNumberValue ()
    throws XPathException;

  public native String getStringValue ()
    throws XPathException;

  public native boolean getBooleanValue ()
    throws XPathException;

  public native Node getSingleNodeValue ()
    throws XPathException;

  public native boolean getInvalidIteratorState();

  public native int getSnapshotLength ()
    throws XPathException;

  public native Node iterateNext ()
    throws XPathException, DOMException;

  public native Node snapshotItem (int index)
    throws XPathException;

  public String toString ()
  {
    short type = getResultType ();
    switch (type)
      {
      case STRING_TYPE:
        return getStringValue ();
      case NUMBER_TYPE:
        return new Double (getNumberValue ()).toString ();
      case BOOLEAN_TYPE:
        return Boolean.valueOf (getBooleanValue ()).toString ();
      case UNORDERED_NODE_SNAPSHOT_TYPE:
        int len = getSnapshotLength ();
        switch (len) {
        case 0:
          return "[no matches]";
        case 1:
          return getSingleNodeValue ().toString ();
        default:
          StringBuffer buffer = new StringBuffer ();
          for (int i = 0; i < len; i++)
            {
              if (i > 0)
                {
                  buffer.append (',');
                }
              buffer.append (snapshotItem (i));
            }
          return buffer.toString ();
        }
      default:
        return getClass ().getName () + "[type=" + type + ",length=" +
          getSnapshotLength () + ']';
      }
  }
  
}

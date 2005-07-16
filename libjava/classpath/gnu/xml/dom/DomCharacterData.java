/* DomCharacterData.java -- 
   Copyright (C) 1999,2000,2001,2004 Free Software Foundation, Inc.

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

package gnu.xml.dom;

import org.w3c.dom.CharacterData;
import org.w3c.dom.DOMException;
import org.w3c.dom.events.MutationEvent;


/**
 * <p> Abstract "CharacterData" implementation.  This
 * facilitates reusing code in classes implementing subtypes of that DOM
 * interface (Text, Comment, CDATASection).  </p>
 *
 * @author David Brownell
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public abstract class DomCharacterData
  extends DomNode
  implements CharacterData
{

  private String text;
  
  // package private
  DomCharacterData(short nodeType, DomDocument doc, String value)
  {
    super(nodeType, doc);
    text = (value == null) ? "" : value;
  }

  // package private
  DomCharacterData(short nodeType, DomDocument doc,
                   char[] buf, int offset, int length)
  {
    super(nodeType, doc);
    text = (buf == null) ? "" : new String(buf, offset, length);
  }

  /**
   * <b>DOM L1</b>
   * Appends the specified data to the value of this node.
   * Causes a DOMCharacterDataModified mutation event to be reported.
   */
  public void appendData(String arg)
  {
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    String value = text + arg;
    mutating(value);
    text = value;
  }
  
  /**
   * <b>DOM L1</b>
   * Modifies the value of this node.
   * Causes a DOMCharacterDataModified mutation event to be reported.
   */
  public void deleteData(int offset, int count)
  {
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    char[] raw = text.toCharArray();
    if (offset < 0 || count < 0 || offset > raw.length)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
    if ((offset + count) > raw.length)
      {
        count = raw.length - offset;
      }
    if (count == 0)
      {
        return;
      }
    try
      {
        char[] buf = new char[raw.length - count];
        System.arraycopy(raw, 0, buf, 0, offset);
        System.arraycopy(raw, offset + count, buf, offset,
                         raw.length - (offset + count));
        String value = new String(buf);
        mutating(value);
        text = value;
      }
    catch (IndexOutOfBoundsException x)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
  }
    
  /**
   * <b>DOM L1</b>
   * Returns the value of this node.
   */
  public String getNodeValue()
  {
    return text;
  }
    
  /**
   * <b>DOM L1</b>
   * Returns the value of this node; same as getNodeValue.
   */
  public final String getData()
  {
    return text;
  }

  /**
   * <b>DOM L1</b>
   * Returns the length of the data.
   */
  public int getLength()
  {
    return text.length();
  }
  
  /**
   * <b>DOM L1</b>
   * Modifies the value of this node.
   */
  public void insertData(int offset, String arg)
  {
    if (isReadonly())
      {
      throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    char[] raw = text.toCharArray();
    char[] tmp = arg.toCharArray ();
    char[] buf = new char[raw.length + tmp.length];
    
    try
      {
        System.arraycopy(raw, 0, buf, 0, offset);
        System.arraycopy(tmp, 0, buf, offset, tmp.length);
        System.arraycopy(raw, offset, buf, offset + tmp.length,
                         raw.length - offset);
        String value = new String(buf);
        mutating(value);
        text = value;
      }
    catch (IndexOutOfBoundsException x)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
  }
    
  /**
   * <b>DOM L1</b>
   * Modifies the value of this node.  Causes DOMCharacterDataModified
   * mutation events to be reported (at least one).
   */
  public void replaceData(int offset, int count, String arg)
  {
    if (readonly)
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    char[] raw = text.toCharArray();
    
    // deleteData
    if (offset < 0 || count < 0 || offset > raw.length)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
    if ((offset + count) > raw.length)
      {
        count = raw.length - offset;
      }
    try
      {
        char[] buf = new char[raw.length - count];
        System.arraycopy(raw, 0, buf, 0, offset);
        System.arraycopy(raw, offset + count, buf, offset,
                         raw.length - (offset + count));
        
        // insertData
        char[] tmp = arg.toCharArray ();
        char[] buf2 = new char[buf.length + tmp.length];
        System.arraycopy(raw, 0, buf, 0, offset);
        System.arraycopy(tmp, 0, buf, offset, tmp.length);
        System.arraycopy(raw, offset, buf, offset + tmp.length,
                         raw.length - offset);
        String value = new String(buf);
        mutating(value);
        text = value;
      }
    catch (IndexOutOfBoundsException x)
      {
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
  }
    
  /**
   * <b>DOM L1</b>
   * Assigns the value of this node.
   * Causes a DOMCharacterDataModified mutation event to be reported.
   */
  public void setNodeValue(String value)
  {
    if (isReadonly())
      {
        throw new DomDOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR);
      }
    if (value == null)
      {
        value = "";
      }
    mutating(value);
    text = value;
  }
 
  /**
   * <b>DOM L1</b>
   * Assigns the value of this node; same as setNodeValue.
   */
  final public void setData(String data)
  {
    setNodeValue(data);
  }

  /**
   * <b>DOM L1</b>
   * Returns the specified substring.
   */
  public String substringData(int offset, int count)
  {
    try
      {
        return text.substring(offset, count);
      }
    catch (StringIndexOutOfBoundsException e)
      {
        if (offset >= 0 && count >= 0 && offset < text.length())
          {
            return text.substring(offset);
          }
        throw new DomDOMException(DOMException.INDEX_SIZE_ERR);
      }
  }

  /**
   * The base URI for character data is <code>null</code>.
   * @since DOM Level 3 Core
   */
  public final String getBaseURI()
  {
    return null;
  }

  private void mutating(String newValue)
  {
    if (!reportMutations)
      {
        return;
      }
    
    // EVENT:  DOMCharacterDataModified, target = this,
    //  prev/new values provided
    MutationEvent  event;
    
    event = (MutationEvent) createEvent("MutationEvents");
    event.initMutationEvent("DOMCharacterDataModified",
                            true /* bubbles */, false /* nocancel */,
                            null, text, newValue, null, (short) 0);
    dispatchEvent(event);
  }
  
}


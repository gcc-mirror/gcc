/* HyperlinkEvent.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing.event;

import java.net.URL;
import java.util.EventObject;

import javax.swing.text.Element;

/**
 * @author Andrew Selkirk
 * @author Ronald Veldema
 */
public class HyperlinkEvent extends EventObject
{
  public static final class EventType
  {
    public static final EventType ENTERED = new EventType("ENTERED"); // TODO
    public static final EventType EXITED = new EventType("EXITED"); // TODO
    public static final EventType ACTIVATED = new EventType("ACTIVATED"); // TODO
    
    private String type;

    /**
     * Creates a new Event type.
     * 
     * @param type String representing the event type.
     */
    private EventType(String type)
    {
      this.type = type;
    }

    /**
     * Returns a <code>String</code> of this object.
     */
    public String toString()
    {
      return type;
    }
  }

  private static final long serialVersionUID = -2054640811732867012L;
  
  private EventType type;
  private URL url;
  private String description;
  private Element element;

  /**
   * Creates a new <code>HyperlinkEvent</code> with the given arguments.
   * 
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   */
  public HyperlinkEvent(Object source, EventType type, URL url)
  {
    this (source, type, url, null, null);
  }

  /**
   * Creates a new <code>HyperlinkEvent</code> with the given arguments.
   * 
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param description The description for this link.
   */
  public HyperlinkEvent(Object source, EventType type, URL url,
			String description)
  {
    this (source, type, url, description, null);
  }
  
  /**
   * Creates a new <code>HyperlinkEvent</code> with the given arguments.
   * 
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param description The description for this link.
   * @param element The element in the document representing the anchor.
   */
  public HyperlinkEvent(Object source, EventType type, URL url,
			String description, Element element)
  {
    super(source);
    this.type = type;
    this.url = url;
    this.description = description;
    this.element = element;
  }

  /**
   * Returns the element of the document repesenting this anchor.
   */
  public Element getSourceElement()
  {
    return element;
  }
  
  /**
   * Returns the URL of this event.
   */
  public URL getURL()
  {
    return url;
  }

  /**
   * Returns the type of this event.
   */
  public EventType getEventType()
  {
    return type;
  }

  /**
   * Returns the description of this event.
   */
  public String getDescription()
  {
    return description;
  }
}

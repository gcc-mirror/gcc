/* HTMLFrameHyperlinkEvent.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.text.html;

import java.net.URL;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.text.Element;

/**
 * HTMLFrameHyperlinkEvent transfers information about the link that was
 * activated in a frame.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class HTMLFrameHyperlinkEvent
  extends HyperlinkEvent
{
  private final String target_frame;

  /**
   * Creates a new hypertext link event.
   *
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param element The element in the document representing the anchor.
   * @param frame - the Frame to display the document in.
   */
  public HTMLFrameHyperlinkEvent(Object source, EventType type, URL url,
                                 Element element, String frame)
  {
    super(source, type, url, frame, element);
    target_frame = frame;
  }

  /**
   * Creates a new hypertext link event.
   *
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param frame - the Frame to display the document in.
   */
  public HTMLFrameHyperlinkEvent(Object source, EventType type, URL url,
                                 String frame)
  {
    super(source, type, url, frame);
    target_frame = frame;
  }

  /**
   * Creates a new hypertext link event.
   *
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param description The description for this link.
   * @param element The element in the document representing the anchor.
   * @param frame - the Frame to display the document in.
   */
  public HTMLFrameHyperlinkEvent(Object source, EventType type, URL url,
                                 String description, Element element,
                                 String frame)
  {
    super(source, type, url, description, element);
    target_frame = frame;
  }

  /**
   * Creates a new hypertext link event.
   *
   * @param source The object this link is associated to.
   * @param type The type of event.
   * @param url The URL this link pointing too.
   * @param description The description for this link.
   * @param frame - the Frame to display the document in.
   */
  public HTMLFrameHyperlinkEvent(Object source, EventType type, URL url,
                                 String description, String frame)
  {
    super(source, type, url, description);
    target_frame = frame;
  }

  /**
   * Gets the string, passed as the target frame identifier.
   *
   * @return the target for the link.
   */
  public String getTarget()
  {
    return target_frame;
  }
}

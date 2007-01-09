/* FormSubmitEvent.java -- Event fired on form submit
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import javax.swing.text.Element;

/**
 * The event fired on form submit.
 *
 * @since 1.5
 */
public class FormSubmitEvent
  extends HTMLFrameHyperlinkEvent
{

  // FIXME: Use enums when available.
  /**
   * The submit method.
   */
  public static class MethodType
  {
    /**
     * Indicates a form submit with HTTP method POST.
     */
    public static final MethodType POST = new MethodType();

    /**
     * Indicates a form submit with HTTP method GET.
     */
    public static final MethodType GET = new MethodType();

    private MethodType()
    {
    }
  }

  /**
   * The submit method.
   */
  private MethodType method;

  /**
   * The actual submit data.
   */
  private String data;

  /**
   * Creates a new FormSubmitEvent.
   *
   * @param source the source
   * @param type the type of hyperlink update
   * @param url the action url
   * @param el the associated element
   * @param target the target attribute
   * @param m the submit method
   * @param d the submit data
   */
  FormSubmitEvent(Object source, EventType type, URL url, Element el,
                  String target, MethodType m, String d)
  {
    super(source, type, url, el, target);
    method = m;
    data = d;
  }

  /**
   * Returns the submit data.
   *
   * @return the submit data
   */
  public String getData()
  {
    return data;
  }

  /**
   * Returns the HTTP submit method.
   *
   * @return the HTTP submit method
   */
  public MethodType getMethod()
  {
    return method;
  }
}

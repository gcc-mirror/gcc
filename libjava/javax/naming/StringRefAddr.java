/* StringRefAddr.java -- RefAddr that uses a String as content.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

package javax.naming;

/**
 * RefAddr that uses a String as content.
 * This can for example be used to address things through URLs.
 *
 * @see Reference
 * @since 1.3
 * @author Anthony Green (green@redhat.com)
 * @author Mark Wielaard (mark@klomp.org)
 */
public class StringRefAddr extends RefAddr
{

  /**
   * The possibly null content of this RefAddr.
   * Set by the constructor and returned by getContent.
   */
  private final String contents;

  /**
   * Contructs a new StringRefAddr with the given type and content.
   */
  public StringRefAddr (String addrType, String contents)
  {
    super(addrType);
    this.contents = contents;
  }

  /**
   * Returns the String contents as given to the constructor.
   */
  public Object getContent ()
  {
    return contents;
  }
}

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

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

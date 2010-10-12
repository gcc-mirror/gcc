/* PrintUriException.java --
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


package gnu.javax.print;

import java.net.URI;

import javax.print.PrintException;
import javax.print.URIException;

/**
 * A <code>PrintException</code> further refining the exception
 * cause by providing an implementation of the print exception
 * interface <code>URIException</code>.
 *
 * @see javax.print.PrintException
 * @see javax.print.URIException
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class PrintUriException extends PrintException
  implements URIException
{
  private int reason;
  private URI uri;

  /**
   * Constructs a <code>PrintUriException</code> with the given reason
   * and unsupported URI instance.
   *
   * @param reason the reason for the exception.
   * @param unsupportedUri the URI which is unsupported.
   *
   * @see URIException
   */
  public PrintUriException(int reason, URI unsupportedUri)
  {
    super();
    this.reason = reason;
    uri = unsupportedUri;
  }

  /**
   * Constructs a <code>PrintUriException</code> with the given reason
   * and unsupported URI instance.
   *
   * @param e chained exception
   * @param reason the reason for the exception.
   * @param unsupportedUri the URI which is unsupported.
   */
  public PrintUriException(Exception e, int reason, URI unsupportedUri)
  {
    super(e);
    this.reason = reason;
    uri = unsupportedUri;
  }

  /**
   * Constructs a <code>PrintUriException</code> with the given reason
   * and unsupported URI instance.
   *
   * @param s detailed message
   * @param reason the reason for the exception.
   * @param unsupportedUri the URI which is unsupported.
   */
  public PrintUriException(String s, int reason, URI unsupportedUri)
  {
    super(s);
    this.reason = reason;
    uri = unsupportedUri;
  }

  /**
   * Constructs a <code>PrintUriException</code> with the given reason
   * and unsupported URI instance.
   *
   * @param s detailed message
   * @param e chained exception
   * @param reason the reason for the exception.
   * @param unsupportedUri the URI which is unsupported.
   */
  public PrintUriException(String s, Exception e,
    int reason, URI unsupportedUri)
  {
    super(s, e);
    this.reason = reason;
    uri = unsupportedUri;
  }

  /**
   * @see URIException#getReason()
   */
  public int getReason()
  {
    return reason;
  }

  /**
   * @see URIException#getUnsupportedURI()
   */
  public URI getUnsupportedURI()
  {
    return uri;
  }
}

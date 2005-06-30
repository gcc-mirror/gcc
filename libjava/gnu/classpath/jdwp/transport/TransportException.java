/* TransportException.java -- Exception for transport configury errors
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp.transport;

/**
 * A transport configury or initialization exception thrown by
 * JDWP transports. This class is a generic exception class
 * that the different transport layers use to report transport-specific
 * exceptions that may occur (which could be very different from
 * one transport to the next.).
 *
 * @author Keith Seitz  <keiths@redhat.com>
 */
public class TransportException
  extends Exception
{
  /**
   * Constructs a <code>TransportException</code> with the
   * given message
   *
   * @param message  a message describing the exception
   */
  public TransportException (String message)
  {
    super (message);
  }

  /**
   * Constructs a <code>TransportException</code> with the
   * given <code>Throwable</code> root cause
   *
   * @param t  the cause of the exception
   */
  public TransportException (Throwable t)
  {
    super (t);
  }
}

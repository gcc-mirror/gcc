/* ServiceConfigurationError.java -- An error on service loading.
   Copyright (C) 2007  Free Software Foundation

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

package java.util;

/**
 * <p>
 * An error thrown when a problem occurs during the loading
 * of a service provider by a {@link ServiceLoader}.  Such
 * an error can occur for a number of reasons:
 * </p>
 * <ul>
 * <li>An I/O error occurs</li>
 * <li>The configuration file doesn't meet the specifications</li>
 * <li>A listed class can not be found</li>
 * <li>A listed class does not implement the service</li>
 * <li>A listed class can not be instantiated</li>
 * </ul>
 * 
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.6
 */
public class ServiceConfigurationError
  extends Error
{
  
  /**
   * Compatible with JDK 1.6
   */
  private static final long serialVersionUID = 74132770414881L;

  /**
   * Constructs a new {@link ServiceConfigurationError}
   * with the specified message.
   *
   * @param message a message describing the error, or
   *                <code>null</code> if none is required.
   */
  public ServiceConfigurationError(String message)
  {
    super(message);
  }

  /**
   * Constructs a new {@link ServiceConfigurationError}
   * with the specified message and cause.
   *
   * @param message a message describing the error, or
   *                <code>null</code> if none is required.
   * @param cause the cause of the error, or
   *              <code>null</code> if this is unknown
   *              or inappropriate.
   */
  public ServiceConfigurationError(String message,
				   Throwable cause)
  {
    super(message,cause);
  }

}

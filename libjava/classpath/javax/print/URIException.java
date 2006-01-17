/* URIException.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package javax.print;

import java.net.URI;

/**
 * <code>URIException</code> specifies methods a specific subclass of 
 * {@link javax.print.PrintException} may implement to provide further 
 * informations of printing errors if URI problems are involved.
 * <p>
 * There exists no <code>PrintException</code> class implementing this 
 * interface. Providing this extension in <code>PrintException</code> 
 * subclasses is left to the concrete print service implementation. 
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface URIException
{
  /**
   * Indicates that the provided <code>URI</code> is not accessible.
   */
  int URIInaccessible = 1;
  
  /**
   * Indicates any other problem which is not defined by
   * the other reason constants.
   */
  int URIOtherProblem = -1;
  
  /**
   * Indicates that the print service does not support a specific
   * uri scheme (for example the ftp scheme).
   */
  int URISchemeNotSupported = 2;
  
  /**
   * Returns the reason for this exception as
   * predefined constants in this interface.
   * 
   * @return The reason.
   */
  int getReason();
  
  /**
   * Returns the unsupported <code>URI</code> which caused this exception.
   * 
   * @return The unsupported <code>URI</code>.
   */
  URI getUnsupportedURI();
}

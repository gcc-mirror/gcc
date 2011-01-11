/* DelegateFactory.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package gnu.javax.rmi.CORBA;

import gnu.CORBA.ObjectCreator;


/**
 * This class produces delegates, using the system properties. If not
 * corresponding property is specified, returns default implementations.
 *
 * @author Wu Gansha (gansha.wu@intel.com)
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class DelegateFactory
{
  /**
   * The name to get a stub delegate.
   */
  public static final String STUB = "Stub";

  /**
   * The name to get the util delegate.
   */
  public static final String UTIL = "Util";

  /**
   * The name to get the ValueHandler delegate.
   */
  public static final String VALUEHANDLER = "ValueHandler";

  /**
   * The name to get the PortableRemoteObject delegate.
   */
  public static final String PORTABLE_REMOTE_OBJECT = "PortableRemoteObject";

  /**
   * Get an instance of the given delegate. As in all cases the singleton
   * instance is used, the caching here would be redundant.
   *
   * @param type a delegate type.
   *
   * @return the associated delegate.
   *
   * @throws InternalError if the delegate class, indicated in the system
   * properties, cannot be instantiated.
   */
  public static Object getInstance(String type)
    throws InternalError
  {
    String propertyName = "javax.rmi.CORBA." + type + "Class";
    String dcname = System.getProperty(propertyName);
    if (dcname == null)
      {
        // // No javax.rmi.CORBA.XXXClass property sepcified.
        dcname = "gnu.javax.rmi.CORBA." + type + "DelegateImpl";
      }
    try
      {
        Class dclass = ObjectCreator.forName(dcname);
        return dclass.newInstance();
      }
    catch (Exception e)
      {
        InternalError ierr = new InternalError("Exception when trying to get "
          + type + "delegate instance:" + dcname);
        ierr.initCause(e);
        throw ierr;
      }
  }
}

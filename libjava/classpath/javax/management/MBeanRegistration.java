/* MBeanRegistration.java -- Interface for beans to hook into registration.
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

package javax.management;

/**
 * Beans may implement this interface in order to perform
 * operations immediately prior to or after their registration
 * or deregistration.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface MBeanRegistration
{

  /**
   * This method is called following deregistration of the bean
   * by the server.
   */
  void postDeregister();

  /**
   * This method is called following both successful and unsuccessful
   * attempts to register the bean.  The supplied boolean value indicates
   * the result of the attempt relative to this call.
   *
   * @param successful true if the registration was successful.
   */
  void postRegister(Boolean successful);

  /**
   * This method is called prior to de-registration, and may throw
   * an exception.
   *
   * @throws Exception if something goes wrong during the bean's pre-deregistration
   *                   operation.  The server will re-throw this exception
   *                   as an {@link MBeanRegistrationException}.
   */
  void preDeregister()
    throws Exception;

  /**
   * This method is called prior to registration, with a reference to the
   * server and {@link ObjectName} supplied to the server for registration.
   * This method may be used to replace this name by one chosen by the bean.
   * Such behaviour is expected if the supplied name is <code>null</code>,
   * but may occur in all cases.  The method may throw an exception, which
   * will cause registration to be aborted.
   *
   * @param server the server with which the bean is being registered.
   * @param name the name the server was supplied with for registration,
   *             which may be <code>null</code>.
   * @throws Exception if something goes wrong during the bean's pre-registration
   *                   operation.  The server will re-throw this exception
   *                   as an {@link MBeanRegistrationException}.
   */
  ObjectName preRegister(MBeanServer server, ObjectName name)
    throws Exception;

}

/* ORBInitializer.java --
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


package org.omg.PortableInterceptor;

import org.omg.CORBA.Object;
import org.omg.CORBA.portable.IDLEntity;

/**
 * <p>
 * Registers the interceptor.
 *
 * Direct interceptor registration would open a security hole. Hence instead the
 * interceptors from the ORB.init(..) method, passing the names of the needed
 * initialized classes via properties.
 * </p>
 * <p>
 * These property names are of the form
 * </p>
 * <p><i>org.omg.PortableInterceptor.ORBInitializerClass.&lt;Service&gt;</i></p>
 * where <i>&lt;Service&gt;</i> is the string name of a class, which implements
 * {@link ORBInitializer}. During <code>ORB.init(..)</code>, the properties
 * begining with <i>org.omg.PortableInterceptor.ORBInitializerClass</i> are
 * collected, the <i>&lt;Service&gt;</i> portion of each property is extracted,
 * the initialiser is instantiated with the <i>&lt;Service&gt;</i> string as its
 * class name and then <code>pre_init</code> and <code>post_init</code>
 * (defined in {@link ORBInitializerOperations}) are called on that initializer.
 * The runtime exceptions, thrown by these two methods, are ignored.
 * </p>
 * <p>
 * <h3>Example</h3>
 * A client-side logging service may have the following ORBInitializer
 * implementation:
 *
 * <code><pre>
 * package gnu.x.logging;
 *
 * import org.omg.PortableInterceptor.*;
 * import org.omg.CORBA.LocalObject;
 *
 * public class LoggingService extends LocalObject implements ORBInitializer
 * {
 *   public void pre_init (ORBInitInfo info)
 *     {
 *       // More than one interceptor can be registered.
 *       ServerRequestInterceptor log_requests = new rLoggingInterceptor();
 *       info.add_server_request_interceptor(log_requests);
 *
 *       IORInterceptor log_iors = new iLoggingInterceptor();
 *       info.add_ior_interceptor(log_iors);
 *     }
 *
 *   public void post_init (ORBInitInfo info)
 *     {
 *       // Unused.
 *     }
 * }
 * </code></pre>
 * <p>
 * Then, one of the used set of properties then must contain the property, named
 * <i>
 * org.omg.PortableInterceptor.ORBInitializerClass.gnu.x.Logging.LoggingService
 * </i>.
 * The value of the property is ignored and may empty string. The
 * agreed locations, where this property will be searched for, are:
 * </p><p>
 * 1. The properties parameter in the ORB.init(..), if any.<br>
 * 2. The System properties.<br>
 * 3. The orb.properties file located in the user.home directory (if any).<br>
 * 4. The orb.properties file located in the java.home/lib directory (if any).
 * </p>
 * <p>
 * The applet parameters and command line arguments are <i>not</i> scanned
 * for the possible initializers.
 * </p>
 * <p>
 * Interceptors are registered on a per-ORB basis. The virtual per-object
 * Interceptors can be simulated by checking the policies on the target from
 * within the interception points to determine whether they should work. The
 * virtual per-POA Interceptors can be obtained instantiating each POA such with
 * a different ORB.
 * </p>
 * <p>
 * The registration code should not call directly any methods on the ORB being
 * registered.
 * </p>
 * <p>
 * The new interceptors cannot be registered after the ORB.init(..) returns.
 * </p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ORBInitializer extends ORBInitializerOperations,
  Object,
  IDLEntity
{
}
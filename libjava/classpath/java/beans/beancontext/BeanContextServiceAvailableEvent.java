/* java.beans.beancontext.BeanContextServiceAvailableEvent
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package java.beans.beancontext;

import java.util.Iterator;

/**
 * Event fired when new services become available through a <code>BeanContextServices</code>.
 *
 * @author John Keiser
 * @since JDK1.2
 * @see java.beans.beancontext.BeanContextServicesListener
 */

public class BeanContextServiceAvailableEvent extends BeanContextEvent {
       private static final long serialVersionUID = -5333985775656400778L;

       /**
         * The <code>Class</code> representing the service which is now
         * available.
         */
        protected Class serviceClass;

        /**
         * Create a new service available event.
         * @param services the <code>BeanContextServices</code> through
         *        which the service is available.  This is also the source
         *        of the event.
         * @param serviceClass the service class that is now available.
         */
        public BeanContextServiceAvailableEvent(BeanContextServices services, Class serviceClass) {
                super(services);
                this.serviceClass = serviceClass;
        }

        /**
         * Get the current service selectors of the service class.
         * This is identical to <code>getSourceAsBeanContextServices().getCurrentServiceSelectors(getServiceClass())</code>
         * @return the current service selectors of the service class.
         */
        public Iterator getCurrentServiceSelectors() {
                return getSourceAsBeanContextServices().getCurrentServiceSelectors(serviceClass);
        }

        /**
         * Get the newly available service class.
         * @return the service class.
         */
        public Class getServiceClass() {
                return serviceClass;
        }

        /**
         * Get the <code>BeanContextServices</code> through which the new service is available.
         * @return the <code>BeanContextServices</code> through which the
         *         new service is available.
         */
        public BeanContextServices getSourceAsBeanContextServices() {
                return (BeanContextServices)getSource();
        }
}

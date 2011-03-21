/* java.beans.BeanDescriptor
   Copyright (C) 1998, 2004 Free Software Foundation, Inc.

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


package java.beans;

/**
 ** BeanDescriptor describes general information about a Bean, plus
 ** stores the Bean's Class and it's customizer's Class.<P>
 **
 ** @author John Keiser
 ** @since JDK1.1
 ** @version 1.1.0, 31 May 1998
 **/

public class BeanDescriptor extends FeatureDescriptor {
        Class<?> beanClass;
        Class<?> customizerClass;

        /** Create a new BeanDescriptor with the given beanClass and
         ** no customizer class.
         ** @param beanClass the class of the Bean.
         **/
        public BeanDescriptor(Class<?> beanClass) {
                this(beanClass,null);
        }

        /** Create a new BeanDescriptor with the given bean class and
         ** customizer class.
         ** @param beanClass the class of the Bean.
         ** @param customizerClass the class of the Bean's Customizer.
         **/
        public BeanDescriptor(Class<?> beanClass, Class<?> customizerClass) {
                this.beanClass = beanClass;
                this.customizerClass = customizerClass;

                // Set the FeatureDescriptor programmatic name.
                String name = beanClass.getName();
                int lastInd = name.lastIndexOf('.');
                if (lastInd != -1)
                  name = name.substring(lastInd + 1);

                setName(name);
        }

        /** Get the Bean's class. **/
        public Class<?> getBeanClass() {
                return beanClass;
        }

        /** Get the Bean's customizer's class. **/
        public Class<?> getCustomizerClass() {
                return customizerClass;
        }
}

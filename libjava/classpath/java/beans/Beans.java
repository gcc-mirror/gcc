/* java.beans.Beans
   Copyright (C) 1998, 1999, 2004, 2005 Free Software Foundation, Inc.

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

import gnu.java.beans.DummyAppletStub;
import gnu.java.io.ClassLoaderObjectInputStream;

import java.applet.Applet;
import java.beans.beancontext.BeanContext;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.net.URL;

/**
 * <code>Beans</code> provides some helper methods that allow the basic
 * operations of Bean-ness.
 *
 * @author John Keiser
 * @author Robert Schuster
 * 
 * @since 1.1
 * @status updated to 1.4
 *
 */
public class Beans
{
  static boolean designTime = false;
  static boolean guiAvailable = true;

  /**
   * Once again, we have a java.beans class with only
   * static methods that can be instantiated.  When
   * will the madness end? :)
   */
  public Beans()
  {
    // Does intentionally nothing here.
  }

   /** Creates a bean.
    * <p>This is a convenience method that calls <code>instantiate(cl, beanName, null, null)</code>.</p>
    * 
    * @see instantiate(ClassLoader, String, BeanContext, AppletInitializer)
    * @param cl ClassLoader to be used or <code>null</code> for the system classloader.
    * @param beanName Name of a serialized bean or class name.
    * @return A newly created bean.
    * @throws IOException If access of an IO resource failed.
    * @throws ClassNotFoundException If the class name is not known or does not lead to a proper bean class. 
    */
    public static Object instantiate(ClassLoader cl, String beanName)
        throws IOException, ClassNotFoundException
    {
        return instantiate(cl, beanName, null, null);
    }

   /** Creates a bean.
    * 
    * <p>This is a convenience method that calls <code>instantiate(cl, beanName, beanContext, null)</code>.</p>
    * 
    * @see instantiate(ClassLoader, String, BeanContext, AppletInitializer)
    * @param cl ClassLoader to be used or <code>null</code> for the system classloader.
    * @param beanName Name of a serialized bean or class name.
    * @param beanContext Context to which the newly created Bean should be added.
    * @return A newly created bean.
    * @throws IOException If access of an IO resource failed.
    * @throws ClassNotFoundException If the class name is not known or does not lead to a proper bean class. 
    */
    public static Object instantiate(
        ClassLoader cl,
        String beanName,
        BeanContext beanContext)
        throws IOException, ClassNotFoundException
    {
        return instantiate(cl, beanName, beanContext, null);
    }

   /** Instantiates a bean according to Beans 1.0.
    * 
    * <p>In Beans 1.0 the instantiation scheme is as follows:</p>
    * <p>The name should be dot-separated (e.g "place.for.beans.myBean") and indicate either a
    * serialized object or a class name. In the first case all dots in the name are replaced with
    * slashes ('/') and ".ser" is appended ("place.for.beans.myBean" becomes "place/for/beans/myBean.ser").
    * The bean is then loaded as an application or system resource depending on whether a
    * <code>ClassLoader</code> was provided.</p>
    * 
    * <p>If no such resource exists or if it contains no bean the name is interpreted as a class name of
    * which an instance is then created.</p>
    * 
    * <p>If a <code>BeanContext</code> instance is available the created bean is added to it.</p>
    * 
    * <p>If the created Bean is an <code>Applet</code> or subclass and an <code>AppletInitializer</code>
    * instance is available the applet is initialized and afterwards activated using the initializer. Additionally
    * every instantiated <code>Applet</code> bean is initialized using the {@link Applet.init} method.
    * Furthermore every applet gets a default <code>AppletStub</code>. The <code>Applet</code>'s
    * document base is the location of the ".ser" file if it was deserialized or the location of its class
    * file if it was instantiated.</p>
    * 
    * <p>A <code>ClassNotFoundException</code> is not only thrown when a class name was unknown
    * but even when the class has public no-argument constructor
    * (<code>IllegalAccessException</code> is wrapped) or an exception is thrown while
    * invoking such a constructor (causing exception is wrapped).</p>
    * 
    * @param cl ClassLoader to be used or <code>null</code> for the system classloader.
    * @param beanName Name of a serialized bean or class name.
    * @param beanContext Context to which the newly created Bean should be added.
    * @param initializer The AppletInitializer which is used for initializing <code>Applet</code> beans.
    * @return A newly created bean.
    * @throws IOException If access of an IO resource failed.
    * @throws ClassNotFoundException If the class name is not known or does not lead to a proper bean class. 
    */
   public static Object instantiate(
        ClassLoader cl,
        String beanName,
        BeanContext beanContext,
        AppletInitializer initializer)
        throws IOException, ClassNotFoundException
   {
        Object bean = null;
        URL beanLocation = null;
        URL classLocation = null;

        // Converts bean name into a resource name (eg. "a.b.c" -> "a/b/c").  
        String resourceName = beanName.replace('.', '/');

        /* Tries to get an input stream of the Bean, reading it as a system resource
         * if no ClassLoader is present or as an application resource if a classloader
         * is given. 
         */
        beanLocation =
            (cl == null)
                ? ClassLoader.getSystemResource(resourceName + ".ser")
                : cl.getResource(resourceName + ".ser");

        // Reads the serialized Bean from the returned URL.
        if (beanLocation != null)
        {
            // Deserializes the bean instance.
            ObjectInputStream ois =
                (cl == null)
                    ? new ObjectInputStream(beanLocation.openStream())
                    : new ClassLoaderObjectInputStream(
                        beanLocation.openStream(),
                        cl);

            bean = ois.readObject();

            /* Implementation note: The result of ObjectInputStream.readObject()
            * may have been null at this point (its a valid value to deserialize)
            * and we explicitly want to try instantiation in such a case
            * (this is important for compatibility).
            */
        }

        // Instantiates the Bean using reflective instantiation if it has not been created yet.
        if (bean == null)
        {
            // Makes sure that the deserialization was NOT done.
            beanLocation = null;

            Class beanClass;
            if (cl == null)
            {
                beanClass = Class.forName(beanName);
                classLocation =
                    ClassLoader.getSystemResource(resourceName + ".class");
            }
            else
            {
                beanClass = cl.loadClass(beanName);
                classLocation = cl.getResource(resourceName + ".class");
            }

            // Instantiates and optionally registers the new bean.
            try
            {
                bean = beanClass.newInstance();
            }
            catch(Exception e) {
		/* Wraps all kinds of Exceptions in a ClassNotFoundException (this behavior
		 * matches with official >= 1.5, this was different for <=1.4)
		 */
		throw new ClassNotFoundException(null, e);
            }
        }

        /* Applet beans are treated in the following way:
         * - all AppletS get a default AppletStub
         * - all AppletS are initialized using the AppletInitializer instance (if it is available)
         * - as every other Bean Applets are added to a BeanContext if one is available
         * - each instantiated Applet is initialized using Applet.init() (this is not done for deserialized ones)
         * - finally AppletS get activated using the AppletInitializerS activate-Method
         * 
         * The order of operations is important for compatibility.    
         */
        Applet applet = null;
        if (bean instanceof Applet)
        {
            // Makes a second instanceof call unneccessary (instanceof is expensive).
            applet = (Applet) bean;

            /* The AppletStub's code and document base is set as follows:
             * The code base is always the URL from where the class data originated
             * (without the package name).
             * If the Applet was deserialized the document base is the location of
             * the serialized instance (usually the ".ser" file) otherwise its the URL
             * from where the class data originated (usually the absolute directory
             * location of the ".class" file).
             */
            applet.setStub(
                new DummyAppletStub(
                    applet
                        .getClass()
                        .getProtectionDomain()
                        .getCodeSource()
                        .getLocation(),
                    (beanLocation == null) ? classLocation : beanLocation));

            // Runs the Applet's initialization using an AppletInitializer.
            if (initializer != null)
            {
                initializer.initialize(applet, beanContext);
            }
        }

        // Adds the new bean to its BeanContext.
        if (beanContext != null)
        {
            beanContext.add(bean);
        }

        if (applet != null)
        {

            // Initializes an instantiated (not deserialized) Applet using its own method.
            if (beanLocation == null)
            {
                applet.init();
            }

            // Runs the Applet's activation using an AppletInitializer.
            if (initializer != null)
            {
                initializer.activate(applet);
            }
        }

        return bean;
   }

   /**
    * Returns the Bean as a different class type.
    * This should be used instead of casting to get a new
    * type view of a Bean, because in the future there may
    * be new types of Bean, even Beans spanning multiple
    * Objects.
    *
    * @param bean the Bean to cast.
    * @param newClass the Class to cast it to.
    *
    * @return the Bean as a new view, or if the operation
    *         could not be performed, the Bean itself.
    */
   public static Object getInstanceOf(Object bean, Class newClass)
   {
        return bean;
   }

   /**
    * Determines whether the Bean can be cast to a different
    * class type.
    * This should be used instead of instanceof to determine
    * a Bean's castability, because in the future there may
    * be new types of Bean, even Beans spanning multiple
    * Objects.
    *
    * @param bean the Bean to cast.
    * @param newClass the Class to cast it to.
    *
    * @return whether the Bean can be cast to the class type
    *         in question.
    */
   public static boolean isInstanceOf(Object bean, Class newBeanClass)
   {
       return newBeanClass.isInstance(bean);
   }

   /**
    * Returns whether the GUI is available to use.
    * <p>Defaults to true.</p>
    *
    * @return whether the GUI is available to use.
    */
   public static boolean isGuiAvailable()
   {
       return guiAvailable;
   }

   /**
    * Returns whether it is design time.  Design time means
    * we are in a RAD tool.
    * <p>Defaults to false.</p>
    *
    * @return whether it is design time.
    */
   public static boolean isDesignTime()
   {
       return designTime;
   }

   /**
    * Sets whether the GUI is available to use.
    * 
    * @param guiAvailable whether the GUI is available to use.
    */
   public static void setGuiAvailable(boolean guiAvailable)
       throws SecurityException
   {
        Beans.guiAvailable = guiAvailable;
   }

   /**
    * Sets whether it is design time.  Design time means we
    * are in a RAD tool.
    *
    * @param designTime whether it is design time.
    */
   public static void setDesignTime(boolean designTime)
       throws SecurityException
   {
       Beans.designTime = designTime;
   }

}

/* java.beans.Beans
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import gnu.java.io.ClassLoaderObjectInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.applet.Applet;

/**
 * <code>Beans</code> provides some helper methods that allow the basic
 * operations of Bean-ness.
 *
 * @author John Keiser
 * @since 1.1
 * @version 1.1.0, 29 Jul 1998
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
    // Do nothing here.
  }

  /**
   * Allows you to instantiate a Bean.  This method takes
   * a ClassLoader from which to read the Bean and the
   * name of the Bean.<P>
   *
   * The Bean name should be a dotted name, like a class.
   * It can represent several things.  Beans will search
   * for the Bean using the name like this:<P>
   * <OL>
   * <LI>Searches for a serialized instance of the Bean
   * using getResource(), mangling the Bean name by
   * replacing the dots with slashes and appending .ser
   * (for example, gnu.beans.BlahDeBlah would cause
   * Beans to search for gnu/beans/BlahDeBlah.ser using
   * getResource()).</LI>
   * <LI>Searches for the Bean class using the beanName,
   * and then instantiates it with the no-arg constructor.
   * At that point, if it is an Applet, it provides it
   * with AppletContext and AppletStub, and then calls
   * init().</LI>
   * </OL>
   *
   * @param cl the ClassLoader to use, or <CODE>null</CODE>
   *        to use the default ClassLoader.
   * @param beanName the name of the Bean.
   *
   * @return the Bean.
   *
   * @XXX
   */
  public static Object instantiate (ClassLoader cl, String beanName)
    throws IOException, ClassNotFoundException
  {
    Object bean;
    InputStream serStream;
    
    if (cl == null)
      {
        serStream = ClassLoader.getSystemResourceAsStream
          (beanName.replace ('.','/')+".ser");
      }
    else
      {
        serStream = cl.getResourceAsStream (beanName.replace ('.', '/')
                                            + ".ser");
      }
    
    if (serStream != null)
      {
        if(cl == null)
          {
            ObjectInputStream ois = new ObjectInputStream(serStream);
            bean = ois.readObject();
          }
        else
          {
            ClassLoaderObjectInputStream ois =
              new ClassLoaderObjectInputStream (serStream, cl);
            bean = ois.readObject();
          }
      }
    else if(cl == null)
      {
        Class beanClass = Class.forName(beanName);
        try
          {
            bean = beanClass.newInstance();
          }
        catch(IllegalAccessException E)
          {
            bean = null;
          }
        catch(InstantiationException E)
          {
            bean = null;
          }
      }
    else
      {
        Class beanClass = cl.loadClass(beanName);
        try
          {
            bean = beanClass.newInstance();
          }
        catch(IllegalAccessException E)
          {
            bean = null;
          }
        catch(InstantiationException E)
          {
            bean = null;
          }
      }

    if(bean instanceof Applet)
      {
        Applet a = (Applet)bean;
        //a.setAppletContext(???);
        //a.setStub(???);
        if(serStream == null)
          {
            a.init();
          }
      }

    return bean;
  }

  /**
   * Get the Bean as a different class type.
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
   * Determine whether the Bean can be cast to a different
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
   * Find out whether the GUI is available to use.
   * Defaults to true.
   *
   * @return whether the GUI is available to use.
   */
  public static boolean isGuiAvailable()
  {
    return guiAvailable;
  }

  /**
   * Find out whether it is design time.  Design time means
   * we are in a RAD tool.
   * Defaults to false.
   *
   * @return whether it is design time.
   */
  public static boolean isDesignTime()
  {
    return designTime;
  }

  /**
   * Set whether the GUI is available to use.
   * @param guiAvailable whether the GUI is available to use.
   */
  public static void setGuiAvailable(boolean guiAvailable)
    throws SecurityException
  {
    Beans.guiAvailable = guiAvailable;
  }

  /**
   * Set whether it is design time.  Design time means we
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

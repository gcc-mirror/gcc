/* RuntimeMXBeanImpl.java - Implementation of an runtime bean
   Copyright (C) 2006 Free Software Foundation

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

package gnu.java.lang.management;

import gnu.classpath.SystemProperties;

import java.lang.management.RuntimeMXBean;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.management.NotCompliantMBeanException;

/**
 * Provides access to information about the virtual machine.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public final class RuntimeMXBeanImpl
  extends BeanImpl
  implements RuntimeMXBean
{

  private static final String SUN_BOOT_CLASS_PATH = "sun.boot.class.path";
  private static final String JAVA_BOOT_CLASS_PATH = "java.boot.class.path";

  private long startTime = -1;

  private String bootClassPath = null;

  private boolean bootClassPathSupported = true;

  /**
   * Constructs a new <code>RuntimeMXBeanImpl</code>.
   *
   * @throws NotCompliantMBeanException if this class doesn't implement
   *                                    the interface or a method appears
   *                                    in the interface that doesn't comply
   *                                    with the naming conventions.
   */
  public RuntimeMXBeanImpl()
    throws NotCompliantMBeanException
  {
    super(RuntimeMXBean.class);
  }

  public String getBootClassPath()
  {
    checkMonitorPermissions();
    if (isBootClassPathSupported())
      return bootClassPath;
    else
      throw
        new UnsupportedOperationException("Retrieving the boot " +
                                          "classpath is not supported.");
  }

  public String getClassPath()
  {
    return System.getProperty("java.class.path");
  }

  public List getInputArguments()
  {
    checkMonitorPermissions();
    return Arrays.asList(VMRuntimeMXBeanImpl.getInputArguments());
  }

  public String getLibraryPath()
  {
    return System.getProperty("java.library.path");
  }

  public String getManagementSpecVersion()
  {
    return "1.0";
  }

  public String getName()
  {
    return VMRuntimeMXBeanImpl.getName();
  }

  public String getSpecName()
  {
    return System.getProperty("java.vm.specification.name");
  }

  public String getSpecVendor()
  {
    return System.getProperty("java.vm.specification.vendor");
  }

  public String getSpecVersion()
  {
    return System.getProperty("java.vm.specification.version");
  }

  public long getStartTime()
  {
    if (startTime == -1)
      startTime = VMRuntimeMXBeanImpl.getStartTime();
    return startTime;
  }

  public Map getSystemProperties()
  {
    Map map = new HashMap();
    Properties props = System.getProperties();
    Iterator entries = props.entrySet().iterator();
    while (entries.hasNext())
      {
        Map.Entry next = (Map.Entry) entries.next();
        Object key = next.getKey();
        Object value = next.getValue();
        if (key instanceof String &&
            value instanceof String)
          map.put(key, value);
      }
    return map;
  }

  public long getUptime()
  {
    return new Date().getTime() - getStartTime();
  }

  public String getVmName()
  {
    return System.getProperty("java.vm.name");
  }

  public String getVmVendor()
  {
    return System.getProperty("java.vm.vendor");
  }

  public String getVmVersion()
  {
    return System.getProperty("java.vm.version");
  }

  public boolean isBootClassPathSupported()
  {
    if (bootClassPath == null)
      {
        bootClassPath = SystemProperties.getProperty(JAVA_BOOT_CLASS_PATH);
        if (bootClassPath == null)
          bootClassPath = SystemProperties.getProperty(SUN_BOOT_CLASS_PATH);
        if (bootClassPath == null)
          bootClassPathSupported = false;
      }
    return bootClassPathSupported;
  }

}

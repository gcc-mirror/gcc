/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;
import java.util.Properties;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date February 7, 2000.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification.
 * Status:  Partially implemented.
 */

public abstract class Provider extends Properties
{
  // FIXME: Check the following property values against specs!
  protected Provider(String name, double version, String info)
  {
    super();
    put("java.security.Provider.Name", name);
    put("java.security.Provider.Version", Double.toString(version));
    put("java.security.Provider.Info", info);
  }

  public String getName()
  {
    return getProperty("java.security.Provider.Name");
  }

  public double getVersion()
  {
    return Double.valueOf(getProperty("java.security.Provider.Version")).doubleValue();
  }

  public String getInfo()
  {
    return getProperty("java.security.Provider.Info");
  }

  public String toString()
  {
    // FIXME: Check this string against the spec.
    return getName() + " " + getProperty("java.security.Provider.Version");
  }
}

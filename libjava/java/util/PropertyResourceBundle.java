/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.InputStream;
import java.io.IOException;
import gnu.gcj.util.EnumerationChain;

/**
 * @author Anthony Green <green@cygnus.com>
 * @date April 29, 1999.
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3,
 * and "The Java Language Specification", ISBN 0-201-63451-1.  */

public class PropertyResourceBundle extends ResourceBundle
{
  private Properties properties;

  public PropertyResourceBundle (InputStream pstream) throws IOException
    {
      // Initialize and load our Properties.
      properties = new Properties();
      properties.load(pstream);
    }

  public Enumeration getKeys()
    {
      if (parent == null)
        return properties.propertyNames();
      else
        return new EnumerationChain (properties.propertyNames(),
                                     parent.getKeys ());
    }

  public Object handleGetObject (String key)
    {
      return properties.getProperty(key);
    }
}    

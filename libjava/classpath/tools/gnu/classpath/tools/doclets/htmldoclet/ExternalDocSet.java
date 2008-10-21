/* gnu.classpath.tools.doclets.htmldoclet.ExternalDocSet
   Copyright (C) 2004 Free Software Foundation, Inc.

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
02111-1307 USA. */

package gnu.classpath.tools.doclets.htmldoclet;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import com.sun.javadoc.ClassDoc;

public class ExternalDocSet
{
   private String url;
   private String packageListDir;
   private URL docSetDirectoryURL;

   public String getPackageListDir()
   {
      return packageListDir;
   }

   public ExternalDocSet(String url,
                         String packageListDir)
   {
      this.url = url;
      this.packageListDir = packageListDir;
   }

   private Set packageNames = new HashSet();
   private boolean javadocCompatible;

   public void load(File targetDirectory)
      throws IOException, MalformedURLException
   {
      if (!url.endsWith("/")) {
         url += "/";
      }

      this.docSetDirectoryURL = new URL(targetDirectory.toURL(),
                                        url);

      URL packageListDirURL;
      if (null != packageListDir) {
         if (!packageListDir.endsWith("/")) {
           packageListDir += "/";
         }
         packageListDirURL = new URL(new File(System.getProperty("user.dir")).toURL(),
                                     packageListDir);
      }
      else {
         packageListDirURL = docSetDirectoryURL;
      }

      URL packageListURL = new URL(packageListDirURL,
                                    "package-list");
      InputStream in = packageListURL.openStream();
      if (null != in) {
         readPackages(in);
         in.close();
      }
      else {
         throw new FileNotFoundException(packageListURL.toString());
      }

      URL gjdocPropertiesURL = new URL(packageListDirURL,
                                       "gjdoc.properties");
      try {
          InputStream propertiesIn = gjdocPropertiesURL.openStream();
          if (null != in) {
              Properties properties = new Properties();
              properties.load(propertiesIn);
              propertiesIn.close();
         
              String gjdocCompatProperty = properties.getProperty("gjdoc.compat");
              if (null != gjdocCompatProperty) {
                  javadocCompatible = "true".equals(properties.getProperty("gjdoc.compat"));
              }
              else {
                  javadocCompatible = true;
              }
          }
          else {
              javadocCompatible = true;
          }
      } 
      catch (FileNotFoundException e) {
          javadocCompatible = true;
      }
   }

   public String getPackageDocURL(String packageName)
   {
      try {
         URL packageURL = new URL(docSetDirectoryURL,
                                  packageName.replace('.', '/'));
         return packageURL.toString();
      }
      catch (MalformedURLException e) {
         // This should not happen since we know that packageName is a
         // proper Java identifiers, so the resulting URL can't be
         // invalid
         throw new RuntimeException(e);
      }
   }

   public String getClassDocURL(String packageName, String typeName)
   {
      try {
         URL fileURL = new URL(docSetDirectoryURL,
                               packageName.replace('.', '/') + "/" + typeName + ".html");
         return fileURL.toString();
      }
      catch (MalformedURLException e) {
         // This should not happen since we know that packageName and
         // typeName are proper Java identifiers, so the resulting URL
         // can't be invalid
         throw new RuntimeException(e);
      }
   }

   protected void readPackages(InputStream in)
      throws IOException
   {
      BufferedReader reader
         = new BufferedReader(new InputStreamReader(in, "UTF-8"));
      String line;
      while ((line = reader.readLine()) != null) {
         line = line.trim();
         packageNames.add(line);
      }
   }

   public Set getPackageNames()
   {
      return packageNames;
   }

   public boolean isJavadocCompatible()
   {
      return javadocCompatible;
   }
}

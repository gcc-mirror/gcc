/* gnu.classpath.tools.doclets.DocletOptionPackageWildcard
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

package gnu.classpath.tools.doclets;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.StringTokenizer;
import java.util.TreeSet;

import com.sun.javadoc.PackageDoc;

/**
 *  Processes a doclet option whose value consists of a
 *  colon-separated list of package wildcards, or - optionally -
 *  equals the string "all", denoting that all packages should match.
 */
public class DocletOptionPackageWildcard
   extends DocletOption
{
   private PackageMatcher packageMatcher;
   private boolean allowAll;
   private boolean specified;

   DocletOptionPackageWildcard(String optionName, boolean allowAll)
   {
      super(optionName);
      this.allowAll = allowAll;
   }
      
   public int getLength()
   {
      return 2;
   }

   public boolean isSpecified()
   {
      return specified;
   }

   public boolean set(String[] optionArr)
   {
      this.specified = true;
      try {
         if (allowAll && "all".equals(optionArr[2])) {
            packageMatcher = null;
         }
         else {
            packageMatcher = new PackageMatcher();
            
            StringTokenizer tokenizer = new StringTokenizer(optionArr[2], ":");
            while (tokenizer.hasMoreTokens()) {
               String packageWildcard = tokenizer.nextToken();
               packageMatcher.addWildcard(packageWildcard);
            }
         }
         return true;
      }
      catch (InvalidPackageWildcardException e) {
         // FIXME: output problem description here, better throw
         // DocletConfigurationException
         return false;
      }
   }

   public SortedSet filter(PackageDoc[] packages)
   {
      if (null != packageMatcher) {
         return packageMatcher.filter(packages);
      }
      else {
         SortedSet result = new TreeSet();
         for (int i=0; i<packages.length; ++i) {
            result.add(packages[i]);
         }
         return result;
      }
   }

   public boolean match(PackageDoc packageDoc)
   {
      if (null != packageMatcher) {
         return packageMatcher.match(packageDoc);
      }
      else {
         return true;
      }
   }
}


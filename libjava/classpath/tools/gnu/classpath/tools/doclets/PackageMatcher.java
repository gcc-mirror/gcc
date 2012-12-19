/* gnu.classpath.tools.doclets.PackageMatcher
   Copyright (C) 2004, 2012 Free Software Foundation, Inc.

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

package gnu.classpath.tools.doclets;

import java.util.Iterator;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.javadoc.PackageDoc;

/**
 *  Filters a set of packages according to a set of wildcards.
 */
public class PackageMatcher
{
   private Set<Pattern> patterns = new HashSet<Pattern>();

   /**
    *  Add a wildcard to be matched. Wildcards can contain asterisk
    *  characters which match zero or more characters.
    *
    *  @throw InvalidPackageWildcardException if the wildcard cannot
    *  match any valid package name.
    */
   public void addWildcard(String wildcard)
      throws InvalidPackageWildcardException
   {
      final int STATE_ID_START = 0;
      final int STATE_ID = 1;

      int state = STATE_ID_START;

      char[] wildcardChars = wildcard.toCharArray();
      StringBuffer regexString = new StringBuffer();

      for (int i=0; i<wildcardChars.length; ++i) {
         char c = wildcardChars[i];
         switch (state) {
         case STATE_ID_START:
            if ('*' == c) {
               regexString.append(".*");
            }
            else if (Character.isJavaIdentifierStart(c)) {
               regexString.append(c);
            }
            else {
               throw new InvalidPackageWildcardException(wildcard);
            }
            state = STATE_ID;
            break;

         case STATE_ID:
            if ('.' == c) {
               regexString.append("\\.");
               state = STATE_ID_START;
            }
            else if ('*' == c) {
               regexString.append(".*");
            }
            else if (Character.isJavaIdentifierPart(c)) {
               regexString.append(c);
            }
            else {
               throw new InvalidPackageWildcardException(wildcard);
            }
         }
      }
      if (STATE_ID_START == state) {
         throw new InvalidPackageWildcardException(wildcard);
      }

      patterns.add(Pattern.compile(regexString.toString()));
   }

   /**
    *  Return a sorted, filtered set of packages. A package from the
    *  array given will be put into the output list if it matches one
    *  or more of the wildcards added to this PackageMatcher before.
    */
   public SortedSet<PackageDoc> filter(PackageDoc[] packageDocs)
   {
      SortedSet<PackageDoc> result = new TreeSet<PackageDoc>();
      for (int i=0; i<packageDocs.length; ++i) {
         if (match(packageDocs[i])) {
            result.add(packageDocs[i]);
         }
      }
      return result;
   }

   /**
    *  Return true when the given PackageDoc matches one or more of
    *  the wildcard added to this PackageMatcher before.
    */
   public boolean match(PackageDoc packageDoc)
   {
      Iterator<Pattern> it = patterns.iterator();
      while (it.hasNext()) {
         Pattern pattern = it.next();
         Matcher matcher = pattern.matcher(packageDoc.name());
         if (matcher.matches()) {
            return true;
         }
      }
      return false;
   }

   public String toString()
   {
      return "PackageMatcher{patterns=" + patterns + "}";
   }
}

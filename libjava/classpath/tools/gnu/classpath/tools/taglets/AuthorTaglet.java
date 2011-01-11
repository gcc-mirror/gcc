/* gnu.classpath.tools.taglets.AuthorTaglet
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package gnu.classpath.tools.taglets;

import java.util.Map;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import com.sun.tools.doclets.Taglet;

import com.sun.javadoc.Tag;

/**
 *  The default Taglet which handles Author information.
 *
 *  @author Julian Scheid (julian@sektor37.de)
 */
public class AuthorTaglet implements Taglet {

   /**
    *  Enum class which denotes whether and how to replace email
    *  addresses in author tags.
    */
   public static class EmailReplacement {
      private EmailReplacement() {}

      /**
       *  Specifies that email addresses should not be replaced.
       */
      public static final EmailReplacement NO_REPLACEMENT = new EmailReplacement();

      /**
       *  Specifies that author tag text matching "Real Name
       *  (user@domain.tld)" is converted to "&lt;a
       *  href="mailto:user@domain.tld"&gt;Real Name&lt;/a&gt;.
       */
      public static final EmailReplacement MAILTO_NAME = new EmailReplacement();

      /**
       *  Specifies that author tag text matching "Real Name
       *  (user@domain.tld)" is converted to "Real Name (&lt;a
       *  href="mailto:user@domain.tld"&gt;user@domain.tld&lt;/a&gt;).
       */
      public static final EmailReplacement NAME_MAILTO_ADDRESS = new EmailReplacement();

      /**
       *  Specifies that author tag text matching "Real Name
       *  (user@domain.tld)" is converted to "Real Name (user AT
       *  domain DOT tld)", where the "AT" and "DOT" replacement are
       *  specified by AuthorTaglet.emailAtReplacement and
       *  AuthorTaglet.emailDotReplacement.
       */
      public static final EmailReplacement NAME_MANGLED_ADDRESS = new EmailReplacement();
   }

   private static EmailReplacement emailReplacementType = EmailReplacement.NO_REPLACEMENT;
   private static String atReplacement = " <b>at</b> ";
   private static String dotReplacement = " <b>dot</b> ";

   private static final String NAME = "author";
   private static final String SINGLE_HEADER = "Author:";
   private static final String MULTI_HEADER = "Authors:";

   private static boolean enabled = true;

   /**
    *  Matches <code>.</code> (dot).
    */
   private static final Pattern dotPattern = Pattern.compile("[.]");

   /**
    *  Matches <code>@</code> (at sign).
    */
   private static final Pattern atPattern = Pattern.compile("[@]");

   /**
    *  Matches <code>Real Name (user@domain.tld)</code>.
    */
   private static final Pattern authorEmailPattern
     = Pattern.compile("^"
                       + "\\s*" // optional whitespace
                       + "(" // group #1 start (real name)
                       + "(?:[^\t\r\n ]|\\()+" // first name
                       + "(?:\\s+(?:[^\t\r\n ]|\\()+)*" // additional names
                       + ")" // group #1 end
                       + "\\s*" // optional whitespace
                       + "[(<]" // opening paren
                       + "\\s*" // optional whitespace
                       + "(" // group #2 start (email address)
                       + "(" // group #3 start (email user)
                       + "[A-z0-9_\\-\\.]+" // username
                       + ")" // group #3 end
                       + "[@]" // at sign
                       + "[A-z0-9_\\-]+(?:[.][A-z0-9_\\-]+)+[A-z]" // domain
                       + ")" // group #2 end
                       + "\\s*" // optional whitespace
                       + "(?:\\)|>)" // closing paren
                       + "$");

   public String getName() {
      return NAME;
   }

   public boolean inField() {
      return true;
   }

   public boolean inConstructor() {
      return true;
   }

   public boolean inMethod() {
      return true;
   }

   public boolean inOverview() {
      return true;
   }

   public boolean inPackage() {
      return true;
   }

   public boolean inType() {
      return true;
   }

   public boolean isInlineTag() {
      return false;
   }

   public static void register(Map tagletMap) {
      AuthorTaglet authorTaglet = new AuthorTaglet();
      tagletMap.put(authorTaglet.getName(), authorTaglet);
   }

   public String toString(Tag tag) {
      if (enabled) {
         return toString(new Tag[] { tag });
      }
      else {
         return null;
      }
   }

   public String toString(Tag[] tags) {
      if (!enabled || tags.length == 0) {
         return null;
      }
      else {
         boolean haveValidTag = false;
         for (int i = 0; i < tags.length && !haveValidTag; ++i) {
            if (tags[i].text().length() > 0) {
               haveValidTag = true;
            }
         }

         if (haveValidTag) {
            StringBuffer result = new StringBuffer();
            result.append("<dl class=\"tag list\">");
            result.append("<dt class=\"tag section header\"><b>");
            if (tags.length == 1) {
               result.append(SINGLE_HEADER);
            }
            else {
               result.append(MULTI_HEADER);
            }
            result.append("</b></dt>");
            for (int i = 0; i < tags.length; i++) {
               result.append("<dd class=\"tag item\">");
               result.append(replaceEmail(tags[i].text()));
               result.append("</dd>");
            }
            result.append("</dl>");
            return result.toString();
         }
         else {
            return null;
         }
      }
   }

   /**
    *  Reformat the tag text according to {@link #emailReplacementType}.
    */
   private String replaceEmail(String text) {

      if (EmailReplacement.NO_REPLACEMENT == emailReplacementType) {
         return text;
      }
      else {
         Matcher matcher = authorEmailPattern.matcher(text);
         if (matcher.matches()) {
            String realName = matcher.group(1);
            String emailAddress = matcher.group(2);
            if (EmailReplacement.MAILTO_NAME == emailReplacementType) {
               return "<a href=\"mailto:" + emailAddress + "\">" + realName + "</a>";
            }
            else if (EmailReplacement.NAME_MAILTO_ADDRESS == emailReplacementType) {
               return realName + " (<a href=\"mailto:" + emailAddress + "\">" + emailAddress + "</a>)";
            }
            else if (EmailReplacement.NAME_MANGLED_ADDRESS == emailReplacementType) {
               Matcher dotMatcher = dotPattern.matcher(emailAddress);
               Matcher atMatcher = atPattern.matcher(dotMatcher.replaceAll(dotReplacement));
               String mangledAddress = atMatcher.replaceAll(atReplacement);
               return realName + " (" + mangledAddress + ")";
            }
            else {
               // this shouldn't happen
               return text;
            }
         }
         else {
            return text;
         }
      }
   }

   /**
    *  Set the email replacement type.
    */
   public static void setEmailReplacementType(EmailReplacement emailReplacementType)
   {
      if (null == emailReplacementType) {
         throw new NullPointerException();
      }
      AuthorTaglet.emailReplacementType = emailReplacementType;
   }

   /**
    *  Set the HTML text by which the <code>@</code> (at sign) in email
    *  addresses should be replaced if the email replacement type is
    *  <code>NAME_MANGLED_ADDRESS</code>.
    */
   public static void setAtReplacement(String atReplacement)
   {
      AuthorTaglet.atReplacement = atReplacement;
   }

   /**
    *  Set the HTML text by which the <code>.</code> (dot) in email
    *  addresses should be replaced if the email replacement type is
    *  <code>NAME_MANGLED_ADDRESS</code>.
    */
   public static void setDotReplacement(String dotReplacement)
   {
      AuthorTaglet.dotReplacement = dotReplacement;
   }

   /**
    *  Enables/disables this taglet.
    */
   public static void setTagletEnabled(boolean enabled)
   {
      AuthorTaglet.enabled = enabled;
   }
}

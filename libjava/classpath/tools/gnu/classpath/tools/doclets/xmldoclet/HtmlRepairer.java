/* gnu.classpath.tools.doclets.xmldoclet.HtmlRepairer.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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

package gnu.classpath.tools.doclets.xmldoclet;

import java.io.*;
import java.util.*;
import com.sun.javadoc.DocErrorReporter;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.MemberDoc;

/**
 *  Provides methods for tidying up HTML source.
 *
 *  @author Julian Scheid
 */
public final class HtmlRepairer {

   private static class TagInfo {

      private Set parentTags = new HashSet();

      public TagInfo(String parentTag) {
	 this.parentTags.add(parentTag);
      }

      public TagInfo(String[] parentTagArr) {
	 for (int i=0; i<parentTagArr.length; ++i) {
	    this.parentTags.add(parentTagArr[i]);
	 }
      }

      public boolean isLegalParentTag(String tag) {
	 return this.parentTags.contains(tag);
      }
   }

   private DocErrorReporter warningReporter;
   private boolean noWarn;
   private boolean noEmailWarn;
   private ClassDoc contextClass;
   private MemberDoc contextMember;
   private StringBuffer output = new StringBuffer();
   private Stack tagStack = new Stack();
   private boolean isLeadingTag = true;
   private boolean throwAwayLeadingPara = false;

   private static Map tagInfoMap;

   private static Set noTextParentTags;

   static {
      tagInfoMap = new HashMap();
      tagInfoMap.put("li", new TagInfo(new String[] { "ul", "ol", "nl", "menu", "dir" }));
      tagInfoMap.put("td", new TagInfo(new String[] { "tr" }));
      tagInfoMap.put("th", new TagInfo(new String[] { "tr" }));
      tagInfoMap.put("tr", new TagInfo(new String[] { "table" }));
      tagInfoMap.put("dt", new TagInfo(new String[] { "dl" }));
      tagInfoMap.put("dd", new TagInfo(new String[] { "dl" }));
      tagInfoMap.put("param", new TagInfo(new String[] { "applet" }));

      String[] noTextParentTagArr = {
         "area", "base", "body", "br", "dd", "dt", "head", "hr", "html", 
         "img", "input", "link", "map", "meta", "ol", "optgroup", "param",
         "select", "table", "tbody", "tfoot", "thead", "tr", "ul",
      };

      noTextParentTags = new HashSet();
      for (int i=0; i<noTextParentTagArr.length; ++i) {
         noTextParentTags.add(noTextParentTagArr[i]);
      }
   }

   public HtmlRepairer(DocErrorReporter warningReporter, 
		       boolean noWarn, boolean noEmailWarn,
		       ClassDoc contextClass, MemberDoc contextMember,
                       boolean throwAwayLeadingPara) {
      this.warningReporter = warningReporter;
      this.noWarn = noWarn;
      this.noEmailWarn = noEmailWarn;
      this.contextClass = contextClass;
      this.contextMember = contextMember;
      this.throwAwayLeadingPara = throwAwayLeadingPara;
   }
  
   private static String replaceStr(String haystack, String needle, String replacement) {
      int ndx=haystack.indexOf(needle);
      if (ndx<0) 
	 return haystack;
      else 
	 return haystack.substring(0, ndx)+replacement
	    + replaceStr(haystack.substring(ndx+needle.length()), needle, replacement);
   }
 
   private void haveText(String text) {

      if (isLeadingTag && throwAwayLeadingPara) {
         if (0 != text.trim().length()) {
            isLeadingTag = false;
         }
      }

      if (tagStack.isEmpty() || !noTextParentTags.contains(tagStack.peek())) {

         text = replaceStr(text, "&lt1", "&lt;1");
         text = replaceStr(text, "&&", "&amp;&amp;");
         text = replaceStr(text, "& ", "&amp; ");
         text = replaceStr(text, "&\t", "&amp;\t");
         text = replaceStr(text, "&\r", "&amp;\r");
         text = replaceStr(text, "&\n", "&amp;\n");
         for (char c='0'; c<='9'; ++c)
            text = replaceStr(text, "&"+c, "&amp;"+c);
         text = replaceStr(text, "\u00a7", "&sect;");
         output.append(text);
      }
      else {
         printWarning("Discarded text in <" + tagStack.peek() + "> element");
      }
   }

   private void haveStartOrEndTag(String tag) {

      boolean _isLeadingTag = isLeadingTag;
      isLeadingTag = false;

      tag = tag.trim();

      boolean isEndTag = tag.startsWith("/");
      boolean isAtomTag = tag.endsWith("/");

      if (isEndTag && isAtomTag) {
	 // got something like '</a/>' which is invalid.
	 // suppose a close tag was intended.
	 tag = tag.substring(0, tag.length()-1);
      }

      if (tag.length() < 1) {
         printWarning("Deleting broken tag");
         return;
      }

      String tagName = tag.substring(isEndTag?1:0, isAtomTag?tag.length()-1:tag.length());
      String tagAttributes = "";

      for (int i=0; i<tagName.length(); ++i) {
	 if (" \t\r\n".indexOf(tagName.charAt(i))>=0) {
	    tagAttributes = tagName.substring(i).trim();
	    tagName = tagName.substring(0, i);
	    break;
	 }
      }

      if (!isEndTag && tagName.indexOf('@')>0) {
	 if (!noEmailWarn) {
	    printWarning("Tag looks like email address: <"+tagName+">");
	 }
	 output.append("&lt;"+tag+"&gt;");
	 return;
      }

      tagName = tagName.toLowerCase();

      if (_isLeadingTag && "p".equals(tagName) && !isEndTag && throwAwayLeadingPara) {
         return;
      }

      if ("p".equals(tagName) || "br".equals(tagName) || "hr".equals(tagName)) {
	 // throw away </p> and </br>
	 if (isEndTag) {
	    return;
	 }
	 // make sure every <p> is a <p/> and every <br> is a <br/>
	 else if (!isAtomTag) {
	    tag += "/";
	    isAtomTag = true;
	 }
      }

      if (isEndTag) {

	 // check whether this close tag is on the stack
	 // if yes, close all tags up to this tag
	 if (tagStack.contains(tagName)) {
	    String popped;
	    do {
	       popped = (String)tagStack.pop();
	       if (!popped.equals(tagName)) 
		  printWarning("Inserting '</"+popped+">");
	       output.append("</"+popped+">");
	    }
	    while (!popped.equals(tagName));
	 }
	 // if not, just throw it away
	 else {
	    printWarning("Deleting <"+tag+">");
	 }
      }
      else {

	 final int STATE_INITIAL = 1;
	 final int STATE_EXPECT_ATTRIBUTENAME = 2;
	 final int STATE_UNQUOTED_ATTRIBUTEVALUE = 3;
	 final int STATE_SINGLEQUOTE_ATTRIBUTEVALUE = 4;
	 final int STATE_DOUBLEQUOTE_ATTRIBUTEVALUE = 5;
	 final int STATE_EXPECT_ATTRIBUTEVALUE = 6;
	 final int STATE_EXPECT_EQUALSIGN = 7;

	 int state = STATE_INITIAL;
	       
	 String newAttributes = "";
	 String attributeName = null;
	 StringBuffer buf = new StringBuffer();

	 char[] attrsAsChars = tagAttributes.toCharArray();
	 for (int i=0, ilim=attrsAsChars.length+1; i<ilim; ++i) {
	    int c;
	    if (i<attrsAsChars.length)
	       c = (int)attrsAsChars[i];
	    else
	       c = -1;

	    switch (state) {

	    case STATE_INITIAL:
	       if (" \t\r\n".indexOf(c)>=0){
		  continue;
	       }
	       else if (-1==c) {
		  continue;
	       }
	       else {
		  state = STATE_EXPECT_ATTRIBUTENAME;
		  buf.append((char)c);
	       }
	       break;

	    case STATE_EXPECT_ATTRIBUTENAME:
	       if ('='==c) {
		  attributeName = buf.toString();
		  buf.setLength(0);
		  state = STATE_EXPECT_ATTRIBUTEVALUE;
	       }
	       else if (-1==c) {
		  attributeName = buf.toString();
		  buf.setLength(0);
		  printWarning("In Tag '"+tag+"':\nAttribute name without a value, inserting value =\""+attributeName+"\"");
	       }
	       else if (" \t\r\n".indexOf(c)>=0) {
		  state = STATE_EXPECT_EQUALSIGN;
	       }
	       else {
		  buf.append((char)c);
	       }
	       break;

	    case STATE_EXPECT_EQUALSIGN:
	       if (" \t\r\n".indexOf(c)>=0){
		  continue;
	       }
	       else if ('='==c) {
		  state = STATE_EXPECT_ATTRIBUTEVALUE;
		  attributeName = buf.toString();
		  buf.setLength(0);
	       }
	       else {
		  attributeName = buf.toString();
		  buf.setLength(0);
		  printWarning("In Tag '"+tag+"':\nAttribute name without a value, inserting value =\""+attributeName+"\"");
		  newAttributes += " "+attributeName+"=\""+attributeName+"\"";
		  buf.append((char)c);
		  state = STATE_EXPECT_ATTRIBUTENAME;
	       }
	       break;

	    case STATE_EXPECT_ATTRIBUTEVALUE:
	       if (" \t\r\n".indexOf(c)>=0){
		  continue;
	       }
	       else if ('\"'==c) {
		  state = STATE_DOUBLEQUOTE_ATTRIBUTEVALUE;
	       }
	       else if ('\''==c) {
		  state = STATE_SINGLEQUOTE_ATTRIBUTEVALUE;
	       }
	       else {
		  state = STATE_UNQUOTED_ATTRIBUTEVALUE;
		  buf.append((char)c);
	       }
	       break;

	    case STATE_UNQUOTED_ATTRIBUTEVALUE:
	       if (-1==c || " \t\r\n".indexOf(c)>=0){
		  state = STATE_INITIAL;
		  newAttributes += " "+attributeName + "=\"" + buf.toString() + "\"";
		  buf.setLength(0);
	       }
	       else {
		  buf.append((char)c);
	       }
	       break;

	    case STATE_SINGLEQUOTE_ATTRIBUTEVALUE:
	       if ('\''==c) {
		  state = STATE_INITIAL;
		  newAttributes += " "+attributeName + "=\"" + buf.toString() + "\"";
		  buf.setLength(0);
	       }
	       else {
		  buf.append((char)c);
	       }
	       break;

	    case STATE_DOUBLEQUOTE_ATTRIBUTEVALUE:
	       if ('\"'==c) {
		  state = STATE_INITIAL;
		  newAttributes += " "+attributeName + "=\"" + buf.toString() + "\"";
		  buf.setLength(0);
	       }
	       else {
		  buf.append((char)c);
	       }
	       break;
	    }
	 }


	 if (!isAtomTag) {

	    // check whether this open tag is equal to the topmost
	    // entry on the stack; if yes, emit a close tag first
	    
	    // corrects stuff like '<tr><td>...<td>...');
	    if (!tagStack.isEmpty() && tagStack.peek().equals(tagName)) {
	       printWarning("Inserting </"+tagName+">");
	       output.append("</"+tagName+">");
	       tagStack.pop();
	    }
	    else {
	       processKnownChildTags(tagName, tagStack, output);
	    }

	    // otherwise, we assume there are no close tags required 
	    // before this open tag.
	    tagStack.push(tagName);

	    output.append("<"+tagName+newAttributes+">");
	 }
	 else {
	    output.append("<"+tagName+newAttributes+"/>");
	 }
      }
   }

   private boolean processKnownChildTags(String tagName, Stack tagStack, StringBuffer output) {

      TagInfo tagInfo = (TagInfo)tagInfoMap.get(tagName);
      if (null != tagInfo) {

	 String parentTag = null;
	 for (Enumeration en = tagStack.elements(); en.hasMoreElements(); ) {
	    String tag = (String)en.nextElement();
	    if (tagInfo.isLegalParentTag(tag)) {
	       parentTag = tag;
	       break;
	    }
	 }
	 if (parentTag != null) {
	    while (((String)tagStack.peek()) != parentTag) {
	       String poppedTagName = (String)tagStack.pop();
	       output.append("</"+poppedTagName+">");
	       printWarning("Inserting </"+poppedTagName+">");
	    }
	    return true;
	 }
      }
      return false;
   }

   private void flush() {
      
      // close all pending tags
      while (!tagStack.isEmpty()) {
	 String tagName = (String)tagStack.pop();
	 printWarning("Inserting </"+tagName+">");
	 output.append("</"+tagName+">");
      }
   }

   /**
    *  Takes HTML fragment and returns a well-formed XHTML
    *  equivalent.
    *
    *  In the returned String, all tags are properly closed and
    *  nested.
    *
    *  Currently, the returned String is not guaranteed to be
    *  well-formed. In particular there are no checks on the tag
    *  names, attribute names and entity names.  
    */
   public String getWellformedHTML(String text) {

      final int	STATE_INITIAL	      = 1;
      final int	STATE_TAG_START	      = 2;
      final int	STATE_TAG	      = 3;
      final int	STATE_TAG_DOUBLEQUOTE = 4;
      final int	STATE_TAG_SINGLEQUOTE = 5;
      final int	STATE_AMP	      = 6;

      int state = STATE_INITIAL;
      output.setLength(0);


      StringBuffer buf = new StringBuffer();
      char[] textAsChars = text.toCharArray();
      
   outer_loop:
      for (int i=0, ilim=textAsChars.length+1; i<ilim; ++i) {
	 int c;

	 if (i<textAsChars.length) {
	    c = textAsChars[i];
	 }
	 else {
	    c = -1;
	 }

	 switch (state) {

	 case STATE_INITIAL:
	    if ('<'==c) {
	       state = STATE_TAG_START;
	       if (buf.length()>0) {
		  haveText(buf.toString());
		  buf.setLength(0);
	       }
	    }
	    else if ('>'==c) {
	       // assume this is a greater-than sign
	       buf.append("&gt;");
	    }
	    else if ('&'==c) {
	       state = STATE_AMP;
	    }
	    else if (-1==c) {
	       if (buf.length()>0) {
		  haveText(buf.toString());
		  buf.setLength(0);
	       }
	       continue;
	    }
	    else {
	       buf.append((char)c);
	    }
	    break;

	 case STATE_AMP:
	    if ('<'==c) {
	       buf.append("&amp;");
	       state = STATE_TAG_START;
	       if (buf.length()>0) {
		  haveText(buf.toString());
		  buf.setLength(0);
	       }
	    }
	    else if ('>'==c) {
	       // assume this is a greater-than sign
	       buf.append("&amp;");
	       buf.append("&gt;");
	       state = STATE_INITIAL;
	    }
	    else if ('&'==c) {
	       buf.append("&amp;");
	       buf.append("&amp;");
	       state = STATE_INITIAL;
	    }
	    else if (-1==c) {
	       buf.append("&amp;");
	       haveText(buf.toString());
	       buf.setLength(0);
	       state = STATE_INITIAL;
	       continue;
	    }
            else {
               // peek forward and see whether this is a valid entity.
               if ('#'==c) {
                  buf.append("&");
                  buf.append((char)c);
                  state = STATE_INITIAL;
                  continue outer_loop;
               }
               else if (Character.isLetter((char)c)) {
                  for (int i2=i+1; i2<ilim-1; i2++) {
                     if (';' == textAsChars[i2]) {
                        buf.append("&");
                        buf.append((char)c);
                        state = STATE_INITIAL;
                        continue outer_loop;
                     }
                     else if (!Character.isLetter((char)c)
                              && !Character.isDigit((char)c)
                              && ".-_:".indexOf((char)c) < 0
                              //&& !isCombiningChar(c)  // FIXME
                              //&& !isExtender(c)       // FIXME
                              ) {
                        break;
                     }
                  }
                  // not a valid entity declaration; assume &amp;
               }
               buf.append("&amp;");
               buf.append((char)c);
               state = STATE_INITIAL;                  
            }

            /*
	    else if ('#'==c || Character.isLetter((char)c)) {
	       buf.append("&");
	       buf.append((char)c);
	       state = STATE_INITIAL;
	    }
	    else {
	       buf.append("&amp;");
	       buf.append((char)c);
	       state = STATE_INITIAL;	       
	    }
            */
	    break;
	    
	 case STATE_TAG_START:
	    if (" \t\r\n".indexOf(c)>=0) {
	       //continue;

	       // new: assume this is a less-sign
	       haveText("&lt;"+c);
	       state = STATE_INITIAL;
	    }
	    else if ('/'==c) {
	       buf.append((char)c);
	       state = STATE_TAG;
	    }
	    else if ('<'==c) {
	       // assume this is a less-sign
	       haveText("&lt;&lt;");
	       state = STATE_INITIAL;	       
	    }
	    else if ('>'==c) {
	       // assume this is a less-sign
	       haveText("&lt;&gt;");
	       state = STATE_INITIAL;	       
	    }
	    //else if ('-'==c || '+'==c || '='==c || '\''==c || "0123456789".indexOf(c)>=0) {
	    else if (!Character.isLetter((char)c)) {
	       // assume this is a less-sign
	       haveText("&lt;"+(char)c);
	       state = STATE_INITIAL;
	    }
	    else {
	       buf.append((char)c);
	       state = STATE_TAG;
	    }
	    break;
	    
	 case STATE_TAG:
	    if ('\"'==c) {
	       buf.append((char)c);
	       state = STATE_TAG_DOUBLEQUOTE;
	    }
	    else if ('\''==c) {
	       buf.append((char)c);
	       state = STATE_TAG_SINGLEQUOTE;
	    }
	    else if ('>'==c) {
	       state = STATE_INITIAL;
	       haveStartOrEndTag(buf.toString());
	       buf.setLength(0);
	    }
	    else if ('<'==c) {
	       // notify user, missing greater-than sign
	       haveStartOrEndTag(buf.toString());
	       buf.setLength(0);
	    }
	    else if (-1==c) {
	       printWarning("Unclosed tag at end-of-comment: <"+buf);
	       haveStartOrEndTag(buf.toString());
	       buf.setLength(0);
	    }
	    else {
	       buf.append((char)c);
	    }
	    break;

	 case STATE_TAG_DOUBLEQUOTE:
	    if ('\"'==c) {
	       buf.append((char)c);
	       state = STATE_TAG;
	    }
	    else if (-1==c) {
	       printWarning("Unclosed attribute value at end-of-comment.");
	       haveStartOrEndTag(buf.toString()+"\"");
	    }
	    else {
	       buf.append((char)c);
	    }
	    break;

	 case STATE_TAG_SINGLEQUOTE:
	    if ('\''==c) {
	       buf.append((char)c);
	       state = STATE_TAG;
	    }
	    else if (-1==c) {
	       printWarning("Unclosed attribute value at end-of-comment.");
	       haveStartOrEndTag(buf.toString()+"'");
	    }
	    else {
	       buf.append((char)c);
	    }
	    break;
	 }
      }

      return output.toString();
   }

   private String getContext() {
      if (null != contextClass) {
         StringBuffer rc = new StringBuffer();
         rc.append(contextClass.qualifiedTypeName());
         if (null != contextMember) {
            rc.append("."+contextMember.toString());
         }
         return rc.toString();
      }
      else {
         return null;
      }
   }

   private void printWarning(String msg) {
      if (null != warningReporter && !noWarn) {
         String context = getContext();
         if (null != context) {
            warningReporter.printWarning("In "+getContext()+": "+msg);
         }
         else {
            warningReporter.printWarning("In overview page: "+msg);
         }
      }
   }

   public String terminateText() {
      output.setLength(0);
      flush();
      return output.toString();
   }
}


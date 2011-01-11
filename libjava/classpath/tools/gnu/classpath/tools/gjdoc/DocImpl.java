/* gnu.classpath.tools.gjdoc.DocImpl
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

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.*;
import java.util.*;
import java.text.*;
import java.io.File;
import javax.swing.text.Segment;

/**
 *  Represents the least common denominator of all Javadoc
 *  comment classes.
 */
public abstract class DocImpl implements Doc, TagContainer {

   protected static Tag[] seeTagEmptyArr = new SeeTagImpl[0];
   protected static Tag[] linkTagEmptyArr = new LinkTagImpl[0];
   protected static Tag[] paramTagEmptyArr = new ParamTagImpl[0];
   protected static Tag[] throwsTagEmptyArr = new ThrowsTagImpl[0];
   protected SourcePosition position;
   private String boilerplateComment;

   // Return the text of the comment for this doc item.
   public String commentText() {

      StringBuffer rc=new StringBuffer();

      Tag[] textTags=(Tag[])tagMap.get("text");
      if (textTags!=null) {
         for (int i=0; i<textTags.length; ++i) {
            rc.append(textTags[i].text());
         }
      }
      return rc.toString();
   }

   // Compares this Object with the specified Object for order.
   public int compareTo(java.lang.Object o) {
      return Main.getInstance().getCollator().compare(name(), ((Doc)o).name());
   }

   // Return the first sentence of the comment as tags.
   public Tag[] firstSentenceTags() {

      Tag[] rc=(Tag[])tagMap.get("first");
      if (rc==null) rc=new Tag[0];
      return rc;
   }

   // Return the full unprocessed text of the comment.
   public String getRawCommentText() {
      if (rawDocumentation!=null)
          return rawDocumentation;
      else if (rawDocOffset>=0)
         return Main.getRootDoc().readRawComment(rawDocOffset);
      else
         return null;
   }

   // Return comment as tags.
   public Tag[] inlineTags() {

      Tag[] rc=(Tag[])tagMap.get("inline");
      if (rc==null) rc=new Tag[0];
      return rc;
   }

   // Is this Doc item a class.
   public boolean isClass() {
      return false;
   }

   // Is this Doc item a constructor? False until overridden.
   public boolean isConstructor() {
      return false;
   }

   // Is this Doc item a error class? False until overridden.
   public boolean isError() {
      return false;
   }

   // Is this Doc item a exception class? False until overridden.
   public boolean isException() {
      return false;
   }

   // Is this Doc item a field? False until overridden.
   public boolean isField() {
      return false;
   }

   // return true if this Doc is include in the active set.
   public boolean isIncluded() {
      return false;
   }

   // Is this Doc item a interface? False until overridden.
   public boolean isInterface() {
      return false;
   }

   // Is this Doc item a simple method (i.e.
   public boolean isMethod() {
      return false;
   }

   public boolean isPackage() {
      return false;
   }

   // Is this Doc item a ordinary class (i.e.
   public boolean isOrdinaryClass() {
      return false;
   }

   // Return the see also tags in this Doc item.
   public SeeTag[] seeTags() {
      return (SeeTag[])getTagArr("see", seeTagEmptyArr);
   }

   protected Tag[] getTagArr(String kindOfTag, Tag[] defaultRc) {
      Tag[] rc=(Tag[])tagMap.get(kindOfTag);
      if (rc==null) rc=defaultRc;
      return rc;
   }

   // Set the full unprocessed text of the comment.
   public void setRawCommentText(String rawDocumentation) {
      this.rawDocumentation=rawDocumentation;
   }

   public void resolveComments() {

      if (rawDocumentation!=null && tagMap.isEmpty()) {
         char[] charArray = rawDocumentation.toCharArray();
         int length = rawDocumentation.length();
         int startOffset = 0;
         int endOffset = 0;
         if (charArray[0] == '/'
             && charArray[1] == '*'
             && charArray[2] == '*'
             && charArray[length - 2] == '*'
             && charArray[length - 1] == '/') {

            startOffset = 3;
            endOffset = 2;
         }

         this.tagMap=parseCommentTags(charArray,
                                      startOffset,
                                      length - endOffset,
                                      getContextClass(),
                                      getContextMember(),
                                      null,
                                      boilerplateComment);

         if (Main.getInstance().isCacheRawComments()) {
            rawDocOffset=Main.getRootDoc().writeRawComment(rawDocumentation);
            rawDocumentation=null;
         }

         resolveTags();
      }
      else if (tagMap.isEmpty() && null != boilerplateComment) {
         tagMap.put("all", new Tag[] { new TagImpl("@boilerplate", boilerplateComment,getContextClass(),null) });
         tagMap.put("@boilerplate", new Tag[] { new TagImpl("@boilerplate", boilerplateComment,getContextClass(),null) });
      }
   }

   public static int skipHtmlWhitespace(char[] buffer, int startIndex) {
      while (startIndex < buffer.length) {
         char c=buffer[startIndex];
         if (!Parser.isWhitespace(c)) {
            break;
         }
         else {
            ++ startIndex;
         }
      }
      return startIndex;
   }

   /**
    *  Looks for an end-of-sentence marker in <code>text</code>,
    *  starting at <code>startIndex</code> and stopping at
    *  <code>endIndex</code>.
    *
    *  @param text  the text to be searched
    *  @param startIndex  index in <code>text</code> at which to start
    *  @param endIndex  index in <code>text</code> at which to stop
    *
    *  @return the index of the character following the end-of-sentence
    *    marker, <code>endIndex</code> if no end-of-sentence
    *    marker could be found, or -1 if not implemented.
    */
   private static int findEndOfSentence(char[] text, int startIndex,
                                        int endIndex)
   {
      if (Main.getInstance().isUseBreakIterator()) {
         Segment segment = new Segment(text, startIndex, endIndex - startIndex);
         BreakIterator breakIterator = BreakIterator.getSentenceInstance(Main.getInstance().getLocale());
         breakIterator.setText(segment);
         int result = breakIterator.next();
         if (BreakIterator.DONE == result) {
            return endIndex;
         }
         else {
            return result;
         }
      }
      else {
         while (startIndex < endIndex) {
            if (text[startIndex] == '.'
                && (startIndex+1 == endIndex
                    || Character.isWhitespace(text[startIndex+1])
                    || isHTMLBreakTag(text, startIndex+1, endIndex)
                    )) {
               return startIndex;
            }

            startIndex++;
         }
         return endIndex;
      }
   }

   /**
    * Returns true is the text from start to end begins with a 'p' or 'br' tag.
    */
   private static boolean isHTMLBreakTag(char[] text, int start, int end)
   {
      String[] breakTags = {
         "p>", "/p>", "h1>", "h2>", "h3>", "h4>", "h5>", "h6>", "hr>",
         "pre>", "/pre>"
      };

      if (text[start] == '<') {

      outer:
         for (int i=0; i<breakTags.length; ++i) {
            String tag = breakTags[i];
            int len = tag.length();
            if (start + len < end) {
               for (int j=0; j<len; ++j) {
                  char c = tag.charAt(j);
                  if (Character.toLowerCase(text[start + 1 + j]) != c) {
                     continue outer;
                  }
               }
               return true;
            }
         }
      }
      return false;
   }

   //private static final StringBuffer buf=new StringBuffer(32768);
   private static final StringBuffer whitespaceBuf=new StringBuffer();
   private static char[] charBuf = new char[60000];
   private static int bufPos = 0;

   private static void appendToBuf(char c)
   {
      if (bufPos < charBuf.length) {
         charBuf[bufPos++] = c;
      }
      else {
         //
      }
   }

   private static void appendToBuf(StringBuffer s)
   {
      if (bufPos + s.length() <= charBuf.length) {
         s.getChars(0, s.length(), charBuf, bufPos);
         bufPos += s.length();
      }
      else {
         //
      }
   }

   private static void setBufLength(int length)
   {
      bufPos = 0;
   }

   private static String bufToString()
   {
      return new String(charBuf, 0, bufPos);
   }

   private static int bufLength()
   {
      return bufPos;
   }

   public static Map parseCommentTags(char[] comment, int startIndex, int endIndex,
                                      ClassDocImpl contextClass, MemberDocImpl contextMember,
                                      AbstractTagImpl contextTag, String boilerplateComment) {

      int rawDocStart=skipHtmlWhitespace(comment, startIndex);

      int firstSentenceEnd = 0;

      if (comment.length>rawDocStart) {

         firstSentenceEnd = findEndOfSentence(comment, rawDocStart, comment.length);

         if (firstSentenceEnd < 0) {
            BreakIterator boundary = BreakIterator.getSentenceInstance(Locale.ENGLISH);
            boundary.setText(new ArrayCharacterIterator(comment, rawDocStart));
            boundary.first();
            boundary.next();
            firstSentenceEnd = boundary.current();
         }

         // Always include period at end of sentence if there is one.
         if (firstSentenceEnd < comment.length
                         && '.' == comment[firstSentenceEnd]) {
            ++ firstSentenceEnd;
         }
      }

      final int STATE_BEGOFLINE            = 1;
      final int STATE_TEXT                 = 2;
      final int STATE_PARAM                = 3;
      final int STATE_PARAMVALUE           = 4;
      final int STATE_PARAMWRAP            = 5;
      final int STATE_INLINEPARAM          = 6;
      final int STATE_INLINEPARAMVALUE     = 7;
      final int STATE_WHITESPACE           = 8;
      final int STATE_INLINEPARAMVALUE_BOL = 9;
      final int STATE_IPV_WHITESPACE       = 10;

      int state=STATE_BEGOFLINE;
      int prevState=STATE_TEXT;

      setBufLength(0);
      whitespaceBuf.setLength(0);

      String paramName="", paramValue="";

      Map tags=new HashMap();
      tags.put("inline", new LinkedList());
      tags.put("first", new LinkedList());
      tags.put("all", new LinkedList());

      final char EOL=(char)-1;

      for (int i=rawDocStart; i<=endIndex; ++i) {
         char c=(i<endIndex)?comment[i]:EOL;
         char peek=(i<endIndex-1)?comment[i+1]:EOL;

         switch (state){

         case STATE_BEGOFLINE:
            if (i==firstSentenceEnd) {
               AbstractTagImpl newTag = addTag(tags, "text", bufToString(), true, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
               setBufLength(0);
            }

            if (Parser.isWhitespace(c)) {
               // ignore
            }
            else if (c=='*') {
               // ignore, but go to STATE_TEXT
               if (peek!='*' && peek!='@' && peek!=EOL) {
                  state=STATE_WHITESPACE;
               }
            }
            else if (c=='@' || (c=='{' && peek=='@') || c==EOL) {
               if (bufLength()>0) {
                  addTag(tags, "text", bufToString(), i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
                  setBufLength(0);
               }
               if (c=='{') {
                  ++i;
                  state=STATE_INLINEPARAM;
               }
               else {
                  state=STATE_PARAM;
               }
            }
            else {
               state=STATE_TEXT;
               appendToBuf(whitespaceBuf);
               whitespaceBuf.setLength(0);
               appendToBuf(c);
            }
            break;

         case STATE_WHITESPACE:
            if (i==firstSentenceEnd) {
               AbstractTagImpl newTag = addTag(tags, "text", bufToString(), true, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
               setBufLength(0);
            }

            if (c=='\n') {
               whitespaceBuf.append(c);
               state=STATE_BEGOFLINE;
            }
            else if (Parser.isWhitespace(c)) {
               whitespaceBuf.append(c);
            }
            else if (c=='@' || (c=='{' && peek=='@') || c==EOL) {
               if (bufLength()>0) {
                  AbstractTagImpl newTag = addTag(tags, "text", bufToString(), i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
                  if (null != newTag) {
                     contextTag = newTag;
                  }
                  setBufLength(0);
               }
               if (c=='{') {
                  ++i;
                  state=STATE_INLINEPARAM;
               }
               else {
                  state=STATE_PARAM;
               }
            }
            else {
               appendToBuf(whitespaceBuf);
               whitespaceBuf.setLength(0);
               appendToBuf(c);
               state=STATE_TEXT;
            }
            break;

         case STATE_PARAMWRAP:
            if (c=='\n') {
               appendToBuf(c);
            }
            else if (Parser.isWhitespace(c)) {
               // ignore
            }
            else if (c=='*') {
               // ignore, but go to STATE_TEXT
               /*
               if (i<endIndex && comment[i+1]!='*' && comment[i+1]!='@') {
                  state=STATE_PARAMVALUE;
               }
               */
            }
            else if (c=='@' || c==EOL) {
               paramValue=bufToString();
               AbstractTagImpl newTag = addTag(tags, paramName, paramValue, i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
               setBufLength(0);
               if (c=='{') {
                  ++i;
                  state=STATE_INLINEPARAM;
               }
               else {
                  state=STATE_PARAM;
               }
            }
            else {
               state=STATE_PARAMVALUE;
               appendToBuf(c);
            }
            break;

         case STATE_PARAM:
            if (!(c==EOL || Parser.isWhitespace(c))) {
               appendToBuf(c);
            }
            else if (c=='\n') {
               paramName=bufToString();
               setBufLength(0);
               state=STATE_PARAMWRAP;
            }
            else {
               paramName=bufToString();
               setBufLength(0);
               state=STATE_PARAMVALUE;
            }
            break;

         case STATE_INLINEPARAM:
            if (c=='}') {
               // tag without value
               paramName=bufToString();
               AbstractTagImpl newTag = addTag(tags, paramName, "", i<firstSentenceEnd, contextClass, contextMember, contextTag, true);
               if (null != newTag) {
                  contextTag = newTag;
               }
               state=prevState;
               setBufLength(0);
            }
            else if (!(c==EOL || Parser.isWhitespace(c))) {
               appendToBuf(c);
            }
            else if (c=='\n') {
               paramName=bufToString();
               setBufLength(0);
               state=STATE_INLINEPARAMVALUE_BOL;
            }
            else {
               paramName=bufToString();
               setBufLength(0);
               state=STATE_INLINEPARAMVALUE;
            }
            break;

         case STATE_PARAMVALUE:
            if (c==EOL) {
               paramValue=bufToString();
               AbstractTagImpl newTag = addTag(tags, paramName, paramValue, i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
            }
            else if (c=='\n') {
               appendToBuf(c);
               state=STATE_PARAMWRAP;
            }
            else {
               appendToBuf(c);
            }
            break;

         case STATE_INLINEPARAMVALUE:
            if (c=='\n') {
               appendToBuf(c);
               state=STATE_INLINEPARAMVALUE_BOL;
            }
            else if (c==EOL || c=='}') {
               paramValue=bufToString();
               AbstractTagImpl newTag = addTag(tags, paramName, paramValue, i<firstSentenceEnd, contextClass, contextMember, contextTag, true);
               if (null != newTag) {
                  contextTag = newTag;
               }
               state=prevState;
               setBufLength(0);
            }
            else {
               appendToBuf(c);
            }
            break;

         case STATE_INLINEPARAMVALUE_BOL:
            if (Parser.isWhitespace(c)) {
               // ignore
            }
            else if (c=='*') {
               // ignore, but go to STATE_TEXT
               if (i<endIndex && peek!='*') {
                  state=STATE_IPV_WHITESPACE;
               }
            }
            else if (c==EOL) {
               if (bufLength()>0) {
                  AbstractTagImpl newTag = addTag(tags, "text", bufToString(), i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
                  if (null != newTag) {
                     contextTag = newTag;
                  }
               }
            }
            else {
               state=STATE_INLINEPARAMVALUE;
               appendToBuf(whitespaceBuf);
               whitespaceBuf.setLength(0);
               appendToBuf(c);
            }
            break;

         case STATE_IPV_WHITESPACE:
            if (c=='\n') {
               whitespaceBuf.append(c);
               state=STATE_INLINEPARAMVALUE_BOL;
            }
            else if (Parser.isWhitespace(c)) {
               whitespaceBuf.append(c);
            }
            else if (c==EOL) {
               if (bufLength()>0) {
                  AbstractTagImpl newTag = addTag(tags, "text", bufToString(), i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
                  if (null != newTag) {
                     contextTag = newTag;
                  }
                  setBufLength(0);
               }
            }
            else {
               appendToBuf(whitespaceBuf);
               whitespaceBuf.setLength(0);
               appendToBuf(c);
               state=STATE_INLINEPARAMVALUE;
            }
            break;

         case STATE_TEXT:
            if (i==firstSentenceEnd) {
               AbstractTagImpl newTag = addTag(tags, "text", bufToString(), true, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
               setBufLength(0);
            }

            if (c==EOL) {
               paramValue=bufToString();
               AbstractTagImpl newTag = addTag(tags, "text", paramValue, i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
            }
            else if (c=='\n') {
               appendToBuf(c);
               state=STATE_BEGOFLINE;
            }
            else if (c=='{' && peek=='@') {
               paramValue=bufToString();
               AbstractTagImpl newTag = addTag(tags, "text", paramValue, i<firstSentenceEnd, contextClass, contextMember, contextTag, false);
               if (null != newTag) {
                  contextTag = newTag;
               }
               ++i;
               setBufLength(0);
               state=STATE_INLINEPARAM;
            }
            else {
               appendToBuf(c);
            }
            break;

         default:
            throw new Error("illegal state "+state);
         }
      }


      if (null == contextMember && null != boilerplateComment && Main.getInstance().isCopyLicenseText()) {
         addTag(tags, "@boilerplate", boilerplateComment, false, contextClass, null, null, false);
      }

      Map rc=new HashMap();

      for (Iterator it=tags.keySet().iterator(); it.hasNext(); ) {
         String key=(String)it.next();
         Tag[] templateArr;
         List list=(List)tags.get(key);

         if ("see".equals(key))
            templateArr=new SeeTag[list.size()];
         else if ("param".equals(key))
            templateArr=new ParamTag[list.size()];
         else if ("serialField".equals(key))
            templateArr=new SerialFieldTag[list.size()];
         else if ("throws".equals(key) || "exception".equals(key))
            templateArr=new ThrowsTag[list.size()];
         else {
            templateArr=new Tag[list.size()];
         }

         rc.put(key, list.toArray(templateArr));
      }

      return rc;
   }

   private ClassDocImpl getContextClass() {
      if (isClass() || isInterface()) {
         return (ClassDocImpl)this;
      }
      else if (isField() || isMethod() || isConstructor()) {
         return (ClassDocImpl)((MemberDocImpl)this).containingClass();
      }
      else {
         return null;
      }
   }

   private MemberDocImpl getContextMember() {
      if (isField() || isMethod() || isConstructor()) {
         return (MemberDocImpl)this;
      }
      else {
         return null;
      }
   }

   protected static AbstractTagImpl addTag(Map tags, String name,
                                           String value, boolean isFirstSentence,
                                           ClassDocImpl contextClass,
                                           MemberDocImpl contextMember,
                                           AbstractTagImpl contextTag,
                                           boolean isInline) {

      AbstractTagImpl tag = null;

      boolean haveValue = (0 != value.trim().length());

      String emptyWarning = "Empty @" + name + " tag.";

      if (name.equals("param")) {
         if (haveValue) {
            tag=new ParamTagImpl(value, contextClass, contextMember);
         }
         else {
            //printWarning(emptyWarning);
         }
      }
      else if (name.equals("see")) {
         if (haveValue) {
            tag=new SeeTagImpl(value, contextClass);
         }
         else {
            //printWarning(emptyWarning);
         }
      }
      else if (name.equals("link") || name.equals("linkplain")) {
         if (haveValue) {
            tag=new LinkTagImpl("@" + name, value, contextClass);
            isInline = true;
         }
         else {
            //printWarning(emptyWarning);
         }
      }
      else if (name.equals("value")) {
         if (haveValue) {
            tag=new ValueTagImpl(value, contextClass);
            isInline = true;
         }
         else {
            //printWarning(emptyWarning);
         }
      }
      else if (name.equals("inheritDoc")) {
         if (haveValue) {
            //printWarning("@inheritDoc tags are not supposed to have any content.");
         }
         tag=new InheritDocTagImpl(contextClass, contextMember, contextTag);
         isInline = true;
      }
      else if (name.equals("serialField")) {
         if (haveValue) {
            tag=new SerialFieldTagImpl(value, contextClass, contextMember);
         }
         else {
            //printWarning(emptyWarning);
         }
      }
      else if (name.equals("throws") || name.equals("exception")) {
         if (haveValue) {
            tag=new ThrowsTagImpl(value, contextClass, contextMember);
         }
         else {
            //printWarning(emptyWarning);
         }
         name="throws";
      }
      else if (name.equals("text")) {
         tag=new TextTagImpl(value);
         isInline = true;
      }
      else {
         tag=new TagImpl("@"+name, value.trim(), contextClass, contextMember);
         // FIXME: consider taglets
      }

      if (tag != null) {
         if (isInline) {
            ((List)tags.get("inline")).add(tag);
            if (isFirstSentence) {
               if (name.equals("text")) {
                  String txt = ((TextTagImpl)tag).getText();
                  Tag newTag;
                  if (txt.startsWith("<p>")) {
                     newTag = new TextTagImpl(txt.substring(3));
                  }
                  else if (txt.endsWith("</p>")) {
                     newTag = new TextTagImpl(txt.substring(0, txt.length() - 4));
                  }
                  else {
                     newTag = tag;
                  }
                  ((List)tags.get("first")).add(newTag);

               }
               else {
                  ((List)tags.get("first")).add(tag);
               }
            }
         }
         else {
            ((List)tags.get("all")).add(tag);
         }

         List l=((List)tags.get(name));
         if (l==null) {
            l=new LinkedList();
            tags.put(name,l);
         }
         l.add(tag);

         return isInline ? tag : contextTag;
      }
      else {
         return null;
      }
   }

   // Return all tags in this Doc item.
   public Tag[] tags() {
      Tag[] rc=(Tag[])tagMap.get("all");
      if (rc==null) rc=new Tag[0];
      return rc;
   }

   // Return tags of the specified kind in this Doc item.
   public Tag[] tags(java.lang.String tagname) {
      Tag[] rc=(Tag[])tagMap.get(tagname);
      if (rc==null) rc=new Tag[0];
      return rc;
   }

   protected String rawDocumentation;
   protected long rawDocOffset=-1;

   protected Map tagMap = new HashMap();

   public Map getTagMap() { return tagMap; }

   protected void resolveTags() {

      Tag[] tags=tags();
      for (int i=0; i<tags.length; ++i) {
         ((AbstractTagImpl)tags[i]).resolve();
      }

      Tag[] inlineTags=inlineTags();
      for (int i=0; i<inlineTags.length; ++i) {
         ((AbstractTagImpl)inlineTags[i]).resolve();
      }
   }

   private static Map classDocToFileMap = new HashMap();

   private static File getFile(ClassDoc classDoc) {
      File result = (File)classDocToFileMap.get(classDoc);
      if (null == result) {
         result = new File(((GjdocPackageDoc)classDoc.containingPackage()).packageDirectory(),
                           classDoc.name() + ".java");
         classDocToFileMap.put(classDoc, result);
      }
      return result;
   }

   public static SourcePosition getPosition(ClassDoc classDoc)
   {
      return new SourcePositionImpl(getFile(classDoc), 0, 0);
   }

   public static SourcePosition getPosition(ClassDoc classDoc, char[] source, int startIndex)
   {
      int column = 0;
      int line = 0;
      for (int i=0; i<startIndex; ++i) {
         if (10 == source[i]) {
            ++ line;
            column = 0;
         }
         else if (13 != source[i]) {
            ++ column;
         }
      }
      while (true) {
         ClassDoc containingClassDoc = classDoc.containingClass();
         if (null != containingClassDoc) {
            classDoc = containingClassDoc;
         }
         else {
            break;
         }
      }

      File file = getFile(classDoc);

      return new SourcePositionImpl(file, line + 1, column + 1);
   }

   public SourcePosition position()
   {
      return this.position;
   }

   public DocImpl(SourcePosition position)
   {
      this.position = position;
   }

   public void setPosition(SourcePosition position)
   {
      this.position = position;
   }

   private static TagContainer checkForInheritedDoc(ClassDoc classDoc,
                                                    MemberDocImpl memberDoc,
                                                    AbstractTagImpl tag)
   {
      DocImpl result;

      if (!(classDoc instanceof ClassDocImpl)) {
         result = null;
      }
      else if (null == memberDoc) {
         result = (DocImpl)classDoc;
      }
      else if (memberDoc.isField()) {
         result = (DocImpl)((ClassDocImpl)classDoc).getFieldDoc(memberDoc.name());
      }
      else if (memberDoc.isMethod()) {
         result = (DocImpl)((ClassDocImpl)classDoc).getMethodDoc(memberDoc.name(),
                                                                 ((MethodDoc)memberDoc).signature());
      }
      else if (memberDoc.isConstructor()) {
         result = (DocImpl)((ClassDocImpl)classDoc).getConstructorDoc(((ConstructorDoc)memberDoc).signature());
      }
      else {
         //assert(false);
         throw new RuntimeException("memberDoc is supposed to be field, method or constructor");
      }

      if (null != result
          && null != memberDoc
          && null != tag) {

         TagContainer tagDoc = null;

         Tag[] tags = result.tags();
         for (int i=0; i<tags.length; ++i) {
            if (tags[i].kind().equals(tag.kind())) {
               if ("@param".equals(tag.kind())) {
                  if (((ParamTagImpl)tags[i]).parameterName().equals(((ParamTagImpl)tag).parameterName())) {
                     tagDoc = (TagContainer)tags[i];
                     break;
                  }
               }
               else if ("@throws".equals(tag.kind())) {
                  if (((ThrowsTagImpl)tags[i]).exceptionName().equals(((ThrowsTagImpl)tag).exceptionName())) {
                     tagDoc = (TagContainer)tags[i];
                     break;
                  }
               }
               else if ("@return".equals(tag.kind())) {
                  tagDoc = (TagContainer)tags[i];
               }
            }
         }

         return tagDoc;
      }

      if (null == result || result.isEmptyDoc()) {
         return null;
      }
      else {
         return result;
      }
   }

   public static TagContainer findInheritedDoc(ClassDoc classDoc,
                                               MemberDocImpl memberDoc,
                                               AbstractTagImpl tag)
   {
      TagContainer result;

      // (Taken from Javadoc Solaris Tool documentation 1.5,
      // section "Automatic Copying of Method Comments")

      // Algorithm for Inheriting Method Comments - If a method does
      // not have a doc comment, or has an {@inheritDoc} tag, the
      // Javadoc tool searches for an applicable comment using the
      // following algorithm, which is designed to find the most
      // specific applicable doc comment, giving preference to
      // interfaces over superclasses:

      // 1. Look in each directly implemented (or extended) interface
      // in the order they appear following the word implements (or
      // extends) in the method declaration. Use the first doc comment
      // found for this method.

      ClassDoc[] interfaces = classDoc.interfaces();
      if (null != interfaces) {
         for (int i=0; i<interfaces.length; ++i) {
            result = checkForInheritedDoc(interfaces[i], memberDoc, tag);
            if (null != result) {
               return result;
            }
         }
      }

      // 2. If step 1 failed to find a doc comment, recursively apply
      // this entire algorithm to each directly implemented (or
      // extended) interface, in the same order they were examined
      // in step 1.

      if (null != interfaces) {
         for (int i=0; i<interfaces.length; ++i) {
            result = findInheritedDoc(interfaces[i], memberDoc, tag);
            if (null != result) {
               return result;
            }
         }
      }

      ClassDoc superclassDoc = classDoc.superclass();

      // 3. If step 2 failed to find a doc comment and this is a class
      // other than Object (not an interface):
      if (!classDoc.isInterface()
          && null != superclassDoc
          && !"java.lang.Object".equals(classDoc.qualifiedTypeName())) {

         // 3a. If the superclass has a doc comment for this method, use it.

         result = checkForInheritedDoc(superclassDoc, memberDoc, tag);
         if (null != result) {
            return result;
         }

         // 3b. If step 3a failed to find a doc comment, recursively
         // apply this entire algorithm to the superclass.

         return findInheritedDoc(superclassDoc,
                                 memberDoc, tag);
      }
      else {
         return null;
      }
   }

   public boolean isEmptyDoc()
   {
      return tagMap.isEmpty();
   }

   void setBoilerplateComment(String boilerplateComment)
   {
      this.boilerplateComment = boilerplateComment;
   }
}

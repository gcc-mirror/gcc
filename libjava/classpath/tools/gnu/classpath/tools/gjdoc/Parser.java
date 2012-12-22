/* gnu.classpath.tools.gjdoc.Parser
   Copyright (C) 2001, 2005, 2008, 2012 Free Software Foundation, Inc.

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

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.ConstructorDoc;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.PackageDoc;

import gnu.classpath.tools.IOToolkit;
import gnu.classpath.tools.NotifyingInputStreamReader;
import gnu.classpath.tools.MalformedInputListener;
import gnu.classpath.tools.MalformedInputEvent;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.Reader;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

   class IgnoredFileParseException extends ParseException
   {
      // marker exception
   }

   abstract class SourceComponent {

      abstract int match(char[] source, int index) throws ParseException;

      int process(Parser parser, char[] source, int startIndex, int endIndex) throws ParseException, IOException {
         return endIndex;
      }

      int getEndIndex(char[] source, int endIndex) throws ParseException {
         return endIndex;
      }
   }

   abstract class BlockSourceComponent extends SourceComponent {

      int getEndIndex(char[] source, int endIndex) throws ParseException {
         return Parser.skipExpression(source, endIndex, 1, '\0');
      }

   }

   class Whitespace extends SourceComponent {

      int match(char[] source, int index) {

         int rc=index;
         int slen=source.length;
         while (rc<slen && Parser.WHITESPACE.indexOf(source[rc])>=0) ++rc;

         return (rc!=index) ? rc : -1;
      }
   }

   class BracketClose extends SourceComponent {

      int match(char[] source, int index) {
         if (source[index]=='}') {
            return index+1;
         }
         else {
            return -1;
         }
      }

     int process(Parser parser, char[] source, int startIndex, int endIndex)
       throws ParseException, IOException
     {
       parser.classClosed();
       return endIndex;
     }
   }

   class CommentComponent extends SourceComponent {

      int match(char[] source, int index) throws ParseException {
         if (index+1<source.length && source[index]=='/' && source[index+1]=='*') {
            for (index+=2; index+1<source.length; ++index) {
               if (source[index]=='*' && source[index+1]=='/')
                  return index+2;
            }
            throw new ParseException("unexpected end of input");
         }
         return -1;
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) {

         if (source[startIndex+0]=='/'
             && source[startIndex+1]=='*'
             && source[startIndex+2]=='*') {

            parser.setLastComment(new String(source, startIndex, endIndex-startIndex));
         }
         else if (null == parser.getBoilerplateComment() && Main.getInstance().isCopyLicenseText()) {
            String boilerplateComment = new String(source, startIndex + 2, endIndex-startIndex - 4);
            if (boilerplateComment.toLowerCase().indexOf("copyright") >= 0) {
               parser.setBoilerplateComment(boilerplateComment);
            }
         }

         return endIndex;
      }
   }

   class SlashSlashCommentComponent extends SourceComponent {

      int match(char[] source, int index) {
         if (index+1<source.length && source[index]=='/' && source[index+1]=='/') {
            index+=2;
            while (index<source.length && source[index]!='\n')
               ++index;
            return index;
         }
         else {
            return -1;
         }
      }
   }

   class EmptyStatementComponent extends SourceComponent {

      int match(char[] source, int index) {
         while (index < source.length
                && Parser.isWhitespace(source[index])) {
            ++ index;
         }
         if (index < source.length && source[index] == ';') {
            return index+1;
         }
         else {
            return -1;
         }
      }
   }

   class ImportComponent extends SourceComponent {

      int match(char[] source, int index) {
         if (index+7<source.length) {
            if (source[index+0]=='i'
                && source[index+1]=='m'
                && source[index+2]=='p'
                && source[index+3]=='o'
                && source[index+4]=='r'
                && source[index+5]=='t'
                && Parser.WHITESPACE.indexOf(source[index+6])>=0) {

               for (index+=7; index<source.length && source[index]!=';'; ++index)
                  ;

               return index+1;
            }
         }
         return -1;
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) throws ParseException, IOException {
         String importString=new String(source,startIndex+7,endIndex-startIndex-7-1).trim();
         parser.importEncountered(importString);
         return endIndex;
      }
   }

   class PackageComponent extends SourceComponent {

      int match(char[] source, int index) {
         if (index+10<source.length) {
            if (source[index+0]=='p'
                && source[index+1]=='a'
                && source[index+2]=='c'
                && source[index+3]=='k'
                && source[index+4]=='a'
                && source[index+5]=='g'
                && source[index+6]=='e'
                && Parser.WHITESPACE.indexOf(source[index+7])>=0) {

               for (index+=7; index<source.length && source[index]!=';'; ++index)
                  ;

               return index+1;
            }
         }
         return -1;
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) {
         String packageName=new String(source,startIndex+8,endIndex-startIndex-8-1).trim();
         parser.packageOpened(packageName);
         return endIndex;
      }
   }

   class FieldComponent extends SourceComponent {

      int match(char[] source, int index) throws ParseException {
         boolean isField=false;
         final int STATE_NORMAL=1;
         final int STATE_SLASHC=2;
         final int STATE_STARC=3;
         final int STATE_FIELDVAL=4;
         final int STATE_STRING=5;
         final int STATE_SINGLEQUOTED=6;
         final int STATE_STRING_BS=7;
         final int STATE_SINGLEQUOTED_BS=8;

         int state=STATE_NORMAL;
         int prevState=STATE_NORMAL;

         int fieldValueLevel = 0;

         for (; index<source.length && !isField; ++index) {
            if (state==STATE_STARC) {
               if (index<source.length-1 && source[index]=='*' && source[index+1]=='/') {
                  ++index;
                  state=prevState;
               }
            }
            else if (state==STATE_SLASHC) {
               if (source[index]=='\n') {
                  state=prevState;
               }
            }
            else if (state==STATE_STRING) {
               if (source[index]=='\\') {
                  state=STATE_STRING_BS;
               }
               else if (source[index]=='\"') {
                  state=prevState;
               }
            }
            else if (state==STATE_STRING_BS) {
               state=STATE_STRING;
            }
            else if (state==STATE_SINGLEQUOTED) {
               if (source[index]=='\\') {
                  state=STATE_SINGLEQUOTED_BS;
               }
               else if (source[index]=='\'') {
                  state=prevState;
               }
            }
            else if (state==STATE_SINGLEQUOTED_BS) {
               state=STATE_SINGLEQUOTED;
            }
            else if (state==STATE_FIELDVAL) {
               if (source[index]=='/') {
                  if (index<source.length-1 && source[index+1]=='*') {
                     state=STATE_STARC;
                     ++index;
                  }
                  else if (index<source.length-1 && source[index+1]=='/') {
                     state=STATE_SLASHC;
                     ++index;
                  }
               }
               else if (source[index]=='{') {
                  ++ fieldValueLevel;
               }
               else if (source[index]=='}') {
                  -- fieldValueLevel;
               }
               else if (source[index]=='\"') {
                  state=STATE_STRING;
               }
               else if (source[index]=='\'') {
                  state=STATE_SINGLEQUOTED;
               }
               else if (source[index]==';' && 0 == fieldValueLevel) {
                  isField=true;
                  break;
               }
            }
            else switch (source[index]) {
            case '/':
               if (index<source.length-1 && source[index+1]=='*') {
                  state=STATE_STARC;
                  ++index;
               }
               else if (index<source.length-1 && source[index+1]=='/') {
                  state=STATE_SLASHC;
                  ++index;
               }
               break;
            case '{':  // class
            case '(':  // method
               return -1;
            case '=':  // field
               state=STATE_FIELDVAL;
               prevState=state;
               continue;
            case ';':  // field
               isField=true;
               break;
            }
            if (isField) break;
         }
         if (!isField || index==source.length) {
            return -1;
         }

         //System.err.println("char is "+source[index]);

         if (source[index]!=';') {
            index=Parser.skipExpression(source, index, 0, ';');
         }
         return index+1;
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) {

         //Debug.log(9,"found package statement: \""+str+"\"");
         //Debug.log(9,"found function component: '"+str+"'");
         //xxx(new FieldDocImpl(ctx.classDoc, ctx.classDoc.containingPackage(), 0, false, false));

         // Ignore superfluous semicoli after class definition
         if (endIndex-startIndex<=1) return endIndex;

         //assert (parser.ctx!=null);
         Collection<FieldDoc> fields = FieldDocImpl.createFromSource(parser.ctx.classDoc,
                                                                     parser.ctx.classDoc.containingPackage(),
                                                                     source, startIndex, endIndex);

         for (Iterator<FieldDoc> it=fields.iterator(); it.hasNext(); ) {
            FieldDocImpl field=(FieldDocImpl)it.next();
            boolean fieldHasSerialTag=!field.isTransient() && !field.isStatic(); //field.hasSerialTag();
            if ((field.isIncluded() || fieldHasSerialTag) && parser.getAddComments()) {
               field.setRawCommentText(parser.getLastComment());
            }
            parser.ctx.fieldList.add(field);
            if (field.isIncluded()) {
               parser.ctx.filteredFieldList.add(field);
            }
            if (fieldHasSerialTag) {
               parser.ctx.sfieldList.add(field);
            }
         }

         parser.setLastComment(null);
         return endIndex;
      }


   }

   class FunctionComponent extends BlockSourceComponent {

      int getEndIndex(char[] source, int endIndex) throws ParseException {
         if (source[endIndex-1]==';') {
            return endIndex;
         }
         else {
            return super.getEndIndex(source, endIndex);
         }
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) throws IOException, ParseException {

         //ctx.fieldList.add(FieldDocImpl.createFromSource(source, startIndex, endIndex));

         //System.out.println("function match '"+new String(source,startIndex,endIndex-startIndex)+"'");
         ExecutableMemberDocImpl execDoc=MethodDocImpl.createFromSource(parser.ctx.classDoc,
                                                                        parser.ctx.classDoc.containingPackage(),
                                                                        source, startIndex, endIndex);

         if (parser.getAddComments())
            execDoc.setRawCommentText(parser.getLastComment());

         parser.setLastComment(null);

         if (execDoc.isMethod()) {
            MethodDoc methDoc = (MethodDoc) execDoc;
            parser.ctx.methodList.add(methDoc);
            if (methDoc.isIncluded()) {
               parser.ctx.filteredMethodList.add(methDoc);
            }
            if (methDoc.name().equals("readObject")
                      || methDoc.name().equals("writeObject")
                      || methDoc.name().equals("readExternal")
                      || methDoc.name().equals("writeExternal")
                      || methDoc.name().equals("readResolve")) {
           // FIXME: add readExternal here?

            parser.ctx.maybeSerMethodList.add(methDoc);
           }
         } else {
             ConstructorDoc constDoc = (ConstructorDoc) execDoc;
             parser.ctx.constructorList.add(constDoc);
             if (constDoc.isIncluded()) {
               parser.ctx.filteredConstructorList.add(constDoc);
             }
         }

         return endIndex;
      }

      int match(char[] source, int index) {
         boolean isFunc=false;
         final int STATE_NORMAL=1;
         final int STATE_SLASHC=2;
         final int STATE_STARC=3;
         int state=STATE_NORMAL;
         for (; index<source.length && !isFunc; ++index) {
            if (state==STATE_STARC) {
               if (source[index]=='*' && source[index+1]=='/') {
                  ++index;
                  state=STATE_NORMAL;
               }
            }
            else if (state==STATE_SLASHC) {
               if (source[index]=='\n') {
                  state=STATE_NORMAL;
               }
            }
            else switch (source[index]) {
            case '/':
               if (source[index+1]=='*') {
                  state=STATE_STARC;
                  ++index;
               }
               else if (source[index+1]=='/') {
                  state=STATE_SLASHC;
                  ++index;
               }
               break;
            case '=':  // field
            case ';':  // field
            case '{':  // class
               return -1;
            case '(':
               isFunc=true;
               break;
            }
            if (isFunc) break;
         }
         if (!isFunc || index==source.length)
            return -1;

         for (; index<source.length && (state!=STATE_NORMAL || (source[index]!='{' && source[index]!=';')); ++index)
            if (state==STATE_SLASHC && source[index]=='\n') {
               state=STATE_NORMAL;
            }
            else if (index<source.length-1) {
               if (state==STATE_STARC) {
                  if (source[index]=='*' && source[index+1]=='/') {
                     state=STATE_NORMAL;
                  }
               }
               else {
                  if (source[index]=='/' && source[index+1]=='*') {
                     state=STATE_STARC;
                  }
                  else if (source[index]=='/' && source[index+1]=='/') {
                     state=STATE_SLASHC;
                  }
               }
            }
         return index+1;
      }


   }

   class StaticBlockComponent extends BlockSourceComponent {

      int process(Parser parser, char[] source, int startIndex, int endIndex) {
         //Debug.log(9,"found package statement: \""+str+"\"");
         //Debug.log(9,"found function component: '"+str+"'");
         parser.setLastComment(null);
         return endIndex;
      }

      int match(char[] source, int index) {
         if (source[index]=='{') return index+1;

         if (index+7<source.length) {
            if (source[index+0]=='s'
                && source[index+1]=='t'
                && source[index+2]=='a'
                && source[index+3]=='t'
                && source[index+4]=='i'
                && source[index+5]=='c') {

               for (index+=6; index<source.length && Parser.WHITESPACE.indexOf(source[index])>=0; ++index)
                  ;

               if (index<source.length && source[index]=='{')
                  return index+1;
               else
                  return -1;
            }
         }
         return -1;
      }

   }

   class ClassComponent extends SourceComponent {

      int match(char[] source, int index) {
         boolean isClass=false;
         for (; index<source.length && !isClass; ++index) {
            switch (source[index]) {
            case '/':  // possible comment
               if (index<source.length-1) {
                  char c = source[index+1];
                  if ('/' == c) {
                     index += 2;
                     while (index<source.length && source[index]!=10) {
                        ++ index;
                     }
                  }
                  else if ('*' == c) {
                     index += 3;
                     while (index<source.length && (source[index-1] != '*' || source[index]!='/')) {
                        ++ index;
                     }
                  }
               }
               break;
            case '@':  // annotation
               index += 1;
               while(index<source.length &&  Character.isJavaIdentifierPart(source[index])) {
                   ++ index;
               }
               if (index<source.length && source[index]=='(') {
                   int parLevel = 1;
                   index += 1;
                   while (index<source.length && parLevel>0) {
                       if (source[index] == '(')
                          ++ parLevel;
                       if (source[index] == ')')
                          -- parLevel;
                       ++ index;
                       if (parLevel==0)
                           break;
                   }
               }
               break;
            case '=':  // field
            case ';':  // field
            case '(':  // function
               return -1;
            case '{':
               isClass=true;
               break;
            }
            if (isClass) break;
         }
         if (!isClass || index>=source.length)
            return -1;

         return index+1;
      }

      int process(Parser parser, char[] source, int startIndex, int endIndex) throws ParseException, IOException {

         parser.classOpened(source, startIndex, endIndex);
         if (parser.getAddComments())
            parser.ctx.classDoc.setRawCommentText(parser.getLastComment());
         parser.setLastComment(null);
         if (parser.ctx.classDoc.isEnum())
           {
             int depth = 0;
             for (int a = endIndex; a < source.length; ++a)
             {
               Debug.log(9, "Enum skipping " + a);
               if (source[a] == '{')
                 {
                   Debug.log(1, "Found inner { in enum");
                   ++depth;
                 }
               if (source[a] == '}')
                 {
                   if (depth > 0)
                     {
                       Debug.log(1, "Found inner } in enum");
                       --depth;
                     }
                   else
                     {
                       Debug.log(1, "Found enum }");
                       parser.classClosed();
                       return a + 1;
                     }
                 }
             }
           }
         int rc=parser.parse(source, endIndex, parser.getClassLevelComponents());
         return rc;
      }

   }


public class Parser {


   static int skipExpression(char[] source, int endIndex, int level, char delimiter) throws ParseException {

      int orgEndIndex=endIndex;

      final int STATE_NORMAL=1;
      final int STATE_STARC=2;
      final int STATE_SLASHC=3;
      final int STATE_CHAR=4;
      final int STATE_STRING=5;

      int state=STATE_NORMAL;
      int prev=0;
      for (; !((level==0 && state==STATE_NORMAL && (delimiter=='\0' || source[endIndex]==delimiter))) && endIndex<source.length; ++endIndex) {
         int c=source[endIndex];
         if (state==STATE_NORMAL) {
            if (c=='}') --level;
            else if (c=='{') ++level;
            else if (c=='/' && prev=='/') { state=STATE_SLASHC; c=0; }
            else if (c=='*' && prev=='/') { state=STATE_STARC; c=0; }
            else if (c=='\'' && prev!='\\') { state=STATE_CHAR; c=0; }
            else if (c=='\"' && prev!='\\') { state=STATE_STRING; c=0; }
         }
         else if (state==STATE_SLASHC) {
            if (c=='\n') state=STATE_NORMAL;
         }
         else if (state==STATE_CHAR) {
            if (c=='\'' && prev!='\\') state=STATE_NORMAL;
            else if (c=='\\' && prev=='\\') c=0;
         }
         else if (state==STATE_STRING) {
            if (c=='\"' && prev!='\\') state=STATE_NORMAL;
            else if (c=='\\' && prev=='\\') c=0;
         }
         else {
            if (c=='/' && prev=='*') { state=STATE_NORMAL; c=0; }
         }
         prev=c;
      }
      if (level>0)
         throw new ParseException("Unexpected end of source.");
      else {
         String rc=new String(source, orgEndIndex, endIndex-orgEndIndex);
         return endIndex;
      }
   }

   private boolean addComments = false;

   public boolean getAddComments()
   {
      return this.addComments;
   }

   public static final String WHITESPACE=" \t\r\n";

   public static final boolean isWhitespace(char c) {
      return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '');
      //return WHITESPACE.indexOf(c)>=0;
   }

   private int currentLine;

   static char[] loadFile(final File file, String encoding)
      throws IOException
   {
      InputStream in = new FileInputStream(file);
      NotifyingInputStreamReader notifyingInput
         = new NotifyingInputStreamReader(in, encoding);
      notifyingInput.addMalformedInputListener(new MalformedInputListener() {
            public void malformedInputEncountered(MalformedInputEvent event) {
               Main.getRootDoc().printWarning("Illegal character in file " + file + ", line " + event.getLineNumber() + ", column " + event.getColumnNumber());
               try {
                  Main.getRootDoc().printWarning(IOToolkit.getLineFromFile(file, event.getLineNumber()));
                  Main.getRootDoc().printWarning(IOToolkit.getColumnDisplayLine(event.getColumnNumber()));
               }
               catch (IOException ignore) {
               }
            }
         });
      Reader reader
         = new BufferedReader(notifyingInput);
      char[] result = IOToolkit.readFully(reader);
      reader.close();
      return result;
   }

   private SourceComponent[] sourceLevelComponents;
   private SourceComponent[] classLevelComponents;

   public SourceComponent[] getClassLevelComponents()
   {
      return this.classLevelComponents;
   }

   public Parser() {
      try {

         sourceLevelComponents=new SourceComponent[] {
            new Whitespace(),
            new CommentComponent(),
            new SlashSlashCommentComponent(),
            new PackageComponent(),
            new EmptyStatementComponent(),
            new ImportComponent(),
            new ClassComponent(),
         };

         classLevelComponents=new SourceComponent[] {
            new Whitespace(),
            new BracketClose(),
            new CommentComponent(),
            new SlashSlashCommentComponent(),
            new FunctionComponent(),
            new StaticBlockComponent(),
            new ImportComponent(),
            new ClassComponent(),
            new FieldComponent(),
         };
      }
      catch (Exception e) {
         e.printStackTrace();
      }
   }

   public int getNumberOfProcessedFiles() {
      return processedFiles.size();
   }

   static Set<File> processedFiles = new HashSet<File>();

   ClassDocImpl processSourceFile(File file, boolean addComments,
                                  String encoding, String expectedPackageName)
      throws IOException, ParseException
   {
     //System.err.println("Processing " + file + "...");
      this.currentFile = file;
      this.currentPackage = null;
      this.currentPackageName = null;
      this.expectedPackageName = expectedPackageName;
      this.outerClass = null;
      this.boilerplateComment = null;

      this.addComments=addComments;

      if (processedFiles.contains(file)) {
         return null;
      }

      processedFiles.add(file);

      Debug.log(1,"Processing file "+file);

      contextStack.clear();
      ctx=null;

      importedClassesList.clear();
      importedStringList.clear();
      importedPackagesList.clear();
      importedStatementList.clear();

      currentLine = 1;

      char[] source = loadFile(file, encoding);

      try {
         parse(source, 0, sourceLevelComponents);

         ClassDoc[] importedClasses = importedClassesList.toArray(new ClassDoc[importedClassesList.size()]);
         PackageDoc[] importedPackages = importedPackagesList.toArray(new PackageDoc[importedPackagesList.size()]);

         if (Main.DESCEND_IMPORTED) {
            for (int i=0; i<importedClasses.length; ++i) {
               Main.getRootDoc().scheduleClass(currentClass, importedClasses[i].qualifiedName());
            }
         }


           if (contextStack.size()>0) {
             Debug.log(1,"-->contextStack not empty! size is "+contextStack.size());
           }

         return outerClass;
      }
      catch (IgnoredFileParseException ignore) {
        Debug.log(1, "File ignored: " + ignore);
         return null;
      }
   }

   int parse(char[] source, int index, SourceComponent[] componentTypes) throws ParseException, IOException {

      while (index<source.length) {

         int match=-1;
         int i=0;
         for (; i<componentTypes.length; ++i) {
            if ((match=componentTypes[i].match(source, index))>=0) {
              //Debug.log(1,componentTypes[i].getClass().getName()+" ("+match+"/"+source.length+")");
               break;
            }
         }

         if (i<componentTypes.length) {
            int endIndex=componentTypes[i].getEndIndex(source, match);
            Debug.log(9, "Processing " + new String(source,index,endIndex-index) + " with " + componentTypes[i]);
            index=componentTypes[i].process(this, source, index, endIndex);
            if (index<0) {
              //Debug.log(9,"exiting parse because of "+componentTypes[i].getClass().getName()+" (\""+new String(source, index, endIndex-index)+"\")");
               return endIndex;
            }
         }
         else {
           //Debug.log(9,"index="+index+", source.length()="+source.length);
            throw new ParseException("unmatched input in line "+currentLine+": "+new String(source, index, Math.min(50,source.length-index)));
         }

      }
      //Debug.log(9,"exiting parse normally, index="+index+" source.length="+source.length);
      return index;
   }

   private static int countNewLines(String source) {
      int i=0;
      int rc=0;
      while ((i=source.indexOf('\n',i)+1)>0)
         ++rc;
      return rc;
   }

   public void processSourceDir(File dir, String encoding, String expectedPackageName)
      throws IOException, ParseException
   {
      Debug.log(9,"Processing "+dir.getParentFile().getName()+"."+dir.getName());
      File[] files=dir.listFiles();
      if (null!=files) {
         for (int i=0; i<files.length; ++i) {
            if (files[i].getName().toLowerCase().endsWith(".java")) {
               processSourceFile(files[i], true, encoding, expectedPackageName);
            }
         }
      }
   }

   void classOpened(char[] source, int startIndex, int endIndex) throws ParseException, IOException {

      referencedClassesList.clear();

      if (null == currentPackage) {

         if (expectedPackageName != null) {
            if (null == currentPackageName ||
                !currentPackageName.equals(expectedPackageName)) {

               Main.getRootDoc().printWarning("Ignoring file " + currentFile + ": (wrong package, " + currentPackageName + "!=" + expectedPackageName + ")");
               throw new IgnoredFileParseException();
            }
         }

         if (null != currentPackageName) {
            currentPackage = Main.getRootDoc().findOrCreatePackageDoc(currentPackageName);
         }
         else {
            currentPackage = Main.getRootDoc().findOrCreatePackageDoc("");
         }
      }

      if (currentPackageName != null)
         importedStatementList.add(currentPackageName + ".*");
      importedStatementList.add("java.lang.*");

      ClassDocImpl classDoc
         = ClassDocImpl.createInstance((ctx!=null)?(ctx.classDoc):null, currentPackage,
                                       null,
                                       importedPackagesList.toArray(new PackageDoc[importedPackagesList.size()]),
                                       source, startIndex, endIndex,
                                       importedStatementList);

      if (ctx != null) {
         ctx.innerClassesList.add(classDoc);
         if (classDoc.isIncluded()) {
            ctx.filteredInnerClassesList.add(classDoc);
         }
      }

      if (importedClassesList.isEmpty()) {
         for (Iterator<String> it=importedStringList.iterator(); it.hasNext(); ) {
            importedClassesList.add(new ClassDocProxy(it.next(), classDoc));
         }
      }
      classDoc.setImportedClasses(importedClassesList.toArray(new ClassDoc[importedClassesList.size()]));

      currentPackage.addClass(classDoc);

      currentClass = classDoc;

      if (null == outerClass) {
         outerClass = classDoc;
      }

      if (classDoc.superclass()!=null)
         referencedClassesList.add(classDoc.superclass());

      Debug.log(1,"classOpened "+classDoc+", adding superclass "+classDoc.superclass());
      Debug.log(1,"Pushing " + ctx);
      contextStack.push(ctx);
      ctx=new Context(classDoc);
      //Debug.log(9,"ctx="+ctx);
   }

   private <T> T[] toArray(List<T> list, T[] template)
   {
      return list.toArray(template);
   }

   void classClosed() throws ParseException, IOException {
      ctx.classDoc.setFields(toArray(ctx.fieldList,new FieldDoc[ctx.fieldList.size()]));
      ctx.classDoc.setFilteredFields(toArray(ctx.filteredFieldList,new FieldDoc[ctx.filteredFieldList.size()]));
      ctx.classDoc.setSerializableFields(toArray(ctx.sfieldList, new FieldDoc[ctx.sfieldList.size()]));
      ctx.classDoc.setMethods(toArray(ctx.methodList, new MethodDoc[ctx.methodList.size()]));
      ctx.classDoc.setFilteredMethods(toArray(ctx.filteredMethodList, new MethodDoc[ctx.filteredMethodList.size()]));
      ctx.classDoc.setMaybeSerMethodList(ctx.maybeSerMethodList);
      ctx.classDoc.setConstructors(toArray(ctx.constructorList, new ConstructorDoc[ctx.constructorList.size()]));
      ctx.classDoc.setFilteredConstructors(toArray(ctx.filteredConstructorList,
                                                   new ConstructorDoc[ctx.filteredConstructorList.size()]));
      ctx.classDoc.setInnerClasses(toArray(ctx.innerClassesList, new ClassDocImpl[ctx.innerClassesList.size()]));
      ctx.classDoc.setFilteredInnerClasses(toArray(ctx.filteredInnerClassesList,
                                                   new ClassDocImpl[ctx.filteredInnerClassesList.size()]));
      ctx.classDoc.setBoilerplateComment(boilerplateComment);

      Main.getRootDoc().addClassDoc(ctx.classDoc);

      if (Main.DESCEND_INTERFACES) {
         for (int i=0; i<ctx.classDoc.interfaces().length; ++i) {
            Main.getRootDoc().scheduleClass(ctx.classDoc, ctx.classDoc.interfaces()[i].qualifiedName());
         }
      }

      Debug.log(1,"classClosed: "+ctx.classDoc);

      ctx=(Context)contextStack.pop();
      Debug.log(1, "Popping " + ctx);
      ClassDoc[] referencedClasses=(ClassDoc[])referencedClassesList.toArray(new ClassDoc[0]);

      if (Main.DESCEND_SUPERCLASS) {
         for (int i=0; i<referencedClasses.length; ++i) {
            Main.getRootDoc().scheduleClass(currentClass, referencedClasses[i].qualifiedName());
         }
      }
   }

   Context      ctx             = null;
   Stack        contextStack    = new Stack();
   class Context {
      Context(ClassDocImpl classDoc) { this.classDoc=classDoc; }
      ClassDocImpl      classDoc                 = null;
      List<FieldDoc> fieldList = new LinkedList<FieldDoc>();
      List<FieldDoc> filteredFieldList = new LinkedList<FieldDoc>();
      List<FieldDoc> sfieldList = new LinkedList<FieldDoc>();
      List<MethodDoc> methodList = new LinkedList<MethodDoc>();
      List<MethodDoc> filteredMethodList = new LinkedList<MethodDoc>();
      List<MethodDoc> maybeSerMethodList = new LinkedList<MethodDoc>();
      List<ConstructorDoc> constructorList = new LinkedList<ConstructorDoc>();
      List<ConstructorDoc> filteredConstructorList = new LinkedList<ConstructorDoc>();
      List<ClassDocImpl> innerClassesList = new LinkedList<ClassDocImpl>();
      List<ClassDocImpl> filteredInnerClassesList = new LinkedList<ClassDocImpl>();
   }

   File currentFile = null;
   String lastComment = null;
   String expectedPackageName = null;
   String currentPackageName = null;
   PackageDocImpl currentPackage = null;
   ClassDocImpl currentClass = null;
   ClassDocImpl outerClass   = null;
   List ordinaryClassesList  = new LinkedList();
   List allClassesList       = new LinkedList();
   List interfacesList       = new LinkedList();

   List<ClassDoc> importedClassesList = new LinkedList<ClassDoc>();
   List<String> importedStringList   = new LinkedList<String>();
   List<PackageDoc> importedPackagesList = new LinkedList<PackageDoc>();
   List<String> importedStatementList = new LinkedList<String>();

   List referencedClassesList = new LinkedList();

   String boilerplateComment = null;

   void packageOpened(String packageName) {
      currentPackageName = packageName;
   }

   void importEncountered(String importString) throws ParseException, IOException {
      //Debug.log(9,"importing '"+importString+"'");

      importedStatementList.add(importString);

      if (importString.endsWith(".*")) {
         importedPackagesList.add(Main.getRootDoc().findOrCreatePackageDoc(importString.substring(0,importString.length()-2)));
      }
      else {
         importedStringList.add(importString);
      }
   }


   void setLastComment(String lastComment) {
      this.lastComment=lastComment;
   }

   String getLastComment() {
      return this.lastComment;
   }

   void setBoilerplateComment(String boilerplateComment)
   {
      this.boilerplateComment = boilerplateComment;
   }

   String getBoilerplateComment()
   {
      return boilerplateComment;
   }

}

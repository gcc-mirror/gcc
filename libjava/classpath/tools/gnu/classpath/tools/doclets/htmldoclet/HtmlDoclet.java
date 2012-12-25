/* gnu.classpath.tools.doclets.htmldoclet.HtmlDoclet
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

package gnu.classpath.tools.doclets.htmldoclet;

import gnu.classpath.tools.IOToolkit;

import gnu.classpath.tools.doclets.AbstractDoclet;
import gnu.classpath.tools.doclets.DocletConfigurationException;
import gnu.classpath.tools.doclets.DocletOption;
import gnu.classpath.tools.doclets.DocletOptionFile;
import gnu.classpath.tools.doclets.DocletOptionFlag;
import gnu.classpath.tools.doclets.DocletOptionString;
import gnu.classpath.tools.doclets.PackageGroup;
import gnu.classpath.tools.doclets.TagletPrinter;
import gnu.classpath.tools.doclets.InlineTagRenderer;

import gnu.classpath.tools.doclets.xmldoclet.HtmlRepairer;

import gnu.classpath.tools.taglets.GnuExtendedTaglet;
import gnu.classpath.tools.taglets.TagletContext;

import gnu.classpath.tools.java2xhtml.Java2xhtml;

import gnu.classpath.tools.StringToolkit;

import com.sun.javadoc.*;
import com.sun.tools.doclets.Taglet;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.net.MalformedURLException;

import java.nio.charset.Charset;

import java.text.DateFormat;
import java.text.MessageFormat;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeSet;

public class HtmlDoclet
   extends AbstractDoclet
   implements InlineTagRenderer
{
   private static String filenameExtension = ".html";

   /**
    *  Contains ExternalDocSet.
    */
   private List<ExternalDocSet> externalDocSets = new LinkedList<ExternalDocSet>();

   /**
    *  Contains String->ExternalDocSet.
    */
   private Map<String,ExternalDocSet> packageNameToDocSet = new HashMap<String, ExternalDocSet>();

   /**
    *  Cache for version string from resource /version.properties
    */
   private String docletVersion;

   /**
    *  For now, do not output a help page.
    */
   private static final boolean outputHelpPage = false;

   /**
    *  Stores the output encoding (either the one specified using
    *  -charset, or the platform default encoding).
    */
   private String outputCharset;

   private void printNavBar(HtmlPage output, String currentPage, ClassDoc currentClass)
   {
         output.beginDiv(CssClass.NAVBAR_TOP);

         boolean overviewLevel
            = ("overview".equals(currentPage)
               || "full-tree".equals(currentPage)
               || "index".equals(currentPage)
               || "split-index".equals(currentPage)
               || "serialized".equals(currentPage)
               || "deprecated".equals(currentPage)
               || "about".equals(currentPage)
               );

         if (!isSinglePackage()) {
            if ("overview".equals(currentPage)) {
               output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               output.print("Overview");
               output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            }
            else {
               output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
               output.beginAnchor(output.getPathToRoot() + "/overview-summary" + filenameExtension);
               output.print("Overview");
               output.endAnchor();
               output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
            }

            output.print(" ");
         }

         if (!overviewLevel || isSinglePackage()) {
            if ("package".equals(currentPage)) {
               output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               output.print("Package");
               output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            }
            else {
               output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
               String packageHref;
               if (isSinglePackage()) {
                  packageHref = output.getPathToRoot() + "/" + getPackageURL(getSinglePackage()) + "package-summary" + filenameExtension;
               }
               else {
                  packageHref = "package-summary" + filenameExtension;
               }
               output.beginAnchor(packageHref);
               output.print("Package");
               output.endAnchor();
               output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
            }
         }
         else {
            output.beginSpan(CssClass.NAVBAR_ITEM_DISABLED);
            output.print("Package");
            output.endSpan(CssClass.NAVBAR_ITEM_DISABLED);
         }

         if (optionUse.getValue() || optionLinkSource.getValue()) {
            output.print(" ");

            if (null != currentClass) {
               if ("class".equals(currentPage)) {
                  output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
                  output.print("Class");
                  output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               }
               else {
                  output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
                  output.beginAnchor(currentClass.name() + filenameExtension);
                  output.print("Class");
                  output.endAnchor();
                  output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
               }
            }
            else {
               output.beginSpan(CssClass.NAVBAR_ITEM_DISABLED);
               output.print("Class");
               output.endSpan(CssClass.NAVBAR_ITEM_DISABLED);
            }

            if (optionUse.getValue()) {
               output.print(" ");

               if (null != currentClass) {
                  if ("uses".equals(currentPage)) {
                     output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
                     output.print("Use");
                     output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
                  }
                  else {
                     output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
                     output.beginAnchor(currentClass.name() + "-uses" + filenameExtension);
                     output.print("Use");
                     output.endAnchor();
                     output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
                  }
               }
               else {
                  output.beginSpan(CssClass.NAVBAR_ITEM_DISABLED);
                  output.print("Use");
                  output.endSpan(CssClass.NAVBAR_ITEM_DISABLED);
               }
            }

            if (optionLinkSource.getValue()) {
               output.print(" ");


               if ("source".equals(currentPage)) {
                  output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
                  output.print("Source");
                  output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               }
               else {

                  if (null != currentClass) {

                     output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
                     String targetClassName = currentClass.name();
                     String targetAnchor = "";
                     if (null != currentClass.containingClass()) {
                        targetClassName = getOuterClassDoc(currentClass).name();
                        targetAnchor = "#line." + currentClass.position().line();
                     }
                     output.beginAnchor(targetClassName + "-source" + filenameExtension + targetAnchor);
                     output.print("Source");
                     output.endAnchor();
                     output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
                  }
                  else {
                     output.beginSpan(CssClass.NAVBAR_ITEM_DISABLED);
                     output.print("Source");
                     output.endSpan(CssClass.NAVBAR_ITEM_DISABLED);
                  }
               }
            }
         }


         if (!optionNoTree.getValue()) {
            output.print(" ");

            if ("full-tree".equals(currentPage)
                || "package-tree".equals(currentPage)) {
               output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               output.print("Tree");
               output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            }
            else {
               output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
               String treeHref;
               if (isSinglePackage() && overviewLevel) {
                  treeHref = getPackageURL(getSinglePackage()) + "tree" + filenameExtension;
               }
               else {
                  treeHref = "tree" + filenameExtension;
               }

               output.beginAnchor(treeHref);
               output.print("Tree");
               output.endAnchor();
               output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
            }
         }

         output.print(" ");

         String indexName;
         if (optionSplitIndex.getValue()) {
            indexName = "alphaindex-1";
         }
         else {
            indexName = "alphaindex";
         }

         if ("index".equals(currentPage) || "split-index".equals(currentPage)) {
            output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            output.print("Index");
            output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
         }
         else {
            output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
            output.beginAnchor(output.getPathToRoot() + "/" + indexName + filenameExtension);
            output.print("Index");
            output.endAnchor();
            output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
         }

         if (!optionNoDeprecatedList.getValue()) {
            output.print(" ");

            if ("deprecated".equals(currentPage)) {
               output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               output.print("Deprecated");
               output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            }
            else {
               output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
               output.beginAnchor(output.getPathToRoot() + "/deprecated" + filenameExtension);
               output.print("Deprecated");
               output.endAnchor();
               output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
            }
         }

         if (outputHelpPage) {
            if (!optionNoHelp.getValue()) {
               output.print(" ");

               if ("help".equals(currentPage)) {
                  output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
                  output.print("Help");
                  output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
               }
               else {
                  output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
                  output.beginAnchor(output.getPathToRoot() + "/help" + filenameExtension);
                  output.print("Help");
                  output.endAnchor();
                  output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
               }
            }
         }

         output.print(" ");

         if ("about".equals(currentPage)) {
            output.beginSpan(CssClass.NAVBAR_ITEM_ACTIVE);
            output.print("About");
            output.endSpan(CssClass.NAVBAR_ITEM_ACTIVE);
         }
         else {
            output.beginSpan(CssClass.NAVBAR_ITEM_ENABLED);
            output.beginAnchor(output.getPathToRoot() + "/about" + filenameExtension);
            output.print("About");
            output.endAnchor();
            output.endSpan(CssClass.NAVBAR_ITEM_ENABLED);
         }

         output.endDiv(CssClass.NAVBAR_TOP);
   }

   private void printNavBarTopRow(HtmlPage output, String currentPage, ClassDoc currentClass)
   {
      output.beginRow();
      output.beginCell(CssClass.NAVBAR_TOP);
      printNavBar(output, currentPage, currentClass);
      output.endCell();
      if (null != optionHeader.getValue()) {
         output.beginCell(CssClass.NAVBAR_TOP_HEADER);
         output.print(replaceDocRoot(output, optionHeader.getValue()));
         output.endCell();
      }
      output.endRow();
   }

   private void printNavBarTopNaviCell(HtmlPage output)
   {
      output.beginCell(CssClass.NAVBAR_TOP_NAVI);
      output.beginAnchor(output.getPathToRoot() + "/index" + filenameExtension, "Show in a frameset", "_top");
      output.print("Frames");
      output.endAnchor();
      output.print(" | ");

      output.beginAnchor(output.getFile().getName(), "Show without frames", "_top");
      output.print("No Frames");
      output.endAnchor();
      output.print(" ");

      output.endCell();
   }

   private void printNavBarTop(HtmlPage output, String currentPage)
   {
      printNavBarTop(output, currentPage, null, null, null);
   }

   private void printNavBarTop(HtmlPage output, String currentPage,
                               ClassDoc currentClass, Object prev, Object next)
   {
      if (!optionNoNavBar.getValue()) {
         output.beginTable(CssClass.NAVBAR_TOP);
         printNavBarTopRow(output, currentPage, currentClass);
         output.beginRow();
         if ("class".equals(currentPage)) {
            output.beginCell(CssClass.NAVBAR_TOP_NAVI);
            ClassDoc prevClass = (ClassDoc)prev;
            ClassDoc nextClass = (ClassDoc)next;
            if (null != prevClass) {
               output.anchor(getClassDocURL(output, prevClass), "Prev Class");
            }
            else {
               output.print("Prev Class");
            }
            output.print(" | ");
            if (null != nextClass) {
               output.anchor(getClassDocURL(output, nextClass), "Next Class");
            }
            else {
               output.print("Next Class");
            }
            output.endCell();
         }
         else if ("split-index".equals(currentPage)) {
            output.beginCell(CssClass.NAVBAR_TOP_NAVI);
            Integer prevLetter = (Integer)prev;
            Integer nextLetter = (Integer)next;
            if (null != prevLetter) {
               output.anchor("alphaindex-" + prevLetter + filenameExtension, "Prev Letter");
            }
            else {
               output.print("Prev Letter");
            }
            output.print(" | ");
            if (null != nextLetter) {
               output.anchor("alphaindex-" + nextLetter + filenameExtension, "Next Letter");
            }
            else {
               output.print("Next Letter");
            }
            output.endCell();
         }
         else {
            output.beginCell(CssClass.NAVBAR_TOP_NAVI);
            output.endCell();
         }

         printNavBarTopNaviCell(output);
         output.endRow();

         if ("class".equals(currentPage)) {
            output.beginRow();

            output.beginCell(CssClass.NAVBAR_TOP_NAVI);
            output.print("Summary: ");

            if (currentClass.innerClasses().length > 0) {
               output.anchor("#summary-inner", "Nested");
            }
            else {
               output.print("Nested");
            }

            output.print(" | ");

            if (currentClass.fields().length > 0) {
               output.anchor("#summary-fields", "Field");
            }
            else {
               output.print("Field");
            }

            output.print(" | ");

            if (currentClass.methods().length > 0) {
               output.anchor("#summary-methods", "Method");
            }
            else {
               output.print("Method");
            }

            output.print(" | ");

            if (currentClass.constructors().length > 0) {
               output.anchor("#summary-constructors", "Constr");
            }
            else {
               output.print("Constr");
            }

            output.endCell();

            output.beginCell(CssClass.NAVBAR_TOP_NAVI);
            output.print("Detail: ");

            if (currentClass.innerClasses().length > 0) {
               output.anchor("#detail-inner", "Nested");
            }
            else {
               output.print("Nested");
            }

            output.print(" | ");

            if (currentClass.fields().length > 0) {
               output.anchor("#detail-fields", "Field");
            }
            else {
               output.print("Field");
            }

            output.print(" | ");

            if (currentClass.methods().length > 0) {
               output.anchor("#detail-methods", "Method");
            }
            else {
               output.print("Method");
            }

            output.print(" | ");

            if (currentClass.constructors().length > 0) {
               output.anchor("#detail-constructors", "Constr");
            }
            else {
               output.print("Constr");
            }

            output.endCell();
            output.endRow();
         }
         output.endTable();
      }
   }

   private void printNavBarTopPackage(HtmlPage output, String currentPage,
                                      PackageDoc prevPackage, PackageDoc nextPackage)
   {
      if (!optionNoNavBar.getValue()) {
         output.beginTable(CssClass.NAVBAR_TOP);
         printNavBarTopRow(output, currentPage, null);

         output.beginRow();
         output.beginCell(CssClass.NAVBAR_TOP_NAVI);
         if (null != prevPackage) {
            output.anchor(output.getPathToRoot() + "/" + getPackageURL(prevPackage) + "package-summary" + filenameExtension, "Prev Package");
         }
         else {
            output.print("Prev Package");
         }
         output.print(" | ");
         if (null != nextPackage) {
            output.anchor(output.getPathToRoot() + "/" + getPackageURL(nextPackage) + "package-summary" + filenameExtension, "Next Package");
         }
         else {
            output.print("Next Package");
         }
         output.endCell();

         printNavBarTopNaviCell(output);
         output.endRow();

         output.endTable();
      }
   }

   private void printNavBarBottom(HtmlPage output, String currentPage)
   {
      printNavBarBottom(output, currentPage, null);
   }

   private void printNavBarBottom(HtmlPage output, String currentPage, ClassDoc currentClass)
   {
      if ("class".equals(currentPage)) {
         String boilerplate = null;
         Tag[] boilerplateTags = getOuterClassDoc(currentClass).tags("@boilerplate");
         if (boilerplateTags.length > 0) {
            boilerplate = boilerplateTags[0].text();
         }
         if (null != boilerplate) {
            output.hr();
            output.beginDiv(CssClass.CLASS_BOILERPLATE);
            output.print(boilerplate);
            output.endDiv(CssClass.CLASS_BOILERPLATE);
            output.hr();
         }
      }

      if (!optionNoNavBar.getValue()) {
         output.beginDiv(CssClass.NAVBAR_BOTTOM_SPACER);
         output.print(" ");
         output.endDiv(CssClass.NAVBAR_BOTTOM_SPACER);
         output.beginTable(CssClass.NAVBAR_BOTTOM);
         output.beginRow();
         output.beginCell();
         printNavBar(output, currentPage, currentClass);
         output.endCell();
         if (null != optionFooter.getValue()) {
            output.beginCell();
            output.print(replaceDocRoot(output, optionFooter.getValue()));
            output.endCell();
         }
         output.endRow();
         output.endTable();
      }

      if (null != optionBottom.getValue()) {
         output.hr();
         output.print(replaceDocRoot(output, optionBottom.getValue()));
      }
   }

   private void printPackagePageClasses(HtmlPage output, ClassDoc[] classDocs, String header)
   {
      if (classDocs.length > 0) {
         output.beginDiv(CssClass.TABLE_CONTAINER);
         output.beginTable(CssClass.PACKAGE_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
         output.rowDiv(CssClass.TABLE_HEADER, header);

         for (int i=0; i<classDocs.length; ++i) {
            ClassDoc classDoc = classDocs[i];
            if (classDoc.isIncluded()) {
               output.beginRow();

               output.beginCell(CssClass.PACKAGE_SUMMARY_LEFT);
               printType(output, classDoc);
               output.endCell();

               output.beginCell(CssClass.PACKAGE_SUMMARY_RIGHT);
               printTags(output, classDoc, classDoc.firstSentenceTags(), true);
               output.endCell();
               output.endRow();
            }
         }
         output.endTable();
         output.endDiv(CssClass.TABLE_CONTAINER);
         output.print("\n");
      }
   }

   private void printPackagesListFile()
      throws IOException
   {
      PrintWriter out
         = new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(getTargetDirectory(),
                                                                                "package-list")),
                                                  "UTF-8"));

      PackageDoc[] packages = getRootDoc().specifiedPackages();
      for (int i=0; i<packages.length; ++i) {
         String packageName = packages[i].name();
         if (packageName.length() > 0) {
            out.println(packageName);
         }
      }

      out.close();
   }

   private void printPackagePage(File packageDir, String pathToRoot,
                                 PackageDoc packageDoc,
                                 PackageDoc prevPackageDoc,
                                 PackageDoc nextPackageDoc)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir, "package-summary" + filenameExtension),
                                    pathToRoot);

      Set<String> keywords = new LinkedHashSet<String>();
      keywords.add(packageDoc.name() + " packages");

      output.beginPage(getPageTitle(packageDoc.name()), getOutputCharset(),
                       keywords, getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_PACKAGE);
      printNavBarTopPackage(output, "package", prevPackageDoc, nextPackageDoc);

      output.beginDiv(CssClass.PACKAGE_TITLE);
      output.print("Package ");
      if (packageDoc.name().length() > 0) {
         output.print(packageDoc.name());
      }
      else {
         output.print("&lt;Unnamed&gt;");
      }
      output.endDiv(CssClass.PACKAGE_TITLE);

      output.beginDiv(CssClass.PACKAGE_DESCRIPTION_TOP);
      printTags(output, packageDoc, packageDoc.firstSentenceTags(), true);
      output.endDiv(CssClass.PACKAGE_DESCRIPTION_TOP);

      printPackagePageClasses(output, packageDoc.interfaces(),
                              "Interface Summary");
      printPackagePageClasses(output, packageDoc.ordinaryClasses(),
                              "Class Summary");
      printPackagePageClasses(output, packageDoc.exceptions(),
                              "Exception Summary");
      printPackagePageClasses(output, packageDoc.errors(),
                              "Error Summary");

      output.anchorName("description");
      output.beginDiv(CssClass.PACKAGE_DESCRIPTION_FULL);
      printTags(output, packageDoc, packageDoc.inlineTags(), false);
      output.endDiv(CssClass.PACKAGE_DESCRIPTION_FULL);

      printNavBarBottom(output, "package");
      output.endBody();
      output.endPage();
      output.close();
   }

   static class TreeNode
      implements Comparable<TreeNode>
   {
      ClassDoc classDoc;
      SortedSet<TreeNode> children = new TreeSet<TreeNode>();

      TreeNode(ClassDoc classDoc) {
         TreeNode.this.classDoc = classDoc;
      }

      public boolean equals(Object other)
      {
         return classDoc.equals(((TreeNode)other).classDoc);
      }

      public int compareTo(TreeNode other)
      {
         return classDoc.compareTo(other.classDoc);
      }

      public int hashCode()
      {
         return classDoc.hashCode();
      }
   }

   private TreeNode addClassTreeNode(Map<String,TreeNode> treeMap, ClassDoc classDoc)
   {
      TreeNode node = treeMap.get(classDoc.qualifiedName());
      if (null == node) {
         node = new TreeNode(classDoc);
         treeMap.put(classDoc.qualifiedName(), node);

         ClassDoc superClassDoc = (ClassDoc)classDoc.superclass();
         if (null != superClassDoc) {
            TreeNode parentNode = addClassTreeNode(treeMap, superClassDoc);
            parentNode.children.add(node);
         }
      }
      return node;
   }

   private TreeNode addInterfaceTreeNode(Map<String,TreeNode> treeMap, ClassDoc classDoc)
   {
      TreeNode node = treeMap.get(classDoc.qualifiedName());
      if (null == node) {
         node = new TreeNode(classDoc);
         treeMap.put(classDoc.qualifiedName(), node);

         ClassDoc[] superInterfaces = classDoc.interfaces();
         if (null != superInterfaces && superInterfaces.length > 0) {
            for (int i=0; i<superInterfaces.length; ++i) {
               TreeNode parentNode = addInterfaceTreeNode(treeMap, superInterfaces[i]);
               parentNode.children.add(node);
            }
         }
         else {
            TreeNode rootNode = treeMap.get("<root>");
            if (null == rootNode) {
               rootNode = new TreeNode(null);
               treeMap.put("<root>", rootNode);
            }
            rootNode.children.add(node);
         }
      }
      return node;
   }

   private void printPackageTreeRec(HtmlPage output, TreeNode node, TreeNode parentNode)
   {
      output.beginElement("li", "class", "node");
      output.beginElement("div");
      if (node.classDoc.isIncluded()) {
         String packageName = node.classDoc.containingPackage().name();
         if (packageName.length() > 0) {
            output.print(packageName);
            output.print(".");
         }
         output.beginSpan(CssClass.TREE_LINK);
         printType(output, node.classDoc);
         output.endSpan(CssClass.TREE_LINK);
      }
      else {
         output.print(possiblyQualifiedName(node.classDoc));
      }
      ClassDoc[] interfaces = node.classDoc.interfaces();
      ClassDoc parentClassDoc = null;
      if (null != parentNode) {
         parentClassDoc = parentNode.classDoc;
      }
      if (interfaces.length > 0
          && !(interfaces.length == 1 && interfaces[0].equals(parentClassDoc))) {
         if (node.classDoc.isInterface()) {
            output.print(" (also implements ");
         }
         else {
            output.print(" (implements ");
         }

         boolean firstItem = true;
         for (int i=0; i<interfaces.length; ++i) {
            ClassDoc implemented = interfaces[i];
            if (!implemented.equals(parentClassDoc)) {
               if (!firstItem) {
                  output.print(", ");
               }
               firstItem = false;
               if (implemented.isIncluded()) {
                  output.print(implemented.containingPackage().name());
                  output.print(".");
                  printType(output, implemented);
               }
               else {
                  output.print(possiblyQualifiedName(implemented));
               }
            }
         }
         output.print(")");
      }

      output.endElement("div");
      output.endElement("li");
      if (!node.children.isEmpty()) {
         output.beginElement("li", "class", "level");
         output.beginElement("ul");
         Iterator<TreeNode> it = node.children.iterator();
         while (it.hasNext()) {
            printPackageTreeRec(output, it.next(), node);
         }
         output.endElement("ul");
         output.endElement("li");
      }
   }

   private void printClassTree(HtmlPage output, ClassDoc[] classDocs)
   {
     Map<String,TreeNode> classTreeMap = new HashMap<String,TreeNode>();

      for (int i=0; i<classDocs.length; ++i) {
         ClassDoc classDoc = classDocs[i];
         if (!classDoc.isInterface()) {
            addClassTreeNode(classTreeMap, classDoc);
         }
      }

      TreeNode root = classTreeMap.get("java.lang.Object");
      if (null != root) {
         output.div(CssClass.PACKAGE_TREE_SECTION_TITLE, "Class Hierarchy");
         output.beginDiv(CssClass.PACKAGE_TREE);
         printPackageTreeRec(output, root, null);
         output.endDiv(CssClass.PACKAGE_TREE);
      }
   }

   private void printInterfaceTree(HtmlPage output, ClassDoc[] classDocs)
   {
      Map<String,TreeNode> interfaceTreeMap = new HashMap<String,TreeNode>();

      for (int i=0; i<classDocs.length; ++i) {
         ClassDoc classDoc = classDocs[i];
         if (classDoc.isInterface()) {
            addInterfaceTreeNode(interfaceTreeMap, classDoc);
         }
      }

      TreeNode interfaceRoot = interfaceTreeMap.get("<root>");
      if (null != interfaceRoot) {
         Iterator<TreeNode> it = interfaceRoot.children.iterator();
         if (it.hasNext()) {
            output.div(CssClass.PACKAGE_TREE_SECTION_TITLE, "Interface Hierarchy");
            output.beginDiv(CssClass.PACKAGE_TREE);
            while (it.hasNext()) {
               TreeNode node = it.next();
               printPackageTreeRec(output, node, null);
            }
            output.endDiv(CssClass.PACKAGE_TREE);
         }
      }

   }

   private void printPackageTreePage(File packageDir, String pathToRoot, PackageDoc packageDoc)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir,
                                             "tree" + filenameExtension),
                                    pathToRoot);
      output.beginPage(getPageTitle(packageDoc.name() + " Hierarchy"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_PACKAGE_TREE);
      printNavBarTop(output, "package-tree");

      output.div(CssClass.PACKAGE_TREE_TITLE, "Hierarchy for Package " + packageDoc.name());

      ClassDoc[] classDocs = packageDoc.allClasses();
      printClassTree(output, classDocs);
      printInterfaceTree(output, classDocs);

      printNavBarBottom(output, "package-tree");
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printFullTreePage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "tree" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Hierarchy"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_FULL_TREE);
      printNavBarTop(output, "full-tree");

      output.div(CssClass.PACKAGE_TREE_TITLE, "Hierarchy for All Packages");

      output.beginDiv(CssClass.FULL_TREE_PACKAGELIST);
      output.div(CssClass.FULL_TREE_PACKAGELIST_HEADER, "Package Hierarchies:");
      output.beginDiv(CssClass.FULL_TREE_PACKAGELIST_ITEM);
      Set<PackageDoc> allPackages = getAllPackages();
      Iterator<PackageDoc> it = allPackages.iterator();
      while (it.hasNext()) {
         PackageDoc packageDoc = it.next();
         output.beginAnchor(getPackageURL(packageDoc) + "tree" + filenameExtension);
         output.print(packageDoc.name());
         output.endAnchor();
         if (it.hasNext()) {
            output.print(", ");
         }
      }
      output.endDiv(CssClass.FULL_TREE_PACKAGELIST_ITEM);
      output.endDiv(CssClass.FULL_TREE_PACKAGELIST);

      ClassDoc[] classDocs = getRootDoc().classes();
      printClassTree(output, classDocs);
      printInterfaceTree(output, classDocs);

      printNavBarBottom(output, "full-tree");
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printIndexEntry(HtmlPage output, Doc entry)
   {
      output.beginDiv(CssClass.INDEX_ENTRY);
      output.beginDiv(CssClass.INDEX_ENTRY_KEY);
      if (entry instanceof PackageDoc) {
         output.beginAnchor(getPackageURL((PackageDoc)entry) + "package-summary" + filenameExtension);
         output.print(entry.name());
         output.endAnchor();
         output.print(" - package");
      }
      else if (entry instanceof ClassDoc) {
         ClassDoc classDoc = (ClassDoc)entry;
         output.beginAnchor(getClassURL(classDoc));
         output.print(entry.name() + getTypeParameters(classDoc));
         output.endAnchor();
         output.print(" - ");
         if (entry.isInterface()) {
            output.print("interface ");
         }
         else if (entry.isException()) {
            output.print("exception ");
         }
         else if (entry.isError()) {
            output.print("error ");
         }
         else {
            output.print("class ");
         }
         String packageName = classDoc.containingPackage().name();
         if (packageName.length() > 0) {
            output.print(packageName);
            output.print(".");
         }
         printType(output, classDoc);
      }
      else {
         ProgramElementDoc memberDoc = (ProgramElementDoc)entry;
         output.beginAnchor(getMemberDocURL(output, memberDoc));
         output.print(entry.name());
         if (memberDoc instanceof ExecutableMemberDoc) {
            output.print(((ExecutableMemberDoc)memberDoc).signature());
         }
         output.endAnchor();
         output.print(" - ");

         if (memberDoc.isStatic()) {
            output.print("static ");
         }

         if (entry.isConstructor()) {
            output.print("constructor for class ");
         }
         else if (entry.isMethod()) {
            output.print("method in class ");
         }
         else if (entry.isField()) {
            output.print("field in class ");
         }
         ClassDoc containingClass = memberDoc.containingClass();
         String packageName = containingClass.containingPackage().name();
         if (packageName.length() > 0) {
            output.print(packageName);
            output.print(".");
         }
         printType(output, containingClass);
      }
      output.endDiv(CssClass.INDEX_ENTRY_KEY);
      output.beginDiv(CssClass.INDEX_ENTRY_DESCRIPTION);
      printTags(output, entry, entry.firstSentenceTags(), true);
      output.endDiv(CssClass.INDEX_ENTRY_DESCRIPTION);
      output.endDiv(CssClass.INDEX_ENTRY);
   }

   private void printFrameSetPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "index" + filenameExtension),
                                    ".",
                                    HtmlPage.DOCTYPE_FRAMESET);

      String title = getWindowTitle();
      output.beginPage(title, getOutputCharset(), getStylesheets());
      output.beginElement("frameset", "cols", "20%,80%");

      String contentURL;
      if (isSinglePackage()) {
         output.atomicElement("frame",
                              new String[] { "src", "name" },
                              new String[] { getPackageURL(getSinglePackage()) + "classes" + filenameExtension, "classes" });
         contentURL = getPackageURL(getSinglePackage()) + "package-summary.html";
      }
      else {
         output.beginElement("frameset", "rows", "25%,75%");
         output.atomicElement("frame",
                              new String[] { "src", "name" },
                              new String[] { "all-packages" + filenameExtension, "packages" });
         output.atomicElement("frame",
                              new String[] { "src", "name" },
                              new String[] { "all-classes" + filenameExtension, "classes" });
         output.endElement("frameset");
         contentURL = "overview-summary" + filenameExtension;
      }
      output.atomicElement("frame",
                           new String[] { "src", "name" },
                           new String[] { contentURL, "content" });
      output.endElement("frameset");
      output.endPage();
      output.close();
   }

   private void printPackagesMenuPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "all-packages" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Package Menu"), getOutputCharset(), getStylesheets());
      output.beginBody(CssClass.BODY_MENU_PACKAGES, false);

      output.beginSpan(CssClass.PACKAGE_MENU_ENTRY);
      output.beginAnchor("all-classes" + filenameExtension,
                         null,
                         "classes");
      output.print("All Classes");
      output.endAnchor();
      output.endSpan(CssClass.PACKAGE_MENU_ENTRY);

      output.div(CssClass.PACKAGE_MENU_TITLE, "Packages");

      output.beginDiv(CssClass.PACKAGE_MENU_LIST);

      Set<PackageDoc> packageDocs = getAllPackages();
      Iterator<PackageDoc> it = packageDocs.iterator();
      while (it.hasNext()) {
         PackageDoc packageDoc = it.next();
         output.beginSpan(CssClass.PACKAGE_MENU_ENTRY);
         output.beginAnchor(getPackageURL(packageDoc) + "classes" + filenameExtension,
                            null,
                            "classes");
         if (packageDoc.name().length() > 0) {
            output.print(packageDoc.name());
         }
         else {
            output.print("&lt;unnamed package&gt;");
         }
         output.endAnchor();
         output.endSpan(CssClass.PACKAGE_MENU_ENTRY);
         output.br();
      }

      output.endDiv(CssClass.PACKAGE_MENU_LIST);
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printClassMenuEntry(HtmlPage output, ClassDoc classDoc)
   {
      CssClass entryClass;
      if (classDoc.isInterface()) {
         entryClass = CssClass.CLASS_MENU_ENTRY_INTERFACE;
      }
      else {
         entryClass = CssClass.CLASS_MENU_ENTRY_CLASS;
      }
      output.beginSpan(entryClass);
      output.beginAnchor(getClassDocURL(output, classDoc),
                         classDoc.qualifiedTypeName(),
                         "content");
      output.print(classDoc.name());
      output.endAnchor();
      output.endSpan(entryClass);
      output.br();
   }

   private void printClassMenuSection(HtmlPage output, Collection classDocs, String header)
   {
      if (!classDocs.isEmpty()) {
         output.div(CssClass.CLASS_MENU_SUBTITLE, header);
         Iterator<ClassDoc> it = classDocs.iterator();
         while (it.hasNext()) {
            ClassDoc classDoc = it.next();
            printClassMenuEntry(output, classDoc);
         }
      }
   }

   private void printClassMenuList(HtmlPage output, ClassDoc[] classDocs, boolean categorized)
   {
      output.beginDiv(CssClass.CLASS_MENU_LIST);

      if (categorized) {
         Set<ClassDoc> classes = new TreeSet<ClassDoc>();
         Set<ClassDoc> interfaces = new TreeSet<ClassDoc>();
         Set<ClassDoc> exceptions = new TreeSet<ClassDoc>();
         Set<ClassDoc> errors = new TreeSet<ClassDoc>();

         for (int i=0; i<classDocs.length; ++i) {
            ClassDoc classDoc = classDocs[i];
            if (classDoc.isInterface()) {
               interfaces.add(classDoc);
            }
            else if (classDoc.isException()) {
               exceptions.add(classDoc);
            }
            else if (classDoc.isError()) {
               errors.add(classDoc);
            }
            else {
               classes.add(classDoc);
            }
         }
         printClassMenuSection(output, interfaces, "Interfaces");
         printClassMenuSection(output, classes, "Classes");
         printClassMenuSection(output, exceptions, "Exceptions");
         printClassMenuSection(output, errors, "Errors");
      }
      else {
         for (int i=0; i<classDocs.length; ++i) {
            ClassDoc classDoc = classDocs[i];
            if (classDoc.isIncluded()) {
               printClassMenuEntry(output, classDoc);
            }
         }
      }

      output.endDiv(CssClass.CLASS_MENU_LIST);
   }

   private void printAllClassesMenuPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "all-classes" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Class Menu"), getOutputCharset(), getStylesheets());
      output.beginBody(CssClass.BODY_MENU_CLASSES, false);

      output.div(CssClass.CLASS_MENU_TITLE, "All Classes");

      printClassMenuList(output, getRootDoc().classes(), false);

      output.endBody();
      output.endPage();
      output.close();
   }

   private void printPackageClassesMenuPage(File packageDir, String pathToRoot, PackageDoc packageDoc)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir,
                                             "classes" + filenameExtension),
                                    pathToRoot);

      output.beginPage(getPageTitle(packageDoc.name() + " Class Menu"),
                       getOutputCharset(), getStylesheets());
      output.beginBody(CssClass.BODY_MENU_CLASSES, false);

      output.beginDiv(CssClass.CLASS_MENU_TITLE);
      output.beginAnchor("package-summary" + filenameExtension, "", "content");
      if (packageDoc.name().length() > 0) {
         output.print(packageDoc.name());
      }
      else {
         output.print("&lt;Unnamed&gt;");
      }
      output.endAnchor();
      output.endDiv(CssClass.CLASS_MENU_TITLE);

      printClassMenuList(output, packageDoc.allClasses(), true);

      output.endBody();
      output.endPage();
      output.close();
   }

   private void printSplitIndex()
      throws IOException
   {
      Map<Character,List<Doc>> categorizedIndex = getCategorizedIndex();
      Iterator<Character> it = categorizedIndex.keySet().iterator();
      int n = 1;
      int count = categorizedIndex.size();
      while (it.hasNext()) {
         Character c = it.next();
         List<Doc> classList = categorizedIndex.get(c);
         printIndexPage(n++, count, c, classList);
      }
   }

   private void printIndexPage()
      throws IOException
   {
      printIndexPage(0, 0, null, null);
   }

   private void printIndexPage(int index, int maxIndex, Character letter, List<Doc> classList)
      throws IOException
   {
      String pageName = "alphaindex";
      if (null != letter) {
         pageName += "-" + index;
      }
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             pageName + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Alphabetical Index"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_INDEX);
      if (null == letter) {
         printNavBarTop(output, "index");
      }
      else {
         printNavBarTop(output, "split-index", null,
                        (index > 1) ? new Integer(index - 1) : null,
                        (index < maxIndex) ? new Integer(index + 1) : null);
      }

      {
         String title;
         if (null == letter) {
            title = "Alphabetical Index";
         }
         else {
            title = "Alphabetical Index: " + letter;
         }
         output.div(CssClass.INDEX_TITLE, title);

         if (null != letter || getCategorizedIndex().keySet().size() > 1) {
            output.beginDiv(CssClass.INDEX_LETTERS);

            Iterator it = getCategorizedIndex().keySet().iterator();
            int n = 1;
            while (it.hasNext()) {
               Character c = (Character)it.next();
               output.beginSpan(CssClass.INDEX_LETTER);
               if (letter != null) {
                  output.beginAnchor("alphaindex-" + n + filenameExtension);
               }
               else {
                  output.beginAnchor("#" + c);
               }
               output.print(c.toString());
               output.endAnchor();
               output.endSpan(CssClass.INDEX_LETTER);
               output.beginSpan(CssClass.INDEX_LETTER_SPACER);
               output.print(" ");
               output.endSpan(CssClass.INDEX_LETTER_SPACER);
               ++n;
            }
         }

         output.endDiv(CssClass.INDEX_LETTERS);
      }

      if (null != letter) {
         printIndexCategory(output, letter, classList);
      }
      else {
         Map<Character,List<Doc>> categorizedIndex = getCategorizedIndex();
         Iterator<Character> categoryIt = categorizedIndex.keySet().iterator();

         while (categoryIt.hasNext()) {
            letter = categoryIt.next();
            classList = categorizedIndex.get(letter);
            output.anchorName(letter.toString());
            printIndexCategory(output, letter, classList);
         }
      }

      printNavBarBottom(output, "index");
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printIndexCategory(HtmlPage output, Character letter, List classList)
   {
      Iterator it = classList.iterator();

      output.div(CssClass.INDEX_CATEGORY_HEADER, letter.toString());
      output.beginDiv(CssClass.INDEX_CATEGORY);
      while (it.hasNext()) {
         Doc entry = (Doc)it.next();
         printIndexEntry(output, entry);
      }
      output.endDiv(CssClass.INDEX_CATEGORY);
   }

   private void printDeprecationSummary(HtmlPage output, List docs, String header)
   {
      if (!docs.isEmpty()) {
         output.beginDiv(CssClass.TABLE_CONTAINER);
         output.beginTable(CssClass.DEPRECATION_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
         output.rowDiv(CssClass.TABLE_HEADER, header);

         Iterator it = docs.iterator();
         while (it.hasNext()) {
            Doc doc = (Doc)it.next();
            output.beginRow();

            output.beginCell(CssClass.DEPRECATION_SUMMARY_LEFT);
            if (doc instanceof Type) {
               printType(output, (Type)doc);
            }
            else {
               ProgramElementDoc memberDoc = (ProgramElementDoc)doc;
               output.beginAnchor(getMemberDocURL(output, memberDoc));
               output.print(memberDoc.containingClass().qualifiedName());
               output.print(".");
               output.print(memberDoc.name());
               if (memberDoc instanceof ExecutableMemberDoc) {
                  output.print(((ExecutableMemberDoc)memberDoc).flatSignature());
               }
               output.endAnchor();
            }
            output.beginDiv(CssClass.DEPRECATION_SUMMARY_DESCRIPTION);
            printTags(output, doc, doc.tags("deprecated")[0].firstSentenceTags(), true);
            output.endDiv(CssClass.DEPRECATION_SUMMARY_DESCRIPTION);

            output.endCell();

            output.endRow();
         }
         output.endTable();
         output.endDiv(CssClass.TABLE_CONTAINER);
         output.print("\n");
      }
   }


   private void printSerializationPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "serialized-form" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Serialized Form"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_DEPRECATED);
      printNavBarTop(output, "serialized");

      output.div(CssClass.SERIALIZED_TITLE, "Serialized Form");

      Iterator<PackageDoc> it = getAllPackages().iterator();

      while (it.hasNext()) {

         PackageDoc packageDoc = it.next();

         List<ClassDoc> serializableClasses = new LinkedList<ClassDoc>();
         ClassDoc[] classes = packageDoc.allClasses();
         for (int i=0; i<classes.length; ++i) {
            ClassDoc classDoc = classes[i];
            if (classDoc.isSerializable() || classDoc.isExternalizable()) {
               serializableClasses.add(classDoc);
            }
         }

         if (!serializableClasses.isEmpty()) {
            output.div(CssClass.SERIALIZED_PACKAGE_HEADER, "Package " + packageDoc.name());

            Iterator<ClassDoc> cit = serializableClasses.iterator();
            while (cit.hasNext()) {
               ClassDoc classDoc = cit.next();

               output.anchorName(classDoc.qualifiedTypeName());

               output.beginDiv(CssClass.SERIALIZED_CLASS_HEADER);
               output.print("Class ");
               printType(output, classDoc, true);
               output.print(" extends ");
               printType(output, classDoc.superclass());
               output.print(" implements Serializable");
               output.endDiv(CssClass.SERIALIZED_CLASS_HEADER);

               FieldDoc serialVersionUidField = findField(classDoc, "serialVersionUID");
               if (null != serialVersionUidField
                   && serialVersionUidField.isFinal()
                   && serialVersionUidField.isStatic()
                   && serialVersionUidField.type().typeName().equals("long")) {

                  String fieldValue = serialVersionUidField.constantValueExpression();
                  if (null != fieldValue) {
                     output.beginDiv(CssClass.SERIALIZED_SVUID_OUTER);
                     output.span(CssClass.SERIALIZED_SVUID_HEADER, "serialVersionUID: ");
                     output.span(CssClass.SERIALIZED_SVUID_VALUE, fieldValue);
                     output.endDiv(CssClass.SERIALIZED_SVUID_OUTER);
                  }
               }
               printMemberDetails(output,
                                  classDoc.serializationMethods(),
                                  "Serialization Methods",
                                  true, null);
               printMemberDetails(output,
                                  classDoc.serializableFields(),
                                  "Serialized Fields",
                                  true, null);
            }
         }
      }

      printNavBarBottom(output, "serialized");

      output.endBody();
      output.endPage();
      output.close();
   }


   private void printDeprecationPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "deprecated" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Deprecated API"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_DEPRECATED);
      printNavBarTop(output, "deprecated");

      output.div(CssClass.DEPRECATION_TITLE, "Deprecated API");

      List<ClassDoc> deprecatedInterfaces = new LinkedList<ClassDoc>();
      List<ClassDoc> deprecatedExceptions = new LinkedList<ClassDoc>();
      List<ClassDoc> deprecatedErrors = new LinkedList<ClassDoc>();
      List<ClassDoc> deprecatedClasses = new LinkedList<ClassDoc>();
      List<FieldDoc> deprecatedFields = new LinkedList<FieldDoc>();
      List<MethodDoc> deprecatedMethods = new LinkedList<MethodDoc>();
      List<ConstructorDoc> deprecatedConstructors = new LinkedList<ConstructorDoc>();

      ClassDoc[] classDocs = getRootDoc().classes();
      for (int i=0; i<classDocs.length; ++i) {
         ClassDoc classDoc = classDocs[i];
         {
            Tag[] deprecatedTags = classDoc.tags("deprecated");
            if (null != deprecatedTags && deprecatedTags.length > 0) {
               if (classDoc.isInterface()) {
                  deprecatedInterfaces.add(classDoc);
               }
               else if (classDoc.isException()) {
                  deprecatedExceptions.add(classDoc);
               }
               else if (classDoc.isError()) {
                  deprecatedErrors.add(classDoc);
               }
               else {
                  deprecatedClasses.add(classDoc);
               }
            }
         }
         ConstructorDoc[] constructors = classDoc.constructors();
         for (int j=0; j<constructors.length; ++j) {
            Tag[] deprecatedTags = constructors[j].tags("deprecated");
            if (null != deprecatedTags && deprecatedTags.length > 0) {
               deprecatedConstructors.add(constructors[j]);
            }
         }
         MethodDoc[] methods = classDoc.methods();
         for (int j=0; j<methods.length; ++j) {
            Tag[] deprecatedTags = methods[j].tags("deprecated");
            if (null != deprecatedTags && deprecatedTags.length > 0) {
               deprecatedMethods.add(methods[j]);
            }
         }
         FieldDoc[] fields = classDoc.fields();
         for (int j=0; j<fields.length; ++j) {
            Tag[] deprecatedTags = fields[j].tags("deprecated");
            if (null != deprecatedTags && deprecatedTags.length > 0) {
               deprecatedFields.add(fields[j]);
            }
         }
      }

      if (!deprecatedInterfaces.isEmpty()
          || !deprecatedClasses.isEmpty()
          || !deprecatedExceptions.isEmpty()
          || !deprecatedErrors.isEmpty()
          || !deprecatedFields.isEmpty()
          || !deprecatedMethods.isEmpty()
          || !deprecatedConstructors.isEmpty()) {

         output.beginDiv(CssClass.DEPRECATION_TOC);
         output.div(CssClass.DEPRECATION_TOC_HEADER, "Contents");
         output.beginDiv(CssClass.DEPRECATION_TOC_LIST);
         if (!deprecatedInterfaces.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#interfaces", "Deprecated Interfaces");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedClasses.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#classes", "Deprecated Classes");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedExceptions.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#exceptions", "Deprecated Exceptions");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedErrors.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#errors", "Deprecated Errors");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedFields.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#fields", "Deprecated Fields");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedMethods.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#methods", "Deprecated Methods");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         if (!deprecatedConstructors.isEmpty()) {
            output.beginDiv(CssClass.DEPRECATION_TOC_ENTRY);
            output.anchor("#constructors", "Deprecated Constructors");
            output.endDiv(CssClass.DEPRECATION_TOC_ENTRY);
         }
         output.endDiv(CssClass.DEPRECATION_TOC_LIST);
         output.endDiv(CssClass.DEPRECATION_TOC);
         output.beginDiv(CssClass.DEPRECATION_LIST);

         output.anchorName("interfaces");
         printDeprecationSummary(output, deprecatedInterfaces, "Deprecated Interfaces");

         output.anchorName("classes");
         printDeprecationSummary(output, deprecatedClasses, "Deprecated Classes");

         output.anchorName("exceptions");
         printDeprecationSummary(output, deprecatedExceptions, "Deprecated Exceptions");

         output.anchorName("errors");
         printDeprecationSummary(output, deprecatedErrors, "Deprecated Errors");

         output.anchorName("fields");
         printDeprecationSummary(output, deprecatedFields, "Deprecated Fields");

         output.anchorName("methods");
         printDeprecationSummary(output, deprecatedMethods, "Deprecated Methods");

         output.anchorName("constructors");
         printDeprecationSummary(output, deprecatedConstructors, "Deprecated Constructors");

         output.endDiv(CssClass.DEPRECATION_LIST);
      }
      else {
         output.beginDiv(CssClass.DEPRECATION_EMPTY);
         output.print("No deprecated classes or class members in this API.");
         output.endDiv(CssClass.DEPRECATION_EMPTY);

      }

      printNavBarBottom(output, "deprecated");
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printAboutPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "about" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("About"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_ABOUT);

      printNavBarTop(output, "about");

      output.div(CssClass.ABOUT_TITLE, "About");

      output.beginDiv(CssClass.ABOUT_GENERATOR);
      output.print("Generated by ");
      output.print("Gjdoc");
      output.print(" HtmlDoclet ");
      output.print(getDocletVersion());
      output.print(", part of ");
      output.beginAnchor("http://www.gnu.org/software/classpath/cp-tools/", "", "_top");
      output.print("GNU Classpath Tools");
      output.endAnchor();
      output.print(", on ");
      DateFormat format = DateFormat.getDateTimeInstance(DateFormat.LONG,
                                                         DateFormat.LONG,
                                                         Locale.US);
      Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"),
                                          Locale.US);
      format.setCalendar(cal);
      output.print(format.format(new Date()));
      output.print(".");
      output.endDiv(CssClass.ABOUT_GENERATOR);

      printNavBarBottom(output, "about");

      output.endBody();
      output.endPage();
      output.close();
   }

   private void printSourcePage(File packageDir, ClassDoc classDoc, String sourceXhtml)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir,
                                             classDoc.name() + "-source" + filenameExtension),
                                    getPathToRoot(packageDir, getTargetDirectory()));
      output.beginPage(getPageTitle("Source for " + classDoc.qualifiedTypeName()),
                       getOutputCharset(),
                       getStylesheets());

      output.beginBody(CssClass.BODY_CONTENT_SOURCE);

      printNavBarTop(output, "source", classDoc, null, null);

      output.div(CssClass.SOURCE_TITLE, "Source for " + classDoc.qualifiedTypeName());
      output.beginDiv(CssClass.SOURCE);
      output.print(sourceXhtml);
      output.endDiv(CssClass.SOURCE);

      printNavBarBottom(output, "source", classDoc);

      output.endBody();
      output.endPage();

      output.close();
   }

   private void printHelpPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "help" + filenameExtension),
                                    ".");
      output.beginPage(getPageTitle("Help"),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_HELP);

      printNavBarTop(output, "help");

      InputStream helpIn;
      if (null != optionHelpFile.getValue()){
         helpIn = new FileInputStream(optionHelpFile.getValue());
      }
      else {
         helpIn = getClass().getResourceAsStream("/htmldoclet/help.xhtml");
      }
      output.insert(new InputStreamReader(helpIn, "utf-8"));
      helpIn.close();

      printNavBarBottom(output, "help");

      output.endBody();
      output.endPage();
      output.close();
   }

   private void printOverviewPage()
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(getTargetDirectory(),
                                             "overview-summary" + filenameExtension),
                                    ".");
      output.beginPage(getWindowTitle(),
                       getOutputCharset(),
                       getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_OVERVIEW);

      printNavBarTop(output, "overview");

      String overviewHeader;
      if (null != optionDocTitle.getValue()) {
         overviewHeader = optionDocTitle.getValue();
      }
      else if (null != optionTitle.getValue()) {
         overviewHeader = optionTitle.getValue();
      }
      else {
         overviewHeader = null;
      }

      if (null != overviewHeader) {
         output.div(CssClass.OVERVIEW_TITLE, overviewHeader);
      }

      output.beginDiv(CssClass.OVERVIEW_DESCRIPTION_TOP);
      printTags(output, getRootDoc(), getRootDoc().firstSentenceTags(), true);
      output.endDiv(CssClass.OVERVIEW_DESCRIPTION_TOP);

      List packageGroups = getPackageGroups();

      if (packageGroups.isEmpty()) {

         printOverviewPackages(output, getAllPackages(),
                               "All Packages");
      }
      else {
         Set otherPackages = new LinkedHashSet();
         otherPackages.addAll(getAllPackages());

         Iterator it = packageGroups.iterator();
         while (it.hasNext()) {
            PackageGroup packageGroup = (PackageGroup)it.next();
            printOverviewPackages(output,
                                  packageGroup.getPackages(),
                                  packageGroup.getName());
            otherPackages.removeAll(packageGroup.getPackages());
         }

         if (!otherPackages.isEmpty()) {
            printOverviewPackages(output,
                                  otherPackages,
                                  "Other Packages");
         }
      }

      output.anchorName("description");
      output.beginDiv(CssClass.OVERVIEW_DESCRIPTION_FULL);
      printTags(output, getRootDoc(), getRootDoc().inlineTags(), false);
      output.endDiv(CssClass.OVERVIEW_DESCRIPTION_FULL);

      printNavBarBottom(output, "overview");
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printOverviewPackages(HtmlPage output, Collection packageDocs, String header)
   {
      output.beginDiv(CssClass.TABLE_CONTAINER);
      output.beginTable(CssClass.OVERVIEW_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
      output.rowDiv(CssClass.TABLE_HEADER, header);

      Iterator it = packageDocs.iterator();
      while (it.hasNext()) {
         PackageDoc packageDoc = (PackageDoc)it.next();
         output.beginRow();

         output.beginCell(CssClass.OVERVIEW_SUMMARY_LEFT);
         output.beginAnchor(getPackageURL(packageDoc) + "package-summary" + filenameExtension);
         output.print(packageDoc.name());
         output.endAnchor();
         output.endCell();

         output.beginCell(CssClass.OVERVIEW_SUMMARY_RIGHT);
         printTags(output, packageDoc, packageDoc.firstSentenceTags(), true);
         output.endCell();
         output.endRow();
      }
      output.endTable();
      output.endDiv(CssClass.TABLE_CONTAINER);
   }

   private void printClassUsagePage(File packageDir, String pathToRoot, ClassDoc classDoc)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir,
                                             classDoc.name() + "-uses" + filenameExtension),
                                    pathToRoot);
      output.beginPage(getPageTitle(classDoc.name()), getOutputCharset(), getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_USES);
      printNavBarTop(output, "uses", classDoc, null, null);

      output.div(CssClass.USAGE_TITLE,
                 "Uses of " + getClassTypeName(classDoc)
                 + " " + classDoc.qualifiedName());

      Map packageToUsageTypeMap = getUsageOfClass(classDoc);
      if (null != packageToUsageTypeMap && !packageToUsageTypeMap.isEmpty()) {

         Iterator packagesIterator = packageToUsageTypeMap.keySet().iterator();
         while (packagesIterator.hasNext()) {
            PackageDoc packageDoc = (PackageDoc)packagesIterator.next();

            output.div(CssClass.USAGE_PACKAGE_TITLE, "Uses in package " + packageDoc.name());

            Map usageTypeToUsersMap = (Map)packageToUsageTypeMap.get(packageDoc);
            Iterator usageTypeIterator = usageTypeToUsersMap.keySet().iterator();
            while (usageTypeIterator.hasNext()) {
               UsageType usageType = (UsageType)usageTypeIterator.next();

               output.beginTable(CssClass.USAGE_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
               output.rowDiv(CssClass.USAGE_TABLE_HEADER, format("usagetype." + usageType.getId(),
                                                                 classDoc.qualifiedName()));

               Set users = (Set)usageTypeToUsersMap.get(usageType);
               Iterator userIterator = users.iterator();
               while (userIterator.hasNext()) {
                  Doc user = (Doc)userIterator.next();

                  output.beginRow();

                  if (user instanceof ClassDoc) {
                     output.beginCell(CssClass.USAGE_SUMMARY_LEFT);
                     output.print("class");
                     output.endCell();

                     output.beginCell(CssClass.USAGE_SUMMARY_RIGHT);
                     output.beginDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     printType(output, ((ClassDoc)user));
                     output.endDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     output.beginDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     printTags(output, ((ClassDoc)user), ((ClassDoc)user).firstSentenceTags(), true);
                     output.endDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     output.endCell();
                  }
                  else if (user instanceof FieldDoc) {
                     FieldDoc fieldDoc = (FieldDoc)user;

                     output.beginCell(CssClass.USAGE_SUMMARY_LEFT);
                     printType(output, ((FieldDoc)user).type());
                     output.endCell();

                     output.beginCell(CssClass.USAGE_SUMMARY_RIGHT);
                     output.beginDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     printType(output, ((FieldDoc)user).containingClass());
                     output.print(".");
                     output.beginAnchor(getMemberDocURL(output, (FieldDoc)user));
                     output.print(((FieldDoc)user).name());
                     output.endAnchor();
                     output.endDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     output.beginDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     printTags(output, ((FieldDoc)user), ((FieldDoc)user).firstSentenceTags(), true);
                     output.endDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     output.endCell();
                  }
                  else if (user instanceof MethodDoc) {
                     MethodDoc methodDoc = (MethodDoc)user;

                     output.beginCell(CssClass.USAGE_SUMMARY_LEFT);
                     printType(output, ((MethodDoc)user).returnType());
                     output.endCell();

                     output.beginCell(CssClass.USAGE_SUMMARY_RIGHT);
                     output.beginDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     printType(output, ((MethodDoc)user).containingClass());
                     output.print(".");
                     output.beginAnchor(getMemberDocURL(output, (MethodDoc)user));
                     output.print(((MethodDoc)user).name());
                     output.endAnchor();
                     printParameters(output, (ExecutableMemberDoc)user);
                     output.endDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     output.beginDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     printTags(output, ((MethodDoc)user), ((MethodDoc)user).firstSentenceTags(), true);
                     output.endDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     output.endCell();
                  }
                  else if (user instanceof ConstructorDoc) {
                     ConstructorDoc constructorDoc = (ConstructorDoc)user;

                     output.beginCell(CssClass.USAGE_SUMMARY_RIGHT);
                     output.beginDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     printType(output, ((ConstructorDoc)user).containingClass());
                     output.print(".");
                     output.beginAnchor(getMemberDocURL(output, (ConstructorDoc)user));
                     output.print(((ConstructorDoc)user).name());
                     output.endAnchor();
                     printParameters(output, (ExecutableMemberDoc)user);
                     output.endDiv(CssClass.USAGE_SUMMARY_SYNOPSIS);
                     output.beginDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     printTags(output, ((ConstructorDoc)user),
                               ((ConstructorDoc)user).firstSentenceTags(), true);
                     output.endDiv(CssClass.USAGE_SUMMARY_DESCRIPTION);
                     output.endCell();
                  }

                  output.endRow();
               }
               output.endTable();
            }
         }
      }
      else {
         output.div(CssClass.USAGE_EMPTY,
                    getClassTypeName(classDoc)
                    + " " + classDoc.qualifiedName() + " is not used by any class in this documentation set.");
      }
      printNavBarBottom(output, "uses", classDoc);
      output.endBody();
      output.endPage();
      output.close();
   }

   private void printSuperTreeRec(HtmlPage output, ListIterator it, int level)
   {
      if (it.hasPrevious()) {
         ClassDoc cd = (ClassDoc)it.previous();
         output.beginElement("li", new String[] { "class" }, new String[] { "inheritance " + level });
         output.beginElement("code");
         if (it.hasPrevious()) {
            printType(output, cd, true);
         }
         else {
            output.print(cd.qualifiedName() + getTypeParameters(cd));
         }
         output.endElement("code");
         output.endElement("li");

         output.beginElement("li");

         if (it.hasPrevious()) {
            output.beginElement("ul", new String[] { "class" }, new String[] { "inheritance " + (level + 1) });
            printSuperTreeRec(output, it, level + 1);
            output.endElement("ul");
         }

         output.endElement("li");
      }
   }

   private static boolean isSubInterface(ClassDoc classDoc, ClassDoc otherClassDoc)
   {
      ClassDoc[] interfaces = otherClassDoc.interfaces();
      for (int i=0; i<interfaces.length; ++i) {
         if (classDoc == interfaces[i]) {
            return true;
         }
         else if (isSubInterface(classDoc, interfaces[i])) {
            return true;
         }
      }
      return false;
   }

   private void printCommaSeparatedTypes(HtmlPage output,
                                         Collection list,
                                         String header,
                                         CssClass cssClass)
   {
      if (!list.isEmpty()) {
         output.beginDiv(cssClass);
         output.div(CssClass.CLASS_KNOWNIMPLEMENTING_HEADER, header);
         output.beginDiv(CssClass.CLASS_KNOWNIMPLEMENTING_ITEM);
         Iterator it = list.iterator();
         while (it.hasNext()) {
            Type type = (Type)it.next();
            printType(output, type);
            if (it.hasNext()) {
               output.print(", ");
            }
         }
         output.endDiv(CssClass.CLASS_KNOWNIMPLEMENTING_ITEM);
         output.endDiv(cssClass);
      }
   }

   private void printClassPage(File packageDir, String pathToRoot,
                               ClassDoc classDoc, ClassDoc prevClassDoc, ClassDoc nextClassDoc)
      throws IOException
   {
      HtmlPage output = newHtmlPage(new File(packageDir,
                                             classDoc.name() + filenameExtension),
                                    pathToRoot);
      Set keywords = new LinkedHashSet();
      {
         keywords.add(classDoc.qualifiedName() + " class");
         FieldDoc[] fieldDocs = classDoc.fields();
         for (int i=0; i<fieldDocs.length; ++i) {
            FieldDoc fieldDoc = fieldDocs[i];
            keywords.add(fieldDoc.name());
         }

         MethodDoc[] methodDocs = classDoc.methods();
         for (int i=0; i<methodDocs.length; ++i) {
            MethodDoc methodDoc = methodDocs[i];
            keywords.add(methodDoc.name() + "()");
         }
      }
      String parameters = getTypeParameters(classDoc);

      output.beginPage(getPageTitle(classDoc.name()), getOutputCharset(),
                       keywords, getStylesheets());
      output.beginBody(CssClass.BODY_CONTENT_CLASS);
      printNavBarTop(output, "class", classDoc, prevClassDoc, nextClassDoc);

      output.beginDiv(CssClass.CLASS_TITLE);
      output.div(CssClass.CLASS_TITLE_PACKAGE,
                 classDoc.containingPackage().name());
      output.div(CssClass.CLASS_TITLE_CLASS,
                 getClassTypeName(classDoc)
                 + " " + classDoc.name()
                 + parameters);
      output.endDiv(CssClass.CLASS_TITLE);

      boolean needSep = false;

      if (classDoc.isInterface()) {

         InterfaceRelation relation
            = (InterfaceRelation)getInterfaceRelations().get(classDoc);

         printCommaSeparatedTypes(output,
                                  relation.superInterfaces,
                                  "All Superinterfaces:",
                                  CssClass.CLASS_KNOWNIMPLEMENTING);

         printCommaSeparatedTypes(output,
                                  relation.subInterfaces,
                                  "Known Subinterfaces:",
                                  CssClass.CLASS_KNOWNIMPLEMENTING);

         printCommaSeparatedTypes(output,
                                  relation.implementingClasses,
                                  "Known Implementing Classes:",
                                  CssClass.CLASS_KNOWNIMPLEMENTING);

         needSep = !relation.superInterfaces.isEmpty()
            || !relation.subInterfaces.isEmpty()
            || !relation.implementingClasses.isEmpty();
      }
      else {
         needSep = true;

         if (!"java.lang.Object".equals(classDoc.qualifiedName())) {
            LinkedList superClasses = new LinkedList();
            for (ClassDoc cd = classDoc; cd != null; cd = cd.superclass()) {
               superClasses.add(cd);
            }
            output.beginDiv(CssClass.CLASS_INHERITANCETREE);
            output.beginElement("ul", new String[] { "class" }, new String[] { "inheritance 0" });
            printSuperTreeRec(output, superClasses.listIterator(superClasses.size()), 0);
            output.endElement("ul");
            output.endDiv(CssClass.CLASS_INHERITANCETREE);

            if (null != classDoc.containingClass()) {
               output.beginDiv(CssClass.CLASS_ENCLOSINGCLASS);
               output.div(CssClass.CLASS_ENCLOSINGCLASS_HEADER, "Enclosing Class:");
               output.beginDiv(CssClass.CLASS_ENCLOSINGCLASS_ITEM);
               printType(output, classDoc.containingClass());
               output.endDiv(CssClass.CLASS_ENCLOSINGCLASS_ITEM);
               output.endDiv(CssClass.CLASS_ENCLOSINGCLASS);
            }

            Set implementedInterfaces = getImplementedInterfaces(classDoc);

            printCommaSeparatedTypes(output,
                                     implementedInterfaces,
                                     "Implemented Interfaces:",
                                     CssClass.CLASS_KNOWNIMPLEMENTING);

            List knownDirectSubclasses = getKnownDirectSubclasses(classDoc);
            if (!knownDirectSubclasses.isEmpty()) {
               output.beginDiv(CssClass.CLASS_SUBCLASSES);
               output.div(CssClass.CLASS_SUBCLASSES_HEADER, "Known Direct Subclasses:");
               output.beginDiv(CssClass.CLASS_SUBCLASSES_ITEM);
               Iterator it = knownDirectSubclasses.iterator();
               while (it.hasNext()) {
                  printType(output, (ClassDoc)it.next());
                  if (it.hasNext()) {
                     output.print(", ");
                  }
               }

               output.endDiv(CssClass.CLASS_SUBCLASSES_ITEM);
               output.endDiv(CssClass.CLASS_SUBCLASSES_HEADER);
               output.endDiv(CssClass.CLASS_SUBCLASSES);
            }
         }
      }

      if (needSep) {
         output.hr();
      }

      output.beginDiv(CssClass.CLASS_SYNOPSIS);
      output.beginDiv(CssClass.CLASS_SYNOPSIS_DECLARATION);
      output.print(getFullModifiers(classDoc) + ' ' + getClassTypeKeyword(classDoc)
                   + ' ');
      output.beginSpan(CssClass.CLASS_SYNOPSIS_NAME);
      if (optionLinkSource.getValue() && null != classDoc.position()) {
         output.beginAnchor(getOuterClassDoc(classDoc).name() + "-source" + filenameExtension + "#line." + classDoc.position());
         output.print(classDoc.name() + parameters);
         output.endAnchor();
      }
      else {
         output.print(classDoc.name() + parameters);
      }
      output.endSpan(CssClass.CLASS_SYNOPSIS_NAME);
      output.endDiv(CssClass.CLASS_SYNOPSIS_DECLARATION);

      if (!classDoc.isInterface()) {
         if (null != classDoc.superclass()) {
            output.beginDiv(CssClass.CLASS_SYNOPSIS_SUPERCLASS);
            output.print("extends ");
            printType(output, classDoc.superclass());
            output.endDiv(CssClass.CLASS_SYNOPSIS_SUPERCLASS);
         }
      }

      ClassDoc[] interfaces = classDoc.interfaces();
      if (interfaces.length > 0) {
         output.beginDiv(CssClass.CLASS_SYNOPSIS_IMPLEMENTS);
         if (!classDoc.isInterface()) {
            output.print("implements ");
         }
         else {
            output.print("extends ");
         }
         for (int i=0; i<interfaces.length; ++i) {
            if (i>0) {
               output.print(", ");
            }
            printType(output, interfaces[i]);
         }
         output.endDiv(CssClass.CLASS_SYNOPSIS_IMPLEMENTS);
      }
      output.endDiv(CssClass.CLASS_SYNOPSIS);

      output.hr();

      if (!optionNoComment.getValue()) {
         output.beginDiv(CssClass.CLASS_DESCRIPTION);
         printTags(output, classDoc, classDoc.inlineTags(), false);
         output.endDiv(CssClass.CLASS_DESCRIPTION);

         printTaglets(output, classDoc.tags(), new HtmlTagletContext(classDoc, output, false));
      }


      Set implementedInterfaces = getImplementedInterfaces(classDoc);

      boolean haveInheritedFields = false;
      boolean haveInheritedMethods = false;
      boolean haveInheritedClasses = false;
      {
         if (!classDoc.isInterface()) {
            ClassDoc superClassDoc = classDoc.superclass();
            while (null != superClassDoc
                   && (!haveInheritedFields
                       || !haveInheritedMethods
                       || !haveInheritedClasses)) {
               if (superClassDoc.fields().length > 0) {
                  haveInheritedFields = true;
               }
               if (superClassDoc.methods().length > 0) {
                  haveInheritedMethods = true;
               }
               if (superClassDoc.innerClasses().length > 0) {
                  haveInheritedClasses = true;
               }
               superClassDoc = superClassDoc.superclass();
            }
         }
      }

      printProgramElementDocs(output, getSortedInnerClasses(classDoc),
                              "Nested Class Summary", haveInheritedClasses,
                              "summary-inner");

      {
         ClassDoc superClassDoc = classDoc.superclass();
         while (null != superClassDoc) {
            printInheritedMembers(output, getSortedInnerClasses(superClassDoc),
                                  "Nested classes/interfaces inherited from class {0}",
                                  superClassDoc);
            superClassDoc = superClassDoc.superclass();
         }
      }

      printProgramElementDocs(output, getSortedFields(classDoc),
                              "Field Summary", haveInheritedFields,
                              "summary-fields");

      {
         ClassDoc superClassDoc = classDoc.superclass();
         while (null != superClassDoc) {
            printInheritedMembers(output, getSortedFields(superClassDoc),
                                  "Fields inherited from class {0}",
                                  superClassDoc);
            superClassDoc = superClassDoc.superclass();
         }
      }

      {
         Iterator it = implementedInterfaces.iterator();
         while (it.hasNext()) {
            ClassDoc implementedInterface
               = (ClassDoc)it.next();
            if (!"java.io.Serializable".equals(implementedInterface.qualifiedName())
                && !"java.io.Externalizable".equals(implementedInterface.qualifiedName())) {
               printInheritedMembers(output, getSortedFields(implementedInterface),
                                     "Fields inherited from interface {0}",
                                     implementedInterface);
            }
         }
      }

      printProgramElementDocs(output, getSortedConstructors(classDoc),
                              "Constructor Summary", false,
                              "summary-constructors");
      printProgramElementDocs(output, getSortedMethods(classDoc),
                              "Method Summary", haveInheritedMethods,
                              "summary-methods");

      if (classDoc.isInterface()) {
         InterfaceRelation relation
            = (InterfaceRelation)getInterfaceRelations().get(classDoc);
         Iterator it = relation.superInterfaces.iterator();
         while (it.hasNext()) {
            ClassDoc superClassDoc = (ClassDoc)it.next();
            printInheritedMembers(output, getSortedMethods(superClassDoc),
                                  "Methods inherited from interface {0}",
                                  superClassDoc);
         }
      }
      else {
         ClassDoc superClassDoc = classDoc.superclass();
         while (null != superClassDoc) {
            printInheritedMembers(output, getSortedMethods(superClassDoc),
                                  "Methods inherited from class {0}",
                                  superClassDoc);
            superClassDoc = superClassDoc.superclass();
         }
      }

      printMemberDetails(output, getSortedFields(classDoc),
                         "Field Details", false, "detail-fields");
      printMemberDetails(output, getSortedConstructors(classDoc),
                         "Constructor Details", false, "detail-constructors");
      printMemberDetails(output, getSortedMethods(classDoc),
                         "Method Details", false, "detail-methods");

      printNavBarBottom(output, "class", classDoc);

      output.endBody();
      output.endPage();
      output.close();
   }

   private void printInheritedMembers(HtmlPage output,
                                      ProgramElementDoc[] memberDocs,
                                      String headerFormat,
                                      ClassDoc superclass)
   {
      if (memberDocs.length > 0) {

         output.beginDiv(CssClass.TABLE_CONTAINER);
         output.beginTable(CssClass.CLASS_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
         String superclassLink;
         if (superclass.isIncluded()) {
            superclassLink = superclass.containingPackage().name()
               + "." + createTypeHref(output, superclass, false);
         }
         else {
            superclassLink = createTypeHref(output, superclass, true);
         }
         output.rowDiv(CssClass.TABLE_SUB_HEADER,
                       new MessageFormat(headerFormat).format(new Object[] {
                          superclassLink
                       }));

         output.beginRow();
         output.beginCell(CssClass.CLASS_SUMMARY_INHERITED);
         for (int i=0; i<memberDocs.length; ++i) {
            ProgramElementDoc memberDoc = memberDocs[i];
            if (i > 0) {
               output.print(", ");
            }
            String title = null;
            if (memberDoc.isMethod()) {
               title = memberDoc.name() + ((MethodDoc)memberDoc).flatSignature();
            }
            else if (memberDoc.isInterface()) {
               title = "interface " + ((ClassDoc)memberDoc).qualifiedName();
            }
            else if (memberDoc.isClass()) {
               title = "class " + ((ClassDoc)memberDoc).qualifiedName();
            }
            output.beginAnchor(getMemberDocURL(output, memberDoc), title);
            output.beginSpan(CssClass.CLASS_SUMMARY_INHERITED_MEMBER);
            output.print(memberDoc.name());
            output.endSpan(CssClass.CLASS_SUMMARY_INHERITED_MEMBER);
            output.endAnchor();
         }
         output.endCell();
         output.endRow();
         output.endTable();
         output.endDiv(CssClass.TABLE_CONTAINER);
      }
   }

   private void collectSpecifiedByRecursive(Set specifyingInterfaces,
                                            ClassDoc classDoc,
                                            MethodDoc methodDoc)
   {
      ClassDoc[] interfaces = classDoc.interfaces();
      for (int i=0; i<interfaces.length; ++i) {
         MethodDoc[] methods = interfaces[i].methods();
         for (int j=0; j<methods.length; ++j) {
            if (methods[j].name().equals(methodDoc.name())
                && methods[j].signature().equals(methodDoc.signature())) {
               specifyingInterfaces.add(methods[j]);
               break;
            }
         }
         collectSpecifiedByRecursive(specifyingInterfaces,
                                     interfaces[i],
                                     methodDoc);
      }
   }

   private void printMemberDetails(HtmlPage output,
                                   ProgramElementDoc[] memberDocs, String header,
                                   boolean isOnSerializedPage,
                                   String anchor)
   {
      if (memberDocs.length > 0) {

         if (null != anchor) {
            output.anchorName(anchor);
         }

         CssClass sectionClass;
         CssClass headerClass;
         if (isOnSerializedPage) {
            sectionClass = CssClass.SERIALIZED_SECTION;
            headerClass = CssClass.SERIALIZED_SECTION_HEADER;
         }
         else {
            sectionClass = CssClass.SECTION;
            headerClass = CssClass.SECTION_HEADER;
         }
         output.div(headerClass, header);
         output.beginDiv(sectionClass);

         for (int i=0; i<memberDocs.length; ++i) {
            if (i>0) {
               output.hr();
            }

            ProgramElementDoc memberDoc = memberDocs[i];

            output.anchorName(getMemberAnchor(memberDoc));

            output.beginDiv(CssClass.MEMBER_DETAIL);
            output.div(CssClass.MEMBER_DETAIL_NAME, memberDoc.name());

            StringBuffer synopsis = new StringBuffer();
            int synopsisLength = 0;

            if (!isOnSerializedPage || !memberDoc.isField()) {
               String fullModifiers = getFullModifiers(memberDoc);
               synopsis.append(fullModifiers);
               synopsisLength += fullModifiers.length();

            }
            if (memberDoc.isMethod() || memberDoc.isField()) {
               Type type;
               if (memberDoc.isMethod()) {
                  type = ((MethodDoc)memberDoc).returnType();
               }
               else {
                  type = ((FieldDoc)memberDoc).type();
               }

               synopsis.append(" ");
               synopsisLength ++;
               synopsis.append(createTypeHref(output, type, false));
               if (null != type.asClassDoc() && type.asClassDoc().isIncluded()) {
                  synopsisLength += type.asClassDoc().name().length();
               }
               else {
                  synopsisLength += type.qualifiedTypeName().length();
               }
               synopsisLength += type.dimension().length();
            }

            synopsis.append(" ");
            synopsisLength ++;

            if (optionLinkSource.getValue() && null != memberDoc.position()) {
               ClassDoc containingClass = memberDoc.containingClass();
               while (null != containingClass.containingClass()) {
                  containingClass = containingClass.containingClass();
               }
               String href = containingClass.name() + "-source" + filenameExtension + "#line." + memberDoc.position().line();
               synopsis.append(output.createHrefString(href, memberDoc.name()));
            }
            else {
               synopsis.append(memberDoc.name());
            }
            synopsisLength += memberDoc.name().length();

            if (memberDoc.isConstructor() || memberDoc.isMethod()) {
               //printParameters(output, (ExecutableMemberDoc)memberDoc);
               synopsis.append("(");
               ++ synopsisLength;
               StringBuffer paddingLeft = new StringBuffer();
               for (int j=0; j<synopsisLength; ++j) {
                  paddingLeft.append(' ');
               }
               Parameter[] parameters = ((ExecutableMemberDoc)memberDoc).parameters();
               for (int j=0; j<parameters.length; ++j) {
                  Parameter parameter = parameters[j];
                  synopsis.append(createTypeHref(output, parameter.type(), false));
                  synopsis.append(" ");
                  synopsis.append(parameter.name());
                  if (j < parameters.length - 1) {
                     synopsis.append(",\n");
                     synopsis.append(paddingLeft);
                  }
               }
               synopsis.append(")");
               ClassDoc[] exceptions = ((ExecutableMemberDoc)memberDoc).thrownExceptions();
               if (exceptions.length > 0) {
                  synopsis.append("\n            throws ");
                  for (int j=0; j<exceptions.length; ++j) {
                     ClassDoc exception = exceptions[j];
                     synopsis.append(createTypeHref(output, exception, false));
                     if (j < exceptions.length - 1) {
                        synopsis.append(",\n                   ");
                     }
                  }
               }
            }

            output.beginDiv(CssClass.MEMBER_DETAIL_SYNOPSIS);
            output.print(synopsis.toString());
            output.endDiv(CssClass.MEMBER_DETAIL_SYNOPSIS);

            output.beginDiv(CssClass.MEMBER_DETAIL_BODY);

            Tag[] deprecatedTags = memberDoc.tags("deprecated");
            if (deprecatedTags.length > 0) {
               output.beginDiv(CssClass.DEPRECATED_INLINE);
               output.beginSpan(CssClass.DEPRECATED_HEADER);
               output.print("Deprecated. ");
               output.endSpan(CssClass.DEPRECATED_HEADER);
               output.beginSpan(CssClass.DEPRECATED_BODY);
            }
            for (int j=0; j<deprecatedTags.length; ++j) {
               printTags(output, memberDoc, deprecatedTags[j].inlineTags(), true);
            }
            if (deprecatedTags.length > 0) {
               output.endSpan(CssClass.DEPRECATED_BODY);
               output.beginDiv(CssClass.DEPRECATED_INLINE);
            }

            output.beginDiv(CssClass.MEMBER_DETAIL_DESCRIPTION);
            printTags(output, memberDoc, memberDoc.inlineTags(), false);
            output.endDiv(CssClass.MEMBER_DETAIL_DESCRIPTION);

            if (memberDoc.isConstructor() || memberDoc.isMethod()) {

               if (memberDoc.isMethod()) {
                  Set specifyingInterfaces = new LinkedHashSet();
                  if (memberDoc.containingClass().isInterface()) {
                     collectSpecifiedByRecursive(specifyingInterfaces,
                                                 memberDoc.containingClass(),
                                                 (MethodDoc)memberDoc);
                  }
                  else {
                     for (ClassDoc cd = memberDoc.containingClass();
                          null != cd; cd = cd.superclass()) {
                        collectSpecifiedByRecursive(specifyingInterfaces,
                                                    cd,
                                                    (MethodDoc)memberDoc);
                     }
                  }

                  if (!specifyingInterfaces.isEmpty()
                      && !isOnSerializedPage) {
                     output.beginDiv(CssClass.MEMBER_DETAIL_SPECIFIED_BY_LIST);
                     output.div(CssClass.MEMBER_DETAIL_SPECIFIED_BY_HEADER, "Specified by:");
                     Iterator it = specifyingInterfaces.iterator();
                     while (it.hasNext()) {
                        MethodDoc specifyingInterfaceMethod = (MethodDoc)it.next();
                        output.beginDiv(CssClass.MEMBER_DETAIL_SPECIFIED_BY_ITEM);
                        output.beginAnchor(getMemberDocURL(output,
                                                           specifyingInterfaceMethod));
                        output.print(memberDoc.name());
                        output.endAnchor();
                        output.print(" in interface ");
                        printType(output, specifyingInterfaceMethod.containingClass());
                        output.endDiv(CssClass.MEMBER_DETAIL_SPECIFIED_BY_ITEM);
                     }
                     output.endDiv(CssClass.MEMBER_DETAIL_SPECIFIED_BY_LIST);
                  }

                  ClassDoc overriddenClassDoc = null;
                  MemberDoc specifyingSuperMethod = null;

                  for (ClassDoc superclassDoc = memberDoc.containingClass().superclass();
                       null != superclassDoc && null == overriddenClassDoc;
                       superclassDoc = superclassDoc.superclass()) {

                     MethodDoc[] methods = superclassDoc.methods();
                     for (int j=0; j<methods.length; ++j) {
                        if (methods[j].name().equals(memberDoc.name())
                            && methods[j].signature().equals(((MethodDoc)memberDoc).signature())) {
                           overriddenClassDoc = superclassDoc;
                           specifyingSuperMethod = methods[j];
                           break;
                        }
                     }
                  }

                  if (null != overriddenClassDoc) {
                     output.beginDiv(CssClass.MEMBER_DETAIL_OVERRIDDEN_LIST);
                     output.div(CssClass.MEMBER_DETAIL_OVERRIDDEN_HEADER, "Overrides:");
                     output.beginDiv(CssClass.MEMBER_DETAIL_OVERRIDDEN_ITEM);

                     output.beginAnchor(getMemberDocURL(output,
                                                        specifyingSuperMethod));
                     output.print(memberDoc.name());
                     output.endAnchor();
                     output.print(" in interface ");
                     printType(output, overriddenClassDoc);

                     output.endDiv(CssClass.MEMBER_DETAIL_OVERRIDDEN_ITEM);
                     output.endDiv(CssClass.MEMBER_DETAIL_OVERRIDDEN_LIST);
                  }
               }

               if (!optionNoComment.getValue()) {

                  ExecutableMemberDoc execMemberDoc
                     = (ExecutableMemberDoc)memberDoc;

                  if (execMemberDoc.paramTags().length > 0) {
                     output.beginDiv(CssClass.MEMBER_DETAIL_PARAMETER_LIST);
                     output.div(CssClass.MEMBER_DETAIL_PARAMETER_HEADER, "Parameters:");
                     Parameter[] parameters = execMemberDoc.parameters();
                     for (int j=0; j<parameters.length; ++j) {
                        Parameter parameter = parameters[j];
                        ParamTag[] paramTags = execMemberDoc.paramTags();
                        ParamTag paramTag = null;
                        for (int k=0; k<paramTags.length; ++k) {
                           if (paramTags[k].parameterName().equals(parameter.name())) {
                              paramTag = paramTags[k];
                              break;
                           }
                        }

                        if (null != paramTag) {
                           output.beginDiv(CssClass.MEMBER_DETAIL_PARAMETER_ITEM);
                           output.beginSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_NAME);
                           output.print(parameter.name());
                           output.endSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_NAME);
                           output.beginSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_SEPARATOR);
                           output.print(" - ");
                           output.endSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_SEPARATOR);
                           output.beginSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_DESCRIPTION);
                           printTags(output, execMemberDoc, paramTag.inlineTags(), false);
                           output.endSpan(CssClass.MEMBER_DETAIL_PARAMETER_ITEM_DESCRIPTION);
                           output.endDiv(CssClass.MEMBER_DETAIL_PARAMETER_ITEM);
                        }
                     }
                     output.endDiv(CssClass.MEMBER_DETAIL_PARAMETER_LIST);
                  }

                  if (execMemberDoc.isMethod()
                      && !"void".equals(((MethodDoc)execMemberDoc).returnType().typeName())) {

                     Tag[] returnTags = execMemberDoc.tags("return");
                     if (returnTags.length > 0) {
                        Tag returnTag = returnTags[0];

                        output.beginDiv(CssClass.MEMBER_DETAIL_RETURN_LIST);
                        output.div(CssClass.MEMBER_DETAIL_RETURN_HEADER, "Returns:");
                        output.beginDiv(CssClass.MEMBER_DETAIL_RETURN_ITEM);

                        printTags(output, execMemberDoc, returnTag.inlineTags(), false);

                        output.endDiv(CssClass.MEMBER_DETAIL_RETURN_ITEM);
                        output.endDiv(CssClass.MEMBER_DETAIL_RETURN_LIST);
                     }
                  }

                  Set thrownExceptions = getThrownExceptions(execMemberDoc);
                  boolean haveThrowsInfo = false;
                  ThrowsTag[] throwsTags = execMemberDoc.throwsTags();
                  for (int k=0; k<throwsTags.length; ++k) {
                     ThrowsTag throwsTag = throwsTags[k];
                     if (null != throwsTags[k].exception()
                         && (isUncheckedException(throwsTags[k].exception())
                             || thrownExceptions.contains(throwsTag.exception()))) {
                        haveThrowsInfo = true;
                        break;
                     }
                  }

                  if (haveThrowsInfo) {
                     output.beginDiv(CssClass.MEMBER_DETAIL_THROWN_LIST);
                     output.div(CssClass.MEMBER_DETAIL_THROWN_HEADER, "Throws:");

                     for (int k=0; k<throwsTags.length; ++k) {
                        ThrowsTag throwsTag = throwsTags[k];
                        if (null != throwsTag.exception()
                            && (isUncheckedException(throwsTag.exception())
                                || thrownExceptions.contains(throwsTag.exception()))) {
                           output.beginDiv(CssClass.MEMBER_DETAIL_THROWN_ITEM);
                           output.beginSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_NAME);
                           printType(output, throwsTags[k].exception());
                           output.endSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_NAME);
                           if (null != throwsTag) {
                              output.beginSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_SEPARATOR);
                              output.print(" - ");
                              output.endSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_SEPARATOR);
                              output.beginSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_DESCRIPTION);
                              printTags(output, execMemberDoc, throwsTag.inlineTags(), false);
                              output.endSpan(CssClass.MEMBER_DETAIL_THROWN_ITEM_DESCRIPTION);
                           }
                           output.endDiv(CssClass.MEMBER_DETAIL_THROWN_ITEM);
                        }
                     }
                     output.endDiv(CssClass.MEMBER_DETAIL_THROWN_LIST);
                  }
               }
            }

            if (!optionNoComment.getValue()) {

               if (memberDoc.isField()) {
                  FieldDoc fieldDoc = ((FieldDoc)memberDoc);
                  if (null != fieldDoc.constantValue()) {
                     output.beginDiv(CssClass.MEMBER_DETAIL_THROWN_LIST);
                     output.div(CssClass.MEMBER_DETAIL_THROWN_HEADER, "Field Value:");
                     output.div(CssClass.MEMBER_DETAIL_THROWN_ITEM,
                                fieldDoc.constantValueExpression().toString());
                     output.endDiv(CssClass.MEMBER_DETAIL_THROWN_LIST);
                  }
               }

               TagletContext context = new HtmlTagletContext(memberDoc, output, isOnSerializedPage);
               printTaglets(output, memberDoc.tags(), context);
            }

            output.endDiv(CssClass.MEMBER_DETAIL_BODY);
            output.endDiv(CssClass.MEMBER_DETAIL);
         }
         output.endDiv(sectionClass);
      }
   }


   private void printParameters(HtmlPage output, ExecutableMemberDoc memberDoc)
   {
      Parameter[] parameters = memberDoc.parameters();
      output.print("(");
      for (int j=0; j<parameters.length; ++j) {
         if (j > 0) {
            output.print(", ");
         }
         printType(output, parameters[j].type());
         output.print("&nbsp;");
         output.print(parameters[j].name());
      }
      output.print(")");
   }

   private void printProgramElementDocs(HtmlPage output,
                                        ProgramElementDoc[] memberDocs,
                                        String header,
                                        boolean forceOutputHeader,
                                        String anchor)
   {
      if (memberDocs.length > 0 || forceOutputHeader) {
         output.anchorName(anchor);
         output.beginDiv(CssClass.TABLE_CONTAINER);
         output.beginTable(CssClass.CLASS_SUMMARY, new String[] { "border", "width" }, new String[] { "1", "100%" });
         output.rowDiv(CssClass.TABLE_HEADER, header);

         for (int i=0; i<memberDocs.length; ++i) {
            ProgramElementDoc memberDoc = memberDocs[i];
            output.beginRow();

            if (!memberDoc.isConstructor()) {
               output.beginCell(CssClass.CLASS_SUMMARY_LEFT);
               output.beginDiv(CssClass.CLASS_SUMMARY_LEFT_SYNOPSIS);
               output.print(getSummaryModifiers(memberDoc) + " ");
               if (memberDoc.isMethod()) {
                  printType(output, ((MethodDoc)memberDoc).returnType());
               }
               else if (memberDoc.isField()) {
                  printType(output, ((FieldDoc)memberDoc).type());
               }
               else if (memberDoc.isInterface()) {
                  output.print(" interface");
               }
               else if (memberDoc.isClass()) {
                  output.print(" class");
               }
               output.endDiv(CssClass.CLASS_SUMMARY_LEFT_SYNOPSIS);
               output.endCell();
            }

            output.beginCell(CssClass.CLASS_SUMMARY_RIGHT);
            output.beginDiv(CssClass.CLASS_SUMMARY_RIGHT_LIST);
            output.beginDiv(CssClass.CLASS_SUMMARY_RIGHT_SYNOPSIS);
            if (memberDoc.isClass() || memberDoc.isInterface()) {
               output.beginAnchor(getClassDocURL(output, (ClassDoc)memberDoc));
            }
            else {
               output.beginAnchor("#" + getMemberAnchor(memberDoc));
            }
            output.print(memberDoc.name());
            output.endAnchor();
            if (memberDoc.isConstructor() || memberDoc.isMethod()) {
               printParameters(output, (ExecutableMemberDoc)memberDoc);
            }
            output.endDiv(CssClass.CLASS_SUMMARY_RIGHT_SYNOPSIS);
            Tag[] firstSentenceTags;
            Tag[] deprecatedTags = memberDoc.tags("deprecated");
            if (deprecatedTags.length > 0) {
               firstSentenceTags = deprecatedTags[0].firstSentenceTags();
            }
            else {
               firstSentenceTags = memberDoc.firstSentenceTags();
            }

            if (null != firstSentenceTags && firstSentenceTags.length > 0) {
               output.beginDiv(CssClass.CLASS_SUMMARY_RIGHT_DESCRIPTION);
               if (deprecatedTags.length > 0) {
                  output.beginDiv(CssClass.DEPRECATED);
                  output.beginSpan(CssClass.DEPRECATED_HEADER);
                  output.print("Deprecated. ");
                  output.endSpan(CssClass.DEPRECATED_HEADER);
                  output.beginSpan(CssClass.DEPRECATED_BODY);
               }
               printTags(output, memberDoc, firstSentenceTags, true);
               if (deprecatedTags.length > 0) {
                  output.endSpan(CssClass.DEPRECATED_BODY);
                  output.beginDiv(CssClass.DEPRECATED);
               }
               output.endDiv(CssClass.CLASS_SUMMARY_RIGHT_DESCRIPTION);
            }
            output.endDiv(CssClass.CLASS_SUMMARY_RIGHT_LIST);
            output.endCell();
            output.endRow();
         }
         output.endTable();
         output.endDiv(CssClass.TABLE_CONTAINER);
      }
   }

   private void printTag(final HtmlPage output,
                         HtmlRepairer repairer,
                         Tag tag, boolean firstSentence,
                         boolean inline,
                         Doc contextDoc)
   {
      TagletContext context = new HtmlTagletContext(contextDoc, output, false);
      if (firstSentence) {
         output.print(renderInlineTags(tag.firstSentenceTags(), context));
      }
      else {
         output.print(renderInlineTags(tag.inlineTags(), context));
      }
   }

   private void printTags(HtmlPage output, Doc contextDoc, Tag[] tags, boolean firstSentence)
   {
      printTags(output, contextDoc, tags, firstSentence, false);
   }

   private void printTags(HtmlPage output, Doc contextDoc, Tag[] tags, boolean firstSentence, boolean inline)
   {
      if (!optionNoComment.getValue()) {
         output.print(renderInlineTags(tags, new HtmlTagletContext(contextDoc, output, false)));
      }

      /*
      if (!optionNoComment.getValue()) {
         output.print(renderInlineTags(tag.firstSentenceTags(), output));
         HtmlRepairer repairer = new HtmlRepairer(getRootDoc(),
                                                  true, false,
                                                  null, null,
                                                  true);
         for (int i=0; i<tags.length; ++i) {
            printTag(output, repairer, tags[i], firstSentence, inline);
         }
         output.print(repairer.terminateText());
      }
      */
   }

   private String getClassDocURL(HtmlPage output, ClassDoc classDoc)
   {
      return output.getPathToRoot()
         + "/"
         + getPackageURL(classDoc.containingPackage())
         + classDoc.name() + filenameExtension;
   }

   private String getMemberDocURL(HtmlPage output, ProgramElementDoc memberDoc)
   {
      ClassDoc classDoc = memberDoc.containingClass();
      PackageDoc packageDoc = classDoc.containingPackage();
      ExternalDocSet externalDocSet = null;
      if (classDoc.containingPackage().name().length() > 0) {
         externalDocSet = packageNameToDocSet.get(packageDoc.name());
      }
      StringBuffer result = new StringBuffer();
      result.append(getClassDocURL(output, classDoc));
      result.append('#');
      if (null == externalDocSet) {
         result.append(getMemberAnchor(memberDoc));
      }
      else {
         result.append(getMemberAnchor(memberDoc, externalDocSet.isJavadocCompatible()));
      }
      return result.toString();
   }

   private void printType(HtmlPage output, Type type)
   {
      printType(output, type, false);
   }

   private void printType(HtmlPage output, Type type, boolean fullyQualified)
   {
      output.print(createTypeHref(output, type, fullyQualified));
   }

   private String createTypeHref(HtmlPage output, Type type, boolean fullyQualified)
   {
      ClassDoc asClassDoc = type.asClassDoc();
      String url = null;
      if (null != asClassDoc && asClassDoc.isIncluded()) {
         url = getClassDocURL(output, asClassDoc);
     }
      else if (!type.isPrimitive()) {
         if (type.qualifiedTypeName().length() > type.typeName().length()) {
            String packageName = type.qualifiedTypeName();
            packageName = packageName.substring(0, packageName.length() - type.typeName().length() - 1);

            ExternalDocSet externalDocSet = packageNameToDocSet.get(packageName);
            if (null != externalDocSet) {
               url = externalDocSet.getClassDocURL(packageName, type.typeName());
            }
         }
      }

      StringBuffer result = new StringBuffer();

      if (null != url && null != asClassDoc) {
        String parameters = getTypeParameters(asClassDoc);
         if (fullyQualified) {
            result.append(output.createHrefString(url,possiblyQualifiedName(asClassDoc) + parameters));
         }
         else {
            StringBuffer title = new StringBuffer();
            title.append(getClassTypeName(asClassDoc));
            title.append(" in ");
            title.append(asClassDoc.containingPackage().name());
            result.append(output.createHrefString(url, asClassDoc.name() + parameters, title.toString()));
         }
      }
      else {
         result.append(possiblyQualifiedName(type));
      }
      result.append(type.dimension());
      return result.toString();
   }

   private void printTaglets(final HtmlPage output, Tag[] tags, TagletContext context)
   {
      super.printMainTaglets(tags, context, new TagletPrinter() {
            public void printTagletString(String tagletString) {
               output.beginDiv(CssClass.TAGLET);
               output.print(tagletString);
               output.endDiv(CssClass.TAGLET);
            }
         });
   }

   private String getPackageURL(PackageDoc packageDoc)
   {
      if (packageDoc.name().length() > 0) {
         ExternalDocSet externalDocSet = packageNameToDocSet.get(packageDoc.name());
         String url;
         if (null != externalDocSet) {
            url = externalDocSet.getPackageDocURL(packageDoc.name());
         }
         else {
            url = packageDoc.name().replace('.', '/');
         }
         if (!url.endsWith("/")) {
            return url + '/';
         }
         else {
            return url;
         }
     }
      else {
         return "";
      }
   }

   private String getClassURL(ClassDoc classDoc)
   {
      ExternalDocSet externalDocSet = null;
      if (classDoc.containingPackage().name().length() > 0) {
         externalDocSet = packageNameToDocSet.get(classDoc.containingPackage().name());
      }
      if (null != externalDocSet) {
         return externalDocSet.getClassDocURL(classDoc.containingPackage().name(),
                                              classDoc.name());
      }
      else {
         return getPackageURL(classDoc.containingPackage()) + classDoc.name() + filenameExtension;
      }
   }

   protected void run()
      throws DocletConfigurationException, IOException
   {
      if (optionSerialWarn.getValue()) {
         printWarning("Option -serialwarn is currently ignored.");
      }

      if (null != optionTitle.getValue()) {
         printWarning("Option -title is deprecated.");
      }

      if (!optionValidHtml.getValue()) {
         printWarning("Option -validhtml hasn't been specified. Generated HTML will not validate.");
      }


      {
         boolean warningEmitted = false;
         Iterator it = externalDocSets.iterator();
         while (it.hasNext()) {
            ExternalDocSet externalDocSet = (ExternalDocSet)it.next();
            printNotice("Fetching package list for external documentation set.");
            try {
               externalDocSet.load(getTargetDirectory());
               if (!isJavadocCompatibleNames() && externalDocSet.isJavadocCompatible()
                   && !warningEmitted) {
                  printWarning("Linking to javadoc-compatible documentation. Generated HTML will not validate ");
                  warningEmitted = true;
               }
            }
            catch (FileNotFoundException e) {
               printWarning("Cannot fetch package list from " + externalDocSet.getPackageListDir());
            }
            Iterator pit = externalDocSet.getPackageNames().iterator();
            while (pit.hasNext()) {
               String packageName = (String)pit.next();
               packageNameToDocSet.put(packageName, externalDocSet);
            }
         }
      }
      printNotice("Building cross-reference information...");
      getInterfaceRelations();
      getAllSubClasses();

      printNotice("Writing overview files...");
      printFrameSetPage();
      if (!isSinglePackage()) {
         printPackagesMenuPage();
         printAllClassesMenuPage();
         printOverviewPage();
         if (!optionNoTree.getValue()) {
            printNotice("Writing full tree...");
            printFullTreePage();
         }
      }
      printPackagesListFile();
      printAboutPage();
      if (!optionNoIndex.getValue()) {
         printNotice("Writing index...");
         if (!optionSplitIndex.getValue()) {
            printIndexPage();
         }
         else {
            printSplitIndex();
         }
      }
      if (outputHelpPage && !optionNoHelp.getValue()) {
         printHelpPage();
      }

      // Copy resources

      File resourcesDir = new File(getTargetDirectory(),
                                   "resources");

      if ((resourcesDir.exists() && !resourcesDir.isDirectory())
          || (!resourcesDir.exists() && !resourcesDir.mkdirs())) {
         throw new IOException("Cannot create directory " + resourcesDir);
      }

      // Copy resources

      String[] resourceNames = {
         "gjdoc.js",
         "gjdochtml-clean-layout.css",
         "gjdochtml-clean-color1.css",
         "inherit.png",
         "xhtml11-target10.dtd",
      };

      for (int i=0; i<resourceNames.length; ++i) {
         String resourceName = resourceNames[i];
         File targetFile = new File(resourcesDir,
                                    resourceName);
         InputStream in = getClass().getResourceAsStream("/htmldoclet/" + resourceName);
         if (in == null) {
                in = new FileInputStream("src/resources/htmldoclet/" + resourceName);
         }
         FileOutputStream out = new FileOutputStream(targetFile);
         IOToolkit.copyStream(in, out);
         in.close();
         out.close();
      }

      // Copy stylesheets

      if (null != optionAddStylesheet.getValue()) {
         File addStylesheetTargetFile = new File(resourcesDir,
                                                 "user.css");

         IOToolkit.copyFile(optionAddStylesheet.getValue(),
                            addStylesheetTargetFile);
      }

      if (null != optionStylesheetFile.getValue()) {
         File stylesheetTargetFile = new File(resourcesDir,
                                              "user.css");

         IOToolkit.copyFile(optionStylesheetFile.getValue(),
                            stylesheetTargetFile);
      }

      // Write gjdoc.properties

      File gjdocPropertiesTargetFile = new File(getTargetDirectory(),
                                                "gjdoc.properties");
      writeGjdocProperties(gjdocPropertiesTargetFile);

      /*
      else {
         InputStream cssIn = getClass().getResourceAsStream("/htmldoclet/gjdochtml-vanilla.css");
         FileOutputStream cssOut = new FileOutputStream(stylesheetTargetFile);
         IOToolkit.copyStream(cssIn, cssOut);
         cssIn.close();
         cssOut.close();
      }
      */

      if (!optionNoDeprecatedList.getValue()) {
         printDeprecationPage();
      }

      printSerializationPage();

      Collection packageDocsCollection = getAllPackages();
      PackageDoc[] packageDocs
         = (PackageDoc[])packageDocsCollection.toArray(new PackageDoc[0]);

      for (int i=0; i<packageDocs.length; ++i) {
         PackageDoc packageDoc = packageDocs[i];
         File packageDir = new File(getTargetDirectory(),
                                    packageDoc.name().replace('.', File.separatorChar));
         if (!packageDir.exists() && !packageDir.mkdirs()) {
            throw new IOException("Couldn't create directory " + packageDir);
         }
         try {
            List packageSourceDirs = getPackageSourceDirs(packageDoc);
            Iterator pdIt = packageSourceDirs.iterator();
            while (pdIt.hasNext()) {
               File sourcePackageDir = (File)pdIt.next();
               copyDocFiles(sourcePackageDir, packageDir);
            }
         }
         catch (IOException ignore) {
         }
         String pathToRoot = getPathToRoot(packageDir, getTargetDirectory());
         String packageName = packageDoc.name();
         if (0 == packageName.length()) {
            packageName = "<unnamed>";
         }
         printNotice("Writing HTML files for package " + packageName);
         printPackagePage(packageDir, pathToRoot, packageDoc,
                          (i > 0) ? packageDocs[i - 1] : null,
                          (i < packageDocs.length - 1) ? packageDocs[i + 1] : null);
         if (!optionNoTree.getValue()) {
            printPackageTreePage(packageDir, pathToRoot, packageDoc);
         }
         printPackageClassesMenuPage(packageDir, pathToRoot, packageDoc);
         ClassDoc[] classDocs = packageDoc.allClasses();
         for (int j=0; j<classDocs.length; ++j) {
            ClassDoc classDoc = classDocs[j];
            if (classDoc.isIncluded()) {
               printClassPage(packageDir, pathToRoot,
                              classDocs[j],
                              (j > 0) ? classDocs[j - 1] : null,
                              (j < classDocs.length - 1) ? classDocs[j + 1] : null
                              );
               if (optionUse.getValue()) {
                  printClassUsagePage(packageDir, pathToRoot, classDocs[j]);
               }
               if (optionLinkSource.getValue() && null == classDoc.containingClass()) {
                  try {
                     File sourceFile = getSourceFile(classDoc);

                     Java2xhtml java2xhtml = new Java2xhtml();
                     Properties properties = new Properties();
                     properties.setProperty("isCodeSnippet", "true");
                     properties.setProperty("hasLineNumbers", "true");
                     java2xhtml.setProperties(properties);

                     StringWriter sourceBuffer = new StringWriter();
                     FileReader sourceReader = new FileReader(sourceFile);
                     IOToolkit.copyStream(sourceReader, sourceBuffer);
                     sourceReader.close();
                     String result = java2xhtml.makeHTML(sourceBuffer.getBuffer(), sourceFile.getName());

                     printSourcePage(packageDir,
                                     classDoc,
                                     result);
                  }
                  catch (IOException e) {
                     printWarning("Cannot locate source file for class " + classDoc.qualifiedTypeName());
                  }
               }
            }
         }
      }
   }

   private String getPathToRoot(File subDir, File rootDir)
   {
      StringBuffer result = new StringBuffer();
      while (!subDir.equals(rootDir)) {
         if (result.length() > 0) {
            result.append("/");
         }
         subDir = subDir.getParentFile();
         result.append("..");
      }
      if (0 == result.length()) {
         result.append(".");
      }
      return result.toString();
   }

   private String getClassTypeName(ClassDoc classDoc)
   {
      if (classDoc.isInterface()) {
         return "Interface";
      }
      else {
         return "Class";
      }
   }

   private String getClassTypeKeyword(ClassDoc classDoc)
   {
      if (classDoc.isInterface()) {
         return "interface";
      }
      else {
         return "class";
      }
   }

   private String getMemberAnchor(ProgramElementDoc memberDoc)
   {
      return getMemberAnchor(memberDoc, isJavadocCompatibleNames());
   }

   private String getMemberAnchor(ProgramElementDoc memberDoc, boolean javadocCompatibility)
   {
      StringBuffer anchor = new StringBuffer();
      anchor.append(memberDoc.name());
      if (memberDoc.isConstructor() || memberDoc.isMethod()) {
         if (javadocCompatibility) {
            anchor.append(((ExecutableMemberDoc)memberDoc).signature());
         }
         else {
            anchor.append(':');
            Parameter[] parameters = ((ExecutableMemberDoc)memberDoc).parameters();
            for (int i=0; i<parameters.length; ++i) {
               anchor.append(parameters[i].type().typeName());
               for (int j=0; j<parameters[i].type().dimension().length()/2; ++j) {
                  anchor.append('-');
               }
               if (i < parameters.length - 1) {
                  anchor.append(':');
               }
            }
         }
      }
      return anchor.toString();
   }

   private String getFullModifiers(ProgramElementDoc memberDoc)
   {
      StringBuffer result = new StringBuffer();
      if (memberDoc.isPackagePrivate()) {
         result.append("(package private) ");
      }
      result.append(memberDoc.modifiers());
      if ((memberDoc.isClass() && ((ClassDoc)memberDoc).isAbstract())
          || (memberDoc.isMethod() && ((MethodDoc)memberDoc).isAbstract())) {
         result.append(" abstract");
      }
      return result.toString();
   }

   private String getSummaryModifiers(ProgramElementDoc memberDoc)
   {
      StringBuffer result = new StringBuffer();
      if (memberDoc.isPackagePrivate()) {
         result.append("(package private) ");
      }
      else if (memberDoc.isPrivate()) {
         result.append("private ");
      }
      else if (memberDoc.isProtected()) {
         result.append("protected ");
      }
      if (memberDoc.isStatic()) {
         result.append("static");
      }
      else if ((memberDoc.isClass() && ((ClassDoc)memberDoc).isAbstract())
          || (memberDoc.isMethod() && ((MethodDoc)memberDoc).isAbstract())) {
         result.append("abstract");
      }
      return result.toString();
   }

   protected DocletOption[] getOptions()
   {
      return options;
   }

   private DocletOptionFlag optionNoNavBar =
     new DocletOptionFlag("-nonavbar");

   private DocletOptionFlag optionNoTree =
     new DocletOptionFlag("-notree");

   private DocletOptionFlag optionNoDeprecatedList =
     new DocletOptionFlag("-nodeprecatedlist");

   private DocletOptionFlag optionNoIndex =
     new DocletOptionFlag("-noindex");

   private DocletOptionFlag optionUse =
     new DocletOptionFlag("-use");

   private DocletOptionFlag optionNoHelp =
     new DocletOptionFlag("-nohelp");

   private DocletOptionFlag optionNoComment =
     new DocletOptionFlag("-nocomment");

   private DocletOptionFlag optionSerialWarn =
     new DocletOptionFlag("-serialwarn");

   private DocletOptionFlag optionSplitIndex =
     new DocletOptionFlag("-splitindex");

   private DocletOptionString optionHeader =
     new DocletOptionString("-header");

   private DocletOptionString optionFooter =
     new DocletOptionString("-footer");

   private DocletOptionString optionBottom =
     new DocletOptionString("-bottom");

   private DocletOptionString optionWindowTitle =
     new DocletOptionString("-windowtitle");

   private DocletOptionString optionDocTitle =
     new DocletOptionString("-doctitle");

   private DocletOptionString optionTitle =
     new DocletOptionString("-title");

   private DocletOptionFile optionHelpFile =
     new DocletOptionFile("-helpfile");

   private DocletOptionFile optionStylesheetFile =
     new DocletOptionFile("-stylesheetfile");

   private DocletOptionFlag optionLinkSource =
     new DocletOptionFlag("-linksource");

   private DocletOption optionLink =
     new DocletOption("-link") {

        public int getLength()
        {
           return 2;
        }

        public boolean set(String[] optionArr)
        {
           externalDocSets.add(new ExternalDocSet(optionArr[1], null));
           return true;
        }
     };

   private DocletOption optionLinkOffline =
     new DocletOption("-linkoffline") {

        public int getLength()
        {
           return 3;
        }

        public boolean set(String[] optionArr)
        {
           externalDocSets.add(new ExternalDocSet(optionArr[1], optionArr[2]));
           return true;
        }
     };

   private DocletOptionString optionDocEncoding =
     new DocletOptionString("-docencoding");

   private DocletOptionString optionEncoding =
     new DocletOptionString("-encoding");

   private DocletOptionString optionCharset =
     new DocletOptionString("-charset");

   private DocletOptionFile optionAddStylesheet =
     new DocletOptionFile("-addstylesheet");

   private DocletOptionFlag optionValidHtml =
     new DocletOptionFlag("-validhtml");

   private DocletOptionString optionBaseUrl =
     new DocletOptionString("-baseurl");

   private DocletOption[] options =
      {
         optionNoNavBar,
         optionNoTree,
         optionNoDeprecatedList,
         optionNoIndex,
         optionNoHelp,
         optionNoComment,
         optionUse,
         optionSplitIndex,
         optionHeader,
         optionFooter,
         optionBottom,
         optionHelpFile,
         optionStylesheetFile,
         optionWindowTitle,
         optionDocTitle,
         optionTitle,
         optionLinkSource,
         optionLink,
         optionLinkOffline,
         optionDocEncoding,
         optionEncoding,
         optionCharset,
         optionAddStylesheet,
         optionValidHtml,
         optionBaseUrl,
      };

   static {
      setInstance(new HtmlDoclet());
   }

   private static String replaceDocRoot(HtmlPage output, String str)
   {
      return StringToolkit.replace(str, "{@docRoot}", output.getPathToRoot());
   }

   private String getOutputDocEncoding()
   {
      String encoding = optionDocEncoding.getValue();

      if (null == encoding) {
         encoding = optionEncoding.getValue();
      }

      return encoding;
   }

   private String getOutputCharset()
   {
      if (null == outputCharset) {

         if (null != optionCharset.getValue()) {
            outputCharset = optionCharset.getValue();
         }
         else {
            String fileEncoding = System.getProperty("file.encoding");
            if (null != fileEncoding) {
               try {
                  outputCharset = Charset.forName(fileEncoding).name();
               }
               catch (Exception ignore) {
               }
            }

            if (null == outputCharset) {
               printWarning("Cannot determine platform default charset, falling back to ISO-8859-1.");
               outputCharset = "ISO-8859-1";
            }
         }
      }
      return outputCharset;
   }

   public InlineTagRenderer getInlineTagRenderer()
   {
      return this;
   }

   public String renderInlineTags(Tag[] tags, TagletContext context)
   {
      StringBuffer result = new StringBuffer();

      HtmlRepairer repairer = new HtmlRepairer(getRootDoc(),
                                               true, false,
                                               null, null,
                                               true);

      for (int i=0; i<tags.length; ++i) {

         Tag tag = tags[i];

         if ("Text".equals(tag.name())) {
            result.append(repairer.getWellformedHTML(tag.text()));
         }
         else if ("@link".equals(tag.name())) {
            result.append(renderSeeTag((SeeTag)tag, context, false));
         }
         else if ("@linkplain".equals(tag.name())) {
            result.append(renderSeeTag((SeeTag)tag, context, true));
         }
         else if ("@docRoot".equals(tag.name())) {
            result.append(((HtmlTagletContext)context).getOutput().getPathToRoot());
         }
         else {
            //TagletContext context = TagletContext.OVERVIEW; // FIXME
            Taglet taglet = (Taglet)tagletMap.get(tag.name().substring(1));
            if (null != taglet) {
               if (taglet instanceof GnuExtendedTaglet) {
                  result.append(((GnuExtendedTaglet)taglet).toString(tag, context));
               }
               else {
                  result.append(taglet.toString(tag));
               }
            }
         }
      }
      result.append(repairer.terminateText());
      return result.toString();
   }

   public String renderSeeTag(SeeTag seeTag, TagletContext context, boolean plainFont)
   {
      StringBuffer result = new StringBuffer();

      String href = null;
      String label = null;
      MemberDoc referencedMember = seeTag.referencedMember();
      if (null != seeTag.referencedClass()) {

         href = getClassDocURL(((HtmlTagletContext)context).getOutput(), seeTag.referencedClass());

         Doc doc = context.getDoc();
         ClassDoc classDoc = null;
         if (doc.isClass() || doc.isInterface()) {
            classDoc = (ClassDoc)doc;
         }
         else if (doc.isField() || doc.isMethod() || doc.isConstructor()) {
            classDoc = ((MemberDoc)doc).containingClass();
         }

         if (null == referencedMember
             || seeTag.referencedClass() != classDoc
             || ((HtmlTagletContext)context).isOnSerializedPage()) {

            if (!seeTag.referencedClass().isIncluded()) {
               label = possiblyQualifiedName(seeTag.referencedClass());
            }
            else {
               label = seeTag.referencedClass().typeName();
            }
            if (null != referencedMember) {
               label += '.';
            }
         }
         else {
            label = "";
         }

         if (null != referencedMember) {
            label += referencedMember.name();
            if (referencedMember.isMethod() || referencedMember.isConstructor()) {
               label += ((ExecutableMemberDoc)referencedMember).flatSignature();
            }
            href  += '#' + getMemberAnchor(referencedMember);
         }
         else if (null != seeTag.referencedMemberName()) {
            href = null;
         }
      }
      else {
         String referencedClassName = seeTag.referencedClassName();

         if (null != referencedClassName) {

            String referencedPackageName = null;

            Iterator it = packageNameToDocSet.keySet().iterator();
            while (it.hasNext()) {
               String packageName = (String)it.next();
               if ((null == referencedPackageName
                    || packageName.length() > referencedPackageName.length())
                   && referencedClassName.startsWith(packageName + '.')) {
                  referencedPackageName = packageName;
               }
            }

            if (null != referencedPackageName) {
               ExternalDocSet externalDocSet
                  = (ExternalDocSet)packageNameToDocSet.get(referencedPackageName);

               String className = referencedClassName.substring(referencedPackageName.length() + 1);
               href = externalDocSet.getClassDocURL(referencedPackageName,
                                                    className);
               label = className;

               String referencedMemberName = seeTag.referencedMemberName();

               if (null != referencedMemberName) {
                  label += '.';
                  label += referencedMemberName;
                  href  += '#' + transformReferencedMemberName(referencedMemberName,
                                                               externalDocSet.isJavadocCompatible());
               }
               else if (null != seeTag.referencedMemberName()) {
                  href = null;
               }
            }
         }
      }

      if (null != seeTag.label()
          && seeTag.label().length() > 0) {
         label = seeTag.label();
      }

      if (null == label) {
         label = seeTag.text();
         if (label.startsWith("#")) {
            label = label.substring(1);
         }
         else {
            label = label.replace('#', '.');
         }
         label.trim();
      }

      if (null != href) {
         result.append("<a href=\"");
         result.append(href);
         result.append("\">");
         if (!plainFont) {
            result.append("<code>");
         }
         result.append(label);
         if (!plainFont) {
            result.append("</code>");
         }
         result.append("</a>");
      }
      else {
         if (!plainFont) {
            result.append("<code>");
         }
         result.append(label);
         if (!plainFont) {
            result.append("</code>");
         }
      }

      return result.toString();
   }

   protected String renderTag(String tagName, Tag[] tags, TagletContext context)
   {
      Doc doc = context.getDoc();

      if ("see".equals(tagName)
          && ((tags.length > 0)
              || (doc.isClass()
                  && (((ClassDoc)doc).isSerializable()
                      || ((ClassDoc)doc).isExternalizable())))) {

         StringBuffer result = new StringBuffer();
         result.append("<dl class=\"tag list\">");
         result.append("<dt class=\"tag section header\"><b>");
         result.append("See Also:");
         result.append("</b></dt>");

         boolean oneLine = true;

         if (oneLine) {
            result.append("<dd>");
         }

         for (int i = 0; i < tags.length; ++i) {
            if (oneLine) {
               if (i > 0) {
                  result.append(", ");
               }
            }
            else {
               result.append("<dd>");
            }
            result.append(renderSeeTag((SeeTag)tags[i], context, false));
            if (!oneLine) {
               result.append("</dd>");
            }
         }

         if ((doc instanceof ClassDoc)
             && (((ClassDoc)doc).isSerializable() || ((ClassDoc)doc).isExternalizable())) {
            if (tags.length > 0) {
               result.append(", ");
            }
            HtmlPage output = ((HtmlTagletContext)context).getOutput();
            result.append("<a href=\"" + output.getPathToRoot() + "/serialized-form" + filenameExtension + "#" + ((ClassDoc)doc).qualifiedName() + "\">Serialized Form</a>");
         }

         if (oneLine) {
            result.append("</dd>");
         }
         result.append("</dl>");
         return result.toString();
      }
      else if (tags.length > 0
               && "serial".equals(tagName)
               && ((HtmlTagletContext)context).isOnSerializedPage()) {

         return renderInlineTags(tags[0].inlineTags(), context);
      }
      else {
         return "";
      }
   }

   private String getWindowTitle()
   {
      if (null == optionWindowTitle.getValue()) {
         return "Generated API Documentation";
      }
      else {
         return optionWindowTitle.getValue();
      }
   }

   private String getPageTitle(String title)
   {
      if (null == optionWindowTitle.getValue()) {
         return title;
      }
      else {
         return title + " (" + optionWindowTitle.getValue() + ")";
      }
   }

   protected String getDocletVersion()
   {
      if (null == docletVersion) {
         docletVersion = gnu.classpath.Configuration.CLASSPATH_VERSION;
      }
      return docletVersion;
   }

   private Map getStylesheets()
   {
      Map sheets = new HashMap();
      if (null != optionStylesheetFile.getValue()) {
         sheets.put("User-specified", new String[] {
            "resources/user.css"
         });
      }
      else {
         List cleanSheets = new LinkedList();
         cleanSheets.add("resources/gjdochtml-clean-layout.css");
         cleanSheets.add("resources/gjdochtml-clean-color1.css");
         if (null != optionAddStylesheet.getValue()) {
            cleanSheets.add("resources/user.css");
         }
         sheets.put("GNU Clean", cleanSheets.toArray(new String[0]));
      }
      return sheets;
   }

   protected boolean isSinglePackage()
   {
      if (getRootDoc().firstSentenceTags().length > 0) {
         return false;
      }
      else if (null != optionDocTitle.getValue()
               || null != optionTitle.getValue()) {
         return false;
      }
      else {
         return super.isSinglePackage();
      }
   }

  private String getTypeParameters(ClassDoc classDoc)
  {
    String parameters = "";
    TypeVariable[] params = classDoc.typeParameters();
    if (params != null && params.length > 0)
      {
        parameters = "&lt;";
        for (int a = 0; a < params.length; ++a)
          {
            parameters += params[a].typeName();
            Type[] bounds = params[a].bounds();
            if (bounds != null)
              {
                parameters += " extends ";
                for (int b = 0; a < bounds.length; ++b)
                  {
                    parameters += bounds[a];
                    if (b != bounds.length - 1)
                      parameters += " & ";
                  }
              }
            if (a != params.length - 1)
              parameters += ",";
          }
        parameters += "&gt;";
      }
    return parameters;
  }

   private String transformReferencedMemberName(String referencedMemberName,
                                                boolean javadocCompatibility)
   {
      if (!javadocCompatibility) {
         StringBuffer result = new StringBuffer();
         for (int i=0; i<referencedMemberName.length(); ++i) {
            char c = referencedMemberName.charAt(i);
            switch (c) {
            case '(': result.append(':'); break;
            case ')': break;
            case ',': result.append(':'); break;
            case '[': result.append('-'); break;
            case ']': break;
            default:  result.append(c); break;
            }
         }
         return result.toString();
      }
      else {
         return referencedMemberName;
      }
   }

   public void writeGjdocProperties(File outputFile)
      throws IOException
   {
      Properties properties = new Properties();
      properties.setProperty("gjdoc.version", getDocletVersion());
      properties.setProperty("gjdoc.compat", Boolean.toString(isJavadocCompatibleNames()));

      FileOutputStream out = new FileOutputStream(outputFile);
      properties.store(out, "GNU Gjdoc API Documentation Set Descriptor");
      out.close();
   }

   public boolean isJavadocCompatibleNames()
   {
      return !optionValidHtml.getValue();
   }

   private HtmlPage newHtmlPage(File file,
                                String pathToRoot)
      throws IOException
   {
      return new HtmlPage(file,
                          pathToRoot,
                          getOutputDocEncoding(),
                          optionBaseUrl.getValue(),
                          getTargetDirectory());
   }

   private HtmlPage newHtmlPage(File file,
                                String pathToRoot,
                                String docType)
      throws IOException
   {
      return new HtmlPage(file,
                          pathToRoot,
                          getOutputDocEncoding(),
                          optionBaseUrl.getValue(),
                          getTargetDirectory(),
                          docType);
   }
}

/* gnu.classpath.tools.doclets.htmldoclet.CssClass
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

/**
 *  Represents a CSS (Cascading Stylesheet) class. Supports
 *  substituting <code>div</code> and <code>span</code> tags by more
 *  specialized HTML tags.
 */
public class CssClass
{
   public static final CssClass BODY_MENU_PACKAGES        = new CssClass("menu packages");
   public static final CssClass BODY_MENU_CLASSES         = new CssClass("menu classes");
   public static final CssClass BODY_CONTENT_CLASS        = new CssClass("content class");
   public static final CssClass BODY_CONTENT_DEPRECATED   = new CssClass("content deprecated");
   public static final CssClass BODY_CONTENT_PACKAGE      = new CssClass("content package");
   public static final CssClass BODY_CONTENT_OVERVIEW     = new CssClass("content overview");
   public static final CssClass BODY_CONTENT_ABOUT        = new CssClass("content about");
   public static final CssClass BODY_CONTENT_HELP         = new CssClass("content help");
   public static final CssClass BODY_CONTENT_PACKAGE_TREE = new CssClass("content packagetree");
   public static final CssClass BODY_CONTENT_FULL_TREE    = new CssClass("content fulltree");
   public static final CssClass BODY_CONTENT_INDEX        = new CssClass("content index");
   public static final CssClass BODY_CONTENT_USES         = new CssClass("content uses");
   public static final CssClass BODY_CONTENT_SOURCE       = new CssClass("content source");

   public static final CssClass OVERVIEW_TITLE = new CssClass("overview title", "h1");
   public static final CssClass OVERVIEW_SUMMARY = new CssClass("overview summary");
   public static final CssClass OVERVIEW_SUMMARY_LEFT = new CssClass("left");
   public static final CssClass OVERVIEW_SUMMARY_RIGHT = new CssClass("right");
   public static final CssClass OVERVIEW_DESCRIPTION_TOP = new CssClass("overview description top");
   public static final CssClass OVERVIEW_DESCRIPTION_FULL = new CssClass("overview description full");

   public static final CssClass DEPRECATION_TITLE = new CssClass("deprecation title", "h1");
   public static final CssClass DEPRECATION_SUMMARY = new CssClass("summary");
   public static final CssClass DEPRECATION_SUMMARY_LEFT = new CssClass("left");
   public static final CssClass DEPRECATION_SUMMARY_DESCRIPTION = new CssClass("description");
   public static final CssClass DEPRECATION_TOC = new CssClass("dep-toc");
   public static final CssClass DEPRECATION_TOC_HEADER = new CssClass("header", "h3");
   public static final CssClass DEPRECATION_TOC_LIST = new CssClass("list", "ul");
   public static final CssClass DEPRECATION_TOC_ENTRY = new CssClass("entry", "li");
   public static final CssClass DEPRECATION_EMPTY = new CssClass("dep-empty", "p");
   public static final CssClass DEPRECATION_LIST = new CssClass("dep-list", "div");

   public static final CssClass SERIALIZED_TITLE = new CssClass("serialized title", "h1");
   public static final CssClass SERIALIZED_PACKAGE_HEADER = new CssClass("serialized package header", "h2");
   public static final CssClass SERIALIZED_CLASS_HEADER = new CssClass("serialized class header", "h3");
   public static final CssClass SERIALIZED_SVUID_OUTER = new CssClass("serialized svuid outer", "p");
   public static final CssClass SERIALIZED_SVUID_HEADER = new CssClass("serialized svuid header", "b");
   public static final CssClass SERIALIZED_SVUID_VALUE = new CssClass("serialized svuid header");
   public static final CssClass SERIALIZED_SECTION = new CssClass("serialized section");
   public static final CssClass SERIALIZED_SECTION_HEADER = new CssClass("serialized section header", "h4");

   public static final CssClass PACKAGE_TITLE = new CssClass("package title", "h1");
   public static final CssClass PACKAGE_SUMMARY = new CssClass("package summary");
   public static final CssClass PACKAGE_SUMMARY_LEFT = new CssClass("left");
   public static final CssClass PACKAGE_SUMMARY_RIGHT = new CssClass("right");
   public static final CssClass PACKAGE_DESCRIPTION_TOP = new CssClass("package description top");
   public static final CssClass PACKAGE_DESCRIPTION_FULL = new CssClass("package description full");
   public static final CssClass PACKAGE_TREE_TITLE = new CssClass("package tree title", "h1");
   public static final CssClass PACKAGE_TREE_SECTION_TITLE = new CssClass("package tree section title", "h2");
   public static final CssClass PACKAGE_TREE = new CssClass("tree", "ul");

   public static final CssClass TREE_LINK = new CssClass("tree link", "b");

   public static final CssClass FULL_TREE_PACKAGELIST = new CssClass("fulltree package list", "dl");
   public static final CssClass FULL_TREE_PACKAGELIST_HEADER = new CssClass("fulltree package header", "dt", "b");
   public static final CssClass FULL_TREE_PACKAGELIST_ITEM = new CssClass("fulltree package item", "dd");

   public static final CssClass PACKAGE_MENU_LIST = new CssClass("package menu-list", "div");
   public static final CssClass PACKAGE_MENU_ENTRY = new CssClass("package menu-entry");
   public static final CssClass PACKAGE_MENU_TITLE = new CssClass("package menu-title", "h4");

   public static final CssClass CLASS_MENU_LIST = new CssClass("classes menu-list", "div");
   public static final CssClass CLASS_MENU_TITLE = new CssClass("classes menu-title", "h4");
   public static final CssClass CLASS_MENU_SUBTITLE = new CssClass("classes menu-subtitle", "p");
   public static final CssClass CLASS_MENU_ENTRY_CLASS = new CssClass("classes menu-entry class");
   public static final CssClass CLASS_MENU_ENTRY_INTERFACE = new CssClass("classes menu-entry interface", "i");

   public static final CssClass INDEX_TITLE = new CssClass("index title", "h1");
   public static final CssClass INDEX_CATEGORY = new CssClass("index category");
   public static final CssClass INDEX_CATEGORY_HEADER = new CssClass("index category header", "h2");
   public static final CssClass INDEX_ENTRY = new CssClass("index entry");
   public static final CssClass INDEX_ENTRY_DESCRIPTION = new CssClass("description");
   public static final CssClass INDEX_ENTRY_KEY = new CssClass("key");
   public static final CssClass INDEX_LETTERS = new CssClass("index letters");
   public static final CssClass INDEX_LETTER = new CssClass("index letter");
   public static final CssClass INDEX_LETTER_SPACER = new CssClass("index letter spacer");

   public static final CssClass CLASS_TITLE = new CssClass("class title outer");
   public static final CssClass CLASS_TITLE_PACKAGE = new CssClass("class title-package", "h3");
   public static final CssClass CLASS_TITLE_CLASS = new CssClass("class title-class", "h1");
   public static final CssClass CLASS_SUBCLASSES = new CssClass("class subclasses", "dl");
   public static final CssClass CLASS_SUBCLASSES_HEADER = new CssClass("class subclasses header", "dt", "b");
   public static final CssClass CLASS_SUBCLASSES_ITEM = new CssClass("class subclasses header", "dd");
   public static final CssClass CLASS_ENCLOSINGCLASS = new CssClass("class enclosing", "dl");
   public static final CssClass CLASS_ENCLOSINGCLASS_HEADER = new CssClass("class enclosing header", "dt", "b");
   public static final CssClass CLASS_ENCLOSINGCLASS_ITEM = new CssClass("class enclosing item", "dd");
   public static final CssClass CLASS_KNOWNIMPLEMENTING = new CssClass("class knownimplementing", "dl");
   public static final CssClass CLASS_KNOWNIMPLEMENTING_HEADER = new CssClass("header", "dt", "b");
   public static final CssClass CLASS_KNOWNIMPLEMENTING_ITEM = new CssClass("item", "dd");
   public static final CssClass CLASS_INHERITANCETREE = new CssClass("class inheritance-tree");
   public static final CssClass CLASS_SYNOPSIS = new CssClass("class synopsis outer");
   public static final CssClass CLASS_SYNOPSIS_NAME = new CssClass("class synopsis name", "b");
   public static final CssClass CLASS_SYNOPSIS_DECLARATION = new CssClass("class synopsis declaration", "div", "code");
   public static final CssClass CLASS_SYNOPSIS_SUPERCLASS = new CssClass("class synopsis superclass", "div", "code");
   public static final CssClass CLASS_SYNOPSIS_IMPLEMENTS = new CssClass("class synopsis implements", "div", "code");
   public static final CssClass CLASS_DESCRIPTION = new CssClass("class description");
   public static final CssClass CLASS_SUMMARY = new CssClass("class summary");
   public static final CssClass CLASS_SUMMARY_LEFT = new CssClass("left", new String[] { "valign" }, new String[] { "top" });
   public static final CssClass CLASS_SUMMARY_LEFT_SYNOPSIS = new CssClass("synopsis", "code");
   public static final CssClass CLASS_SUMMARY_RIGHT = new CssClass("right");
   public static final CssClass CLASS_SUMMARY_RIGHT_LIST = new CssClass("list", "dl");
   public static final CssClass CLASS_SUMMARY_RIGHT_SYNOPSIS = new CssClass("synopsis", "dt", "code");
   public static final CssClass CLASS_SUMMARY_RIGHT_DESCRIPTION = new CssClass("description", "dd");
   public static final CssClass CLASS_SUMMARY_INHERITED = new CssClass("inherited");
   public static final CssClass CLASS_SUMMARY_INHERITED_MEMBER = new CssClass("member", "code");
   public static final CssClass CLASS_BOILERPLATE = new CssClass("boilerplate", "pre", new String[] { "style" }, new String[] { "font-size: x-small;" });

   public static final CssClass USAGE_TITLE = new CssClass("usage title", "h1");
   public static final CssClass USAGE_PACKAGE_TITLE = new CssClass("usage package title", "h2");
   public static final CssClass USAGE_USAGE_TITLE = new CssClass("usage usage title", "h3");
   public static final CssClass USAGE_SUMMARY = new CssClass("usage summary");
   public static final CssClass USAGE_SUMMARY_LEFT = new CssClass("left");
   public static final CssClass USAGE_SUMMARY_RIGHT = new CssClass("right");
   public static final CssClass USAGE_SUMMARY_SYNOPSIS = new CssClass("synopsis");
   public static final CssClass USAGE_SUMMARY_DESCRIPTION = new CssClass("description");
   public static final CssClass USAGE_TABLE_HEADER = new CssClass("table header", "h3");
   public static final CssClass USAGE_EMPTY = new CssClass("usage empty", "p");

   public static final CssClass MEMBER_DETAIL = new CssClass("member detail outer");
   public static final CssClass MEMBER_DETAIL_NAME = new CssClass("member detail name", "h3");
   public static final CssClass MEMBER_DETAIL_BODY = new CssClass("member detail name", "blockquote");
   public static final CssClass MEMBER_DETAIL_SYNOPSIS = new CssClass("member detail synopsis", "pre");
   public static final CssClass MEMBER_DETAIL_DESCRIPTION = new CssClass("member detail description");
   public static final CssClass MEMBER_DETAIL_SPECIFIED_BY_LIST = new CssClass("member detail specified by list", "dl");
   public static final CssClass MEMBER_DETAIL_SPECIFIED_BY_HEADER = new CssClass("member detail specified by header", "dt", "b");
   public static final CssClass MEMBER_DETAIL_SPECIFIED_BY_ITEM = new CssClass("member detail specified by item", "dd");
   public static final CssClass MEMBER_DETAIL_OVERRIDDEN_LIST = new CssClass("member detail overridden list", "dl");
   public static final CssClass MEMBER_DETAIL_OVERRIDDEN_HEADER = new CssClass("member detail overridden header", "dt", "b");
   public static final CssClass MEMBER_DETAIL_OVERRIDDEN_ITEM = new CssClass("member detail overridden item", "dd");
   public static final CssClass MEMBER_DETAIL_PARAMETER_LIST = new CssClass("parameter", "div", "dl");
   public static final CssClass MEMBER_DETAIL_PARAMETER_HEADER = new CssClass("header", "dt", "b");
   public static final CssClass MEMBER_DETAIL_PARAMETER_ITEM = new CssClass("item", "dd");
   public static final CssClass MEMBER_DETAIL_PARAMETER_ITEM_NAME = new CssClass("name", "code");
   public static final CssClass MEMBER_DETAIL_PARAMETER_ITEM_SEPARATOR = new CssClass("separator");
   public static final CssClass MEMBER_DETAIL_PARAMETER_ITEM_DESCRIPTION = new CssClass("description");
   public static final CssClass MEMBER_DETAIL_RETURN_LIST = new CssClass("member detail return list", "div", "dl");
   public static final CssClass MEMBER_DETAIL_RETURN_HEADER = new CssClass("member detail return header", "dt", "b");
   public static final CssClass MEMBER_DETAIL_RETURN_ITEM = new CssClass("member detail return item", "dd");
   public static final CssClass MEMBER_DETAIL_THROWN_LIST = new CssClass("member detail thrown list", "div", "dl");
   public static final CssClass MEMBER_DETAIL_THROWN_HEADER = new CssClass("member detail thrown header", "dt", "b");
   public static final CssClass MEMBER_DETAIL_THROWN_ITEM = new CssClass("member detail thrown item", "dd");
   public static final CssClass MEMBER_DETAIL_THROWN_ITEM_NAME = new CssClass("name", "code");
   public static final CssClass MEMBER_DETAIL_THROWN_ITEM_SEPARATOR = new CssClass("separator");
   public static final CssClass MEMBER_DETAIL_THROWN_ITEM_DESCRIPTION = new CssClass("description");

   public static final CssClass TABLE_HEADER = new CssClass("table header", "h2");
   public static final CssClass TABLE_SUB_HEADER = new CssClass("table sub header", "h3");
   public static final CssClass TABLE_CONTAINER = new CssClass("table container", "dl", "dd");

   public static final CssClass SECTION = new CssClass("section", "div");
   public static final CssClass SECTION_HEADER = new CssClass("section header", "h2");

   public static final CssClass NAVBAR_TOP = new CssClass("navbar div top");
   public static final CssClass NAVBAR_TOP_NAVI = new CssClass("navi");
   public static final CssClass NAVBAR_TOP_HEADER = new CssClass("header", new String[] { "rowspan" }, new String[] { "2" });
   public static final CssClass NAVBAR_BOTTOM = new CssClass("navbar div bottom");
   public static final CssClass NAVBAR_BOTTOM_SPACER = new CssClass("navbar bottom spacer", "p");
   public static final CssClass NAVBAR_ITEM_ENABLED = new CssClass("navbar item enabled");
   public static final CssClass NAVBAR_ITEM_DISABLED = new CssClass("navbar item disabled");
   public static final CssClass NAVBAR_ITEM_ACTIVE = new CssClass("navbar item active");

   public static final CssClass TAGLET = new CssClass("taglet", "div");

   public static final CssClass ABOUT_TITLE = new CssClass("about title", "h1");
   public static final CssClass ABOUT_GENERATOR = new CssClass("about generator", "p");

   public static final CssClass SOURCE = new CssClass("source body");
   public static final CssClass SOURCE_TITLE = new CssClass("source title", "h1");

   public static final CssClass DEPRECATED = new CssClass("deprecated", "span");
   public static final CssClass DEPRECATED_INLINE = new CssClass("deprecated", "p");
   public static final CssClass DEPRECATED_HEADER = new CssClass("deprecated header", "b");
   public static final CssClass DEPRECATED_BODY = new CssClass("deprecated", "i");

   private String name;
   private String elementName;
   private String innerElementName;
   private String[] attributeNames;
   private String[] attributeValues;

   private CssClass(String name)
   {
      this(name, null);
   }

   private CssClass(String name, String elementName)
   {
      this(name, elementName, null);
   }

   private CssClass(String name, String elementName, String innerElementName)
   {
      this(name, elementName, innerElementName, null, null);
   }

   private CssClass(String name, String elementName, String[] attributeNames, String[] attributeValues)
   {
      this(name, null, null, attributeNames, attributeValues);
   }

   private CssClass(String name, String[] attributeNames, String[] attributeValues)
   {
      this(name, null, null, attributeNames, attributeValues);
   }

   private CssClass(String name, String elementName, String innerElementName, String[] attributeNames, String[] attributeValues)
   {
      this.name = name;
      this.elementName = elementName;
      this.innerElementName = innerElementName;
      this.attributeNames = attributeNames;
      this.attributeValues = attributeValues;
   }

   public String getSpanElementName()
   {
      if (null != this.elementName) {
         return this.elementName;
      }
      else {
         return "span";
      }
   }

   public String getDivElementName()
   {
      if (null != this.elementName) {
         return this.elementName;
      }
      else {
         return "div";
      }
   }

   public String getInnerElementName()
   {
      return this.innerElementName;
   }

   public String[] getAttributeNames()
   {
      return this.attributeNames;
   }

   public String[] getAttributeValues()
   {
      return this.attributeValues;
   }

   public String getName()
   {
      return name;
   }
}

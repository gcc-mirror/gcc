<?xml version="1.0" encoding="utf-8"?>

<!-- gjdoc_common.xsl
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
     02111-1307 USA.
     -->

<!-- Common templates for HTML generation.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:param name="gjdoc.outputfile.info" select="''"/>
  <xsl:param name="gjdoc.refdocs1" select="''"/>
  <xsl:param name="gjdoc.pathtoroot" select="''"/>
  <xsl:param name="gjdoc.xmldoclet.version" select="''"/>
  <xsl:param name="gjdoc.allowimages" select="0"/>
  <xsl:param name="gjdoc.option.nonavbar" select="''"/>
  <xsl:param name="gjdoc.option.noindex" select="''"/>
  <xsl:param name="gjdoc.option.notree" select="''"/>
  <xsl:param name="gjdoc.option.nohelp" select="''"/>
  <xsl:param name="gjdoc.option.nocomment" select="''"/>
  <xsl:param name="gjdoc.option.splitindex" select="''"/>
  <xsl:param name="gjdoc.option.linksource" select="''"/>
  <xsl:param name="gjdoc.option.windowtitle" select="''"/>
  <xsl:param name="gjdoc.option.helpfile" select="''"/>
  <xsl:param name="gjdoc.option.stylesheetfile" select="''"/>
  <xsl:param name="gjdoc.option.header" select="''"/>
  <xsl:param name="gjdoc.option.footer" select="''"/>
  <xsl:param name="gjdoc.option.bottom" select="''"/>
  <xsl:param name="gjdoc.option.doctitle" select="''"/>
  <xsl:param name="gjdoc.option.nodeprecatedlist" select="''"/>
  <xsl:param name="gjdoc.option.uses" select="''"/>
</xsl:stylesheet>

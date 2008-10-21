<?xml version="1.0" encoding="utf-8"?>

<!-- packagedoc.xsl
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

<!-- Creates the package descriptor files for HTML documentation. 
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="xml"
    encoding="utf-8"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="/">

    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="$gjdoc.outputfile.info"/>
        </xsl:call-template>
        <xsl:call-template name="include_common"/>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(getTitle())">

        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_curr_package" select="1"/>
          <xsl:with-param name="p_show_package_tree" select="1"/>
          <xsl:with-param name="p_top" select="1"/> 
        </xsl:call-template>

        <div class="pagebody">

        <h1 class="classdoc-title">Package <xsl:value-of select="$gjdoc.outputfile.info"/></h1>

        <div class="classdoc-package-comment-body">
          <xsl:apply-templates select="/gjdoc:rootdoc/gjdoc:packagedoc[@name=$gjdoc.outputfile.info]/gjdoc:firstSentenceTags/node()"/>
        </div>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isInterface]">
          <table border="1" cellspacing="0" width="100%" class="classdoc-table">
            <tr><td colspan="2" class="section-header"><div class="section-header">Interface Summary</div></td></tr>
            <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isInterface]">
              <xsl:sort select="@name" order="ascending"/>
              <xsl:call-template name="output-classes-summary"/>
            </xsl:for-each>
          </table>          
        </xsl:if>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isOrdinaryClass]">
          <table border="1" cellspacing="0" width="100%" class="classdoc-table">
            <tr><td colspan="2" class="section-header"><div class="section-header">Class Summary</div></td></tr>
            <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isOrdinaryClass]">
              <xsl:sort select="@name" order="ascending"/>
              <xsl:call-template name="output-classes-summary"/>
            </xsl:for-each>
          </table>
        </xsl:if>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isException]">
          <table border="1" cellspacing="0" width="100%" class="classdoc-table">
            <tr><td colspan="2" class="section-header"><div class="section-header">Exception Summary</div></td></tr>
            <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isException]">
              <xsl:sort select="@name" order="ascending"/>
              <xsl:call-template name="output-classes-summary"/>
            </xsl:for-each>
          </table>
        </xsl:if>

        <xsl:if test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isError]">
          <table border="1" cellspacing="0" width="100%" class="classdoc-table">
            <tr><td colspan="2" class="section-header"><div class="section-header">Error Summary</div></td></tr>
            <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:containingPackage/@name=$gjdoc.outputfile.info][gjdoc:isError]">
              <xsl:sort select="@name" order="ascending"/>
              <xsl:call-template name="output-classes-summary"/>
            </xsl:for-each>
          </table>
        </xsl:if>

        <a name="description"><p> </p></a>
        <h1 class="classdoc-sub-title package-description">Package <xsl:value-of select="$gjdoc.outputfile.info"/> Description</h1>

        <div class="classdoc-package-comment-body">
          <xsl:apply-templates select="/gjdoc:rootdoc/gjdoc:packagedoc[@name=$gjdoc.outputfile.info]/gjdoc:inlineTags/node()"/>
        </div>

      </div>

        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_package" select="0"/>
          <xsl:with-param name="p_curr_package" select="1"/>
          <xsl:with-param name="p_show_package_tree" select="1"/>
          <xsl:with-param name="p_top" select="0"/> 
        </xsl:call-template>
      </body>
    </html>

  </xsl:template>

  <xsl:template name="output-classes-summary">
    <xsl:variable name="v_currentclass" select="@qualifiedtypename"/>
    <xsl:variable name="v_sub_xml_filename" select="concat(@qualifiedtypename,'.xml')"/>
    <tr>
      <td class="class-link">
        <a href="{concat(@name, '.html')}" class="package-link">
          <xsl:value-of select="@name"/>
        </a>
      </td>
      <td class="class-summary-description">
        <xsl:variable name="v_docstring" select="document($v_sub_xml_filename,/gjdoc:rootdoc)/gjdoc:classdoc[@qualifiedtypename=$v_currentclass]/gjdoc:firstSentenceTags/node()"/>
        <xsl:choose>
          <xsl:when test="$v_docstring">
            <xsl:for-each select="$v_docstring">
              <xsl:value-of select="." disable-output-escaping="yes"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <i>No description available.</i>
          </xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>
  </xsl:template>
</xsl:stylesheet>

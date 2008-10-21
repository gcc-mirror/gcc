<?xml version="1.0" encoding="utf-8"?>

<!-- fulltree.xsl
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

<!-- Creates the fulltree.html file for HTML documentation. 
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

  <xsl:template name="output_tree_recursive">
    <xsl:param name="p_level" select="0"/>
    
    <xsl:value-of select="gjdoc:containingPackage/@name"/><xsl:text>.</xsl:text><a class="tree-class-link" href="{concat($gjdoc.pathtoroot,translate(gjdoc:containingPackage/@name,'.','/'),'/',@name,'.html')}"><xsl:value-of select="@name"/></a> 
    <xsl:if test="gjdoc:implements">
      <xsl:text> (implements </xsl:text>
      <xsl:for-each select="gjdoc:implements">
        <xsl:sort select="@qualifiedtypename" order="ascending"/>
        <xsl:choose>
          <xsl:when test="string-length($gjdoc.outputfile.info)=0">
            <xsl:call-template name="link_to_class_full">
              <xsl:with-param name="p_name" select="@typename"/>
              <xsl:with-param name="p_qualifiedname" select="@qualifiedtypename"/>
            </xsl:call-template>            
          </xsl:when>
          <xsl:when test="gjdoc:containingPackage/@name=$gjdoc.outputfile.info">
            <xsl:call-template name="link_to_class">
              <xsl:with-param name="p_name" select="@typename"/>
              <xsl:with-param name="p_qualifiedname" select="@qualifiedtypename"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="link_to_class_full">
              <xsl:with-param name="p_name" select="@typename"/>
              <xsl:with-param name="p_qualifiedname" select="@qualifiedtypename"/>
            </xsl:call-template>            
          </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="position() != last()">
          <xsl:text>, </xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>)</xsl:text>
    </xsl:if>

    <xsl:variable name="v_qualifiedname">
      <xsl:value-of select="@qualifiedtypename"/>
    </xsl:variable>
    <xsl:variable name="v_result">
    <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:superclass/@qualifiedtypename=$v_qualifiedname]">
      <xsl:sort select="@qualifiedtypename" order="ascending"/>
      <xsl:variable name="v_is_used">
        <xsl:choose>
          <xsl:when test="string-length($gjdoc.outputfile.info)=0">
            <xsl:value-of select="true()"/>            
          </xsl:when>
          <xsl:when test="gjdoc:containingPackage/@name=$gjdoc.outputfile.info">
            <xsl:value-of select="true()"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="is_class_parent_of_package_class">
              <xsl:with-param name="p_package" select="$gjdoc.outputfile.info"/>
              <xsl:with-param name="p_qualifiedtypename" select="@qualifiedtypename"/>
            </xsl:call-template>            
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:if test="string-length($v_is_used)>0">
        <li class="tree">
          <xsl:call-template name="output_tree_recursive">
            <xsl:with-param name="p_level"><xsl:value-of select="$p_level+1"/></xsl:with-param>
          </xsl:call-template>
        </li>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <xsl:if test="string-length($v_result)>0">
    <ul><xsl:copy-of select="$v_result"/></ul>
  </xsl:if>
  </xsl:template>

  <xsl:template name="is_class_parent_of_package_class">
    <xsl:param name="p_package"/>
    <xsl:param name="p_qualifiedtypename"/>

    <xsl:choose>
      
      <xsl:when test="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:superclass/@qualifiedtypename=$p_qualifiedtypename and gjdoc:containingPackage/@name=$p_package]">
        <xsl:value-of select="true()"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[gjdoc:superclass/@qualifiedtypename=$p_qualifiedtypename]">
          <xsl:call-template name="is_class_parent_of_package_class">
            <xsl:with-param name="p_package" select="$p_package"/>
            <xsl:with-param name="p_qualifiedtypename" select="@qualifiedtypename"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/">

    <html>
      <head>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="'Full Tree'"/>
        </xsl:call-template>
        <xsl:call-template name="include_common"/>
      </head>
      <body class="classdoc" onload="if(parent.contentPageLoaded)parent.contentPageLoaded(document.title)">
        <div class="pagebody">

        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="string-length($gjdoc.outputfile.info)&gt;0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_curr_package_tree" select="string-length($gjdoc.outputfile.info)&gt;0"/>
          <xsl:with-param name="p_show_full_tree" select="string-length($gjdoc.outputfile.info)&gt;0"/>
          <xsl:with-param name="p_curr_full_tree" select="string-length($gjdoc.outputfile.info)&lt;=0"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_top" select="1"/> 
        </xsl:call-template>

        <h1 class="classdoc-title">
          <xsl:choose>
            <xsl:when test="string-length($gjdoc.outputfile.info)=0">
              Full Class Tree
              <xsl:if test="/gjdoc:rootdoc/gjdoc:title">
                <xsl:text> for </xsl:text>
                <xsl:value-of select="/gjdoc:rootdoc/gjdoc:title"/>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              Class Hierarchy for Package <xsl:value-of select="$gjdoc.outputfile.info"/>
            </xsl:otherwise>
          </xsl:choose>
        </h1>

        <xsl:for-each select="/gjdoc:rootdoc/gjdoc:classdoc[@qualifiedtypename='java.lang.Object']">
          <ul class="tree">
            <li class="tree">
              <xsl:call-template name="output_tree_recursive"/>
            </li>
          </ul>
        </xsl:for-each>


        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="string-length($gjdoc.outputfile.info)&lt;=0"/>
          <xsl:with-param name="p_show_package_tree" select="0"/>
          <xsl:with-param name="p_curr_package_tree" select="string-length($gjdoc.outputfile.info)&gt;0"/>
          <xsl:with-param name="p_show_full_tree" select="string-length($gjdoc.outputfile.info)&gt;0"/>
          <xsl:with-param name="p_curr_full_tree" select="string-length($gjdoc.outputfile.info)&lt;=0"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/>
          <xsl:with-param name="p_top" select="0"/> 
        </xsl:call-template>
        </div>
      </body>
    </html>
    
  </xsl:template>
</xsl:stylesheet>

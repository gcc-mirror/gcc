<?xml version="1.0" encoding="utf-8"?>

<!-- classdoc-uses.xsl
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
     02111-1307 USA.
     -->

<!-- Outputs an HTML file showing the source code for the class
     designated by $gjdoc.outputfile.info.
     -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gjdoc="http://www.gnu.org/software/cp-tools/gjdocxml"
  xmlns:html="http://www.w3.org/TR/REC-html40"
  xmlns="http://www.w3.org/TR/REC-html40">

  <xsl:include href="html_common.xsl"/>

  <xsl:output method="html"
    encoding="utf-8"
    doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
    doctype-system="http://www.w3.org/TR/html4/loose.dtd"
    indent="no"/>

  <xsl:strip-space elements="*"/>

  <xsl:template match="gjdoc:referencing-package">
    <h2 class="classdoc-header">
      Uses in package
      <xsl:value-of select="@name"/>
    </h2>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="gjdoc:user">
    <dt>
      <xsl:choose>
        <xsl:when test="@field">
          <a href="{concat($gjdoc.pathtoroot, translate(ancestor::gjdoc:referencing-package/@name, '.', '/'), '/', @class, '.html#', @field)}">
            <xsl:value-of select="concat(@class, '.', @field)"/>
          </a>
        </xsl:when>
        <xsl:when test="@method">
          <a href="{concat($gjdoc.pathtoroot, translate(ancestor::gjdoc:referencing-package/@name, '.', '/'), '/', @class, '.html#', @method, @signature)}">
            <xsl:value-of select="concat(@class, '.', @method, @flatSignature)"/>
          </a>
        </xsl:when>
        <xsl:when test="@class">
          <a href="{concat($gjdoc.pathtoroot, translate(ancestor::gjdoc:referencing-package/@name, '.', '/'), '/', @class, '.html')}">
            <xsl:value-of select="@class"/>
          </a>
        </xsl:when>
      </xsl:choose>
    </dt>
  </xsl:template>

  <xsl:template match="gjdoc:usage-type">

    <h3 class="classdoc-header">
      <xsl:variable name="v_qualifiedtypename"
        select="ancestor::gjdoc:classdoc/@qualifiedtypename"/>
      <xsl:variable name="v_packagename"
        select="../@name"/>
      <xsl:choose>
        <xsl:when test="@id='class-derived-from'">
          <xsl:text>Classes in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> derived from </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='field-of-type'">
          <xsl:text>Fields in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> of type </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='method-with-return-type'">
          <xsl:text>Methods in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> returning </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='method-with-parameter-type'">
          <xsl:text>Methods in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> accepting parameters of type </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='method-with-thrown-type'">
          <xsl:text>Methods in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> throwing </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='constructor-with-parameter-type'">
          <xsl:text>Constructors in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> accepting parameters of type </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:when test="@id='constructor-with-thrown-type'">
          <xsl:text>Constructors in </xsl:text><xsl:value-of select="$v_packagename"/><xsl:text> throwing </xsl:text><xsl:value-of select="$v_qualifiedtypename"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>Unknown ID </xsl:text><xsl:value-of select="@id"/>
        </xsl:otherwise>
      </xsl:choose>
    </h3>
    <dl>
      <xsl:apply-templates/>
    </dl>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <xsl:call-template name="include_common"/>
        <xsl:call-template name="output_title">
          <xsl:with-param name="p_pagetitle" select="concat(ancestor::gjdoc:classdoc/@name, ' Class Usage')"/>
        </xsl:call-template>
      </head>
      <body>
        <div class="pagebody">

        <!-- Top Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="1"/>
          <xsl:with-param name="p_show_package_tree" select="1"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/> 
          <xsl:with-param name="p_top" select="1"/> 
          <xsl:with-param name="p_show_source" select="concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_show_class" select="concat($gjdoc.pathtoroot, translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_curr_uses" select="1"/>
        </xsl:call-template>
        
        <xsl:for-each select="document(concat($gjdoc.outputfile.info, '.xml'),/gjdoc:rootdoc)/gjdoc:classdoc/gjdoc:references">

          <h1 class="classdoc-header">
            Uses of class
            <xsl:value-of select="ancestor::gjdoc:classdoc/@qualifiedtypename"/>
            <xsl:apply-templates/>
          </h1>
        </xsl:for-each>

        <!-- Bottom Navigation Bar -->
        <xsl:call-template name="output_navbar">
          <xsl:with-param name="p_show_frames" select="1"/>
          <xsl:with-param name="p_show_noframes" select="1"/>
          <xsl:with-param name="p_show_package" select="1"/>
          <xsl:with-param name="p_show_package_tree" select="1"/>
          <xsl:with-param name="p_show_full_tree" select="1"/>
          <xsl:with-param name="p_show_index" select="1"/>
          <xsl:with-param name="p_show_help" select="1"/> 
          <xsl:with-param name="p_top" select="0"/> 
          <xsl:with-param name="p_show_source" select="concat($gjdoc.pathtoroot, 'src-html/', translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_show_class" select="concat($gjdoc.pathtoroot, translate(gjdoc:containingPackage/@name, '.', '/'), '/', @name, '.html')"/>
          <xsl:with-param name="p_curr_uses" select="1"/>
        </xsl:call-template>

        </div>
      </body>
    </html>
  
  </xsl:template>

</xsl:stylesheet>

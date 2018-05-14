<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<xsl:param name="DIR_SEP" select="'/'"/>

  <xsl:template match="/">
    <!--note, <job> is the top-level element after definition
        and job files have been merged-->
    <xsl:apply-templates select="job"/>
  </xsl:template>
  
  <xsl:template match="job">
   <xsl:apply-templates select="vars"/>
   <xsl:apply-templates select="config"/>
  </xsl:template>
  
  <xsl:template match="vars">
   <xsl:apply-templates select="var"/>
  </xsl:template>

  <xsl:template match="config">
    <xsl:text>MODULE_NAMES='</xsl:text>
      <xsl:for-each select="model">
        <xsl:value-of select="@name"/>
        <xsl:text> </xsl:text>	
      </xsl:for-each>
    <xsl:text>'</xsl:text>
  </xsl:template>

  <xsl:template match="var">
<!--start of test for glimmer vars-->
   <xsl:variable name="outputVar">
    <xsl:choose>
     <xsl:when test="@name='GLIMMER_MASTER_CONFIG'"><!-- or ... -->
      <xsl:choose>
       <xsl:when test="/job/config/model/@name='glimmer'">
        <xsl:text>true</xsl:text>
       </xsl:when>
       <xsl:otherwise>
        <xsl:text>false</xsl:text>
       </xsl:otherwise>
      </xsl:choose>
     </xsl:when>
     <xsl:otherwise> 
      <xsl:text>true</xsl:text>
     </xsl:otherwise> 
    </xsl:choose>
   </xsl:variable>
<!--end of test for glimmer-->
   <xsl:if test="$outputVar='true'">
    <xsl:value-of select="normalize-space(@name)"/>
    <xsl:text>=</xsl:text>
     <xsl:for-each select="node()">
      <xsl:choose>
       <xsl:when test="local-name()='varref'">
        <xsl:value-of select="normalize-space(/job/vars/var[@name=normalize-space(current()/text())]/text())"/>
       </xsl:when>
       <xsl:when test="local-name()='sep'">
        <xsl:value-of select="$DIR_SEP"/>
       </xsl:when>
       <xsl:otherwise>
        <xsl:value-of select="normalize-space(.)"/>
       </xsl:otherwise>
      </xsl:choose>
     </xsl:for-each>
    <xsl:text>&#xa;</xsl:text>
   </xsl:if>
  </xsl:template>

</xsl:stylesheet>

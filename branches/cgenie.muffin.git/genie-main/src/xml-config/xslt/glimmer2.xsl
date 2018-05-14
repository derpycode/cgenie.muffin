<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<xsl:param name="DIR_SEP" select="'/'"/>

<!-- the <config> version -->
<!--
<xsl:template match="config/model">
  <xsl:choose>
    <xsl:when test="@name='glimmer'">
     <xsl:apply-templates select="param" mode="glimmer"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>
-->

<!-- the <parameters> version -->
<xsl:template match="parameters/model">
  <xsl:choose>
    <xsl:when test="@name='glimmer'">
     <xsl:apply-templates select="param" mode="glimmer"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="param" mode="glimmer">
 <xsl:text>glimmer_</xsl:text>
 <xsl:value-of select="@name"/>
 <xsl:text> = </xsl:text>
 <xsl:apply-templates select="value" mode="glimmer"/>
</xsl:template>

<xsl:template match="value" mode="glimmer">
 <xsl:for-each select="node()">
  <xsl:choose>
   <xsl:when test="local-name()='varref'">
    <xsl:choose>
     <xsl:when test="/job/vars/var[@name=normalize-space(current()/text())]">
      <xsl:value-of select="/job/vars/var[@name=normalize-space(current()/text())]/text()"/>
     </xsl:when>
     <xsl:when test="/job/testing/var[@name=normalize-space(current()/text())]">
      <xsl:call-template name="outputMixed">
       <xsl:with-param name="newVal" select="/job/testing/var[@name=normalize-space(current()/text())]"/>
      </xsl:call-template>
     </xsl:when>
     <xsl:otherwise>
      <xsl:message terminate="yes">
       <xsl:text>Error: toNml.xsl, cannot find the value of the variable </xsl:text><xsl:value-of select="."/>
      </xsl:message>
     </xsl:otherwise>
    </xsl:choose>
   </xsl:when>
   <xsl:when test="local-name()='sep'">
    <xsl:value-of select="$DIR_SEP"/>
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="normalize-space(.)"/>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:for-each>
</xsl:template>

<xsl:template match="node()">
 <xsl:apply-templates/>
</xsl:template>

<xsl:template name="outputMixed">
 <xsl:param name="newVal"/>
    <xsl:for-each select="$newVal">
     <xsl:choose>
      <xsl:when test="local-name()='varref'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:when test="local-name()='sep'">
       <xsl:apply-templates select="."/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="."/>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:for-each>
</xsl:template>


</xsl:stylesheet>

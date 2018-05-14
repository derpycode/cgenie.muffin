<?xml version="1.0" encoding="utf-8"?>
<!-- Stylesheet for the auto-generation of schemas describing the structure
and content of configuration metadata for modules of the GENIE framework.

Copyright 2008 GENIE Project, University of Southampton
Andrew Price, $Date$
$Revision$
GENIE Toolbox for Matlab -->
<xsl:stylesheet version="1.0"
				xmlns="http://www.genie.ac.uk/schema"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:xs="http://www.w3.org/2001/XMLSchema"
				xmlns:xdb="http://xmlns.oracle.com/xdb"
				xmlns:xalan="org.apache.xalan.xslt.extensions.Redirect"
				xmlns:common="http://exslt.org/common"
				extension-element-prefixes="common xalan">

	<xsl:include href="complexType.xsl"/>

	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xsl:param name="XSD_DIR" select="'.'"/>
	<xsl:param name="DIR_SEP" select="'/'"/>
	<xsl:param name="date" select="'date unknown'"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="parameters">
		<xsl:for-each select="*">
			<xsl:call-template name="parameters.file">
				<xsl:with-param name="modelname">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameters.file">
		<xsl:param name="modelname"/>
		<xsl:for-each select="*">
			<xsl:call-template name="parameters.file.namelist">
				<xsl:with-param name="modelname">
					<xsl:value-of select="$modelname"/>
				</xsl:with-param>
				<xsl:with-param name="nmlname">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameters.file.namelist">
		<xsl:param name="modelname"/>
		<xsl:param name="nmlname"/>
		<xsl:for-each select="*">
			<xsl:choose>
				<xsl:when test="element-available('common:document')">
					<common:document href="{concat($XSD_DIR,$DIR_SEP,$modelname,'.',$nmlname,'.',local-name(),'.xsd')}" method="xml">
						<xsl:call-template name="type">
							<xsl:with-param name="level">-1</xsl:with-param>
							<xsl:with-param name="typename">
								<xsl:value-of select="$modelname"/>
								<xsl:text>.</xsl:text>
								<xsl:value-of select="$nmlname"/>
								<xsl:text>.</xsl:text>
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</common:document>
				</xsl:when>
				<xsl:when test="element-available('xalan:write')">
					<xalan:open file="{concat($XSD_DIR,$DIR_SEP,$modelname,'.',$nmlname,'.',local-name(),'.xsd')}" append="true"/>
					<xalan:write file="{concat($XSD_DIR,$DIR_SEP,$modelname,'.',$nmlname,'.',local-name(),'.xsd')}" append="true">
						<xsl:call-template name="type">
							<xsl:with-param name="level">-1</xsl:with-param>
							<xsl:with-param name="typename">
								<xsl:value-of select="$modelname"/>
								<xsl:text>.</xsl:text>
								<xsl:value-of select="$nmlname"/>
								<xsl:text>.</xsl:text>
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xalan:write>
					<xalan:close file="{concat($XSD_DIR,$DIR_SEP,$modelname,'.',$nmlname,'.',local-name(),'.xsd')}"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:message terminate="yes">
						<xsl:text>No file output support found</xsl:text>
					</xsl:message>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="type">
		<xsl:param name="addAnyElement">false</xsl:param>
		<xsl:param name="level"/>
		<xsl:param name="typename"/>
		<xsl:element name="xs:schema">
			<xsl:attribute name="targetNamespace">http://www.genie.ac.uk/schema</xsl:attribute>
			<xsl:attribute name="xdb:storeVarrayAsTable">true</xsl:attribute>
			<xsl:attribute name="elementFormDefault">qualified</xsl:attribute>
			<xsl:element name="xs:annotation">
				<xsl:element name="xs:documentation">
					Auto-generated schema for module <xsl:value-of select="$typename"/>. Created by $Revision$ of parameters2types.xsl on <xsl:value-of select="$date"/>.
				</xsl:element>
			</xsl:element>
			<xsl:element name="xs:complexType">
				<xsl:attribute name="name">
					<xsl:value-of select="$typename"/>
				</xsl:attribute>
				<xsl:element name="xs:choice">
					<xsl:for-each select="*">
						<xsl:call-template name="complexType">
							<xsl:with-param name="level">
								<xsl:value-of select="$level+1"/>
							</xsl:with-param>
							<xsl:with-param name="typename">
								<xsl:value-of select="local-name()"/>
							</xsl:with-param>
						</xsl:call-template>
					</xsl:for-each>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">idx</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">type</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
				<xsl:element name="xs:attribute">
					<xsl:attribute name="name">size</xsl:attribute>
					<xsl:attribute name="type">xs:string</xsl:attribute>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>

</xsl:stylesheet>

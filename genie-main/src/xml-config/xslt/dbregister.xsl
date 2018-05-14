<?xml version="1.0" encoding="utf-8"?>
<!-- Stylesheet for the auto-generation of the script that registers the
schemas in the database.

Copyright 2008 GENIE Project, University of Southampton
Andrew Price, $Date$
$Revision$
GENIE Toolbox for Matlab -->
<xsl:stylesheet version="1.0"
				xmlns="http://www.genie.ac.uk/schema"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="text"/>

	<xsl:param name="compositeFile">genie-composite.xsd</xsl:param>
	<xsl:param name="date" select="'date unknown'"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/">
		<xsl:text disable-output-escaping="yes">-- Script to register the database schema for the GENIE data management system
--
-- Copyright 2005-2008 GENIE project, University of Southampton
-- Andrew Price, $Date$
-- $Revision$, GENIE Toolbox for Matlab
--
-- Instance generated on </xsl:text>
		<xsl:value-of select="$date"/>
		<xsl:text>
--
-- This script is designed to be run from an account with SYSDBA privilege

-- Create a directory in the database to stage the XML files
CREATE OR REPLACE DIRECTORY xmldir AS '/path/to/XML/';

-- Change the schema so that the XML is registered under the correct user
ALTER SESSION SET CURRENT_SCHEMA = %USR_SCHEMA%;

-- Register the XML schemas
declare
        PROCEDURE registerXML(schema_ref VARCHAR2, xml_file VARCHAR2) AS
          l_bfile bfile;
        BEGIN
          l_bfile := bfilename('XMLDIR', xml_file);
          dbms_lob.open(l_bfile);
          dbms_xmlschema.registerSchema(schema_ref, l_bfile);
          dbms_lob.close(l_bfile);
        END;
begin</xsl:text>
		<xsl:apply-templates/>
		<xsl:text disable-output-escaping="yes">
        registerXML('http://www.genie.ac.uk/schema/dif_v9.4.xsd', 'dif_v9.4.xsd');
        registerXML('http://www.genie.ac.uk/schema/genie-datatypes.xsd', 'genie-datatypes.xsd');
        registerXML('http://www.genie.ac.uk/schema/</xsl:text>
		<xsl:value-of select="$compositeFile"/>
		<xsl:text>', '</xsl:text>
		<xsl:value-of select="$compositeFile"/>
		<xsl:text>');
end;
/
		</xsl:text>
	</xsl:template>

	<xsl:template match="parameters">
		<xsl:for-each select="*">
			<xsl:call-template name="parameters.model">
				<xsl:with-param name="modelname">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameters.model">
		<xsl:param name="modelname"/>
		<xsl:for-each select="*">
			<xsl:call-template name="parameters.model.file">
				<xsl:with-param name="modelname">
					<xsl:value-of select="$modelname"/>
				</xsl:with-param>
				<xsl:with-param name="filename">
					<xsl:value-of select="local-name()"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="parameters.model.file">
		<xsl:param name="modelname"/>
		<xsl:param name="filename"/>
		<xsl:for-each select="*">
			<xsl:variable name="xsdname">
				<xsl:value-of select="$modelname"/>
				<xsl:text>.</xsl:text>
				<xsl:value-of select="$filename"/>
				<xsl:text>.</xsl:text>
				<xsl:value-of select="local-name()"/>
			</xsl:variable>
			<xsl:text>
        registerXML('http://www.genie.ac.uk/schema/</xsl:text>
			<xsl:value-of select="$xsdname"/>
			<xsl:text>.xsd', '</xsl:text>
			<xsl:value-of select="$xsdname"/>
			<xsl:text>.xsd');</xsl:text>
		</xsl:for-each>
	</xsl:template>

</xsl:stylesheet>
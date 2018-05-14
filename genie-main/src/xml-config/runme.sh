#!/bin/bash

if [ -z $1 ]
then
    echo "Usage: $0 <full path of xml configuration file>"
    exit 1
elif [ ! -f $1 ]
then
    echo "Error: $1 file not found"
    exit 1
else
    xsltproc -stringparam user_xml ../$1 xslt/merge.xsl xml/definition.xml > merged.xml
    xsltproc xslt/toNml.xsl merged.xml > /dev/null
    xsltproc xslt/vars.xsl merged.xml > vars.sh
    xsltproc xslt/testing.xsl merged.xml > testing.sh
    xsltproc xslt/build.xsl merged.xml > build.sh
#    xsltproc xslt/glimmer2.xsl merged.xml > glimmer.sh
      # xsltproc xslt/toMatlab.xsl merged.xml > merged-matlab.xml
      # In Matlab:
      #    >> xmlstr=fileread('merged-matlab.xml');
      #    >> job=xml_parse(xmlstr);
      #    >> job.parameters.control.data_genie.GENIE_CONTROL_NML.koverall_total=720;
      #    >> fid=fopen('merged-matlab-new.xml');
      #    >> fprintf(fid, '%s', job);
      #    >> fclose(fid);
      # xsltproc xslt/fromMatlab.xsl merged-matlab-new.xml > merged-new.xml
      # xsltproc xslt/toNml.xsl merged-new.xml
    #rm -f merged.xml
fi

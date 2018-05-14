#!/usr/local/bin/python
#
# $Id$
#
'''A program to compare two NetCDF files.'''

import sys
import types
from sets import Set
import os.path
import cdms
import MV

# define useful globals
TRUE,FALSE=1,0

def simpleComp(fileA, fileB, tol=0.0):
    '''A simple comparison function.

       Attribute names and values are compared first.
       Then the data arrays are compared within a
       tolerance on a cell by cell basis.'''
    
    # open files
    fpA = cdms.open(fileA)
    fpB = cdms.open(fileB)

    # == global attributes ==
    # compare attribute names
    message = 'global attributes: '
    globalNamesA = Set(fpA.listglobal())
    globalNamesB = Set(fpB.listglobal())
    symmetricDiff = globalNamesA ^ globalNamesB
    if len(symmetricDiff) != 0:
        (detailA, detailB) = setDiff(globalNamesA,globalNamesB,symmetricDiff)
        return (FALSE,message,detailA,detailB)
    # compare values
    for globalName in globalNamesA:
        # limit our checks to attributes with string values
        if isinstance(eval(r'fpA.'+globalName),types.StringType):
            globalValueA = eval(r'fpA.'+globalName)
            globalValueB = eval(r'fpB.'+globalName)
            if globalValueA != globalValueB:
                message += globalName + ' values'
                return(FALSE,message,globalValueA,globalValueB)

    # == dimensions ==
    # compare dimension names
    dimNamesA = Set(fpA.listdimension())
    dimNamesB = Set(fpB.listdimension())
    symmetricDiff = dimNamesA ^ dimNamesB
    if len(symmetricDiff) != 0:
        message = 'dimensions:'
        (detailA, detailB) = setDiff(dimNamesA,dimNamesB,symmetricDiff)
        return (FALSE,message,detailA,detailB)
    # loop over dimensions
    for dimName in dimNamesA:
        message = 'dimensions: '+ dimName
        # compare attribute names
        dimAttNamesA = Set(fpA[dimName].attributes.keys())
        dimAttNamesB = Set(fpA[dimName].attributes.keys())
        symmetricDiff = dimAttNamesA ^ dimAttNamesB
        if len(symmetricDiff) != 0:
            (detailA, detailB) = setDiff(dimAttNamesA,dimAttNamesB,symmetricDiff)
            return (FALSE,message,detailA,detailB)
        # compare attribute values
        for dimAttName in dimAttNamesA:
            # assuming objects we can compare
            dimAttValueA = eval(r"fpA['"+dimName+r"']."+dimAttName)
            dimAttValueB = eval(r"fpB['"+dimName+r"']."+dimAttName)
            if dimAttValueA != dimAttValueB:
                message += ': '+dimAttName
                return (FALSE,message,dimAttValueA,dimAttValueB)
        # compare data
        dimDataShapeA = MV.shape(fpA[dimName])
        dimDataShapeB = MV.shape(fpB[dimName])
        if dimDataShapeA != dimDataShapeB:
            message += ': data array shape'
            return (FALSE,message,str(dimDataShapeA),str(dimDataShapeB))
        maxDelta = MV.maximum(abs(fpA[dimName][:] - fpB[dimName][:]))
        if maxDelta > tol:
            message += ': delta: '+str(maxDelta)+' > '+str(tol)
            return (FALSE,message,'','')

    # == variables ==
    # compare variable names
    varNamesA = Set(fpA.listvariables())
    varNamesB = Set(fpB.listvariables())
    symmetricDiff = varNamesA ^ varNamesB
    if len(symmetricDiff) != 0:
        message = 'variables:'
        (detailA, detailB) = setDiff(varNamesA,varNamesB,symmetricDiff)
        return (FALSE,message,detailA,detailB)
    # loop over variables
    for varName in varNamesA:
        message = 'variables: '+varName
        # compare attribute names
        varAttNamesA = Set(fpA[varName].attributes.keys())
        varAttNamesB = Set(fpA[varName].attributes.keys())
        symmetricDiff = varAttNamesA ^ varAttNamesB
        if len(symmetricDiff) != 0:
            (detailA, detailB) = setDiff(varAttNamesA,varAttNamesB,symmetricDiff)
            return (FALSE,message,detailA,detailB)
        # compare attribute values
        for varAttName in varAttNamesA:
            # assuming objects we can compare
            varAttValueA = eval(r"fpA['"+varName+r"']."+varAttName)
            varAttValueB = eval(r"fpB['"+varName+r"']."+varAttName)
            if varAttValueA != varAttValueB:
                message += ': '+varAttName
                return (FALSE,message,varAttValueA,varAttValueB)
        # compare data
        varDataShapeA = MV.shape(fpA[varName])
        varDataShapeB = MV.shape(fpB[varName])
        if varDataShapeA != varDataShapeB:
            message += ': data array shape'
            return (FALSE,message,str(varDataShapeA),str(varDataShapeB))
        maxDelta = MV.maximum(abs(fpA[varName][:] - fpB[varName][:]))
        if maxDelta > tol:
            message += ': delta: '+str(maxDelta)+' > '+str(tol)
            return (FALSE,message,'','')

    # close files
    fpA.close()
    fpB.close()
    return (TRUE,'','','')

def setDiff(setA, setB, symmetricDiff):
    detailA = setA & symmetricDiff
    detailB = setB & symmetricDiff
    return (detailA,detailB)

def compVarSlice(fileA, fileB, var, dim, tol=0.0, start=0, end=0):
    '''Compare a slice (of given dimension) through named variable'''

    # open files
    fpA = cdms.open(fileA)
    fpB = cdms.open(fileB)
    
    # check named variable present in both files
    varsA = Set(fpA.listvariables())
    varsB = Set(fpB.listvariables())
    commonVars = varsA & varsB
    if var not in commonVars:
        fpA.close()
        fpB.close()
        return (FALSE,var+' not common',varsA,varsB)

    # ditto for named dimension
    dimsA = Set(fpA.listdimension())
    dimsB = Set(fpB.listdimension())
    commonDims = dimsA & dimsB
    if dim not in commonDims:
        fpA.close()
        fpB.close()
        return (FALSE,dim+' not common',dimsA,dimsB)

    # get the slices
    sliceA = eval(r"fpA('"+var+"',"+dim+"=slice("+str(start)+","+str(end)+"))")
    sliceB = eval(r"fpB('"+var+"',"+dim+"=slice("+str(start)+","+str(end)+"))")

    # close files
    fpA.close()
    fpB.close()

    # ensure dimensions of slices correct
    if sliceA.shape != sliceB.shape:
        return (FALSE,'different shapes',sliceA.shape,sliceB.shape)
    if sliceA.shape[0] != end - start:
        return (FALSE,'slice size wrong',str(sliceA.shape[0]),str(end-start))
    if sliceA.shape[0] == 0:
        return (FALSE,'slice size zero',str(sliceA.shape[0]),str(end-start))

    # make actual comparison
    maxDelta = MV.maximum(abs(sliceA - sliceB))
    if maxDelta > tol:
        return (FALSE,'max diff > '+str(tol),'','')
    else:
        return (TRUE,'','','')

    # could take difference of the averages too
    
if __name__ == '__main__':

    tol = 0.0
    
    # parse command line
    numArgs = len(sys.argv)
    if numArgs < 3 or numArgs > 4:
        print 'usage: fileA.nc fileB.nc tol'
        sys.exit(1)
    fileA = sys.argv[1]
    fileB = sys.argv[2]
    if numArgs == 4:
        tol = float(sys.argv[3])

    # make sure files exist!
    if not os.path.isfile(fileA):
        print "does not exist: %s" % fileA
        sys.exit(1)
    if not os.path.isfile(fileB):
        print "does not exist: %s" % fileB
        sys.exit(1)

    # make the comparison
    (status,message,detailA,detailB) = simpleComp(fileA,fileB,tol)

    # report
    if status == FALSE:
        print "files differ."
        print "%s" % message
        print "fileA: %s" % detailA
        print "fileB: %s" % detailB
        sys.exit(1)


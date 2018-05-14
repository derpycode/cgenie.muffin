#!/usr/bin/python

#$Id$

import sys
import os.path
import re

valid_ext_list = ['.f','.F','.f90']

def find_includes(filename):
    '''returns a list of included files found in some Fortran source

    opens the given file, scans each line using a regular expression,
    appends to a list of included files if required, closes the file.'''

    # list of files (common blocks etc.) 'included' in source
    filelist = []
    
    f = open(filename)                 # open the filehandle
    while 1:
        line = f.readline()            # read the next line
        if not line: break             # break loop on EOF
        line = line.strip()            # strip '\n'
        if line == '': continue        # ignore blank lines
        
        # match lines we're interested in with a regular expression
        # first compile the regexp
        p = re.compile(r'#?include\s*[\'\"](\w*\.\w*)[\'\"]', re.IGNORECASE)
        # now try to match
        m = p.match(line)
        # if there is a match
        if m:
            file = m.group(1)         # the included filename 
            if file not in filelist:  # test for duplicates
                filelist.append(file) # add to list if not
                
    f.close()                         # close the filehandle
    filelist.sort()                   # sort the list
    return filelist                   # and return it

# if being called as a script
if __name__ == '__main__':
    '''print a makefile dependency line for some fortran source'''

    # only one command line arg permitted
    if len(sys.argv) != 2:
        sys.exit(2)
    filename = sys.argv[1]

    # ensure that the arg is a genuine file
    if not os.path.isfile(filename):
        sys.exit(1)
    else:
        # disect the filename
        head, tail = os.path.split(filename)
        base, ext = os.path.splitext(tail)
        # require that the file extension is conventional
        # for a Fortran source
        if ext not in valid_ext_list:
            sys.exit(1)
        # find the included filename
        include_list = find_includes(filename)
        # print the makefile line
        # e.g. "myfile.o : myfile.f inc1.cmn inc2.cmn"
        return_string = base + '.o : ' + tail + ' '
        for inc in include_list:
            return_string += inc + ' '
        print return_string
        # and exit
        sys.exit(0)

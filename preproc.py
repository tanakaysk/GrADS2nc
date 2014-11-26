#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import datetime

def mssgUsage():
    print 'Preprocessor of GrADS control file.'
    print '  Usage:'
    print '    python preproc.py [fileCtl] [fileNml]'
    print '      fileCtl [char] name of GrADS description file.'
    print '      fileNml [char] name of namelist.ncmake template file.'


def setAxisDef(lineItems):
    dimLen = int(lineItems[1])
    dimLoc = []
    if lineItems[2] == 'linear':
        locIni = float(lineItems[3])
        locInv = float(lineItems[4])
        for i in range(dimLen):
            dimLoc.append(locIni + locInv * i)

    elif lineItems[2] == 'levels':
        for cvalue in lineItems[3:]:
            dimLoc.append(float(cvalue))

    return dimLen, dimLoc


def setTAxisDef(lineItems):
    months = {'jan':1, 'feb':2, 'mar':3, 'apr':4,  'may':5,  'jun':6, 
              'jul':7, 'aug':8, 'sep':9, 'oct':10, 'nov':11, 'dec':12}

    dimLen = int(lineItems[1])
    tini = lineItems[3]
    yr = int(tini[-4:])
    mt = months[tini[-7:-4]]
    dy = int(tini[-9:-7])

    hr = 0
    mn = 0
    sc = 0
    if len(tini) > 9:
        hr = int(tini[0:2])
        if tini[2:3] == ':':
            mn = int(tini[3:5])
            if tini[5:6] == ':':
                sc = int(tini[6:8])
    print 'Reference date: {yr:04d}-{mt:02d}-{dy:02d} {hr:02d}:{mn:02d}:{sc:02d}'.format(
        yr = yr, mt = mt, dy = dy, hr = hr, mn = mn, sc = sc)

    dateRef = datetime.datetime(year = yr, month = mt, day = dy, hour = hr, minute = mn, second = sc)

    return dimLen, dateRef


def setAxisAppend(lineItems, dimLoc):
    for cvalue in lineItems:
        dimLoc.append(float(cvalue))
    return dimLoc


def setVariableDef(lineItems):
    varName = lineItems[0]
    if lineItems[1] == '1' or lineItems[1] == '0':
        varDim = 3
    else:
        varDim = 4
    long_name = ' '.join(lineItems[3:])

    return varName, varDim, long_name


def fileOutNamelistAxis(fnml, dimName, dimLen, standard_name, axis, units, fileCor):
    fnml.write(
        '&nml_coordinate\n'
        '  dimName       = \'' + dimName + '\',\n'
        '  dimLength     = ' + str(dimLen) + ',\n'
        '  dimKind       = \'float\',\n'
        '  standard_name = \'' + standard_name + '\',\n'
        '  long_name     = \'' + standard_name + '\',\n'
        '  axis          = \'' + axis + '\',\n'
        '  units         = \'' + units + '\',\n'
        '  fileCord      = \'' + fileCor + '\',\n/\n\n')


def fileOutNamelistZAxis(fnml, dimLen, fileCor):
    fnml.write(
        '&nml_coordinate\n'
        '  dimName       = \'depth\',\n'
        '  dimLength     = ' + str(dimLen) + ',\n'
        '  dimKind       = \'float\',\n'
        '  standard_name = \'depth\',\n'
        '  long_name     = \'depth of model levels\',\n'
        '  axis          = \'Z\',\n'
        '  positive      = \'down\',\n'
        '  units         = \'m\',\n'
        '  fileCord      = \''+ fileCor + '\',\n/\n\n')


def fileOutNamelistTAxis(fnml, timeIndex, dateRef, fileCor):
    nmlDateRef = '{yr:04d}{mt:02d}{dy:02d}{hr:02d}{mn:02d}{sc:02d}000000'.format(
        yr = dateRef.year, mt = dateRef.month, dy = dateRef.day, 
        hr = dateRef.hour, mn = dateRef.minute, sc = dateRef.second)
    fnml.write(
        '&nml_coordinate\n'
        '  dimName       = \'time\',\n'
        '  dimLength     = 1,\n'
        '  dimKind       = \'float\',\n'
        '  standard_name = \'time\',\n'
        '  long_name     = \'days since reference date\',\n'
        '  axis          = \'T\',\n'
        '  units         = \'day\',\n'
        '  calendar      = \'gregorian\',\n'
        '  dateRef       = \'' + nmlDateRef + '\',\n'
        '  fileCord      = \'' + fileCor + '\',\n/\n\n')


def fileOutNamelistVariable(fnml, varName, varDim, varLongName, FillValue):
    if varDim == 3:
        dimList = '\'Lon\', \'Lat\', \'time\''
    elif varDim == 4:
        dimList = '\'Lon\', \'Lat\', \'depth\', \'time\''
    else:
        dimList = '\'Lon\', \'Lat\''

    fnml.write(
        ('&nml_variable\n'
         '  varName       = \'' + varName + '\',\n'
         '  ndim          = {dim},\n'
         '  varKind       = \'float\',\n'
         '  dimList       = ' + dimList + ',\n'
         '  standard_name = \'@standard name@\',\n'
         '  long_name     = \'' + varLongName + '\',\n'
         '  units         = \'@unit@\',\n'
         '  undef         = {undef},\n'
         '  scale_factor  = 1.e0,\n'
         '  add_offset    = 0.e0,\n'
         '  fileVar       = \'@GrADS file@\',\n'
         '  nrec          = 1, 1, 1 \n/\n\n').format(dim = varDim, undef = FillValue))


def fileOutCoordinate(fileCor, dimLen, dimLoc):
    fcor = open(fileCor, 'w')
    strwrt = ''
    for i in range(dimLen):
        strwrt += '{0:11.4f}'.format(dimLoc[i])
        if i % 10 == 9:
            fcor.write(strwrt + '\n')
            strwrt = ''
    if len(strwrt) > 0:
        fcor.write(strwrt + '\n')
    fcor.close()


def preprocessor(filectl, filenml):
    fctl = open(filectl, 'r')
    lines = fctl.readlines()
    fctl.close()

    flgVarDef = False
    flgDimXDef = False
    flgDimYDef = False
    flgDimZDef = False
    varNames = []
    varDims = []
    varLongNames = []
    dimYLoc = []
    dimZLoc = []

    for line in lines:
        item = (line.lower()).split()

        # undefined value
        if item[0] == 'undef':
            undefValue = float(item[1])

        # X-axis
        if item[0] == 'xdef':
            flgVarDef = False
            flgDimXDef = True
            flgDimYDef = False
            flgDimZDef = False
            dimXLen, dimXLoc = setAxisDef(item)

        # Y-axis
        elif item[0] == 'ydef':
            flgVarDef = False
            flgDimXDef = False
            flgDimYDef = True
            flgDimZDef = False
            dimYLen, dimYLoc = setAxisDef(item)

        # Z-axis
        elif item[0] == 'zdef':
            flgVarDef = False
            flgDimXDef = False
            flgDimYDef = False
            flgDimZDef = True
            dimZLen, dimZLoc = setAxisDef(item)
        
        # T-axis
        elif item[0] == 'tdef':
            flgVarDef = False
            flgDimXDef = False
            flgDimYDef = False
            flgDimZDef = False
            tidx, dateRef = setTAxisDef(item)

        # number of variables
        elif item[0] == 'vars':
            varnum = int(item[1])
            flgDimXDef = False
            flgDimYDef = False
            flgDimZDef = False
            flgVarDef = True
            print 'Number of variables: ', varnum

        # end of variable definition section
        elif item[0] == 'endvars':
            flgVarDef = False

        else:
            if flgDimXDef:
                dimXLoc = setAxisAppend(item, dimXLoc)
            if flgDimYDef:
                dimYLoc = setAxisAppend(item, dimYLoc)
            if flgDimZDef:
                dimZLoc = setAxisAppend(item, dimZLoc)
            if flgVarDef:
                name, ndim, lname = setVariableDef(item)
                varNames.append(name)
                varDims.append(ndim)
                varLongNames.append(lname)

    # output to the namelist file
    fnml = open(filenml, 'w')
    fnml.write(
        '&nml_files\n'
        '  file_nc = \'@output file name@\',\n/\n\n'
        '&nml_general\n'
        '  ncor   = 4,\n'
        '  nvar   = ' + str(varnum) + ',\n'
        '  title  = \'@title of the file@\',\n'
        '  source = \'@description of the file@\',\n/\n\n')

    fileOutNamelistAxis(fnml, 'Lat', dimYLen, 'latitude', 'Y', 'degree_north', 'lat.txt')
    fileOutNamelistAxis(fnml, 'Lon', dimYLen, 'longitude', 'X', 'degree_east', 'lon.txt')
    fileOutNamelistZAxis(fnml, dimZLen, 'z.txt')
    fileOutNamelistTAxis(fnml, tidx, dateRef, 'days.txt')
    for i in range(len(varNames)):
        fileOutNamelistVariable(fnml, varNames[i], varDims[i], varLongNames[i], undefValue)
    fnml.close()

    # output to the coordinate file
    fileOutCoordinate('lon.txt', dimXLen, dimXLoc)
    fileOutCoordinate('lat.txt', dimYLen, dimYLoc)
    fileOutCoordinate('z.txt', dimZLen, dimZLoc)
    fileOutCoordinate('days.txt', 1, [tidx])


if __name__ == '__main__':
    args = sys.argv
    if len(args) != 3:
        mssgUsage()
        sys.exit()

    preprocessor(args[1], args[2])

#!/bin/bash

# Script to compile ncmakes
#
# root directory of ncmake
TOOL_HOMED="./"
# directory of common programming platform
CPPFD="/home/tanakaysk/common"
# Fortran 95 compiler
COMPILE_F95=gfortran44 #ifrot

if [ ! -d ${TOOL_HOMED} ] ; then
    echo "Error: Invalid directory path to Layer model sources"
    echo "       Check variable MDL_HOMED."
    exit 1
fi

target="all"
fdbg=0
if [ $# -eq 1 ] ; then
    if [ $1 == "debug" ] ; then
	fdbg=1
	target="all"
    else
	fdbg=0
	target=$1
    fi
fi

nm_host=`hostname`
nm_user=`who -m | awk '{print $1}'`

case ${nm_host} in
    ubuntu )
	CPPFD=/home/${nm_user}/common
	COMPILE_F95=gfortran44
	;;
    OCEAN-02 | OCEAN-03 )
	CPPFD=/home/${nm_user}/common
	COMPILE_F95=gfortran44
	;;
    yf4p22 )
	if [ ${nm_user} == "tanakaysk" ] ; then
	    CPPFD=/home/${nm_user}/common
	    COMPILE_F95=gfortran44
	fi
	;;
    * )
	nm_host=default
	dcheck=0
	if [ ! -d ${CPPFD} ] ; then
	    echo "Error: Invalid directory path to Common development platforms"
	    echo "       Check variable CPPFD."
	    exit 2
	fi
	if [ ! -e ${CPPFD}/compiler/compiler.mk.${COMPILE_F95} ] ; then
	    echo "Error: compiler macro file does not exist"
	    echo "       Check variable COMPILE_F95."
	    exit 3
	fi
	;;
esac

export CPPFD=${CPPFD}
export TOOL_HOMED=${TOOL_HOMED}

cp -v ${CPPFD}/env/env.mk.${nm_host} env.mk
if [ ${fdbg} -eq "0" ] ; then
    cp -v ${CPPFD}/compiler/compiler.mk.${COMPILE_F95} compiler.mk
else
    cp -v ${CPPFD}/compiler/compiler.mk.${COMPILE_F95}.dbg compiler.mk
fi
cp -v ncmake.mk.${COMPILE_F95} ncmake.mk

make ${target}

if [ ${target} = "clean" ] ; then
    rm -f env.mk compiler.mk ncmake.mk
fi

exit 0

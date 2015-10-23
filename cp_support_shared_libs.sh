#!/bin/bash
local d="$1"            # JAIL ROOT
local pFILE="$2"        # copy bin file libs
local files=""
## use ldd to get shared libs list ###
files="$(ldd $pFILE |  awk '{ print $3 }' | sed  '/^$/d')"

for i in $files
do
  dcc="${i%/*}" # get dirname only
  [ ! -d ${d}${dcc} ] && mkdir -p ${d}${dcc}
  ${_cp} -f $i ${d}${dcc}
done

# Works with 32 and 64 bit ld-linux
sldl="$(ldd $pFILE | grep 'ld-linux' | awk '{ print $1}')"
sldlsubdir="${sldl%/*}"
[ ! -f ${d}${sldl} ] && ${_cp} -f ${sldl} ${d}${sldlsubdir}

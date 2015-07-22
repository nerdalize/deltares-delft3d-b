#!/bin/sh

# ============ set runid
sedid=sed
mudid=mud

# ============ set exedir
exedir=$D3D_HOME/$ARCH/flow/bin
export D3D_HOME

echo ===================================================================
echo === testcase $sedid and $mudid =============== start =================
echo ===================================================================

# ============ remove temp files
rm runid
rm tri-diag.*
rm md-diag*.*
rm TMP*
rm *.msg
rm tri-prt.*
rm com-*
rm trih-*
rm trim-*
rm trid-*
rm fourier*.*
rm dio-*
rm dio.errors
rm gpp*

# ============ interface settings
# d3dtmpl -flow -simulation -rm -use runid
# set WRITE_WIDGET=yes

# ============ create file runid with sedid
echo $sedid > runid
echo === start tdatom.exe sediment ===
$exedir/tdatom.exe
echo === end tdatom.exe sediment ===

# ============ create file runid with mudid
echo $mudid > runid
echo === start tdatom.exe mud ===
$exedir/tdatom.exe
echo === end tdatom.exe mud ===

echo press enter to continue
read dummy

PATH="$D3D_HOME/$ARCH/flow/bin:$PATH"
esmContext=`esm_create 2> /dev/null`
if [ $? -eq 0 ]; then
    # use context
    DIO_SHM_ESM=$esmContext
    export DIO_SHM_ESM 
    echo "Starting processes for shared mem context: $esmContext"

    echo press enter to continue
    read dummy

    echo === start trisim.exe mud ===
    #xterm -e $exedir/trisim.exe &
    $exedir/trisim.exe &
    echo === end trisim.exe mud ===

    #echo press enter to continue
    #read dummy
    echo waiting 5 seconds
    sleep 5

    echo === start trisim.exe sediment ===
    echo $sedid > runid
    #xterm -e $exedir/trisim.exe &
    $exedir/trisim.exe
    echo === end trisim.exe sediment ===

    echo press enter to continue
    read dummy
    # delete context
    esm_delete $esmContext 2> /dev/null
    if [ $? -ne 0 ]; then
        echo "Cannot delete ESM context $esmContext:"
        esm_delete $esmContext
    fi
else
    echo "Cannot create ESM context:"
    esm_create
fi

# remove temp files
#rm runid
#rm TMP*.*
#rm tri*.*
#rm *.msg
#rm com*.*
#rm fourier*.*
#rm md-diag*.*

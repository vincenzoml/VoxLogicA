#!/bin/bash

usage() {
    echo
    echo "usage: $0 basedir scriptname"
    echo "basedir is the base directory where the dataset is located (e.g. brats17)"
    echo "scriptname is the template script name with .imgql extension"
    echo
}

process() {
    OUTPUTDIR="$OUTPUT/$OUTPUTPREFIX/$INPUTDIR"
    EXECUTE="$OUTPUTDIR"/input.imgql
    LOG="$OUTPUTDIR"/log.txt
    mkdir -p "$OUTPUTDIR"
    cat "$SCRIPTNAME" |
	sed 's@$NAME@'"$NAME"'@g' |
	sed 's@$INPUTDIR@'"$INPUTDIR"'@g' |
	sed 's@$OUTPUTDIR@'"$OUTPUTDIR"'@g' > "$EXECUTE"
    echo -n "$SCRIPTNAME started at $(date) on $NAME..."
    echo "$SCRIPTNAME started at $(date) on $NAME" > "$LOG"
    "$VOXLOGICA" "$EXECUTE" >> "$LOG"
    RET=$?
    if [ "$RET" -ne "0" ]; then
	echo " [failed with code $RET (see $LOG for details)]"
    cat $LOG
    else
	echo " [OK]"
	if ! [ -f "$STATS" ]; then
	    echo -ne "filename\t" > "$STATS"
	    cat "$LOG" | grep '\[user\]'| cut -b 23- | sort | cut -f 1 -d "=" | tr "\n" "\t" >> "$STATS"
	    echo >> "$STATS"
	fi
	echo -ne "$NAME\t" >> "$STATS"
	cat "$LOG" | grep '\[user\]' | cut -b 23- | sort | cut -f 2 -d "=" | tr "\n" "\t" | tr -d '"' >> "$STATS"
	echo >> "$STATS"
    fi
}

canonical() {
    echo "$(dirname "$1")/$(basename "$1")"
}

if [ "$VOXLOGICA" == "" ]; then
    VOXLOGICA=""
    PATHS="~/bin/VoxLogicA /opt/VoxLogicA/VoxLogicA /usr/local/bin/VoxLogicA"
    for vlpath in $PATHS
        do  if test -f $vlpath; then  
                VOXLOGICA=$vlpath
                break
            fi
    done
fi

if [ "$VOXLOGICA" == "" ]; then
    echo No VoxLogicA executable found in $PATHS
    echo please set the VOXLOGICA environment variable to point to the VoxLogicA executable prior to running this script
    exit 1
fi

if [ "$#" -ne 2 ]; then
    usage
else
    echo Using executable "$VOXLOGICA"
	OUTPUT=output
    DIR=$(canonical "$1")
    SCRIPTNAME=$(canonical "$2")
    OUTPUTPREFIX=$(basename "$SCRIPTNAME" ".imgql")
    STATS="$OUTPUT/$OUTPUTPREFIX-stats.csv"
    rm -f "$STATS"
    find -L "$DIR" -mindepth 1 -maxdepth 1 -type d -print0 |
    while read -d $'\0' INPUTDIR
    do NAME=$(basename "$INPUTDIR")
        process
    done
fi


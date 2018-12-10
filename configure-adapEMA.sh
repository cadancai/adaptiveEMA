#!/bin/sh

#usage
#[path to script]/configure-adapEMA.sh [path to the scripts folder] [path to put the adapEMA system] 

if [ "$#" -ne 2 ]; then
	echo 'Error: the number of parameters should be 2.'
	echo "Usage: [path to script]/configure-adapEMA.sh [path to put the adapEMA system]"
	exit 1
fi

scriptFolder=$1
systemFolderLocation=$2


#create the system directory
mkdir  $systemFolderLocation/adapEMA

#go into the directory
cd $systemFolderLocation/adapEMA

#create the various folders
mkdir newData
mkdir notifications
mkdir policies
mkdir prepData
mkdir scripts

#move all scripts to the scripts folder
cp $scriptFolder/* $systemFolderLocation/adapEMA/scripts



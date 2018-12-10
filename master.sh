#!/bin/sh

#usage
#[path to script]/master.sh [path to system folder] [project folder] [s3 bucket] [epsilon for e-greedy]

if [ "$#" -ne 4 ]; then
	echo 'Error: the number of parameters should be 4.'
	echo "Usage: [path to script]/master.sh [path to system folder] [project folder] [s3 bucket]"
	exit 1
fi

systemFolder=$1
projFolder=$2
s3Bucket=$3
epsilon=$4


#sync the data from S3 bucket to the system folder
Rscript $systemFolder/scripts/syncData.R $s3Bucket/ $systemFolder

#copy the data in the past day to the newData folder
Rscript $systemFolder/scripts/copyData.R $systemFolder $projFolder

#preprocess the data in the newData folder and save them in the prepData folder
Rscript $systemFolder/scripts/prepData.R $systemFolder $projFolder

#update the policy using the preprocessed data in the prepData folder
Rscript $systemFolder/scripts/updatePolicy.R $systemFolder $projFolder $epsilon

#push the updated policy and notification json files into the s3 bucket
Rscript $systemFolder/scripts/putPolicyNotification.R $systemFolder $s3Bucket/

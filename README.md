# adaptiveEMA
Scripts for adaptive EMA on the server side.

##Introduction

This system is a subcomponent that runs with the mobile sensing app [Sensus](https://predictive-technology-laboratory.github.io/sensus/), which is developed by our lab Predictive Technology Lab at University of Virginia. More infos will be added in the coming future as we are testing it.

##Instructions

1. git clone this repository into any server side node (e.g., AWS EC2 instance).
2. run the configure-adapEMA.sh script to setup the system.
3. schedule a crontab job with the master.sh script that runs at a certain frequency (e.g., daily)

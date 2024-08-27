#!/bin/bash

if [[ $# != 1 ]] ; then
	echo 'Scriptul asteapta numarul de instante'
	exit 255
fi

echo "" > info.txt

for ((i = 1; i <= "$1"; i++)) ; do
	iperf -c 172.16.0.100 &>> info.txt &
done

# iperf -s -u -b --10240 & iperf -s

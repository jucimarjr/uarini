pid=$!

cd threadring/src/
sh script.sh

#Encerra script 
sleep 2

kill $pid

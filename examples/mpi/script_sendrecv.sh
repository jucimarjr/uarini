pid=$!

cd sendrecv/src/
sh script.sh

#Encerra script 
sleep 2

kill $pid

pid=$!

sh script_pingping.sh
sh script_pingpong.sh
sh script_threadring.sh
sh script_alltoall.sh
sh script_sendrecv.sh

#Encerra script 
sleep 2

kill $pid

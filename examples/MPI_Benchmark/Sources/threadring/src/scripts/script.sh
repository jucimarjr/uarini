pid=$!

cd scala/
sh script_scala.sh
cd ../erlang/
sh script_erl.sh

#Encerra script 
sleep 2

kill $pid

pid=$!

cd erlang/
sh script_erl.sh
cd ../java/
sh script_java.sh
cd ../scala/
sh script_scala.sh

#Encerra script 
sleep 2

kill $pid

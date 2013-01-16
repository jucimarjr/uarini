cd erlang/
erlc *.erl

cd ../java/
javac *.java

cd ../scala/
scalac threadring.scala

pid=$!
 
#Inicia a Aplicacao
for k in 1000 10000 20000 30000 50000 100000
do
for j in 5000 10000 50000 100000 500000 1000000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do

echo "$h DataSize $j Repetitions $k Process - Threadring - Erlang"
cd ../erlang
erl -smp enable -noshell -eval "threadring:run([\"$h\",\"$j\",\"$k\"])." -s init stop

echo "$h DataSize $j Repetitions $k Process - Threadring - Java"
cd ../java
java SendRecvPrincipal $h $j $k

echo "$h DataSize $j Repetitions $k Process - Threadring - Scala"
cd ../scala
scala -J-Xmx8g threadring $h $j $k threadring_scala_"$h"_"$j"_"$k".txt


done
done
done
done

#Encerra script 
sleep 2

kill $pid

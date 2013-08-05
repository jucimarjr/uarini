pid=$!

cd ../../scala
scalac sendrecv.scala
 
#Inicia a Aplicacao
for k in 1000 10000 20000 30000 50000 100000
do
for j in 5000 10000 50000 100000 500000 1000000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
scala -J-Xmx8g sendrecv $h $j $k sendrecv_scala_"$h"_"$j"_"$k".txt
echo "$h DataSize $j Repetitions $k Process - Scala"
done
done
done
done

#Encerra script 
sleep 2

kill $pid

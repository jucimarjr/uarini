cd ../../java/
javac *.java
pid=$!

java AllToAllPrincipal 10 10 10

for k in 1000 2000 3000 4000 5000 10000
do
for j in 5000 10000 50000 100000 500000 1000000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
java -server -XX:+TieredCompilation -XX:+AggressiveOpts AllToAllPrincipal $h $j $k
echo "$h DataSize $j Repetitions $k Process - Java - AllToAll"
done
done
done
done

#Encerra script 
sleep 2
 
kill $pid

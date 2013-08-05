pid=$!


for k in 5000 10000 50000
do
for j in 5000 10000 50000 100000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
echo "$h DataSize $j Repetitions $k Processes - Ruby - ThreadRing"
ruby threadring.rb $h $j $k
done
done
done
done

#Encerra script 
sleep 2

kill $pid

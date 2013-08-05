pid=$!


for a in 5000 10000 50000 100000 500000 1000000 5000000
do
for b in 5000 10000 50000 100000
do
for c in 1 2 3 4 5 6 7 8 9 10 
do
echo "$b DataSize $a Repetitions - Ruby - PingPing"
ruby pingping.rb $b $a
done
done
done

#Encerra script 
sleep 2

kill $pid

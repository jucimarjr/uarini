pid=$!

cd ../../erlang
erlc *.erl
 
#Inicia a Aplicacao
for j in 5000 10000 50000 100000 500000 1000000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
erl -noshell -eval "pingping:run($h,$j)" -s init stop
echo "$h DataSize $j Repetitions - Erlang - PingPing"
done
done
done

#Encerra script 
sleep 2

kill $pid

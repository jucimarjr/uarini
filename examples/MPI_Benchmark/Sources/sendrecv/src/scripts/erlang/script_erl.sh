cd ../../erlang
erlc *.erl

#output_file="saida.txt"      #arquivo de saída
#interval=1                      #taxa de amostragem em segundos

#Inicia rodada de testes
#LC_ALL=C sar -rR -u ALL $interval > $output_file &
pid=$!

for k in 1000 10000 20000 30000 50000 100000
do
for j in 5000 10000 50000 100000 500000 1000000
do
for h in 5000 10000 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
erl -smp enable -noshell -eval "sendrecv:run([\"$h\",\"$j\",\"$k\"])." -s init stop
echo "$h DataSize $j Repetitions $k Process - Erlang"
done
done
done
done
#Encerra script 
sleep 2
 
kill $pid

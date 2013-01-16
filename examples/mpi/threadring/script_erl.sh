cd ../../erlang
erlc *.erl

#output_file="saida.txt"      #arquivo de saída
#interval=1                      #taxa de amostragem em segundos

#Inicia rodada de testes
#LC_ALL=C sar -rR -u ALL $interval > $output_file &
pid=$!

for k in 10000
do
for j in 100000
do
for h in 50000
do
for i in 1 2 3 4 5 6 7 8 9 10 
do
echo "$h DataSize $j Repetitions $k Process - Erlang"
erl -smp enable -noshell -eval "threadring:run([\"$h\",\"$j\",\"$k\"])." -s init stop
done
done
done
done
#Encerra script 
sleep 2
 
kill $pid

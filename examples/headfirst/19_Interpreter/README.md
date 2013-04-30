INTERPRETER
===========

Definição pela GOF(Gang of Four): "Dada uma linguagem, definir uma representação para sua gramática junto com um interpretador que
                                   usa a representação para interpretar sentenças na linguagem."

No exemplo dado foi criada uma linguagem simples que interpreta uma sentença matemática pós-fixada e calcula o resultado da sentenca
matemática. Esta sentença é toda literal, e o valor de cada variável é inserido separadamente. Foram usadas variáveis que podem ser
alocadas em uma tabela hash, representando as variáveis e o valor de cada uma. Para cada regra da linguagem foi criada uma classe,
nesse caso Sum (soma), Minus (subtração), Number (Valor numérico de uma variável) e Variable (Variáveis). A classe Evaluator é
responsável por, dada a sentença inicial, montar a Árvore Sintática desta sentença para que possa ser utilizado o método interpret(),
responsável por resolver a Árvore Sintática, substituindo as variáveis por seus respectivos valores e retornando o resultado
final do cálculo.

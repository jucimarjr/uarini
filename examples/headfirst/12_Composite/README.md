COMPOSITE
=========

Definição pela GOF(Gang of Four): "Compor objetos em estruturas de árvore para representar hierarquias todo-parte. Composite permite
                                   que clientes tratem objetos individuais e composições de objetos de maneira uniforme."

Este exemplo trata-se de uma implementação simples de estrutura feita de estruturas. Cada estrutura pode ser uma linha (Row) ou coluna
(Column). Dentro de uma linha podem existir linhas, colunas ou elementos. Isto também é válido para as colunas, que podem possuir, da
mesma forma, linhas, colunas ou elementos. O padrão Composite é utilizado neste exemplo na implementação desta estrutura de árvore, onde
um elemento pode representar um valor ou uma subarvore. Este exemplo implementa também o percorrimento nesta estrutura de árvore com
a utilização do método transverse(), de forma recursiva, chamando a si mesmo e passando como parâmetro os filhos do elemento atual.

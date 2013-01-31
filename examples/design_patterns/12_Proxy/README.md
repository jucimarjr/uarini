PROXY
======

Um proxy, em sua forma mais geral, é uma classe que funciona como uma interface para outra classe. A classe proxy poderia conectar-se a qualquer coisa: uma conexão de rede, um objeto grande em memória, um arquivo, ou algum recurso que é difícil ou impossível de ser duplicado. Um exemplo bem conhecido do padrão proxy é um objeto ponteiro de referência de contagem.
Em situações onde múltiplas cópias de um objeto complexo deve existir o padrão proxy pode ser adaptado para incorporar o padrão flyweight a fim de reduzir o rastro de memória das aplicações. Normalmente uma instância de um objeto complexo é criada e vários objetos proxies são criados, todos contendo uma referência ao único objeto complexo original. Quaisquer operações realizadas nos proxies são enviadas ao objeto original. Uma vez que todas as instâncias do proxy estiverem fora do escopo, a memória do objeto complexo pode ser desalocada.


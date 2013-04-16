ITERATOR
========

Definição pela GOF(Gang of Four): "Prover uma maneira de acessar os elementos de um objeto agregado sequencialmente sem expor sua
                                   representação interna."

Neste exemplo existem dois tipos diferentes de menus, um para o café da manhã e outro para o almoço. Porém cada um destes menus
guarda seus itens de forma diferente. Para percorrerem seus itens, também utilizam implementações distintas. O objetivo é criar 
uma solução para percorrer ambos os ítens da mesma forma, sem modificar a implementação atual. A solução está no padrão Iterator,
que permite que, com a criação de uma nova classe chamada Iterator, seja possível encapsular as formas distintas de armazenamento 
dos itens de ambos os menus, assim como percorrer estes itens. Na implementação vai existir um Iterator para cada menu, assim como
a interface Iterator. Cada Iterator irá mascarar a forma de armazenamento e busca dos ítens dos menus. O código torna-se encapsulado
e, apesar de ambos os menus utilizarem formas diferentes de armazenar os itens, para o usuário não há diferenças entre eles.

FACADE
======

Definição pela GOF(Gang of Four): "Oferecer uma interface única para um conjunto de interfaces de um subsistema. Facade define uma
                                   interface de nível mais elevado que torna o subsistema mais fácil de usar."

No exemplo do Home Theather, foi verificado que para que fosse possível apenas assistir um filme, era necessário realizar diversas
ações, como por exemplo ligar o projetor, ligar o Dvd Player, inserir o disco no Dvd Player, preparar a pipoca, desligar as luzes,
etc. O objetivo de Facade neste caso é para facilitar as atividades, de por exemplo, assistir Dvd, ouvir Cd, ouvir Rádio, entre
outras atividades. Foi criada a classe HomeTheaterFacade que encapsula todas as pequenas ações necessárias para que se realize uma 
dessas atividades. Dessa forma é possível assistir a um filme apenas chamando o método watchMovie() desta classe, e todas as pequenas
tarefas necessárias já são realizadas dentro da classe HomeTheaterFacade. O sistema torna-se muito mais fácil de ser utilizado, pois 
a interface com o cliente torna-se bastante simplificada.

FLYWEIGHT
=========

Definição pela GOF(Gang of Four): "Usar compartilhamento para suportar grandes quantidades de objetos refinados eficientemente."

No exemplo utilizado, é modelado um restaurante que serve chás. Neste cenário, diversas mesas (tables) podem pedir diversos tipos de
sabores diferentes de chá (TeaFlavors), assim como cada mesa pode pedir vários tipos de chá, repetidos ou não. A quantidade de tipos
de sabores de chá é pequena (Chai, Camomile, Earl Grey), porém a quantidade de pedidos pode ser muito grande. Utilizando este padrão,
ao invés de, para cada pedido, criar um novo objeto do tipo chá com o sabor desejado, foi criada uma lista com todos os diferentes
sabores pedidos (Flavors). Sempre que um novo pedido é feito, não importando de qual mesa, a lista de sabores é consultada e, se o
tipo de sabor do chá já tiver sido pedido anteriormente, ele é apenas reutilizado (compartilhado), pois o objeto referente a ele já
foi criado, sem que haja a necessidade de criar inúmeros objetos para cada sabor, possibilitando que sejam feitos inúmeros pedidos
de chá, e economizando memória da aplicação.

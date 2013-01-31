VISITOR
======

“Geralmente, as novas funcionalidades de um objeto seriam adicionadas nos objetos existentes, se não fosse o uso de design patterns”
O Visitor é um design pattern muito interessante que permite acrescentarmos operações em um objeto sem a necessidade de alterá-lo. 
Normalmente o Visitor representará uma operação comum que atuará em uma família de objetos de um mesmo tipo, podendo esta operação ser específica para sub-tipos daquela família de objetos.
O Visitor é também uma maneira elegante de você reduzir radicalmente o uso de extensos if(objeto instanceof X), else if(objeto instanceof Y), propondo um modelo onde uma classe Visitor poderá representar a operação para todos os seus sub-tipos e esta operação poderá ser acionada pelo objeto sem ele conhecer sua realização concreta.

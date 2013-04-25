BRIDGE
======

Definição pela GOF(Gang of Four): "Desacoplar uma abstração de sua implementação para que os dois possam variar independentemente."

No exemplo utilizado, são modelados tipos de veículos (Car, Bike) e formas nas quais estes veículos podem ser construídos, por exemplo,
montados ou fabricados (Assemble, Produce). Workshop é a interface que irá ser implementada pelos métodos que são os tipos de construção
e Vehicle é a classe abstrata cujas classes do tipo veículos irão herdá-la. Dessa forma, com a utilização do padrão Bridge, é possível
inserir vários outros tipos de veículos e várias formas de estes veículos serem construídos, sem grandes impactos na modelagem, pois
a implementação e a abstração estão desacopladas. Se este padrão não estivesse sendo utilizado, cada veículo teria que possuir suas
próprias classes para os diferentes tipos de montagem. E a cada inserção de um novo veículo, todos os tipos de montagem teriam que
ser novamente repetidos. Com este padrão, isto é evitado, não ocorrendo o risco de inconsistências e de duplicação de código.

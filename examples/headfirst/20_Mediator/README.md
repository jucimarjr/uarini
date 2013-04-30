MEDIATOR
========

Definição pela GOF(Gang of Four): "Definir um objeto que encapsula como um conjunto de objetos interage. Mediator promove acoplamento
                                   fraco ao manter objetos que não se referem um ao outro explicitamente, permitindo variar sua 
                                   interação independentemente."

No exemplo dado foi implementado um cenário de controle de tráfego aéreo (Air Traffic Controller - ATC). Existem dois objetos principais
que interagem nesse cenário: Flight (Vôo) e Runway (Pista de pouso). Para que um pouso de um avião seja permitido, é necessário que a
pista de pouso esteja livre para pousos. Portanto, à primeira vista, bastaria que um objeto se comunicasse com outro para que eles
tivessem conhecimento de seus estados internos. Mas, se, por acaso, existem diversos tipos de vôos e/ou diferentes tipos de pistas de pouso,
a quantidade de objetos cresce e a comunicação fica descentralizada e complexa. Para centralizar a comunicação e desacoplar os objetos
que devem interagir, foi usado o padrão Mediator, criando-se assim a classe ATCMediator, que possui os estados dos objetos, e cada objeto
se comunica apenas com o Mediator, permitindo que os objetos não interajam entre si diretamente, tornando a modelagem mais livre de erros.

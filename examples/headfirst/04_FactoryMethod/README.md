FACTORY METHOD
==============

Definição pela GOF(Gang of Four): "Definir uma interface para criar um objeto mas deixar que subclasses decidam que classe instanciar.
                                   Factory Method permite que uma classe delegue a responsabilidade de instanciamento às subclasses."

No exemplo das Lojas de Pizza NYPizzaStore e ChicagoPizzaStore, cada uma delas possui os mesmos sabores de pizza, no caso, Cheese,
Clam, Pepperoni e Veggie. Mas para cada loja, os sabores possuem diferentes características, diferentes ingredientes e precisam 
ser montadas de diferentes formas. Existe também a classe PizzaStore, responsável por decidir qual das lojas vai criar a pizza. 
Esta é a principal característica deste padrão. Dependendo do tipo de pedido de pizza que é passado para a classe PizzaStore, 
esta classe irá decidir se envia para a NYPizzaStore ou ChicagoPizzaStore. Apartir deste ponto, a pizza é montada de acordo com 
a loja escolhida.
